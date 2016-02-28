# rough plan for 1,2 
# transform data for expected fittability
# carve up train set 20/80, train against 80
# then regress (various methods) swingingstrike against everything meaningful, producing prob swingingstrike (regress not category)
# and run against reserved train set for score
# find best model and run against test

#X is toward catcher's right, Z is height

# Init_Pos_Y is always 50 and Plate_Y is always 0
# Vel is mph, 
#accel is ft/sec^2? 
#position & break X,Y,Z are ?
#spin rate = rpm

# question & data organized like sequencing should be ignored. Comment to that effect.


library("readr")
library("dplyr")
train=read_delim(file = "TrainData.txt", delim = "|")




# ONE - Avg per batter
batterAvgSandM = train %>% 
  select (batterid, swingingstrike) %>%
  group_by(batterid) %>%
  summarise (batterAvgSandM=mean(swingingstrike), count=n())

avgSandM =mean(train$swingingstrike) #20.4%

#blend batter avg with lg avg based on count(pitchesSeen)  100% league at 0, 100% batter at ? (median is 156)
decay=function(x) exp(-.01*x)
#in principle, you could see where this stabilizes as a function of batter pitches seen and use that. exponential decay with .01 seems reasonable
batterWtAvg = decay(batterAvgSandM$count)*avgSandM + (1-decay(batterAvgSandM$count))*batterAvgSandM$batterAvgSandM
batterAvgSandM=cbind(batterAvgSandM, batterWtAvg)

cleanTrain=train %>% left_join(batterAvgSandM, by="batterid") 
drops <- c("gameid", "event", "pitcher_name", "batter_name", "batterAvgSandM", "count", "swingingstrike", "Plate_Y", "Init_Pos_Y")
cleanTrain=cbind(cleanTrain[,!names(cleanTrain) %in% drops], swingingstrike=cleanTrain$swingingstrike)



# TWO - 4 throws,bats combinations
cleanTrain$throwsBats = paste(cleanTrain$throws,cleanTrain$bats,sep= "")
cleanTrain$throwsBats <- factor(cleanTrain$throwsBats)


### THREE - in Zone



isStrike = function(X,Z, SZ_Top, SZ_Bottom){
  ifelse (abs(X)<8.5/12 && SZ_Bottom < Z && Z < SZ_Top, TRUE, FALSE)
}

cleanTrain$isStrike=mapply(isStrike, cleanTrain$Plate_X, cleanTrain$Plate_Z, cleanTrain$SZ_Top, cleanTrain$SZ_Bottom)


#z as +/- from center of zone, then square it. linear relationship to whiff. (see plot)
relativeZsq = function(Z, top, bottom) {
  layer=top-bottom
  middle = bottom + layer/2
  relativeZ = Z - middle
  relativeZ^2
}

cleanTrain$relativeZsq = relativeZsq (cleanTrain$Plate_Z, cleanTrain$SZ_Top, cleanTrain$SZ_Bottom)


#outside, ^2. linear relationship to whiff. (see plot)
outside = function(X,bats) {
  X * ifelse(bats =="L",-1,1)
}

cleanTrain$outside <- outside(cleanTrain$Plate_X, cleanTrain$bats)
cleanTrain$outside2 <- cleanTrain$outside^2





# FOUR: difference between pitcher's avg. fastball and current pitch
pitchCount <- train %>%
  group_by(pitcherid) %>%
  summarise(count=n())


avgFB <- train %>% 
  select(pitcherid, pitchtype, Init_Vel) %>%
  filter(pitchtype=='FB') %>%
  group_by(pitcherid) %>%
  summarise(FB=mean(Init_Vel), count=n())



lgAvgFB = train %>% 
  select(pitchtype, Init_Vel) %>%
  filter(pitchtype=='FB') %>%
  summarise(FB=mean(Init_Vel))

avgFB$pitcherWtAvgFB = decay(avgFB$count)*lgAvgFB$FB + (1-decay(avgFB$count))*avgFB$FB
cleanTrain=cleanTrain %>% left_join(avgFB, by="pitcherid") 
cleanTrain$velDiff = cleanTrain$pitcherWtAvgFB - cleanTrain$Init_Vel
drops <- c("FB", "count", "swingingstrike","SZ_Top", "SZ_Bottom", "swingingstrike", "Plate_Z", "Plate_X")
cleanTrain=cbind(cleanTrain[,!names(cleanTrain) %in% drops], swingingstrike=cleanTrain$swingingstrike)

# make appropriate things into factors and clean-up

cleanTrain=na.omit(cleanTrain) 
cleanTrain$throws <- factor(cleanTrain$throws)
cleanTrain$bats <- factor(cleanTrain$bats)
cleanTrain$pitchtype <- factor(cleanTrain$pitchtype)
# could make year a factor also, but there is actually a pretty monotonic trend.

#don't want to fit on pitcherid or batterid get rid of that.
#cleanTrain$batterid <- factor(cleanTrain$batterid)
#cleanTrain$pitcherid <- factor(cleanTrain$pitcherid)
cleanTrain$batterid <- NULL
cleanTrain$pitcherid <- NULL
#Save cleanTrain$pitchid in case we need it later, then delete from df

cleanTrain_pitchID = cleanTrain$pitchID
cleanTrain$pitchID <- NULL



###############




# Need to do the same thing to TestData.txt 

# BUT... for personal averages on batter whiff & pitcher FB Vel might not have a record for the person in test
# Use league avg in that case
avgFBVel = as.numeric(lgAvgFB[1,1])
#avgSandM

test=read_delim(file = "TestData.txt", delim = "|")


# ONE - Avg per batter

cleanTest=test %>% left_join(batterAvgSandM, by="batterid") 
cleanTest$batterWtAvg[is.na(cleanTest$batterWtAvg)] <- avgSandM



# TWO - 4 throws,bats combinations
cleanTest$throwsBats = paste(cleanTest$throws,cleanTest$bats,sep= "")
cleanTest$throwsBats <- factor(cleanTest$throwsBats)


### THREE - in Zone

cleanTest$isStrike = mapply(isStrike, cleanTest$Plate_X, cleanTest$Plate_Z, cleanTest$SZ_Top, cleanTest$SZ_Bottom)
cleanTest$relativeZsq = relativeZsq (cleanTest$Plate_Z, cleanTest$SZ_Top, cleanTest$SZ_Bottom)
cleanTest$outside <- outside(cleanTest$Plate_X, cleanTest$bats)
cleanTest$outside2 <- cleanTest$outside^2



# FOUR: difference between pitcher's avg. fastball and current pitch

cleanTest=cleanTest %>% left_join(avgFB, by="pitcherid") 
cleanTest$pitcherWtAvgFB[is.na(cleanTest$pitcherWtAvgFB)] <- avgFBVel
cleanTest$velDiff = cleanTest$pitcherWtAvgFB - cleanTest$Init_Vel




# make appropriate things into factors and clean-up

cleanTest=na.omit(cleanTest) 
cleanTest$throws <- factor(cleanTest$throws)
cleanTest$bats <- factor(cleanTest$bats)
cleanTest$pitchtype <- factor(cleanTest$pitchtype)
# could make year a factor also, but there is actually a pretty monotonic trend.

#don't want to fit on pitcherid or batterid get rid of that.
cleanTest$batterid <- NULL
cleanTest$pitcherid <- NULL
#Save cleanTest$pitchid in case we need it later, then delete from df

cleanTest_pitchID = cleanTest$pitchID
cleanTest$pitchID <- NULL


drops <- c("gameid", "event", "pitcher_name", "batter_name", "batterAvgSandM", "count.x", "count.y", 
           "Plate_X", "Plate_Z", "SZ_top", "SZ_Bottom", "Plate_Y", "Init_Pos_Y", "FB")
cleanTest=cleanTest[,!names(cleanTest) %in% drops]

#something weird with cleanTest$gameid. Not dropped.
cleanTest <- cleanTest[,-1]


rm(avgFB, batterAvgSandM, lgAvgFB,  pitchCount, test, train, drops, batterWtAvg, decay, outside, isStrike, relativeZsq, avgFBVel, avgSandM)
