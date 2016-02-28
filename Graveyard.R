#GRAVEYARD - ideas tried and discarded


#explore trend by year
train %>% group_by(yearid) %>% summarise(yearMean=mean(swingingstrike))
#does trend up from 19 in 2011 to 21 in 2015. Could account for this
#not going to use right now

#################

inZoneZ = function(top,bottom,Z) {
  layer=top-bottom
  (Z-bottom)/layer}
# # % of strike zone the pitch is high(>1) or low(<0)
# #this could be infinity if strike zone layer = 0. Which it is at least once.
# 
# #Force bottom to be within 1 & 2, top to be within 3 & 4 (like 99.9% of the data)
# 
PitchInZoneZ = inZoneZ(pmin(pmax(cleanTrain$SZ_Top,3),4), pmin(pmax(cleanTrain$SZ_Bottom,1),2), cleanTrain$Plate_Z)
cleanTrain = cbind(cleanTrain, PitchInZoneZ)

inZoneX = function(X) {
  width=17/12
  (X)/width
}
# # % of strike zone the pitch is catchers' left(<-1) or right(>1)
# 
PitchInZoneX = inZoneX(cleanTrain$Plate_X)
cleanTrain = cbind(cleanTrain, PitchInZoneX)


##################


avgOffSpeed <- train %>%
  select(pitcherid, pitchtype, Init_Vel) %>%
  filter(pitchtype != 'FB') %>%
  group_by(pitcherid) %>%
  summarise(OS=mean(Init_Vel))


lgAvgOffSpeed = train %>%
  select(pitchtype, Init_Vel) %>%
  filter(pitchtype!='FB') %>%
  summarise(OS=mean(Init_Vel))

lgAvgDiff = (lgAvgFB-lgAvgOffSpeed)[[1]]

velDiff = avgFB %>%
 full_join(avgOffSpeed,by = 'pitcherid') %>%
 full_join(pitchCount, by='pitcherid')

velDiff$delta = velDiff$FB - velDiff$OS
pitcherWtAvgDelta = decay(velDiff$count)*lgAvgDiff + (1-decay(velDiff$count))*velDiff$delta
velDiff=cbind(velDiff, pitcherWtAvgDelta)

velDiff$pitcherWtAvgDelta[is.na(velDiff$pitcherWtAvgDelta)] = lgAvgDiff

cleanTrain <- cleanTrain %>% left_join(velDiff, by="pitcherid")

plot(PitchInZoneX[1:1000],PitchInZoneZ[1:1000], col=ifelse(cleanTrain$swingingstrike[1:1000]==1 ,"red","black"))


#drop #, "FB", "OS", "count", "delta")
#rm(train, PitchInZoneX, PitchInZoneZ)


#############################

#idea of make initial guess then fit to residual 

cleanTrain$surprise <- cleanTrain$swingingstrike - cleanTrain$batterWtAvg
cleanTrainIndex = cleanTrain[,1]
cleanTrain=cleanTrain[,-1]
drops <- c("swingingstrike")
cleanTrain=cleanTrain[,!names(cleanTrain) %in% drops]
#get rid of pitchID, swingingstrike

actual <- testing$surprise+testing$batterWtAvg


#####################

#Try model based purely on univariate plots and instinct.  Need to at least beat this.  

simplePredictor = function (type, speed, spin) {
  x=.2
  if (type=="CH" || type=="SL") x=.3
  if (type=="FB" && speed < 88) x=.2 - (speed-80)*(.09/8) #linear with .2 at 80, .11 at 88
  if (type=="FB" && speed >= 88 && speed < 98) x=.11 + (speed-88)*(.14/10) #linear with .11 at 88, .25 at 98, 
  if (type=="FB" && speed >= 98) x= .25 + (speed-98)*.07/5 #.25 at 98,   .32 at 103
  if (type=="CB" && speed < 78) x=.25
  if (type=="CB" && speed >= 78) x=.25 + (speed-78)*.25/8 #linear with .25 at 78, .5 at 85
  if (type=="KN") x=.1 + spin*.3/2000 #linear with .1 at 0, .4 at 2000
  if (type=="SC") x=.2
  x
}

simplePredict =  pmin(pmax(simplePredictor(testing$pitchtype, testing$Init_Vel, testing$SpinRate),0),1)
RMSE(simplePredict, actual) #.43
Rsquare(simplePredict, actual) #-.13?
LogLoss(simplePredict, actual) #1.45





library("randomForest")
# rfFit <- randomForest(surprise ~ ., data=training, ntree=200)

#should do with foreach package or parallelRandomForest package
#library(ParallelForest)
#fforest = grow.forest(surprise ~ ., data=training )
# library(doParallel)
# cl <- makeCluster(3)
# doParallel::registerDoParallel(cl)
# library(foreach)
# rf <- foreach(ntree=rep(10, 4), .combine=combine, .packages='randomForest') %dopar%
#   + randomForest(surprise ~ ., training, ntree=ntree)




# library("mgcv")
# 
# if (detectCores()>1) {
#   cl <- makeCluster(detectCores()-1)
# } else cl <- NULL
# 
# bamFit <- bam(surprise ~ ., method="GCV.Cp", data=training, family=binomial(link="logit"), cluster=cl)
#bam needs specific columns?  Whatis s(), te(), ti(), t2().  Also, works with factors?



library("gbm")

gbmPredict <- predict(gbmFit.500.5.9, testing)
gbmFit.500.5.9 <- gbm(surprise ~ ., training, distribution="gaussian", n.trees=500,
                      cv.folds=5, interaction.depth=9)
save(gbmFit.500.5.9, file="gbmFit.500.5.9.rda")
RMSE(gbmPredict, testing$surprise) #.38
Rsquare(gbmPredict, testing$surprise) #.09




# how about normalizing variables, bagging, ensemble,...


normalize = function (x){
  m=mean(x)
  s=sd(x)
  (x-m)/s}


#could try normalizing all variables.  Want actually to preserve training m,sd to apply that to test before predict
normCleanTrain = cleanTrain
#for (i in 5:22) normCleanTrain[,i] = normalize(normCleanTrain[,i])
m=colMeans(cleanTrain[,c(5:22)])
s=apply(cleanTrain[,c(5:22)], 2, sd )


library(doParallel)
cl <- makeCluster(3)
doParallel::registerDoParallel(cl)
library (foreach)

bagging<-function(training,testing,length_divisor=4,iterations=15)
{
  predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
    train_pos<-1:nrow(training) %in% training_positions
    lm_fit<-earth(surprise ~ .,data=training[train_pos,])
    predict(lm_fit,newdata=testing)
  }
  rowMeans(predictions)
}

x=bagging(training, testing)
xFinal <- pmin(pmax(x+testing$batterWtAvg,0),1)
RMSE(xFinal, actual) #.36
Rsquare(xFinal, actual) #.18


#bagging on various methods doesn't do much for them.

# try simple ensemble

ePredict = (.19 * dtreePredict + .18 * xfinal) / (.19 + .18)
eFinal <- pmin(pmax(ePredict+testing$batterWtAvg,0),1)
RMSE(eFinal, actual) #.36
Rsquare(eFinal, actual) #.195


#helps a very little bit


# better to have slider break in X or Z plane?  No clear pattern.
example2 = cleanTrain %>% 
  filter (pitchtype=="SL" & throws=="R" & bats=="R") 



SLwhiffByBreakBucket=example2 %>%
  select(Break_X, Break_Z, swingingstrike) %>%
  group_by(Break_X=round(Break_X,1), Break_Z=round(Break_Z,1)) %>%
  summarize(whiff = mean(swingingstrike), count=n()) %>%
  filter(count > 20)


qplot(SLwhiffByBreakBucket$Break_X, SLwhiffByBreakBucket$Break_Z, colour=SLwhiffByBreakBucket$whiff) +
  scale_colour_gradient(limits=c(0, 1), low="red", high="green") 
