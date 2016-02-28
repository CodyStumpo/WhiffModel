### exploratory data analysis


library("readr")
library("dplyr")
train=read_delim(file = "TrainData.txt", delim = "|")


#swStrike by pitchtype

train %>%
  select (pitchtype, swingingstrike) %>%
  group_by (pitchtype) %>%
  summarise(swstr=mean(swingingstrike)) %>%
  arrange (swstr)

# pitchtype     swstr
# (chr)     (dbl)
# 1        FB 0.1452065
# 2        SC 0.1848739
# 3        KN 0.2160977
# 4        CB 0.2898656
# 5        CH 0.2920714
# 6        SL 0.3130217


#how about by bats, throws combination
train %>%
  select (bats, throws, swingingstrike) %>%
  group_by (bats, throws) %>%
  summarise(swstr=mean(swingingstrike)) %>%
  arrange (swstr)

# bats throws     swstr
# (chr)  (chr)     (dbl)
# 1     L      R 0.1895500
# 2     R      L 0.1962648
# 3     R      R 0.2164693
# 4     L      L 0.2273185

train$event <- factor(train$event)
levels(train$event)
# everything here is a swing. No takes.  
# So IF you can get someone to swing at the pitch, will they miss it.  
# practically, should separate inzone advice from out of zone advice.

#highest whiff rates
train %>% 
  select(pitcherid, pitcher_name, swingingstrike) %>% 
  group_by(pitcherid) %>% 
  summarise(name = max(pitcher_name), count=n(), whiff=mean(swingingstrike)) %>% 
  filter(count > 50) %>%
  select(name, whiff) %>%
  top_n(5, whiff) 


# name     whiff
# (chr)     (dbl)
# 1 Garcia, Christian 0.4531250
# 2  Cabrera, Alberto 0.3694268
# 3    Kimbrel, Craig 0.3488996
# 4  Chapman, Aroldis 0.3752688
# 5    Martin, Rafael 0.3678161




library(ggplot2)


### inspect whiff by Velocity

whiffByTypeAndVelBucket=train %>%
  select(Init_Vel, pitchtype, swingingstrike) %>%
  group_by(pitchtype, mph=round(Init_Vel,1)) %>%
  summarize(whiff = mean(swingingstrike), count=n()) %>%
  filter (count > 50)


ggplot(whiffByTypeAndVelBucket)+geom_point(aes(mph, whiff, color=pitchtype), size=2)

# Right away we get very strong relationship.   Fastball by mph, curveball by mph, slider/changeup, other (KN/SC)


#test on prediction to see if plot looks about right.  It does! 
whiffByTypeAndVelBucket=cbind(testing,glmPredictPlus) %>%
  select(Init_Vel, pitchtype, glmPredictPlus) %>%
  group_by(pitchtype, mph=round(Init_Vel,1)) %>%
  summarize(whiff = mean(glmPredictPlus), count=n()) %>%
  filter (count > 50)


ggplot(whiffByTypeAndVelBucket)+geom_point(aes(mph, whiff, color=pitchtype), size=2)

#use blended model

modelWhiffByTypeAndVelBucket=testing
modelWhiffByTypeAndVelBucket$swingingstrike = as.vector(ePredict)
modelWhiffByTypeAndVelBucket = modelWhiffByTypeAndVelBucket %>%
  select(Init_Vel, pitchtype, swingingstrike) %>%
  group_by(pitchtype, mph=round(Init_Vel,1)) %>%
  summarize(whiff = mean(swingingstrike), count=n()) %>%
  filter (count > 50)


ggplot(modelWhiffByTypeAndVelBucket)+geom_point(aes(mph, whiff, color=pitchtype), size=2)


#make indicator variables for these segments  
#FB < 88
#FB > 88
#CB >78
#CB < 78
#SL or CH

#pitchers fb%


###### Now inspect whiff by spin

whiffByTypeAndSpinBucket=train %>%
  select(SpinRate, pitchtype, swingingstrike) %>%
  group_by(pitchtype, rpm=round(SpinRate,-2)) %>%
  summarize(whiff = mean(swingingstrike), count=n()) %>%
  filter (count > 50)


ggplot(whiffByTypeAndSpinBucket)+geom_point(aes(rpm, whiff, color=pitchtype), size=2)

#for knucklers, spin rate is great predictor


#test on prediction to see if plot looks about right.  It does! 
whiffByTypeAndSpinBucket=cbind(testing,glmPredictPlus) %>%
  select(SpinRate, pitchtype, glmPredictPlus) %>%
  group_by(pitchtype, rpm=round(SpinRate,-2)) %>%
  summarize(whiff = mean(glmPredictPlus), count=n()) %>%
  filter (count > 50)


ggplot(whiffByTypeAndSpinBucket)+geom_point(aes(rpm, whiff, color=pitchtype), size=2)



#### whiff by Z


whiffByTypeAndZBucket=train %>%
  select(Plate_Z, pitchtype, swingingstrike) %>%
  group_by(pitchtype, Z=round(Plate_Z,1)) %>%
  summarize(whiff = mean(swingingstrike), count=n()) %>%
  filter (count > 50)


ggplot(whiffByTypeAndZBucket)+geom_point(aes(Z, whiff, color=pitchtype), size=2)

ggplot(whiffByTypeAndZBucket)+geom_point(aes((Z-mean(train$Plate_Z))^2, whiff, color=pitchtype), size=2)
#more linear response if center Z around mean and square it.  
#Maybe put Z in the first place like X, inches above/below center of zone, then just square it



whiffByTypeAndXBucket=train %>%
  select(Plate_X, pitchtype, swingingstrike) %>%
  group_by(pitchtype, X=round(Plate_X,1)) %>%
  summarize(whiff = mean(swingingstrike), count=n()) %>%
  filter (count > 50)


ggplot(whiffByTypeAndXBucket)+geom_point(aes(X, whiff, color=pitchtype), size=2)


outside = function(X,bats) {
X * ifelse(bats =="L",-1,1)
}

train$outside <- outside(train$Plate_X, train$bats)



whiffByTypeAndOutsideBucket=train %>%
  select(outside, pitchtype, swingingstrike) %>%
  group_by(pitchtype, outside=round(outside,1)) %>%
  summarize(whiff = mean(swingingstrike), count=n()) %>%
  filter (count > 50)


ggplot(whiffByTypeAndOutsideBucket)+geom_point(aes(outside, whiff, color=pitchtype), size=2)

#so keep data as inches away from middle of plate.  Interact with LL, RR, LR, RL
#make a more linear variable to whiff, outside^2
ggplot(whiffByTypeAndOutsideBucket)+geom_point(aes(outside^2, whiff, color=pitchtype), size=2)

dTreeFitPruned2 = prune(dtreeFit, .001)
plot(dTreeFitPruned2, uniform = TRUE)
text(dTreeFitPruned2, use.n=TRUE, all=TRUE, cex=.6)

# 20%
# If not-low pitch, whiff goes to 15%
#  If high pitch, whiff goes to 13%, else 25%
#   If high and high-contact batter, whiff goes to 10%
#     If high and high-contact batter, and pitchtype = CB or FB, whiff goes to 9% else 20%
#   If high and low-contact batter, whiff goes to 17%
#    If high and low-contact batter and outside -> 15%, else 28%
#    If hgih and low-contact batter and outside and FB/CB -> 14% else 20%
# If low pitch, whiff goes to 51%
#  If very low pitch, whiff goes to 75%, else 43%
#   If very low pitch and pitchtype = FB, KN, SC -> 55% else (SL, CH, CB) -> 81%
#    If very very low pitch and pitchtype = SL, CH, CB -> 91%, else 74%
#   If pretty low pitch and pitchtype = FB, KN, SC -> 32% else (SL,CH,CB) -> 52%
#    If pretty low pitch and pitchtype = FB, KN, SC and high-contact batter -> 25% else 41%
#   If pretty low pitch and pitchtype = (SL,CH,CB) and outside, 49%, else 71% 
#    If pretty low pitch and pitchtype = (SL,CH,CB) and outside and high-contact batter, 41% else 57%


#made a version of cleanTrain in it with Plate_Z and batterWtAvg, so this works
x=cleanTrain %>% select(swingingstrike, Plate_Z, pitchtype, batterWtAvg) %>%
  filter(Plate_Z > 3 & batterWtAvg < .15) %>%
  group_by(pitchtype, Z=round(Plate_Z,1), batter= round(batterWtAvg, 2)) %>%
  summarize(whiff = mean(swingingstrike), count=n()) %>%
  filter (count > 20)

#summary(x$whiff)
# yes, high pitches to high-contact batters have ~10% whiff rate
qplot(x$Z, x$batter, colour=x$whiff) + scale_colour_gradient(limits=c(0, 1), low="green", high="red")
