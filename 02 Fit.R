set.seed(42)
index <- sample(1:nrow(cleanTrain), nrow(cleanTrain)/5, replace=FALSE)
testing <- cleanTrain[index,]
training <- cleanTrain[-index,]
rm(index)

save(testing, file="testing.rda")

actual=testing$swingingstrike

#library("caret")

#once you have model, can predict swStrike rate from a pitch's characteristics.  
# Show on Blue Jays pitchers their avg FB, CH, SL,... predicted swStrike
# Then imagine a coaching change (release point higher, spin rate lower, whatever, and get new swStrike)



# Root Mean Square Error
RMSE <- function(predicted, actual){
  (mean((predicted - actual)^2))^0.5
}

# R^2
Rsquare <- function(predicted, actual){
  mu <- mean(actual)
  rse <- mean((predicted - actual)^2)/mean((mu - actual)^2)
  1-rse
}

#LogLoss
LogLoss <- function(yhat, y){
  n=length(y)
  epsilon = 0.0000000000001
  yhat = pmin(pmax(0+epsilon, yhat),1-epsilon)
  
  summands=rep(0,n)
  for (i in 1:n){
    summands[i]=y[i] * log(yhat[i])+(1-y[i])*log(1-yhat[i])
  }
  (-1/n) * sum(summands)
}


#Try some fast to fit models, to get a sense

library("earth") #MARS
earthFit <- earth(swingingstrike ~ ., data=training)
earthPredict <- pmin(pmax(predict(earthFit, testing),0),1)
RMSE(earthPredict, actual) #.364
Rsquare(earthPredict, actual) #.188
LogLoss(earthPredict, actual) #.458


glmFit <- glm(swingingstrike ~ ., training, family=binomial())
glmPredict <- predict(glmFit, testing, type="response")
RMSE(glmPredict, actual) #.362
Rsquare(glmPredict, actual) #.197
LogLoss(glmPredict, actual) #.420

#KN seems predictable entirely by spin rate.  Try it
KNtest=which(testing$pitchtype=="KN")
LogLoss(glmPredict[KNtest],actual[KNtest]) #.54 for KN as part of big model

KNtrain=which(training$pitchtype=="KN")

KN_lmFit <- lm(swingingstrike ~ SpinRate, training[KNtrain,])
KN_lmPredict <- predict(KN_lmFit, testing[KNtest,])

LogLoss(KN_lmPredict,actual[KNtest]) #.49
# better prediction for KN by pulling it out of main model


#try taking advantage of observed patterns in the data by regressing on interaction variables & filtered ranges
# formula for regression lm(y ~ x*(x < 15) + x*(x > 15))
# knuckleball and spin
# fb < 1700 rpm, fb >=1700 rpm
# fb < 88mph & fb > 88 mph
# CB < 80 & cb > 80


#paste(names(training), collapse ="+")
glmFitPlus <- glm(swingingstrike ~ yearid+throws+bats+pitchtype+Init_Vel+Init_Pos_X+Init_Pos_Z+Init_Vel_X+Init_Vel_Y+
                Init_Vel_Z+Init_Accel_X+Init_Accel_Y+Init_Accel_Z+Plate_Vel+Break_X+Break_Z+SpinRate+batterWtAvg+
                throwsBats+isStrike+relativeZsq+outside+outside2+pitcherWtAvgFB+velDiff+
                SpinRate*(pitchtype=="KN")+SpinRate*Init_Vel*(pitchtype=="FB")+
                (Init_Vel < 88)*(pitchtype=="FB")+(Init_Vel >= 88)*(pitchtype=="FB")+
                (Init_Vel < 78)*(pitchtype=="CB")+(Init_Vel >= 78)*(pitchtype=="CB")+  
                (SpinRate < 1700)*(pitchtype=="FB")+(SpinRate >= 1700)*(pitchtype=="FB"),
              training, family=binomial())

glmPredictPlus <- predict(glmFitPlus, testing, type="response")
RMSE(glmPredictPlus, actual) #.362
Rsquare(glmPredictPlus, actual) #.197
LogLoss(glmPredictPlus, actual) #.42

#literally no difference!




library(rpart)
dtreeFit <- rpart(swingingstrike ~ ., data=training, method="anova", control=rpart.control(cp=0.0001, xval=10))
dtreePredict <- predict(dtreeFit, testing)

RMSE(dtreePredict, actual) #.36
Rsquare(dtreePredict, actual) #.191
#save(dtreeFit, file="dtreeFit.rda")
LogLoss(dtreePredict, actual) #.42


dTreeFitPruned = prune(dtreeFit, dtreeFit$cptable[which.min(dtreeFit$cptable[,"xerror"]),"CP"])


# try simple ensemble

ePredict = (dtreePredict + earthPredict + glmPredictPlus) / 3
RMSE(ePredict, actual) #.36
Rsquare(ePredict, actual) #.20
LogLoss(ePredict, actual) #.418

save(ePredict, file="ePredict.rda")

# OK, make submission.  Run predictors on cleanTest and blend

submitEarth = pmin(pmax(predict(earthFit, cleanTest),0),1)
submitGLMPlus = predict(glmFitPlusPlus, cleanTest, type="response")
submitDtree <- predict(dtreeFit, cleanTest)
submitEnsemble = (submitEarth + submitGLMPlus + submitDtree) / 3


submit = as.data.frame(cbind(pitchID=cleanTest_pitchID,swingingstrike=submitEnsemble), stringsAsFactors = FALSE)
submit$swingingstrike = as.numeric(submit$swingingstrike)

#a handful of pitches got left out. predict league average for them.

overCleanedPitches = setdiff(test$pitchID, cleanTest_pitchID)
overallAverage = sum(train$swingingstrike) / nrow(train)
toAdd = data.frame(pitchID = overCleanedPitches, swingingstrike = overallAverage)
submit = rbind(submit, toAdd)

#should put rows in original order of test
submit=submit[match(test$pitchID, submit$pitchID),]
write.csv(submit, file="CodyStumpoSubmission2.csv")

