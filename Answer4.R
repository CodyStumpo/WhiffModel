library("readr")
library("dplyr")
train=read_delim(file = "TrainData.txt", delim = "|")


# 4.In addition to the 3 things above, we’d like you to create some summary data on the entirety of the training data set.  
# For pitchers with a minimum of 50 FB in the training data set, 
# find the top five fastest FB by init_vel and list 
# the pitch, pitcher, number of pitches thrown, avg init_vel, avg X break and avg Z break for each of the pitchers in the top five.  
# Repeat this for pitchers who have thrown 50+ CB, SL and CH in the training set 
# (so 4 separate result sets w/5 pitchers in each result set).  
# Please save this result set in a csv file. 

#find top 5 fastballs
fastest5 = function(pt) {
train %>% 
  select(pitcherid, pitcher_name, Init_Vel,pitchtype, Break_X, Break_Z) %>% 
  filter(pitchtype == pt & pitcherid != 175) %>% #the Harry Doyle exception
  group_by(pitcherid) %>% 
  summarise(pitchtype=max(pitchtype), name=max(pitcher_name), count=n(), avgVel=mean(Init_Vel),avgBreakX = mean(Break_X), avgBreakZ=mean(Break_Z)) %>% 
  filter(count > 50) %>%
  select(pitchtype, name, count, avgVel, avgBreakX, avgBreakZ) %>%
  top_n(5, avgVel) %>%
  arrange(avgVel)
}

fb5=fastest5("FB")
cb5=fastest5("CB")
sl5=fastest5("SL")
ch5=fastest5("CH")

answer4=rbind(fb5, cb5, sl5, ch5)
write.csv(answer4,file = "CodyStumpoAnswer4.csv", row.names = FALSE)

# Looked into suspicious outlier...
# Trying to trick me! Harry Doyle's Changeup shows up at 95.  
# hdoyle = train %>% filter(pitcherid==175) shows several 100 mph CH.  Faster than FB. 
# Harry Doyle is the announcer from Major League (cute) 
# Also there is no Harry Doyle in historical records