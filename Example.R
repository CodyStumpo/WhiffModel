example = cleanTest %>% 
  filter (pitchtype=="FB" & throws=="R" & bats=="R" & Init_Vel >91 & Init_Vel < 93 & SpinRate > 2100 & SpinRate < 2200) 
example$yearid=2015

means = colMeans(example[,c(5:18, 22:25)])

for (i in 5:18) example[,i] = means[i-4]
for (i in 22:25) example[,i] = means[i-7]

#you'd want to do this with more realistic physics as affects initila velocity and accel in the 3 dimensions, and break so the pitch gets to where I'm saying it's going

#We have 2600 examples above
#let's break X & Z into 50 x 50 grid.  X going from -2 to 2 and Z going from 0 to 5

X = seq(from=-2, to =2, length.out=50 )
Z = seq(from=0, to =5, length.out=50 )
X2 = X^2
#typical middle of the strike zone is 1.8 ft
midZ = median(train$SZ_Top - train$SZ_Bottom)
relZ2 = (Z - midZ)^2

example = example[c(1:2500),]
example$inside=rep(X, 50)
example$inside2=rep(X2, 50)
example$relativeZsq=sort(rep(Z, 50))
example$relativeZsq=(example$relativeZsq-midZ)^2

example$isStrike=mapply(isStrike, example$inside, sort(rep(Z, 50)), 3.5, 1.5)
exDTree = predict(dtreeFit, example)
exGLM <- predict(glmFitPlus, example, type="response")
exEarth <- pmin(pmax(predict(earthFit, example),0),1)

example$swingingstrike = as.numeric((exDTree + exGLM + exEarth) / 3)

save(example, file="example.rda")

qplot(example$inside, sort(rep(Z, 50)), colour=example$swingingstrike) + scale_colour_gradient(limits=c(0, 1), low="red", high="green") +
  geom_rect(xmin = -8.5/12, xmax = 8.5/12,   ymin = 1.5, ymax = 3.5, 
           fill=NA, linetype=1, color="black", size=1) +
  labs(title = "Modeled 92mph FB @ 2150 rpm from RHP to RHB", x="Inside (catcher's POV)", y = "Pitch Height", colour="Whiff")

