lfpr <- read.csv("laborf.csv",header = TRUE)
lfpr <- lfpr[,-1]

LFP<-lfpr$LFP
PEU <- (lfpr$PEU)
logPEU <- log(lfpr$PEU)
UPG<-lfpr$UPG
FLP<- lfpr$FLP
SVA<-lfpr$SVA
TFA <-lfpr$TFA
GDPI<-lfpr$GDPI
logGDPI<-log(lfpr$GDPI)
GDPC<-lfpr$GDPC
logGDPC<-log(lfpr$GDPC)
GDPG<-lfpr$GDPG

#get SDs
sd(LFP)
sd(PEU)
sd(UPG)
sd(FLP)
sd(SVA)
sd(TFA)
sd(GDPI)
sd(GDPC)
sd(GDPG)

#correlation matrix
cor(lfpr)

#plot of all the explanatory variables to see if any tranformations are appropriate
par(mfrow = c(2,2))
plot(FLP, LFP, main = "LFP vs FLP") 
plot(UPG, LFP, main = "LFP vs. UPG")
plot(TFA, LFP, main = "LFP vs. TFA")
plot(GDPG, LFP, main = "LFP vs. GDPG")
par(mfrow = c(1,2))
plot(PEU,  LFP, main = "LFP vs. PEU")
plot(logPEU,  LFP, main = "LFP vs. log of PEU", xlab = "log(PEU)")
par(mfrow = c(1,2))
plot(GDPC, LFP, main = "LFP vs. GDPC")
plot(logGDPC, LFP, main = "LFP vs. log of GDPC", xlab = "log(GDPC)")
par(mfrow = c(2,2))
plot(GDPI, LFP, main = "LFP vs. GDPI")
plot(logGDPI, LFP, main = "LFP vs. log of GDPI", xlab = "log(GDPI)")
plot(SVA, LFP, main = "LFP vs. SVA")

#the new data set that we want to use with the log transformed variables
lfpr.transform = data.frame(cbind(LFP, FLP, logPEU, UPG, SVA, TFA, logGDPI, logGDPC, GDPG))

#this is a summary and model for the data with all variables before any
#log transformation
summary(lfpr)
model1 <- lm(LFP~., data = lfpr)
summary(model1)
residSE1=sqrt(sum(model1$resid^2)/model1$df.resid)
plot(FLP, model1$resid)
plot(PEU, model1$resid)
plot(UPG, model1$resid)
plot(SVA, model1$resid)
plot(TFA, model1$resid)
plot(GDPI, model1$resid)
plot(GDPC, model1$resid)
plot(GDPG, model1$resid)
plot(model1$fitted,model1$resid)
abline(h=2*residSE1)
abline(h=-2*residSE1)
qqnorm(model1$residuals)
qqline(model1$residuals, col="red")

#this is a summary and model for the data with all the variables with the
#appropriate log transformations
model2 <- lm (LFP~., data = lfpr.transform)
summary(model2)
residSE2=sqrt(sum(model2$resid^2)/model2$df.resid)
plot(FLP, model2$resid)
plot(logPEU, model2$resid)
plot(UPG, model2$resid)
plot(SVA, model2$resid)
plot(TFA, model2$resid)
plot(logGDPI, model2$resid)
plot(logGDPC, model2$resid)
plot(GDPG, model2$resid)
plot(model2$fitted,model2$resid)
abline(h=2*residSE2)
abline(h=-2*residSE2)

qqnorm(model2$residuals)
qqline(model2$residuals, col="red")

library(leaps)
exh <- regsubsets(LFP~., data=lfpr.transform) #exhaustive method
exh.sum = summary(exh)
which.max(exh.sum$adjr2) #6 
which.min(exh.sum$cp) #3 

backward<- regsubsets(LFP~., data=lfpr.transform, method = "backward") 
back.sum = summary(backward)
which.max(back.sum$adjr2) #6 
which.min(back.sum$cp) #4

#model with 3 variables: GDP, FLP, UPG
model.3vars<- lm(LFP~UPG+FLP+GDPG, data = lfpr.transform)
summary(model.3vars)
extractAIC(model.3vars)
#model with 4 variables: FLP, UPG, TFA, logGDPC
model.4vars<- lm(LFP~FLP+UPG+TFA+logGDPC, data = lfpr.transform)
summary(model.4vars)
extractAIC(model.4vars)
#model with 5 variables: FLP, UPG, TFA, logGDPC, GDPG
model.5vars<- lm(LFP~FLP+UPG+TFA+logGDPC+GDPG, data = lfpr.transform)
summary(model.5vars)
extractAIC(model.5vars)
#model with 6 variables: FLP, logPEU, UPG, TFA, logGDPC, GDPG
model.6vars<- lm(LFP~FLP+logPEU+UPG+TFA+logGDPC+GDPG, data = lfpr.transform)
summary(model.6vars)
extractAIC(model.6vars) #highest among the models

plot(model.3vars)  #qq-plot looks good
plot(model.4vars)
plot(model.6vars) 

#below are the residual plots for the 3 models. they are all pretty much the same so just focus
#on the 3 variable model
#residual plots for 3 variables
residSE.3vars=sqrt(sum(model.3vars$resid^2)/model.3vars$df.resid)
plot(FLP, model.3vars$resid)
abline(h=2*residSE.3vars)
abline(h=-2*residSE.3vars)
plot(UPG, model.3vars$resid)
abline(h=2*residSE.3vars)
abline(h=-2*residSE.3vars)
plot(GDPG, model.3vars$resid)
abline(h=2*residSE.3vars)
abline(h=-2*residSE.3vars)
plot(model.3vars$fitted,model.3vars$resid)
abline(h=2*residSE.3vars)
abline(h=-2*residSE.3vars)

qqnorm(model.3vars$residuals)
qqline(model.3vars$residuals, col="red")

#residual plots for 4 variables
residSE.4vars=sqrt(sum(model.4vars$resid^2)/model.4vars$df.resid)
plot(FLP, model.4vars$resid)
plot(UPG, model.4vars$resid)
plot(TFA, model.4vars$resid)
plot(logGDPC, model.4vars$resid)
plot(model.4vars$fitted,model.4vars$resid)
abline(h=2*residSE.4vars)
abline(h=-2*residSE.4vars)

#residual plots for 6 variables 
residSE.6vars=sqrt(sum(model.6vars$resid^2)/model.6vars$df.resid)
plot(FLP, model.6vars$resid)
plot(logPEU, model.6vars$resid)
plot(UPG, model.6vars$resid)
plot(TFA, model.6vars$resid)
plot(logGDPC, model.6vars$resid)
plot(GDPG, model.6vars$resid)
plot(model.6vars$fitted,model.6vars$resid)
abline(h=2*residSE.6vars)
abline(h=-2*residSE.6vars)

diag = ls.diag(model.6vars)
n = 123
p = 7
dfits.test = 2*(sqrt(p/n))
dfits.test

x = 1:123
plot(x,diag$dfits, xlab = "Observations")
abline(a= dfits.test, b=0, col = 2)
which (diag$dfits > dfits.test) # 3  99 101 102 118

dcooks = 4/n
plot(x,diag$cooks, xlab = "Observations")
abline(a= dcooks, b=0, col=2)
which (diag$cooks > dcooks) #3  16  81  84  99 101 102 118 123

#Identify countries with sig values in both: 3, 99 101 102 118
c <- read.csv("laborf.csv",header = TRUE)
a = c[3,] #Angola
b = c[99,] #St. Lucia
d = c[101,] #Samoa
e = c[102,] #Saudi Arabia
f = c[118,] #United Arab Emirates

       
#Cross-validation for 4 models
n <- nrow(lfpr.transform)
set.seed(1)
id.subset1 <- sort(sample(1:n, round(n/2), replace=FALSE))
training.set <- lfpr.transform[id.subset1,]
holdout.set <- lfpr.transform[-id.subset1,]

model3.subset <- lm(LFP~UPG+FLP+GDPG, data=training.set)
model3.pred <- predict(model3.subset, holdout.set)
model3.err <- sqrt(sum((holdout.set$LFP - model3.pred)^2)/length(model3.pred))
model3.err #2.9716

model4.subset <- lm(LFP~FLP+UPG+TFA+logGDPC, data=training.set)
model4.pred <- predict(model4.subset, holdout.set)
model4.err <- sqrt(sum((holdout.set$LFP - model4.pred)^2)/length(model4.pred))
model4.err #2.973

model5.subset <- lm(LFP~FLP+UPG+TFA+logGDPC+GDPG, data=training.set)
model5.pred <- predict(model5.subset, holdout.set)
model5.err <- sqrt(sum((holdout.set$LFP - model5.pred)^2)/length(model5.pred))
model5.err  #2.944

model6.subset <- lm(LFP ~ FLP + logPEU + UPG + TFA + logGDPC + GDPG, data=training.set)
model6.pred <- predict(model6.subset, holdout.set)
model6.err <- sqrt(sum((holdout.set$LFP - model6.pred)^2)/length(model6.pred))
model6.err #3.028

