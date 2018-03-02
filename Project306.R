load("movies.Rdata")

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(leaps)
# Remove NA values.
complete_index=complete.cases(movies)  
movies=movies[complete_index,]

# Transform thtr_rel_month into two binary variables: oscar_season and summer_season.
movies <- mutate(movies,oscar_season=as.factor(ifelse(thtr_rel_month %in% c(10, 11, 12), 'yes', 'no')))
movies <- mutate(movies,summer_season=as.factor(ifelse(thtr_rel_month %in% c(5, 6, 7, 8), 'yes', 'no')))

# Create the model dataset with variables of interests. 
useful_col=c(13,2,3,4,5,15,16,18,20,21,22,23,24,33,34)
dataset=data.frame(movies[,useful_col])

# Scatter plot for examining the homoscedasticity. (numercial variables) (No need to transform)
par(mfrow = c(2,3))
scatter_runtime=plot(dataset$runtime,dataset$imdb_rating,xlab="runtime",ylab="imdb_rating")
scatter_runtime=plot(dataset$critics_score,dataset$imdb_rating,xlab="crities_score",ylab="imdb_rating")
scatter_runtime=plot(dataset$audience_score,dataset$imdb_rating,xlab="audience_score",ylab="imdb_rating")

scatter_log_runtime = plot(dataset$runtime,log(dataset$imdb_rating), xlab = "runtime",ylab = "log(imdb_rating)")
scatter_log_criticsScore = plot(dataset$critics_score,log(dataset$imdb_rating), xlab = "critics_score", ylab = "log(imdb_rating)")
scatter_log_audience = plot(dataset$audience_score,log(dataset$imdb_rating), xlab = "audience_score",ylab= "log(imdb_rating)")

# Box-plot for categorical variables.
par(mfrow = c(2,3))
boxplot(imdb_rating~title_type,data=dataset,xlab="title_type",ylab="imdb_rating")
boxplot(imdb_rating~mpaa_rating,data=dataset,xlab="mpaa_rating",ylab="imdb_rating")
boxplot(imdb_rating~critics_rating,data=dataset,xlab="critics_rating",ylab="imdb_rating")

boxplot(log(imdb_rating)~title_type,data=dataset,xlab="title type",ylab="log(imdb_rating)")
boxplot(log(imdb_rating)~mpaa_rating,data=dataset,xlab="mpaa_rating",ylab="log(imdb_rating)")
boxplot(log(imdb_rating)~critics_rating,data=dataset,xlab="critics_rating",ylab="log(imdb_rating)")

# Create histograms of the movie rating scores for selecting response variable
g1 <- ggplot(data=dataset, aes(x=imdb_rating)) + 
  geom_histogram(binwidth=0.5, fill="red") +
  xlab("IMDB Ratings")

g2 <- ggplot(data=dataset, aes(x=critics_score)) + 
  geom_histogram(binwidth=5, fill="blue") +
  xlab("Critics Scores")

g3 <- ggplot(data=dataset, aes(x=audience_score)) + 
  geom_histogram(binwidth=5, fill="purple") +
  xlab("Audience Scores")

grid.arrange(g1, g2, g3, nrow=1,
             top="Distributions of Numerical Rating Variables")


#boxplot y/n
#To get the sumamry statistics.
moviesnew <- dataset %>% gather('binaryVariables','show',best_pic_win:summer_season)
ggplot(data=moviesnew,aes(x=binaryVariables, y= imdb_rating,fill=show))+geom_boxplot()
numerical=data.frame(dataset[,c(1,4,7,8)])
numerical=mutate(numerical,log_imdb_rating=log(imdb_rating))
summary(numerical)
summary(dataset$genre)
cor(numerical)

#plot qq plot with outliers
par(mfrow = c(2,1))
fullmodel1=lm(imdb_rating~., data=dataset)
qqnorm(fullmodel$residuals)
qqline(fullmodel$residuals,col="red")
#plot residual with outliers
plot(fullmodel$residuals, xlab ="Predicted with outliers")

#remove outliers 
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
qa = quantile(dataset$imdb_rating)[2] - 1.5*IQR(dataset$imdb_rating) #3.8
outlierReplace(dataset, 1, which(dataset$imdb_rating < 3.8) , NA)

#update dataset to dataset_no_outlier (remove NA)
complete_i=complete.cases(dataset)
dataset_no_outlier=dataset[complete_i,]

#plot qq plot without outliers
par(mfrow = c(2,1))
fullmodel2=lm(imdb_rating~.,data=dataset_no_outlier)
qqnorm(fullmodel$residuals)
qqline(fullmodel$residuals, col="red")
#plot residual without outliers
plot(fullmodel$residuals,xlab="predicated without outliers")

sum1=summary(fullmodel)

#residual plot of explanary variables
par(mfrow = c(1,3))
plot(dataset_no_outlier$runtime, fullmodel$residuals)
abline(0,0,col ="red")
plot(dataset_no_outlier$critics_score, fullmodel$residuals)
abline(0,0,col ="red")
plot(dataset_no_outlier$audience_score, fullmodel$residuals)
abline(0,0,col ="red")
#add square_audience_score
dataset_no_outlier$square_audience_score = dataset_no_outlier$audience_score^2 /100
fullmodel=lm(imdb_rating~.,data=dataset_no_outlier)
summary(fullmodel)

#variables selection using three method
regsubsets.out.backward <- regsubsets(imdb_rating~., data=dataset_no_outlier, nbest=1,
                                      nvmax=NULL, method="backward")
regsubsets.out.forward <- regsubsets(imdb_rating~., data=dataset_no_outlier, nbest=1,
                                     nvmax=NULL, method="forward")
regsubsets.out.exhaustive <- regsubsets(imdb_rating~., data=dataset_no_outlier, nbest=1,
                                        nvmax=NULL, method="exhaustive")

plot(regsubsets.out.backward,scale = "Cp",main = "Cp backward")
plot(regsubsets.out.backward,scale = "adjr2",main = "Adjusted R^2 backward")

#selected variables (adjusted R^2):

#oscar_season
#best_pic_win
#best_actor_win
#mpaa_rating
#genre
#runtime
#critics_rating
#critics_score
#audience_score
#square_audience_score

#selected variables (Cp):

#oscar_season
#mpaa_rating
#genre
#runtime
#critics_rating
#critics_score
#audience_score
#square_audience_score

#relevel categorical variables (include two ratings)
dataset_no_outlier$mpaa_rating = relevel(factor(dataset_no_outlier$mpaa_rating), ref = "Unrated")
dataset_no_outlier$genre = relevel(factor(dataset_no_outlier$genre), ref = "Animation")
dataset_no_outlier$critics_rating = relevel(factor(dataset_no_outlier$critics_rating), ref = "Fresh")


#fit two  bestmodel according to adjusted R^2 and Cp (include two ratings)
bestmodelR=lm(imdb_rating~oscar_season+best_pic_win+best_actor_win+mpaa_rating+genre+runtime+critics_rating+critics_score+audience_score+square_audience_score,data=dataset_no_outlier)
summary(bestmodelR)

bestmodelC=lm(imdb_rating~oscar_season+mpaa_rating+genre+runtime+critics_rating+critics_score+audience_score+square_audience_score,data=dataset_no_outlier)
summary(bestmodelC)

set.seed(666)



#select half of the data randomly
n <- nrow(dataset_no_outlier)
id.subset1 <- sort(sample(1:n, round(n/1.5), replace = FALSE))
#initially subset1 is the training set and subset2 is the hold-out set 
dat.subset1 <- dataset_no_outlier[id.subset1,]
dat.subset2 <- dataset_no_outlier[-id.subset1,] 

#hold out
#compare to the selected model-adjusted R^2
bestModelR.subset1 <- lm(imdb_rating~oscar_season+best_pic_win+best_actor_win+mpaa_rating+genre+runtime+critics_rating+critics_score+audience_score+square_audience_score, data = dat.subset1)
bestModelR.pred1 <- predict(bestModel.subset1, dat.subset2)
bestModelR.err1 <- sqrt(sum((dat.subset2$imdb_rating - bestModel.pred1)^2)/length(bestModel.pred1))
bestModelR.err1 #1.306908

#compare to the selected model-Cp
bestModelC.subset1 <- lm(imdb_rating~oscar_season+mpaa_rating+genre+runtime+critics_rating+critics_score+audience_score+square_audience_score, data = dat.subset1)
bestModelC.pred1 <- predict(bestModel.subset1, dat.subset2)
bestModelC.err1 <- sqrt(sum((dat.subset2$imdb_rating - bestModel.pred1)^2)/length(bestModel.pred1))
bestModelC.err1 #1.306908

# Leave-one out
#Function to calculate the leave-one-out cross validation error.
ls.cvrmse <- function(ls.out)
  # Compute the leave-one-out cross-validated root mean squared error of prediction.
  # Handles missing values.
  # ls.out is a fitted regression model from lsfit or lm.
  # (c) Copyright William J. Welch 1997
{
  res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  # Identify NA's and remove them.
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}

bestModelR.cvrmse <- ls.cvrmse(bestmodelR)
bestModelC.cvrmse <- ls.cvrmse(bestmodelC)

bestModelR.cvrmse #0.3883042
bestModelC.cvrmse #0.3881472


# Remove the two ratings related to the movie goodness.
dataset_remov = subset(dataset_no_outlier,select = -c(7:8,16))
fit.remov = lm(imdb_rating~., data = dataset_remov)
summary(fit.remov)
qqnorm(fit.remov$residuals)

#backward select variables
backward_remov <- regsubsets(imdb_rating~., data=dataset_remov, nbest=1,
                             nvmax=NULL, method="backward")
plot(backward_remov, scale = "adjr2")
# Variables are:

# genre, 
# runtime, 
# mpaa_rating, 
# critics_rating, 
# best_pic_win,


#forward select variables
forward_remov <- regsubsets(imdb_rating~.,data=dataset_remov,nbest=1,
                            nvmax=NULL, method="forward")
plot(forward_remov, scale = "adjr2")
# Variables are:

# genre, 
# runtime, 
# mpaa_rating, 
# critics_rating, 
# best_pic_win,
# oscar_season

#exhaustive select variables
exhaustive_remov <- regsubsets(imdb_rating~., data=dataset_remov,nbest=1,
                               nvmax=NULL, method="exhaustive")
plot(exhaustive_remov, scale = "adjr2")

# Variables are:

# genre, 
# runtime, 
# mpaa_rating, 
# critics_rating, 
# best_pic_win,

#relevel categorical variables (exclude two ratings)
dataset_remov$mpaa_rating = relevel(factor(dataset_remov$mpaa_rating), ref = "PG-13")
dataset_remov$critics_rating = relevel(factor(dataset_remov$critics_rating), ref = "Rotten")

#model with 5 variables:genre, runtime, mpaa_rating, critics_rating, best_pic_win, (exhaustive & backward)
model.5vars<- lm(imdb_rating~genre+runtime+mpaa_rating+critics_rating+best_pic_win, data = dataset_remov)
summary(model.5vars)
extractAIC(model.5vars)
# [1]   20.0000 -520.6464

#model with 6 variables:genre, runtime, mpaa_rating, critics_rating, best_pic_win,oscar_season.(forward)
model.6vars<- lm(imdb_rating~genre+runtime+mpaa_rating+critics_rating+best_pic_win+oscar_season, data = dataset_remov)
summary(model.6vars)
extractAIC(model.6vars)
# [1]   21.0000 -519.4861

set.seed(306)

#initially subset1 is the training set and subset2 is the hold-out set 
dat.subset1.remov <- dataset_remov[id.subset1,]
dat.subset2.remov<- dataset_remov[-id.subset1,] 

#hold out
#compare to the selected 5-variable model
rem.bestModel5.subset1 <- lm(imdb_rating~genre+runtime+mpaa_rating+critics_rating+best_pic_win, data = dat.subset1.remov)
rem.bestModel5.pred1 <- predict(rem.bestModel5.subset1, dat.subset2.remov)
rem.bestModel5.err1 <- sqrt(sum((dat.subset2.remov$imdb_rating - rem.bestModel5.pred1)^2)/length(rem.bestModel5.pred1))
rem.bestModel5.err1
#0.7332361

#compare to the selected 6-variable model
rem.bestModel6.subset1 <- lm(imdb_rating~genre+runtime+mpaa_rating+critics_rating+best_pic_win+oscar_season, data = dat.subset1.remov)
rem.bestModel6.pred1 <- predict(rem.bestModel6.subset1, dat.subset2.remov)
rem.bestModel6.err1 <- sqrt(sum((dat.subset2.remov$imdb_rating - rem.bestModel6.pred1)^2)/length(rem.bestModel6.pred1))
rem.bestModel6.err1
#0.7336062

# Leave-one out
bestModel5.cvrmse <- ls.cvrmse(model.5vars)
bestModel6.cvrmse <- ls.cvrmse(model.6vars)

bestModel5.cvrmse #0.662861
bestModel6.cvrmse #0.663883


#summary for bestModelC (include two ratings)
summary(bestModelC)

#summary for model.5vars (exclude two ratings)
summary(model.5vars)

#predictions for La La Land, Logan, Beauty and Beast, Rogue One
imdb_rating=c(8.6, 8.5, 7.8, 8.0)
runtime=c(128,141,129,133)
audience_score=c(83,92,85,88)
square_audience_score=audience_score^2/100
genre=c('Drama','Action & Adventure', 'Musical & Performing Arts' , 'Action & Adventure')
mpaa_rating=c('PG','R','PG','PG-13')
critics_rating=c('Fresh','Fresh','Fresh','Fresh')
critics_score=c(93,92,71,85)
oscar_season=c('yes','no','no','yes')
best_pic_win=c('no','no','no','no')
predict_dataset=data.frame(imdb_rating=imdb_rating,runtime=runtime,audience_score=audience_score, square_audience_score=square_audience_score,genre=genre, mpaa_rating=mpaa_rating,critics_rating=critics_rating, critics_score=critics_score, oscar_season=oscar_season, best_pic_won=best_pic_won)

predict_modelC=predict(bestmodelC, newdata=predict_dataset,interval="predict")
predict_modelC

predict_model5=predict(model.5vars, newdata=predict_dataset,interval="predict")
predict_model5



