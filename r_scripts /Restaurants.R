#This just cleans your environment 
rm(list=ls())
library(mice)
restaurants_IL2 <- read.csv("Documents/Groningen/RUG/Customer Models/Assignment 1/restaurants_IL.csv")
summary(restaurants_IL2)
str(restaurants_IL2)

#We erase the first column and repeated values. 
restaurants_IL2 <- restaurants_IL2[-c(1)]
restaurants_IL2 <- unique(restaurants_IL2)

#boxplots to check for outliers, even though there are some outliers, especially in review count, 
#They still seem to make sense, it's not unreasonable to think that a restaurant can have over 1000 online 
#reviews, even though it might not be the norm. 
boxplot(restaurants_IL2$stars)
boxplot(restaurants_IL2$review_count)
hist(restaurants_IL2$stars)
hist(restaurants_IL2$review_count)

# what to do with None?
# Replace with NA's
restaurants_IL2[restaurants_IL2=="None"]=NA

# make the factors as real factors:
restaurants_IL2$state=as.factor(restaurants_IL2$state)#Not done in the tutorial session file 
restaurants_IL2$categories=as.factor(restaurants_IL2$categories)#Same, but I think worth doing 
restaurants_IL2$RestaurantsAttire=as.factor(as.character(restaurants_IL2$RestaurantsAttire))
restaurants_IL2$GoodForKids=as.factor(as.character(restaurants_IL2$GoodForKids))
restaurants_IL2$RestaurantsReservations=as.factor(as.character(restaurants_IL2$RestaurantsReservations))
restaurants_IL2$Caters=as.factor(as.character(restaurants_IL2$Caters))
restaurants_IL2$NoiseLevel=as.factor(as.character(restaurants_IL2$NoiseLevel))
restaurants_IL2$RestaurantsTakeOut=as.factor(as.character(restaurants_IL2$RestaurantsTakeOut))
restaurants_IL2$RestaurantsPriceRange2=as.factor(as.character(restaurants_IL2$RestaurantsPriceRange2))
restaurants_IL2$OutdoorSeating=as.factor(as.character(restaurants_IL2$OutdoorSeating))
restaurants_IL2$WiFi=as.factor(as.character(restaurants_IL2$WiFi))
restaurants_IL2$RestaurantsDelivery=as.factor(as.character(restaurants_IL2$RestaurantsDelivery))
restaurants_IL2$BusinessAcceptsCreditCards=as.factor(as.character(restaurants_IL2$BusinessAcceptsCreditCards))
restaurants_IL2$dessert=as.factor(as.character(restaurants_IL2$dessert))
restaurants_IL2$latenight=as.factor(as.character(restaurants_IL2$latenight))
restaurants_IL2$lunch=as.factor(as.character(restaurants_IL2$lunch))
restaurants_IL2$dinner=as.factor(as.character(restaurants_IL2$dinner))
restaurants_IL2$brunch=as.factor(as.character(restaurants_IL2$brunch))
restaurants_IL2$breakfast=as.factor(as.character(restaurants_IL2$breakfast))
restaurants_IL2$garage=as.factor(as.character(restaurants_IL2$garage))
restaurants_IL2$street=as.factor(as.character(restaurants_IL2$street))
restaurants_IL2$validated=as.factor(as.character(restaurants_IL2$validated))
restaurants_IL2$lot=as.factor(as.character(restaurants_IL2$lot))
restaurants_IL2$valet=as.factor(as.character(restaurants_IL2$valet))
restaurants_IL2$Open7days=as.factor(restaurants_IL2$Open7days)
restaurants_IL2$OpenSundays=as.factor(restaurants_IL2$OpenSundays)
restaurants_IL2$checkedin100=as.factor(restaurants_IL2$checkedin100)



# Replace with 0
restaurants_IL2$posreview[is.na(restaurants_IL2$posreview)]=0
restaurants_IL2$posreview2016[is.na(restaurants_IL2$posreview2016)]=0
restaurants_IL2$posreview2017[is.na(restaurants_IL2$posreview2017)]=0
restaurants_IL2$posreview2018[is.na(restaurants_IL2$posreview2018)]=0

restaurants_IL2$negreview[is.na(restaurants_IL2$negreview)]=0
restaurants_IL2$negreview2016[is.na(restaurants_IL2$negreview2016)]=0
restaurants_IL2$negreview2017[is.na(restaurants_IL2$negreview2017)]=0
restaurants_IL2$negreview2018[is.na(restaurants_IL2$negreview2018)]=0

# what to do with NA's in checked-ins?
# it seems like the restaurants where closed at >2016. Replace the NA's with 0
restaurants_IL2$checkedin100[is.na(restaurants_IL2$checkedin100)]=0
restaurants_IL2$checkedin2016[is.na(restaurants_IL2$checkedin2016)]=0
restaurants_IL2$checkedin2017[is.na(restaurants_IL2$checkedin2017)]=0
restaurants_IL2$checkedin2018[is.na(restaurants_IL2$checkedin2018)]=0

# dealing with missings
a=md.pattern(restaurants_IL2) # what is the missing pattern? (look at the last row of a)
print(a[nrow(a),])


#Konstantin's subset
restaurants_IL2a=subset(restaurants_IL2,select=-c(1, 3:5, 33:43))

# now double check the NA's
a=md.pattern(restaurants_IL2a) # what is the missing pattern? (look at the last row of a)
print(a[nrow(a),])


predictorMatrix <- matrix(0,nrow = ncol(restaurants_IL2a), ncol = ncol(restaurants_IL2a)) # Make a matrix of zeros
colnames(predictorMatrix)=colnames(restaurants_IL2a)
rownames(predictorMatrix)=colnames(restaurants_IL2a)

containingNA=colnames(a)[a[nrow(a),]>0]
containingNA=containingNA[-length(containingNA)] # columns containing NA's

#Konstatin used different ones for prediction 
usedforprediction=c("state", "stars","review_count","categories","BusinessAcceptsCreditCards","RestaurantsPriceRange2",
                    "checkedin100","OpenSundays", "Open7days","NoiseLevel","Caters",containingNA)

#Konstatin predictors 
usedforprediction=c("state", "stars","review_count","categories",
                    "checkedin100","OpenSundays", "Open7days",containingNA)


predictorMatrix[usedforprediction,containingNA] <- 1 
diag(predictorMatrix) <- 0 #diagonal must be zero

#impute data
restaurants_IL2a_data_imputed <- mice(restaurants_IL2a, predictorMatrix = predictorMatrix, m=1, maxit = 50, seed = 500)
summary(restaurants_IL2a_data_imputed)


#get one of the complete data sets
restaurants_IL2a_data_imputed_complete <- complete(restaurants_IL2a_data_imputed,1)
str(restaurants_IL2a_data_imputed_complete)

#Check if you still have missing patterns 
a=md.pattern(restaurants_IL2a_data_imputed_complete) # what is the missing pattern? (look at the last row of a)


#the complete data sets can be used to estimate your model of choice
#and the results of all m models can be combined as in the earlier example
write.csv(restaurants_IL2a_data_imputed_complete, file="restaurants_IL2a_imputed.csv")

restaurants.imputed <- restaurants_IL2a_data_imputed_complete

library(glmnet)

#Literature model - state, stars, categories, reservations, noiselevel, price range are significant
logit1 <- glm(checkedin100~state + stars + categories + 
                GoodForKids + RestaurantsReservations + Caters + NoiseLevel + RestaurantsPriceRange2 + 
                BusinessAcceptsCreditCards, family = binomial(link = "logit"), data = restaurants.imputed)
summary(logit1)
#Estimate interpretation? State, stars, caters, reservations and price range up to a point have a positive 
#increase in the probability of checkedin100. 
#AIC 3095.1

#Full model with review count - opensundays, 7 days, parking options, outdoor seating, specially review count 
#are significant. 
logitfull <- glm(checkedin100~., family = binomial(link = "logit"), data = restaurants.imputed)
summary(logitfull)
#AIC 1575.3

#I don't get a warning when review count is excluded from the model 
logitfull2 <- glm(checkedin100~.-review_count, family = binomial(link = "logit"), data = restaurants.imputed)
summary(logitfull2)
#AIC 2697.5

####Marginal effects 
library(mfx)
logitmfx(formula = checkedin100~state + stars + categories + 
           GoodForKids + RestaurantsReservations + Caters + NoiseLevel + RestaurantsPriceRange2 + 
           BusinessAcceptsCreditCards, data = restaurants.imputed)
#When the state is NC the probablity that checkedin100 is 1, increases by 0.15% 
#Why is there a negative effect in all categories? Further exploration of this variable? 
#Same with noise levels? 

nullModel <- glm(checkedin100 ~ 1, family = binomial(link = "logit"), data = restaurants.imputed)
logLik(nullModel)
logLik(logit1)
logLik(logitfull)
logLik(logitfull2)

######PSEUDO R2 
1-(logLik(logit1)/logLik(nullModel)) #0.1125 
1-(logLik(logitfull)/logLik(nullModel))#0.5672 
1-(logLik(logitfull2)/logLik(nullModel))#0.2419 
#Again the best model is the full model, explains most of the variance of the DV 

#####VIF values - VIF is not the best approach for binary data.
library(car)
library(faraway)
vif(logitfull)#reviewcount shows really high levels of multicollinearity, overall all variables do. 
vif(logitfull2)#once you take reviewcount out multicollinearity improves a lot, with mainly state, categories and open7days and Sundays being in the doble digits. 
vif(logit1)#kind get the same results as the model above

####HITRATE for different models 
#Literature model - I get a different result than Konstatin probably because of review count. 
restaurants.imputed$predvals <- predict(logit1, type = "response")
restaurants.imputed$predchoice <- ifelse(restaurants.imputed$predvals > 0.5, 1, 0)
table(restaurants.imputed$checkedin100, restaurants.imputed$predchoice)
((1080+606)/(1080+606+484+357))*100 #66.71%

restaurants.imputed$predvals2 <- predict(logitfull, type = "response")
restaurants.imputed$predchoice2 <- ifelse(restaurants.imputed$predvals2 > 0.5, 1, 0)
table(restaurants.imputed$checkedin100, restaurants.imputed$predchoice2)
((1301+893)/(1301+893+136+197))*100 #86.82%

restaurants.imputed$predvals3 <- predict(logitfull2, type = "response")
restaurants.imputed$predchoice3 <- ifelse(restaurants.imputed$predvals3 > 0.5, 1, 0)
table(restaurants.imputed$checkedin100, restaurants.imputed$predchoice3)
((1121+745)/(1121+745+316+345))*100 #73.84%

#######FULL MODEL is the best model with hitrate
#####FULL MODEL is also the best model with AIC 

chisq.test(restaurants$RestaurantsReservations, restaurants$RestaurantsDelivery)
chisq.test(restaurants$RestaurantsPriceRange2, restaurants$RestaurantsAttire)
chisq.test(restaurants$categories, restaurants$latenight, simulate.p.value = TRUE)#There is correlation 
chisq.test(restaurants$categories, restaurants$dessert, simulate.p.value = TRUE)
chisq.test(restaurants$categories, restaurants$brunch, simulate.p.value = TRUE)
chisq.test(restaurants$categories, restaurants$lunch, simulate.p.value = TRUE)
chisq.test(restaurants$categories, restaurants$breakfast, simulate.p.value = TRUE)
chisq.test(restaurants$categories, restaurants$dinner, simulate.p.value = TRUE)




#######Reducing our dataset to deal with multicolinearity and simplify our best model since it is the full model
restaurants.parking <- restaurants_IL2a_data_imputed_complete
restaurants.parking$parking <- ifelse(restaurants.parking$garage == 'True'| restaurants.parking$valet == 'True'| restaurants.parking$lot == 'True'| restaurants.parking$validated == 'True'| restaurants.parking$street  == 'True', TRUE, FALSE)
str(restaurants.parking)
#should we make parking a factor? 
restaurants.parking=subset(restaurants.parking,select=-c(garage, valet, lot, validated, street))
restaurants.parking$NoiseLevel[restaurants.parking$NoiseLevel == 'very_loud'] <- 'loud'
restaurants.parking$NoiseLevel <- as.factor(as.character(restaurants.parking$NoiseLevel)) #make NoiseLevel only 3 levels (quiet, loud, average)
restaurants.parking$RestaurantsPriceRange2[restaurants.parking$RestaurantsPriceRange2 == '4'] <- '3'
restaurants.parking$RestaurantsPriceRange2 <- as.factor(as.character(restaurants.parking$RestaurantsPriceRange2)) #make Price only 3 levels (1,2,3 -> low, avg, high)

 #####ALL the same test as before but with the new dataset. 
#Literature model- NEW PARKING DATASET 
logit1.parking <- glm(checkedin100~state + stars + categories + 
                GoodForKids + RestaurantsReservations + Caters + NoiseLevel + RestaurantsPriceRange2 + 
                BusinessAcceptsCreditCards, family = binomial(link = "logit"), data = restaurants.parking)
summary(logit1.parking)
#Estimate interpretation same as before. AIC 3095.1, also same as before 

logitfull.parking <- glm(checkedin100~., family = binomial(link = "logit"), data = restaurants.parking)
summary(logitfull.parking)
#AIC 1573.6, AIC before parking variable 1573.3.

#I don't get a warning when review count is excluded from the model 
logitfull2.parking <- glm(checkedin100~.-review_count, family = binomial(link = "logit"), data = restaurants.parking)
summary(logitfull2.parking)
#AIC 2655.1 compared to 2697 from before, this model has improved. 

####Marginal effects - NEW PARKING DATASET 
library(mfx)
logitmfx(formula = checkedin100~state + stars + categories + 
           GoodForKids + RestaurantsReservations + Caters + NoiseLevel + RestaurantsPriceRange2 + 
           BusinessAcceptsCreditCards, data = restaurants.parking)
#When the state in NC the probablity that checkedin100 is 1, increases by 0.15% 
#Why is there a negative effect in all categories? Further exploration of this variable? 
#Same with noise levels? 

nullModel.parking <- glm(checkedin100 ~ 1, family = binomial(link = "logit"), data = restaurants.parking)
logLik(nullModel.parking)
logLik(logit1.parking)
logLik(logitfull.parking)
logLik(logitfull2.parking)

######PSEUDO R2 
1-(logLik(logit1.parking)/logLik(nullModel.parking))#0.1152
1-(logLik(logitfull.parking)/logLik(nullModel.parking))#0.5654
1-(logLik(logitfull2.parking)/logLik(nullModel.parking))#0.2518
#Again the best model is the full model, explains most of the variance of the DV 

#####VIF values - VIF is not the best approach for binary data.
library(car)
library(faraway)
vif(logitfull.parking)#reviewcount shows really high levels of multicollinearity, overall all variables do. 
vif(logitfull2.parking)#once you take reviewcount out multicollinearity improves a lot, with mainly state, categories and open7days and Sundays being in the doble digits. 
vif(logit1.parking)#kind get the same results as the model above

####HITRATE for different models - NEW PARKING DATASET 
#Literature model - I get a different result than Konstatin probably because of review count. 
restaurants.parking$predvals <- predict(logit1, type = "response")
restaurants.parking$predchoice <- ifelse(restaurants.parking$predvals > 0.5, 1, 0)
table(restaurants.parking$checkedin100, restaurants.parking$predchoice)
((1080+606)/(1080+606+484+357))*100 #66.71% a bit smaller than 67.66%

restaurants.parking$predvals2 <- predict(logitfull, type = "response")
restaurants.parking$predchoice2 <- ifelse(restaurants.parking$predvals2 > 0.5, 1, 0)
table(restaurants.parking$checkedin100, restaurants.parking$predchoice2)
((1305+891)/(1305+891+132+199))*100 #86.90% hitrate with full model including review count 

restaurants.parking$predvals3 <- predict(logitfull2, type = "response")
restaurants.parking$predchoice3 <- ifelse(restaurants.parking$predvals3 > 0.5, 1, 0)
table(restaurants.parking$checkedin100, restaurants.parking$predchoice3)
((1118+765)/(1118+765+325+319))*100 #74.51% hitrate with full model and no review count 
#######FULL MODEL is the best model with hitrate


chisq.test(restaurants.parking$RestaurantsReservations, restaurants.parking$RestaurantsDelivery)
chisq.test(restaurants.parking$RestaurantsPriceRange2, restaurants.parking$RestaurantsAttire)
chisq.test(restaurants.parking$categories, restaurants.parking$latenight, simulate.p.value = TRUE)#There is correlation 
chisq.test(restaurants.parking$categories, restaurants.parking$dessert, simulate.p.value = TRUE)
chisq.test(restaurants.parking$categories, restaurants.parking$brunch, simulate.p.value = TRUE)
chisq.test(restaurants.parking$categories, restaurants.parking$lunch, simulate.p.value = TRUE)
chisq.test(restaurants.parking$categories, restaurants.parking$breakfast, simulate.p.value = TRUE)
chisq.test(restaurants.parking$categories, restaurants.parking$dinner, simulate.p.value = TRUE)
chisq.test(restaurants.parking$Open7days, restaurants.parking$OpenSundays)#There's correlation 
#We also noticed multicollinearity when doing VIF test, and it's logical to assume that they give out the same information 

#We leave out the OpenSundays variable 
restaurants.2 = subset(restaurants.parking, select = -c(OpenSundays, predchoice,predchoice2,predchoice3, predvals,predvals2, predvals3, breakfast,brunch,dinner,lunch,latenight,dessert))

#Literature model- 
logit1.1 <- glm(checkedin100~state + stars + categories + 
                GoodForKids + RestaurantsReservations + Caters + NoiseLevel + RestaurantsPriceRange2 + 
                BusinessAcceptsCreditCards, family = binomial(link = "logit"), data = restaurants.2)
summary(logit1.1)
#Estimate interpretation same as before. AIC 3095.1, also same as before 

logitfull.1 <- glm(checkedin100~., family = binomial(link = "logit"), data = restaurants.2)
summary(logitfull.1)
#AIC 1592.6, it gets a bit worst. AIC before parking variable 1573.3.

#I don't get a warning when review count is excluded from the model 
logitfull2.1 <- glm(checkedin100~.-review_count, family = binomial(link = "logit"), data = restaurants.2)
summary(logitfull2.1)
#AIC 2795, also gets worst compared to 2697 from before,. 


nullModel.1 <- glm(checkedin100 ~ 1, family = binomial(link = "logit"), data = restaurants.2)
logLik(nullModel.1)
logLik(logit1.1)
logLik(logitfull.1)
logLik(logitfull2.1)

######PSEUDO R2 
1-(logLik(logit1.1)/logLik(nullModel.1))#0.1152
1-(logLik(logitfull.1)/logLik(nullModel.1))#0.5558, a bit lower than before 
1-(logLik(logitfull2.1)/logLik(nullModel.1))#0.2075, about 5 points worst 

#####VIF 
vif(logitfull.1)#reviewcount shows really high levels of multicollinearity, overall all variables do. 
vif(logitfull2.1)#categories still have high multicollinearity, most variables are pretty decent though 
vif(logit1.1)#kind get the same results as the model above

####HITRATE for different models - NEW PARKING DATASET 
#Literature model - I get a different result than Konstatin probably because of review count. 
restaurants.2$predvals <- predict(logit1.1, type = "response")
restaurants.2$predchoice <- ifelse(restaurants.2$predvals > 0.5, 1, 0)
table(restaurants.2$checkedin100, restaurants.2$predchoice)
((1080+606)/(1080+606+484+357))*100 #66.71% a bit smaller than 67.66%

restaurants.2$predvals2 <- predict(logitfull.1, type = "response")
restaurants.2$predchoice2 <- ifelse(restaurants.2$predvals2 > 0.5, 1, 0)
table(restaurants.2$checkedin100, restaurants.2$predchoice2)
((1307+871)/(1307+871+130+219))*100 #86.18% it goes down by very little in comparison with previous model 

restaurants.2$predvals3 <- predict(logitfull2.1, type = "response")
restaurants.2$predchoice3 <- ifelse(restaurants.2$predvals3 > 0.5, 1, 0)
table(restaurants.2$checkedin100, restaurants.2$predchoice3)
((1104+730)/(1104+730+333+360))*100 #72.57% goes down by very little as well. 
#######FULL MODEL is the best model with hitrate

#Not sure what to do with this model, it does not reduced multicollinearity the way I expected, but it also doesn't affect the rest of the tests significantly. 

chisq.test(restaurants.2$RestaurantsReservations, restaurants.2$RestaurantsDelivery)#p-value of 1??? 
chisq.test(restaurants.2$RestaurantsPriceRange2, restaurants.2$RestaurantsAttire)#correlated
chisq.test(restaurants.2$categories, restaurants.2$latenight)#There is correlation 



#check for correlation between checkedin and review count 
#check for multicollinearity 
#DescTools - PseudoR2
#Holdout sample? 

#####QUESTION 2 
restaurants.final = subset(restaurants.2, select = -c(predchoice3,predchoice2,predchoice,predvals,predvals2,predvals3)) 
restaurants.final.2 = subset(restaurants.parking, select = -c(predchoice3,predchoice2,predchoice,predvals3,predvals2,predvals))
#create mlogit data 

library(mlogit)
restaurants.mlogit <- mlogit.data(data = restaurants.final, choice = "stars", shape = "wide")
restaurants.mlogit2 <- mlogit.data(data = restaurants.final.2, choice = "stars", shape = "wide")
#model with only individual (in our case each restaurant is an individual) specific variables 
m1 <- mlogit(stars ~0 |state+categories+Caters+RestaurantsPriceRange2+RestaurantsAttire+Open7days+GoodForKids+NoiseLevel+OutdoorSeating+RestaurantsDelivery+checkedin100+RestaurantsReservations+RestaurantsTakeOut+WiFi+BusinessAcceptsCreditCards+parking, reflevel = "1", data = restaurants.mlogit)
summary(m1)
AIC(m1)

m1.2 <- mlogit(stars ~ 0 |state+categories+Caters+RestaurantsPriceRange2+RestaurantsAttire+Open7days+GoodForKids+NoiseLevel+OutdoorSeating+RestaurantsDelivery+checkedin100+RestaurantsReservations+RestaurantsTakeOut+WiFi+BusinessAcceptsCreditCards+parking+lunch+dessert+breakfast+dinner+latenight+brunch, reflevel = "1", data = restaurants.mlogit2)
summary(m1.2)
AIC(m1.2)
#I had to run it again without review count cause I got a correlation issue when doing it with it. 
#Do the intercept estimates mean that all else equal or 0, for all restaurants 4.5 has the highes probability. 
#All intercepts are positive. They are all more likely than 1 star. 
#Goodforkids seem to be the only variable that's significant? When the restaurant is goodforkids the higher the probability of 
#that restaurant getting higher stars, specifically 2 stars? That's not too good, maybe I am reading this wrong. 

#Literature model 
m.literature <- mlogit(stars ~0 |state+categories+Caters+RestaurantsPriceRange2+GoodForKids+NoiseLevel+checkedin100+RestaurantsReservations+BusinessAcceptsCreditCards+parking, reflevel = "1", data = restaurants.mlogit)
summary(m.literature)
AIC(m.literature)

m.literature.2 <-  mlogit(stars ~0 |state+categories+Caters+RestaurantsPriceRange2+GoodForKids+NoiseLevel+checkedin100+RestaurantsReservations+BusinessAcceptsCreditCards+parking, reflevel = "1", data = restaurants.mlogit2)
summary(m.literature.2)
AIC(m.literature.2)
#It seems to me that caters and price range are two of the most important variables observed that affect stars, even though they are not significant? 

m3 <- mlogit(stars ~0 |RestaurantsPriceRange2+GoodForKids+NoiseLevel, reflevel = "1", data = restaurants.mlogit)
summary(m3)
AIC(m3)
#Caters give you a high probability of 4.5 stars. Not sure how to interpret price ranges 
exp(coef(m3))

#Alexandro's no state model 
m1.no.state <- mlogit(stars ~ 0 |+categories+RestaurantsPriceRange2+Open7days+GoodForKids+NoiseLevel+OutdoorSeating+RestaurantsDelivery+checkedin100+RestaurantsReservations+RestaurantsTakeOut +parking+lunch+dessert+dinner+breakfast+latenight+brunch, reflevel = "1", data = restaurants.mlogit2)
summary(m1.no.state)
AIC(m1.no.state)

m1.2$choice = apply(fitted(m1.2, type = "probabilities"), 1 , which.max)
m1.2$choice = factor(m1.2$choice, levels = c(1,2,3,4,5,6,7,8,9), labels = c("1","1.5","2","2.5","3","3.5","4","4.5","5"))
table(restaurants.final.2$stars, m1.2$choice)

(3+18+21+38+103+222+417+33+1)/2527#33%

m1.no.state$choice = apply(fitted(m1.no.state, type = "probabilities"), 1 , which.max)
m1.no.state$choice = factor(m1.no.state$choice, levels = c(1,2,3,4,5,6,7,8,9), labels = c("1","1.5","2","2.5","3","3.5","4","4.5","5"))
table(restaurants.final.2$stars, m1.no.state$choice)

(15+17+37+96+218+422+33)/2527#33% which is larger that 25% so it predicts better than a random model 
#We don't predict 1 and 5 star values 

#Model no state, performs better than the full model in terms of degrees of freedom, p-value shows that both model perform equally
lrtest(m6, m1.no.state)

###Market share 
m1.no.state$fitted = apply(fitted(m1.no.state, type = "probabilities"), 2, mean)
rbind(m1.no.state$fitted, m1.no.state$freq/sum(m1.no.state$freq))
m1.no.state$fitted
m1.no.state$freq/sum(m1.no.state$freq)


m1.2$fitted = apply(fitted(m1.2, type = "probabilities"),2, mean)
m1.2$fitted.values
m1.2$freq/sum(m1.2$freq)

m1$fitted = apply(fitted(m1, type = "probabilities"), 2, mean)
m1$fitted
m1$freq/sum(m1$freq)

#####TEST FOR IIA 
m1.1000 <- mlogit(stars ~ 0 |+categories+RestaurantsPriceRange2+Open7days+GoodForKids+NoiseLevel+OutdoorSeating+RestaurantsDelivery+checkedin100+RestaurantsReservations +parking+lunch+dessert+dinner+breakfast+brunch, reflevel = "1", data = restaurants.mlogit2)
m1.noestate.sub <- mlogit (stars ~ 0 |categories+RestaurantsPriceRange2+Open7days+GoodForKids+NoiseLevel+OutdoorSeating+RestaurantsDelivery+checkedin100+RestaurantsReservations +parking+lunch+dessert+dinner+breakfast+brunch, reflevel = "1", alt.subset = c("1","2","3","3.5","4","4.5","5"), data = restaurants.mlogit2)
hmftest(m1.1000, m1.noestate.sub)
