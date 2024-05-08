#####Question 2 
library(mlogit)
library(MASS)
library(dplyr)
library(magrittr)

restaurants.stars.ordered <- restaurants.parking

restaurants.stars.ordered$stars[restaurants.stars.ordered$stars == '1.5'] <- "1"
restaurants.stars.ordered$stars[restaurants.stars.ordered$stars == '2.5'] <- "2"
restaurants.stars.ordered$stars[restaurants.stars.ordered$stars == '3.5' ] <- "3"
restaurants.stars.ordered$stars[restaurants.stars.ordered$stars =='4.5'] <- "4"

restaurants.stars.ordered$stars <- as.factor(restaurants.stars.ordered$stars) 
restaurants.stars.ordered$stars <- ordered(restaurants.stars.ordered$stars, levels=c("1","2","3","4","5"))
restaurants.stars.ordered$Open7days<-as.numeric(restaurants.stars.ordered$Open7days)
restaurants.stars.ordered$RestaurantsPriceRange2<-as.numeric(restaurants.stars.ordered$RestaurantsPriceRange2)
restaurants.stars.ordered$RestaurantsReservations<-as.numeric(restaurants.stars.ordered$RestaurantsReservations)
restaurants.stars.ordered$lunch<-as.numeric(restaurants.stars.ordered$lunch)
restaurants.stars.ordered$brunch<-as.numeric(restaurants.stars.ordered$brunch)
restaurants.stars.ordered$NoiseLevel<-as.numeric(restaurants.stars.ordered$NoiseLevel)
restaurants.stars.ordered$OutdoorSeating<-as.numeric(restaurants.stars.ordered$OutdoorSeating)
restaurants.stars.ordered$RestaurantsDelivery<-as.numeric(restaurants.stars.ordered$RestaurantsDelivery)
restaurants.stars.ordered$breakfast<-as.numeric(restaurants.stars.ordered$breakfast)
restaurants.stars.ordered$latenight<-as.numeric(restaurants.stars.ordered$latenight)
restaurants.stars.ordered$GoodForKids<-as.numeric(restaurants.stars.ordered$GoodForKids)
str(restaurants.stars.ordered)


##Full ordered 
ordered.full <- polr(stars~., data = restaurants.stars.ordered, Hess = TRUE)
summary(ordered.full)
logLik(ordered.full)
#AIC 5586.878
ordered.null <- polr(stars~1, data = restaurants.stars.ordered, Hess = TRUE)
summary(ordered.null)
#AIC 6065.517

###Ordered logit 
ordered.model <-polr (stars ~ RestaurantsPriceRange2 + Open7days+ GoodForKids +NoiseLevel+ OutdoorSeating+ RestaurantsDelivery+ RestaurantsReservations+ parking+ lunch+ breakfast+ brunch+ latenight,data = restaurants.stars.ordered, Hess = TRUE)
summary(ordered.model)
##5769.297 

#LITERATURE MODEL 
ordered.literature <- polr(data=restaurants.stars.ordered, stars~ checkedin100 + categories + 
  GoodForKids + RestaurantsReservations + Caters + NoiseLevel + RestaurantsPriceRange2 + parking+ OutdoorSeating +
  BusinessAcceptsCreditCards, Hess = TRUE)
summary(ordered.literature)
# AIC: 5907.009

###Final ordered model 
ordered.final <- polr( data = restaurants.stars.ordered,stars ~  Caters +  
                         OutdoorSeating+ + latenight +GoodForKids+
                         dinner + brunch + breakfast + Open7days + lunch+
                         parking, Hess = TRUE)
summary(ordered.final)
lrtest(ordered.final, ordered.noiselevel)
AIC(ordered.final, ordered.noiselevel)
BIC(ordered.final, ordered.noiselevel)
ordered.noiselevel <- polr( data = restaurants.stars.ordered,stars ~  Caters +  
                         OutdoorSeating+ + latenight +GoodForKids + RestaurantsPriceRange2 +
                         dinner + brunch + breakfast + Open7days + lunch+
                         parking, Hess = TRUE)
summary(ordered.final)

# AIC: 5757.98 
AIC(ordered.final,ordered.literature )
lrtest(ordered.final, ordered.literature)
# LL ordered final = -2865.0
# LL literatura model = 2935.5
# They are significantly different 
lrtest(ordered.final, ordered.full)

(ctable<-coef(summary(ordered.final)))
p<-pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2
(cbind(ctable,"p value"=p))# NoiseLevelquiet  


##Overlaps ordered final - no overlap 
-5.7467+(1.96*0.3986)
-3.8076-(1.96*0.3851)
-3.8076+(1.96*0.3851)
-1.7160-(1.96*0.3795)
-1.7160+(1.96*0.3795)
1.9883-(1.96*0.3938)

#Overlap? OG model 
-6.4455+(1.96*0.4360) #-5.59094
-4.8100-(1.96*0.3751)#-5.545196
-4.8100+(1.96*0.3751)#-4.074804
-3.8263-(1.96*0.3656)#-4.542876
-3.8263+(1.96*0.3656)#-3.109724
-2.8729-(1.96*0.3626)#-3.583596
-2.8729+(1.96*0.3626)#-2.162204
-1.8773-(1.96*0.3613)#-2.585448
-1.8773+(1.96*0.3613)#-1.169152
-0.7911-(1.96*0.3596)#-1.495916
-0.7911+(1.96*0.3596)#-0.086284
0.7777-(1.96*0.3583)#0.075432
0.7777+(1.96*0.3583)#1.479968
2.8698-(1.96*0.3778)#2.129312


##test parallel line assumptions 
install.packages("brant")
library(brant)

brant(ordered.final)


logLik(ordered.model)
AIC(ordered.model)
#We get error and the parallel assumption clearly does not hold, so we collapse stars levels


install.packages("DescTools")
library(DescTools)
PseudoR2(ordered.final, which = "CoxSnell")
PseudoR2(ordered.literature, which = "CoxSnell")
PseudoR2(ordered.final, which = "Nagelkerke")
PseudoR2(ordered.literature, which = "Nagelkerke")
PseudoR2(ordered.final, which = "McFadden")
PseudoR2(ordered.literature)

confusionMatrix(restaurants.stars.ordered$stars,restaurants.stars.ordered$predictions.1)
restaurants.stars.ordered$predictions.1<-predict(ordered.final)

confusionMatrix(restaurants.stars.ordered$stars,restaurants.stars.ordered$predictions.1)
restaurants.stars.ordered$predictions.1<-predict(ordered.noiselevel)

###AIC test####
fit <- polr( data = restaurants.stars.ordered,stars ~.-state -categories -review_count, Hess = TRUE)
step <- stepAIC(fit, direction="both")
step$anova # display results
