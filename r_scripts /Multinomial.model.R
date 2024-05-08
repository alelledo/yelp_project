library(mlogit)

restaurants.for.mlogit <- restaurants.parking

restaurants.for.mlogit$stars[restaurants.for.mlogit$stars == '1.5'] <- "1"
restaurants.for.mlogit$stars[restaurants.for.mlogit$stars == '2.5'] <- "2"
restaurants.for.mlogit$stars[restaurants.for.mlogit$stars == '3.5' ] <- "3"
restaurants.for.mlogit$stars[restaurants.for.mlogit$stars =='4.5'] <- "4"

restaurants.for.mlogit$GoodForKids<-as.numeric(restaurants.for.mlogit$GoodForKids)
restaurants.for.mlogit$Open7days<-as.numeric(restaurants.for.mlogit$Open7days)
restaurants.for.mlogit$RestaurantsPriceRange2<-as.numeric(restaurants.for.mlogit$RestaurantsPriceRange2)
restaurants.for.mlogit$checkedin100<-as.numeric(restaurants.for.mlogit$checkedin100)
restaurants.for.mlogit$RestaurantsReservations<-as.numeric(restaurants.for.mlogit$RestaurantsReservations)
restaurants.for.mlogit$lunch<-as.numeric(restaurants.for.mlogit$lunch)
restaurants.for.mlogit$brunch<-as.numeric(restaurants.for.mlogit$brunch)
restaurants.for.mlogit$NoiseLevel<-as.numeric(restaurants.for.mlogit$NoiseLevel)
restaurants.for.mlogit$OutdoorSeating<-as.numeric(restaurants.for.mlogit$OutdoorSeating)
restaurants.for.mlogit$RestaurantsDelivery<-as.numeric(restaurants.for.mlogit$RestaurantsDelivery)
restaurants.for.mlogit$breakfast<-as.numeric(restaurants.for.mlogit$breakfast)
restaurants.for.mlogit$latenight<-as.numeric(restaurants.for.mlogit$latenight)
restaurants.for.mlogit$Caters<-as.numeric(restaurants.for.mlogit$Caters)

restaurants.mlogit <- mlogit.data(data = restaurants.for.mlogit, choice = "stars", shape = "wide")



###MODELS
nullModel<-mlogit(stars ~ 0 |1,data = restaurants.mlogit)
summary(nullModel)
AIC(nullModel)


fullModel <- mlogit(stars ~ 0 |state+Caters+RestaurantsPriceRange2+
                      Open7days+GoodForKids+NoiseLevel+OutdoorSeating+RestaurantsDelivery+RestaurantsReservations+
                      BusinessAcceptsCreditCards+parking+lunch+dessert+review_count+
                      breakfast+dinner+latenight+brunch, reflevel = "1",data = restaurants.mlogit)
summary(fullModel)
AIC(fullModel)
#In the full model we get huge standard errors for some variables. 


m2 <- mlogit(stars ~ 0 | GoodForKids + RestaurantsReservations + 
                        Caters + NoiseLevel +  RestaurantsPriceRange2 + 
                        OutdoorSeating  + RestaurantsDelivery + 
                         dessert + latenight + lunch + 
                        dinner + brunch + breakfast +  Open7days  + review_count+
                        parking , reflevel = "1", data = restaurants.mlogit)
summary(m2)
str(m2)
lrtest(m2, fullModel)
#fullmodel -2669.6
#m2 -2626.3
AIC(fullModel,m2)
#fullmodel 5571.132
#m2 5388.650
BIC(fullModel,m2)
m2.1 <- mlogit(stars ~ 0 | GoodForKids + RestaurantsReservations + 
               Caters + NoiseLevel +  RestaurantsPriceRange2 + 
               OutdoorSeating  + RestaurantsDelivery + 
               dessert + latenight + lunch + BusinessAcceptsCreditCards+
               dinner + brunch + breakfast +  Open7days  + review_count+
               parking , reflevel = "1", data = restaurants.mlogit)
lrtest(m2,m2.1)
####HITRATE

m2$choice = apply(fitted(m2, type = "probabilities"), 1 , which.max)
m2$choice = factor(m2$choice, levels = c(1,2,3,4,5), labels = c("1","2","3","4","5"))
table(restaurants.for.mlogit$stars, m2$choice)
7+67+731+547
1351/2527
#53% 

#Market shares, which again make no sense
m2$fitted = apply(fitted(m2, type = "probabilities"), 2, mean)
rbind(m2$fitted, m2$freq/sum(m2$freq))


####ODDS
exp(coef(m2))
#The odds of choosing "3 stars" over 1 star changes by a factor of 3.39 if good for kids is true. 

###MARGINAL EFFECTS 
library(zoo)
means <- with(restaurants.mlogit, data.frame(RestaurantsPriceRange2 = tapply(RestaurantsPriceRange2, index (m1, 2),mean),NoiseLevel= tapply(NoiseLevel, index(m1, 2),mean)))

effects(m2, covariate = "GoodForKids")      

effects(m2,covariate ="RestaurantsPriceRange2")

effects(m2,covariate ="review_count")

effects(m1,covariate ="lunch")

effects(m1,covariate ="brunch")

effects(m2,covariate ="NoiseLevel")



###IIA - Hausman McFadden

m2.sub <- mlogit(stars ~ 0 | GoodForKids + RestaurantsReservations + 
               Caters + NoiseLevel +  RestaurantsPriceRange2 + 
               OutdoorSeating  + RestaurantsDelivery  + dessert + latenight + lunch + 
               dinner + brunch + breakfast +  Open7days  + review_count+
               parking , reflevel = "1", alt.subset = c("2","3","4","5"), data = restaurants.mlogit)
hmftest(m2, m2.sub)
