#Nested logit 
restaurants.nested <- restaurants.parking
restaurants.nest.logit <- mlogit.data(data = restaurants.nested, choice = "stars", shape = "wide")
nl<-mlogit(stars~ 0| + GoodForKids + RestaurantsReservations
           +Caters + NoiseLevel + RestaurantsPriceRange2 + OutdoorSeating
           +RestaurantsDelivery + dessert + review_count
           +latenight + lunch + dinner + brunch  + parking,
           data=restaurants.nest.logit,nests = list (onetwo=c("1","1.5","2","2.5"),three=c("3","3.5"),fourfive=c("4","4.5","5")))
summary(nl)


#LL -4202.4, r2 0.089718

nested.full <- mlogit(stars ~ 0 | state+Caters+RestaurantsPriceRange2+
                        GoodForKids+NoiseLevel+OutdoorSeating+RestaurantsDelivery+RestaurantsReservations
                      +parking+lunch+dessert+review_count+
                        breakfast+dinner+latenight+brunch ,data = restaurants.nest.logit,
                      nests = list (onetwo=c("1","1.5","2"),three=c("2.5","3","3.5"),fourfive=c("4","4.5","5")))
summary(nested.full)

lrtest(nl, nl.2)
AIC(nl, nl.2)

lrtest(nl,nl.3)
AIC(nl,nl.3)


nl.2<-mlogit(stars~ 0| + GoodForKids + RestaurantsReservations
           +Caters + NoiseLevel + RestaurantsPriceRange2 + OutdoorSeating
           +RestaurantsDelivery + dessert + review_count
           +latenight + lunch + dinner + brunch  + parking,
           data=restaurants.nest.logit,nests = list (onetwo=c("1","1.5","2"),three=c("2.5","3","3.5"),fourfive=c("4","4.5","5")))
summary(nl)

nl.3 <- mlogit(stars~ 0| + GoodForKids + RestaurantsReservations
               +Caters + NoiseLevel + RestaurantsPriceRange2 + OutdoorSeating
               +RestaurantsDelivery + dessert + review_count
               +latenight + lunch + dinner + brunch  + parking,
               data=restaurants.nest.logit,nests = list (onetwo=c("1","1.5","2","2.5"),three=c("2.5","3","3.5","4","4.5","5")))

#####HITRATE 

nl.2$choice = apply(fitted(nl.2, type = "probabilities"), 1 , which.max)
nl.2$choice = factor(nl.2$choice, levels = c(1,2,3,4,5,6,7,8,9), labels = c("1","1.5","2","2.5","3","3.5","4","4.5","5"))
table(restaurants.nested$stars, nl.2$choice)
(2+8+17+18+68+246+416+6)/2527 #30%

####ODDS 
exp(coef(nl.2))


#MARGINAL EFFECTS 

effects(nl.2, covariate = GoodForKids)
