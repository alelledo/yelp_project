#Nested logit 
restaurants.nested <- restaurants.parking
restaurants.nest.logit <- mlogit.data(data = restaurants.nested, choice = "stars", shape = "wide")
restaurants.nested$OutdoorSeating<-as.numeric(restaurants.nested$OutdoorSeating)

nl<-mlogit(stars~ 0| GoodForKids + RestaurantsReservations + 
             Caters + NoiseLevel +  RestaurantsPriceRange2 + 
             OutdoorSeating  + RestaurantsDelivery + 
             dessert + latenight + lunch + Open7days +
             dinner + brunch + breakfast  + review_count+
             parking ,
           data=restaurants.nest.logit,nests = list (onetwo=c("1","1.5","2"),three=c("2.5","3","3.5"),fourfive=c("4","4.5","5")))


nl.2<-mlogit(stars~ 0| GoodForKids + RestaurantsReservations + 
             Caters + NoiseLevel + 
             OutdoorSeating  + RestaurantsDelivery + 
             dessert + latenight + lunch + 
             dinner + brunch + breakfast  + review_count+
             parking ,
           data=restaurants.nest.logit,nests = list (onetwo=c("1","1.5","2"),three=c("2.5","3","3.5"),fourfive=c("4","4.5","5")))
summary(nl.2)
summary(nl)
AIC(nl)
lrtest(nl,nl.2)
lrtest(nl,nested.full)
AIC(nl,nested.full)

m1.Alexandros<-update(nl,nests=NULL)
lrtest(m1.Alexandros,nl)

nl$choice = apply(fitted(nl, type = "probabilities"), 1 , which.max)
nl$choice = factor(nl$choice, levels = c(1,2,3,4,5,6,7,8,9), labels = c("1","1.5","2","2.5","3","3.5","4","4.5","5"))
table(restaurants.nested$stars, nl$choice)
(2+8+17+22+67+244+412+6)/2527 #30%
str(nl)


####ODDS 
exp(coef(nl))
