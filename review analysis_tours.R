#analysing impact of tour reviews on number of future bookings 

library(ProjectTemplate)
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/")
load.project()

#**Reviews per guide versus Bookings per guide

#reviews per guide
reviews$tour.text.reviewed <- 0
reviews$tour.text.reviewed[reviews$tour_review != ""] <- 1
tour.text.reviewed <- as.data.frame(table(reviews$tour.text.reviewed, reviews$tour_id))
tour.text.reviewed <- subset(tour.text.reviewed, Var1 == 1)
tour.text.reviewed$Var1 <- NULL
colnames(tour.text.reviewed) <- c("Tour_id", "Number_of_Tour_Reviews")

#bookings per guide
#only want to look at bookings through anyroad - for which we have review info
bookings_nonref <- subset(bookings, referred_from=="")

tour.bookings <- as.data.frame(table(bookings_nonref$tour_id))
colnames(tour.bookings) <- c("Tour_id", "Number_of_Bookings")
(tour.bookings[with(tour.bookings, order(Number_of_Bookings)), ]
str(tour.bookings)


#ignore stgeorge
tour.bookings <- subset(tour.bookings, Tour_id != 953)
tour.text.reviewed <- subset(tour.text.reviewed, Tour_id != 953)

#join tables
tour.bookings.v.reviews <- merge(tour.text.reviewed, tour.bookings, by="Tour_id", all = FALSE)


#get r squared
res=lm(Number_of_Tour_Reviews ~ Number_of_Bookings, tour.bookings.v.reviews)
r.percent <- paste(round(100*summary(res)$r.squared, 2), "%", sep="")
r.percent <- paste ("R squared =", r.percent, sep = " ")
summary(res)

#get p
p.value <- format(round(anova(res)$'Pr(>F)'[1],3), nsmall=43)
p.value <- paste("P value =", p.value, sep = " ")

#plot against each other
tour.bookings_v_tour.reviews <-
  ggplot(tour.bookings.v.reviews, aes(x=Number_of_Tour_Reviews, y=Number_of_Bookings)) + 
  geom_point(colour = "purple") +
  geom_smooth(method=lm) + #linear regression line with 95% confidence region
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Tour review count") + ylab("Tour booking count") +
  annotate("text", x = 2, y = 400, label = r.percent) +
  annotate("text", x = 2, y = 350, label = p.value)

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="YY_tour bookings v reviews.jpg", plot=tour.bookings_v_tour.reviews)


