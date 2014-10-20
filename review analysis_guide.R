#analysing impact of guide reviews on number of future bookings 

library(ProjectTemplate)
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/")
load.project()

#**Reviews per guide versus Bookings per guide

#reviews per guide
reviews$guide.text.reviewed <- 0
reviews$guide.text.reviewed[reviews$guide_review != ""] <- 1
guide.text.reviewed <- as.data.frame(table(reviews$guide.text.reviewed, reviews$guide_id))
guide.text.reviewed <- subset(guide.text.reviewed, Var1 == 1)
guide.text.reviewed$Var1 <- NULL
colnames(guide.text.reviewed) <- c("Guide_id", "Number_of_Guide_Reviews")

#bookings per guide
#only want to look at bookings through anyroad - for which we have review info
bookings_nonref <- subset(bookings, referred_from=="")

guide.bookings <- as.data.frame(table(bookings_nonref$guide_id))
colnames(guide.bookings) <- c("Guide_id", "Number_of_Bookings")

#ignore stgeorge
guide.bookings <- subset(guide.bookings, Guide_id != 732)
guide.text.reviewed <- subset(guide.text.reviewed, Guide_id != 732)

#join tables
guide.bookings.v.reviews <- merge(guide.text.reviewed, guide.bookings, by="Guide_id", all = FALSE)
#guide.bookings.v.reviews$Guide_id <- factor(guide.bookings.v.reviews$Guide_id, levels=guide.bookings.v.reviews$Guide_id[order(guide.bookings.v.reviews$Number_of_Bookings)])


#get r squared
res=lm(Number_of_Guide_Reviews ~ Number_of_Bookings, guide.bookings.v.reviews)
names(summary(res))
r.percent <- paste(round(100*summary(res)$r.squared, 2), "%", sep="")
r.percent <- paste ("R squared =", r.percent, sep = " ")

#get p
p.value <- format(round(anova(res)$'Pr(>F)'[1],3), nsmall=3)
p.value <- paste("P value =", p.value, sep = " ")

#plot against each other
guide.bookings_v_guide.reviews <-
  ggplot(guide.bookings.v.reviews, aes(x=Number_of_Guide_Reviews, y=Number_of_Bookings)) + 
  geom_point(colour = "purple") +
  geom_smooth(method=lm) + #linear regression line with 95% confidence region
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Guide review count") + ylab("Guide booking count") +
  annotate("text", x = 2, y = 400, label = r.percent) +
  annotate("text", x = 2, y = 350, label = p.value)

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="XX_guide bookings v reviews.jpg", plot=guide.bookings_v_guide.reviews)


