#pull all direct bookng tours into csv file - with number of bookings - to choose tours for feature

library(ProjectTemplate)
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/")
load.project()

#only want to look at bookings through anyroad - for which we have review info
bookings_nonref <- subset(bookings.tours, referred_from=="")

slug.bookings <- as.data.frame(table(bookings_nonref$slug))
str(slug.bookings)

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/diagnostics/")
sink('top tours.txt')
head(slug.bookings[order(-slug.bookings$Freq),])
sink()

sink('top tours.csv')
slug.bookings
sink()
