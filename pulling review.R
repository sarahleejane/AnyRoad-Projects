#***Pulling out all reviews and associated tours - to feature in campaigns

library(ProjectTemplate)
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/")
#getwd()
load.project()

#get unique tour ids and slugs
slugs.tours <- unique(bookings.users.tours.slots[, c('tour_id', 'slug')])

#get info needed from reviews
names(reviews)
review.details <- reviews[,c('tour_id', 'tour_review', 'tour_rating', 'created_at', 'tourist_id')]

#join with reviews
tours.reviews <- merge(x=review.details, y=slugs.tours, by='tour_id')
head(tours.reviews)

#pull in name and country of the reviewer
guest.details <- bookings.users.tours.slots[,c('tourist_id', 'firstname', 'country.1')]
tours.reviews <- merge(x=tours.reviews, y=guest.details, by='tourist_id')

#remove blank reviews
tours.reviews <- subset(tours.reviews, tours.reviews$tour_review!='')

#output to csv
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/diagnostics/")
write.csv(tours.reviews, file="tour reviews.csv")
