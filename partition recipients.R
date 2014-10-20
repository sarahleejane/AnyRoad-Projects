#***Takes all users, segments, and divides segments into test groups for email campaign

library(ProjectTemplate)
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/")
#getwd()
load.project()

#**Taking all bookings - want to know where they came from and evenly distribute to A B
data_excl_sg <- bookings.users.tours.slots

#remove tours with discount code - dummy bookings
data_excl_sg <- subset(data_excl_sg, is.na(data_excl_sg$discount_id))

#label stgeorge
data_excl_sg$bsource <- ifelse(data_excl_sg$guide_id == 732,"St George", "Indirect")

#label anyroad direct tours 
table(data_excl_sg$bsource)

data_excl_sg[(data_excl_sg$referred_from=="" &
    data_excl_sg$referrer_token=="") |
  grepl("anyroad", data_excl_sg$referred_from),]$bsource <- "Direct"

#**First name, email, city, date of tour, billing address country, source
user.details <- data_excl_sg[ , c('email',
                                  'firstname', 
                                  'city',
                                  'tourist_id',
                                  'date',
                                  'country.1',
                                  'bsource',
                                  'country')]

#next step - for duplicate users, take latest tour
head(user.details[,c('tourist_id', 'date')],50)

#order by tourist_id, then date
user.details <- user.details[order(user.details$tourist_id, -as.numeric(user.details$date)), ]

#run through, removing second tourist_ids
for (i in 2:nrow(user.details)){
  if (user.details$tourist_id[i] == user.details$tourist_id[i-1]){
    user.details$tourist_id[i] <- 0
  }
}

user.details <- subset(user.details, user.details$tourist_id != 0)

#split into review/no review - even #s per direct, indirect, st george
library(caret)
set.seed(1)
review.rows <- createDataPartition(user.details$bsource,
                                     p = 0.5, list = FALSE)
review.batch <- user.details[review.rows, ]
no.review.batch <- user.details[-review.rows, ]

#output to csv
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/diagnostics/")
write.csv(no.review.batch, file="featured tours_no review.csv")
write.csv(review.batch, file="featured tours_review.csv")
