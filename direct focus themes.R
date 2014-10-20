#***Analysing booking patterns for all tours that were booked directly through AnyRoad
#Except for St George

library(ProjectTemplate)
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/")
#getwd()
load.project()

#**Clean data to get direct AnyRoad bookings
#remove stgeorge
data_excl_sg <- subset(bookings.users.tours.slots, bookings.users.tours.slots$guide_id != 732)

#remove tours not booked through anyroad
#"referrer_id" "referrer_token" "referred_from"  
#referrer_id all NA anyway
data_excl_sg <- (subset(data_excl_sg, (data_excl_sg$referred_from=="" & data_excl_sg$referrer_token=="") | grepl("anyroad", data_excl_sg$referred_from)))

#remove tours with discount code - dummy bookings
data_excl_sg <- subset(data_excl_sg, is.na(data_excl_sg$discount_id))

#just take creative - show up on AnyRoad website
data_excl_sg <- subset(data_excl_sg, data_excl_sg$creative == "t")

#**Number of tours given per guide, per tourist
library(ggplot2) 
#for guest
table_users <- as.data.frame(table(table(data_excl_sg$tourist_id)))

tours_per_guest <- 
  ggplot(table_users, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("bookings per guest") + ylab("guest count") +
  geom_text(data=table_users,aes(x=Var1,y=Freq,label=Freq),vjust=-0.5, size=3) 

#for guide
table_users1 <- as.data.frame(table(data_excl_sg$guide_id))
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="AA_bookings_per_guest.jpg", plot=tours_per_guest)

tours_per_guide <- 
  ggplot(table_users1, aes(x=Freq)) + 
  geom_histogram(binwidth = 1, colour="black", fill="white") + 
  xlab("bookings per guide") + ylab("guide count")  

ggsave(filename="BB_bookings_per_guide.jpg", plot=tours_per_guide)

#***bookings per tour
table_users2 <- as.data.frame(table(data_excl_sg$tour_id))

bookings_per_tour <- 
  ggplot(table_users2, aes(x=Freq)) + 
  geom_histogram(binwidth = 1, colour="black", fill="white") + 
  xlab("bookings per tour") + ylab("tour count")  

ggsave(filename="BBB_bookings_per_tour.jpg", plot=bookings_per_tour)

#top tours booked
#get slug info
slug.tour_id <- data_excl_sg[ , c('slug', 'tour_id')]
#remove slug duplicates
slug.tour_id <- unique(slug.tour_id)

#plot the top ten booked tours
#pull in slug
table_users2_slug <- merge(x=table_users2, y=slug.tour_id, by.x="Var1", by.y="tour_id", all=FALSE)

#order slugs for graph later
table_users2_slug$slug <- reorder(table_users2_slug$slug, -table_users2_slug$Freq)

#get top ten
table_users2_slug <- table_users2_slug[order(-table_users2_slug$Freq), ]
table_users2_slug <- subset(table_users2_slug, table_users2_slug$Freq > 3)

#prepare slugs for plot with wrap
levels(table_users2_slug$slug) <- gsub(pattern = "-", replacement = " ", x = levels(table_users2_slug$slug))

#**show top %
#order table
table_users2_slug <- table_users2_slug[order(-table_users2_slug$Freq), ]

#calc cumsum
table_users2_slug$cumsum <- cumsum(table_users2_slug$Freq)

#shift cumsum column down one for includive ranking
cumsum_vector <- table_users2_slug$cumsum
new_end <- length(cumsum_vector)-1
table_users2_slug$cumsum <- c(0,(cumsum_vector[0:new_end]))

top <- sum(table_users2_slug$Freq)*.8

table_users2_slug$ranking <- ifelse(table_users2_slug$cumsum < top,"Top 80%","")

#plot these beautifully
library(stringr)

most_booked_tours_by_slug <- 
  ggplot(table_users2_slug, aes(x=slug, y=Freq, fill=ranking)) + 
  geom_bar(stat="identity", colour="black") + 
  xlab("Top Tours") + ylab("Number of Bookings") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 1)) +
  geom_text(data=table_users2_slug,aes(x=slug,y=Freq,label=Freq),vjust=-0.5, size=3) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("grey", "white"))

ggsave(filename="BBBB_top ten tours.jpg", plot=most_booked_tours_by_slug)


#**Number of bookings per country
#Fix - consistent 2 letter codes
levels(data_excl_sg$country)[levels(data_excl_sg$country)=="Turkey"] <- "TR"
levels(data_excl_sg$country)[levels(data_excl_sg$country)=="Brazil"] <- "BR"

table_country <- as.data.frame(table(data_excl_sg$country, exclude = ""))

#Remove freq 0,1
table_country <- subset(table_country, table_country$Freq != 0 & table_country$Freq!=1)

#order by frequency
table_country$Var1 <- reorder(table_country$Var1, -table_country$Freq)

#**show top %
#3**lets try cumulative sum - then colour code top 80%
#order table
table_country <- table_country[order(-table_country$Freq), ]

#calc cumsum
table_country$cumsum <- cumsum(table_country$Freq)

#shift cumsum column down one for includive ranking
cumsum_vector <- table_country$cumsum
new_end <- length(cumsum_vector)-1
table_country$cumsum <- c(0,(cumsum_vector[0:new_end]))

top <- sum(table_country$Freq)*.8

table_country$ranking <- ifelse(table_country$cumsum < top,"Top 80%","")

#plot
guides_per_country <-
  ggplot(table_country, aes(x=Var1, y=Freq, fill=ranking)) + 
  geom_bar(stat="identity", colour="black") + 
  xlab("tours per country") + ylab("tour count") +
  geom_text(data=table_country,aes(x=Var1,y=Freq,label=Freq),vjust=-0.5, size=3) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("grey", "white"))

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="CC_guides_per_country.jpg", plot=guides_per_country)

#**Time of year of tours taken
data_excl_sg$date.formatted <- as.Date(data_excl_sg$date)
data_excl_sg$year.deduced <- 0

#remove odd looking dates?
library('zoo')
data_excl_sg$year.deduced <- as.yearmon(data_excl_sg$date.formatted, "%b %Y")
data_excl_sg$year.deduced <- format(data_excl_sg$year, "%Y")
#don't need to remove any years this time

table_all_dates <- as.data.frame(table(data_excl_sg$date.formatted))
table_all_dates$Var1 <- as.Date(table_all_dates$Var1)

#plot line graph over continuous dates 
guides_per_date <-
  ggplot(table_all_dates, aes(x=Var1, y=Freq)) + 
  geom_point(color="firebrick") +
  xlab("tours per date") + ylab("tour count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="PP_guides_per_date_without_line.jpg", plot=guides_per_date)


#**Types of tour taken
tour_types <- data_excl_sg[, c('art_architecture', 'food_drinks', 'historical', 'outdoors_nature', 'active_adventure', 'cultural_religious', 'general_area', 'event', 'fashion_design', 'nightlife', 'layover_tour')]

#count number of t's in each column
library(plyr)
count_each_t <- ldply(tour_types, function(c)sum(c=="t"))

#order factors and dataframe descending
count_each_t$.id <- reorder(count_each_t$.id, -count_each_t$V1)
count_each_t <- count_each_t[order(-count_each_t$V1),]

#**plot cumulative percentage
count_each_t$V1 <- as.numeric(count_each_t$V1)
count_each_t$cumsum <- cumsum(count_each_t$V1)

#shift cumsum column down one for inclusive ranking
cumsum_vector <- count_each_t$cumsum
new_end <- length(cumsum_vector)-1
count_each_t$cumsum <- c(0,(cumsum_vector[0:new_end]))

top <- sum(count_each_t$V1)*.8
count_each_t$ranking <- ifelse(count_each_t$cumsum < top,"Top 80%","")

tours_per_type <- 
  ggplot(count_each_t, aes(x=.id, y=V1, fill=ranking)) + 
  geom_bar(stat="identity", colour="black") + 
  xlab("tours per type") + ylab("tour count") +
  geom_text(data=count_each_t,aes(x=.id,y=V1,label=V1),vjust=-0.5, size=3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("grey", "white"))

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="EE_tours_per_type.jpg", plot=tours_per_type)


#**tour repeats per guest - how many guests repeat a tour at all?
tour_and_tourist <- data_excl_sg[, c('tour_id', 'tourist_id')]
cols <- c('tour_id', 'tourist_id')

#join columns together
tour_and_tourist$merged <- apply( tour_and_tourist[ , cols ] , 1 , paste , collapse = "-" )
tour_and_tourist.repeats <- as.data.frame(table(tour_and_tourist$merged))

#tour_and_tourist.repeats <- subset(tour_and_tourist.repeats, Freq > 1)
table.tour_and_tourist.repeats <- as.data.frame(table(tour_and_tourist.repeats$Freq))

#plot histogram of tours repeated by users
number_of_tour_repeats <-
  ggplot(table.tour_and_tourist.repeats, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("number of bookings of one tour by a guest") + ylab("") +
  geom_text(data=table.tour_and_tourist.repeats,aes(x=Var1, y=Freq,label=Freq),
            vjust=-0.5, size=3)

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="FF_tours_repeat_booking.jpg", plot=number_of_tour_repeats)

#try to get repeat per tour
tour_and_tourist.repeats <- subset(tour_and_tourist.repeats, Freq > 1)

#tour_id is before -
tours_repeats <- tour_and_tourist.repeats
tours_repeats$tour_id <- 0

getTourId <- function(data) {
  dash.start <- regexpr("-", data$Var1, TRUE)
  data$tour_id <- as.numeric(substr(data$Var1, 0, dash.start-1))
  return (data$tour_id)
}

tours_repeats$tour_id <- getTourId(tours_repeats)

#want slug and number of times it appears in this table
#pull in tour slug
slug.tour_id <- data_excl_sg[ , c('slug', 'tour_id')]

#remove slug duplicates
slug.tour_id <- unique(slug.tour_id)

#merge tables
tour_repeats_slug <- as.data.frame(merge(x=tours_repeats, y=slug.tour_id, by="tour_id", all=FALSE))
count_slugs <- as.data.frame(table(tour_repeats_slug$slug))

#remove 0 slugs
count_slugs <- subset(count_slugs, count_slugs$Freq>0)

#prepare slugs for plot with wrap
levels(count_slugs$Var1) <- gsub(pattern = "-", replacement = " ", x = levels(count_slugs$Var1))

#order factors
count_slugs$Var1 <- reorder(count_slugs$Var1, -count_slugs$Freq)

#plot bar graph of freq of repeated bookings by guest
library(stringr)

tour_repeats_by_slug <- 
  ggplot(count_slugs, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("") + ylab("Number of times a guest took the tour twice") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 1)) +
  geom_text(data=count_slugs,aes(x=Var1,y=Freq,label=Freq),vjust=-0.5, size=3) 

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="FFF_tour_slugs_with_repeat_bookings.pdf", plot=tour_repeats_by_slug, 
       width = 14, height = 8, dpi=100)

#In what countries are tours available?
#only consider enabled tours
tours.enabled <- subset(tours, tours$is_enabled == "t")

#get rid of stgeorge
tours.enabled <- tours.enabled[tours.enabled$id != 663&tours.enabled$id != 929&tours.enabled$id != 953&tours.enabled$id != 952,]

#just take creative - show up on AnyRoad website
tours.enabled <- subset(tours.enabled, tours.enabled$creative == "t")

#get rid of country ""
tours.country <- subset(tours.enabled, tours.enabled$country!="")

#remove factors with 0,1 country count
tours.country <- as.data.frame(table(tours.country$country))
tours.country <- subset(tours.country, tours.country$Freq!=0)
tours.country <- subset(tours.country, tours.country$Freq!=1)
tours.country <- subset(tours.country, tours.country$Freq!=2)

#Fix - consistent 2 letter codes
levels(tours.country$Var1)
levels(tours.country$Var1)[levels(tours.country$Var1)=="Turkey"] <- "TR"
levels(tours.country$Var1)[levels(tours.country$Var1)=="Brazil"] <- "BR"
levels(tours.country$Var1)[levels(tours.country$Var1)=="Poland"] <- "PO"
levels(tours.country$Var1)[levels(tours.country$Var1)=="South Africa"] <- "ZA"
levels(tours.country$Var1)[levels(tours.country$Var1)=="United Kingdom"] <- "UK"
levels(tours.country$Var1)[levels(tours.country$Var1)=="U.S. Virgin Islands"] <- "VI"

#order by frequency
tours.country$Var1 <- reorder(tours.country$Var1, -tours.country$Freq)

#**show top %
table_country <- tours.country

#order table
table_country <- table_country[order(-table_country$Freq), ]

#calc cumsum
table_country$cumsum <- cumsum(table_country$Freq)

#shift cumsum column down one for includive ranking
cumsum_vector <- table_country$cumsum
new_end <- length(cumsum_vector)-1
table_country$cumsum <- c(0,(cumsum_vector[0:new_end]))

top <- sum(table_country$Freq)*.8

table_country$ranking <- ifelse(table_country$cumsum < top,"Top 80%","")
all_countries_offered <- table_country #for mapping points for later
table_country <- subset(table_country, table_country$ranking == "Top 80%")

#plot
tours_offered_per_country <-
  ggplot(table_country, aes(x=Var1, y=Freq, fill=ranking)) + 
  geom_bar(stat="identity", colour="black") + 
  xlab("countries where 80% of all tours are offered") + ylab("tour count") +
  geom_text(data=table_country,aes(x=Var1,y=Freq,label=Freq),vjust=-0.5, size=3) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values=c("white"))

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="CCC_tours_offered_per_country.jpg", plot=tours_offered_per_country)


#**Billing address of guests - vs tour address?
country.guests <- as.data.frame(table(data_excl_sg$country.1))
#remove 0 Freq, ""
country.guests <- subset(country.guests, country.guests$Freq > 0)
country.guests <- subset(country.guests, country.guests$Var1 != "")
country.guests$Var1 <- factor(country.guests$Var1)

#order by frequency
country.guests$Var1 <- reorder(country.guests$Var1, -country.guests$Freq)

#**show top %
#order table
country.guests <- country.guests[order(-country.guests$Freq), ]
country.guests

#calc cumsum
country.guests$cumsum <- cumsum(country.guests$Freq)

#shift cumsum column down one for includive ranking
cumsum_vector <- country.guests$cumsum
new_end <- length(cumsum_vector)-1
country.guests$cumsum <- c(0,(cumsum_vector[0:new_end]))

top <- sum(country.guests$Freq)*.8

country.guests$ranking <- ifelse(country.guests$cumsum < top,"Top 80%","")

#plot
address_per_country <-
  ggplot(country.guests, aes(x=Var1, y=Freq, fill=ranking)) + 
  geom_bar(stat="identity", colour="black") + 
  xlab("guest's home country") + ylab("guest count") +
  geom_text(data=country.guests,aes(x=Var1,y=Freq,label=Freq),vjust=-0.5, size=3) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("grey", "white"))

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="address_per_country.jpg", plot=address_per_country)


#**Do guests take holidays where they live?
#take booking id, tour id, address id
home.holiday <- data.frame(data_excl_sg$id, data_excl_sg$country, data_excl_sg$country.1)
names(home.holiday) <- c('booking_id', 'tour_country', 'home_country')

#remove blanks
home.holiday <- subset(home.holiday, home.holiday$tour_country != "" & home.holiday$home_country != "")

#calc if away or home
home.holiday$location <- ifelse(as.character(home.holiday$tour_country) == as.character(home.holiday$home_country),'home','away')

#count aways and homes
away.home <- (count(home.holiday$location))

#also count for US only, since we have a focus on US
home.holiday.us <- subset(home.holiday, home.holiday$tour_country == "US")
away.home.us <- (count(home.holiday.us$location))

#plot
away.home.plot <-
  ggplot(away.home, aes(x=x, y=freq)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("for all tours") + ylab("guest count") +
  geom_text(data=away.home,aes(x=x,y=freq,label=freq),vjust=-0.5, size=3) +
  theme(legend.position="none") 

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="home away.jpg", plot=away.home.plot)

away.home.us.plot <-
  ggplot(away.home.us, aes(x=x, y=freq)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("for US tours") + ylab("guest count") +
  geom_text(data=away.home.us,aes(x=x,y=freq,label=freq),vjust=-0.5, size=3) +
  theme(legend.position="none") 

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="home away us.jpg", plot=away.home.us.plot)

#plot number who booked home per country
home.only <- subset(home.holiday, home.holiday$location == "home")
home.only.count <- as.data.frame(table(home.only$tour_country))
home.only.count <- subset(home.only.count, home.only.count$Freq!=0)
home.only.count$Var1 <- reorder(home.only.count$Var1, -home.only.count$Freq)

#plot away and home for each country on map
home.only.plot <-
  ggplot(home.only.count, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("countries where tours taken at home") + ylab("tour count") +
  geom_text(data=home.only.count, aes(x=Var1,y=Freq,label=Freq),vjust=-0.5, size=3) +
  theme(legend.position="none") 

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
ggsave(filename="home only.jpg", plot=home.only.plot)
