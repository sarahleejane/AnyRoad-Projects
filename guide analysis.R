#analyse demographics, preferences and inactivity of guides

library(ProjectTemplate)
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/")
#getwd()
load.project()

#***guides offered versus bookings
#**get number of guides per date 
#take guides from users
guides <- subset(users, users$is_guide=="t")

#Remove dummy users
guides <- subset(guides, !grepl("test", guides$email))

#Pull St George
guides <- subset(guides, guides$id!= 732)
table(guides$active)

#Label active guides
guides$last_sign_in <- as.Date(guides$last_sign_in_at, format = '%Y-%m-%d')
guides$active <- as.Date("2014-09-02")-guides$last_sign_in
guides$active <- ifelse(guides$active > 30, "inactive", "active")

#**NEVER EDIT GUIDES AGAIN


#pull out id and created at
guides.timeline <- guides[,c("id", "created_at")]

#pull out date from created at
getDate <- function(data) {
  date_string <- substr(data$created_at, 0, 10)
  data$created_date <- as.Date(date_string, "%Y-%m-%d")
  return (data$created_date)
}

guides.timeline$created_date <- getDate(guides.timeline)

#count number of creations per date
tours.available <- as.data.frame(table(guides.timeline$created_date))
tours.available$Var1 <- as.Date(tours.available$Var1, "%Y-%m-%d")

#cumsum creations per date
#don't need to order table
str(tours.available)

#calc cumsum
tours.available$cumsum <- cumsum(tours.available$Freq)
str(tours.available)

#remove freq
tours.available$Freq <- NULL

#**get number of bookings per date
#take date instead of when booked - gives a good indication anyway - not sure when booked looks right

#clean data to get all real bookings
#remove tours with discount code - dummy bookings
bookings.clean <- subset(bookings.users.tours.slots, is.na(bookings.users.tours.slots$discount_id))

#pull st george
bookings.clean <- subset(bookings.clean, bookings.clean$guide_id != 732)

#pull out id and created at
bookings.timeline <- bookings.clean[,c("id", "date")]
bookings.timeline$date <- as.Date(bookings.timeline$date, "%Y-%m-%d")

#count number of creations per date
bookings.count <- as.data.frame(table(bookings.timeline$date))
bookings.count$Var1 <- as.Date(bookings.count$Var1, "%Y-%m-%d")

#cumsum creations per date
#don't need to order table
str(tours.available)

#calc cumsum
bookings.count$cumsum <- cumsum(bookings.count$Freq)
str(bookings.count)

#remove freq
bookings.count$Freq <- NULL

#**Melt data sets together
library(reshape)
creations.bookings <- melt(list(tours.available = tours.available, 
                                bookings.count = bookings.count), 
                           id.vars = "Var1")
  
colnames(creations.bookings)[4] <- "Key"

#plot - !!NOT GOOD, DON'T WANT CUMULATIVE BOOKINGS
library(ggplot2)
ggplot(creations.bookings, aes(x=Var1, y=value, colour = Key)) + 
  geom_line() +
  xlab("") + ylab("") 

#look for correation
#group both data sets into months
bookings.month <- bookings.timeline
tours.month <- guides.timeline

library(zoo)
bookings.month$date <- as.yearmon(bookings.month$date, "%b %Y")
bookings.month$date <- as.Date(bookings.month$date)

tours.month$created_date <- as.yearmon(tours.month$created_date, "%b %Y")
tours.month$created_date <- as.Date(tours.month$created_date) 

#get counts per month each
bookings.month <- as.data.frame(table(bookings.month$date))
head(bookings.month)
colnames(bookings.month)[2] <- "Bookings.per.Month"

tours.month <- as.data.frame(table(tours.month$created_date))
tours.month$cumsum <- cumsum(tours.month$Freq)
tours.month$Freq <- NULL
colnames(tours.month)[2] <- "Guides.signed.up" 
head(tours.month)

#merge data sets by date
bookings.month$Var1 <- as.Date(as.character(bookings.month$Var1), format="%Y-%m-%d")
tours.month$Var1 <- as.Date(as.character(tours.month$Var1), format="%Y-%m-%d")

str(bookings.month$Var1)
str(tours.month)

bookings.tours.month <- 
  as.data.frame(merge(x=tours.month, 
                      y=bookings.month, 
                      by="Var1",
                      all=TRUE))

#booking NA should be 0
str(bookings.tours.month)

bookings.tours.month$Bookings.per.Month <- 
  ifelse(is.na(bookings.tours.month$Bookings.per.Month),
         as.integer(0), bookings.tours.month$Bookings.per.Month) 

#Guides.signed.up should be same as earlier if NA
for(i in 1:nrow(bookings.tours.month)){
  if (is.na(bookings.tours.month$Guides.signed.up[i])){
    bookings.tours.month$Guides.signed.up[i] = bookings.tours.month$Guides.signed.up[i-1]
  }
} 

#bucket dates by quarter
bookings.tours.month$Timeline <- '2014 Q3-4'
levels(bookings.tours.month$Timeline) <- factor( c('2012 Q3-4', '2013 Q1-2', 
                                           '2013 Q3-4', '2014 Q1-2', '2014 Q3-4'))

bookings.tours.month[bookings.tours.month$Var1 < '2013-01-01',]$Timeline <- '2012 Q3-4'

bookings.tours.month[bookings.tours.month$Var1 >= '2013-01-01' & 
                            bookings.tours.month$Var1 < '2013-07-01',]$Timeline <- '2013 Q1-2'

bookings.tours.month[bookings.tours.month$Var1 >= '2013-07-01' & 
                            bookings.tours.month$Var1 < '2014-01-01',]$Timeline <- '2013 Q3-4'

bookings.tours.month[bookings.tours.month$Var1 >= '2014-01-01' & 
                            bookings.tours.month$Var1 < '2014-07-01',]$Timeline <- '2014 Q1-2'

bookings.tours.month[bookings.tours.month$Var1 >= '2014-07-01' & 
                            bookings.tours.month$Var1 < '2015-01-01',]$Timeline <- '2014 Q3-4'

bookings.tours.month <- subset(bookings.tours.month, bookings.tours.month$Timeline != '2014 Q3-4')

#plot for correlation
Guides_v_booking <-
ggplot(bookings.tours.month, aes(x = Guides.signed.up, y = Bookings.per.Month, 
                                 fill=Timeline)) + 
  geom_point(size = 5, pch=21, alpha=I(0.7))

#save plot
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_Guides_v_Bookings.jpg", plot=Guides_v_booking)

#***Guide demographics
#**Gender of guides
gender <- guides[ , c('gender', 'birthdate', 'active')]
gender <- subset(gender, guides$gender != "" & guides$gender != "Other")

levels(gender$gender)[levels(gender$gender)=="m" | levels(gender$gender)=="M"] <- "Male"
gender$gender <- factor(gender$gender)

gender.count <- as.data.frame(table(gender$gender))
gender.count

library(ggplot2)
gender.plot <- 
  ggplot(gender.count, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  ylab("guide count") + xlab("") +
  geom_text(data=gender.count,aes(x=Var1,y=Freq,label=Freq),vjust=-0.5, size=3) 

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_guide_gender.jpg", plot=gender.plot)

#**Gender and age of guides
#get age
info <- gender #getting errors from table & col same name

info$birthdate <- as.Date(info$birthdate, format = '%Y-%m-%d')
info$age <- round((as.Date("2014-10-03")-(info$birthdate))/365, digits=0)
info$age
info <- subset(info, !is.na(info$age) & info$age < 80 & info$age > 15 )
info$age <- as.numeric(info$age)

#split age into bins - use cut function
cut(info$age, c(0, 25, 35, 45 ,55, 65, 75, 100), 
    labels = c('< 26', '26 - 35', '36 - 45', '46 - 55', '56 - 65', '66 - 75', '> 85'))

#plot age v gender
age_v_gender <-
  ggplot(info, aes(x = age, y = gender, fill =active)) + 
  geom_point(size = 5, pch=21, alpha=I(0.7), position="jitter") +
  scale_fill_manual(values=c('green','grey')) +
  theme(legend.title=element_blank())

#save plot
setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_Age_Gender.jpg", plot=age_v_gender)


#**city of guides on a map - with colour gender, size age
city.gender.age <- guides[ , c('city', 'country', 'gender', 'birthdate', 'active')]
city.gender.age <- subset(city.gender.age, city.gender.age$city!="" & city.gender.age$country!="")
city.gender.age$citycountry <- apply(city.gender.age[ , c('city', 'country') ] , 1 , paste , collapse = "-" )
head(city.gender.age)

#get longitude & latitude for each city from geolite - remember match city and country
# GeoLiteCity.Location$citycountry <- apply(GeoLiteCity.Location[ , c('city', 'country') ] , 1 , paste , collapse = "-" )
# 
# #remove duplicate cities
# GeoLiteCity.Location <- GeoLiteCity.Location[order(GeoLiteCity.Location$citycountry), ]
# GeoLiteCity.Location$duplicate <- 1
# 
# for (i in 2:nrow(GeoLiteCity.Location)){
#   if (GeoLiteCity.Location$citycountry[i] == GeoLiteCity.Location$citycountry[i-1]){
#     GeoLiteCity.Location$duplicate[i] <- 0
#   }
# }
# 
# GeoLiteCity.Unique <- subset(GeoLiteCity.Location, GeoLiteCity.Location$duplicate==1)
# write.csv(GeoLiteCity.Unique, file = "GeoLiteCityUnique.csv")

#***go from here - saved reduced copy of city lats & longs
GeoLiteCity.reduced <- GeoLiteCityUnique[ , c('citycountry', 'latitude', 'longitude')]


#merge together
city.gender.age.ll <- as.data.frame(merge(x=city.gender.age, y=GeoLiteCity.reduced, 
                                          by="citycountry", all=FALSE))
head(city.gender.age.ll,100)

#plot map
library(maps)
library(ggplot2)
world_map <- map_data("world")

#Creat a base plot with gpplot2
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")

#Strip the map down so it looks super clean (and beautiful!)
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank()) 

base_world <- base_world_messy + cleanup

#Add simple data points to map
map_guides <- 
  base_world +
  geom_point(data=city.gender.age.ll, 
             aes(x=longitude, y=latitude, fill=active),  
             pch=21, size=3, alpha=I(0.6)) +
  geom_jitter(position="jitter") +
  scale_fill_manual(values=c('dark green', 'grey')) +
  theme(legend.position = "top", legend.title=element_blank())

map_guides

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_guide_locations.png", plot=map_guides, dpi=600)

#**guide location, gender, age on map
#clean up gender
city.gender.age.clean <- subset(city.gender.age.ll, city.gender.age.ll$gender != "" & city.gender.age.ll$gender != "Other")
levels(city.gender.age.clean$gender)[levels(city.gender.age.clean$gender)=="m" | 
                                       levels(city.gender.age.clean$gender)=="M"] <- "Male"
city.gender.age.clean$gender <- factor(city.gender.age.clean$gender)

#clean up age
city.gender.age.clean$birthdate <- as.Date(city.gender.age.clean$birthdate, format = '%Y-%m-%d')
city.gender.age.clean$age <- 
  round((as.Date("2014-10-03")-(city.gender.age.clean$birthdate))/365, digits=0)
city.gender.age.clean <- subset(city.gender.age.clean, !is.na(city.gender.age.clean$age) &
                                  city.gender.age.clean$age < 80 & city.gender.age.clean$age > 15 )
city.gender.age.clean$age <- as.numeric(city.gender.age.clean$age)

#split age into bins - use cut function
city.gender.age.clean$age.groups <- cut(city.gender.age.clean$age, c(0, 25, 45, 65, 100), 
    labels = c('< 26', '26 - 45', '46 - 65', '> 65'))


#Plot city, gender
map_guides_gender <- 
  base_world +
  geom_point(data=city.gender.age.clean, 
             aes(x=longitude, y=latitude, fill=gender), size=3, 
             colour="Black",pch=21, alpha=I(0.6)) +
  geom_jitter(position="jitter") +
  theme(legend.position = "top")


map_guides_gender

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_guide_locations_gender.png", plot=map_guides_gender, dpi=600)


#Plot city, age on map
map_guides_age <- 
  base_world +
  geom_point(data=city.gender.age.clean, 
             aes(x=longitude, y=latitude, fill=age.groups), size=3, 
             colour="Black",pch=21, alpha=I(0.6)) +
  geom_jitter(position="jitter") +
  theme(legend.position = "top")
  

map_guides_age

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_guide_locations_age.png", plot=map_guides_age, dpi=600)

#***Guide preferences
#**Guide interests
interests <- as.data.frame(guides[, 'interests'])

#remove punctuation
interests$clean <- gsub("[[:punct:]]", "\\1", as.matrix(interests))
interests <- subset(interests, interests$clean != "")

#remove stopwords & create a list of all words appearing
library(tm)
stopWords <- stopwords("en")

interests$clean1 <- 0
all.interests <- vector(mode="list")
'%nin%' <- Negate('%in%')

nzchar('l')

for (i in 1:nrow(interests)){
  t <- unlist(strsplit(interests$clean[i], " "))
  t.nostop <- t[t %nin% stopWords]
  
  if (length(t.nostop>0)){
    interests$clean1[i] <- paste(t[t %nin% stopWords], collapse=" ")
    for (j in 1:length(t.nostop)){
      if (nchar(t.nostop[j])>1){
        all.interests  <- c(all.interests, t.nostop)
      }
    }
  }
}

#get all unique words
unique.interests <- unique(unlist(tolower(all.interests)))

count.ints <- as.data.frame(sample(0, length(unique.interests), replace=TRUE))
names(count.ints) <- 'interest'
count.ints$count <- 0

#count how many guests like each interest
for (j in 1:length(unique.interests)){
  search_term <- unique.interests[j]
  grep_term <- paste(c("\\b", search_term, "\\b"), collapse="")
  #number of rows with search term in it
  counter <- length(grep(grep_term, interests$clean1, ignore.case=TRUE))
  count.ints$interest[j] <- search_term
  count.ints$count[j] <- counter
}

count.ints <- subset(count.ints, count.ints$interest!="")


#plot these words and their counts in a tag cloud
library("wordcloud")

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
png("ZZ_wc_interests.png",, width=12, height=8, units="in", res=600)
wordcloud(words = count.ints$interest, freq=count.ints$count, scale=c(6,2), 
          max.words=60, rot.per=0, colors=brewer.pal(6,"Dark2"), random.order=F)
dev.off()


#**see how many guides fall into top words
count.ints$interest <- reorder(count.ints$interest, -count.ints$count)

#get top tep
interest.top <- count.ints[order(-count.ints$count), ]
interest.top <- head(interest.top, 10)

#plot on bar graph
library(ggplot2)
guides_per_term <- 
  ggplot(interest.top, aes(x=interest, y=count)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("interest term") + ylab("number of guides interested in term") +
  geom_text(data=interest.top,aes(x=interest,y=count,label=count),vjust=-0.5, size=3) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_guides_per_interest_term.png", plot=guides_per_term)

#**Guide hobbies
interests <- as.data.frame(guides[, 'hobbies'])

#remove punctuation
interests$clean <- gsub("[[:punct:]]", "\\1", as.matrix(interests))
interests <- subset(interests, interests$clean != "")

#remove stopwords & create a list of all words appearing
library(tm)
stopWords <- stopwords("en")

interests$clean1 <- 0
all.interests <- vector(mode="list")
'%nin%' <- Negate('%in%')

nzchar('l')

for (i in 1:nrow(interests)){
  t <- unlist(strsplit(interests$clean[i], " "))
  t.nostop <- t[t %nin% stopWords]
  
  if (length(t.nostop>0)){
    interests$clean1[i] <- paste(t[t %nin% stopWords], collapse=" ")
    for (j in 1:length(t.nostop)){
      if (nchar(t.nostop[j])>1){
        all.interests  <- c(all.interests, t.nostop)
      }
    }
  }
}

#get all unique words
unique.interests <- unique(unlist(tolower(all.interests)))

count.ints <- as.data.frame(sample(0, length(unique.interests), replace=TRUE))
names(count.ints) <- 'interest'
count.ints$count <- 0

#count how many guests like each interest
for (j in 1:length(unique.interests)){
  search_term <- unique.interests[j]
  grep_term <- paste(c("\\b", search_term, "\\b"), collapse="")
  #number of rows with search term in it
  counter <- length(grep(grep_term, interests$clean1, ignore.case=TRUE))
  count.ints$interest[j] <- search_term
  count.ints$count[j] <- counter
}

count.ints <- subset(count.ints, count.ints$interest!="")


#plot these words and their counts in a tag cloud
library("wordcloud")

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
png("ZZ_wc_hobbies.png", width=12, height=8, units="in", res=600)
wordcloud(words = count.ints$interest, freq=count.ints$count, scale=c(6,2), 
          max.words=60, rot.per=0, colors=brewer.pal(6,"Dark2"), random.order=F)
dev.off()


#**see how many guides fall into top words
count.ints$interest <- reorder(count.ints$interest, -count.ints$count)

#get top tep
interest.top <- count.ints[order(-count.ints$count), ]
#interest.top <- head(interest.top, 10) - only 11 total anyway

#plot on bar graph
library(ggplot2)
guides_per_term <- 
  ggplot(interest.top, aes(x=interest, y=count)) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("interest term") + ylab("number of guides interested in term") +
  geom_text(data=interest.top,aes(x=interest,y=count,label=count),vjust=-0.5, size=3) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_guides_per_hobby_term.png", plot=guides_per_term)


#***Effectors on activity
#**Who is inactive - hasn't logged in within 30 days
guide.patterns <- guides
# guide.patterns$last_sign_in <- as.Date(guide.patterns$last_sign_in_at, format = '%Y-%m-%d')
# guide.patterns$active <- as.Date("2014-09-02")-guide.patterns$last_sign_in
# guide.patterns <- subset(guide.patterns, !is.na(guide.patterns$active))
# guide.patterns$active <- ifelse(guide.patterns$active > 30, "inactive", "active")
# table(guide.patterns$active)

#get fields filled in by guide - only take fields still available to guides

 guide.filled <- as.data.frame(guide.patterns[,c('city','interests',
                                                 'aboutme','firstname','country','hobbies',
                                                 'phone','address','email','birthdate',
                                                 'state','languages','lastname',
                                                 'tagline','active')])

#lets look at what they filled in
guide.filled$filled <- 0
for (row in 1:nrow(guide.filled)){
  count=0
  for (col in 1:ncol(guide.filled)){
    if(guide.filled[row,col] !="" & !is.na(guide.filled[row,col])){
      count = count + 1      
    }
    guide.filled$filled[row] <- count-2 #account for "filled", "active" column
  }
}

#so how can we compare active and inactive number of columns filled?
fill.analysis <- as.data.frame(guide.filled[,c('filled','active')])

#start with a simple plot to see if any pattern
library(ggplot2)
qplot(x=fill.analysis$filled, y=fill.analysis$active, position="jitter")
#so inacive don't fill the higher numbers quite as much

#if we plot a pmf it will be easier to compare- not obvious message
ggplot(fill.analysis, aes(filled, group=active, fill=active)) +
  geom_bar(aes(y=..density..), position='dodge')

#**try regression on number of filled fields to activity
fill.analysis$active.num <- 0
fill.analysis$active.num <- ifelse(fill.analysis$active == "active", 1, 0)

library(caret)
logistic.regression <- glm(active.num ~ filled, data=fill.analysis, family=binomial("logit"))
logistic.regression #shows no correlation
summary(logistic.regression)

#**try and use regression model to see if any particular cols have high impact
guide.model <- guide.filled
guide.model$filled <- NULL

#indicate if each field is filled by guide, then run regression on which attribute predicts active
for (row in 1:nrow(guide.model)){
  for (col in 1:(ncol(guide.model)-1)){
    if (is.factor((guide.model[,col]))){
      if (row==1){
        levels(guide.model[,col]) <- c(levels(guide.model[,col]), 0, 1)
      }
    }
    if(guide.model[row,col] !="" & !is.na(guide.filled[row,col])){
      guide.model[row,col] <- 1
    }
    else {
      guide.model[row,col] <- 0
    }
  }
}

#make active numerical
guide.model$active.num <- 0
guide.model$active.num <- ifelse(guide.model$active == "active", 1, 0)
guide.model$active <- NULL

head(guide.model)
str(guide.model)

#make everything a factor or 0 or 1 only
for (i in 1:ncol(guide.model)){
  guide.model[,i] <- factor(guide.model[,i])
}

#plot each field with active v inactive
#'city', 'country', 'phone', 'profession', 'tagline', 'active'

#**going to find percentage of filled v non filled, for each field, for active / inactive
#split active & inactive
guide.active <- subset(guide.model, guide.model$active.num == 1)
guide.inactive <- subset(guide.model, guide.model$active.num == 0)
guide.active$active.num <- NULL
guide.inactive$active.num <- NULL

#count filled & not filled
library(plyr)
guide.active.filled <- ldply(guide.active, function(c) sum(c=="1"))
guide.active.unfilled <- ldply(guide.active, function(c) sum(c=="0"))
guide.inactive.filled <- ldply(guide.inactive, function(c) sum(c=="1"))
guide.inactive.unfilled <- ldply(guide.inactive, function(c) sum(c=="0"))

#add labels
guide.active.filled$Activity <- "active"
guide.active.unfilled$Activity <- "active"
guide.inactive.filled$Activity <- "inactive"
guide.inactive.unfilled$Activity <- "inactive"

guide.active.filled$Field <- "filled"
guide.active.unfilled$Field <- "unfilled"
guide.inactive.filled$Field <- "filled"
guide.inactive.unfilled$Field <- "unfilled"

#find percentages
total.active <- as.integer(nrow(guide.active))
total.inactive <- nrow(guide.inactive)

guide.active1 <- rbind(guide.active.filled, guide.active.unfilled)
guide.inactive1 <- rbind(guide.inactive.filled, guide.inactive.unfilled)

for (i in 1:nrow(guide.active1)){
  guide.active1[i,2] <- guide.active1[i,2]/total.active 
}

for (i in 1:nrow(guide.inactive1)){
  guide.inactive1[i,2] <- guide.inactive1[i,2]/total.inactive 
}

#bring back into 1 table
guide.total <- rbind(guide.active1, guide.inactive1)

#This graph is good, but a percentage diff between active / inactive would be super clear
activity_effector <-
  ggplot(guide.total, aes(x=Activity, y=V1, fill=Field)) + 
  geom_bar(stat='identity') + facet_grid(.~.id) 

#setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs/")
#ggsave(filename="zz_what_effects_activity.jpg", plot=activity_effector)

#calc percentage difference: active - inactive
guide.total
guide.inactive1.fill <- subset(guide.inactive1, guide.inactive1$Field == "filled")
guide.active1.fill <- subset(guide.active1, guide.active1$Field == "filled")


guide.diff <- merge(x=guide.active1.fill, y=guide.inactive1.fill, 
                                     by=".id", all=TRUE)

guide.diff$diff <- guide.diff$V1.x - guide.diff$V1.y

#remove fields with 0 difference
guide.diff <- subset(guide.diff, guide.diff$diff!=0)

#get y positions for labels
guide.diff$posn <- ifelse(guide.diff$diff<0, 0, guide.diff$diff)

#order
guide.diff$.id <- reorder(guide.diff$.id, -guide.diff$diff)

#plot difference - maybe even for all fields?
library(scales)

activity_diff <-
  ggplot(guide.diff, aes(x=.id, y=diff, fill=diff)) + 
  geom_bar(stat='identity') +
  scale_fill_gradient(low="orangered", high="seagreen3", 
                      guide = guide_legend(title = "% diff"), labels=percent) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  geom_text(data=guide.diff,aes(x=.id,y=posn,label=.id), vjust=-1, size=2) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  scale_y_continuous(labels = percent) 

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_filled_differences.png", plot=activity_diff)


#**use regression to highlight significant fields
head(guide.model)
logistic.regression <- glm(active.num ~ city + interests + certifications + aboutme + 
                             country + hobbies + phone + school + address + birthdate + state + 
                             languages + profession + gender + tagline + degree, 
                           data=guide.model, family=binomial())
logistic.regression 
summary(logistic.regression)
#gives **country, *phone, *profession, *tagline, .languages as significant factors
#reduce deviance by 54.5

logistic.regression1 <- glm(active.num ~ country + phone + languages + profession + tagline, 
                           data=guide.model, family=binomial())
logistic.regression1 
summary(logistic.regression1)
#gives ***profession, **tagline, **languages, *country as significant
#reduce deviance by 39.6 - worse than above - so don't use

#**take a closer look at entries with significance - country, phone, profession
country <- as.data.frame(guide.filled[,c("country","active")])
names(country) <- c("country.listed", "activity")

#calc % diff for each
country.active <- subset(country, country$activity == "active")
country.active <- as.data.frame(table(country.active))

total <- sum(country.active$Freq)
country.active$percentage <- sapply(country.active$Freq, FUN=function(x)(x/total))

country.inactive <- subset(country, country$activity == "inactive")
country.inactive <- as.data.frame(table(country.inactive))

total <- sum(country.inactive$Freq)
country.inactive$percentage <- sapply(country.inactive$Freq, FUN=function(x)(x/total))

countries <- as.data.frame(merge(x=country.active, y=country.inactive, 
                                 by="country.listed", all=TRUE))
countries$diff <- countries$percentage.x - countries$percentage.y

#plot as above
#get y positions for labels
countries$posn <- ifelse(countries$diff<0, 0, countries$diff)

#order
countries$country.listed <- reorder(countries$country.listed, -countries$diff)

#remove countrie with percentage diff 0
countries <- as.data.frame(subset(countries, countries$diff > 0.01 | countries$diff < -0.01))

#plot difference - maybe even for all fields?
library(scales)

country_diff <-
  ggplot(countries, aes(x=country.listed, y=diff, fill=diff)) + 
  geom_bar(stat='identity') +
  scale_fill_gradient(low="orangered", high="seagreen3") +
  guides(fill=FALSE )+
  xlab("") + ylab("") +
  geom_text(data=countries,aes(x=country.listed,y=posn,label=country.listed),vjust=-0.5, size=3) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  scale_y_continuous(labels = percent)

setwd("C:/Users/user/Documents/AnyRoad/R Data Analysis/Bookings.Users.Tours/graphs")
ggsave(filename="ZZ_country_differences.png", plot=country_diff)