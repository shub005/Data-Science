
#######################################################################################################
#  Recommendation System Assignment- Santosh Kumar:shub005@gmail.com
#######################################################################################################
#  Problem Statement
#  Sales manager for "BeerMart", an online beer store in the United States. 
#  Requirement to build a recommendation system (collaborative) for the store, where customers will be recommended the beer that they are most likely to buy. 
#  Collected the data about the ratings that the customers have provided in the past. 

#######################################################################################################
setwd("C:/Users/Ranjita/Desktop/Data Analytics_IITM/Elective_Ecommerce/3_Assignment - Recommendation Systems")
beer<-read.csv('beer_data.csv')
install.packages("recommenderlab")
library(dplyr)
library(ggplot2)
library(recommenderlab)

View(beer)
str(beer) 
# beer_beeridid - Integer
# review_overall - Numeric
# review_profilename(name of reviewer) - Factor
summary(beer) #review_profilename have missing values, reviews are in range 0-5
dim(beer) #475984 user rating & 3 features

#Removing missing values
nrow(beer[(is.na(beer$review_profilename) | beer$review_profilename==""), ]) #Total 100 reviews have empty review_profilename
beer<-beer[!(beer$review_profilename==""), ]# Removed empty review_profilename

#Checking for duplicates where both item and user  or same user has given multiple ratings to same beer 
beer[!(duplicated(beer[c("beer_beerid","review_profilename")]) | duplicated(beer[c("beer_beerid","review_profilename")], fromLast = TRUE)), ] %>% nrow()
beer %>% distinct(beer_beerid,review_profilename,.keep_all = TRUE) %>% nrow() #474462 distict reviews
#Removing duplicates having both beer & user duplicated. Therefore, no two reviews from single user  are counted
beer<-distinct(beer,beer_beerid,review_profilename,.keep_all = TRUE)

#######################################################################################################
######   Data Prepration ########
#######################################################################################################
#  Choose only those beers that have at least N number of reviews
  #  Figure out an appropriate value of N using EDA; 
  #  this may not have one correct answer, but you shouldn't choose beers having extremely low number of ratings


# finding out all distinct beers & total reviews they received
beer_reviews_count <- beer %>% group_by(beer_beerid) %>% summarise(total_beer_reviews=n())
dim(beer_reviews_count) #Total 40308 distinct beers
#  Therefore,  8.46% of total reviews beers are unique
summary(beer_reviews_count) #Beers have reviews count in range of 1 to 977

#Finding  number of distinct users
beer %>% group_by(review_profilename) %>% summarise(total_user_reviews=n()) %>% nrow() #22497 distinct users

#Checking which user have reviewed maximum beers
beer %>% group_by(review_profilename) %>% summarise(user_review_count=n()) %>% top_n(1)
# northyorksammy have given max reviews:1842 

# Total review count to decide appropiate value of N
qplot(beer_reviews_count$total_beer_reviews, geom = "histogram") 
#  Most of the beer have single review
beer_reviews_count %>% subset(total_beer_reviews==1) %>% dim() 
#  44.7% beers have only 1 review.

#Creating seprate dataframe with frequency of count of reviews
review_frequency<-beer_reviews_count %>% group_by(total_beer_reviews) %>% summarise(review_occurance=n())
review_frequency
ggplot(review_frequency,aes(x=total_beer_reviews,y=review_occurance)) + geom_point() 
#Review  drops by almost half with each increase in total_beer_review

#Removing  beers with single review & check remaining data distribution
review_frequency_subset <- review_frequency %>% subset(total_beer_reviews!=1)
ggplot(review_frequency,aes(x=total_beer_reviews,y=review_occurance)) + geom_point()
qplot(review_frequency_subset$review_occurance, geom = "histogram")  # Total  reviews per beer drop at almost fixed rate with increase in number of reviews

#Calculating ratio of review count to total reviews to verify this
review_frequency$review_ratio<-(review_frequency$review_occurance*100)/40308
ggplot(review_frequency,aes(x=total_beer_reviews,y=review_ratio)) + geom_point() #still large ratios are supressing everything

#Plotting records with review_ratio<1, Beyond row 11 is being taken
ggplot(review_frequency[-c(1:10),],aes(x=total_beer_reviews,y=review_ratio)) + geom_point()
ggplot(review_frequency[-c(1:10),],aes(x=total_beer_reviews,y=review_ratio)) + geom_point() + geom_jitter(width = .5,height = .25)
#  Almost constant after ratio drops near .004

#Lets further filter this to ratios less then .004
review_frequency_subset<-subset(review_frequency,review_frequency$review_ratio<.004)
ggplot(review_frequency_subset,aes(x=total_beer_reviews,y=review_ratio)) + geom_point() 
ggplot(review_frequency_subset[-c(1:10),],aes(x=total_beer_reviews,y=review_ratio)) + geom_point() + geom_jitter(width = 6000,height = 0.02)
View(review_frequency_subset)
#So roughly after N=135 the review ratio almost becomes constant

# value of N:
beerall_rrmatrix <- as(beer[,c(2,1,3)], "realRatingMatrix")
summary(rowCounts(beerall_rrmatrix,method="Z-score")) #Average rating per user, Mean:21.09, Median: 3
summary(colCounts(beerall_rrmatrix,method="Z-score")) #Average rating per beer, Mean:11.77, Median: 2

#Filtering by beers we can take beers having atleast 50 reviews & users having reviewed atleast 50 beers as there are more 
beer_reviews_count_subset<-subset(beer_reviews_count,beer_reviews_count$total_beer_reviews>=50)
#Now we are left with a dataset of these distinct beer ids each having more than 50 reviews each
ggplot(beer_reviews_count_subset,aes(x=total_beer_reviews)) + geom_bar()
table(beer_reviews_count_subset$total_beer_reviews) #Checking frequency of total_reviews

#Filtering beer dataset based on users who have atleast reviewd 50 beers each
user_reviews_count<- beer %>% group_by(review_profilename) %>% summarise(total_user_reviews=n()) 
user_reviews_count_subset<-subset(user_reviews_count,user_reviews_count$total_user_reviews>=50)

#Filtering original data by these beer and user ids
important_beers<-merge(beer,beer_reviews_count_subset,by.x="beer_beerid",by.y="beer_beerid")
important_beers<-merge(important_beers,user_reviews_count_subset,by.x="review_profilename",by.y="review_profilename")

summary(important_beers) 
#This beer subset have adequate ratings by both users & beers 


#######################################################################################################
#2. Convert this data frame to a "realratingMatrix" before you build your collaborative filtering models
#######################################################################################################

beers_rrmatrix <- as(important_beers[,c(1,2,3)], "realRatingMatrix")
class(beers_rrmatrix)


#Converting  the matrix to a dataframe
beers_df <- as(beers_rrmatrix, "data.frame")
str(beers_df)
summary(beers_df)

#######################################################################################################
################ II-Data Exploration #########################
#######################################################################################################

#1. Determine how similar the first ten users are with each other and visualise it

similar_users <- similarity(beers_rrmatrix[1:10,],method = "cosine",which = "users") 

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#######################################################################################################
#2. Compute and visualise the similarity between the first 10 beers

similar_beers <- similarity(beers_rrmatrix[,1:10],method = "cosine",which = "items")

#Similarity matrix
as.matrix(similar_beers)

#Visualise similarity matrix
image(as.matrix(similar_beers), main = "Beer similarity")

#######################################################################################################
#3. What are the unique values of ratings?
beers_df %>% group_by(rating) %>% summarise(rating_frequency=n()) %>% nrow() 
# 9 distinct ratings
beers_df %>% group_by(rating) %>% summarise(rating_frequency=n()) %>% View()
#rating 4.0 & 4.5 are most common, 1.0 & 1.5 are least common

#######################################################################################################
# 4. Visualise the rating values and notice:
#(i) The average beer ratings
avg_beer_ratings<-beers_df %>% group_by(item) %>% summarise(average_rating=mean(rating))
ggplot(avg_beer_ratings,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="# of Beers")
summary(avg_beer_ratings$average_rating)
#Mean=3.804 & Median=3.867, left skewed

#Also checking on original full dataset of beers
avg_beer_ratings_all<-beer %>% group_by(beer_beerid) %>% summarise(average_rating=mean(review_overall))
ggplot(avg_beer_ratings_all,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="# of Beers")
summary(avg_beer_ratings_all$average_rating)
# Mean=3.8 & Median=3.671, uneven distribution

#(ii) The average user ratings
avg_user_ratings<-beers_df %>% group_by(user) %>% summarise(average_rating=mean(rating))
ggplot(avg_user_ratings,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="Number of Users")
summary(avg_user_ratings$average_rating)
#Mean=3.871 & Median=3.894, left skewed

#Also checking on original full dataset of beers
avg_user_ratings_all<-beer %>% group_by(review_profilename) %>% summarise(average_rating=mean(review_overall))
ggplot(avg_user_ratings_all,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="# of Users")
summary(avg_user_ratings_all$average_rating)
#Mean=3.991 & Median=4, uneven distribution

#(iii) The average number of ratings given to the beers
avg_beer_reviews<-important_beers %>% group_by(beer_beerid) %>% summarise(average_reviews=mean(total_beer_reviews))
ggplot(avg_beer_reviews,aes(x=average_reviews)) + geom_histogram() + labs(x="Average Rating", y="# of Beers")
summary(avg_beer_reviews$average_reviews)
# average each beer gets Mean- 143 Reviews 

#From  full dataset of beers
avg_user_ratings_all<-beer_reviews_count %>% group_by(beer_beerid) %>% summarise(average_rating=mean(total_beer_reviews))
ggplot(avg_user_ratings_all,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="# of Users")
summary(avg_user_ratings_all$average_rating)
#So on average each beer gets  Mean= 11.77 Reviews

#(iv) The average number of ratings given by the users
avg_user_reviews<-important_beers %>% group_by(review_profilename) %>% summarise(average_reviews=mean(total_user_reviews))
ggplot(avg_user_reviews,aes(x=average_reviews)) + geom_histogram()
summary(avg_user_reviews$average_reviews)
# Each user gives at an average 158 reviews, but the distribution is skewed

# From full dataset of beers
avg_user_ratings_all<-user_reviews_count %>% group_by(review_profilename) %>% summarise(average_rating=mean(total_user_reviews))
ggplot(avg_user_ratings_all,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="# of Users")
summary(avg_user_ratings_all$average_rating)
# Each user gives at an average 21.09 reviews, but the distribution is skewed


#######################################################################################################
############### III- Recommendation Models   ######################
#######################################################################################################
#1. Dividing data into training and testing datasets. Experiment with 'split' and 'cross-validation' evaluation schemes
#i) Scheme1 with train/test(90/10) using split without cross validation & goodRating as 4  
scheme1 <- evaluationScheme(beers_rrmatrix, method = "split", train = .75,k = 1, given = -1, goodRating = 4)
scheme1
#ii) Scheme2 using cross-validation without cross validation(5 folds) & goodRating as 4 
scheme2 <- evaluationScheme(beers_rrmatrix, method = "cross-validation",k = 5, given = -1, goodRating = 4)
scheme2

#######################################################################################################
#2. Building IBCF and UBCF models 
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))

#Evaluating algorithms & predicting next n beers
results1 <- evaluate(scheme1, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results1)

results2 <- evaluate(scheme2, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results2)

#######################################################################################################
#3. Comparing the performance of the two models and suggest the one that should be deployed
#ROC curves for UBCF and IBCF
plot(results1, annotate = 1:4, legend="topleft")
plot(results2, annotate = 1:4, legend="topleft")
# UBCF is better then IBCF with higher values of n

#######################################################################################################
#4. Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"
# Making Recomendations using UBCF 
r <- Recommender(beers_rrmatrix, method = "UBCF") 
r

recom_cokes <- predict(r, beers_rrmatrix['cokes'], n=5)
#recom_cokes <- predict(r, beers_rrmatrix['cokes'], n=5,param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
#recom_cokes_best5<-bestN(recom_cokes, n=5)
as(recom_cokes, "list")
#recommendation for cokes: "7971"  "47658" "1158"  "1717"  "1339" 

recom_genog <- predict(r, beers_rrmatrix['genog'], n=5)
as(recom_genog, "list")
#recommendation for geong: "2093" "6075" "571"  "782"  "932"  

recom_giblet <- predict(r, beers_rrmatrix['giblet'], n=5)
as(recom_giblet, "list")
#recommendation for giblet: "7971"  "1545"  "34420" "29619" "19960"
#######################################################################################################
#######################################################################################################
