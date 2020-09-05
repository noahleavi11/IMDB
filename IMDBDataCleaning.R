##
## This is all my code to analyze the IMDB database
##

## Libraries I Need
library(tidyverse)

## Read in the data
imdb.train <- read_csv("./IMDBTrain.csv")
imdb.test <- read_csv("./IMDBTest.csv")

## Merge the two datasets together so when I clean the
## training dataset I also treat the test set the same way
names(imdb.test)[names(imdb.test)=="Id"] <- "movie_title"
imdb <- bind_rows(train=imdb.train, test=imdb.test, .id="Set")

####################################
## Some Exploratory Data Analysis ##
####################################

## Overall summary of the data
summary(imdb)

## Scatterplot of Budget vs. Score
## Budget is in local currency, need to convert to common currency
ggplot(data=imdb, mapping=aes(x=budget, y=imdb_score)) +
  geom_point()
imdb %>% filter(budget>100000000, country=="USA") %>% 
  select(movie_title)

## Scatterplot of gross vs imdb
ggplot(data=imdb, mapping=aes(x=gross, y=imdb_score)) +
  geom_point()
with(imdb, cor(gross, imdb_score, use="complete.obs"))


##stuff i am adding

##checking distribution of num of critic reviews to fill in missing values; decided on median
boxplot(x = imdb[,'num_critic_for_reviews'])

#see the rows the correspond to na values in num of reviews column
imdb[which(is.na(imdb$num_critic_for_reviews)),]


##way to clean up num critic for reviews
imdb[is.na(imdb[,"num_critic_for_reviews"]), "num_critic_for_reviews"] <- 
  median(x = imdb[["num_critic_for_reviews"]], na.rm = TRUE)

#way to clean up the same thing
imdb$num_critic_for_reviews <- imdb$num_critic_for_reviews %>% 
  replace_na(median(x = imdb[['num_critic_for_reviews']], na.rm = TRUE))


