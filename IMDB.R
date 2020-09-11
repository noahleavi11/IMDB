##
## This is all my code to analyze the IMDB database
##

## Libraries I Need
library(tidyverse)
library(caret)
library(DataExplorer)

## Read in the data
imdb <- read_csv("CleanedIMDBData.csv")

##
## Some Variable Transformations which may help
## models predict better
##

## Dummary (Indicator) Variables
IVTrans <- dummyVars(imdb_score~.-movie_title-Set, data=imdb)
imdb.iv <- predict(IVTrans, newdata=imdb)  %>% as.data.frame() %>%
  bind_cols(., imdb %>% select(movie_title, Set, imdb_score))

## Principal Components Transformation
pcTrans <- preProcess(x=imdb %>% select(-imdb_score), method="pca")
imdb.pca <- predict(pcTrans, newdata=imdb)
plot_correlation(imdb.pca, type="continuous", cor_args=list(use="pairwise.complete.obs"))

## Center and Scaling
trans.cs <- preProcess(x=imdb %>% select(-imdb_score), method=c("center", "scale"))
imdb.cs <- predict(trans.cs, newdata=imdb)
trans01 <- preProcess(x=imdb %>% select(-imdb_score), method="range",
                      rangeBounds=c(0,1))
imdb.01 <- predict(trans01, newdata=imdb)

####################################
## Fit some models for prediction ##
####################################

## Split the test and training data
imdb.train <- imdb %>% filter(!is.na(imdb_score))
imdb.test <- imdb %>% filter(is.na(imdb_score))

## Fit a linear regression model
linreg <- train(form=imdb_score~.,
                data=(imdb.train %>% select(-Set, -movie_title)),
                method="lm",
                trControl=trainControl(method="repeatedcv",
                                       number=10, #Number of pieces of your data
                                       repeats=3) #repeats=1 = "cv"
)
linreg$results
linreg.preds <- data.frame(Id=imdb.test$movie_title, Predicted=predict(linreg, newdata=imdb.test))
write_csv(x=linreg.preds, path="./LinearRegressionPredictionsHeaton.csv")

## Fit an Elastic Net model
elnet <- train(form=imdb_score~.,
               data=(imdb.train %>% select(-Set, -movie_title)),
               method="glmnet",
               trControl=trainControl(method="repeatedcv",
                                      number=10, #Number of pieces of your data
                                      repeats=3) #repeats=1 = "cv"
)
plot(elnet)

## Tune the elastic net
enet.grid <- expand.grid(alpha=seq(0.4, 0.8, length=10),
                         lambda=seq(0, 0.02, length=10))
elnet <- train(form=imdb_score~.,
               data=(imdb.train %>% select(-Set, -movie_title)),
               method="glmnet",
               trControl=trainControl(method="repeatedcv",
                                      number=10, #Number of pieces of your data
                                      repeats=3), #repeats=1 = "cv"
               tuneGrid=enet.grid
)
plot(elnet)
elnet.preds <- data.frame(Id=imdb.test$movie_title, Predicted=predict(elnet, newdata=imdb.test))
write_csv(x=elnet.preds, path="./ElasticNetPredictionsHeaton.csv")







<<<<<<< HEAD
## Scatterplot of Budget vs. Score
## Budget is in local currency, need to convert to common currency
ggplot(data=imdb, mapping=aes(x=budget, y=imdb_score)) +
  geom_
limdb %>% filter(budget>100000000, country=="USA") %>% 
  select(movie_title)

## Scatterplot of gross vs imdb
ggplot(data=imdb, mapping=aes(x=gross, y=imdb_score)) +
  geom_point()
with(imdb, cor(gross, imdb_score, use="complete.obs"))


a = unique(imdb['country'])


countries_currency_units = read_csv("countries_currency_units.csv", col_names = c("Country","Currency", "Currency code"))
country_codes = read_csv("country_codes.csv")
exchange_rates = read_csv("exchange rates.csv", col_names = c("Currency code", "Currency name", "Units per USD", "USD per Unit"))

newdf = countries_currency_units %>% inner_join(exchange_rates, by = "Currency code")

adf = newdf %>% inner_join(country_codes, "Country")

colnames(adf)[1] <- "country"

fdf = left_join(x=imdb,y=adf,by = c("country" = "Alpha-3 code"),all.x=TRUE)

#bdf = imdb %>% left_join(adf, by.x = "country", by.y = "Alpha-3 code")

#cdf = bdf %>% left_join(adf, by.x = "country", by.y = "Alpha-3 code")
=======

>>>>>>> 862e28532b87cafe4a0a65868f5a126ced748056
