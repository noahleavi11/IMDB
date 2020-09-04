##
## This is all my code to analyze the IMDB database
##

## Libraries I Need
library(tidyverse)

## Read in the data
imdb <- read_csv("./IMDBTrain.csv")
imdb.test <- read_csv("./IMDBTest.csv")

##
## Exploratory Data Analysis
##

## Overall summary of the data
summary(imdb)

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
