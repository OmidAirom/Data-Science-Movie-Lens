##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# changing timeout to more than 60 seconds to prevent error
options(timeout = 3600)



dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

head(movies)
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

head(edx)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

head(edx)
head(validation)
# ================================================================
# ================================================================
# ================================================================
# ================================================================
# ================================================================



# dimensions of edx and validation data
dim(edx)
dim(validation)
# taking a look at the edx and validation data
head(edx)
head(validation)

summary(edx)

# unique users and movies in the edx dataset
edx %>% summarize(unique_users = n_distinct(userId),
                  unique_movies =  n_distinct(movieId))

# histogram & mean of the rating column in edx dataset
hist(edx$rating)

# histogram of number of ratings of movies
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 40) +
  scale_x_log10() +
  xlab("Ratings") +
  ylab("Movies")

# average rating of movies
movies_by_title <- edx %>% group_by(title)
avg_of_movies <- movies_by_title %>% summarize(count = n()
                                               , avg_movie_rating = mean(rating))

# histogram of average rating of movies
avg_of_movies %>% 
  ggplot(aes(avg_movie_rating)) + 
  geom_histogram(bins = 40) +
  xlim(0, 5)

# users ratings & its histogram
users <- edx %>% count(userId)
users %>% ggplot(aes(n)) +
  geom_histogram(bins = 40) +
  scale_x_log10() +
  xlab("Ratings") + 
  ylab("Users")

# average of user ratings & its histogram
edx %>%
  group_by(userId) %>%
  summarize(avg_ur = mean(rating)) %>%
  ggplot(aes(avg_ur)) +
  geom_histogram(bins = 40) +
  xlab("Ratings") +
  ylab("Users") 

# top 10 rated movies
edx %>% group_by(title) %>%
  summarise(count = n()) %>%
  slice_max(count, n=10)

# Modelling

# average of ratings of the movies
ratings_avg <- mean(edx$rating) 
prediction <- RMSE(validation$rating, ratings_avg)
prediction
results <- tibble(model = "ratings average of the movies", RMSE = prediction)
results %>% knitr::kable()

# movie effect
movie_effect <- edx %>%
  group_by(movieId) %>%
  summarize(avg = mean(rating - ratings_avg))

movie_effect %>% ggplot(aes(avg)) +
  geom_histogram(bins = 40) +
  ylab("Movies Count") 

predicted_ratings <- ratings_avg +  validation %>%
  left_join(movie_effect, by='movieId') %>%
  pull(avg)

head(predicted_ratings)

new_prediction <- RMSE(predicted_ratings, validation$rating)
results <- bind_rows(results, data_frame(model = "movie effect" 
                                         ,RMSE = new_prediction ))

results
results %>% knitr::kable()


# final model
# movie & user model
users <- edx %>% left_join(movie_effect, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100)

users <- users %>% mutate(users_avg = mean(rating - ratings_avg - avg))

users %>% ggplot(aes(users_avg)) +
  geom_histogram(bins = 40) +
  ylab("Movies Count") 

users <- edx %>%
  left_join(movie_effect, by='movieId') %>%
  group_by(userId) %>%
  summarize(users_avg = mean(rating - ratings_avg - avg))

head(movie_effect)
head(users)

a <- users %>% select(users_avg, userId)
head(a)
nrow(a)
nrow(movie_effect)

new_preds <- validation %>%
  left_join(movie_effect, by='movieId')
new_preds <- new_preds %>%
  left_join(a, by='userId') 
new_preds <- new_preds %>%
  mutate(pred = ratings_avg + avg + users_avg) %>%
  pull(pred)

head(new_preds)

head(validation)
nrow(validation)

mu_model <- RMSE(new_preds, validation$rating)
mu_model

results

results <- bind_rows(results
                    ,data_frame(model="movie and user effect"
                    ,RMSE = mu_model))
results %>% knitr::kable()
