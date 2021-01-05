# MovieLens Project
# Author: Marisa Ivonne Zamora Carrillo
# January 2021



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

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
head(ratings)

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

head(movies)

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

head(movies)

movielens <- left_join(ratings, movies, by = "movieId")
nrow(movielens)
head(movielens)

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

head(validation)


# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

head(edx)
rm(dl, ratings, movies, test_index, temp, movielens, removed)



### Data Analysis----------------------------------------------------------



edx %>% summarize(Rows = nrow(edx),
                  Users = n_distinct(userId),
                  Movies = n_distinct(movieId))%>%formattable()

colnames(edx)

head(edx)%>%formattable()

### WRANGLE AND PREPARE THE DATA ------------------------------------------------

# SEPARATE THE GENRE 

edx<- edx %>%
  separate_rows(genres,
                sep = "\\|")

# GET the release date

edx <-
  edx %>% mutate(release = substr(title, nchar(title) - 6, nchar(title))) %>% 
  mutate(release = as.integer(substr(release, 3, 6)))

str(edx)
head(edx)

# get the date from the time stamp
edx <-
  edx %>% mutate(date = as.POSIXct(timestamp, origin = "1970-01-01"))

edx <- edx  %>% 
  mutate(month = format(edx$date, "%m")) %>%
  mutate(year = format(edx$date, "%Y"))


# select the columns needed
edx <-
  edx %>% select(userId,movieId,rating,title,genres,release,month,year)
head(edx)%>%formattable()

#do the same with the validation dataset 

# SEPARATE THE GENRE 

validation<- validation %>%
  separate_rows(genres,
                sep = "\\|")

# GET the release date

validation <-
  validation %>% mutate(release = substr(title, nchar(title) - 6, nchar(title))) %>% 
  mutate(release = as.integer(substr(release, 3, 6)))


# get the date from the time stamp
validation <-
  validation%>% mutate(date = as.POSIXct(timestamp, origin = "1970-01-01"))

validation <- validation %>% 
  mutate(month = format(validation$date, "%m")) %>%
  mutate(year = format(validation$date, "%Y"))

head(validation)

# select the columns needed
validation <-
  validation %>% select(userId,movieId,rating,title,genres,release,month,year)
head(validation)

# change the data type in both datasets
edx$release <- as.numeric(edx$release)
edx$year <- as.numeric(edx$year)
edx$month <- as.numeric(edx$month)

validation$year <- as.numeric(validation$year)
validation$month <- as.numeric(validation$month)
validation$release <- as.numeric(validation$release)
save.image(file='myEnvironment.RData')

str(edx)

### Explotaroty analysis ------------------------------------------------

# Users

users_per_year <- edx %>% group_by(year)%>%
  summarise(user=n_distinct(userId))

ggplot(data=users_per_year,aes(x=year,y=user,fill=year))+
  geom_bar(stat = "identity") +
  labs(title = "Users per Year", x = "Year", 
       y = "Users")

edx%>%group_by(year,userId)%>%
  summarise(ratings=n())%>%arrange(desc(ratings))%>%formattable()


# Ratings

summary(edx$rating)


ggplot(data = edx, aes(x = rating,fill=rating)) +
  geom_bar() +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Frequency")



ratings_per_year <- edx %>% group_by(year)%>%
  summarise(ratings=n())

ggplot(data=ratings_per_year,aes(x=year,y=ratings,fill=year))+
  geom_bar(stat = "identity") +
  labs(title = "Ratings per Year", x = "Year", 
       y = "Ratings")


ratings_per_movie <- edx %>% group_by(title)%>%
  summarise(ratings=n()) %>% arrange(desc(ratings))%>%head(n=20) 

ggplot(data=ratings_per_movie,aes(x=title,y=ratings,fill=title))+
  geom_bar(stat = "identity") +
  labs(title = "Ratings per Movie", x = "Title", 
       y = "Ratings")

ratings_once <- edx %>% group_by(title)%>%
  summarise(ratings=n()) %>% filter(ratings == 1) %>% select(title)

ratings_once %>% formattable()


ratings_per_genre <- edx %>% group_by(genres)%>%
  summarise(ratings=n()) 

ggplot(data=ratings_per_genre,aes(x=genres,y=ratings,fill=genres))+
  geom_bar(stat = "identity") +
  labs(title = "Ratings per Genre", x = "Genre", 
       y = "Frequency")

ratings_per_genre %>% formattable()


mean_per_genre <- edx %>% group_by(genres)%>%
  summarise(mean=mean(rating)) 

ggplot(data=mean_per_genre,aes(x=genres,y=mean,fill=genres))+
  geom_bar(stat = "identity") +
  labs(title = "Mean per Genre", x = "Genre", 
       y = "Mean")

mean_per_genre %>% formattable()

save.image(file='myEnvironment.RData')
load('myEnvironment.RData')

### Predictive Model building and evaluation ------------------------------------

# Create a dataframe to keep the results
RMSE_results <- data.frame(model=character(0),variables=character(0),RMSE=numeric())

# Calculate the first model only by mean
mu_hat <- mean(edx$rating)
RMSE_mean_result <- RMSE(validation$rating, mu_hat)
RMSE_results <- RMSE_results %>% add_row(model="Mean",variables = "All", RMSE=RMSE_mean_result)


# Calculate the average grouping by movie

movie <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

qplot(b_i, geom = "histogram",main="Movie Means",fill=I("blue"), color = I("blue"), data =
        movie)

# Compute the predicted rating in the validation dataset
rmse_movie_model <- validation %>%
  left_join(movie, by='movieId') %>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)
rmse_movie_model_result <- RMSE(validation$rating, rmse_movie_model)
RMSE_results <- RMSE_results %>% add_row(model="Mean",variables = "Movie", RMSE=rmse_movie_model_result)


# Calculate the average grouping by user and movie

user <- edx %>%
  left_join(movie, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

qplot(b_u, geom = "histogram",main="Movie-User Means",fill=I("blue"), color = I("black"), bins=25, data =
        user)
# Compute the predicted ratings on validation dataset
rmse_movie_user_model <- validation %>%
  left_join(movie, by='movieId') %>%
  left_join(user, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

rmse_movie_user_model_result <- RMSE(validation$rating, rmse_movie_user_model)
RMSE_results <- RMSE_results %>% add_row(model="Mean",variables = "Movie-User", RMSE=rmse_movie_user_model_result)

RMSE_results %>% formattable()


# Calculate the average grouping by user, movie and genre

movie_user_genre <- edx %>%
  left_join(movie, by='movieId') %>%
  left_join(user, by='userId') %>%
  group_by(genres) %>%
  summarize(b_u_x = mean(rating - mu_hat - b_i - b_u))

qplot(b_u_x, geom = "histogram",main="Movie-User-Genre Means",fill=I("blue"), color = I("black"), bins=25, data =
        movie_user_genre)
# Compute the predicted ratings on validation dataset
rmse_movie_user_genre_model <- validation %>%
  left_join(movie, by='movieId') %>%
  left_join(user, by='userId') %>%
  left_join(movie_user_genre, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_x) %>%
  pull(pred)
rmse_movie_user_genre_model_result <- RMSE(validation$rating, rmse_movie_user_genre_model)

RMSE_results <- RMSE_results %>% add_row(model="Mean",variables = "Movie-User-Genre", RMSE=rmse_movie_user_genre_model_result)


# REGULARIZATION

lambdas <- seq(0, 9, 0.15)

# REgularized by movie

rmses <- sapply(lambdas, function(lambda) {
  # Calculate average by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  # Compute the predicted ratings on validation dataset
  predicted_ratings <- validation %>%
    left_join(b_i, by = 'movieId') %>%
    mutate(pred = mu_hat + b_i) %>%
    pull(pred)
  # Predict the RMSE on the validation set
  return(RMSE(validation$rating, predicted_ratings))
})

# plot the result of lambdas
lambdas_RMSE <- data.frame(RMSE = rmses, lambdas = lambdas)
ggplot(lambdas_RMSE, aes(lambdas, rmses)) +
  theme_classic()  +
  geom_point() +
  labs(title = "Regularized Movie Based Model",
       y = "RMSEs",
       x = "lambdas")

# Get the lambda value that minimize the RMSE
best_lambda <- lambdas[which.min(rmses)]


# Predict the RMSE on the validation set
regularized_movie_model <- min(rmses)

RMSE_results <- RMSE_results %>% add_row(model="Regularized",variables = "Movie", RMSE=regularized_movie_model)

# Regularized by movie and user

rmses <- sapply(lambdas, function(lambda) {
  # Calculate the average by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  # Calculate the average by user
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
  # Compute the predicted ratings on validation dataset
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu_hat + b_i + b_u) %>%
    pull(pred)
  # Predict the RMSE on the validation set
  return(RMSE(validation$rating, predicted_ratings))
})

# plot the result of lambdas
lambdas_RMSE_MU <- data.frame(RMSE = rmses, lambdas = lambdas)
ggplot(lambdas_RMSE_MU, aes(lambdas, rmses)) +
  theme_classic()  +
  geom_point() +
  labs(title = "Regularized Movie-User Based Model",
       y = "RMSEs",
       x = "lambdas")

# Get the lambda value that minimize the RMSE
best_lambda <- lambdas[which.min(rmses)]
# Predict the RMSE on the validation set
regularized_movie_user_model <- min(rmses)

RMSE_results <- RMSE_results %>% add_row(model="Regularized",variables = "Movie-User", RMSE=regularized_movie_user_model)



# Regulired by movie-user-genre
rmses <- sapply(lambdas, function(lambda) {
  # Calculate the average by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  # Calculate the average by user
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
  # Calculate the average by genre
  b_u_x <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_u_x = sum(rating - b_i - mu_hat - b_u) / (n() + lambda))
  # Compute the predicted ratings on validation dataset
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_u_x, by='genres') %>%
    mutate(pred = mu_hat + b_i + b_u + b_u_x) %>%
    pull(pred)
  # Predict the RMSE on the validation set
  return(RMSE(validation$rating, predicted_ratings))
})

# plot the result of lambdas
lambdas_RMSE_MUG <- data.frame(RMSE = rmses, lambdas = lambdas)
ggplot(lambdas_RMSE_MUG, aes(lambdas, rmses)) +
  theme_classic()  +
  geom_point() +
  labs(title = "Regularized Movie-User-Genre Based Model",
       y = "RMSEs",
       x = "lambdas")


# Get the lambda value that minimize the RMSE
best_lambda <- lambdas[which.min(rmses)]
# Predict the RMSE on the validation set
regularized_movie_user_genre_model <- min(rmses)

RMSE_results <- RMSE_results %>% add_row(model="Regularized",variables = "Movie-User-Genre", RMSE=regularized_movie_user_genre_model)

## Results -----------------------------------------------------------------
RMSE_results %>% formattable()
