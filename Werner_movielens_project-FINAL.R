# ---
#   title: 'HarvardX - Data Science Professional Certificate - Capstone - Project MovieLens'
# author: "Werner Alencar Advincula Dassuncao"
# date: "`r Sys.Date()`"
# output:
#   pdf_document: 
#   toc: yes
# ---


## ----config_chunk, echo = FALSE, warning=FALSE, message=FALSE-------------------------------------------------------------------------------
# Adjust the size of the output to 80 % when creating the pdf.
knitr::opts_chunk$set(out.width = '80%') 


## ----check and install packages, message=FALSE----------------------------------------------------------------------------------------------
if(!require(tidyverse)) install.packages('tidyverse', repos = 'http://cran.us.r-project.org')
if(!require(recosystem)) install.packages('recosystem', repos='http://cran.us.r-project.org')
if(!require(caret)) install.packages('caret', repos = 'http://cran.us.r-project.org')
if(!require(data.table)) install.packages('data.table', repos = 'http://cran.us.r-project.org')
if(!require(dplyr)) install.packages('dplyr', repos = 'http://cran.us.r-project.org')
if(!require(knitr)) install.packages('knitr', repos = 'http://cran.us.r-project.org')
if(!require(ggplot2)) install.packages('ggplot2', repos = 'http://cran.us.r-project.org')
if(!require(anytime)) install.packages('anytime', repos = 'http://cran.us.r-project.org')
if(!require(recommenderlab)) install.packages('recommenderlab', repos = 'http://cran.us.r-project.org')
if(!require(lsa)) install.packages('lsa', repos = 'http://cran.us.r-project.org')
if(!require(irlba)) install.packages('irlba', repos = 'http://cran.us.r-project.org')
if(!require(tinytex)) install.packages('tinytex', repos = 'http://cran.us.r-project.org')
if(!require(kableExtra)) install.packages('kableExtra', repos = 'http://cran.us.r-project.org')
library(tinytex)
library(tidyverse)
library(recosystem)
library(caret)
library(data.table)
library(knitr)
library(dplyr)
library(ggplot2)
library(anytime)
library(recommenderlab)
library(lsa)
library(irlba)
library(kableExtra)


## ----download the source data, message=FALSE------------------------------------------------------------------------------------------------
# Download the source data
dl <- tempfile()
download.file('http://files.grouplens.org/datasets/movielens/ml-10m.zip', dl)


## ----movies table---------------------------------------------------------------------------------------------------------------------------
# MovieID::Title::Genres
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# Transform the movies character vector into a data.frame object: # R 4.0 or later:
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))

# Show the first 5 observations:
head(movies, 5) %>% kable()


## ----ratings table--------------------------------------------------------------------------------------------------------------------------
# Read and parse ratings.dat, add column names
# UserID::MovieID::Rating::Timestamp
ratings <- fread(text = gsub('::', '\t', readLines(unzip(dl, 'ml-10M100K/ratings.dat'))),
                 col.names = c('userId', 'movieId', 'rating', 'timestamp'))

# Show the first 5 observations:
head(ratings,5) %>% kable()


## ----join ratings and movies objects--------------------------------------------------------------------------------------------------------
# join ratings and movie objects
movielens <- left_join(ratings, movies, by = 'movieId')

# Show the first 5 observations:
head(movielens,5) %>% kable()


## ----create validation set at 10% of movielens, warning=FALSE-------------------------------------------------------------------------------
# Setting the size of the validation set at 10 % of the edx data:
set.seed(1981, sample.kind = 'Rounding') # if using R 3.5 or earlier, use `set.seed(1981)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp_set <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp_set %>% semi_join(edx, by = 'movieId') %>% semi_join(edx, by = 'userId')

# Add rows removed from validation set back into edx set
removed <- anti_join(temp_set, validation)
edx <- rbind(edx, removed)

# Remove objects no longer required from memory
rm(dl, movielens, ratings, movies, test_index, temp_set, removed)


## ----create a release and rating year columns, message=FALSE, warning=FALSE-----------------------------------------------------------------
# create a movie release year and rating year columns
edx <- edx %>% 
  mutate( date_time = anytime(timestamp), # convert column to POSIXct format
          release_year = as.numeric(
                              str_sub(title,-5,-2)), # grab the 4 digits inside the parenthesis
          rating_year = as.numeric(format(date_time, 
                                          format = '%Y'))) %>% # grab the year from date_time
  select(userId, movieId, rating, title, genres, release_year, rating_year)


## ----summary edx, echo = FALSE--------------------------------------------------------------------------------------------------------------
# Show the summary
summary(edx) 


## ----custom_summary, echo = FALSE-----------------------------------------------------------------------------------------------------------
# Display a custom summary
edx %>% summarize(n_movies = n_distinct(movieId), 
                        n_users = n_distinct(userId), 
                        rating_min = min(rating), 
                        rating_max = max(rating),
                        release_min = min(release_year),
                        release_max = max(release_year),
                        rate_year_min = min(rating_year),
                        rate_year_max = max(rating_year)) %>%
  kable(caption = 'Custom summary of the EDX dataset')


## ----distribution of the ratings plot, echo = FALSE-----------------------------------------------------------------------------------------
# Store all distinct ratings to display on the x label
x_label <- unique(edx$rating)

# Show a plot of the distribution of the ratings
plot <- edx %>% 
  ggplot(aes(x = rating)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 0.25, color = 'black') +
  scale_x_continuous(breaks = x_label) +
  scale_fill_gradient('Count', low = 'white', high = 'red') +
  geom_vline(aes(xintercept = mean(rating)), colour = 'blue', linetype = 'dotdash', size = 1) +
  labs(title = 'Histogram of all ratings in EDX and mean line', x = 'Rating value', y = 'Count')


## ----display fancy plot, echo = FALSE, fig.cap='Histogram of all ratings in EDX and average line'-------------------------------------------
# Disploy fancy plot
plot
# free memory
rm(plot)


## ----summary most frequent ratings, echo = FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------
edx %>% group_by(rating) %>%
  summarize(Rating = rating, Count = n()) %>% distinct(Rating, Count) %>% arrange(desc(Count))  %>%
  knitr::kable(caption = 'Rating counts from most to least frequent')


## ----users, echo = FALSE--------------------------------------------------------------------------------------------------------------------
# max, min and average number of ratings per user
edx %>% group_by(userId) %>% mutate(number_of_ratings = n()) %>% ungroup() %>%
  summarize(max = max(number_of_ratings),
            min = min(number_of_ratings),
            avg = mean(number_of_ratings)) %>%
  kable(caption = 'Maximum, minium and average number of ratings per user', digits = 2)


## ----movie, echo = FALSE--------------------------------------------------------------------------------------------------------------------
# maximum, minimum and average number of ratings per movie
m <- edx %>% group_by(movieId) %>% mutate(number_of_ratings = n()) %>% ungroup() %>%
  summarize(max = max(number_of_ratings),
            min = min(number_of_ratings),
            avg = mean(number_of_ratings)) 
  
m %>% kable(caption = 'Maximum, minium and average number of ratings per movie', digits = 2)


## ----top_10_movies per average rating, echo=FALSE, message=FALSE----------------------------------------------------------------------------
# top_10_movies per average rating
top_10_movies <- edx %>% group_by(title, movieId) %>% 
  summarize(average_rating = mean(rating)) %>% arrange(desc(average_rating)) %>% 
  ungroup() %>% top_n(10) %>% select(title, average_rating)
top_10_movies %>% kable(caption = 'Top 10 movies per average rating', digits = 2)


## ----top_10_movies per count of ratings including average rating, echo = FALSE, message=FALSE-----------------------------------------------
# top_10_movies per average rating
top_10_movies_count <- edx %>% group_by(title, movieId) %>% 
  summarize(average_rating = mean(rating), count = n()) %>% arrange(desc(count)) %>% 
  ungroup() %>% top_n(10) %>% select(title, count, average_rating)
top_10_movies_count %>% 
  kable(caption = 'Top 10 movies per count of ratings, including the average rating',
        digits = 2)


## ----worst_10_movies per average ratings including quantity of ratings, echo = FALSE, message=FALSE-----------------------------------------
# worst_10_movies average rating, including quantity of ratings
worst_10_movies_count <- edx %>% group_by(title, movieId) %>% 
  summarize(average_rating = mean(rating), count = n()) %>% arrange(average_rating) %>% 
  ungroup() %>% head(10) %>% select(title, count, average_rating)
worst_10_movies_count %>% 
  kable(caption = 'Worst 10 movies per average rating, incl. number of ratings', 
        digits = 2)


## ----plot rating average versus quantity of ratings, echo = FALSE, message=FALSE, fig.cap='Scatterplot of average X quantity of ratings per movie'----
# Plot number of ratings versus ratings average per movie title 
p <- edx %>% group_by(title, movieId) %>% 
  mutate(Count = n(), average_rating = mean(rating)) %>% 
  distinct(Count, title, average_rating) %>%
  arrange(desc(Count),title, average_rating)  

p %>% 
  ggplot(aes(x = average_rating, y = Count)) +
  geom_point(color = 'red', alpha = 0.2) +
  coord_flip() +
  theme(panel.background = element_rect(fill = 'lightblue', colour = 'blue')) +
  labs(title = 'Scatterplot of average X quantity of ratings per movie', 
       x = 'Average rating', y = 'Number of ratings')



## ----boxplot average rating versus number of ratings, echo = FALSE, fig.cap='boxplot average rating versus number of ratings'---------------
p %>% 
  filter(Count <= 30) %>%
  ggplot(aes(x = factor(Count), y = average_rating)) +
  geom_boxplot(color = 'blue', alpha = 0.2, fill = 'green') +
  theme(panel.background = element_rect(fill = 'lightblue', colour = 'blue')) +
  labs(title = 'Boxplot of average rating X quantity of ratings per movie (30 ratings or less)',
       y = 'Average rating', x = 'Number of ratings')


## ----fig.cap='User rating pattern over the years', echo = FALSE-----------------------------------------------------------------------------
edx %>% group_by(release_year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(release_year, rating), ) +
  geom_point() +
  geom_smooth() +
  theme(panel.background = element_rect(fill = 'lightblue', colour = 'blue')) +
  labs(title = 'User rating pattern over the years', x = 'Movie release year',
       y = 'Average rating per year')


## ----Building the train and test sets, warning=FALSE----------------------------------------------------------------------------------------
# To increase performance, I will drop all unused columns 
edx <- edx %>% select(movieId, userId, rating)

# In order to replicate the results here, you need to set the seed to 1981
set.seed(1981, sample.kind = 'Rounding')

# Reserving 20% of the edx data for testing, train data is 80%:
test_idx <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_idx,]
test_set <- edx[test_idx,]

test_set <- test_set %>% 
  semi_join(train_set, by = 'movieId') %>% 
  semi_join(train_set, by = 'userId')


## ----dimensions_train_and_test_sets---------------------------------------------------------------------------------------------------------
dim(train_set)
dim(test_set)


## ----definition RMSE function---------------------------------------------------------------------------------------------------------------
RMSE <- function(true_ratings, predictions) {
  sqrt(mean((true_ratings - predictions)^2,na.rm = TRUE))
}


## ----overall_average------------------------------------------------------------------------------------------------------------------------
mu <- mean(train_set$rating)
average_rmse <- RMSE(test_set$rating, mu)

# Create a table to store our RMSE results
models_rmse <- tibble(Method = 'Overall rating average',  RMSE = average_rmse)
models_rmse %>% knitr::kable(caption = 'Overall Rating Average')


## ----movie_effect model---------------------------------------------------------------------------------------------------------------------
movie_bias <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

pred_movie_bias <- test_set %>% 
  left_join(movie_bias, by = 'movieId') %>%
  mutate(prediction = mu + b_i) %>%
  pull(prediction)

movie_bias_rmse <- RMSE(test_set$rating, pred_movie_bias)
models_rmse <- rbind(models_rmse, tibble(Method = 'Movie bias', RMSE = movie_bias_rmse))
models_rmse[nrow(models_rmse):nrow(models_rmse),] %>% kable(caption = 'Movie effect')


## ----generate movie_bias plot, echo = FALSE, fig.cap='Movie effect histogram'---------------------------------------------------------------
movie_bias %>%  qplot(b_i, geom = 'histogram', bins = 30, data = ., 
                      fill = I('lightblue'), color = I('blue'), 
                      main = 'Histogram of the movie effect') 


## ----movie_and_user_effect, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
# Calculate movie_user_bias
movie_user_bias <- train_set %>% 
  left_join(movie_bias, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

pred_movie_user <- test_set %>%
  left_join(movie_bias, by = 'movieId') %>%
  left_join(movie_user_bias, by = 'userId') %>%
  mutate(prediction = mu + b_i + b_u) %>%
  pull(prediction)

movie_user_rmse <- RMSE(test_set$rating, pred_movie_user)
models_rmse <- rbind(models_rmse, 
                     tibble(Method = 'Movie and User biases', 
                            RMSE = movie_user_rmse) )
models_rmse[nrow(models_rmse):nrow(models_rmse),] %>% kable(caption = 'Movie-User effect')


## ----plot movie_user bias, echo=FALSE, message=FALSE, warning=FALSE, fig.cap='Histogram of Movie-User effect'-------------------------------
# Plot the movie_user_bias distribution
movie_user_bias %>% qplot(b_u, geom = 'histogram', 
                          bins = 30, data = ., 
                          color = I('green'),
                          main = 'Histogram of the movie-user effect')


## ----test lambda values---------------------------------------------------------------------------------------------------------------------
# Create a sequence of values to test
lambdas <- seq(0, 7, 0.25)
# Calculate the rmses for the lambda values.
rmses <- sapply(lambdas, function(lambda){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu) / (n() + lambda))
  
  b_u <- train_set %>% left_join(b_i, by = 'movieId') %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - b_i - mu) / (n() + lambda))
  
  predictions <- test_set %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    mutate(prediction = mu + b_i + b_u) %>%
    pull(prediction)
  
  RMSE(test_set$rating, predictions)})


## ----lambda_plot, echo = FALSE,  fig.cap='Lambda Versus RMSE plot'--------------------------------------------------------------------------
# Create a plot
tibble(lambdas) %>% ggplot(aes(lambdas, rmses)) + 
  geom_line(col = 'red') + 
  geom_point(col = 'red') + 
  geom_vline(xintercept = lambdas[which.min(rmses)], 
             col = 'blue', 
             linetype = 'longdash') +
  ggtitle('RMSE variability per lambda value')


## ----ideal lambda, echo = FALSE-------------------------------------------------------------------------------------------------------------
lambda <- lambdas[which.min(rmses)]


## ----regularized movie_bias-----------------------------------------------------------------------------------------------------------------
# Train the model
reg_movie_bias <- train_set %>% 
  group_by(movieId) %>% 
  summarize(r_b_i = sum(rating - mu) / (n() + lambda), 
            r_n_i = n())

reg_movie_user_bias <- train_set %>% 
  left_join(reg_movie_bias, by = 'movieId') %>% 
  group_by(userId) %>% 
  summarize(r_b_u = sum(rating - mu - r_b_i) / (n() + lambda), 
                                 r_n_u = n())
# Test the model on the test_set
pred_reg_movie_user <- test_set %>% 
  left_join(reg_movie_bias, by = 'movieId') %>%
  left_join(reg_movie_user_bias, by = 'userId') %>%
  mutate(prediction = mu + r_b_i + r_b_u) %>%
  pull(prediction)

reg_movie_user_rmse = RMSE(test_set$rating, pred_reg_movie_user)

# Add the latest result to the rmse table
models_rmse <- rbind(models_rmse, tibble(Method = 'Regularized Movie User biases', 
                                         RMSE = reg_movie_user_rmse))
# Show the rmses table
models_rmse[nrow(models_rmse):nrow(models_rmse),] %>% kable(caption = 'Regularized Movie User biases')


## ----List the models available for the realRatingMatrix, echo = FALSE-----------------------------------------------------------------------
rec_models <- recommenderRegistry$get_entries(dataType = 'realRatingMatrix')
names(rec_models)


## ----sparse_matrix--------------------------------------------------------------------------------------------------------------------------
# creating a copy of edx data and change data types.
edx_copy <- edx

# Coercing the values to numeric
edx_copy$userId <- as.numeric(as.factor(edx_copy$userId))
edx_copy$movieId <- as.numeric(as.factor(edx_copy$movieId))
edx_copy$rating <- as.numeric(edx_copy$rating)

# Create a sparseMatrix object
ratings_matrix <- 
  sparseMatrix(i = edx_copy$userId,
               j = edx_copy$movieId,
               x = edx_copy$rating,
               
               dims = c(length(unique(edx_copy$userId)),
                        length(unique(edx_copy$movieId))),
               
               dimnames = list(paste('user_', unique(edx_copy$userId), sep = ''),
                               paste('movie_', unique(edx_copy$movieId), sep = '')))

# Show the ratings_matrix structure
str(ratings_matrix)


## ----Create a realRatingMatrix object, message=FALSE, warning=FALSE-------------------------------------------------------------------------
# Create a realRatingMatrix object
recom_matrix <- new('realRatingMatrix', data = ratings_matrix)


## ----heatmap recom_matrix, echo = FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Create a heatmap for the realRatingMatrix object
max <- 50
image(recom_matrix[1:max, 1:max])


## ----minimum number of movies---------------------------------------------------------------------------------------------------------------
movies_min <- quantile(rowCounts(recom_matrix), 0.9);movies_min

## ----minimum number of users----------------------------------------------------------------------------------------------------------------
users_min <- quantile(colCounts(recom_matrix), 0.9);users_min


## ----recom_matrix optimization--------------------------------------------------------------------------------------------------------------
recom_matrix <- recom_matrix[rowCounts(recom_matrix) > movies_min,
                           colCounts(recom_matrix) > users_min]


## ----structure of the recom_matrix object, echo = FALSE-------------------------------------------------------------------------------------
# Show structure of recom_matrix
str(recom_matrix)


## ----split the edx data in train and test sets, message=FALSE, warning=FALSE----------------------------------------------------------------
# split the edx data in train and test sets
set.seed(1981, sample.kind = 'Rounding')

# Create train and test sets, with 80% and 20% of the edx data set, respectively.
evaluation <- evaluationScheme(recom_matrix, 
                               method='split', 
                               train = 0.8, 
                               given=-5,
                               goodRating = 3,
                               k = 1)  
evaluation


## ----training_set info----------------------------------------------------------------------------------------------------------------------
# Show training_set info
getData(evaluation, 'train')


## ----set used to build recommendations------------------------------------------------------------------------------------------------------
getData(evaluation,'known')


## ----test the recommendations---------------------------------------------------------------------------------------------------------------
# Show test set info
getData(evaluation, 'unknown')


## -------------------------------------------------------------------------------------------------------------------------------------------
# Show the tune parameters for the IBCF model
rec_models$IBCF_realRatingMatrix$parameters


## ----Recommenderlab IBCF method, message=FALSE, warning=FALSE-------------------------------------------------------------------------------
# set seed
set.seed(1981, sample.kind = 'Rounding')

# Generate the model
IBCF_method <- Recommender(getData(evaluation, 'train'), method = 'IBCF', 
                           param=list(normalize = 'center', 
                                      method='Cosine', k = 30)) # k = 350
# Make predictions
pred_IBCF_method <- predict(IBCF_method, 
                            getData(evaluation, 'known'), 
                            type = 'ratings')
# Test the method
IBCF_method_rmse <- calcPredictionAccuracy(pred_IBCF_method, getData(evaluation, 'unknown'))

# Update the rmse table
models_rmse <- rbind(models_rmse, tibble(Method = 'RecommenderLab IBCF', 
                                         RMSE = IBCF_method_rmse[1]))
# Show the table
models_rmse[nrow(models_rmse):nrow(models_rmse),] %>% kable(caption = 'Recommenderlab IBCF')


## ----Recommenderlab UBCF method, message=FALSE, warning=FALSE-------------------------------------------------------------------------------
# set seed
set.seed(1981, sample.kind = 'Rounding')

# Create UBCF model
UBCF_method <- Recommender(getData(evaluation, 'train'), method = 'UBCF',
                           param=list(normalize = 'center', method = 'Cosine', nn = 50))

pred_UBCF_method <- predict(UBCF_method, getData(evaluation, 'known'), type = 'ratings')

# Test the models accuracy
UBCF_method_rmse <- calcPredictionAccuracy(pred_UBCF_method, getData(evaluation, 'unknown'))

# Update the table 
models_rmse <- rbind(models_rmse, tibble(Method = 'RecommenderLab UBCF', 
                                         RMSE = UBCF_method_rmse[1]))
# Show the table
models_rmse[nrow(models_rmse):nrow(models_rmse),] %>% kable(caption = 'Recommenderlab UBCF')


## ----Recommenderlab POPULAR, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------
set.seed(1981, sample.kind = 'Rounding')
popular_method <- Recommender(recom_matrix, method = 'POPULAR',
                              param = list(normalize = 'center'))

# Evaluating the rmse for the popular_method
set.seed(1981, sample.kind = 'Rounding')
popular_method <- Recommender(getData(evaluation, 'train'), 
                              method = 'POPULAR')
pred_popular_method <- predict(popular_method, 
                               getData(evaluation, 'known'), 
                               type = 'ratings')

popular_method_rmse <- calcPredictionAccuracy(pred_popular_method, getData(evaluation,'unknown'))

models_rmse <- rbind(models_rmse, tibble(Method = 'RecommenderLab POPULAR', 
                                         RMSE = popular_method_rmse[1]))
models_rmse[nrow(models_rmse):nrow(models_rmse),] %>% kable(caption = 'Recommenderlab Popular Items')


## ----prediction example on the first 10 users-----------------------------------------------------------------------------------------------
#prediction example on the first 10 users
pred_popular_method <- predict(popular_method, recom_matrix[1:10], type = 'ratings')
as(pred_popular_method, 'matrix')[1:10,1:10]


## ----Recosystem Parallel Matrix Factorization, message=FALSE, warning=FALSE-----------------------------------------------------------------

set.seed(1981, sample.kind='Rounding')

train_set_reco_mf <- train_set %>% select(userId, movieId, rating) 
#train_set_reco_mf <- as.matrix(train_set_reco_mf)

test_set_reco_mf <- test_set %>% select(userId, movieId, rating) 
#test_set_reco_mf <- as.matrix(test_set_reco_mf)


# Create sample and test sets for the edx:
train_mf <- with(train_set_reco_mf, data_memory(user = userId, item = movieId, rating = rating))

test_mf <- with(test_set_reco_mf, data_memory(user = userId, item = movieId, rating = rating))

# Create the recosystem model
reco <- recosystem::Reco()

# Select tuning parameters:
opts <- reco$tune(train_mf, opts = list(dim = c(10, 20, 30), lrate = c(1.0, 0.2),
                                        costp_l1 = 0, costq_l1 = 0,
                                        nthread = 4, niter = 10))
opts

# Train the model
MF_model <- reco$train(train_mf, opts = c(opts$min, nthread = 4, niter = 10))
MF_model

# calculate the predictions
predictions_mf <- reco$predict(test_mf, out_memory())

head(predictions_mf)

MF_method_rmse <- RMSE(predictions_mf, test_set_reco_mf$rating)

models_rmse <- rbind(models_rmse, tibble(Method = 'Recosystem Parallel Matrix Factorization', 
                                         RMSE = MF_method_rmse[1]))
models_rmse[nrow(models_rmse):nrow(models_rmse),] %>% kable(caption = 'Recosystem Parallel Matrix Factorization')


## ----Table with all rmse results------------------------------------------------------------------------------------------------------------
models_rmse %>% knitr::kable(caption = 'Performance comparison of the methods on the test set')


## ----prepare validation set-----------------------------------------------------------------------------------------------------------------
# Select the userId, movieId and rating columns from the validation set
validation_reco_mf <- validation %>% select(userId, movieId, rating) 

# Load the validation data using the data_memory() function from recosystem 
valid_mf <- with(validation_reco_mf, data_memory(user = userId, 
                                                 item = movieId, 
                                                 rating = rating))

# Create predictions using the Parallel Matrix Factorization model trained above
pred_validation_mf <- reco$predict(valid_mf, out_memory())

# Calculate the RMSE for the model on the validation set
MF_validation_rmse <- RMSE(validation_reco_mf$rating, pred_validation_mf)

