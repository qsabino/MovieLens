##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(Metrics)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Install and load more packages that needed
if(!require(knitr)) {install.packages("knitr")
  library(knitr)}
#if(!require(signif)) {install.packages("signif")
#  library(signif)}
if(!require(rafalib)) {install.packages("rafalib")
  library(rafalib)}



##########################################################
# edx data set visualization and exploration
##########################################################

#---------------------
# 1. edx set overview
#---------------------
# Controls the number of digits to print when printing numeric values
options(digits = 6)

# edx_summary table. Give an over view of edx data set
edx_summary <- data.frame(n_rows = nrow(edx),
                          n_columns = ncol(edx),
                          n_users = n_distinct(edx$userId),
                          n_movies = n_distinct(edx$movieId),
                          average_rating = round(mean(edx$rating),2),
                          n_genres = n_distinct(edx$genres),
                          first_rating_date = date(as_datetime(min(edx$timestamp), origin = "1970-01-01")),
                          last_rating_date = date(as_datetime(max(edx$timestamp), origin = "1970-01-01"))
                          )

# Print table of edx_summary
edx_summary
                         
# edx structure
str(edx)

# Print first six rows of edx set
head(edx)

# Check for missing values
missing_values <- sapply(edx, function(x) sum(is.na(x)))
missing_values # 0 missing value



#---------------------------
# 2. Explore rating feature
#---------------------------

# Unique ratings list
unique_ratings <- sort(unique(edx$rating))
unique_ratings # no zero ratings

# Ratings distribution tibble
ratings_distribution <- edx %>% 
  group_by(rating) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

# Most to lease given ratings plot
ratings_distribution %>%  
  mutate(rating = factor(rating), rank = ifelse(rating %in% c(3,4,5),"high","low")) %>%
  ggplot(aes(x = reorder(rating, count), y = count/10^6, fill = rank)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightpink","grey")) +
  theme(legend.position = "none") +
  ggtitle("Given Ratings In Order") +
  xlab("Rating") +
  ylab("Count in millions") +
  coord_flip() # 4, 3, 5 have most given ratings.
  


#------------------------------
# 3. Explore ratings per movie
#------------------------------

# Ratings per movie tibble
ratings_per_movie <- edx %>% 
  group_by(movieId) %>% 
  summarize(n_ratings = n(),
            avg_rating = mean(rating)) %>%
  arrange(desc(n_ratings))
ratings_per_movie

# Number of ratings per movie plot
ratings_per_movie %>% 
  ggplot(aes(x = movieId, y = n_ratings)) +
  geom_point(alpha = 0.2, color = "lightpink") +
  geom_smooth(color = "purple") +
  ggtitle("Ratings Per Movie") +
  xlab("MovieId") +
  ylab("Number of ratings") # There is no movies with ID between 10000 and 25000. Smaller movieId have higher number of ratings

# Movie's Ratings Histogram
ratings_per_movie %>% 
  ggplot(aes(x = n_ratings)) +
  geom_histogram(fill = "lightpink", color = "purple") +
  # include average ratings per movie in plot.
  geom_vline(aes(xintercept = mean(n_ratings)), color = "darkred") +
  annotate("text", x = 1300, y = 750,
           label = print(round(mean(ratings_per_movie$n_ratings),0)),
           color = "darkred", size = 4) +
  ggtitle("Movie's Ratings Histogram") +
  xlab("Number of ratings (log scale x-asis)") +
  ylab("Number of movies") +
  # aipplies a logarithmic transformation to the x-axis with 10 breaks
  scale_x_log10(n.breaks = 10) # It's a nearly symmetric plot, large ratings probably for blockbuster movies

#  Half the movies are rated between 30 and 565 times
summary(ratings_per_movie$n_ratings)

# Or 75% of the movies are rated less than or equal to 565 times
quantile(ratings_per_movie$n_ratings,
         probs = 0.75, 
         na.rm = TRUE)

# Movie's Average Rating Histogram
ratings_per_movie %>% 
  ggplot(aes(x = avg_rating)) +
  geom_histogram(fill = "lightpink", color = "purple") +
  ggtitle("Movie's Average Rating Histogram") +
  xlab("Average rating") +
  ylab("Number of movies") # Left skewness indicates that there are some movies with lower rating values. These movies are pulling over all avg rating per movie to the left.



#-----------------------------
# 4. Explore ratings per user
#-----------------------------

# Ratings per user tibble
ratings_per_user <- edx %>% 
  group_by(userId) %>% 
  summarize(n_ratings = n(),
            avg_rating = mean(rating)) %>%
  arrange(desc(n_ratings))
ratings_per_user

# Number of ratings per user plot
ratings_per_user %>%
  ggplot(aes(x = userId, y = n_ratings)) +
  geom_point(alpha = 0.2, color = "lightpink") +
  geom_smooth(color = "purple") +
  ggtitle("Ratings Per User") +
  xlab("UserId") +
  ylab("Number of ratings") # Majority of users have rated less than 1000 movies. There are some outliers

# User's Ratings Histogram
ratings_per_user %>%
  ggplot(aes(x = n_ratings)) +
  geom_histogram(fill = "lightpink", color = "purple") +
  # include average ratings per user in plot
  geom_vline(aes(xintercept = mean(n_ratings)), color = "darkred")+
  annotate("text", x = 150, y = 6000,
           label = print(round(mean(ratings_per_user$n_ratings),0)),
           color = "darkred", size = 4) +
  ggtitle("User's Ratings Histogram") +
  xlab("Number of ratings (log scale x-asis)") +
  ylab("Number of users") +
  scale_x_log10(n.breaks = 10) # Right skewness indicates that not many users rated large number of movies. Some users are more active than others at rating movies

# Haft of the users rated between 32 and 141 movies
summary(ratings_per_user$n_ratings) # 6616 ratings by a user. That could be an outlier

# Or 75% of users rated less than or equal to 141 movies
quantile(ratings_per_user$n_ratings,
         probs = 0.75, 
         na.rm = TRUE)

# User's Average rating histogram
ratings_per_user %>%
  ggplot(aes(x = avg_rating, )) +
  geom_histogram(fill = "lightpink", color = "purple") +
  ggtitle("User's Average Rating Histogram") +
  xlab("Average rating") +
  ylab("Number of uses") # Symmetric histogram indicates user's avg_rating is nearly normal distribution.



#------------------------------------------------
# 5. Explore genres feature and ratings per genre
#------------------------------------------------

# List of genres
# Codes gotten from the answer to Q5 of Quiz: MovieLens Dataset
genres <- edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(n_movies = n()) %>%
  arrange(desc(n_movies))

# Genres tibble
genres <- genres %>% 
  mutate(avg_rating = sapply(genres, function(g) {
    ind <- which(str_detect(edx$genres, g))
    round(mean(edx$rating[ind]),2)
    }))
genres

# Five genres with highest n_movies
top5_genres <- head(genres,5)$genres

# Number of movies per genres plot
genres %>%
  mutate(top5 = ifelse(genres %in% top5_genres, "top5","non")) %>%
  ggplot(aes(x = reorder(genres, n_movies), y = n_movies/10^6, fill = top5)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("grey","lightpink")) +
  theme(legend.position = "none") +
  ggtitle("Number of Movies Per Genre") +
  xlab("Genres") +
  ylab("Number of moviess in millions") +
  coord_flip() # Drama, Comedy, Action, Thriller and Adventure are five genres with highest number of movies made

# Average rating per genre plot
genres %>%
  ggplot(aes(x = reorder(genres, avg_rating), avg_rating)) +
  geom_bar(stat = "identity", fill= "lightpink") +
  ggtitle("Average Rating Per Genre") +
  xlab("Genres") +
  ylab("Average rating") +
  coord_flip() # The plot shows that most genres were given a rating between 3 and 4. Horror has the worst rating



#------------------------------
# 6. Explore timestamp feature
#------------------------------

# Add "release_year" column, tells the year a movie was released, extracted from title
edx <- edx %>%
  # extract year from title feature.
  mutate(release_year = str_sub(title, start = -5, end = -2) %>% as.integer()) %>% 
  # remove year from movie's title.
  mutate(title = str_sub(title, 1, -8)) 

# Add "rating_year" column, tells the year a movie was rated in, extracted from timestamp
edx <- edx %>% 
  mutate(rating_year = year(as_datetime(timestamp, origin = "1970-01-01")) %>% as.integer())

# rating_year summary tibble
rating_year_sum <- edx %>% 
  group_by(rating_year) %>%
  summarize(n_ratings = n(),
            avg_rating = mean(rating)) %>%
  select(rating_year, n_ratings, avg_rating)
rating_year_sum

# Number of ratings per rating_year plot
rating_year_sum %>% 
  ggplot(aes(x = rating_year, y = n_ratings)) +
  geom_bar(stat = "identity", fill = "lightpink") + 
  ggtitle("Ratings Per Year") +
  xlab("Rating year") +
  ylab("Number of ratings") # The two years of 1997 and 1998 have low number of rating. Data for 2009 may be incomplete

# Average rating per rating_year plot
rating_year_sum %>% 
  ggplot(aes(x = rating_year, y = avg_rating)) +
  geom_bar(stat = "identity", fill = "lightpink") + 
  ggtitle("Average Rating Per Rating Year") +
  xlab("Rating year") +
  ylab("Average rating") # User ratings are not significantly affected by time.

# release_year summary tibble
release_year_sum <- edx %>% 
  group_by(release_year) %>%
  summarize(n_ratings = n(), 
            avg_rating = mean(rating)) %>% 
  select(release_year, n_ratings, avg_rating)
release_year_sum

# Number of ratings per released_year plot
release_year_sum %>% 
  ggplot(aes(x = release_year, y = n_ratings)) +
  geom_bar(stat = "identity", fill = "lightpink") + 
  ggtitle("Ratings Per Released Year") +
  xlab("Released year") +
  ylab("Number of ratings") # Left skewness indicates movies released before 1980 have fewer number of rating

# Average rating per release_year plot
release_year_sum %>% 
  ggplot(aes(x = release_year, y = avg_rating)) +
  geom_point(color = "lightpink") +
  geom_smooth(color = "purple") +
  ggtitle("Average Rating Per Release Year") +
  xlab("Released year") +
  ylab("Average rating") # Movies released between 1920 to 1980 seem to have higher average rating than movies released later years



##########################################################
#  Train, test data sets split for building models
##########################################################

# Codes are similar to the provided codes used to split edx and final_holdout_test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
edx_temp_test <- edx[test_index,]

# Confirm userId and movieId in test set are also in train set
edx_test <-edx_temp_test %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

#Add the rows removed from the edx_temp_test back into edx_train
removed <- anti_join(edx_temp_test, edx_test) # 17 entries
edx_train <- rbind(edx_train, removed)
rm(edx_temp_test, test_index, removed)



##########################################################
# Building models
##########################################################

# The matrix for a random sample of 100 movies and 100 users
users <- sample(unique(edx$userId), 100)
mypar() # optimizes graphical parameters
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% 
  t(.) %>% # transpose the metrix
  image(1:100, 1:100, . , xlab="moviesId", ylab="userId") +
  abline(h = 0:100+0.5, v = 0:100+0.5, col = "grey") # yellow indicating a user/movie combination for which we have a rating.



#---------------------------
# 1. Target: RMSE < 0.86490.
#---------------------------

# Create a tibble to store RMSEs, rmse_results
rmse_results <- tibble(Model = "Target: less than",
                       RMSE = signif(0.86490, 5)
                       )

# Format RMSE column as numeric
rmse_results$RMSE <- rmse_results$RMSE %>% as.numeric()

# Print rmse_results tibble
rmse_results %>% kable()



#----------------------------------------
# 2. Mean baseline model y^ = μ + ϵ(u,i).
#----------------------------------------

# Calculate average rating across training data, mu
mu <- mean(edx_train$rating)

# Predict all ratings with mu 
y_hat_mu <- rep(mu, nrow(edx_test)) 

# calculate the RMSE then add on to rmse_results tibble
rmse_results <- rbind(rmse_results, 
                      tibble(Model = "Mean baseline", 
                             RMSE = signif(rmse(edx_test$rating, y_hat_mu), 5)
                             )
                      ) 

# Print rmse_results tibble
rmse_results %>% kable()



#--------------------------------------------
# 3. Median baseline model y^ = med + ϵ(u,i).
#--------------------------------------------

# Find median rating across training data, med
med <- median(edx_train$rating)

# Predict all ratings with median
y_hat_med <- rep(med, nrow(edx_test))

# Calculate the RMSE
rmse_results <- rbind(rmse_results, 
                      tibble(Model = "Median baseline", 
                             RMSE = signif(rmse(edx_test$rating, y_hat_med), 5)
                             )
                      )

# Print rmse_results tibble
rmse_results %>% kable()



#--------------------------------------------
# 4. Movie bias model y^ = μ + ϵ(u,i) + b_i.
#--------------------------------------------

# Compute movie bias term, b_i
b_i <- edx_train %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

# b_i plot
b_i %>%
  ggplot(aes(x = b_i)) + 
  geom_histogram(bins = 20, fill = "lightpink", color = "purple") +
  ggtitle("Movie Bias Histogram") +
  xlab("Bias value") +
  ylab("Count") # these estimates vary substantially

# Predict all ratings with mu + b_i
y_hat_bi <- mu + edx_test %>% 
  left_join(b_i, by = "movieId") %>% 
  .$b_i

# Calculate RMSE with movie ranking effect
rmse_results <- rbind(rmse_results, 
                      tibble(Model = "Mean + Movie bias", 
                             RMSE = signif(rmse(edx_test$rating, y_hat_bi), 5)
                             )
                      ) 

# Print rmse_results tibble
rmse_results %>% kable() # Notice the improvement of RMSE



#----------------------------------------------------------
# 5. Movie and user bias model y^ = μ + ϵ(u,i) + b_i + b_u. 
#----------------------------------------------------------

# Compute user bias term, b_u
b_u <- edx_train %>%
  left_join(b_i, by = 'movieId') %>%
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i))

# b_u plot
b_u %>%
  ggplot(aes(x = b_u)) + 
  geom_histogram(bins = 20, fill = "lightpink", color = "purple") +
  ggtitle("User Bias Histogram") +
  xlab("Bias value") +
  ylab("Count") # there is substantial variability across users as well
           
# Predict all ratings with mu + b_i + b_u
y_hat_bu <- edx_test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>% 
  mutate(y_hat_bu = mu + b_i + b_u) %>%
  .$y_hat_bu

# Calculate the RMSE with movie and user ranking effect
rmse_results <- rbind(rmse_results, 
                      tibble(Model = "Mean + Movie bias + User bias", 
                             RMSE = signif(rmse(edx_test$rating, y_hat_bu), 5)
                             )
                      )

# Print rmse_results tibble
rmse_results %>% kable() # Notice further improvement of the RMSE 



#-----------------------------------------------------------------------
# 6. Movie, user and genre bias model y^ = μ + ϵ(u,i) + b_i + b_u + b_g.
#-----------------------------------------------------------------------

# Compute genre bias term, b_g
b_g <- edx_train %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>% 
  group_by(genres) %>% 
  summarise(b_g = mean(rating - mu - b_i - b_u))

# b_g plot
b_g %>%
  ggplot(aes(x = b_g)) + 
  geom_histogram(bins = 20, fill = "lightpink", color = "purple") +
  ggtitle("Genre Bias Histogram") +
  xlab("Bias value") +
  ylab("Count") # There are variability across genres as well

# Predict all ratings with mu + b_i + b_u + b_g
y_hat_bg <- edx_test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>% 
  left_join(b_g, by = "genres") %>% 
  mutate(y_hat_bg = mu + b_i + b_u + b_g) %>%
  .$y_hat_bg

# Calculate the RMSE with movie and user ranking effect
rmse_results <- rbind(rmse_results, 
                      tibble(Model = "Mean + Movie bias + User bias + Genre bias", 
                             RMSE = signif(rmse(edx_test$rating, y_hat_bg), 5)
                             )
                      ) 

# Print rmse_results tibble
rmse_results %>% kable() # Notice more improvement of the RMSE 



#--------------------
# 7. Regularization.
#--------------------

# Regularization function
regularization <- function(lambda, train, test){ 
  
  # Calculate average rating across training data, mu
  mu <- mean(train$rating)
  
  # Movie bias regularization term
  b_i_reg <- train %>% 
    group_by(movieId) %>%
    summarise(b_i_reg = sum(rating - mu) / (n() + lambda)) 
  
  # User bias regularization term
  b_u_reg <- train %>% 
    left_join(b_i_reg, by = "movieId") %>%
    filter(!is.na(b_i_reg)) %>%
    group_by(userId) %>%
    summarise(b_u_reg = sum(rating - mu - b_i_reg) / (n() + lambda))
  
  # Genre bias regularization term
  b_g_reg <- train %>% 
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>% 
    filter(!is.na(b_i_reg), !is.na(b_u_reg)) %>%
    group_by(genres) %>%
    summarise(b_g_reg = sum(rating - mu - b_i_reg - b_u_reg) / (n() + lambda))
  
  # Predict all ratings using regularization terms
  y_hat <- test %>% 
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    left_join(b_g_reg, by = "genres") %>% 
    filter(!is.na(b_i_reg), !is.na(b_u_reg), !is.na(b_g_reg)) %>%
    mutate(y_hat = mu + b_i_reg + b_u_reg + b_g_reg) %>%
    .$y_hat
  
  # Calculate and return rmses
  return(rmse(test$rating, y_hat))
}

# Cross validation to find best lambda
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, regularization, edx_train, edx_test)

# Find best lambda
lambda <- lambdas[which.min(rmses)]
lambda

# lambdas vs. rsmes plot
qplot(x = lambdas, y = rmses) 

# Define rmse with the best lambda
rmse <- min(rmses)

# Add rmse to rmse_results tibble
rmse_results <- rbind(rmse_results, 
                      tibble(Model = "Mean + Regularization of Movie bias, User bias and Genre bias", 
                             RMSE = signif(rmse, 5)
                             )
                      )

# Print rmse_results tibble
rmse_results %>% kable() # Notably improvement of RMSE



################################################################################
# Result: Regularization linear final model validation using final_holdout_test
################################################################################

# Calculate RMSE of final regularization linear model
RMSE <- regularization(lambda, edx_train, final_holdout_test)

rmse_results <- rbind(rmse_results, 
                      tibble(Model = "Final model validation with final_holdout_test", 
                             RMSE = signif(RMSE, 5)
                             )
                     ) 

# Print rmse_results tibble
rmse_results %>% kable()

