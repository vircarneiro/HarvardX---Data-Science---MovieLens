################################################################
### Create edx set, validation set (final hold-out test set) 
################################################################

# Install and Loading libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(datetime)) install.packages("datetime", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(datetime)
library(lubridate)

# Download dataset MovieLens 10M
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################################################
### Data Validation - Edx dataset    
################################################################

str(edx)

# Finding missing values 
sum(is.na(edx)) #0

# Finding wrong information
edx_users <- edx %>% 
  select(userId) %>%
  distinct() # 69878
str(edx_users)

edx_movieId <- edx %>% 
  select(movieId) %>%
  distinct() # 10677
str(edx_movieId)

## 2 Movies with different movieId, but same title.
edx_movieId_title <- edx %>% 
  select(movieId,title) %>% 
  distinct() # 10677
str(edx_movieId_title)

which(duplicated(edx_movieId_title$title))

edx_movieId_title[6538]

edx %>% select(movieId,title,genres) %>%
  filter(title == "War of the Worlds (2005)") %>%
  group_by (movieId,title,genres) %>%
  distinct(n())

edx %>% filter(title == "War of the Worlds (2005)", genres == "Action")

edx_title_genres <- edx %>% 
  select(title,genres) %>% 
  distinct() #10677
str(edx_title_genres)

which(duplicated(edx_title_genres))

edx_rating <- edx %>% 
  select(rating) %>%
  distinct() # 10 
str(edx_rating)

edx_title <- edx %>% 
  select(title) %>%
  distinct() # 10676
str(edx_title)

edx_genres <- edx %>% 
  select(genres) %>%
  distinct() #797
str(edx_genres)  


# There are 7 "(no genres listed)" genres
#table(distinct(edx_genres))

edx %>% filter(genres == "(no genres listed)")
str(edx)

# Remove "(no genres listed)" genres
edx <- edx %>%filter(genres != "(no genres listed)")

str(edx)

# Clean workspace 
remove(edx_genres, edx_movieId_title, edx_rating, edx_title, edx_title_genres)

################################################################
### Data Validation - Validation dataset
################################################################

str(validation)

# Finding missing values 
sum(is.na(validation)) #0

# Finding wrong information
validation_users <- validation %>% 
  select(userId) %>%
  distinct() # 68534
str(validation_users)

validation_movieId <- validation %>% 
  select(movieId) %>%
  distinct() # 9809
str(validation_movieId)

## 2 Movies with different movieId, but same Title

validation_movieId_title <- validation %>% 
  select(movieId,title) %>% 
  distinct() # 9809
str(validation_movieId_title)

which(duplicated(validation_movieId_title$title))

validation_movieId_title[6458]

validation %>% select(movieId,title,genres) %>%
  filter(title == "War of the Worlds (2005)") %>%
  group_by (movieId,title,genres) %>%
  distinct(n())

validation %>% filter(title == "War of the Worlds (2005)", genres == "Action")

validation_title_genres <- validation %>% 
  select(title,genres) %>% 
  distinct() #9809
str(validation_title_genres)

which(duplicated(validation_title_genres)) 

validation_title <- validation %>% 
  select(title) %>%
  distinct() # 9808
str(validation_title)

validation_genres <- validation %>% 
  select(genres) %>%
  distinct() #773
str(validation_genres)  

# There are not "(no genres listed)" genres
#table(distinct(edx_genres))

validation %>% filter(genres == "(no genres listed)")

str(validation)

# Clean workspace 
remove(validation_genres, validation_movieId_title, 
       validation_title, validation_title_genres)

################################################################
### Data Validation - Edx vs Validation datasets
################################################################

str(anti_join(validation_users,edx_users)) # 0 -> all the userId in validation dataset are in edx dataset

str(anti_join(validation_movieId,edx_movieId)) # 0 -> all the moviesId in validation dataset are in edx dataset

# Clean workspace 
remove(validation_users, edx_users, validation_movieId, edx_movieId)

################################################################
### Data Visualization - Edx and Validation datasets
################################################################

####################################################
# Feature Engineering - Edx dataset
####################################################

# Knowing how many different genres has edx
edxgenres <- edx %>% 
  select(genres) %>%
  distinct()

# One-hot encoding - Split the genres by columns
edxgenres <- separate(data=edxgenres,col=genres,into = c("a","b","c","d","e","f","g","h"), sep="\\|")
edxgenres <- stack(edxgenres)
edxgenres <- edxgenres %>% 
  select(values) %>%
  distinct()
edxgenres <- edxgenres %>% drop_na()

# Columns generation
for (x in edxgenres$values) {
  new_column <- grepl(x, edx$genres, fixed=TRUE)
  edx$new_column <- as.numeric(new_column)
  colnames(edx)[ncol(edx)] <- str_replace(x, "-", "_") 
}

# Columns generation (give a value according genres)
counter <- 0
column <- NULL

for (x in edxgenres$values) {
  
  new_column <- grepl(x, edx$genres, fixed=TRUE)
  if (is.null(column)) {
    column <- as.numeric(new_column) * (2^counter)
  }
  else {
    column <- column + as.numeric(new_column) * (2^counter)
  }
  
  counter <- counter + 1
}
edx$genres = column

# Extract year from title
# Example from here https://community.rstudio.com/t/extract-text-between-brakets/43448/5
edx$year_movie <- as.numeric(str_extract(edx$title, regex("(?<=\\()\\d{4}(?=\\))")))

edx_title_year_distinct <- edx %>% 
  select(title,year_movie)%>% 
  arrange(title)%>%
  distinct()

# Remove column tittle
edx <- edx[,!("title")]

# Years
edx_years<- edx %>% 
  select(year_movie) %>% 
  group_by(year_movie) %>% 
  distinct()

# Extract Year/Month/day Rated
edx <- edx %>% mutate(timestamp = as.date(timestamp))
edx <- edx %>% mutate(year_rated = as.numeric(format(edx$timestamp, "%Y")))
edx <- edx %>% mutate(month_rated = as.numeric(format(edx$timestamp, "%m")))
edx <- edx %>% mutate(day_rated = as.numeric(format(edx$timestamp, "%d")))

# Remove timestamp
edx <- edx[,!("timestamp")]

# Add new difference between year_rated - year_movie
edx <- edx %>% mutate(dif_rated_movie = year_rated - year_movie)

# Finding missing values 
sum(is.na(edx))

str(edx)

##########################################################
# Feature Engineering - Validation dataset
##########################################################

head(validation)

# Knowing how many different genres has edx
validationgenres <- validation %>% 
  select(genres) %>%
  distinct() 

# One-hot encoding
validationgenres <- separate(data=validationgenres,col=genres,into = c("a","b","c","d","e","f","g","h"), sep="\\|")
validationgenres <- stack(validationgenres)
validationgenres <- validationgenres %>% 
  select(values) %>%
  distinct()
validationgenres <- validationgenres %>% drop_na()

# Columns generation
for (x in validationgenres$values) {
  new_column <- grepl(x, validation$genres, fixed=TRUE)
  validation$new_column <- as.numeric(new_column)
  colnames(validation)[ncol(validation)] <- str_replace(x, "-", "_")
}

# Columns generation (give a value acording genres)
counter <- 0
column <- NULL

for (x in edxgenres$values) {
  new_column <- grepl(x, validation$genres, fixed=TRUE)
  
  if (is.null(column)) {
    column <- as.numeric(new_column) * (2^counter)
    head(column)
  }
  else {
    column <- column + as.numeric(new_column) * (2^counter)
  }
  counter <- counter + 1
}

validation$genres = column

# Extract year from title
# Example from here (https://community.rstudio.com/t/extract-text-between-brakets/43448/5)
validation$year_movie <- as.numeric(str_extract(validation$title, regex("(?<=\\()\\d{4}(?=\\))")))

validation_title_year_distinct <- validation %>% 
  select(title,year_movie)%>% 
  arrange(title)%>%
  distinct()

# Remove column tittle
validation <- validation[,!("title")]

# Years
validation_years<- validation %>% 
  select(year_movie) %>% 
  group_by(year_movie) %>% 
  distinct()

# Extract Year/Month/day Rated
validation <- validation %>% mutate(timestamp = as.date(timestamp))
validation <- validation %>% mutate(year_rated = as.numeric(format(validation$timestamp, "%Y")))
validation <- validation %>% mutate(month_rated = as.numeric(format(validation$timestamp, "%m")))
validation <- validation %>% mutate(day_rated = as.numeric(format(validation$timestamp, "%d")))

# Remove timestamp
validation <- validation[,!("timestamp")]

# Add new difference between year_rated - year_movie
validation <- validation %>% mutate(dif_rated_movie = year_rated - year_movie)

# Finding missing values 
sum(is.na(validation))

str(validation)
str(edx)

##########################################################
# Data visualization - Edx dataset
##########################################################

# Edx Ratings by Users
edx_rating_users <- edx %>% 
  select(userId, movieId) %>% 
  group_by(userId) %>%
  summarize(qty = n()) 

edx_rating_users %>% 
  ggplot(aes(x = userId, y = qty)) + 
  geom_point(color= '#f895c4')+ 
  labs(title = "Edx - Ratings by Users",
       x = "Users",
       y = "Quantity of Movies")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Edx Release Movies by Year
edx_movies_year_movie <- edx %>% 
  select(movieId, year_movie) %>% 
  group_by(year_movie) %>%
  distinct() %>%
  summarize(qty = n())

edx_movies_year_movie %>% 
  ggplot(aes(x = year_movie, y = qty)) + 
  geom_point(color= '#f895c4')+ 
  scale_y_continuous(breaks = c(0, 50, 150, 200, 250, 300, 350, 400, 450))+
  labs(title = "Edx - Release Movies by Year",
       x = "Years",
       y = "Quantity of Movies")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Edx Movies by Genres 
edx_movies_genres <- edx %>% 
  select(movieId,Comedy,Action,Adventure,Animation,Drama,Crime,Sci_Fi,
         Horror,Thriller,Film_Noir,Mystery,Western,Documentary,Romance,Fantasy,
         Musical,War,Children,IMAX) %>%
  distinct()

edx_movies_genres_colSums <- data.frame(Qty=colSums(edx_movies_genres[,-1])) %>% arrange(desc(Qty))

ggplot(data = edx_movies_genres_colSums, aes(x = rownames(edx_movies_genres_colSums),y = Qty)) +
  geom_bar(stat = "identity", color= '#f895c4', fill = '#f895c4') +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500))+
  labs(title = "Edx - Movies by Genres",
       x = "Genres",
       y = "Quantity of Movies") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Edx Year Rated Vs Year Release
edx_movie_release_rated <- edx %>% select(userId, movieId, year_movie, year_rated) %>%
  summarize(difference = year_rated - year_movie) %>%
  group_by (difference) %>%
  summarize(Qty = n()) %>%
  arrange(Qty)

ggplot(data = edx_movie_release_rated, aes(x = difference, y = Qty)) +
  geom_bar(stat = "identity", color= '#f895c4', fill = '#f895c4') +
  labs(title = "Edx - Difference Between Years Rated and Years Release",
       x = "Difference in Years (rated - premier)",
       y = "Quantity of Movies") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

##########################################################
# Data visualization - Validation dataset
##########################################################

# Validation Ratings by Users
validation_rating_users <- validation %>% 
  select(userId, movieId) %>% 
  group_by(userId) %>%
  summarize(qty = n())

validation_rating_users %>% 
  ggplot(aes(x = userId, y = qty)) + 
  geom_point(color='#98bddc')+ 
  labs(title = "Validation - Ratings by Users",
       x = "Users",
       y = "Quantity of Movies")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Validation Release Movies by Year
validation_movies_year_movie <- validation %>% 
  select(movieId, year_movie) %>% 
  group_by(year_movie) %>%
  distinct() %>%
  summarize(qty = n())

validation_movies_year_movie %>% 
  ggplot(aes(x = year_movie, y = qty)) + 
  geom_point(color='#98bddc')+ 
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450))+
  labs(title = "Validation - Release Movies by Year",
       x = "Years",
       y = "Quantity of Movies")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Validation Movies by Genres
validation_movies_genres <- validation %>% 
  select(movieId,Comedy,Action,Adventure,Animation,Drama,Crime,Sci_Fi,
         Horror,Thriller,Film_Noir,Mystery,Western,Documentary,Romance,Fantasy,
         Musical,War,Children,IMAX) %>%
  distinct()

validation_movies_genres_colSums <-data.frame(Qty=colSums(validation_movies_genres[,-1]))

ggplot(data = validation_movies_genres_colSums, aes(x = rownames(validation_movies_genres_colSums),y = Qty)) +
  geom_bar(stat = "identity", color= '#98bddc', fill = '#98bddc') +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500))+
  labs(title = "Validation - Movies by Genres",
       x = "Genres",
       y = "Quantity of Movies") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Validation Year Rated Vs Year Release
validation_movie_release_rated <- validation %>% select(userId, movieId, year_movie, year_rated) %>%
  summarize(difference = year_rated - year_movie) %>%
  group_by (difference) %>%
  summarize(Qty = n()) %>%
  arrange(Qty)

ggplot(data = validation_movie_release_rated, aes(x = difference, y = Qty)) +
  geom_bar(stat = "identity", color= '#98bddc', fill = '#98bddc') +
  labs(title = "Validation - Difference Between Years Rated and Years Release",
       x = "Difference in Years (rated - premier)",
       y = "Quantity of Movies") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

################################################################
### Edx - Prepare dataset to run XGBooost
################################################################

# Clean genres columns with 0 and 1 values.  
edx <- edx[,-c("Comedy","Action","Adventure","Animation","Drama","Crime","Sci_Fi",
               "Horror","Thriller","Film_Noir","Mystery","Western","Documentary","Romance","Fantasy",
               "Musical","War","Children","IMAX")]

validation <- validation[,-c("Comedy","Action","Adventure","Animation","Drama","Crime","Sci_Fi",
                             "Horror","Thriller","Film_Noir","Mystery","Western","Documentary","Romance","Fantasy","Musical","War","Children","IMAX")]

str(edx)

# Function to calculate mode
getmode <- function(v) {
  val <- unique(v)
  val[which.max(tabulate(match(v, val)))]
}

# Target Encoding by User (TRAIN)
enc <- edx[,c("userId","rating")] %>%
  group_by(userId) %>% 
  summarise(
    userId_sd=sd(rating),
    userId_mean=mean(rating),
    userId_median=median(rating),
    userId_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by User (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by Movie (TRAIN)
enc <- edx[,c("movieId","rating")] %>%
  group_by(movieId) %>% 
  summarise(
    movieId_sd=sd(rating),
    movieId_mean=mean(rating),
    movieId_median=median(rating),
    movieId_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by Movie (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by Movie Year (TRAIN)
enc <- edx[,c("year_movie","rating")] %>%
  group_by(year_movie) %>% 
  summarise(
    year_movie_sd=sd(rating),
    year_movie_mean=mean(rating),
    year_movie_median=median(rating),
    year_movie_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by Movie Year (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by Year Rated (TRAIN)
enc <- edx[,c("year_rated","rating")] %>%
  group_by(year_rated) %>% 
  summarise(
    year_rated_sd=sd(rating),
    year_rated_mean=mean(rating),
    year_rated_median=median(rating),
    year_rated_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by Year Rated (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by Month Rated (TRAIN)
enc <- edx[,c("month_rated","rating")] %>%
  group_by(month_rated) %>% 
  summarise(
    month_rated_sd=sd(rating),
    month_rated_mean=mean(rating),
    month_rated_median=median(rating),
    month_rated_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by Month Rated (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by Day Rated (TRAIN)
enc <- edx[,c("day_rated","rating")] %>%
  group_by(day_rated) %>% 
  summarise(
    day_rated_sd=sd(rating),
    day_rated_mean=mean(rating),
    day_rated_median=median(rating),
    day_rated_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by Day Rated (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by UserId, year_rated (TRAIN)
enc <- edx[,c("userId","year_rated","rating")] %>%
  group_by(userId, year_rated) %>% 
  summarise(
    userId_year_rated_sd=sd(rating),
    userId_year_rated_mean=mean(rating),
    userId_year_rated_median=median(rating),
    userId_year_rated_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by UserId, year_movie (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by UserId, month_rated (TRAIN)
enc <- edx[,c("userId","month_rated","rating")] %>%
  group_by(userId, month_rated) %>% 
  summarise(
    userId_month_rated_sd=sd(rating),
    userId_month_rated_mean=mean(rating),
    userId_month_rated_median=median(rating),
    userId_month_rated_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by UserId, year_movie (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by UserId, day_rated (TRAIN)
enc <- edx[,c("userId","day_rated","rating")] %>%
  group_by(userId, day_rated) %>% 
  summarise(
    userId_day_rated_sd=sd(rating),
    userId_day_rated_mean=mean(rating),
    userId_day_rated_median=median(rating),
    userId_day_rated_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by UserId, year_movie (VALIDATION)
validation <- left_join(validation,enc)

# Target Encoding by movieId, year_movie (TRAIN)
enc <- edx[,c("movieId","year_movie","rating")] %>%
  group_by(movieId, year_movie) %>% 
  summarise(
    movieId_year_movie_sd=sd(rating),
    movieId_year_movie_mean=mean(rating),
    movieId_year_movie_median=median(rating),
    movieId_year_movie_mode=getmode(rating),
  )

edx <- left_join(edx,enc)

# Target Encoding by movieId, year_movie (VALIDATION)
validation <- left_join(validation,enc)

# Check NA in Train
print("------ TRAIN ------")
as.table(sapply(edx, function(y) sum(length(which(is.na(y))))))

# Check NA in Validation
print("------ VALIDATION ------")
as.table(sapply(validation, function(y) sum(length(which(is.na(y))))))

# Assign zero to NA (TRAIN)
edx[is.na(edx)] <- 0

# Assign zero to NA (VALIDATION)
validation[is.na(validation)] <- 0

# Free memory
enc=NULL
gc()

##########################################################
# Edx - Create Train and Test dataset
##########################################################

# Create training and test sets from edx dataset.
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_edx <- edx[-test_index,]
test_edx <- edx[test_index,]

# Free memory
edx = NULL
gc()

str(train_edx)
str(test_edx)

# Sabe train_edx, test_edx and validation
saveRDS(train_edx, file="train_edx.rds")
saveRDS(test_edx, file="test_edx.rds")
saveRDS(validation, file="validation.rds")

# Read train_edx, test_edx and validation
train_edx <- readRDS(file="train_edx.rds")
test_edx <- readRDS(file="test_edx.rds")
validation <- readRDS(file="validation.rds")

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(xgboost)

##-.-.-.-.-.-.-.-.-.-.- MODEL 1 - DEFAULT VALUES -.-.-.-.-.-.-.-.-.-.-.##
# IT WILL BE CREATED WITH THE DEFAULT VALUES. THIS IS THE *BASELINE MODEL*.

# max_depth = 6 (default)
# eta = 0.3 (default)
# min_child_weight = 1 (default)

set.seed(1, sample.kind = "Rounding")

y <- train_edx$rating
x <- data.matrix(train_edx[,!("rating")])
d_train <- xgb.DMatrix(data = x, label = y)

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])
d_test <- xgb.DMatrix(data = x, label = y)

# Free memory
y <- NULL
x <- NULL
gc()

watchlist <- list(train = d_train, eval = d_test)

fit <- xgb.train(data=d_train,
                 nrounds = 5000,
                 verbose = FALSE, 
                 tree_method = 'hist',
                 early_stopping_rounds = 20, 
                 watchlist = watchlist,
                 eval_metric = 'rmse')

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])

yhat = predict(fit, x)
RMSE_model_I <- RMSE(y, yhat) 

#RMSE TEST DATASET
rmse_table <- data_frame(Method = "BASELINE - TEST", RMSE = RMSE_model_I)
rmse_table %>% knitr::kable(caption = "RMSEs")

# Importance Matrix.
# Show us how useful is each feature in the construction of the boosted decision trees.
importance_matrix_model_I <- xgb.importance(model = fit)
xgb.plot.importance(importance_matrix = importance_matrix_model_I)

##---- RMSE_model_I - TEST -> 0.8310898 ----##

##-.-.-.-.-.-.-.-.-.-.- MODEL 2 - MAX TREE DEPTH = 8 -.-.-.-.-.-.-.-.-.-.-.##
# IN THIS MODEL I'M CHANGING THE TREE DEPTH FROM 6 TO 8.  WILLING AN IMPROVEMENT IN THE RMSE.

# max_depth = 8
# eta = 0.3 (default)
# min_child_weight = 1 (default)

set.seed(1, sample.kind="Rounding")

y <- train_edx$rating
x <- data.matrix(train_edx[,!("rating")])
d_train <- xgb.DMatrix(data = x, label = y)

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])
d_test <- xgb.DMatrix(data = x, label = y)

# Free memory
y <- NULL
x <- NULL
gc()

watchlist <- list(train = d_train, eval = d_test)

fit <- xgb.train(data=d_train, 
                 nrounds = 5000,
                 verbose = FALSE, 
                 tree_method ='hist',
                 max_depth = 8,
                 early_stopping_rounds = 20, 
                 watchlist = watchlist,
                 eval_metric = 'rmse')

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])

yhat = predict(fit, x)
RMSE_model_II <- RMSE(y, yhat)

#RMSE TEST DATASET
rmse_table <- rbind(rmse_table, data_frame(Method = "MAX TREE DEPTH = 8 - TEST", RMSE = RMSE_model_II))
rmse_table %>% knitr::kable(caption = "RMSEs")

# Importance matrix
importance_matrix_model_II <- xgb.importance(model = fit)
xgb.plot.importance(importance_matrix = importance_matrix_model_II)

##---- RMSE_model_II- TEST -> 0.8302726 ----##

##-.-.-.-.-.-.-.-.-.-.- MODEL 3 - MAX TREE DEPTH = 10  -.-.-.-.-.-.-.-.-.-.-.##
# BECAUSE THERE WAS AN IMPROVEMENT IN MODEL 2, I'M INCREASING THE TREE DEPTH FROM 8 TO 10.

# max_depth = 10
# eta = 0.3 (DEFAULT)
# min_child_weight = 1 (default)

set.seed(1, sample.kind="Rounding")

y <- train_edx$rating
x <- data.matrix(train_edx[,!("rating")])
d_train <- xgb.DMatrix(data = x, label = y)

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])
d_test <- xgb.DMatrix(data = x, label = y)

# Free Memory
y <- NULL
x <- NULL
gc()

watchlist <- list(train = d_train, eval = d_test)

fit <- xgb.train(data=d_train, 
                 nrounds = 5000,
                 verbose = FALSE,
                 tree_method = 'hist',
                 max_depth = 10,
                 early_stopping_rounds = 20,  
                 watchlist = watchlist,
                 eval_metric = 'rmse')

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])

yhat = predict(fit, x)
RMSE_model_III <- RMSE(y, yhat)

#RMSE TEST DATASET
rmse_table <- rbind(rmse_table, data_frame(Method = "MAX TREE DEPTH = 10 - TEST", RMSE = RMSE_model_III))
rmse_table %>% knitr::kable(caption = "RMSEs")

# Imprtance Matrix
importance_matrix_model_III <- xgb.importance(model = fit)
xgb.plot.importance(importance_matrix = importance_matrix_model_III)

##---- RMSE_model_III - TEST -> 0.8316796 ----##

'THERE WAS NOT IMPROVEMENT IN THIS MODEL CHANGING THE TREE DEPTH TO 10.  THEN, 
FOR THE REST OF THE MODELS IM KEEPING THE TREE DEPTH = 8 AS THE MOST OPTIMAL.'

##-.-.-.-.-.-.-.-.-.-.- MODEL 4 - MAX TREE DEPTH = 8 AND MIN CHILD WEIGHT = 300 -.-.-.-.-.-.-.-.-.-.-.##
#IN THIS MODEL I'M CHANGING THE CHILD WEIGHT FROM THE DEFAUL 1 TO 300.  WILLING AN IMPROVEMENT IN THE RMSE.

# max_depth = 8
# eta = 0.3 (DEFAULT)
# min_child_weight = 300

set.seed(1, sample.kind="Rounding")

y <- train_edx$rating
x <- data.matrix(train_edx[,!("rating")])
d_train <- xgb.DMatrix(data = x, label = y)

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])
d_test <- xgb.DMatrix(data = x, label = y)

# Free Memory
y <- NULL
x <- NULL
gc()

watchlist <- list(train = d_train, eval = d_test)

fit <- xgb.train(data=d_train, 
                 nrounds = 5000, 
                 verbose = FALSE,
                 tree_method = 'hist',
                 max_depth = 8,
                 min_child_weight = 300,
                 early_stopping_rounds = 20, 
                 watchlist = watchlist,
                 eval_metric = 'rmse')

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])

yhat = predict(fit, x)
RMSE_model_IV <- RMSE(y, yhat)

#RMSE TEST DATASET
rmse_table <- rbind(rmse_table, data_frame(Method = "MAX TREE DEPTH = 8 AND CHILD WEIGHT = 300 - TEST", RMSE = RMSE_model_IV))
rmse_table %>% knitr::kable(caption = "RMSEs")

#Importance Matrix
importance_matrix_model_IV <- xgb.importance(model = fit)
xgb.plot.importance(importance_matrix = importance_matrix_model_IV)

##---- RMSE_model_IV - TEST -> 0.8288294 ----##

'THERE WAS AN IMPROVEMENT IN THESE MODEL CHANGING THE CHILD WEIGHT.  
THEN, FOR THE REST OF THE MODELS IM KEEPING THE CHILD WEIGHT = 300 AS THE MOST OPTIMAL.'

##-.-.-.-.-.-.-.-.-.-.- MODEL 5 - MAX TREE DEPTH = 8, ETA = 0.1 AND MIN CHILD = 300 -.-.-.-.-.-.-.-.-.-.-.##
#FOR THIS MODEL I'M CHANGING THE ETA FROM THE DEFAULT 0.3 TO 0.1.  WILLING AN IMPROVEMENT IN THE RMSE.

# max_depth = 8 
# eta = 0.1
# min_child_weight = 300

set.seed(1, sample.kind="Rounding")

y <- train_edx$rating
x <- data.matrix(train_edx[,!("rating")])
d_train <- xgb.DMatrix(data = x, label = y)

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])
d_test <- xgb.DMatrix(data = x, label = y)

# Free memory
y <- NULL
x <- NULL
gc()

watchlist <- list(train = d_train, eval = d_test)

fit <- xgb.train(data=d_train, 
                 nrounds = 5000, 
                 verbose = FALSE, 
                 tree_method = 'hist',
                 max_depth = 8,
                 min_child_weight = 300,
                 eta = 0.1,
                 early_stopping_rounds = 20,  
                 watchlist = watchlist,
                 eval_metric = 'rmse')

y <- test_edx$rating
x <- data.matrix(test_edx[,!("rating")])

yhat = predict(fit, x)
RMSE_model_V <- RMSE(y, yhat)

#RMSE TEST DATASET
rmse_table <- rbind(rmse_table, data_frame(Method = "MAX TREE DEPTH = 8, ETA = 0.1, MIN CHILD = 300  - TEST", RMSE = RMSE_model_V))
rmse_table %>% knitr::kable(caption = "RMSEs")

##---- RMSE_model_V - TEST -> 0.8249370 ----##

'BECAUSE THIS MODEL REACHED AN ACCEPTABLE RMSE IN TEST. I AM GOING TO USE THE VALIDATION DATASET
TO PREDICT THE RATING THAT A USER WILL GIVE TO A MOVIE.'

## Validation Dataset

y <- validation$rating
x <- data.matrix(validation[,!("rating")])

yhat = predict(fit, x)
RMSE_model_VI <- RMSE(y, yhat)

# RMSE VALIDATION DATASET
rmse_table <- rbind(rmse_table, data_frame(Method = "MAX TREE DEPTH = 8 AND ETA = 0.1, MIN CHILD = 300 - VALIDATION", RMSE = RMSE_model_VI))
rmse_table %>% knitr::kable(caption = "RMSEs") 

##---- RMSE_model_V - VALIDATION -> 0.8582224 ----##

# Importance Matrix
importance_matrix_model_VI <- xgb.importance(model = fit)
xgb.plot.importance(importance_matrix = importance_matrix_model_VI)


