source("ImportData.R")
library(dplyr)

################# We only want english books  ################

names(books) #variable names
unique(books$language_code) #distinct languages

#creating a subset of only english books 
all_books <- books[books$language_code %in% c("eng", "en-US", "en-CA","en" ),]
#testing to see if it worked
all_books$language_code <- droplevels(all_books$language_code)
unique(all_books$language_code)
nrow(all_books) # checking the no. of english books (8473)

#######################################################################
############### Consider only english books from ratings.csv #################

eng_book_ratings <- merge(ratings, all_books[, c("book_id", "language_code")], by = "book_id")
eng_book_ratings <- eng_book_ratings[, -4]

################################################################################
######### We want users who have rated at least 50 books #######################
user_ratings <- eng_book_ratings %>% 
  group_by(user_id) %>%
  summarize(count = n())
user_ratings <- user_ratings[user_ratings$count >= 50,]
min(user_ratings$count)
nrow(user_ratings)
nrow(eng_book_ratings) # checking no. of ratings before 
eng_book_ratings <- eng_book_ratings[eng_book_ratings$user_id %in% user_ratings$user_id,]
nrow(eng_book_ratings) #checking no. of ratings after 
################################################################################
######### We want books that have at least 100 ratings ##########################
bk_ratings <- eng_book_ratings %>% 
  group_by(book_id) %>%
  summarize(count = n())
bk_ratings <- bk_ratings[bk_ratings$count >= 100,]
min(bk_ratings$count) #check if it worked 
nrow(eng_book_ratings) # checking no. of ratings before 
eng_book_ratings <- eng_book_ratings[eng_book_ratings$book_id %in% bk_ratings$book_id,]
nrow(eng_book_ratings) #checking no. of ratings after 
#################################################################################
