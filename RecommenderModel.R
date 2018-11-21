source("DataCleaning.R")
library(recommenderlab)
library(dplyr)

# Loading the normalized ratings matrix:
load("normalized_ratings.RData")

#Building the UBCF model 
rec_mod = Recommender(normalized_ratings, method = "UBCF", param=list(method="cosine",nn=4))

# Make predictions for the first user and store it in a list. 
user <- 1
predictions = predict(rec_mod, normalized_ratings[user,], type = "ratings")
pred_list = as(predictions, "list")

#Convert list to data frame, add book_id to it as well. 
predictions_df <- as.data.frame(pred_list)
names(predictions_df) <- "rating"
predictions_df$book_id <- row.names(predictions_df)

# extracting the top 20 recommendations 
top20 <- predictions_df %>%
  arrange(desc(rating)) %>%
  top_n(20, wt = rating) %>%
  select(book_id, rating)

# converting book_id to numeric 
top20$book_id <- as.numeric(top20$book_id)

# merging tables in order to get the book titles 
recommended_books = left_join(top20, all_books, by = "book_id")
recommended_books$title
