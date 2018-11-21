source("ImportData.R")
source("DataCleaning.R")

library(reshape2)
library(recommenderlab)

updated_ratings_matrix <- dcast(data = eng_book_ratings, user_id~book_id, value.var = "rating")
# write.csv(updated_ratings_matrix, file = "updated_ratings_matrix.csv")

updated_ratings_matrix[is.na(updated_ratings_matrix)]<- 0
updated_ratings_matrix = as.matrix(updated_ratings_matrix[,-1])
sparse_ratings <- as(updated_ratings_matrix, "sparseMatrix")
rm(updated_ratings_matrix)
gc()

####### creating the ratings matrices
# memory.limit(size = 180000)
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings
# save(updated_ratings_matrix, file = "updated_ratings_matrix.RData")

####### normalizing the ratings matrices
normalized_ratings <- normalize(real_ratings)
rec_mod = Recommender(normalized_ratings, method = "UBCF", param=list(method="Cosine",nn=10))
Top_5_pred = predict(rec_mod, normalized_ratings[], n=10)
Top_5_List = as(Top_5_pred, "list")
Top_5_List

#Convert to data frame, change column name to book id 
Top5_df <- as.data.frame(Top_5_List)
names(Top5_df) <- "book_id"
head(Top5_df)
Top5_df$book_id <- as.numeric(Top5_df$book_id)
recommended_books = left_join(Top5_df, all_books, by = "book_id")
recommended_books$title
class(all_books$book_id)
# save(norm_updated_ratings_mat, file = "norm_updated_ratings_mat.RData")

####### binarising the ratings matrices
#bin_ratings_mat <- binarize(updated_ratings_matrix, minRating = 3)

