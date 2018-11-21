source("ImportData.R")
source("DataCleaning.R")

library(reshape2)
library(recommenderlab)

updated_ratings_matrix <- dcast(data = eng_book_ratings, user_id~book_id, value.var = "rating")
# write.csv(updated_ratings_matrix, file = "updated_ratings_matrix.csv")

#Converting it into a sparse matrix to save space 
updated_ratings_matrix[is.na(updated_ratings_matrix)]<- 0
updated_ratings_matrix = as.matrix(updated_ratings_matrix[,-1])
sparse_ratings <- as(updated_ratings_matrix, "sparseMatrix")
rm(updated_ratings_matrix)
gc()

####### creating the ratings matrices
# memory.limit(size = 180000)
real_ratings <- new("realRatingMatrix", data = sparse_ratings)

####### normalizing the ratings matrix
normalized_ratings <- normalize(real_ratings)
#save(normalized_ratings, file = "normalized_ratings.RData")

####### binarising the ratings matrices
#bin_ratings_mat <- binarize(updated_ratings_matrix, minRating = 3)

