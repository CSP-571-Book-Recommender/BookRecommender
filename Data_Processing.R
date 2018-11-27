source("ImportData.R")
source("DataCleaning.R")

library(reshape2)
library(recommenderlab)

updated_ratings_matrix <- dcast(data = eng_book_ratings, user_id~book_id, value.var = "rating")
# write.csv(updated_ratings_matrix, file = "updated_ratings_matrix.csv")

###### Removing the user ids column and renaming the columns
row.names(updated_ratings_matrix) <- paste0("u_",updated_ratings_matrix$user_id)
updated_ratings_matrix$user_id <- NULL
names(updated_ratings_matrix) <- paste0("b_", names(updated_ratings_matrix))

# memory.limit(size = 180000)
gc()
updated_ratings_matrix <- as.matrix(updated_ratings_matrix)
real_ratings <- as(updated_ratings_matrix, "realRatingMatrix")
# save(real_ratings, file = "real_ratings.RData")

####### normalizing the ratings matrix with z-score
normalized_ratings <- normalize(real_ratings, method = "Z-score")
#save(normalized_ratings, file = "normalized_ratings.RData")

##############################################################################
# #Converting it into a sparse matrix to save space 
# updated_ratings_matrix[is.na(updated_ratings_matrix)]<- 0
# updated_ratings_matrix = as.matrix(updated_ratings_matrix[,-1])
# sparse_ratings <- as(updated_ratings_matrix, "sparseMatrix")
# rm(updated_ratings_matrix)
# 
# 
# ####### creating the ratings matrices
# real_ratings <- new("realRatingMatrix", data = sparse_ratings)
# 
# ####### normalizing the ratings matrix
# normalized_ratings <- normalize(real_ratings)
# #save(normalized_ratings, file = "normalized_ratings.RData")
# 
# ####### binarising the ratings matrices
# #bin_ratings_mat <- binarize(updated_ratings_matrix, minRating = 3)
