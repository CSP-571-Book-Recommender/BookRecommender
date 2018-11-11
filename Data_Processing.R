source("ImportData.R")
source("DataCleaning.R")


library(recommenderlab)

updated_ratings_matrix <- dcast(data = eng_book_ratings, user_id~book_id, value.var = "rating")
# write.csv(updated_ratings_matrix, file = "updated_ratings_matrix.csv")

####### creating the ratings matrices
# memory.limit(size = 180000)
updated_ratings_matrix <- as(as.matrix(updated_ratings_matrix), "realRatingMatrix")
# save(updated_ratings_matrix, file = "updated_ratings_matrix.RData")

####### normalising the ratings matrices
norm_updated_ratings_mat <- normalize(updated_ratings_matrix)
# save(norm_updated_ratings_mat, file = "norm_updated_ratings_mat.RData")

####### binarising the ratings matrices
bin_ratings_mat <- binarize(updated_ratings_matrix, minRating = 3)

