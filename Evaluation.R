source("RecommenderModel.R")
library(lubridate)

error_df <- data.frame(nn_values = seq(2, 200, 2),
                       rmse = rep(NA,100),
                       mse = rep(NA,100),
                       mae = rep(NA,100))
                       

####### trying out different nn values to get the best UBCF model

for(i in error_df$nn_values)
{
start <- Sys.time()

e <- evaluationScheme(real_ratings[1:2000], method="cross-validation", train=0.9, given=25, k = 10)
reco_trial <- Recommender(getData(e, "train"), "UBCF", param=list(method="cosine",nn=i))

pred <- predict(reco_trial, getData(e, "known"), type="ratings")
error <- calcPredictionAccuracy(pred, getData(e, "unknown"))
error

error_df[error_df$nn_values == i, c("rmse","mse","mae")] <- error
end <- Sys.time()

diff <- difftime(time1 = end, time2 = start, units = "secs")
diff
message("n = ",i," done. Time taken = ", diff, " seconds")
}

min(error_df$rmse)
minimum_error_nn <- error_df$nn_values[which.min(error_df$rmse)] 
minimum_error_nn

######## building the most optimal UBCF Recommender
opt_reco = Recommender(real_ratings, method = "UBCF", param=list(method="cosine",nn=minimum_error_nn))
