library(RCurl)
ratings_url = getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/ratings.csv")
ratings = read.csv(text = ratings_url)