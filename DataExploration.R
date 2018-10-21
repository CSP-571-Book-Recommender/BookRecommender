library(RCurl)

#Reading in the data set directly from the github url
ratings_url = getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/ratings.csv")
ratings = read.csv(text = ratings_url)