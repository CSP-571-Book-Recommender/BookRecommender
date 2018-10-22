library(RCurl)

#Reading in the data set directly from the github url
ratings_url = getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/ratings.csv")
ratings = read.csv(text = ratings_url)

#data Exploration here
library(dplyr)
library(ggplot2)
head(ratings)

#no. of ratings per user 
rat <- ratings %>% 
  group_by(user_id) %>%
  summarise(ratings_per_user = n())
qplot(rat$ratings_per_user, geom = "histogram", xlab = "Ratings per user", fill=I("grey"), 
      col=I("black"))

mean(rat$ratings_per_user)
min(rat$ratings_per_user)
max(rat$ratings_per_user)

# no. of ratings per book
rat <- ratings %>% 
  group_by(book_id) %>%
  summarise(ratings_per_book = n())
qplot(rat$ratings_per_book, geom = "histogram", xlab = "Ratings per book", xlim = c(0, 1000), fill=I("grey"), 
      col=I("black")) 

mean(rat$ratings_per_book)
max(rat$ratings_per_book)
min(rat$ratings_per_book)
