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

library(dplyr)
library(reshape2)
library(ggplot2)

# books <- read.csv("books.csv", stringsAsFactors = F)
# tags <- read.csv("tags.csv", stringsAsFactors = F)
# book_tags <- read.csv("book_tags.csv", stringsAsFactors = F)
# to_read <- read.csv("to_read.csv", stringsAsFactors = F)
# ratings <- read.csv("ratings.csv", stringsAsFactors = F)

books <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/books.csv")
tags <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/tags.csv")
book_tags <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/book_tags.csv")
to_read <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/to_read.csv")
ratings <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/ratings.csv")


length(unique(ratings$user_id))
length(unique(ratings$book_id))

length(unique(book_tags$tag_id))

no_of_ratings_per_user <- as.data.frame(ratings %>% group_by(user_id) %>% summarise(no_of_ratings = length(book_id)))

hist(x = no_of_ratings_per_user$no_of_ratings, breaks = 1000, 
     main = "Number of ratings by each user distribution", 
     xlab = "Number of ratings", ylab = "Number of users")

rating_freq <- as.data.frame(table(ratings$rating))
names(rating_freq) <- c("rating", "freq")

barplot(rating_freq$freq, names.arg = rating_freq$rating, xlab = "rating", ylab = "Frequency",
        main = "ratings distribution", xpd = F, beside = T)

hist(x = books$average_rating, breaks = 1000, 
     main = "Book rating distribution", 
     xlab = "Average ratings", ylab = "Number of books")

publication_year_dist <- as.data.frame(table(books$original_publication_year))
names(publication_year_dist) <- c("publication_year", "number_of_books")

hist(x = books$original_publication_year, breaks = 1000, 
     main = "Publication year distribution", ylim = c(0,575), xlim = c(-2000, 2018),
     xlab = "Publication year", ylab = "Number of books")

hist(x = books$work_ratings_count, breaks = 10000, 
     main = "Ratings distribution",xlim = c(0, 200000),
     xlab = "Number of ratings", ylab = "Number of books")

#vivek
top10_books <- ratings %>% group_by(book_id) %>% summarise(number_of_ratings = length(unique(user_id)))%>% 
               arrange(desc(number_of_ratings)) %>% mutate(rank = c(1:length(number_of_ratings))) %>% filter(rank <= 10) %>%
               select(book_id,number_of_ratings)
top10_books <- merge(top10_books, books[,c("book_id", "original_title", "average_rating")], by = "book_id")

active_readers <- ratings %>% group_by(user_id) %>% summarise(number_of_ratings = length(unique(book_id)))%>% 
               arrange(desc(number_of_ratings)) %>% mutate(rank = c(1:length(number_of_ratings))) %>% filter(rank <= 10) %>%
               select(user_id,number_of_ratings)
