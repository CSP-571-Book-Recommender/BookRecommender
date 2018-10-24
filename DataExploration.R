library(RCurl)

###### Reading from local repository
# books <- read.csv("books.csv", stringsAsFactors = F)
# tags <- read.csv("tags.csv", stringsAsFactors = F)
# book_tags <- read.csv("book_tags.csv", stringsAsFactors = F)
# to_read <- read.csv("to_read.csv", stringsAsFactors = F)
# ratings <- read.csv("ratings.csv", stringsAsFactors = F)


#Reading in the data set directly from the github url
ratings_url = getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/ratings.csv")
ratings = read.csv(text = ratings_url)
rm(ratings_url)

books_url <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/books.csv")
books <- read.csv(text = books_url)
rm(books_url)

tags_url <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/tags.csv")
tags <- read.csv(text = tags_url)
rm(tags_url)

book_tags_url <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/book_tags.csv")
book_tags <- read.csv(text = book_tags_url)
rm(book_tags_url)

to_read_url <- getURL("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/to_read.csv")
to_read <- read.csv(text = to_read_url)
rm(to_read_url)

#data Exploration here
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
head(ratings)

#no. of ratings per user 
rat <- ratings %>% 
  group_by(user_id) %>%
  summarise(ratings_per_user = n())
qplot(rat$ratings_per_user, geom = "histogram", xlab = "Ratings per user", fill=I("grey"), 
      col=I("black"))
## Try using a large number of bins, at least 500. 
## The graph is Number of users vs number of number of ratings. We need to name the axes accordingly.
## The graphs also need titles.
mean(rat$ratings_per_user)
min(rat$ratings_per_user)
max(rat$ratings_per_user)

# no. of ratings per book
rat <- ratings %>% 
  group_by(book_id) %>%
  summarise(ratings_per_book = n())

qplot(rat$ratings_per_book, geom = "histogram", xlab = "Ratings per book", xlim = c(0, 1000), fill=I("grey"), 
      col=I("black")) 
## The same naming issue here - axes and title; Try differnet number of bins.
## try running without the xlim option

mean(rat$ratings_per_book)
max(rat$ratings_per_book)
min(rat$ratings_per_book)


length(unique(ratings$user_id))
length(unique(ratings$book_id))
length(unique(book_tags$tag_id))

###### Frequency distribution of ratings 
rating_freq <- as.data.frame(table(ratings$rating))
names(rating_freq) <- c("rating", "freq")

barplot(rating_freq$freq, names.arg = rating_freq$rating, xlab = "rating", ylab = "Frequency",
        main = "Frequency distribution of ratings", xpd = F, beside = T)

###### Book ratings distribution
hist(x = books$average_rating, breaks = 1000, 
     main = "Book rating distribution", 
     xlab = "Average ratings", ylab = "Number of books")

###### Frequency distribution of publication year 
publication_year_dist <- as.data.frame(table(books$original_publication_year))
names(publication_year_dist) <- c("publication_year", "number_of_books")

hist(x = books$original_publication_year, breaks = 1000, 
     main = "Frequency distribution of publication year", ylim = c(0,575), xlim = c(-2000, 2018),
     xlab = "Publication year", ylab = "Number of books")

###### top 10 most popular books
top10_books <- ratings %>% group_by(book_id) %>% summarise(number_of_ratings = length(unique(user_id)))%>% 
               arrange(desc(number_of_ratings)) %>% mutate(rank = c(1:length(number_of_ratings))) %>% filter(rank <= 10) %>%
               select(book_id,number_of_ratings)
top10_books <- merge(top10_books, books[,c("book_id", "original_title", "average_rating")], by = "book_id")

###### top 10 most active/avid readers
active_readers <- ratings %>% group_by(user_id) %>% summarise(number_of_ratings = length(unique(book_id)))%>% 
               arrange(desc(number_of_ratings)) %>% mutate(rank = c(1:length(number_of_ratings))) %>% filter(rank <= 10) %>%
               select(user_id,number_of_ratings)

###### genre frequency distribution
main_tags_labels = c('romance','fiction','young-adult','fantasy','science-fiction',
                     'children','best','covers','non-fiction', 'history','mystery',
                     'paranormal','love','horror','historical','sci-fi',
                     'historical-fiction','nonfiction','series','contemporary',
                     'thriller','women','novels','suspense','classics' ,'graphic-novels', 
                     'historical-romance', 'christian')

genre_tags <- tags[tags$tag_name %in% main_tags_labels,]
genre_tags$tag_name <- as.character(genre_tags$tag_name)

book_genres <- as.data.frame(merge(book_tags,genre_tags, by = "tag_id", all = F))
book_genres$tag_name <- as.character(book_genres$tag_name)

book_genres[book_genres$tag_name == "nonfiction",c("tag_id", "tag_name")] <- genre_tags[genre_tags$tag_name == "non-fiction",]
book_genres[book_genres$tag_name == "history",c("tag_id", "tag_name")] <- genre_tags[genre_tags$tag_name == "historical",]
book_genres[book_genres$tag_name == "science-fiction",c("tag_id", "tag_name")] <- genre_tags[genre_tags$tag_name == "sci-fi",]
book_genres[book_genres$tag_name == "love",c("tag_id", "tag_name")] <- genre_tags[genre_tags$tag_name == "romance",]

book_genres <- as.data.frame(book_genres %>% group_by(goodreads_book_id,tag_id,tag_name) %>% summarise(count = sum(count)))

book_genre_dist <- as.data.frame(table(book_genres$tag_name))
names(book_genre_dist) <- c("genre", "Freq")
book_genre_dist$genre <- as.character(book_genre_dist$genre)

ggplot(book_genre_dist, aes(x=genre, y=Freq)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Frequency Distribution of Genre") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  geom_text(aes(label=Freq), hjust = -0.25, vjust= 0.5, color="black", size=3.5)+coord_flip()

###### language frequency distribution
lang <- as.data.frame(table(books$language_code))
names(lang)[1] <- "Language"
lang$Language <- as.character(lang$Language)
lang <- lang[lang$Language != "",]
lang <- lang[order(lang$Freq, decreasing = T),]
lang$Language[!lang$Language %in% lang$Language[1:5]] <- "Others"
lang <- as.data.frame(lang %>% group_by(Language) %>% summarise(count = sum(Freq)))

ggplot(lang, aes(x=Language, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Frequency Distribution of Language") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  geom_text(aes(label=count), hjust = -0.25, vjust= 0.5, color="black", size=3.5)+coord_flip()

###### filter the users
ratings_per_user <- ratings %>% group_by(user_id) %>% summarise(number_of_ratings = unique(length(book_id)))
ratings_per_user <- ratings_per_user[ratings_per_user$number_of_ratings >= 80,]

ratings1 <- ratings[ratings$user_id %in% ratings_per_user$user_id,] 
ratings_matrix <- dcast(data = ratings1, user_id~book_id, value.var = "rating")

total_ratings_matrix <- dcast(data = ratings, user_id~book_id, value.var = "rating")

