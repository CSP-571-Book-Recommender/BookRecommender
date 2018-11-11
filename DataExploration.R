source("ImportData.R")

library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(DT)
library(data.table)


#checking for duplicates
anyDuplicated(books)
anyDuplicated(ratings)
anyDuplicated(tags)
anyDuplicated(book_tags)

#Check for missing values
any(is.na(books)) # True
#check which columns of books has missing value 
names(which(colSums(is.na(books))>0))
any(is.na(ratings))
any(is.na(to_read))
any(is.na(tags))
any(is.na(book_tags))

#data Exploration here
head(ratings)

# no. of ratings per user 
rat <- ratings %>% 
  group_by(user_id) %>%
  summarise(ratings_per_user = n())
qplot(rat$ratings_per_user, geom = "histogram", xlab = "ratings per user", ylab = "Frequency",
        main = "Frequency distribution of ratings per user", binwidth = 0.5)

mean(rat$ratings_per_user)
min(rat$ratings_per_user)
max(rat$ratings_per_user)

# no. of ratings per book
rat <- ratings %>% 
  group_by(book_id) %>%
  summarise(ratings_per_book = n())

qplot(rat$ratings_per_book, geom = "histogram", xlab = "ratings per book", ylab = "Frequency",
      main = "Frequency distribution of ratings per book",xlim = c(0, 1000), binwidth = 0.5) 

mean(rat$ratings_per_book)
max(rat$ratings_per_book)
min(rat$ratings_per_book)


length(unique(ratings$user_id))
length(unique(ratings$book_id))
length(unique(book_tags$tag_id))

nrow(books)
###### Frequency distribution of ratings 
rating_freq <- as.data.frame(table(ratings$rating))
names(rating_freq) <- c("rating", "freq")

rating_freq
barplot(rating_freq$freq, names.arg = rating_freq$rating, xlab = "rating", ylab = "Frequency",
        main = "Frequency distribution of ratings", xpd = F, beside = T)

# This is revealing same info as frequency dist of ratings. 
# ###### Book ratings distribution
# hist(x = books$average_rating, breaks = 1000, 
#      main = "Book rating distribution", 
#      xlab = "Average ratings", ylab = "Number of books")

###### Frequency distribution of publication year 
any(is.na(books$original_publication_year))
publication_year_dist <- as.data.frame(table(books$original_publication_year))
names(publication_year_dist) <- c("publication_year", "number_of_books")

hist(x = books$original_publication_year, breaks = 1000, 
     main = "Frequency distribution of publication year", ylim = c(0,575), xlim = c(-2000, 2018),
     xlab = "Publication year", ylab = "Number of books")

###### genre frequency distribution
main_tags_labels = c('romance','fiction','young-adult','fantasy','science-fiction',
                     'children','covers','non-fiction', 'history','mystery',
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

bg <- book_genre_dist %>%
  group_by(genre) %>%
  arrange(desc(Freq))

#ggplot(bg, aes(reorder(genre, Freq), Freq)) + 
ggplot(bg, aes(reorder(genre, Freq), Freq))+
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Frequency Distribution of Genre") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  geom_text(aes(label=Freq), hjust = -0.25, vjust= 0.5, color="black", size=3.5)+ coord_flip()

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
  geom_text(aes(label=count), hjust = -0.25, vjust= 0.5, color="black", size=3.5)

# top 5 rated books 
top5_books <- books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(desc(average_rating)) %>% 
  top_n(5, wt = average_rating) %>% 
  select(image, title, average_rating) 
  datatable(top5_books, class = "display", escape = FALSE, 
            caption = "Top 5 Highest Rated Books",
            options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

# top 5 most popular books 
top5_popular <- books %>%
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(desc(ratings_count)) %>%
  top_n(5, wt = ratings_count) %>%
  select(image, title, ratings_count)
datatable(top5_popular, class = "display", escape = FALSE, 
          caption = "Top 5 Most Popular Books",
          options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


head(to_read)
popular_to_read <- to_read %>%
  group_by(book_id) %>%
  summarize(to_read_count = n())

head(popular_to_read)
#no. of books classified as to_read 
nrow(popular_to_read)

# merging with books dataset to get the book_titles 
popular_to_read <- merge(popular_to_read, books[,c("book_id", "title", "average_rating", "ratings_count","small_image_url")], by = "book_id")


# top5 to-read books 

top5_toRead <- popular_to_read %>%
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(desc(to_read_count)) %>%
  top_n(5, wt = to_read_count) %>%
  select(image, title, to_read_count)

datatable(top5_toRead, class = "display", escape = FALSE, 
          caption = "Top 5 Books marked as To-Read",
          options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

#book with the most 5 star rating 
top_5star <- books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(desc(ratings_5)) %>%
  top_n(5, wt = ratings_5) %>%
  select(image, title, ratings_5)
datatable(top_5star, class = "display", escape = FALSE, 
          caption = "Top 5 Books that have the most 5 star ratings",
          options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

#Book with the most 1 star ratings 
top_1star <- books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(desc(ratings_1)) %>%
  top_n(5, wt = ratings_1) %>%
  select(image, title, ratings_1)
datatable(top_1star, class = "display", escape = FALSE, 
          caption = "Top 5 Books that have the most 1 star ratings",
          options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

# does popularity influence a books average rating ?
#cor(books$average_rating, books$ratings_count, method = "pearson")

ggscatter(books, x = "ratings_count", y = "average_rating",
          add = "reg.line",add.params = list(color = "blue"), conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", color = "gray",
          title = "Does Popularity influence a books average rating?",
          xlab = "ratings count", ylab = "average rating", xlim = c(0,200000))

#Do authors who have written more books get a higher rating? 
author_ratings <- books %>%
  mutate(authors = factor(authors) ) %>%
  group_by(authors) %>%
  summarise(no_of_books = n(), average_rating = sum(average_rating)/no_of_books) 

#cor(author_ratings$average_rating, author_ratings$no_of_books, method = "pearson")
ggscatter(author_ratings, x = "no_of_books", y = "average_rating",
          add = "reg.line",add.params = list(color = "blue"), conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", color = "gray",
          title = "Does no. of books written by author influence a books average rating?",
          xlab = "Number of Books Written", ylab = "average rating")


###### filter the users
ratings_per_user <- ratings %>% group_by(user_id) %>% summarise(number_of_ratings = unique(length(book_id)))
ratings_per_user <- ratings_per_user[ratings_per_user$number_of_ratings >= 80,]

ratings1 <- ratings[ratings$user_id %in% ratings_per_user$user_id,] 
ratings_matrix <- dcast(data = ratings1, user_id~book_id, value.var = "rating")

total_ratings_matrix <- dcast(data = ratings, user_id~book_id, value.var = "rating")

#head(author_ratings)
# 
# ###### top 10 most popular books
# top10_books <- ratings %>% group_by(book_id) %>% summarise(number_of_ratings = length(unique(user_id)))%>% 
#                arrange(desc(number_of_ratings)) %>% mutate(rank = c(1:length(number_of_ratings))) %>% filter(rank <= 10) %>%
#                select(book_id,number_of_ratings)
# top10_books <- merge(top10_books, books[,c("book_id", "original_title", "average_rating")], by = "book_id")
# 
# ###### top 10 most active/avid readers
# active_readers <- ratings %>% group_by(user_id) %>% summarise(number_of_ratings = length(unique(book_id)))%>% 
#                arrange(desc(number_of_ratings)) %>% mutate(rank = c(1:length(number_of_ratings))) %>% filter(rank <= 10) %>%
#                select(user_id,number_of_ratings)

library(recommenderlab)

####### creating the ratings matrices
ratings_matrix <- as(as.matrix(ratings_matrix), "realRatingMatrix")
# save(ratings_matrix, file = "ratings_matrix.RData")

total_ratings_matrix <- as(as.matrix(total_ratings_matrix), "realRatingMatrix")
# save(total_ratings_matrix, file = "total_ratings_matrix.RData")

####### normalising the ratings matrices
norm_ratings_mat <- normalize(ratings_matrix)
norm_total_ratings_mat <- normalize(total_ratings_matrix) 

####### binarising the ratings matrices
bin_ratings_mat <- binarize(ratings_matrix, minRating = 3)
bin_total_ratings_mat <- binarize(total_ratings_matrix, minRating = 3) 

