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
