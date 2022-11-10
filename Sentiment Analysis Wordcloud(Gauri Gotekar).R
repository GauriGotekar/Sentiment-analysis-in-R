##Accessing the required libraries
library(janeaustenr)
library(dplyr)
library(tokenizers)
library(tm)
library(qdap)
library(ggplot2)
library(wordcloud)
library(ggthemes)
library(tidytext)
library(textdata)

##Storing austen_books in original _books 
original_books <- austen_books() %>%
  group_by(book) %>%                       ##Filtering by book
  mutate(linenumber = row_number()) %>%    ##Adding new variable, using "mutate"
  ungroup()
original_books


#Tidying the text for effective analysis - one word/line
tidy_books <- original_books %>%
  unnest_tokens(word, text)                ##Splitting columns into tokens
tidy_books

##Stopwords
View(stop_words)

##Remove stopwords
View("stop_words")
data("stop_words")
tidy_books <- tidy_books %>%
  anti_join(stop_words)                   ##Removing stop_words from tidy books
tidy_books



##The most common words in all the books as a whole.
##Count() from dplyr

pride_prejudice <- tidy_books %>%
  filter (book == "Pride & Prejudice") %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup()
pride_prejudice


library(reshape2)
library(wordcloud)
library(stringr)

##Using filter and word count on sentiment before executing wordcloud
pride_prejudice %>%
  group_by(sentiment) %>%
  count(word) %>%
  acast(word ~ sentiment, value.var = "n", fill=0) %>%
  comparison.cloud(colors= c("green","red"), title.size = 2, scale = c(4,1), random.order = FALSE, max.words = 70)

warning() ## To see the warnings that were being shown