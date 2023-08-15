setwd("~/Documents/100daysviz/day4_fedword")
suppressWarnings({

# Load libraries
library("pdftools")
library("dplyr")
library("wordcloud")
library("tidytext")
library("stringr")

# change pdf to text
pdf_path <- "fomcminutes20230201.pdf"
text <- pdftools::pdf_text(pdf_path)

# Convert character vector to a data frame
df <- data.frame(text = text, stringsAsFactors = FALSE)

# Clean and tokenize text
clean_text <- df %>%
  unnest_tokens(word, text) %>% # break sentences into single text per column
  anti_join(stop_words) %>% # remove stop words like "a", "the", "and"
  mutate(word = str_remove_all(word, "\\W|\\d")) %>% # get rid of non-words and digits
  filter(str_length(word) > 0) # remove words with 0 characters (blank space)

# Calculate word freq
word_freq = clean_text %>%
  count(word, sort=TRUE)

# Create word cloud
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
})