install.packages("wordcloud")
install.packages("tm")

library("wordcloud")
load("ms_words.RData")

# crude <- tm_map(ms_words, removePunctuation)
wordcloud(ms_words, max.words = 50, random.order = FALSE)

