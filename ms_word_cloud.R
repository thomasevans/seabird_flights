install.packages("wordcloud")
install.packages("tm")

library("tm")
library("wordcloud")
load("ms_words.RData")

ms_words <- paste(ms_words)

ms_words <- tm_map(ms_words, removePunctuation)


wordcloud(ms_words, max.words = 50, random.order = FALSE)

