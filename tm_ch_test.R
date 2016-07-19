nThreads <- 4

work_dir <- "/home/rstudio2/R/imgo"

sample.clean <- readLines(wordcut_res_file)

library(tm)
#vec_src <- lapply(sample.clean, function(x) x <- paste(x,collapse=" "))
corpus <- Corpus(VectorSource(vec_src))
library(SnowballC)
corpus <- tm_map(corpus, stripWhitespace)   # *Stripping whitespace
corpus <- tm_map(corpus, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
library(tmcn)
#corpus <- tm_map(corpus, removeWords, stopwordsCN())
corpus <- tm_map(corpus, PlainTextDocument)
