library(readr)
library(dplyr)
library(tm)
library(ggplot2)
library(stringr)
library(data.table)
library(tidytext)


blogs<-readLines('en_US.blogs.txt', warn=TRUE)
news<-readLines('en_US.news.txt', warn=TRUE)
twitter<-readLines('en_US.twitter.txt', warn=TRUE)

blogs_sam<-blogs[sample(1:length(blogs),50000)]
news_sam<-news[sample(1:length(news),50000)]
twit_sam<-twitter[sample(1:length(twitter),50000)]

sam_data<- c(blogs_sam, news_sam, twit_sam)
file_name <- "data.txt"
writeLines(sam_data, file_name)

proc_sam_data <- sam_data %>% tolower() %>% 
    tm::removePunctuation(ucp=TRUE) %>% tm::stripWhitespace()

##Download profanity list from 
#https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
#and create chr vector
profanity_list<-readLines('en_profanity.txt', warn=TRUE)

cleandata<- removeWords(proc_sam_data, profanity_list)

library(quanteda)
library(quanteda.textstats)
#tokens <- tokens(cleandata, what="word")

tokens <- tokens(cleandata, what = "word", remove_numbers = T, remove_punct = T,
                 remove_symbols = T, remove_separators = T, remove_twitter = T, remove_hyphens = T,
                 remove_url = T)

makeNgrams <- function(inputTokens, n, outName) {
    ## inputTokens: tokenized object
    ## number of grams
    ## output file name
    tokWordNg <- tokens_ngrams(inputTokens, n = n, concatenator = ' ')
    dfmWordNg <- dfm(tokWordNg, tolower = T)
    nGram <- textstat_frequency(dfmWordNg)
    write.csv(nGram, file = paste0(outName, '.csv'), row.names = F)
}

makeNgrams(tokens, 1L, 'uniGram')
makeNgrams(tokens, 2L, 'biGram')
makeNgrams(tokens, 3L, 'triGram')
makeNgrams(tokens, 4L, 'quadGram')
makeNgrams(tokens, 5L, 'quinGram')
makeNgrams(tokens, 6L, 'sixGram')
makeNgrams(tokens, 7L, 'septGram')


library(data.table)
generatePred <- function(inputFile, thresh = 1L) {
    ## This function makes the prediction look up table
    ## inputFile: the ngram csv file generated from quanteda
    ## thresh: threshold to remove low frequency words (default is 1)
    nGram <- fread(inputFile, select = c('feature', 'frequency'))
    nGram <- nGram[nGram$frequency > thresh]
    
    nGram <- nGram[, query := strsplit(feature, " [^ ]+$")][]
    nGram <- nGram[, predict := sub('.* (.*)$','\\1', feature)][]
    
    fwrite(nGram, paste0(sub('.csv', '', inputFile), 'Pred.csv'))
    
}

generatePred('uniGram.csv')
generatePred('biGram.csv')
generatePred('triGram.csv')
generatePred('quadGram.csv')
generatePred('quinGram.csv')
generatePred('sixGram.csv')
generatePred('septGram.csv')


unipred_df <- read.csv("uniGramPred.csv")
bipred_df <- read.csv("biGramPred.csv")
tripred_df <- read.csv("triGramPred.csv")
quadpred_df <- read.csv("quadGramPred.csv")
quinpred_df <- read.csv("quinGramPred.csv")
sixpred_df <- read.csv("sixGramPred.csv")
septpred_df <- read.csv("septGramPred.csv")

# Merge by common columns (e.g., 'ID' and 'Date')
ngram_preds <- bind_rows(unipred_df, bipred_df, tripred_df, quadpred_df, 
                         quinpred_df, sixpred_df, septpred_df)

write.csv(ngram_preds, file = 'ngram_preds.csv', row.names = F)

nGram <- fread('ngram_preds.csv', select = c('query', 'predict', 'frequency'))
nGram <- nGram[order(-frequency)]

nGram2 <- nGram %>% filter(query != predict)

fwrite(nGram2, file = 'predictionTable.csv')
