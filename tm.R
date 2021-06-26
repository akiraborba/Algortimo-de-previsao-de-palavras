library(quanteda)
library(quanteda.textstats)
library(data.table)
#library(parallel)
library(stringi)

i.Blog.US <- file("~/RStudio/Testes/Coursera/final/en_US/en_US.blogs.txt")
Blog.Us <- readLines(i.Blog.US, n = -1)
close(i.Blog.US)

i.News.Us <- file("~/RStudio/Testes/Coursera/final/en_US/en_US.news.txt")
News.Us <- readLines(i.News.Us, n = -1)
close(i.News.Us)

i.Twitter.Us <- file("~/RStudio/Testes/Coursera/final/en_US/en_US.twitter.txt")
Twitter.Us <- readLines(i.Twitter.Us, n = -1)
close(i.Twitter.Us)

i.badwords <- file("C:/Users/Arthur Luna/Downloads/badwords")
badwords <- readLines(i.badwords, n = -1)
close(i.badwords)
rm(i.badwords)

split.nw <- split(News.Us, ceiling(seq_along(1:length(News.Us))/(length(News.Us)/20)))
rm(News.Us, i.News.Us)
split.blog <- split(Blog.Us, ceiling(seq_along(1:length(Blog.Us))/(length(Blog.Us)/20)))
rm(Blog.Us, i.Blog.US)
split.tw <- split(Twitter.Us, ceiling(seq_along(1:length(Twitter.Us))/(length(Twitter.Us)/20)))
rm(Twitter.Us, i.Twitter.Us)
gc()



set.seed(34567)
split.nw <- lapply(split.nw, function(x) x[as.logical(rbinom(length(x), 1, 0.5))])
split.blog <- lapply(split.blog, function(x) x[as.logical(rbinom(length(x), 1, 0.5))])
split.tw <- lapply(split.tw, function(x) x[as.logical(rbinom(length(x), 1, 0.5))])


News.corpus <- lapply(split.nw, function (x) gsub(x, pattern = "[[:punct:]]", replacement = " "))
News.corpus <- lapply(News.corpus, function (x) gsub(x, pattern = "[[:digit:]]", replacement = " "))
News.corpus <- lapply(News.corpus, function (x) gsub(x, pattern = "[^\u0001-\u007F]+", replacement = ""))
News.corpus <- lapply(News.corpus, tolower)
News.corpus <- lapply(News.corpus, function (x) stri_replace_all_fixed(x, badwords, replacement = "omglobness",vectorize_all = F ))


Twitter.corpus <- lapply(split.tw, function (x) gsub(x, pattern = "[[:punct:]]", replacement = " "))
Twitter.corpus <- lapply(Twitter.corpus, function (x) gsub(x, pattern = "[[:digit:]]", replacement = " "))
Twitter.corpus <- lapply(Twitter.corpus, function (x) gsub(x, pattern = "[^\u0001-\u007F]+", replacement = ""))
Twitter.corpus <- lapply(Twitter.corpus, tolower)
Twitter.corpus <- lapply(Twitter.corpus, function (x) stri_replace_all_fixed(x, badwords, replacement = "omglobness",vectorize_all = F ))

Blog.corpus <- lapply(split.blog, function (x) gsub(x, pattern = "[[:punct:]]", replacement = " "))
Blog.corpus <- lapply(Blog.corpus, function (x) gsub(x, pattern = "[[:digit:]]", replacement = " "))
Blog.corpus <- lapply(Blog.corpus, function (x) gsub(x, pattern = "[^\u0001-\u007F]+", replacement = ""))
Blog.corpus <- lapply(Blog.corpus, tolower)
Blog.corpus <- lapply(Blog.corpus, function (x) stri_replace_all_fixed(x, badwords, replacement = "omglobness",vectorize_all = F ))

rm(split.nw, split.blog, split.tw)
gc()

a <- c()
for(i in 1:20){
        a[i] <- length(Blog.corpus[[i]]) + length(Twitter.corpus[[i]]) + length(News.corpus[[i]])}

all.corpus <- corpus(unlist(c(News.corpus, Blog.corpus, Twitter.corpus)), docnames = 1:sum(a))
tokens <- tokens(all.corpus, what = "word")
rm(Blog.corpus, News.corpus, Twitter.corpus)
gc()

ngram.matrix.5 <- tokens_ngrams(tokens, n = 5)
ngram.matrix.5 <- dfm(ngram.matrix.5)
ngram.matrix.5 <- dfm_trim(ngram.matrix.5, min_termfreq = 2)
ngram.matrix.5 <- as.data.table(textstat_frequency(ngram.matrix.5))[,1:2]
gc()

ngram.matrix.4 <- tokens_ngrams(tokens, n = 4)
ngram.matrix.4 <- dfm(ngram.matrix.4)
ngram.matrix.4 <- dfm_trim(ngram.matrix.4, min_termfreq = 2)
ngram.matrix.4 <- as.data.table(textstat_frequency(ngram.matrix.4))[,1:2]
gc()

ngram.matrix.3 <- tokens_ngrams(tokens, n = 3)
ngram.matrix.3 <- dfm(ngram.matrix.3)
ngram.matrix.3 <- dfm_trim(ngram.matrix.3, min_termfreq = 2)
ngram.matrix.3 <- as.data.table(textstat_frequency(ngram.matrix.3))[,1:2]
gc()

ngram.matrix.2 <- tokens_ngrams(tokens, n = 2)
ngram.matrix.2 <- dfm(ngram.matrix.2)
ngram.matrix.2 <- dfm_trim(ngram.matrix.2, min_termfreq = 2)
ngram.matrix.2 <- as.data.table(textstat_frequency(ngram.matrix.2))[,1:2]

rm(tokens)
gc()


search.term <- function(term) {
        search.4 <- NA
        search.3 <- NA
        search.2 <- NA
        results.4 <- data.frame()
        results.3 <- data.frame()
        results.2 <- data.frame()
        search.term <- tolower(term)
        search.term <- gsub("[[:punct:]]", " ", search.term)
        search.term <- strsplit(search.term, " ")
        term.length <- length(search.term[[1]])
        
        if (term.length >= 4){
                search.4 <- search.term[[1]][(term.length - 3):term.length]
                search.4 <- paste(search.4, collapse = "_")}
        
        if (term.length >= 3){
                search.3 <- search.term[[1]][(term.length - 2):term.length]
                search.3 <- paste(search.3, collapse = "_")}
        
        if (term.length >= 2){
                search.2 <- search.term[[1]][(term.length - 1):term.length]
                search.2 <- paste(search.2, collapse = "_")}
        
        if (term.length >= 1){
                search.1 <- search.term[[1]][term.length]
                search.1 <- paste(search.1, collapse = "_")}
        strings <- list(search.1, search.2, search.3, search.4)
        
        if(!is.na(search.4)){
                results.4 <- ngram.matrix.5[c(grep(paste("^", strings[4], "_", sep=""), ngram.matrix.5$feature)),]}
        if(nrow(results.4) != 0){
                print("busquei 4 termos")
                return(results.4)
                }
        if(nrow(results.4) == 0 & !is.na(search.3)) {
                results.3 <- ngram.matrix.4[c(grep(paste("^", strings[3], "_", sep=""), ngram.matrix.4$feature)),]}
        if(nrow(results.3) != 0) {
                print("busquei 3 termos")
                return(results.3)
                }
        if(nrow(results.4) == 0 & nrow(results.3) == 0 & !is.na(search.2)){
                results.2 <- ngram.matrix.3[c(grep(paste("^", strings[2], "_", sep=""), ngram.matrix.3$feature)),]}
        if(nrow(results.2) != 0) {
                print("busquei 2 termos")
                return(results.2)
                }
        if(nrow(results.4) == 0 & nrow(results.3) == 0 & nrow(results.2) == 0){
                results.1 <- ngram.matrix.2[c(grep(paste("^", strings[1], "_", sep=""), ngram.matrix.2$feature)),]
                print("busquei 1 termo")
                return(results.1)
                }}
        
write.csv(x = ngram.matrix.2, file = "ngram_2_june2021.csv")
write.csv(x = ngram.matrix.3, file = "ngram_3_june2021.csv")
write.csv(x = ngram.matrix.4, file = "ngram_4_june2021.csv")
write.csv(x = ngram.matrix.5, file = "ngram_5_june2021.csv")
