library(shiny)
library(stringi)

##### Loading Data #####

badwords.file <- file("badwords.txt")
badwords <- readLines(badwords.file, -1)
close(badwords.file)
rm(badwords.file)

ngram.matrix.2 <- read.csv("ngram_2_june2021b.csv")
ngram.matrix.3 <- read.csv("ngram_3_june2021b.csv")
ngram.matrix.4 <- read.csv("ngram_4_june2021b.csv")
ngram.matrix.5 <- read.csv("ngram_5_june2021b.csv")

#### Creating the functions ####

from.raw.to.sentence <- function(x){
    sentence <- strsplit(x, "_")
    sentence <- paste(sentence[[1]], collapse =  " ")
    sentence <- gsub("^i ", "I ", sentence)
    sentence <- gsub(" i ", " I ", sentence)
    sentence <- gsub(" i$", " I", sentence)
    sentence}

clean.term.transformation <- function(x) {
    x <- gsub("_m_", "_am_", x)
    x <- gsub("_s_", "_is_", x)
    x <- gsub("_re_", "_are_", x)
    x <- gsub("_ve_", "_have_", x)
    x <- gsub("_t_", "_not_", x)
    x <- gsub("_ll_", "_will_", x)
    x <- gsub("_d_", "_would_", x)
    x <- gsub("_u_", "_you_", x)
    x <- gsub("_let_s_", "_let's_", x)
    x <- gsub("_don_", "_do_", x)
    x <- gsub("_didn_", "_did_", x)
    x <- gsub("_doesn_", "_does_", x)
    x <- gsub("_haven_", "_have_", x)
    x <- gsub("_hasn_", "_has_", x)
    x <- gsub("_aren_", "_are_", x)
    x <- gsub("_isn_", "_is_", x)
    x <- gsub("_wasn_", "_was_", x)
    x <- gsub("_weren_", "_were_", x)
    x <- gsub("_wouldn_", "_would_", x)
    x <- gsub("_couldn_", "_could_", x)
    x <- gsub("_won_not_", "_will_not_", x)
    x <- gsub("_m$", "_am", x)
    x <- gsub("_s$", "_is", x)
    x <- gsub("_re$", "_are", x)
    x <- gsub("_ve$", "_have", x)
    x <- gsub("_t$", "_not", x)
    x <- gsub("_ll$", "_will", x)
    x <- gsub("_d$", "_would", x)
    x <- gsub("_u$", "_you", x)
    x <- gsub("_let_s$", "_let's", x)
    x <- gsub("_don$", "_do", x)
    x <- gsub("_didn$", "_did", x)
    x <- gsub("_doesn$", "_does", x)
    x <- gsub("_haven$", "_have", x)
    x <- gsub("_hasn$", "_has", x)
    x <- gsub("_aren$", "_are", x)
    x <- gsub("_isn$", "_is", x)
    x <- gsub("_wasn$", "_was", x)
    x <- gsub("_weren$", "_were", x)
    x <- gsub("_wouldn$", "_would", x)
    x <- gsub("_couldn$", "_could", x)
    x <- gsub("_won_not$", "_will_not", x)
    x <- gsub("^m_", "am_", x)
    x <- gsub("^s_", "is_", x)
    x <- gsub("^re_", "are_", x)
    x <- gsub("^ve_", "have_", x)
    x <- gsub("^t_", "not_", x)
    x <- gsub("^ll_", "will_", x)
    x <- gsub("^d_", "would_", x)
    x <- gsub("^u_", "you_", x)
    x <- gsub("^let_s_", "let's_", x)
    x <- gsub("^don_", "do_", x)
    x <- gsub("^didn_", "did_", x)
    x <- gsub("^doesn_", "does_", x)
    x <- gsub("^haven_", "have_", x)
    x <- gsub("^hasn_", "has_", x)
    x <- gsub("^aren_", "are_", x)
    x <- gsub("^isn_", "is_", x)
    x <- gsub("^wasn_", "was_", x)
    x <- gsub("^weren_", "were_", x)
    x <- gsub("^wouldn_", "would_", x)
    x <- gsub("^couldn_", "could_", x)
    x <- gsub("^won_not_", "will_not_", x)
    return(x)}

search.term <- function(term) {
    
    turn <- 1
    
    search.4 <- 0
    search.2 <- 0
    search.3 <- 0
    search.1 <- 0
    
    search.term <- tolower(term)
    search.term <- gsub("[[:punct:]]", " ", search.term)
    search.term <- stri_replace_all_fixed(search.term, badwords, replacement = "omgloobness",vectorize_all = F )
    search.term <- strsplit(search.term, " ")
    term.length <- length(search.term[[1]])
    
    if (term.length >= 4){
        search.4 <- search.term[[1]][(term.length - 3):term.length]
        search.4 <- paste(search.4, collapse = "_")
        search.4 <- clean.term.transformation(search.4)}
    
    if (term.length >= 3){
        search.3 <- search.term[[1]][(term.length - 2):term.length]
        search.3 <- paste(search.3, collapse = "_")
        search.3 <- clean.term.transformation(search.3)}
    
    if (term.length >= 2){
        search.2 <- search.term[[1]][(term.length - 1):term.length]
        search.2 <- paste(search.2, collapse = "_")
        search.2 <- clean.term.transformation(search.2)}
    
    if (term.length >= 1){
        search.1 <- search.term[[1]][term.length]
        search.1 <- paste(search.1, collapse = "_")
        search.1 <- clean.term.transformation(search.1)}
    
    strings <- list(search.1, search.2, search.3, search.4)
    
    if (turn == 1){
        if(search.4 != 0){
            results.4 <- ngram.matrix.5[c(grep(paste("^", strings[4], "_", sep=""), ngram.matrix.5$feature)),]
            if(nrow(results.4) != 0){return(results.4)}
            else{turn <- turn + 1}}
        else{turn <- turn + 1}}
    
    if (turn == 2){
        if(search.3 != 0){
            results.3 <- ngram.matrix.4[c(grep(paste("^", strings[3], "_", sep=""), ngram.matrix.4$feature)),]
            if(nrow(results.3) != 0){return(results.3)}
            else {turn <- turn + 1}}
        else{turn <- turn + 1}}
    
    if (turn == 3){
        if(search.2 != 0){
            results.2 <- ngram.matrix.3[c(grep(paste("^", strings[2], "_", sep=""), ngram.matrix.3$feature)),]
            if(nrow(results.2) != 0){return(results.2)}
            else {turn <- turn + 1}}
        else{turn <- turn + 1}}
    
    if (turn == 4){
        if(search.1 != 0){
            results.1 <- ngram.matrix.2[c(grep(paste("^", strings[1], "_", sep=""), ngram.matrix.2$feature)),]
            if(nrow(results.1) != 0){return(results.1)}
            else {print("Oh my gloobness, this is embarrasing. I have found nothing. May we try again?")}}
        else{print("Oh my gloobness, this is embarrasing. I have found nothing. May we try again?")}}}

    
    

shinyServer(function(input, output) {

    single.word <- reactive({
        if(isTruthy(input$search.request)){
        single.word <- as.vector(search.term(input$search.request)[1,2])
        single.word <- strsplit(single.word, "_")
        length.single.word <- length(single.word[[1]])
        single.word <- single.word[[1]][length.single.word]
        if(single.word == "i"){single.word <- "I"}
        single.word}})
    
    result.df <- reactive({
        if(isTruthy(input$search.request)){
            df <- head(search.term(input$search.request))
            df <- df[,2:3]
            df$feature <- lapply(df$feature, from.raw.to.sentence)
            df$frequency <- lapply(df$frequency, function(x) x/sum(df$frequency))
            names(df) <- c("Ngrams", "Possibility")
            df}})
    
    output$search.result <- renderText({
        if(isTruthy(input$search.request)){single.word()}
        else{"Hey, write something!"}})
    
    output$search.original <- renderText({
        if(isTruthy(input$search.request)){input$search.request}
        else{"You have not written anything yet!"}})
    
    output$search.composed <- renderText({
        if(isTruthy(input$search.request)){paste(input$search.request, single.word(), " ")}
        else{"You have not written anything yet!"}})
    
    output$word.df <- renderTable({
        result.df()
    })
    
})