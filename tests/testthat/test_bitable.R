##' Add book number 
##'
##' We want to find bigrams of a specific word in this book
##' @title Add book number
##' @param x book number
##' @param y word
##' @return bigrams of the word
##' @author Yi Xu
##' @export 
##' @import gutenbergr tidytext tidyverse
##' @examples 
##' bitable(1342,c('lady'))

bitable <- function(x, y) {
    book_content <- gutenberg_download(x)
    book_content <- book_content %>%
        mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
        filter(chapter > 0) %>%
        drop_na(text) %>%
        select(-gutenberg_id)
    book_cut <- book_content %>%
        unnest_tokens(word, text) %>%
        count(word, sort = TRUE) %>%
        mutate(id = 1:n())
    bigram_freq <- book_content %>%
        select(-chapter) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, into = c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
        drop_na()
    bigram_freq %>%
        filter(word2 == y) %>%
        count(street = str_c(word1, word2, sep = " "), sort = TRUE)
}

