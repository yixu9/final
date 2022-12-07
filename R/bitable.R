##'
##' We want to find bigrams of a specific word in this book, if you input a word, 
##' you can get the word associated with it and also the frequency of it.
##' @title bigram frequency table
##' @param x data name
##' @param y word
##' @return bigrams of the word
##' @author Yi Xu
##' @export
##' @import tidytext dplyr 

bitable <- function(x, y) {
    book_content <- x %>%
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
        count(y = str_c(word1, word2, sep = " "), sort = TRUE)
}

