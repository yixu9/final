##' Add book number 
##'
##' We want to add book number from gutenberg and make a wordcloud for the 200 most frequent words in the book. 
##' @title Add book number
##' @param x book number
##' @return a wordcloud 
##' @author Yi Xu
##' @import tidytext
##' @import gutenbergr 
##' @import tidyverse 
##' @import wordcloud2
##' @export
##' @examples
##' wcloud(1342)

wcloud <- function(x) {
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
    book_freq <- book_cut %>%
        bind_tf_idf(id, word, n) %>%
        arrange(desc(tf_idf))
    book_freq <- as.data.frame(anti_join(book_freq, stop_words) %>%
        top_n(200, n))
    wordcloud2(book_freq, size = 0.7, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
}

