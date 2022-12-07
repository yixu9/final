##' Add book number 
##'
##' We want to add book number from gutenberg and make a histogram for the 10 most frequent words in the book. 
##' @title Add book number
##' @param x book number
##' @return 10 most frequent words
##' @author Yi Xu
##' @import gutenbergr 
##' @import tidytext 
##' @import tidyverse
##' @examples
##' wf_hist(1342)
##' @export
wf_hist <- function(x) {
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
    book_freq %>%
        select(word, n) %>%
        head(n = 10) %>%
        mutate(word = fct_inorder(word)) %>%
        ggplot(aes(x = word, y = n)) + geom_col() + theme(legend.position = "none") + xlab("The 10 most frequent words") + ylab("Frequency")
}
