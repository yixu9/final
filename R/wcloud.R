#' We want to make a wordcloud for the 200 most frequent words in the book.
#' @title Making a wordcloud graph.
#' @param x The book file data name.
#' @return A wordcloud.
#' @author Yi Xu
#' @import wordcloud2 usethis
#' @export
wcloud <- function(x) {
    book_content <- x %>%
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
    wordcloud2(book_freq, size = 0.7, minRotation = -pi/5, maxRotation = -pi/5, rotateRatio = 1)
}

