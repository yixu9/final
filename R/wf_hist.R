##' We want to make a histogram for the 10 most frequent words in the book.
##' @title Histogram for frequency
##' @param x book file name
##' @return 10 most frequent words
##' @author Yi Xu
##' @import ggplot2
##' @import tidytext
##' @importFrom utils head
##' @export
wf_hist <- function(x) {
    book_content <- mutate(x,chapter = cumsum( str_detect(text, regex("^chapter ", ignore_case = TRUE))))
    book_content <- subset(book_content,!chapter==0) %>%
      drop_na(text) %>%
      select(-gutenberg_id)
    book_cut <- book_content %>%      #dividing the book into words.
      unnest_tokens(word,text) %>%
      count(word, sort = TRUE) %>%
      mutate(id = 1:n())
    book_freq <- book_cut %>%
      bind_tf_idf(id, word, n) %>%
      arrange(desc(tf_idf))
    book_freq %>% select(word, n) %>% head(n = 10) %>%
      ggplot(aes(x=word,y=n)) +
      geom_col(fill = "lightblue") +
      labs(title = c("The 10 most frequent words in the book"),
           ylab = c("Frequency"),
           xlab = c("Word"))
}
