##' Analysis main sentiment of each chapter in the book
##'
##' Add book number from gutenberg and make a histogram for main sentiment of each chapter in the book.
##' @title analysis main sentiment
##' @param x book number
##' @return histogram for main sentiment of each chapter
##' @author Yahui Zhang
##' @import gutenbergr
##' @import tidytext
##' @import tidyr
##' @import dplyr
##' @import ggplot2
##' @import stringr
##' @export
##' @examples
##' Emo(1342)
Emo <- function(x) {
    BOOK_0 <- gutenberg_download(x)
    # Create variable 'linenumber' as the line number where the sentence is located and variable 'chapter' as the chapter to which the
    # sentence belongs
    BOOK_locates <- mutate(BOOK_0, linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^CHAPTER", ignore_case = TRUE))))
    # Split text into single words
    BOOK_single <- unnest_tokens(BOOK_locates, word, text)
    # Remove stop words which are useless for emotion express
    BOOK_emo <- anti_join(BOOK_single, stop_words)
    # Using bing lexicon to analysis sentiment. The rule is binary scale with -1 indicating negative sentiment and 1 indicating
    # positive sentiment. Create variable 'sentiment' to store binary category result
    BOOK_dic <- inner_join(BOOK_emo, get_sentiments("bing"))
    # Count and store the frequencies of positive and negative sentiment separately
    BOOK_emoFre0 <- count(BOOK_dic, Chapter = chapter, sentiment)
    BOOK_emoFre <- spread(BOOK_emoFre0, sentiment, n)
    # Calculate the difference between positive and negative sentiment frequencies, store the difference in variable 'Main_sentiment'
    # to represents main sentiment of each chapter
    BOOK_emoScore <- mutate(BOOK_emoFre, Main_sentiment = positive - negative)
    # Plot main sentiment of each chapter
    ggplot(BOOK_emoScore, aes(Chapter, Main_sentiment, fill = Main_sentiment)) + labs(title = "Sentiment Analysis of Book") + geom_col()
}
