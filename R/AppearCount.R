##' Count appearance times of character in each chapter
##'
##' Add book number from gutenberg and find appearance times of character in each chapter
##' @title Counr appearance times
##' @param x book number
##' @param y name of character
##' @return dataframe for appearance times of character in each chapter
##' @author Yahui Zhang
##' @export

AppearCount <- function(x, y) {
    BOOK_0 <- x
    # Split each chapter information
    chapterNum <- (grep("^CHAPTER|^Chapter", BOOK_0$text, value = TRUE))
    chapterEnd <- c(grep("^CHAPTER|^Chapter", BOOK_0$text), length(text) + 1)
    # Store text depends on chapter location
    ChapterSplit <- list()
    for (i in 1:(length(chapterEnd) - 1)) {
        data <- BOOK_0$text[chapterEnd[i]:chapterEnd[i + 1] - 1]
        ChapterSplit[[i]] <- data
        names(ChapterSplit)[i] <- chapterNum[i]
    }
    # Imput character list
    chars <- y
    # Set dataframe to store appearance times for characters in each cahpter
    AppearanceSummary <- data.frame(Chapter = names(ChapterSplit))
    for (i in 1:length(y)) {
        AppearanceSummary[, i + 1] <- sapply(ChapterSplit, function(x) sum(grepl(y[i], x)))
        names(AppearanceSummary)[i + 1] <- y[i]
    }
    AppearanceSummary
}
