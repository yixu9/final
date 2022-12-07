##' Add book number 
##' We want to show the network of bigrams in this book
##' @title Add book number
##' @param x book number
##' @return bigrams of the word
##' @author Yi Xu
##' @export 
##' @import gutenbergr tidytext tidyverse ggraph usethis
##' @examples
##' binet(1342)
binet <- function(x){
  book_content <- gutenberg_download(x)
  book_content <- mutate(book_content,chapter = cumsum( str_detect(text, regex("^chapter ", ignore_case = TRUE)))) 
  book_content <- subset(book_content,!chapter==0)
  book_content %>% drop_na(text) %>% select(-gutenberg_id)
  book_cut <- book_content %>% #dividing the book into words.
    unnest_tokens(word,text) %>%
    count(word, sort = TRUE) %>%
    mutate(id = 1:n()) 
  bigram_freq <- book_content %>%#dividing words into two words.
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>% drop_na()
  bigram_counts <- bigram_freq %>% count(word1, word2, sort = TRUE) # Counting the words
  bigram_graph <- bigram_counts %>% filter(n > 10) %>% 
    as_tbl_graph()
  arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  ggraph(bigram_graph, layout = "fr") + 
    geom_edge_link(aes(alpha = n), show.legend = F, 
                   arrow = arrow, end_cap = circle(0.07, "inches")) + 
    geom_node_point(color = "purple", size = 6) + 
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
}
