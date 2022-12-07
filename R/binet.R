##' We want to show the network of bigrams in this book
##' @title A grid chart for bigrams
##' @param x book file data name
##' @return bigrams of the word
##' @author Yi Xu
##' @import ggraph tidygraph tidyr
##' @export
binet <- function(x){
  book_content <- mutate(x,chapter = cumsum( str_detect(text, regex("^chapter ", ignore_case = TRUE))))
  book_content <- subset(book_content,!chapter==0)%>%
    drop_na(text) %>%
    select(-gutenberg_id)
  book_cut <- book_content %>%     #dividing the book into words.
    unnest_tokens(word,text) %>%
    count(word, sort = TRUE) %>%
    mutate(id = 1:n())
  bigram_freq <- book_content %>%  #dividing words into two words.
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>% drop_na()
  counts <- bigram_freq %>% count(word1, word2, sort = TRUE) # Counting the words
  graph <- counts %>% filter(n > 10) %>% as_tbl_graph()
  ggraph(graph, layout = 'kk') +
    geom_edge_link(aes(alpha = n)) +
    geom_node_point(color = "purple") +
    geom_node_text(aes(label = name), vjust = 1)
}
