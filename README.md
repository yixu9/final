# Texting mining: take 'Pride and Prejudice' as an example

Dealing with numerical data is something people are used to, this kind of data is standardized and has a clear structure. But for text data, their structure is more complex and less standardized. In the R language, the tidytext package extends the functions for processing text data and realizing visualization. Text analysis of books can quickly extract the content information, thus enable readers use less time to accumulate more knowledge. Text analysis provides readers with the opportunity to read selectively according to their needs, and it is especially helpful for readers who do not have the time to focus on the entire book.    
  
Inspired by the ideas above, we created the package named ‘textm’. This report will take ‘Pride and Prejudice’ as an example to show the use of this package. Here, we first pre-process the novel text, then conduct term frequency analysis and word cloud creation, build word relationship patterns, character appearance summary and sentiment analysis, together with histogram and bigram to visualize the analysis results, finally provide an objective and intuitive understanding of ‘Pride and Prejudice’.
