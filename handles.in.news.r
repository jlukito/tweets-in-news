## The function findhandles()  will search a text file for references to Twitter handles.
## It will return a string of handles from your text file.
## For any questions, please email me directly.
#
## AUTHOR --- JOSEPHINE LUKITO, jlukito@wisc.edu
library(dplyr)
library(tidyr)
library(stringr)
library(tokenizers)

# This function is a tokenizer that does not remove punctuations
tokenize_words_mod <- function(x) {
  return(unlist(tokenize_words(x, lowercase = FALSE, strip_punct = FALSE)))}

# The earlier function separates the @ symbol as a separate token.
# This function identifies tokens following the "@" symbol, which are presumably handle names
findhandles <- function(text) {
  holder <- as.data.frame(lapply(as.character(placeholder), tokenize_words_mod)) %>%
    unnest() %>%
    mutate (id = row_number())
  names(holder)[1] <- "handle"
  wordid <- subset(holder, holder[,1] == "@", select = id) %>% mutate(id = id + 1) %>% setDT()
  setDT(holder)
  holder[wordid, on = .(id), ':=' (handleyes = TRUE)]
  url.list <- subset(holder, handleyes == TRUE) %>% flatten()
  return(url.list[,1])
}