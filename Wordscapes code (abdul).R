library(magrittr)
library(dplyr)
library(stringr)
library(tidytext)
library(textreadr)
library(janeaustenr)
library(textdata)
library(rvest)
library(twitteR)
library(tm)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(wordcloud)
library(tidyr)
library(scales)
library(pdftools)
install.packages("pdftools")

#Importing all .txt files from one directory # a txt works like a csv file with multiple rows
setwd("C:/Users/abdul/OneDrive/Documents/Class R/Text analytic/txt files")

nm <- list.files(path="C:/Users/abdul/OneDrive/Documents/Class R/Text analytic/txt files")

#using read document to import the data:
my_data <- read_document(file=nm[1]) #This comes out as a vector
my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector

my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))
View(my_txt_text)

str(my_txt_text)

wordscape_df <- data_frame(line=2, text=my_txt_text)


wordscape_tokens <- wordscape_df %>%
  unnest_tokens(word,text) %>%
  anti_join(my_stop_words)%>% #using my own custom stop words (merged with regular stop_words)
  #count(word,sort = TRUE)

View(wordscape_tokens)


wordscape_df<- removeNumbers(wordscape_df)


custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  '2020', "CUSTOM",
  'february',"CUSTOM",
  'january',"CUSTOM",
  'december',"CUSTOM",
  '2019',"CUSTOM",
  'review',"CUSTOM"
)

# Binding the custom stop words to stop_words
my_stop_words <- stop_words %>% 
  bind_rows(custom_stop_words)


###############################
########### CORPUS ###########
#############################

wordscape_corpus <- Corpus(VectorSource(wordscape_df$text))

inspect(wordscape_corpus_clean[1:5])

wordscape_corpus_clean <- tm_map(wordscape_corpus,removeWords,stopwords("english"))

wordscape_corpus_clean <- tm_map(wordscape_corpus_clean,tolower)

wordscape_corpus_clean <- tm_map(wordscape_corpus_clean,removePunctuation)

wordscape_corpus_clean <- tm_map(wordscape_corpus_clean,removeNumbers)


other_wordscape_df <- data.frame(text = sapply(wordscape_corpus_clean, as.character), stringsAsFactors = FALSE)


wordscape_tokens <- other_wordscape_df %>%
  unnest_tokens(word,text) %>%
  anti_join(my_stop_words)%>% #using my own custom stop words (merged with regular stop_words)
  #count(word,sort = TRUE)

View(wordscape_tokens)


###############################
###########Plotting###########
#############################

wordscape_hist <- other_wordscape_df %>%
  unnest_tokens(word, text) %>%
  anti_join(my_stop_words) %>%
  count(word, sort=TRUE) %>%
  top_n(35,n) %>%
  mutate(word2 = fct_reorder(word, n)) %>%
  ggplot(aes(word2, n))+
  geom_col(fill = "dodgerblue1")+
  xlab("Word")+
  ylab("Number of mentions") +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red",
                       midpoint = 10) +
  coord_flip()

wordscape_hist




#Wordcloud based on frequency, grouping the most frequent words together
wordscape_tokens %>%
  top_n(100,n) %>%
  with(wordcloud(word, n,min.freq =1,colors= brewer.pal(8,"Set2"),random.order = F,rot.per =0.35 ))


words <- wordscape_tokens %>% count(word, sort=TRUE)


wordcloud(words = wordscape_df$word, freq = wordscape_df$freq, min.freq = 1,           max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))



#########################
######### Afinn #######
#########################

afinn <- wordscape_tokens %>%
  inner_join(get_sentiments("afinn"))%>%
  #group_by(index= n %/% 0.005) %>% #using integer division to define larger sections of text
  summarise(sentiment=mean(value)) %>%
  mutate(method="AFINN")


View(afinn)


#########################
######### Bing #######
#########################

###Bing sentiment
wordscape_bing <- other_wordscape_df %>%
  unnest_tokens(word, text) %>%
  anti_join(my_stop_words)%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

wordscape_bing


##Bing sentiment grouped by sentiment
wordscape_bing%>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


wordscape_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words = 150)

#########################
######### NRC #######
#########################

wordscape_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(0.5,1),
                   fixed.asp =T,
                   title.size = 1)







#########################
##########Bigrams########
#########################

my_bigrams <- other_wordscape_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

my_bigrams #We want to see the bigrams (words that appear together, "pairs")

my_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

View(bigram_counts)



bigram_graph <- bigram_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



#########################
##########trigrams########
#########################

my_trigrams <- other_wordscape_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

my_trigrams #We want to see the bigrams (words that appear together, "triplets")

my_trigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

library(saotd)
trigrams_separated <- my_trigrams %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word)

#creating the new bigram, "no-stop-words":
trigram_counts <- trigrams_filtered %>%
  count(word1, word2,word3, sort = TRUE)
#want to see the new bigrams
trigram_counts

View(trigram_counts) ###########conclusion = trigram not useful

