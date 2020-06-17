library(tidyr)
library(tidytext)

# Analysis ####

# Top Words ####

# unest tweet texts 
tweet_words <- giant_comb %>% select(status_id, text) %>% unnest_tokens(word,text)

# Most popular words analysis ####
# what are the most popular words?
tweet_words %>% count(word,sort=T) %>% filter(word != "app") %>% slice(1:100) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  xlab("")

# app additional common twitter words to stop words
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp")))

# keep only interesting words
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

# put into graph 
tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% filter(word != "app") %>%  
  slice(1:50) %>% ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60,  hjust = 1)) + 
  xlab("")


# BIGRAMS

CVA_bigrams<-giant_comb %>% select(status_id, text) %>% unnest_tokens(bigram,text,token = "ngrams", n = 2) 

bigrams_CVA <-CVA_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ")

# bigrams
bigrams_CVA %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 80) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# TRIGRAMS

CVA_trigrams<-giant %>% select(status_id, text) %>% unnest_tokens(trigram,text,token = "ngrams", n = 3) 

# bigrams_CVA <-CVA_bigrams %>%
#   separate(bigram, c("word1", "word2"), sep = " ")%>%
#   filter(!word1 %in% my_stop_words$word) %>%
#   filter(!word2 %in% my_stop_words$word) %>% 
#   unite(bigram, word1, word2, sep = " ")

# bigrams
CVA_trigrams %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Time plot ####

ts_plot(giant, "hours")
ts_plot(giant_040520, "hours")
ts_plot(giant_comb, "hours")

ts_plot(generic_ca_comb, "hours")

# large spike on april 27th with launch of covidsafe app in australia. 


# Sentiment Analysis ####

nrc_lex <- get_sentiments("nrc")
bing_lex <- get_sentiments("bing")

bing_lex$sentiment[bing_lex$word == "virus"]<-NA
bing_lex$sentiment[bing_lex$word == "symptoms"]<-NA
bing_lex$sentiment[bing_lex$word == "infection"]<-NA
bing_lex$sentiment[bing_lex$word == "infections"]<-NA
bing_lex$sentiment[bing_lex$word == "infected"]<-NA


CA_sentiment_bing <- tweet_words_interesting %>% left_join(bing_lex) 

CA_sentiment_bing %>% filter(!is.na(sentiment))%>% ggplot(aes(x=sentiment))+ geom_histogram(stat = "count")

CA_sentiment_nrc <- tweet_words_interesting %>% left_join(nrc_lex) 

CA_sentiment_nrc %>% filter(!is.na(sentiment))%>% ggplot(aes(x=sentiment))+ geom_histogram(stat = "count")


bing_word_counts <- CA_sentiment_bing %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(70) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# should virus, symptom, infection/infections/infected be part of this? - review 

# Location Analysis ####

locations<-giant_comb %>% filter(!is.na(country))
# very few tweets have location enabled - 517 out of 14871 - approximately 3.4% - however perhaps this gives some indication of where the tweets in general are coming from

giant_comb %>% filter(!is.na(country))%>%  count(country, sort = TRUE) %>% 
  ggplot(aes(x=reorder(country, n, function(n)-n), y=n))+ 
  geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")


generic_ca_comb %>% filter(!is.na(country))%>%  count(country, sort = TRUE) %>% 
  ggplot(aes(x=reorder(country, n, function(n)-n), y=n))+ 
  geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")


# Hashtags ####

hashtags<-giant_comb %>% filter(!is.na(hashtags))

hashtags <- as.character(hashtags$hashtags)
hashtags <- gsub("c\\(", "", hashtags)
hashtags

library(wordcloud)

set.seed(1234)
wordcloud(hashtags, min.freq=15,  random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "Dark2"))


hashtags2<-generic_ca_comb %>% filter(!is.na(hashtags))

hashtags2 <- as.character(hashtags2$hashtags)
hashtags2 <- gsub("c\\(", "", hashtags2)
hashtags2


set.seed(1234)
wordcloud(hashtags2, min.freq=10,  random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "Dark2"))

# predominantly Australia as app there has been most widespread and put into action but this could change over the preceeding weeks as it comes into force in other countries


# Notes ####

# maybe use botornot account to filter ? https://github.com/mkearney/tweetbotornot

# Action points to do:  ####

# what words are the most positive/ negative/ fearful etc.
  # does context match up with this - for ecample contact seen as good isolation as bad but probably neutral - come up with custom stopwords
# split up by days/ weeks to see how things have changed - positve and negative 
# filter by country and context to see diferent peaks - e.g. conversation seems to be dominated by australia currently but could turn to UK with isle of wright
# what are the top linked articles etc within this 
# what are the most mentioned screen names in the dataset - WHO is being connected to (e.g. australian PM, health ministers etc.)

count(giant_comb$mentions_screen_name)






