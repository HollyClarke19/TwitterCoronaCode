

```r
library(tidyr)
library(tidytext)
```


# Analysis 

## Top Words 

Unest words, remove stopwords, show in graph from 

'giant' are produced by search term: 
search_tweets(  "\"corona app\" OR \"covid app\" OR \"corona virus app\" OR \"coronavirus app\" OR \"covid 19 app\" OR \"covid-19 app\" OR \"contact trac\" OR \"covid symptom track\" OR \"covidsafe app\"", include_rts = FALSE, retryonratelimit = TRUE, lang = "en")

generic_ca are produced by the term: 
search_tweets("\"corona app\" OR \"covid app\" OR \"corona virus app\" OR \"coronavirus app\" OR \"covid 19 app\" OR \"covid-19 app\"", include_rts = FALSE, retryonratelimit = TRUE, lang = "en")

giant includes mentions specificly of apps like contact tracing, covid symptom tracker and covidsafe (australia)


```r
# unest tweet texts 
tweet_words <- giant_comb %>% select(status_id, text) %>% unnest_tokens(word,text)

# Most popular words analysis ####
# what are the most popular words?
tweet_words %>% count(word,sort=T) %>% filter(word != "app") %>% slice(1:30) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  xlab("")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
# app additional common twitter words to stop words
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp")))
```

```
## Warning in bind_rows_(x, .id): binding character and factor vector, coercing into character vector
```

```r
# keep only interesting words
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)
```

```
## Joining, by = "word"
```

```r
# put into graph 
tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% filter(word != "app") %>%  
  slice(1:30) %>% ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60,  hjust = 1)) + 
  xlab("")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png)

Many words simply around the topic of the apps e.g. covidsafe, covid, downloacd contact, million (popularity of downloads).
Also actors in the tech field goodle/ apple. 
Context of Australia mainly - auspol, australians
Words such as privicy, rejects, trust, health are also prominent relating to sensativity of health data. 


```r
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
  filter(n > 90) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Mostly context based - news organisations, references to Austalian PM, mentions of search terms, apple google prominent as they had a joint initiative around tracing app. Also Indian context - setu mera. Some positive - save lives


```r
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
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

As above 

# Time plot

```r
ts_plot(giant_comb, "hours")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
ts_plot(generic_ca_comb, "hours")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)

Large spike on April 27th with launch of covidsafe app in australia. Levels have been maintained quite high since then. For non covidsafe terms there is also a large spike on May 2nd. Need for further searching to see why this may be the case - 




```r
# Sentiment Analysis ####

nrc_lex <- get_sentiments("nrc")
bing_lex <- get_sentiments("bing")

# remove virus related words as these are neutral in this context 
bing_lex$sentiment[bing_lex$word == "virus"]<-NA
bing_lex$sentiment[bing_lex$word == "symptoms"]<-NA
bing_lex$sentiment[bing_lex$word == "infection"]<-NA
bing_lex$sentiment[bing_lex$word == "infections"]<-NA
bing_lex$sentiment[bing_lex$word == "infected"]<-NA


CA_sentiment_bing <- tweet_words_interesting %>% left_join(bing_lex) 
```

```
## Joining, by = "word"
```

```r
CA_sentiment_bing %>% filter(!is.na(sentiment))%>% ggplot(aes(x=sentiment))+ geom_histogram(stat = "count")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
CA_sentiment_nrc <- tweet_words_interesting %>% left_join(nrc_lex) 
```

```
## Joining, by = "word"
```

```r
CA_sentiment_nrc %>% filter(!is.na(sentiment))%>% ggplot(aes(x=sentiment))+ geom_histogram(stat = "count")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-2.png)

```r
bing_word_counts <- CA_sentiment_bing %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
```

```
## Joining, by = c("word", "sentiment")
```

```r
bing_word_counts
```

```
## # A tibble: 1,838 x 3
##    word       sentiment     n
##    <chr>      <chr>     <int>
##  1 trust      positive    619
##  2 rejects    negative    549
##  3 concerns   negative    425
##  4 safe       positive    405
##  5 support    positive    256
##  6 protect    positive    250
##  7 issues     negative    235
##  8 capability positive    225
##  9 top        positive    191
## 10 positive   positive    154
## # ... with 1,828 more rows
```

```r
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
```

```
## Selecting by n
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-3.png)

Words normally seen as negative but relating to virus made neutral e.g. sypmtom, infect etc. 

# Location Analysis

```r
locations<-giant_comb %>% filter(!is.na(country))

giant_comb %>% filter(!is.na(country))%>%  count(country, sort = TRUE) %>% 
  ggplot(aes(x=reorder(country, n, function(n)-n), y=n))+ 
  geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
generic_ca_comb %>% filter(!is.na(country))%>%  count(country, sort = TRUE) %>% 
  ggplot(aes(x=reorder(country, n, function(n)-n), y=n))+ 
  geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png)

Very few tweets have location enabled - 517 out of 14871 - approximately 3.4% - however perhaps this gives some indication of where the tweets in general are coming from

Predominantly Australia as app there has been most widespread and put into action but this could change over the preceeding weeks as it comes into force in other countries. However UK is also second. 

# Hashtags 

```r
hashtags<-giant_comb %>% filter(!is.na(hashtags))

hashtags <- as.character(hashtags$hashtags)
hashtags <- gsub("c\\(", "", hashtags)

library(wordcloud)

set.seed(1234)
wordcloud(hashtags, min.freq=15,  random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "Dark2"))
```

```
## Warning in tm_map.SimpleCorpus(corpus, tm::removePunctuation): transformation drops documents
```

```
## Warning in tm_map.SimpleCorpus(corpus, function(x) tm::removeWords(x, tm::stopwords())): transformation drops documents
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
hashtags2<-generic_ca_comb %>% filter(!is.na(hashtags))

hashtags2 <- as.character(hashtags2$hashtags)
hashtags2 <- gsub("c\\(", "", hashtags2)


set.seed(1234)
wordcloud(hashtags2, min.freq=10,  random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "Dark2"))
```

```
## Warning in tm_map.SimpleCorpus(corpus, tm::removePunctuation): transformation drops documents

## Warning in tm_map.SimpleCorpus(corpus, tm::removePunctuation): transformation drops documents
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png)

# Mentioned Users 


```r
count(giant_comb$mentions_screen_name)
```

```
## Error in UseMethod("summarise_"): no applicable method for 'summarise_' applied to an object of class "list"
```

```r
users<-giant_comb %>% filter(!is.na(mentions_screen_name))

users <- as.character(users$mentions_screen_name)
users <- gsub("c\\(", "", users)

set.seed(1234)
wordcloud(users, min.freq=15,  random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "Dark2"))
```

```
## Warning in tm_map.SimpleCorpus(corpus, tm::removePunctuation): transformation drops documents
```

```
## Warning in tm_map.SimpleCorpus(corpus, function(x) tm::removeWords(x, tm::stopwords())): transformation drops documents
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

Most mentioned users by far as Scott Marrison (PM of Australia) and Greg Hunt (MP and Health Minister of Australia). This shows that Australia is dominating the conversation around this currently 

# Notes 

maybe use botornot account to filter ? https://github.com/mkearney/tweetbotornot

# Action points to do: 

 what words are the most positive/ negative/ fearful etc.

 does context match up with this - for ecample contact seen as good isolation as bad but probably neutral - come up with custom stopwords

split up by days/ weeks to see how things have changed - positve and negative 

filter by country and context to see diferent peaks - e.g. conversation seems to be dominated by australia currently but could turn to UK with isle of wight

what are the top linked articles etc within this 

what are the most mentioned screen names in the dataset - WHO is being connected to (e.g. australian PM, health ministers etc.)

knit2html("CoronaTweetsAnalysis.Rmd")




