library(textmineR)
library(ggraph)
library(igraph)
library(tidyverse)
library(gridExtra)
library(grid)
library(ggplot2)


GCA<-generic_ca_comb %>% select(text, status_id) 
GCA$text<- gsub("http\\S+\\s*", "", GCA$text) #replace all strings starting with url with blank 
GCA <-GCA %>% distinct(text, .keep_all= TRUE) # keep only those that are unique - this prevents them beig dominated by specific working of news headline. 
GCA$text<- gsub("&amp;", "and", TT$text) 

# 18170 to 15397 

#stop_words_edit_vec<- stop_words_edit$word %>% unique()

my_stop_words <- stop_words %>% select(-lexicon) %>% unique() %>% 
  bind_rows(data.frame(word = c("app", "corona", "coronavirus", "app", "19", "covid", "amp")))

# create DTM
dtm <- CreateDtm(GCA$text, 
                   doc_names = GCA$status_id, # each person treated as seperate doc
                   remove_numbers = TRUE, # keep numbers 
                 stopword_vec = my_stop_words,
                   ngram_window = c(1, 1)) # n grams of 1 to 3 words


dtm <- dtm[,colSums(dtm) > 2] # keep only words that appear more than once 

# need some better custom stopwords specifically remove https


tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)


# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
# dtm = dtm

# Create set of LDA models to see which has highest coherence 

k_list <- seq(11, 31, by = 1) # create k list of 1 -20 

model_list<- list() # create empty model list to store models 

for(i in k_list){ # for each k 1:20
  set.seed(64321) # set seed so repeatable
  m <- FitLdaModel(dtm = dtm, k = i, iterations = 500) # fit a model with k in k list 
  m$k <- i # attach k to model 
  m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5) # calculate coherence 
  model_list<-append(model_list, list(m)) # add model to model list 
}


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                              coherence = sapply(model_list, function(x) mean(x$coherence)), 
                              stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal()  + ylab("Coherence")

model_list_big<- model_list


# creating models from 1-100 in intervals of 10 indicated the optimal coherence window to between around 20 

model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
set.seed(64321)
model$top_terms <- GetTopTerms(phi = model$phi, M = 30)
top20_wide <- as.data.frame(model$top_terms)


model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
set.seed(64321)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])

set.seed(64321)
plot(model$hclust)



dendrogram<-as.dendrogram(hclust(as.dist(model$topic_linguistic_dist), "ward.D"))

dendo<-ggraph(dendrogram, 'dendrogram') + 
  geom_edge_bend(edge_colour = "#1DA1F2", edge_width =1) +
  theme_graph(background = "white",
              plot_margin = margin(0, 0, 0, 0)
)

dendo 

model[["hclust"]][["order"]]
#  8  7  6  5  9 10  3  1  4 11  2 12

top20_wide <- top20_wide[, c(8, 2, 7, 6, 5, 10,9, 3, 1, 4, 11, 12)]

names(top20_wide)<- c("Topic 1", "Topic 2","Topic 3","Topic 4","Topic 5","Topic 6","Topic 7","Topic 8",
                      "Topic 9","Topic 10","Topic 11","Topic 12")

tt<- ttheme_minimal(base_size = 9, padding = unit(c(0, 2), "mm" ),
                    colhead=list(bg_params=list(fill="#1DA1F2"), fg_params=list(col="#FFFFFF")))
tbl <- tableGrob(top20_wide, rows=NULL, theme = tt) 
grid.draw(tbl)
tbl$widths <- unit(rep(1/ncol(tbl), ncol(tbl)), "null")
grid.arrange(dendo, tbl, heights = c(1,3))



modelsummary<-SummarizeTopics(model)

modelsummary %>% ggplot(aes(x=topic, y= prevalence))+ geom_bar(stat = "identity")
modelsummary %>% ggplot(aes(x=topic, y= coherence))+ geom_bar(stat = "identity")

# removing all urls from text. 
GCATM_text<-generic_ca_comb %>% select(text, status_id) 
GCATM_text$text<- gsub("http\\S+\\s*", "", GCATM_text$text) #replace all strings starting with url with blank 
GCATM_text <-GCATM_text%>% distinct(text, .keep_all= TRUE) # keep only those that are unique - this prevents them beig dominated by specific working of news headline. 
# THIS CURRENTLY REDUCES DF FROM 17212 TO 14562 (2650 deleted that are the exact same text) 

#turn custom stop words into a vector to be used in dtm function

my_stop_words <- stop_words %>% select(-lexicon) %>% unique() %>% 
  bind_rows(data.frame(word = c("app", "covid", "corona", "coronavirus")))


NoURLdtm <- CreateDtm(GCATM_text$text, 
                 doc_names =GCATM_text$status_id, # each person treated as seperate doc
                 remove_numbers = TRUE, # keep numbers 
                 stopword_vec = my_stop_words,
                 # stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"), I don't think stemming seems necessary in this case ? 
                 ngram_window = c(1, 2)) # n g

NoURLModel <- FitLdaModel(dtm = NoURLdtm, k = 50, iterations = 500) # fit a model with k in k list 
NoURLModel$top_terms <- GetTopTerms(phi = NoURLModel$phi, M = 20)
top20_wide <- as.data.frame(NoURLModel$top_terms)

NoURLModel$topic_linguistic_dist <- CalcHellingerDist(NoURLModel$phi)
set.seed(64321)
NoURLModel$hclust <- hclust(as.dist(NoURLModel$topic_linguistic_dist), "ward.D")
NoURLModel$hclust$labels <- paste(NoURLModel$hclust$labels, NoURLModel$labels[ , 1])

set.seed(64321)
plot(NoURLModel$hclust)

top20_wide <- top20_wide[, c(45, 46, 27, 28,  4, 18, 29, 37,  3, 14, 33, 16, 47, 15, 23, 38, 49, 43, 19, 20,  2, 21, 42, 22, 44, 13,  9,
                             50, 10, 32, 11, 40, 30, 36,  7, 34, 48,  6, 25, 26, 39,  8,  5, 35, 12, 41,  1, 31, 17, 24)]





######

# track and trace topic modelling 

TT<-track_trace_comb %>% select(text, status_id) 
TT$text<- gsub("http\\S+\\s*", "", TT$text) #replace all strings starting with url with blank 
TT <-TT %>% distinct(text, .keep_all= TRUE) # keep only those that are unique - this prevents them beig dominated by specific working of news headline. 
TT$text<- gsub("&amp;", "and", TT$text) 

library(tidytext)
data(stop_words)

my_stop_words <- stop_words %>% select(-lexicon) %>% unique() %>% 
  bind_rows(data.frame(word = c("app", "track", "trace")))

TTdtm <- CreateDtm(TT$text, 
                      doc_names =TT$status_id, # each person treated as seperate doc
                      remove_numbers = TRUE, # keep numbers 
                      stopword_vec = my_stop_words,
                      # stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"), I don't think stemming seems necessary in this case ? 
                      ngram_window = c(1, 2)) # n g

TTModel <- FitLdaModel(dtm = TTdtm, k = 20, iterations = 500) # fit a model with k in k list 
TTModel$top_terms <- GetTopTerms(phi = TTModel$phi, M = 20)
top20_wide <- as.data.frame(TTModel$top_terms)

TTModel$topic_linguistic_dist <- CalcHellingerDist(TTModel$phi)
set.seed(64321)
TTModel$hclust <- hclust(as.dist(TTModel$topic_linguistic_dist), "ward.D")
TTModel$hclust$labels <- paste(TTModel$hclust$labels, TTModel$labels[ , 1])

set.seed(64321)
plot(TTModel$hclust)