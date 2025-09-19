#################################
### MSBA 1 HULT 2021-2022
### Business Insight Assignment
### "The Fast Fashion Revolution"
### Created by: Maria Paula Lopez Moreno
### Date: 12.02.2021
### Version 1.0
################################

# This project is going to be focused in analyze different text related with fast fashion and the
# impact that is having around the world.

### Calling Libraries ###
library(pdftools)
library(tm)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(textdata)
library(stringr)

#########################
### Importing my data ###
#########################

setwd("D:/Documentos/HULT/UNIVERSITY/TEXT ANALYTICS & NPL/Business Case/PDF files")
nm <- list.files(path="D:/Documentos/HULT/UNIVERSITY/TEXT ANALYTICS & NPL/Business Case/PDF files")
fastfashionarticles <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))

fastfashiondf <- data.frame(fastfashionarticles)
fastfashiondf$article <- c("article 1", "article 2", "article 3", "article 4", "article 5",
                           "article 6", "article 7", "article 8")
fastfashiondf <- fastfashiondf %>%  
  unite ("text", 1:12 ,sep = " ", remove = FALSE, na.rm = FALSE)

### Creating a new dataframe with Articles and Descriptions ###

myffdf <- cbind.data.frame(text = fastfashiondf$text, article = fastfashiondf$article)

### Understanding my data ###

##########################
### Tokenizing my data ###
##########################

# Calling data
data("stop_words")

### Adding some words that we found that are frequent but has no business insights for the analysis ###
stopff <- tribble(~word,~lexicon,
                  "https", "CUSTOM",
                  "17", "CUSTOM",
                  "92", "CUSTOM",
                  "20", "CUSTOM",
                  "2019", "CUSTOM",
                  "2018", "CUSTOM",
                  "2019032026843", "CUSTOM",
                  "xjsutum_kyo.twitter", "CUSTOM",
                  "khaleejtimes", "CUSTOM",
                  "fashionunited.com", "CUSTOM",
                  "https", "CUSTOM",
)
stop_words2 <- stop_words %>%
  bind_rows(stopff)

token_ff <- myffdf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) %>%
  count(word, sort = TRUE)

### Seeing and analyzing the Total Frequency in all the documents ###
tfhist_ff <- token_ff %>%
  mutate(word=reorder(word, n)) %>%
  top_n(10,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(tfhist_ff)

# As expected, the most common words in the article are fashion, fast, clothing and industry
# It is important to see that environmental, sustainable, consumers and consumption are also common words due that problem that the fast fashion industry represents

### Seeing and Analyzing Frequency per Article ###
tokenperarticle_ff <- myffdf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) %>%
  count(word, article, sort = T)

freq_hist_ff <- tokenperarticle_ff %>%
    group_by(article) %>%  
    top_n(15) %>%
    ungroup() %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = article))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~article, scales = "free_y")+
    xlab(NULL)+
    coord_flip()
  
print(freq_hist_ff)

# Based on the frequency per article I assume:
# 1st Article is related with health and environmental problems related to fashion industry
# 2nd Article is related with the consumption of clothing and the ethics behind the fashion industry
# 3rd Article is can be an overall research of the market and the changes with the consumer behavior and the fashion industry
# 4th Article is related with the cashmere (material used in many clothes) and probably with it's impact in the environment and the use of maybe other type of materials
# 5th Article is related with the textile industry in Africa 
# 6th Article is related with the company Zara and it's role in the fast fashion industry
# 7th Article is probably related with an overall document about fast fashion and it's impact in the world
# 8th Article is probably related with the principal brand that are doing something or are someway related with the fast fashion industry

##########################
### Sentiment Analysis ###
##########################

# Calling data
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

token_ffs <- myffdf %>%
  unnest_tokens(word, text)

afinn_ff <- token_ffs %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  token_ffs%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  token_ffs %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>% #I'm extracting the sentiments as a binary variable
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_ff, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# In general the articles have positive sentiments. 

# Analyzing the sentiments
bing_counts <- token_ffs %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+ #this line of code puts our sentiment in columns (we are going to have as many columns as sentiments)
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# fast is one of the words with major frequency and it is show as a positive one. Nevertheless, in our context it represents a negative sentiment

################
### TF - IDF ###
################

ff_token <- myffdf %>%
  unnest_tokens(word, text) %>%
  count(article, word, sort=TRUE) %>%
  ungroup()

total_words <- ff_token %>%
  group_by(article) %>%
  summarize(total=sum(n))

ff_words <- left_join(ff_token, total_words)

ggplot(ff_words, aes(n/total, fill = article))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~article, ncol=2, scales="free_y")

# The result is not the expected. Adjust the formula and re-run the code
# Adjusting bins to a lower level and xlim y to a higher one. This must be done because the frequencies of the words are small due of the total amount of words.

ggplot(ff_words, aes(n/total, fill = article))+
  geom_histogram(bins = 10,show.legend=FALSE)+
  xlim(NA, 0.01) +
  facet_wrap(~article, ncol=2, scales="free_y")

# The result is better. The articles, in general, are normally distributed and right-skewed.

##################
### ZIPF's law ###
##################

freq_by_rank <- ff_words %>%
  anti_join(stop_words2) %>% 
  group_by(article) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

# As we saw in the first part fashion is the most common word. There are other words as fashion, environmental, consumption, clothing that we know that are frequent, but are not giving us different business insights.

# Seeing the most unique but most frequent words in the articles
article_words <- ff_words %>%
  anti_join(stop_words2) %>% 
  bind_tf_idf(word, article, n) %>%
  arrange(desc(tf_idf))

article_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(article) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=article))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~article, ncol=2, scales="free")+
  coord_flip()

# there are some numbers as 2006 that can mean something important about data.
# names of brands as Benetton or Zara can give us new insights about the article 6
# we have new sustainable materials as piñatex, recycle materials as plastic, that tell us about more important innovations
# about our article from Africa, we have an specific location Bagamoyo in Tanzania.

###############
### Bigrams ###
###############

# The semantic structure is something important. Thus, the analysis of the documents is going to be run with some parts of the speech that can give more insights about the fast fashion industry.
# The first part showed that the documents are not too extensive (based on the frequency of the words). Analyzing bigrams is a good method to find out more business insights and can clear if the sentiment analysis is accurate or not. 

ff_bigrams <- myffdf %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort = TRUE)

ff_bigrams_separated <- ff_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words2$word) %>%
  filter(!word2 %in% stop_words2$word) %>% 
  top_n(100,n) 

ffbigram <- ff_bigrams_separated %>%  
  unite ("bigram", 1:2 ,sep = " ", remove = FALSE, na.rm = FALSE)

ffbigram1 <- cbind.data.frame(bigram = ffbigram$bigram, n = ffbigram$n) 

freq_ffb <- ffbigram1 %>%
  mutate(bigram=reorder(bigram, n)) %>%
  filter(n > 20) %>% 
  ggplot(aes(bigram, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_ffb)

# Different results as the supply chain, the environmental justice, the hand clothing, international journal and income countries
# Can give us more insights about what is fast fashion and why it is important to know more about it as consummers

# Bigram Networks

bigram_graph <- ff_bigrams_separated %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# With this analysis we can see more things about the documents and the frequent phrases.
# An example is that John Wiley & Sons are the editors of one of the documents that is an international journal of consumer studies.
# Nevertheless, we cannot clean all this words or the numbers because can be some way related also with the business insights that can be taken from other documents.

# Let's analyze the same framework per article
ff_articlebigrams <- myffdf %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, article, sort = T)

ff_articlebigrams_separated <- ff_articlebigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words2$word) %>%
  filter(!word2 %in% stop_words2$word) %>% 
  top_n(100,n)

## article 1 ##
bigrama1_graph <- ff_articlebigrams_separated %>% 
  filter(article == "article 1") %>%
  graph_from_data_frame()

bigrama1_graph

ggraph(bigrama1_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


## article 2 ##
bigrama2_graph <- ff_articlebigrams_separated %>% 
  filter(article == "article 2") %>%
  graph_from_data_frame()

bigrama2_graph

ggraph(bigrama2_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# As this one is the article related with jhon wiley we are going to remove the words that we found that are not givinig us business insights just for this one.
stopffa2 <- tribble(~word,~lexicon,
                  "https", "CUSTOM",
                  "17", "CUSTOM",
                  "92", "CUSTOM",
                  "20", "CUSTOM",
                  "2019", "CUSTOM",
                  "2018", "CUSTOM",
                  "2019032026843", "CUSTOM",
                  "xjsutum_kyo.twitter", "CUSTOM",
                  "khaleejtimes", "CUSTOM",
                  "fashionunited.com", "CUSTOM",
                  "https", "CUSTOM",
                  "john", "CUSTOM",
                  "wiley", "CUSTOM",
                  "sons", "CUSTOM",
                  "212", "CUSTOM",
                  "222", "CUSTOM",
                  "39", "CUSTOM",
                  "2015", "CUSTOM",
                  "studies", "CUSTOM",
                  "international", "CUSTOM",
                  "journal", "CUSTOM"
)
stop_words2a2 <- stop_words %>%
  bind_rows(stopffa2)

ff_article2bigrams <- myffdf %>%
  filter(article == "article 2") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort = T)

ff_article2bigrams_separated <- ff_article2bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words2a2$word) %>%
  filter(!word2 %in% stop_words2a2$word) %>% 
  top_n(100,n)

cleanbigrama2_graph <- ff_article2bigrams_separated %>% 
  filter(n > 3) %>%
  graph_from_data_frame()

cleanbigrama2_graph

ggraph(cleanbigrama2_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Now we are getting more insights about this particular article.

## article 3 ##
bigrama3_graph <- ff_articlebigrams_separated %>% 
  filter(article == "article 3") %>%
  graph_from_data_frame()

bigrama3_graph

ggraph(bigrama3_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# This particular article give us one interesting insight
# They cite a lot of information from other research from Barnes, L., and G. Lea-Greenwood that was published in 2006 related with the fast fashion supply chain

## article 4 ##
bigrama4_graph <- ff_articlebigrams_separated %>% 
  filter(article == "article 4") %>%
  graph_from_data_frame()

bigrama4_graph

ggraph(bigrama4_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Here we can see that the article is related with a gradually phase of the cashmere industry to cotton or more sustainable sourced materials.

## article 5 ##
bigrama5_graph <- ff_articlebigrams_separated %>% 
  filter(article == "article 5") %>%
  graph_from_data_frame()

bigrama5_graph

ggraph(bigrama5_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

## article 6 ##
bigrama6graph <- ff_articlebigrams_separated %>% 
  filter(article == "article 6") %>%
  graph_from_data_frame()

bigrama6graph

ggraph(bigrama6graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# In this article we have a similar situation that in the article 2. It is a publication of a journal of the London Business School.
stopffa6<- tribble(~word,~lexicon,
                    "https", "CUSTOM",
                    "17", "CUSTOM",
                    "92", "CUSTOM",
                    "20", "CUSTOM",
                    "2019", "CUSTOM",
                    "2018", "CUSTOM",
                    "2019032026843", "CUSTOM",
                    "xjsutum_kyo.twitter", "CUSTOM",
                    "khaleejtimes", "CUSTOM",
                    "fashionunited.com", "CUSTOM",
                    "https", "CUSTOM",
                    "journal", "CUSTOM",
                    "compilation", "CUSTOM",
                    "london", "CUSTOM",
                    "business", "CUSTOM",
                    "school", "CUSTOM",
                    "strategy", "CUSTOM",
                    "review", "CUSTOM",
                    "summer", "CUSTOM",
)

stop_words2a6 <- stop_words %>%
  bind_rows(stopffa6)

ff_article6bigrams <- myffdf %>%
  filter(article == "article 6") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort = T)

ff_article6bigrams_separated <- ff_article6bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words2a6$word) %>%
  filter(!word2 %in% stop_words2a6$word) %>% 
  top_n(100,n)

cleanbigrama6_graph <- ff_article6bigrams_separated %>% 
  filter(n > 3) %>% 
  graph_from_data_frame()

cleanbigrama6_graph

ggraph(cleanbigrama6_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# The first thing that we can see cleaning this article, is that the bigrams have low frequencies so it is probably a short document related with the lessons about fast fashion retailers.

## article 7 ##
bigrama7_graph <- ff_articlebigrams_separated %>% 
  filter(article == "article 7") %>%
  graph_from_data_frame()

bigrama7_graph

ggraph(bigrama7_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# This one is a short note about fast fashion and the conscious collection
# we are going to change our source so we can have more words for our business insights
ff_articlebigrams_separated_short <- ff_articlebigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words2$word) %>%
  filter(!word2 %in% stop_words2$word) %>% 
  filter(n>3)

bigrama7_graph_short <- ff_articlebigrams_separated_short %>% 
  filter(article == "article 7") %>%
  graph_from_data_frame()

bigrama7_graph_short

ggraph(bigrama7_graph_short, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# This article analyze the amount of people around the globe and the usage of different materials an resources and its relation with the price

## article 8 ##
bigrama8_graph <- ff_articlebigrams_separated %>% 
  filter(article == "article 8") %>%
  graph_from_data_frame()

bigrama8_graph

ggraph(bigrama8_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Same problem as last article

bigrama8_graph_short <- ff_articlebigrams_separated_short %>% 
  filter(article == "article 8") %>%
  graph_from_data_frame()

bigrama8_graph_short

ggraph(bigrama8_graph_short, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# A short document about the rights of the workers, analyze the social media and the influencer culture related with the fast fashion production and diferent brands, principally located in uk.

#####################
### IDF + Bigrams ###
#####################
# Based on the analysis, the data is going to be filtered again with the stop words that are not relevant in overall.
# This last analysis is a combination of the frameworks TF_IFD and Bigrams and it is going to be applied to the overall information.

stopfinal <- tribble(~word,~lexicon,
                  "https", "CUSTOM",
                  "17", "CUSTOM",
                  "92", "CUSTOM",
                  "20", "CUSTOM",
                  "2019", "CUSTOM",
                  "2018", "CUSTOM",
                  "2019032026843", "CUSTOM",
                  "xjsutum_kyo.twitter", "CUSTOM",
                  "khaleejtimes", "CUSTOM",
                  "fashionunited.com", "CUSTOM",
                  "https", "CUSTOM",
                  "john", "CUSTOM",
                  "wiley", "CUSTOM",
                  "sons", "CUSTOM",
                  "212", "CUSTOM",
                  "222", "CUSTOM",
                  "39", "CUSTOM",
                  "2015", "CUSTOM",
                  "studies", "CUSTOM",
                  "international", "CUSTOM",
                  "journal", "CUSTOM",
                  "compilation", "CUSTOM",
                  "london", "CUSTOM",
                  "business", "CUSTOM",
                  "school", "CUSTOM",
                  "strategy", "CUSTOM",
                  "review", "CUSTOM",
                  "summer", "CUSTOM",
                  "cnn", "CUSTOM"
)
stopffwords <- stop_words %>%
  bind_rows(stopfinal)

finalbigram <- myffdf %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, article, sort = TRUE)

# Separating the data frame based on the analysis made in the bigrams per article (long and short documents)
# Filtering the longer articles

long_articles <- finalbigram %>% 
  filter(!article == "article 7" & !article == "article 8")

short_articles <- finalbigram %>% 
  filter(article == "article 7" | article == "article 8")

# Important to notice that here we must use | instead of & or we are going to have an empty data frame.

# Analyzing long articles
final_bigrams_separated_l <- long_articles %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stopffwords$word) %>%
  filter(!word2 %in% stopffwords$word) %>% 
  top_n(100,n)

fffinalbigram_l <- final_bigrams_separated_l %>%  
  unite ("bigram", 1:2 ,sep = " ", remove = FALSE, na.rm = FALSE)

fffinalbigram1 <- cbind.data.frame(bigram = fffinalbigram_l$bigram, article = fffinalbigram_l$article, n = fffinalbigram_l$n)

total_bigrams <- fffinalbigram1 %>%
  group_by(article) %>%
  summarize(total=sum(n))

fffinal_bigrams <- left_join(fffinalbigram1, total_bigrams)

article_bigrams <- fffinal_bigrams %>%
  bind_tf_idf(bigram, article, n) %>%
  arrange(desc(tf_idf))

article_bigrams %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>%
  group_by(article) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill=article))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~article, ncol=2, scales="free")+
  coord_flip()

# The graph for the article 4th is not easy to understand. Filtering the data to have just this article
article_bigrams %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>%
  group_by(article) %>%
  filter(article == "article 4") %>% 
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill=article))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~article, ncol=2, scales="free")+
  coord_flip()

# # Analyzing long articles
final_bigrams_separated_s <- short_articles %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stopffwords$word) %>%
  filter(!word2 %in% stopffwords$word) %>% 
  filter(n>3) 

fffinalbigram_s <- final_bigrams_separated_s %>%  
  unite ("bigram", 1:2 ,sep = " ", remove = FALSE, na.rm = FALSE)

fffinalbigram2 <- cbind.data.frame(bigram = fffinalbigram_s$bigram, article = fffinalbigram_s$article, n = fffinalbigram_s$n)

total_bigrams_S <- fffinalbigram2 %>%
  group_by(article) %>%
  summarize(total=sum(n))

fffinal_bigrams_s <- left_join(fffinalbigram2, total_bigrams_S)

article_bigrams_s <- fffinal_bigrams_s %>%
  bind_tf_idf(bigram, article, n) %>%
  arrange(desc(tf_idf))

article_bigrams_s %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>%
  group_by(article) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill=article))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~article, ncol=2, scales="free")+
  coord_flip()

# Conclusions from the text analysis:
# The fast fashion industry represents a challenge specially for the environmental impact that it has.
# Brands as Zara, HM and Benetton are part of this industry, and now they are focusing their efforts in trying to replace their materials to create more sustainable fashion clothing, even when the tendencies are rapidly changing.
# The impact of the fashion industry is not only over the environment, but also over political matters as workers rights. 
# The consumers are responsible of the way there are consuming and have to be aware of the ethical fashion behind their clothe.
# The general analysis of the sentiment of the articles related to fast fashion shows a positive sentiment. Nevertheless, after complementing the analysis with the semantic structure of the words, the "fast" labeled as a positive sentiment, actually represents a negative one. That changes the general sentiment, reinforcing the importance of look over the challenges that the fast fashion industry represents.