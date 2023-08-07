library(tidyverse)
library(tidytext)
library(wordcloud2)
library(highcharter)
library(readxl)
library(DT)

ethics <- read_excel("corpus.xlsx")

stop <- read_csv("stop.txt")

#GENERAL CORPUS
corpusai <- data.frame(ethics$Country,ethics$Year,ethics$Text)
corpusai <- head(corpusai,33)
colnames(corpusai) <- c("Country","Year","Text")
datac <- ethics$Text

#FREQUENCIES AND CO-OCCURENCES
text_analysis_function <- function(data, column_name, save_word_frequency = FALSE, save_ngrams = FALSE) {
  df <- tibble(txt = data[[column_name]])
  
  text_df <- df %>%
    unnest_tokens(word, txt)
  
  lexicon <- text_df %>%
    anti_join (stop, by ="word")
  
  frequency <- lexicon %>%
    count(word, sort = TRUE) 
  word_frequency_chart <- frequency %>%
    filter(n > 8) %>%
    hchart(
      'bar', hcaes(x = word, y = n)
    )  %>%
    hc_xAxis(title = list(text = "Words")) %>% 
    hc_yAxis(title = list(text = "N"))
  
  if(save_word_frequency){
    print(word_frequency_chart)
  }else {
    word_frequency_chart
  }
  
  wordcloud_chart <- wordcloud2(frequency, size = 0.5, color = colorRampPalette(c("#1B72AA","#455573", "#CF3C3A", "#e27c7c", "#6b5f88","#986691","#787878","#CDCDCD"))(50))
  
  #BIGRAMS
  
  text_en_new <- df
  
  colnames(text_en_new) <- "word"
  
  bigrams <- text_en_new %>%
    unnest_tokens(bigram,word, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop$word,
           !word2 %in% stop$word) %>%
    count(word1, word2, sort = TRUE)
  
  bigrams_united <- bigrams %>%
    unite(bigram, word1, word2, sep = " ")
  
  bigrams_chart <- bigrams_united %>%
    filter(n > 2) %>%
    hchart(
      'bar', hcaes(x = bigram, y = n)
    )  %>%
    hc_xAxis(title = list(text = "Bigrams")) %>% 
    hc_yAxis(title = list(text = "Frequency"))
  if(save_ngrams){
    print(bigrams_chart)
  }else {
    bigrams_chart
  }
  
  ###TRIGRAMS
  
  trigrams <- text_en_new %>%
    unnest_tokens(trigram, word, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop$word,
           !word2 %in% stop$word,
           !word3 %in% stop$word) %>%
    count(word1, word2, word3, sort = TRUE)
  
  trigrams_united <- trigrams %>%
    filter(!is.na(word1) & !is.na(word2) & !is.na(word3)) %>%
    unite(trigram, word1, word2,word3, sep = " ") 
  
  trigrams_chart <- trigrams_united %>%
    filter(n > 1) %>%
    hchart(
      'bar', hcaes(x = trigram, y = n)
    )  %>%
    hc_xAxis(title = list(text = "Trigrams")) %>% 
    hc_yAxis(title = list(text = "Frequency"))
  
  if(save_ngrams){
    print(trigrams_chart)
  }else {
    trigrams_chart
  }
  list(frequency = frequency, trigrams = trigrams, bigrams = bigrams, word_frequency_chart = word_frequency_chart, wordcloud_chart = wordcloud_chart, bigrams_chart = bigrams_chart, trigrams_chart = trigrams_chart)
}

results <- text_analysis_function(corpusai, "Text", save_word_frequency = TRUE, save_ngrams = TRUE)
results$word_frequency_chart
results$wordcloud_chart
results$bigrams_chart
results$trigrams_chart

corpusfirst <- ethics$Text
corpusfirst <- head(corpusfirst,9)
corpusfirst <- as.data.frame(corpusfirst)
colnames(corpusfirst) <- c("Text")
results1wave <- text_analysis_function(corpusfirst, "Text", save_word_frequency = TRUE, save_ngrams = TRUE)

results1wave$word_frequency_chart
results1wave$wordcloud_chart
results1wave$bigrams_chart
results1wave$trigrams_chart

results1wave$frequency
results1wave$bigrams
results1wave$trigrams

freqb <- results1wave$frequency
freqb <- head(freqb,60)
gray_colors <- brewer.pal(3, "Greys")[-1]
wordcloud2(freqb, size = 0.5, color = gray_colors)

#SECOND WAVE AI

corpusec <- ethics$Text
corpusec <- tail(corpusec,24)
corpusec <- as.data.frame(corpusec)
colnames(corpusec) <- c("Text")
results2wave <- text_analysis_function(corpusec, "Text", save_word_frequency = TRUE, save_ngrams = TRUE)

results2wave$word_frequency_chart
results2wave$wordcloud_chart
results2wave$bigrams_chart
results2wave$trigrams_chart

results2wave$frequency

df2 <- as.data.frame(results2wave$frequency)
df2 <- df2 %>%
  filter(n >= 10)
View(df2)
results2wave$bigrams
results2wave$trigrams

freqc <- results2wave$frequency
freqc <- head(freqc,90)
gray_colors <- brewer.pal(3, "Greys")[-1]
wordcloud2(freqc, size = 0.9, color = gray_colors)


#SENTIMENT ANALYSIS

library(curl)

df <- as.data.frame(corpusai$Text)
colnames(df) <- c("text")
df <- tibble(txt = df$text)

sentiments <- function(df) {
  # Read the data
  bing <- read_csv(curl("https://raw.githubusercontent.com/laurence001/mixology/main/compared/bing.csv"))
  inquire <- read_csv(curl("https://raw.githubusercontent.com/laurence001/mixology/main/compared/inquire.csv"))
  subjectivity <- read_csv(curl("https://raw.githubusercontent.com/laurence001/mixology/main/compared/subjectivity.csv"))
  loughran <- read_csv(curl("https://raw.githubusercontent.com/laurence001/mixology/main/compared/loughran.csv"))
  nrc <- read_csv(curl("https://raw.githubusercontent.com/laurence001/mixology/main/compared/nrc.csv"))
  stop <- read_csv(curl("https://raw.githubusercontent.com/laurence001/mixology/main/compared/stopwords.csv"))
  
  text_df <- df %>%
    unnest_tokens(word, txt)
  
  sentiment_bing <- text_df %>%
    anti_join(stop, by = "word") %>%
    inner_join(bing, by = "word")
  total_bing <- nrow(sentiment_bing)
  colnames(sentiment_bing) <- c("word", "sentiment")
  bing_counts <- sentiment_bing %>%
    count(sentiment) %>%
    mutate(pc = n / total_bing * 100)
  
  sentiment_inquire <- text_df %>%
    anti_join (stop, by ="word") %>%
    inner_join(inquire, by = "word")
  total_inquire <- nrow(sentiment_inquire)
  colnames(sentiment_inquire) <- c("word","sentiment")
  inquire_counts <- sentiment_inquire %>%
    count(sentiment) %>%
    mutate(pc = n/total_inquire*100)
  
  sentiment_subjectivity <- text_df %>%
    anti_join (stop, by ="word") %>%
    inner_join(subjectivity, by = "word")
  total_subjectivity <- nrow(sentiment_subjectivity)
  colnames(sentiment_subjectivity) <- c("word","sentiment")
  subjectivity_counts <- sentiment_subjectivity %>%
    count(sentiment) %>%
    mutate(pc = n/total_subjectivity*100)
  
  sentiment_loughran <- text_df %>%
    anti_join (stop, by ="word") %>%
    inner_join(loughran, by = "word")
  total_loughran <- nrow(sentiment_loughran)
  colnames(sentiment_loughran) <- c("word","sentiment")
  loughran_counts <- sentiment_loughran %>%
    count(sentiment) %>%
    mutate(pc = n/total_loughran*100)
  
  sentiment_nrc <- text_df %>%
    anti_join (stop, by ="word") %>%
    inner_join(nrc, by = "word")
  total_nrc <- nrow(sentiment_nrc)
  colnames(sentiment_nrc) <- c("word","sentiment")
  nrc_counts <- sentiment_nrc %>%
    count(sentiment) %>%
    mutate(pc = n/total_nrc*100)
  
  subjectivity_counts <- subset(subjectivity_counts, sentiment %in% c("negative", "positive"))
  loughran_counts <- subset(loughran_counts, sentiment %in% c("negative", "positive"))
  nrc_counts <- subset(nrc_counts, sentiment %in% c("negative", "positive"))
  
  lexicon <- c("Bing", "Bing", "Inquire", "Inquire", "Subjectivity", "Subjectivity", "Loughran", "Loughran", "NRC", "NRC")
  compared <- rbind(bing_counts, inquire_counts, subjectivity_counts, loughran_counts, nrc_counts)
  finaldf <- cbind(lexicon, compared)
  finaldf$pc <- round(finaldf$pc, 2)
  
  # Plot the ggplot chart
  greyscale_colors <- c("#747474", "#818181", "#B0B0B0", "#C5C5C5", "#E6E6E7", "#F7F7F8")
  
  ggplot_chart <- ggplot(finaldf, aes(x = sentiment, y = pc, fill = lexicon)) +
    geom_col(position = position_dodge(width = 0.9)) +  # Adjust width to your preference
    scale_fill_manual(values = greyscale_colors, name = NULL) +
    labs(title = " ", x = " ", y = "Percentage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "bottom",
          legend.box = "horizontal")
  
  # Return the finaldf and the ggplot chart
  return(list(finaldf = finaldf, ggplot_chart = ggplot_chart, sentiment_bing = sentiment_bing))
}
df23 <- tail(df,24)
result <- sentiments(df)
result23 <- sentiments(df23)
finaldf <- result$finaldf
finaldf
finaldf23 <- result23$finaldf
finaldf23

finaldf23$sentiment <- as.factor(finaldf23$sentiment)

neg <- finaldf23  %>%
  select(sentiment, pc) %>%
  filter (sentiment == "negative")
neg
summary(neg)


bing <- result23$sentiment_bing
bing
ggplot_chart <- result$ggplot_chart
ggplot_chart

ggplot_chart23 <- result23$ggplot_chart
ggplot_chart23

#TOPIC MODELING

library(tm)
library(topicmodels)

corpus <- Corpus(VectorSource(corpusai$Text))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

DTM <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(5, 50))))

dtm_df <- as.data.frame(as.matrix(DTM))

dtm_df$Term <- rownames(dtm_df)

dtm_long <- gather(dtm_df, Topic, Frequency, -Term)

ggplot(dtm_long, aes(x = Topic, y = Frequency, fill = Term)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal") +
  labs(title = "Document-Term Matrix",
       x = "Topic",
       y = "Frequency")

#LDA 

k1 <- 9
set.seed(123)
topicModel1 <- LDA(DTM, k1, method = "Gibbs", control = list(iter = 1000, verbose = 25))

# Convert the LDA topic model to a data frame
topic_df <- tidy(topicModel1, matrix = "beta")

# Add the term frequencies to the data frame
term_freq <- rowSums(as.matrix(DTM))
topic_df$Term_Frequency <- term_freq[match(topic_df$term, rownames(term_freq))]

N <- 100
top_terms <- topic_df[order(-topic_df$Term_Frequency), ][1:N, ]

ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal") +
  labs(title = "LDA Topic Model - Top 10 Terms by Frequency",
       x = "Term",
       y = "Beta (Topic-Term Probability)",
       fill = "Topic")

# Get the topic probabilities for each document
topic_probs <- as.data.frame(topics(topicModel1))

topic_probs$Document <- rownames(topic_probs)

topic_probs_long <- pivot_longer(topic_probs, cols = starts_with("topic"), names_to = "Topic", values_to = "Probability")

ggplot(topic_probs_long, aes(x = Document, y = Probability, fill = factor(Topic))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal") +
  labs(title = "LDA Topic Model - Topic Probabilities for Each Document",
       x = "Document",
       y = "Probability",
       fill = "Topic")

tidy_topic_model1 <- tidy(topicModel1, matrix = "beta")

top_terms_df <- tidy_topic_model1 %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms_df, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = ifelse(beta >= 0.05, term, "")), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  labs(title = "LDA Topic Model - Top 15 Terms for Each Topic",
       x = "Term",
       y = "Probability") +
  coord_flip() +
  facet_wrap(~ topic, scales = "free", ncol = 3)


k2 <- 4
set.seed(1502)
topicModel2 <- LDA(DTM, k2, control = list(seed = 1502))

tidy_topic_model2 <- tidy(topicModel2, matrix = "beta")

top_terms_df <- tidy_topic_model2 %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms_df, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = ifelse(beta >= 0.05, term, "")), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  labs(title = "LDA Topic Model - Top 25 Terms for Each Topic",
       x = "Term",
       y = "Probability") +
  coord_flip() +
  facet_wrap(~ topic, scales = "free", ncol = 3)


k3 <- 4
topicModel3 <- CTM(DTM, k3, method = "VEM")

tidy_topic_model3 <- tidy(topicModel3, matrix = "beta")

top_terms_df <- tidy_topic_model3 %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms_df, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = ifelse(beta >= 0.05, term, "")), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  labs(title = "CTM Topic Model - Top 15 Terms for Each Topic",
       x = "Term",
       y = "Probability") +
  coord_flip() +
  facet_wrap(~ topic, scales = "free", ncol = 3)

#TABLE

tableethics <- data.frame(ethics$Year,ethics$Organisation,ethics$Country,ethics$Text)

colnames(tableethics) <- c("Year","Organisation","Country","Text")

datatable(tableethics)
