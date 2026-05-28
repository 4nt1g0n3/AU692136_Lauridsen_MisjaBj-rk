## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(pdftools)
library(tidytext)
library(textdata) 
library(ggwordcloud)
library(dplyr)
library(tibble)
library(Sentida)

# Access Sentida lexicon
sentida_env <- asNamespace("Sentida")
sentida_lexicon <- get("aarup", envir = sentida_env)

## ----anker80--------------------------------------------------------------------------------
#Importation of data
anker80_path <- here::here("data", "anker80.pdf")
#Extraction of text from PDF
anker80_text <- pdf_text(anker80_path)

## ----clean_anker80--------------------------------------------------------------------------
# Split speech text into individual lines and remove whitespace
anker80_df <- data.frame(anker80_text)%>%
 mutate(text_full=str_split(anker80_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))

## ----tokenise_anker80-----------------------------------------------------------------------
# Break speech text into individual word tokens
 anker80_tokens <- anker80_df %>% 
     unnest_tokens(word, text_full)

## ----remove-stopwords-----------------------------------------------------------------------
# Remove common stopwords that do not carry semantic meaning
anker80_wc <- anker80_tokens %>% 
     count(word) %>% 
     arrange(-n)
anker80_wc
anker80_stop <- anker80_tokens %>% 
  anti_join(stop_words) %>% 
  select(-anker80_text)

## ----remove-numbers-------------------------------------------------------------------------
anker80_swc <- anker80_stop %>% 
     count(word) %>% 
     arrange(-n)
anker80_no_numeric <- anker80_stop %>% 
  filter(is.na(as.numeric(word)))

## ----sentiment-analysis---------------------------------------------------------------------
# Match speech words with Sentida sentiment scores
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
anker80_Sentida <- anker80_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
anker80_Sentida

## ----sentiment-histogram--------------------------------------------------------------------
# Count how many words belong to each sentiment category
anker80_Sentida_hist <- anker80_Sentida %>% 
  count(score)

## ----mean-sentiment-------------------------------------------------------------------------
# Calculate average sentiment score for the speech
anker80_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.515

## ----visualise_sentiment--------------------------------------------------------------------
# Visualise sentiment distribution
ggplot(data = anker80_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----baunsgaard68---------------------------------------------------------------------------
baunsgaard68_path <- here::here("data", "baunsgaard68.pdf")
baunsgaard68_text <- pdf_text(baunsgaard68_path)
baunsgaard68_df <- data.frame(baunsgaard68_text)%>%
 mutate(text_full=str_split(baunsgaard68_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 baunsgaard68_tokens <- baunsgaard68_df %>% 
     unnest_tokens(word, text_full)
baunsgaard68_wc <- baunsgaard68_tokens %>% 
     count(word) %>% 
     arrange(-n)
baunsgaard68_wc
baunsgaard68_stop <- baunsgaard68_tokens %>% 
  anti_join(stop_words) %>% 
  select(-baunsgaard68_text)
baunsgaard68_swc <- baunsgaard68_stop %>% 
     count(word) %>% 
     arrange(-n)
baunsgaard68_no_numeric <- baunsgaard68_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
baunsgaard68_Sentida <- baunsgaard68_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
baunsgaard68_Sentida
baunsgaard68_Sentida_hist <- baunsgaard68_Sentida %>% 
  count(score)
baunsgaard68_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.469
ggplot(data = baunsgaard68_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----baunsgaard69---------------------------------------------------------------------------
baunsgaard69_path <- here::here("data", "baunsgaard69.pdf")
baunsgaard69_text <- pdf_text(baunsgaard69_path)
baunsgaard69_df <- data.frame(baunsgaard69_text)%>%
 mutate(text_full=str_split(baunsgaard69_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 baunsgaard69_tokens <- baunsgaard69_df %>% 
     unnest_tokens(word, text_full)
baunsgaard69_wc <- baunsgaard69_tokens %>% 
     count(word) %>% 
     arrange(-n)
baunsgaard69_wc
baunsgaard69_stop <- baunsgaard69_tokens %>% 
  anti_join(stop_words) %>% 
  select(-baunsgaard69_text)
baunsgaard69_swc <- baunsgaard69_stop %>% 
     count(word) %>% 
     arrange(-n)
baunsgaard69_no_numeric <- baunsgaard69_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
baunsgaard69_Sentida <- baunsgaard69_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
baunsgaard69_Sentida
baunsgaard69_Sentida_hist <- baunsgaard69_Sentida %>% 
  count(score)
baunsgaard69_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.484
ggplot(data = baunsgaard69_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----baunsgaard70---------------------------------------------------------------------------
baunsgaard70_path <- here::here("data", "baunsgaard70.pdf")
baunsgaard70_text <- pdf_text(baunsgaard70_path)
baunsgaard70_df <- data.frame(baunsgaard70_text)%>%
 mutate(text_full=str_split(baunsgaard70_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 baunsgaard70_tokens <- baunsgaard70_df %>% 
     unnest_tokens(word, text_full)
baunsgaard70_wc <- baunsgaard70_tokens %>% 
     count(word) %>% 
     arrange(-n)
baunsgaard70_wc
baunsgaard70_stop <- baunsgaard70_tokens %>% 
  anti_join(stop_words) %>% 
  select(-baunsgaard70_text)
baunsgaard70_swc <- baunsgaard70_stop %>% 
     count(word) %>% 
     arrange(-n)
baunsgaard70_no_numeric <- baunsgaard70_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
baunsgaard70_Sentida <- baunsgaard70_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
baunsgaard70_Sentida
baunsgaard70_Sentida_hist <- baunsgaard70_Sentida %>% 
  count(score)
baunsgaard70_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.389
ggplot(data = baunsgaard70_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----baunsgaard71---------------------------------------------------------------------------
baunsgaard71_path <- here::here("data", "baunsgaard71.pdf")
baunsgaard71_text <- pdf_text(baunsgaard71_path)
baunsgaard71_df <- data.frame(baunsgaard71_text)%>%
 mutate(text_full=str_split(baunsgaard71_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 baunsgaard71_tokens <- baunsgaard71_df %>% 
     unnest_tokens(word, text_full)
baunsgaard71_wc <- baunsgaard71_tokens %>% 
     count(word) %>% 
     arrange(-n)
baunsgaard71_wc
baunsgaard71_stop <- baunsgaard71_tokens %>% 
  anti_join(stop_words) %>% 
  select(-baunsgaard71_text)
baunsgaard71_swc <- baunsgaard71_stop %>% 
     count(word) %>% 
     arrange(-n)
baunsgaard71_no_numeric <- baunsgaard71_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
baunsgaard71_Sentida <- baunsgaard71_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
baunsgaard71_Sentida
baunsgaard71_Sentida_hist <- baunsgaard71_Sentida %>% 
  count(score)
baunsgaard71_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.469
ggplot(data = baunsgaard71_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----fogh07---------------------------------------------------------------------------------
fogh07_path <- here::here("data", "fogh07.pdf")
fogh07_text <- pdf_text(fogh07_path)
fogh07_df <- data.frame(fogh07_text)%>%
 mutate(text_full=str_split(fogh07_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 fogh07_tokens <- fogh07_df %>% 
     unnest_tokens(word, text_full)
fogh07_wc <- fogh07_tokens %>% 
     count(word) %>% 
     arrange(-n)
fogh07_wc
fogh07_stop <- fogh07_tokens %>% 
  anti_join(stop_words) %>% 
  select(-fogh07_text)
fogh07_swc <- fogh07_stop %>% 
     count(word) %>% 
     arrange(-n)
fogh07_no_numeric <- fogh07_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
fogh07_Sentida <- fogh07_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
fogh07_Sentida
fogh07_Sentida_hist <- fogh07_Sentida %>% 
  count(score)
fogh07_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.454
ggplot(data = fogh07_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----frederiksen19--------------------------------------------------------------------------
frederiksen19_path <- here::here("data", "frederiksen19.pdf")
frederiksen19_text <- pdf_text(frederiksen19_path)
frederiksen19_df <- data.frame(frederiksen19_text)%>%
 mutate(text_full=str_split(frederiksen19_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 frederiksen19_tokens <- frederiksen19_df %>% 
     unnest_tokens(word, text_full)
frederiksen19_wc <- frederiksen19_tokens %>% 
     count(word) %>% 
     arrange(-n)
frederiksen19_wc
frederiksen19_stop <- frederiksen19_tokens %>% 
  anti_join(stop_words) %>% 
  select(-frederiksen19_text)
frederiksen19_swc <- frederiksen19_stop %>% 
     count(word) %>% 
     arrange(-n)
frederiksen19_no_numeric <- frederiksen19_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
frederiksen19_Sentida <- frederiksen19_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
frederiksen19_Sentida
frederiksen19_Sentida_hist <- frederiksen19_Sentida %>% 
  count(score)
frederiksen19_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.545
ggplot(data = frederiksen19_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----frederiksen20--------------------------------------------------------------------------
frederiksen20_path <- here::here("data", "frederiksen20.pdf")
frederiksen20_text <- pdf_text(frederiksen20_path)
frederiksen20_df <- data.frame(frederiksen20_text)%>%
 mutate(text_full=str_split(frederiksen20_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 frederiksen20_tokens <- frederiksen20_df %>% 
     unnest_tokens(word, text_full)
frederiksen20_wc <- frederiksen20_tokens %>% 
     count(word) %>% 
     arrange(-n)
frederiksen20_wc
frederiksen20_stop <- frederiksen20_tokens %>% 
  anti_join(stop_words) %>% 
  select(-frederiksen20_text)
frederiksen20_swc <- frederiksen20_stop %>% 
     count(word) %>% 
     arrange(-n)
frederiksen20_no_numeric <- frederiksen20_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
frederiksen20_Sentida <- frederiksen20_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
frederiksen20_Sentida
frederiksen20_Sentida_hist <- frederiksen20_Sentida %>% 
  count(score)
frederiksen20_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.447
ggplot(data = frederiksen20_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----frederiksen21--------------------------------------------------------------------------
frederiksen21_path <- here::here("data", "frederiksen21.pdf")
frederiksen21_text <- pdf_text(frederiksen21_path)
frederiksen21_df <- data.frame(frederiksen21_text)%>%
 mutate(text_full=str_split(frederiksen21_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 frederiksen21_tokens <- frederiksen21_df %>% 
     unnest_tokens(word, text_full)
frederiksen21_wc <- frederiksen21_tokens %>% 
     count(word) %>% 
     arrange(-n)
frederiksen21_wc
frederiksen21_stop <- frederiksen21_tokens %>% 
  anti_join(stop_words) %>% 
  select(-frederiksen21_text)
frederiksen21_swc <- frederiksen21_stop %>% 
     count(word) %>% 
     arrange(-n)
frederiksen21_no_numeric <- frederiksen21_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
frederiksen21_Sentida <- frederiksen21_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
frederiksen21_Sentida
frederiksen21_Sentida_hist <- frederiksen21_Sentida %>% 
  count(score)
frederiksen21_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.683
ggplot(data = frederiksen21_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----hartling74-----------------------------------------------------------------------------
hartling74_path <- here::here("data", "hartling74.pdf")
hartling74_text <- pdf_text(hartling74_path)
hartling74_df <- data.frame(hartling74_text)%>%
 mutate(text_full=str_split(hartling74_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 hartling74_tokens <- hartling74_df %>% 
     unnest_tokens(word, text_full)
hartling74_wc <- hartling74_tokens %>% 
     count(word) %>% 
     arrange(-n)
hartling74_wc
hartling74_stop <- hartling74_tokens %>% 
  anti_join(stop_words) %>% 
  select(-hartling74_text)
hartling74_swc <- hartling74_stop %>% 
     count(word) %>% 
     arrange(-n)
hartling74_no_numeric <- hartling74_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
hartling74_Sentida <- hartling74_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
hartling74_Sentida
hartling74_Sentida_hist <- hartling74_Sentida %>% 
  count(score)
hartling74_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.518
ggplot(data = hartling74_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----løkke09--------------------------------------------------------------------------------
løkke09_path <- here::here("data", "løkke09.pdf")
løkke09_text <- pdf_text(løkke09_path)
løkke09_df <- data.frame(løkke09_text)%>%
 mutate(text_full=str_split(løkke09_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 løkke09_tokens <- løkke09_df %>% 
     unnest_tokens(word, text_full)
løkke09_wc <- løkke09_tokens %>% 
     count(word) %>% 
     arrange(-n)
løkke09_wc
løkke09_stop <- løkke09_tokens %>% 
  anti_join(stop_words) %>% 
  select(-løkke09_text)
løkke09_swc <- løkke09_stop %>% 
     count(word) %>% 
     arrange(-n)
løkke09_no_numeric <- løkke09_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
løkke09_Sentida <- løkke09_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
løkke09_Sentida
løkke09_Sentida_hist <- løkke09_Sentida %>% 
  count(score)
løkke09_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.634
ggplot(data = løkke09_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----løkke10--------------------------------------------------------------------------------
løkke10_path <- here::here("data", "løkke10.pdf")
løkke10_text <- pdf_text(løkke10_path)
løkke10_df <- data.frame(løkke10_text)%>%
 mutate(text_full=str_split(løkke10_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 løkke10_tokens <- løkke10_df %>% 
     unnest_tokens(word, text_full)
løkke10_wc <- løkke10_tokens %>% 
     count(word) %>% 
     arrange(-n)
løkke10_wc
løkke10_stop <- løkke10_tokens %>% 
  anti_join(stop_words) %>% 
  select(-løkke10_text)
løkke10_swc <- løkke10_stop %>% 
     count(word) %>% 
     arrange(-n)
løkke10_no_numeric <- løkke10_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
løkke10_Sentida <- løkke10_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
løkke10_Sentida
løkke10_Sentida_hist <- løkke10_Sentida %>% 
  count(score)
løkke10_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.675
ggplot(data = løkke10_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----løkke15--------------------------------------------------------------------------------
løkke15_path <- here::here("data", "løkke15.pdf")
løkke15_text <- pdf_text(løkke15_path)
løkke15_df <- data.frame(løkke15_text)%>%
 mutate(text_full=str_split(løkke15_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 løkke15_tokens <- løkke15_df %>% 
     unnest_tokens(word, text_full)
løkke15_wc <- løkke15_tokens %>% 
     count(word) %>% 
     arrange(-n)
løkke15_wc
løkke15_stop <- løkke15_tokens %>% 
  anti_join(stop_words) %>% 
  select(-løkke15_text)
løkke15_swc <- løkke15_stop %>% 
     count(word) %>% 
     arrange(-n)
løkke15_no_numeric <- løkke15_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
løkke15_Sentida <- løkke15_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
løkke15_Sentida
løkke15_Sentida_hist <- løkke15_Sentida %>% 
  count(score)
løkke15_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.511
ggplot(data = løkke15_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----løkke16--------------------------------------------------------------------------------
løkke16_path <- here::here("data", "løkke16.pdf")
løkke16_text <- pdf_text(løkke16_path)
løkke16_df <- data.frame(løkke16_text)%>%
 mutate(text_full=str_split(løkke16_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 løkke16_tokens <- løkke16_df %>% 
     unnest_tokens(word, text_full)
løkke16_wc <- løkke16_tokens %>% 
     count(word) %>% 
     arrange(-n)
løkke16_wc
løkke16_stop <- løkke16_tokens %>% 
  anti_join(stop_words) %>% 
  select(-løkke16_text)
løkke16_swc <- løkke16_stop %>% 
     count(word) %>% 
     arrange(-n)
løkke16_no_numeric <- løkke16_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
løkke16_Sentida <- løkke16_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
løkke16_Sentida
løkke16_Sentida_hist <- løkke16_Sentida %>% 
  count(score)
løkke16_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.613
ggplot(data = løkke16_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----løkke17--------------------------------------------------------------------------------
løkke17_path <- here::here("data", "løkke17.pdf")
løkke17_text <- pdf_text(løkke17_path)
løkke17_df <- data.frame(løkke17_text)%>%
 mutate(text_full=str_split(løkke17_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 løkke17_tokens <- løkke17_df %>% 
     unnest_tokens(word, text_full)
løkke17_wc <- løkke17_tokens %>% 
     count(word) %>% 
     arrange(-n)
løkke17_wc
løkke17_stop <- løkke17_tokens %>% 
  anti_join(stop_words) %>% 
  select(-løkke17_text)
løkke17_swc <- løkke17_stop %>% 
     count(word) %>% 
     arrange(-n)
løkke17_no_numeric <- løkke17_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
løkke17_Sentida <- løkke17_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
løkke17_Sentida
løkke17_Sentida_hist <- løkke17_Sentida %>% 
  count(score)
løkke17_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.706
ggplot(data = løkke17_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----løkke18--------------------------------------------------------------------------------
løkke18_path <- here::here("data", "løkke18.pdf")
løkke18_text <- pdf_text(løkke18_path)
løkke18_df <- data.frame(løkke18_text)%>%
 mutate(text_full=str_split(løkke18_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 løkke18_tokens <- løkke18_df %>% 
     unnest_tokens(word, text_full)
løkke18_wc <- løkke18_tokens %>% 
     count(word) %>% 
     arrange(-n)
løkke18_wc
løkke18_stop <- løkke18_tokens %>% 
  anti_join(stop_words) %>% 
  select(-løkke18_text)
løkke18_swc <- løkke18_stop %>% 
     count(word) %>% 
     arrange(-n)
løkke18_no_numeric <- løkke18_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
løkke18_Sentida <- løkke18_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
løkke18_Sentida
løkke18_Sentida_hist <- løkke18_Sentida %>% 
  count(score)
løkke18_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.660
ggplot(data = løkke18_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup00--------------------------------------------------------------------------------
nyrup00_path <- here::here("data", "nyrup00.pdf")
nyrup00_text <- pdf_text(nyrup00_path)
nyrup00_df <- data.frame(nyrup00_text)%>%
 mutate(text_full=str_split(nyrup00_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup00_tokens <- nyrup00_df %>% 
     unnest_tokens(word, text_full)
nyrup00_wc <- nyrup00_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup00_wc
nyrup00_stop <- nyrup00_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup00_text)
nyrup00_swc <- nyrup00_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup00_no_numeric <- nyrup00_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup00_Sentida <- nyrup00_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup00_Sentida
nyrup00_Sentida_hist <- nyrup00_Sentida %>% 
  count(score)
nyrup00_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.624
ggplot(data = nyrup00_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup01--------------------------------------------------------------------------------
nyrup01_path <- here::here("data", "nyrup01.pdf")
nyrup01_text <- pdf_text(nyrup01_path)
nyrup01_df <- data.frame(nyrup01_text)%>%
 mutate(text_full=str_split(nyrup01_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup01_tokens <- nyrup01_df %>% 
     unnest_tokens(word, text_full)
nyrup01_wc <- nyrup01_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup01_wc
nyrup01_stop <- nyrup01_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup01_text)
nyrup01_swc <- nyrup01_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup01_no_numeric <- nyrup01_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup01_Sentida <- nyrup01_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup01_Sentida
nyrup01_Sentida_hist <- nyrup01_Sentida %>% 
  count(score)
nyrup01_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.363
ggplot(data = nyrup01_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup93--------------------------------------------------------------------------------
nyrup93_path <- here::here("data", "nyrup93.pdf")
nyrup93_text <- pdf_text(nyrup93_path)
nyrup93_df <- data.frame(nyrup93_text)%>%
 mutate(text_full=str_split(nyrup93_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup93_tokens <- nyrup93_df %>% 
     unnest_tokens(word, text_full)
nyrup93_wc <- nyrup93_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup93_wc
nyrup93_stop <- nyrup93_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup93_text)
nyrup93_swc <- nyrup93_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup93_no_numeric <- nyrup93_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup93_Sentida <- nyrup93_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup93_Sentida
nyrup93_Sentida_hist <- nyrup93_Sentida %>% 
  count(score)
nyrup93_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.537
ggplot(data = nyrup93_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup94--------------------------------------------------------------------------------
nyrup94_path <- here::here("data", "nyrup94.pdf")
nyrup94_text <- pdf_text(nyrup94_path)
nyrup94_df <- data.frame(nyrup94_text)%>%
 mutate(text_full=str_split(nyrup94_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup94_tokens <- nyrup94_df %>% 
     unnest_tokens(word, text_full)
nyrup94_wc <- nyrup94_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup94_wc
nyrup94_stop <- nyrup94_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup94_text)
nyrup94_swc <- nyrup94_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup94_no_numeric <- nyrup94_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup94_Sentida <- nyrup94_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup94_Sentida
nyrup94_Sentida_hist <- nyrup94_Sentida %>% 
  count(score)
nyrup94_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.546
ggplot(data = nyrup94_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup95--------------------------------------------------------------------------------
nyrup95_path <- here::here("data", "nyrup95.pdf")
nyrup95_text <- pdf_text(nyrup95_path)
nyrup95_df <- data.frame(nyrup95_text)%>%
 mutate(text_full=str_split(nyrup95_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup95_tokens <- nyrup95_df %>% 
     unnest_tokens(word, text_full)
nyrup95_wc <- nyrup95_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup95_wc
nyrup95_stop <- nyrup95_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup95_text)
nyrup95_swc <- nyrup95_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup95_no_numeric <- nyrup95_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup95_Sentida <- nyrup95_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup95_Sentida
nyrup95_Sentida_hist <- nyrup95_Sentida %>% 
  count(score)
nyrup95_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.548
ggplot(data = nyrup95_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup96--------------------------------------------------------------------------------
nyrup96_path <- here::here("data", "nyrup96.pdf")
nyrup96_text <- pdf_text(nyrup96_path)
nyrup96_df <- data.frame(nyrup96_text)%>%
 mutate(text_full=str_split(nyrup96_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup96_tokens <- nyrup96_df %>% 
     unnest_tokens(word, text_full)
nyrup96_wc <- nyrup96_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup96_wc
nyrup96_stop <- nyrup96_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup96_text)
nyrup96_swc <- nyrup96_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup96_no_numeric <- nyrup96_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup96_Sentida <- nyrup96_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup96_Sentida
nyrup96_Sentida_hist <- nyrup96_Sentida %>% 
  count(score)
nyrup96_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.561
ggplot(data = nyrup96_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup97--------------------------------------------------------------------------------
nyrup97_path <- here::here("data", "nyrup97.pdf")
nyrup97_text <- pdf_text(nyrup97_path)
nyrup97_df <- data.frame(nyrup97_text)%>%
 mutate(text_full=str_split(nyrup97_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup97_tokens <- nyrup97_df %>% 
     unnest_tokens(word, text_full)
nyrup97_wc <- nyrup97_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup97_wc
nyrup97_stop <- nyrup97_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup97_text)
nyrup97_swc <- nyrup97_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup97_no_numeric <- nyrup97_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup97_Sentida <- nyrup97_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup97_Sentida
nyrup97_Sentida_hist <- nyrup97_Sentida %>% 
  count(score)
nyrup97_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.614
ggplot(data = nyrup97_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup98--------------------------------------------------------------------------------
nyrup98_path <- here::here("data", "nyrup98.pdf")
nyrup98_text <- pdf_text(nyrup98_path)
nyrup98_df <- data.frame(nyrup98_text)%>%
 mutate(text_full=str_split(nyrup98_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup98_tokens <- nyrup98_df %>% 
     unnest_tokens(word, text_full)
nyrup98_wc <- nyrup98_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup98_wc
nyrup98_stop <- nyrup98_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup98_text)
nyrup98_swc <- nyrup98_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup98_no_numeric <- nyrup98_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup98_Sentida <- nyrup98_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup98_Sentida
nyrup98_Sentida_hist <- nyrup98_Sentida %>% 
  count(score)
nyrup98_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.581
ggplot(data = nyrup98_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----nyrup99--------------------------------------------------------------------------------
nyrup99_path <- here::here("data", "nyrup99.pdf")
nyrup99_text <- pdf_text(nyrup99_path)
nyrup99_df <- data.frame(nyrup99_text)%>%
 mutate(text_full=str_split(nyrup99_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 nyrup99_tokens <- nyrup99_df %>% 
     unnest_tokens(word, text_full)
nyrup99_wc <- nyrup99_tokens %>% 
     count(word) %>% 
     arrange(-n)
nyrup99_wc
nyrup99_stop <- nyrup99_tokens %>% 
  anti_join(stop_words) %>% 
  select(-nyrup99_text)
nyrup99_swc <- nyrup99_stop %>% 
     count(word) %>% 
     arrange(-n)
nyrup99_no_numeric <- nyrup99_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
nyrup99_Sentida <- nyrup99_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
nyrup99_Sentida
nyrup99_Sentida_hist <- nyrup99_Sentida %>% 
  count(score)
nyrup99_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.326
ggplot(data = nyrup99_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter82-----------------------------------------------------------------------------
schluter82_path <- here::here("data", "schluter82.pdf")
schluter82_text <- pdf_text(schluter82_path)
schluter82_df <- data.frame(schluter82_text)%>%
 mutate(text_full=str_split(schluter82_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter82_tokens <- schluter82_df %>% 
     unnest_tokens(word, text_full)
schluter82_wc <- schluter82_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter82_wc
schluter82_stop <- schluter82_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter82_text)
schluter82_swc <- schluter82_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter82_no_numeric <- schluter82_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter82_Sentida <- schluter82_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter82_Sentida
schluter82_Sentida_hist <- schluter82_Sentida %>% 
  count(score)
schluter82_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.481
ggplot(data = schluter82_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter83-----------------------------------------------------------------------------
schluter83_path <- here::here("data", "schluter83.pdf")
schluter83_text <- pdf_text(schluter83_path)
schluter83_df <- data.frame(schluter83_text)%>%
 mutate(text_full=str_split(schluter83_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter83_tokens <- schluter83_df %>% 
     unnest_tokens(word, text_full)
schluter83_wc <- schluter83_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter83_wc
schluter83_stop <- schluter83_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter83_text)
schluter83_swc <- schluter83_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter83_no_numeric <- schluter83_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter83_Sentida <- schluter83_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter83_Sentida
schluter83_Sentida_hist <- schluter83_Sentida %>% 
  count(score)
schluter83_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.692
ggplot(data = schluter83_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter84-----------------------------------------------------------------------------
schluter84_path <- here::here("data", "schluter84.pdf")
schluter84_text <- pdf_text(schluter84_path)
schluter84_df <- data.frame(schluter84_text)%>%
 mutate(text_full=str_split(schluter84_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter84_tokens <- schluter84_df %>% 
     unnest_tokens(word, text_full)
schluter84_wc <- schluter84_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter84_wc
schluter84_stop <- schluter84_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter84_text)
schluter84_swc <- schluter84_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter84_no_numeric <- schluter84_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter84_Sentida <- schluter84_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter84_Sentida
schluter84_Sentida_hist <- schluter84_Sentida %>% 
  count(score)
schluter84_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean of 0.525
ggplot(data = schluter84_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter85-----------------------------------------------------------------------------
schluter85_path <- here::here("data", "schluter85.pdf")
schluter85_text <- pdf_text(schluter85_path)
schluter85_df <- data.frame(schluter85_text)%>%
 mutate(text_full=str_split(schluter85_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter85_tokens <- schluter85_df %>% 
     unnest_tokens(word, text_full)
schluter85_wc <- schluter85_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter85_wc
schluter85_stop <- schluter85_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter85_text)
schluter85_swc <- schluter85_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter85_no_numeric <- schluter85_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter85_Sentida <- schluter85_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter85_Sentida
schluter85_Sentida_hist <- schluter85_Sentida %>% 
  count(score)
schluter85_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean of 0.501
ggplot(data = schluter85_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter86-----------------------------------------------------------------------------
schluter86_path <- here::here("data", "schluter86.pdf")
schluter86_text <- pdf_text(schluter86_path)
schluter86_df <- data.frame(schluter86_text)%>%
 mutate(text_full=str_split(schluter86_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter86_tokens <- schluter86_df %>% 
     unnest_tokens(word, text_full)
schluter86_wc <- schluter86_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter86_wc
schluter86_stop <- schluter86_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter86_text)
schluter86_swc <- schluter86_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter86_no_numeric <- schluter86_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter86_Sentida <- schluter86_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter86_Sentida
schluter86_Sentida_hist <- schluter86_Sentida %>% 
  count(score)
schluter86_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean of 0.541
ggplot(data = schluter86_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter87-----------------------------------------------------------------------------
schluter87_path <- here::here("data", "schluter87.pdf")
schluter87_text <- pdf_text(schluter87_path)
schluter87_df <- data.frame(schluter87_text)%>%
 mutate(text_full=str_split(schluter87_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter87_tokens <- schluter87_df %>% 
     unnest_tokens(word, text_full)
schluter87_wc <- schluter87_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter87_wc
schluter87_stop <- schluter87_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter87_text)
schluter87_swc <- schluter87_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter87_no_numeric <- schluter87_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter87_Sentida <- schluter87_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter87_Sentida
schluter87_Sentida_hist <- schluter87_Sentida %>% 
  count(score)
schluter87_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.690
ggplot(data = schluter87_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter88-----------------------------------------------------------------------------
schluter88_path <- here::here("data", "schluter88.pdf")
schluter88_text <- pdf_text(schluter88_path)
schluter88_df <- data.frame(schluter88_text)%>%
 mutate(text_full=str_split(schluter88_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter88_tokens <- schluter88_df %>% 
     unnest_tokens(word, text_full)
schluter88_wc <- schluter88_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter88_wc
schluter88_stop <- schluter88_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter88_text)
schluter88_swc <- schluter88_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter88_no_numeric <- schluter88_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter88_Sentida <- schluter88_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter88_Sentida
schluter88_Sentida_hist <- schluter88_Sentida %>% 
  count(score)
schluter88_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.537
ggplot(data = schluter88_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter89-----------------------------------------------------------------------------
schluter89_path <- here::here("data", "schluter89.pdf")
schluter89_text <- pdf_text(schluter89_path)
schluter89_df <- data.frame(schluter89_text)%>%
 mutate(text_full=str_split(schluter89_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter89_tokens <- schluter89_df %>% 
     unnest_tokens(word, text_full)
schluter89_wc <- schluter89_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter89_wc
schluter89_stop <- schluter89_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter89_text)
schluter89_swc <- schluter89_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter89_no_numeric <- schluter89_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter89_Sentida <- schluter89_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter89_Sentida
schluter89_Sentida_hist <- schluter89_Sentida %>% 
  count(score)
schluter89_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.714
ggplot(data = schluter89_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter90-----------------------------------------------------------------------------
schluter90_path <- here::here("data", "schluter90.pdf")
schluter90_text <- pdf_text(schluter90_path)
schluter90_df <- data.frame(schluter90_text)%>%
 mutate(text_full=str_split(schluter90_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter90_tokens <- schluter90_df %>% 
     unnest_tokens(word, text_full)
schluter90_wc <- schluter90_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter90_wc
schluter90_stop <- schluter90_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter90_text)
schluter90_swc <- schluter90_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter90_no_numeric <- schluter90_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter90_Sentida <- schluter90_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter90_Sentida
schluter90_Sentida_hist <- schluter90_Sentida %>% 
  count(score)
schluter90_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.687
ggplot(data = schluter90_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter91-----------------------------------------------------------------------------
schluter91_path <- here::here("data", "schluter91.pdf")
schluter91_text <- pdf_text(schluter91_path)
schluter91_df <- data.frame(schluter91_text)%>%
 mutate(text_full=str_split(schluter91_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter91_tokens <- schluter91_df %>% 
     unnest_tokens(word, text_full)
schluter91_wc <- schluter91_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter91_wc
schluter91_stop <- schluter91_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter91_text)
schluter91_swc <- schluter91_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter91_no_numeric <- schluter91_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter91_Sentida <- schluter91_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter91_Sentida
schluter91_Sentida_hist <- schluter91_Sentida %>% 
  count(score)
schluter91_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.641
ggplot(data = schluter91_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----schluter92-----------------------------------------------------------------------------
schluter92_path <- here::here("data", "schluter92.pdf")
schluter92_text <- pdf_text(schluter92_path)
schluter92_df <- data.frame(schluter92_text)%>%
 mutate(text_full=str_split(schluter92_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 schluter92_tokens <- schluter92_df %>% 
     unnest_tokens(word, text_full)
schluter92_wc <- schluter92_tokens %>% 
     count(word) %>% 
     arrange(-n)
schluter92_wc
schluter92_stop <- schluter92_tokens %>% 
  anti_join(stop_words) %>% 
  select(-schluter92_text)
schluter92_swc <- schluter92_stop %>% 
     count(word) %>% 
     arrange(-n)
schluter92_no_numeric <- schluter92_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
schluter92_Sentida <- schluter92_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
schluter92_Sentida
schluter92_Sentida_hist <- schluter92_Sentida %>% 
  count(score)
schluter92_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.575
ggplot(data = schluter92_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----mygdal27-------------------------------------------------------------------------------
mygdal27_path <- here::here("data", "mygdal27.pdf")
mygdal27_text <- pdf_text(mygdal27_path)
mygdal27_df <- data.frame(mygdal27_text)%>%
 mutate(text_full=str_split(mygdal27_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 mygdal27_tokens <- mygdal27_df %>% 
     unnest_tokens(word, text_full)
mygdal27_wc <- mygdal27_tokens %>% 
     count(word) %>% 
     arrange(-n)
mygdal27_wc
mygdal27_stop <- mygdal27_tokens %>% 
  anti_join(stop_words) %>% 
  select(-mygdal27_text)
mygdal27_swc <- mygdal27_stop %>% 
     count(word) %>% 
     arrange(-n)
mygdal27_no_numeric <- mygdal27_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
mygdal27_Sentida <- mygdal27_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
mygdal27_Sentida
mygdal27_Sentida_hist <- mygdal27_Sentida %>% 
  count(score)
mygdal27_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.635
ggplot(data = mygdal27_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----mygdal28-------------------------------------------------------------------------------
mygdal28_path <- here::here("data", "mygdal28.pdf")
mygdal28_text <- pdf_text(mygdal28_path)
mygdal28_df <- data.frame(mygdal28_text)%>%
 mutate(text_full=str_split(mygdal28_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 mygdal28_tokens <- mygdal28_df %>% 
     unnest_tokens(word, text_full)
mygdal28_wc <- mygdal28_tokens %>% 
     count(word) %>% 
     arrange(-n)
mygdal28_wc
mygdal28_stop <- mygdal28_tokens %>% 
  anti_join(stop_words) %>% 
  select(-mygdal28_text)
mygdal28_swc <- mygdal28_stop %>% 
     count(word) %>% 
     arrange(-n)
mygdal28_no_numeric <- mygdal28_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
mygdal28_Sentida <- mygdal28_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
mygdal28_Sentida
mygdal28_Sentida_hist <- mygdal28_Sentida %>% 
  count(score)
mygdal28_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.577
ggplot(data = mygdal28_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----stauning25-----------------------------------------------------------------------------
stauning25_path <- here::here("data", "stauning25.pdf")
stauning25_text <- pdf_text(stauning25_path)
stauning25_df <- data.frame(stauning25_text)%>%
 mutate(text_full=str_split(stauning25_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 stauning25_tokens <- stauning25_df %>% 
     unnest_tokens(word, text_full)
stauning25_wc <- stauning25_tokens %>% 
     count(word) %>% 
     arrange(-n)
stauning25_wc
stauning25_stop <- stauning25_tokens %>% 
  anti_join(stop_words) %>% 
  select(-stauning25_text)
stauning25_swc <- stauning25_stop %>% 
     count(word) %>% 
     arrange(-n)
stauning25_no_numeric <- stauning25_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
stauning25_Sentida <- stauning25_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
stauning25_Sentida
stauning25_Sentida_hist <- stauning25_Sentida %>% 
  count(score)
stauning25_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.891
ggplot(data = stauning25_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----stauning26-----------------------------------------------------------------------------
stauning26_path <- here::here("data", "stauning26.pdf")
stauning26_text <- pdf_text(stauning26_path)
stauning26_df <- data.frame(stauning26_text)%>%
 mutate(text_full=str_split(stauning26_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 stauning26_tokens <- stauning26_df %>% 
     unnest_tokens(word, text_full)
stauning26_wc <- stauning26_tokens %>% 
     count(word) %>% 
     arrange(-n)
stauning26_wc
stauning26_stop <- stauning26_tokens %>% 
  anti_join(stop_words) %>% 
  select(-stauning26_text)
stauning26_swc <- stauning26_stop %>% 
     count(word) %>% 
     arrange(-n)
stauning26_no_numeric <- stauning26_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
stauning26_Sentida <- stauning26_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
stauning26_Sentida
stauning26_Sentida_hist <- stauning26_Sentida %>% 
  count(score)
stauning26_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.637
ggplot(data = stauning26_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----stauning29-----------------------------------------------------------------------------
stauning29_path <- here::here("data", "stauning29.pdf")
stauning29_text <- pdf_text(stauning29_path)
stauning29_df <- data.frame(stauning29_text)%>%
 mutate(text_full=str_split(stauning29_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 stauning29_tokens <- stauning29_df %>% 
     unnest_tokens(word, text_full)
stauning29_wc <- stauning29_tokens %>% 
     count(word) %>% 
     arrange(-n)
stauning29_wc
stauning29_stop <- stauning29_tokens %>% 
  anti_join(stop_words) %>% 
  select(-stauning29_text)
stauning29_swc <- stauning29_stop %>% 
     count(word) %>% 
     arrange(-n)
stauning29_no_numeric <- stauning29_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
stauning29_Sentida <- stauning29_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
stauning29_Sentida
stauning29_Sentida_hist <- stauning29_Sentida %>% 
  count(score)
stauning29_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.660
ggplot(data = stauning29_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----stauning30-----------------------------------------------------------------------------
stauning30_path <- here::here("data", "stauning30.pdf")
stauning30_text <- pdf_text(stauning30_path)
stauning30_df <- data.frame(stauning30_text)%>%
 mutate(text_full=str_split(stauning30_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 stauning30_tokens <- stauning30_df %>% 
     unnest_tokens(word, text_full)
stauning30_wc <- stauning30_tokens %>% 
     count(word) %>% 
     arrange(-n)
stauning30_wc
stauning30_stop <- stauning30_tokens %>% 
  anti_join(stop_words) %>% 
  select(-stauning30_text)
stauning30_swc <- stauning30_stop %>% 
     count(word) %>% 
     arrange(-n)
stauning30_no_numeric <- stauning30_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
stauning30_Sentida <- stauning30_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
stauning30_Sentida
stauning30_Sentida_hist <- stauning30_Sentida %>% 
  count(score)
stauning30_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.407
ggplot(data = stauning30_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----stauning31-----------------------------------------------------------------------------
stauning31_path <- here::here("data", "stauning31.pdf")
stauning31_text <- pdf_text(stauning31_path)
stauning31_df <- data.frame(stauning31_text)%>%
 mutate(text_full=str_split(stauning31_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 stauning31_tokens <- stauning31_df %>% 
     unnest_tokens(word, text_full)
stauning31_wc <- stauning31_tokens %>% 
     count(word) %>% 
     arrange(-n)
stauning31_wc
stauning31_stop <- stauning31_tokens %>% 
  anti_join(stop_words) %>% 
  select(-stauning31_text)
stauning31_swc <- stauning31_stop %>% 
     count(word) %>% 
     arrange(-n)
stauning31_no_numeric <- stauning31_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
stauning31_Sentida <- stauning31_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
stauning31_Sentida
stauning31_Sentida_hist <- stauning31_Sentida %>% 
  count(score)
stauning31_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.402
ggplot(data = stauning31_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----stauning32-----------------------------------------------------------------------------
stauning32_path <- here::here("data", "stauning32.pdf")
stauning32_text <- pdf_text(stauning32_path)
stauning32_df <- data.frame(stauning32_text)%>%
 mutate(text_full=str_split(stauning32_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 stauning32_tokens <- stauning32_df %>% 
     unnest_tokens(word, text_full)
stauning32_wc <- stauning32_tokens %>% 
     count(word) %>% 
     arrange(-n)
stauning32_wc
stauning32_stop <- stauning32_tokens %>% 
  anti_join(stop_words) %>% 
  select(-stauning32_text)
stauning32_swc <- stauning32_stop %>% 
     count(word) %>% 
     arrange(-n)
stauning32_no_numeric <- stauning32_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
stauning32_Sentida <- stauning32_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
stauning32_Sentida
stauning32_Sentida_hist <- stauning32_Sentida %>% 
  count(score)
stauning32_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.411
ggplot(data = stauning32_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----stauning33-----------------------------------------------------------------------------
stauning33_path <- here::here("data", "stauning33.pdf")
stauning33_text <- pdf_text(stauning33_path)
stauning33_df <- data.frame(stauning33_text)%>%
 mutate(text_full=str_split(stauning33_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 stauning33_tokens <- stauning33_df %>% 
     unnest_tokens(word, text_full)
stauning33_wc <- stauning33_tokens %>% 
     count(word) %>% 
     arrange(-n)
stauning33_wc
stauning33_stop <- stauning33_tokens %>% 
  anti_join(stop_words) %>% 
  select(-stauning33_text)
stauning33_swc <- stauning33_stop %>% 
     count(word) %>% 
     arrange(-n)
stauning33_no_numeric <- stauning33_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
stauning33_Sentida <- stauning33_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
stauning33_Sentida
stauning33_Sentida_hist <- stauning33_Sentida %>% 
  count(score)
stauning33_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.734
ggplot(data = stauning33_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----stauning34-----------------------------------------------------------------------------
stauning34_path <- here::here("data", "stauning34.pdf")
stauning34_text <- pdf_text(stauning34_path)
stauning34_df <- data.frame(stauning34_text)%>%
 mutate(text_full=str_split(stauning34_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 stauning34_tokens <- stauning34_df %>% 
     unnest_tokens(word, text_full)
stauning34_wc <- stauning34_tokens %>% 
     count(word) %>% 
     arrange(-n)
stauning34_wc
stauning34_stop <- stauning34_tokens %>% 
  anti_join(stop_words) %>% 
  select(-stauning34_text)
stauning34_swc <- stauning34_stop %>% 
     count(word) %>% 
     arrange(-n)
stauning34_no_numeric <- stauning34_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
stauning34_Sentida <- stauning34_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
stauning34_Sentida
stauning34_Sentida_hist <- stauning34_Sentida %>% 
  count(score)
stauning34_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.441
ggplot(data = stauning34_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----anker73--------------------------------------------------------------------------------
anker73_path <- here::here("data", "anker73.pdf")
anker73_text <- pdf_text(anker73_path)
anker73_df <- data.frame(anker73_text)%>%
 mutate(text_full=str_split(anker73_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 anker73_tokens <- anker73_df %>% 
     unnest_tokens(word, text_full)
anker73_wc <- anker73_tokens %>% 
     count(word) %>% 
     arrange(-n)
anker73_wc
anker73_stop <- anker73_tokens %>% 
  anti_join(stop_words) %>% 
  select(-anker73_text)
anker73_swc <- anker73_stop %>% 
     count(word) %>% 
     arrange(-n)
anker73_no_numeric <- anker73_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
anker73_Sentida <- anker73_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
anker73_Sentida
anker73_Sentida_hist <- anker73_Sentida %>% 
  count(score)
anker73_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.510
ggplot(data = anker73_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----anker75--------------------------------------------------------------------------------
anker75_path <- here::here("data", "anker75.pdf")
anker75_text <- pdf_text(anker75_path)
anker75_df <- data.frame(anker75_text)%>%
 mutate(text_full=str_split(anker75_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 anker75_tokens <- anker75_df %>% 
     unnest_tokens(word, text_full)
anker75_wc <- anker75_tokens %>% 
     count(word) %>% 
     arrange(-n)
anker75_wc
anker75_stop <- anker75_tokens %>% 
  anti_join(stop_words) %>% 
  select(-anker75_text)
anker75_swc <- anker75_stop %>% 
     count(word) %>% 
     arrange(-n)
anker75_no_numeric <- anker75_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
anker75_Sentida <- anker75_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
anker75_Sentida
anker75_Sentida_hist <- anker75_Sentida %>% 
  count(score)
anker75_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.629
ggplot(data = anker75_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----anker76--------------------------------------------------------------------------------
anker76_path <- here::here("data", "anker76.pdf")
anker76_text <- pdf_text(anker76_path)
anker76_df <- data.frame(anker76_text)%>%
 mutate(text_full=str_split(anker76_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 anker76_tokens <- anker76_df %>% 
     unnest_tokens(word, text_full)
anker76_wc <- anker76_tokens %>% 
     count(word) %>% 
     arrange(-n)
anker76_wc
anker76_stop <- anker76_tokens %>% 
  anti_join(stop_words) %>% 
  select(-anker76_text)
anker76_swc <- anker76_stop %>% 
     count(word) %>% 
     arrange(-n)
anker76_no_numeric <- anker76_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
anker76_Sentida <- anker76_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
anker76_Sentida
anker76_Sentida_hist <- anker76_Sentida %>% 
  count(score)
anker76_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.576
ggplot(data = anker76_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----anker77--------------------------------------------------------------------------------
anker77_path <- here::here("data", "anker77.pdf")
anker77_text <- pdf_text(anker77_path)
anker77_df <- data.frame(anker77_text)%>%
 mutate(text_full=str_split(anker77_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 anker77_tokens <- anker77_df %>% 
     unnest_tokens(word, text_full)
anker77_wc <- anker77_tokens %>% 
     count(word) %>% 
     arrange(-n)
anker77_wc
anker77_stop <- anker77_tokens %>% 
  anti_join(stop_words) %>% 
  select(-anker77_text)
anker77_swc <- anker77_stop %>% 
     count(word) %>% 
     arrange(-n)
anker77_no_numeric <- anker77_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
anker77_Sentida <- anker77_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
anker77_Sentida
anker77_Sentida_hist <- anker77_Sentida %>% 
  count(score)
anker77_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.579
ggplot(data = anker77_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----anker78--------------------------------------------------------------------------------
anker78_path <- here::here("data", "anker78.pdf")
anker78_text <- pdf_text(anker78_path)
anker78_df <- data.frame(anker78_text)%>%
 mutate(text_full=str_split(anker78_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 anker78_tokens <- anker78_df %>% 
     unnest_tokens(word, text_full)
anker78_wc <- anker78_tokens %>% 
     count(word) %>% 
     arrange(-n)
anker78_wc
anker78_stop <- anker78_tokens %>% 
  anti_join(stop_words) %>% 
  select(-anker78_text)
anker78_swc <- anker78_stop %>% 
     count(word) %>% 
     arrange(-n)
anker78_no_numeric <- anker78_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
anker78_Sentida <- anker78_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
anker78_Sentida
anker78_Sentida_hist <- anker78_Sentida %>% 
  count(score)
anker78_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.431
ggplot(data = anker78_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----anker79--------------------------------------------------------------------------------
anker79_path <- here::here("data", "anker79.pdf")
anker79_text <- pdf_text(anker79_path)
anker79_df <- data.frame(anker79_text)%>%
 mutate(text_full=str_split(anker79_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 anker79_tokens <- anker79_df %>% 
     unnest_tokens(word, text_full)
anker79_wc <- anker79_tokens %>% 
     count(word) %>% 
     arrange(-n)
anker79_wc
anker79_stop <- anker79_tokens %>% 
  anti_join(stop_words) %>% 
  select(-anker79_text)
anker79_swc <- anker79_stop %>% 
     count(word) %>% 
     arrange(-n)
anker79_no_numeric <- anker79_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
anker79_Sentida <- anker79_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
anker79_Sentida
anker79_Sentida_hist <- anker79_Sentida %>% 
  count(score)
anker79_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.551
ggplot(data = anker79_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----anker81--------------------------------------------------------------------------------
anker81_path <- here::here("data", "anker81.pdf")
anker81_text <- pdf_text(anker81_path)
anker81_df <- data.frame(anker81_text)%>%
 mutate(text_full=str_split(anker81_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 anker81_tokens <- anker81_df %>% 
     unnest_tokens(word, text_full)
anker81_wc <- anker81_tokens %>% 
     count(word) %>% 
     arrange(-n)
anker81_wc
anker81_stop <- anker81_tokens %>% 
  anti_join(stop_words) %>% 
  select(-anker81_text)
anker81_swc <- anker81_stop %>% 
     count(word) %>% 
     arrange(-n)
anker81_no_numeric <- anker81_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
anker81_Sentida <- anker81_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
anker81_Sentida
anker81_Sentida_hist <- anker81_Sentida %>% 
  count(score)
anker81_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.528
ggplot(data = anker81_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----kragh72--------------------------------------------------------------------------------
kragh72_path <- here::here("data", "kragh72.pdf")
kragh72_text <- pdf_text(kragh72_path)
kragh72_df <- data.frame(kragh72_text)%>%
 mutate(text_full=str_split(kragh72_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 kragh72_tokens <- kragh72_df %>% 
     unnest_tokens(word, text_full)
kragh72_wc <- kragh72_tokens %>% 
     count(word) %>% 
     arrange(-n)
kragh72_wc
kragh72_stop <- kragh72_tokens %>% 
  anti_join(stop_words) %>% 
  select(-kragh72_text)
kragh72_swc <- kragh72_stop %>% 
     count(word) %>% 
     arrange(-n)
kragh72_no_numeric <- kragh72_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
kragh72_Sentida <- kragh72_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
kragh72_Sentida
kragh72_Sentida_hist <- kragh72_Sentida %>% 
  count(score)
kragh72_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.599
ggplot(data = kragh72_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----fogh03---------------------------------------------------------------------------------
fogh03_path <- here::here("data", "fogh03.pdf")
fogh03_text <- pdf_text(fogh03_path)
fogh03_df <- data.frame(fogh03_text)%>%
 mutate(text_full=str_split(fogh03_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 fogh03_tokens <- fogh03_df %>% 
     unnest_tokens(word, text_full)
fogh03_wc <- fogh03_tokens %>% 
     count(word) %>% 
     arrange(-n)
fogh03_wc
fogh03_stop <- fogh03_tokens %>% 
  anti_join(stop_words) %>% 
  select(-fogh03_text)
fogh03_swc <- fogh03_stop %>% 
     count(word) %>% 
     arrange(-n)
fogh03_no_numeric <- fogh03_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
fogh03_Sentida <- fogh03_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
fogh03_Sentida
fogh03_Sentida_hist <- fogh03_Sentida %>% 
  count(score)
fogh03_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.538
ggplot(data = fogh03_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----fogh04---------------------------------------------------------------------------------
fogh04_path <- here::here("data", "fogh04.pdf")
fogh04_text <- pdf_text(fogh04_path)
fogh04_df <- data.frame(fogh04_text)%>%
 mutate(text_full=str_split(fogh04_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 fogh04_tokens <- fogh04_df %>% 
     unnest_tokens(word, text_full)
fogh04_wc <- fogh04_tokens %>% 
     count(word) %>% 
     arrange(-n)
fogh04_wc
fogh04_stop <- fogh04_tokens %>% 
  anti_join(stop_words) %>% 
  select(-fogh04_text)
fogh04_swc <- fogh04_stop %>% 
     count(word) %>% 
     arrange(-n)
fogh04_no_numeric <- fogh04_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
fogh04_Sentida <- fogh04_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
fogh04_Sentida
fogh04_Sentida_hist <- fogh04_Sentida %>% 
  count(score)
fogh04_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.543
ggplot(data = fogh04_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----fogh05---------------------------------------------------------------------------------
fogh05_path <- here::here("data", "fogh05.pdf")
fogh05_text <- pdf_text(fogh05_path)
fogh05_df <- data.frame(fogh05_text)%>%
 mutate(text_full=str_split(fogh05_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 fogh05_tokens <- fogh05_df %>% 
     unnest_tokens(word, text_full)
fogh05_wc <- fogh05_tokens %>% 
     count(word) %>% 
     arrange(-n)
fogh05_wc
fogh05_stop <- fogh05_tokens %>% 
  anti_join(stop_words) %>% 
  select(-fogh05_text)
fogh05_swc <- fogh05_stop %>% 
     count(word) %>% 
     arrange(-n)
fogh05_no_numeric <- fogh05_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
fogh05_Sentida <- fogh05_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
fogh05_Sentida
fogh05_Sentida_hist <- fogh05_Sentida %>% 
  count(score)
fogh05_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.549
ggplot(data = fogh05_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----fogh06---------------------------------------------------------------------------------
fogh06_path <- here::here("data", "fogh06.pdf")
fogh06_text <- pdf_text(fogh06_path)
fogh06_df <- data.frame(fogh06_text)%>%
 mutate(text_full=str_split(fogh06_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 fogh06_tokens <- fogh06_df %>% 
     unnest_tokens(word, text_full)
fogh06_wc <- fogh06_tokens %>% 
     count(word) %>% 
     arrange(-n)
fogh06_wc
fogh06_stop <- fogh06_tokens %>% 
  anti_join(stop_words) %>% 
  select(-fogh06_text)
fogh06_swc <- fogh06_stop %>% 
     count(word) %>% 
     arrange(-n)
fogh06_no_numeric <- fogh06_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
fogh06_Sentida <- fogh06_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
fogh06_Sentida
fogh06_Sentida_hist <- fogh06_Sentida %>% 
  count(score)
fogh06_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.586
ggplot(data = fogh06_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()

## ----fogh08---------------------------------------------------------------------------------
fogh08_path <- here::here("data", "fogh08.pdf")
fogh08_text <- pdf_text(fogh08_path)
fogh08_df <- data.frame(fogh08_text)%>%
 mutate(text_full=str_split(fogh08_text,pattern='\n'))%>%
 unnest(text_full)%>%
 mutate(text_full=str_trim(text_full))
 fogh08_tokens <- fogh08_df %>% 
     unnest_tokens(word, text_full)
fogh08_wc <- fogh08_tokens %>% 
     count(word) %>% 
     arrange(-n)
fogh08_wc
fogh08_stop <- fogh08_tokens %>% 
  anti_join(stop_words) %>% 
  select(-fogh08_text)
fogh08_swc <- fogh08_stop %>% 
     count(word) %>% 
     arrange(-n)
fogh08_no_numeric <- fogh08_stop %>% 
  filter(is.na(as.numeric(word)))
Sentida_pos <- sentida_lexicon %>% 
  filter(score %in% c(3,4,5))
Sentida_neg <- sentida_lexicon %>% 
  filter(score %in% c(-3,-4,-5))
Sentida_neg
fogh08_Sentida <- fogh08_stop %>% 
  inner_join(sentida_lexicon, by = c("word" = "stem"))
fogh08_Sentida
fogh08_Sentida_hist <- fogh08_Sentida %>% 
  count(score)
fogh08_Sentida %>% 
  summarise(mean_sentiment = mean(score))
#mean is 0.359
ggplot(data = fogh08_Sentida_hist, aes(x = score, y = n)) +
  geom_col(aes(fill = score)) +
  theme_bw()


## ----Depression graph-----------------------------------------------------------------------
# Import data
depressionen <- read.csv2(
  here::here("data", "depressionen.csv")
) %>%
  mutate(mean.sentiment.value = as.numeric(mean.sentiment.value))

# Create graph to plot the mean sentiment value in opening speeches before and during the Great Depression
ggplot(depressionen, aes(x = year, y = mean.sentiment.value)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", linewidth = 1) +
  scale_x_continuous(breaks = 1925:1934) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Depressionen",
    x = "Year",
    y = "Mean Sentiment"
  ) +
  theme_bw()


## ----70s oil crises fattig80erne and 90sdotcombubble----------------------------------------
# Import data
a70erne00erne <- read.csv2(
  here::here("data", "a70erne00erne.csv")
) %>%
  mutate(mean.sentiment.value = as.numeric(mean.sentiment.value))

# Create graph for the entire period of the 70s, 80s, and 90s since each crisis flows into the next in this period
ggplot(a70erne00erne, aes(x = year, y = mean.sentiment.value)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", linewidth = 1) +
  scale_x_continuous(breaks = 1968:2001) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "70s Oil Crises, Fattig Firserne and Dotcom Bubble",
    x = "Year",
    y = "Mean Sentiment"
  ) +
  theme_bw()


## ----first oil crisis 1973------------------------------------------------------------------
# First Oil Crisis
oil1 <- a70erne00erne %>%
  filter(year >= 1968 & year <= 1974)

ggplot(oil1, aes(x = year, y = mean.sentiment.value)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", linewidth = 1) +
  scale_x_continuous(breaks = 1968:1974) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "First Oil Crisis",
    x = "Year",
    y = "Mean Sentiment"
  ) +
  theme_bw()


## ----second oil crisis 1979-----------------------------------------------------------------
# Second Oil Crisis
oil2 <- a70erne00erne %>%
  filter(year >= 1975 & year <= 1980)

ggplot(oil2, aes(x = year, y = mean.sentiment.value)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", linewidth = 1) +
  scale_x_continuous(breaks = 1975:1980) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Second Oil Crisis",
    x = "Year",
    y = "Mean Sentiment"
  ) +
  theme_bw()


## ----fattig firserne------------------------------------------------------------------------
# Fattig Firserne
fattig80 <- a70erne00erne %>%
  filter(year >= 1981 & year <= 1989)

ggplot(fattig80, aes(x = year, y = mean.sentiment.value)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", linewidth = 1) +
  scale_x_continuous(breaks = 1981:1989) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Fattig Firserne",
    x = "Year",
    y = "Mean Sentiment"
  ) +
  theme_bw()


## ----nyrupeconomy---------------------------------------------------------------------------
# Nyrup Economic Revival
nyrup <- a70erne00erne %>%
  filter(year >= 1990 & year <= 2001)

ggplot(nyrup, aes(x = year, y = mean.sentiment.value)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", linewidth = 1, se = FALSE) +
  scale_x_continuous(breaks = 1990:2001) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Nyrup Economic Revival",
    x = "Year",
    y = "Mean Sentiment"
  ) +
  theme_bw()
#important to note that it is only by the end of the decade that there is an actual improvement in the economy, meaning Fattig firserne's stagnant economy bled into the 90s


## ----2008 financial crisis------------------------------------------------------------------
# Import data
a2008financialcrisis <- read.csv2(
  here::here("data", "a2008financial.csv")
) %>%
  mutate(mean.sentiment.value = as.numeric(mean.sentiment.value))

# Create graph the 2008 world financial crisis
ggplot(a2008financialcrisis, aes(x = Year, y = mean.sentiment.value)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", linewidth = 1) +
  scale_x_continuous(breaks = 2003:2010) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "2008 World Financial Crisis",
    x = "Year",
    y = "Mean Sentiment"
  ) +
  theme_bw()


## ----covid financial------------------------------------------------------------------------
# Import data
covidfinancial <- read.csv2(
  here::here("data", "covidfinancial.csv")
) %>%
  mutate(mean.sentiment.value = as.numeric(mean.sentiment.value))

# Create graph the Covid-19 financial situation in Denmark
ggplot(covidfinancial, aes(x = Year, y = mean.sentiment.value)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", linewidth = 1) +
  scale_x_continuous(breaks = 2015:2021) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Covid-19 Financial Situation",
    x = "Year",
    y = "Mean Sentiment"
  ) +
  theme_bw()

