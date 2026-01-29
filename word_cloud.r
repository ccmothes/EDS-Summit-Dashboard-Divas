library(tidyverse)
library(tidytext)
library(wordcloud)

# Create folder for wordclouds
dir.create("wordclouds", showWarnings = FALSE)

# Custom stop words
# Includes words that are in the question prompts
custom_stop_words <- tibble(
  word = c(
    "https", "http", "www", "com", "org", "youtube", "googledoc",
    "thrivingearthexchange.org", "ambassadors.openaq.org",
    "select", "please", "scale", "text", "applicable", "specify",
    "e.g", "etc", "na", "nbsp",
    "1", "2", "3", "4", "5", "0",
    "choice", "selected", "based", "project", "community", "outcomes"
  )
)

# Clean text function (we noticed someone added a youtube link)
clean_text <- function(text) {
  text %>%
    str_remove_all("https?://[^\\s]+") %>%
    str_remove_all("www\\.[^\\s]+") %>%
    str_remove_all("[0-9]+") %>%
    str_remove_all("[^[:alpha:]\\s]")
}

# SAVE Q5: How do you define success?
png("wordclouds/Q5_define_success.png", width = 1000, height = 800)
data %>% 
  select(Q5) %>% 
  filter(!is.na(Q5) & Q5 != "") %>%
  mutate(Q5 = clean_text(Q5)) %>%
  unnest_tokens(output = word, input = Q5) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  mutate(word = group_similar_words(word)) %>%
  count(word) %>% 
  with(wordcloud(words = word, freq = n, random.order = FALSE,
                 scale = c(2.5, 0.5), min.freq = 1, max.words = 100,
                 colors = c("#6FA8F5", "#FF4D45", "#FFC85E")))
title(main = "How do you define success?", font.main = 1, cex.main = 1.8)
dev.off()

# SAVE Q10: ONE metric of success
png("wordclouds/Q10_one_metric.png", width = 1000, height = 800)
data %>% 
  select(Q10) %>% 
  filter(!is.na(Q10) & Q10 != "") %>%
  mutate(Q10 = clean_text(Q10)) %>%
  unnest_tokens(output = word, input = Q10) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  mutate(word = group_similar_words(word)) %>%
  count(word) %>% 
  with(wordcloud(words = word, freq = n, random.order = FALSE,
                 scale = c(2.5, 0.5), min.freq = 1, max.words = 100,
                 colors = c("#6FA8F5", "#FF4D45", "#FFC85E")))
title(main = "ONE metric of success", font.main = 1, cex.main = 1.8)
dev.off()

# SAVE Q16: Define "actionable outcomes"
png("wordclouds/Q16_actionable_outcomes.png", width = 1000, height = 800)
data %>% 
  select(Q16) %>% 
  filter(!is.na(Q16) & Q16 != "") %>%
  mutate(Q16 = clean_text(Q16)) %>%
  unnest_tokens(output = word, input = Q16) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  mutate(word = group_similar_words(word)) %>%
  count(word) %>% 
  with(wordcloud(words = word, freq = n, random.order = FALSE,
                 scale = c(2.5, 0.5), min.freq = 1, max.words = 100,
                 colors = c("#6FA8F5", "#FF4D45", "#FFC85E")))
title(main = "Define 'actionable outcomes'", font.main = 1, cex.main = 1.8)
dev.off()

# SAVE Q23: Case studies
png("wordclouds/Q23_case_studies.png", width = 1000, height = 800)
data %>% 
  select(Q23) %>% 
  filter(!is.na(Q23) & Q23 != "") %>%
  mutate(Q23 = clean_text(Q23)) %>%
  unnest_tokens(output = word, input = Q23) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  mutate(word = group_similar_words(word)) %>%
  count(word) %>% 
  with(wordcloud(words = word, freq = n, random.order = FALSE,
                 scale = c(2.5, 0.5), min.freq = 1, max.words = 100,
                 colors = c("#6FA8F5", "#FF4D45", "#FFC85E")))
title(main = "Case Studies Examples", font.main = 1, cex.main = 1.8)
dev.off()