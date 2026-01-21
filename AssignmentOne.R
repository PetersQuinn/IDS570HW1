#kept getting path errors: admittedly asked chatGPT how to fix and it gave this:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

file_a <- "texts/A07594__Circle_of_Commerce.txt"
file_b <- "texts/B14801__Free_Trade.txt"

text_a <- read_file(file_a)
text_b <- read_file(file_b)

texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)
#check they are there
print(texts)

data("stop_words")

custom_stopwords <- tibble(
  word = c("vnto", "haue", "doo", "hath", "bee", "ye", "thee")
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

# I) Corpus diagnostics (BEFORE stopword removal)

corpus_diagnostics <- texts %>%
  mutate(n_chars = str_length(text)) %>%
  unnest_tokens(word, text) %>%              
  mutate(word = str_to_lower(word)) %>%     
  group_by(doc_title) %>%
  summarise(
    n_chars = first(n_chars),
    n_word_tokens = n(),
    n_word_types = n_distinct(word),
    .groups = "drop"
  )

cat("CORPUS DIAGNOSTICS (before stopword removal)")
print(corpus_diagnostics)

word_counts <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)

cat("WORD COUNTS (after stopword removal)")
print(word_counts)

doc_lengths <- word_counts %>%
  group_by(doc_title) %>%
  summarise(total_words = sum(n), .groups = "drop")

cat("DOC LENGTHS (after stopword removal)")
print(doc_lengths)

word_counts_normalized <- word_counts %>%
  left_join(doc_lengths, by = "doc_title") %>%
  mutate(relative_freq = n / total_words)

cat("WORD COUNTS NORMALIZED (sample)")
print(word_counts_normalized %>% slice(1:20))

trade_compare <- word_counts_normalized %>%
  filter(word == "trade") %>%
  select(doc_title, word, n, total_words, relative_freq) %>%
  arrange(doc_title)

cat("'trade' comparison (raw vs relative)")
print(trade_compare)


# Normalized version of top-20 plot

plot_n_words <- 20

#find top 20 words using the SAME criterion as Week 02 (raw counts)
word_comparison_tbl <- word_counts %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

top_words <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  select(word)

#keep only those top 20 words, but plot normalized frequencies
word_plot_data_norm <- word_counts_normalized %>%
  semi_join(top_words, by = "word") %>%
  mutate(word = fct_reorder(word, relative_freq, .fun = max))

#plot
p <- ggplot(word_plot_data_norm, aes(x = relative_freq, y = word)) +
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words (stopwords removed, normalized)",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words selected by maximum raw count across both texts; bars show relative frequency"
    ),
    x = "Relative frequency (proportion of total words in document)",
    y = NULL
  ) +
  theme_minimal()

print(p)


