library(tidyverse)
library(tidytext)
library(topicmodels)
library(stringr)
data("AssociatedPress", package = "topicmodels")

ap_td1 <- tidy(AssociatedPress)
ap_td1 

ap_dtm1 <- ap_td1 %>%
  anti_join(stop_words, by = c(term = "word")) %>%
  cast_dtm(document, term, count)
ap_dtm1

ap_lda <- LDA(ap_dtm, k = 4, control = list(seed = 11091987))
ap_lda

ap_lda_td <- tidy(ap_lda)

top_terms <- ap_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip()

perplexity(ap_lda)

n_topics <- c(2, 4, 10, 20, 50, 100)
ap_lda_compare <- n_topics %>%
  map(LDA, x = ap_dtm, control = list(seed = 1109))

data_frame(k = n_topics,
           perplex = map_dbl(ap_lda_compare, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point() +
  geom_line() +
  labs(title = "Evaluating LDA topic models",
       subtitle = "Optimal number of topics (smaller is better)",
       x = "Number of topics",
       y = "Perplexity")