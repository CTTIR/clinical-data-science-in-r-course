# Lab W2 · Next-word frequencies
# Clinical Data Science in R · Course, workshop W2 (teaching unit 05)
# A deliberately tiny next-word (bigram) model built from twelve synthetic teaching sentences.
# It shows the generation principle of language models, not how any real model is built or trained.
# Run in a fresh session from the repository root:  Rscript r/w2-next-word.R
# Stand-alone runs write figures and tables to ./output (or to the folder in CDSR_OUTPUT).

## ---- w2-setup ----
# Locate the folder with style.R and course.R, whatever the working directory
script_dir <- local({
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  here <- if (length(arg) == 1) dirname(gsub("~+~", " ", sub("^--file=", "", arg), fixed = TRUE)) else "."
  candidates <- c(here, "r", ".", "../r")
  candidates[file.exists(file.path(candidates, "style.R"))][1]
})
source(file.path(script_dir, "style.R"))
source(file.path(script_dir, "course.R"))

## ---- w2-packages ----
library(ggplot2)

## ---- w2-corpus ----
# Twelve synthetic teaching sentences. Each one is true in its own context.
corpus <- c(
  "a negative alert does not exclude a fracture",
  "a careful review can exclude a fracture",
  "a ct scan can exclude a fracture in most patients",
  "an mri can exclude a fracture",
  "the alert can help the team",
  "the alert can miss a fracture",
  "the alert can be wrong",
  "the alert suggests a second look",
  "the alert suggests a fracture",
  "a second look can change the decision",
  "the team reviews the image again",
  "a fracture needs a splint"
)
# Split into words (tokens) and mark sentence start <s> and end </s>
tokens <- lapply(strsplit(corpus, " "), function(w) c("<s>", w, "</s>"))
length(unlist(tokens))

## ---- w2-bigrams ----
# Count every pair of neighbouring words (bigram)
pairs <- do.call(rbind, lapply(tokens, function(w) {
  data.frame(word = head(w, -1), next_word = tail(w, -1))
}))
bigrams <- as.data.frame(table(word = pairs$word, next_word = pairs$next_word), stringsAsFactors = FALSE)
bigrams <- bigrams[bigrams$Freq > 0, ]
names(bigrams)[3] <- "count"
# What follows "alert", and what follows "can"?
bigrams[bigrams$word == "alert", ]
bigrams[bigrams$word == "can", ]
stopifnot(
  sum(bigrams$count[bigrams$word == "alert"]) == 6,
  bigrams$count[bigrams$word == "alert" & bigrams$next_word == "can"] == 3,
  bigrams$count[bigrams$word == "alert" & bigrams$next_word == "does"] == 1,
  bigrams$count[bigrams$word == "can" & bigrams$next_word == "exclude"] == 3
)

## ---- w2-next-prob ----
# Probability of the next word given the current word, with a temperature T:
# counts are raised to the power 1/T and renormalised (T < 1 sharpens, T > 1 flattens)
next_prob <- function(word, temperature = 1) {
  b <- bigrams[bigrams$word == word, ]
  w <- b$count^(1 / temperature)
  setNames(w / sum(w), b$next_word)
}
round(next_prob("alert"), 3)
stopifnot(isTRUE(all.equal(next_prob("alert")[["can"]], 3 / 6)),
          isTRUE(all.equal(next_prob("alert")[["does"]], 1 / 6)))

## ---- fig-w2-temperature ----
temp_df <- do.call(rbind, lapply(c(0.5, 1, 2), function(t) {
  p <- next_prob("alert", t)
  data.frame(temperature = paste("T =", t), next_word = names(p), probability = unname(p))
}))
temp_df$next_word <- factor(temp_df$next_word, levels = c("can", "suggests", "does"))
p_temp <- ggplot(temp_df, aes(x = next_word, y = probability, fill = temperature)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", probability)), position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = unname(cdsr_colours[c("blue", "orange", "grey")])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(x = "Next word after \"alert\"", y = "Probability", fill = "Temperature",
       caption = "Bigram model from 12 synthetic teaching sentences; 6 occurrences of \"alert\" followed by another word.") +
  theme_cdsr()
show_figure(p_temp, "w2-temperature.png", height = 3.8)

## ---- w2-sentence-prob ----
# Probability of a whole continuation given its first words: the product of next-word probabilities
sentence_prob <- function(sentence, given = 3) {
  w <- c("<s>", strsplit(sentence, " ")[[1]], "</s>")
  steps <- seq(given + 1, length(w) - 1)   # condition on the first `given` words of the sentence
  prod(sapply(steps, function(i) {
    p <- next_prob(w[i])
    if (w[i + 1] %in% names(p)) p[[w[i + 1]]] else 0
  }))
}
true_sentence  <- "a negative alert does not exclude a fracture"
false_sentence <- "a negative alert can exclude a fracture"
probs <- c(true = sentence_prob(true_sentence), false = sentence_prob(false_sentence))
round(probs, 4)
# Target value: the false continuation is more probable than the true one
stopifnot(probs[["false"]] > probs[["true"]],
          !false_sentence %in% corpus)   # ... although it never occurs in the corpus

## ---- w2-generate ----
# Continue "a negative alert" word by word; greedy = always the most probable next word
generate <- function(start = c("<s>", "a", "negative", "alert"), temperature = 1, greedy = FALSE,
                     max_words = 12) {
  w <- start
  while (tail(w, 1) != "</s>" && length(w) < max_words + 1) {
    p <- next_prob(tail(w, 1), temperature)
    w <- c(w, if (greedy) names(p)[which.max(p)] else sample(names(p), 1, prob = p))
  }
  paste(w[!w %in% c("<s>", "</s>")], collapse = " ")
}
greedy_text <- generate(greedy = TRUE)
greedy_text
set.seed(2026)
samples <- vapply(1:20, function(i) generate(temperature = 1), character(1))
sample_table <- sort(table(samples), decreasing = TRUE)
sample_table
stopifnot(greedy_text == false_sentence,
          sum(samples == false_sentence) == 5,     # the false sentence, word for word, 5 times in 20
          sum(samples == true_sentence) == 0)      # the true sentence never appears complete

## ---- w2-save ----
save_csv(data.frame(sentence = corpus), "w2-corpus.csv")
save_csv(bigrams, "w2-bigrams.csv")
