# Lab W1 · Counting people, images and readings
# Clinical Data Science in R · Course, workshop W1 (teaching units 02-04)
# All data are synthetic and describe no real person.
# Run in a fresh session from the repository root:  Rscript r/w1-case-table.R
# Stand-alone runs write figures and tables to ./output (or to the folder in CDSR_OUTPUT).

## ---- w1-setup ----
# Locate the folder with style.R and course.R, whatever the working directory
script_dir <- local({
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  here <- if (length(arg) == 1) dirname(gsub("~+~", " ", sub("^--file=", "", arg), fixed = TRUE)) else "."
  candidates <- c(here, "r", ".", "../r")
  candidates[file.exists(file.path(candidates, "style.R"))][1]
})
source(file.path(script_dir, "style.R"))
source(file.path(script_dir, "course.R"))

## ---- w1-packages ----
library(ggplot2)

## ---- w1-raw-table ----
# Twelve rows as exported from a fictitious wrist-injury register (synthetic)
raw <- data.frame(
  row         = 1:12,
  person_id   = c("P01", "P01", "P01", "P02", "P02", "P3", "P03", "P04", "P04", "P04", "P05", "P05"),
  image_id    = c("P01-A", "P01-A", "P01-B", "P02-A", "P02-A", "P03-A", "P03-A", "P04-A", "P04-B", "P04-B",
                  "P05-A", "P05-A"),
  reader      = c("radiology", "emergency", "radiology", "radiology", "emergency", "radiology", "emergency",
                  "radiology", "radiology", "emergency", "radiology", "emergency"),
  fracture_read = c("yes", "yes", "yes", "no", "yes", "no", "no", "yes", "yes", "yes", "no", "no"),
  wrist_cm    = c(18.2, 18.2, 18.0, 16.1, 16.1, 17.4, 174, 19.0, 18.6, 18.6, 15.8, 15.8),
  age_years   = c(34, 34, 34, 71, 71, 52, 52, 88, 88, 88, 23, 23),
  hours_since_injury = c(2, 2, 50, 5, 5, 1, 1, 26, 74, 74, 3, 3),
  free_text   = c("cyclist, fell on the way to the bakery she owns", "", "follow-up image", "",
                  "lives alone, daughter is a nurse on ward 4", "", "", "fall in care home, room 12", "", "",
                  "goalkeeper, regional league", "")
)
raw[, c("row", "person_id", "image_id", "reader", "fracture_read", "wrist_cm")]

## ---- w1-count-naive ----
# Counting without looking: every row, every distinct text in person_id and image_id
c(rows = nrow(raw), persons = length(unique(raw$person_id)), images = length(unique(raw$image_id)))

## ---- w1-find-errors ----
# 1. Identifier error: the same person written as "P3" and "P03"
sort(unique(raw$person_id))
# 2. Unit error: wrist circumference outside a plausible adult range (12 to 30 cm)
raw[raw$wrist_cm < 12 | raw$wrist_cm > 30, c("row", "person_id", "wrist_cm")]

## ---- w1-correct ----
# Corrected copy: identifiers padded to two digits, the value in millimetres converted to centimetres
clean <- raw
clean$person_id <- sprintf("P%02d", as.integer(sub("^P", "", clean$person_id)))
clean$wrist_cm[clean$wrist_cm > 30] <- clean$wrist_cm[clean$wrist_cm > 30] / 10
counts <- c(rows = nrow(clean), persons = length(unique(clean$person_id)),
            images = length(unique(clean$image_id)))
counts
# Target values: 12 readings of 7 images from 5 people; the unit error is in row 7
stopifnot(counts[["rows"]] == 12, counts[["persons"]] == 5, counts[["images"]] == 7,
          length(unique(raw$person_id)) == 6, which(raw$wrist_cm > 30) == 7,
          all(clean$wrist_cm >= 12 & clean$wrist_cm <= 30))

## ---- w1-per-image ----
# Readings per image and agreement between readers on the same image
per_image <- aggregate(fracture_read ~ image_id, data = clean,
                       FUN = function(x) paste(x, collapse = "/"))
per_image$readings <- as.vector(table(clean$image_id)[per_image$image_id])
per_image$agree <- !grepl("yes", per_image$fracture_read) | !grepl("no", per_image$fracture_read)
per_image
stopifnot(sum(!per_image$agree) == 1, per_image$image_id[!per_image$agree] == "P02-A")

## ---- fig-w1-units ----
plot_data <- clean
plot_data$reader <- factor(plot_data$reader, levels = c("radiology", "emergency"))
p_units <- ggplot(plot_data, aes(x = person_id, y = image_id, shape = reader, colour = fracture_read)) +
  geom_point(size = 3.5, position = position_nudge(x = ifelse(plot_data$reader == "radiology", -0.12, 0.12))) +
  scale_colour_manual(values = c(yes = unname(cdsr_colours["red"]), no = unname(cdsr_colours["blue"]))) +
  scale_shape_manual(values = cdsr_shapes[1:2]) +
  labs(x = "Person (corrected identifier)", y = "Image", shape = "Reader", colour = "Fracture read",
       caption = "Synthetic register extract: 12 readings of 7 images from 5 people.") +
  theme_cdsr()
show_figure(p_units, "w1-units.png", height = 4)

## ---- w1-dictionary ----
# Data dictionary: key, unit, time zero and reason for collection for every column
dictionary <- data.frame(
  column = names(clean),
  meaning = c("row number of the export", "person identifier (key for person-level questions)",
              "image identifier (key for image-level questions)", "who read the image",
              "reader's judgement: fracture yes/no", "wrist circumference of the injured side",
              "age at presentation", "time from injury to imaging", "free-text note"),
  unit = c("-", "-", "-", "-", "-", "cm", "years", "hours", "-"),
  time_zero = c("-", "-", "-", "-", "time of reading", "time of imaging", "presentation",
                "injury (self-reported)", "-"),
  collected_for = c("export", "care", "care", "care", "care", "care", "care", "care", "care"),
  needed_for_teaching = c("no", "yes (pseudonym)", "yes (pseudonym)", "yes", "yes", "yes", "yes", "yes", "no")
)
dictionary

## ---- w1-rule-vs-learned ----
# Part C (unit 03): a fixed rule against a threshold learnt from training data (60 synthetic people)
set.seed(12)
m <- 60
swell <- data.frame(person_id = sprintf("S%02d", 1:m), fracture = rbinom(m, 1, 0.35))
# difference in wrist circumference, injured minus uninjured side, in cm (synthetic)
swell$diff_cm <- round(rnorm(m, mean = ifelse(swell$fracture == 1, 1.7, 0.6), sd = 0.65), 1)
swell$phase <- ifelse(seq_len(m) <= 40, "training", "application")
train <- swell[swell$phase == "training", ]
apply_set <- swell[swell$phase == "application", ]

# Rule: written down before seeing any data
rule_cm <- 1.0
# Learnt threshold: the cut-off that maximises sensitivity + specificity in the training people
candidates <- sort(unique(train$diff_cm))
youden <- sapply(candidates, function(t) {
  mean(train$diff_cm[train$fracture == 1] >= t) + mean(train$diff_cm[train$fracture == 0] < t) - 1
})
learnt_cm <- candidates[which.max(youden)]

evaluate <- function(threshold, d) {
  alert <- d$diff_cm >= threshold
  c(threshold = threshold, alerts = sum(alert), detected = sum(alert & d$fracture == 1),
    fractures = sum(d$fracture == 1), false_alerts = sum(alert & d$fracture == 0))
}
comparison <- rbind(rule = evaluate(rule_cm, apply_set), learnt = evaluate(learnt_cm, apply_set))
comparison
stopifnot(nrow(train) == 40, nrow(apply_set) == 20,
          length(intersect(train$person_id, apply_set$person_id)) == 0,
          learnt_cm == 1.3,
          all(comparison["rule", c("alerts", "detected", "fractures", "false_alerts")] == c(7, 5, 5, 2)),
          all(comparison["learnt", c("alerts", "detected", "fractures", "false_alerts")] == c(6, 4, 5, 2)))

## ---- fig-w1-threshold ----
p_thr <- ggplot(train, aes(x = diff_cm, y = factor(fracture, labels = c("no fracture", "fracture")))) +
  geom_point(position = position_jitter(height = 0.12, width = 0, seed = 1),
             colour = cdsr_colours[["blue"]], shape = cdsr_shapes[1], size = 2.2) +
  geom_vline(xintercept = rule_cm, linetype = cdsr_linetypes[2], colour = cdsr_colours[["grey"]]) +
  geom_vline(xintercept = learnt_cm, linetype = cdsr_linetypes[1], colour = cdsr_colours[["orange"]]) +
  annotate("text", x = rule_cm, y = 2.45, label = "rule", hjust = 1.1, colour = cdsr_colours[["grey"]]) +
  annotate("text", x = learnt_cm, y = 2.45, label = "learnt", hjust = -0.1, colour = cdsr_colours[["orange"]]) +
  labs(x = "Difference in wrist circumference, injured minus uninjured side (cm)", y = NULL,
       caption = "Synthetic training data, n = 40 people. Dashed line: fixed rule; solid line: learnt threshold.") +
  theme_cdsr()
show_figure(p_thr, "w1-threshold.png", height = 3.6)

## ---- w1-relearn ----
# Counter-check: learn the threshold again from a different set of 40 training people
set.seed(7)
other_train <- swell[sample(m, 40), ]
cand2 <- sort(unique(other_train$diff_cm))
learnt_again <- cand2[which.max(sapply(cand2, function(t) {
  mean(other_train$diff_cm[other_train$fracture == 1] >= t) +
    mean(other_train$diff_cm[other_train$fracture == 0] < t) - 1
}))]
c(first = learnt_cm, second = learnt_again)
stopifnot(learnt_again == 1.6)

## ---- w1-teaching-table ----
# Part D (unit 04): the reduced teaching table keeps only what the exercise needs
teaching <- clean[, c("person_id", "image_id", "reader", "fracture_read", "wrist_cm", "age_years",
                      "hours_since_injury")]
teaching$age_group <- cut(teaching$age_years, c(17, 39, 64, 79, 120),
                          labels = c("18-39", "40-64", "65-79", "80+"))
teaching$age_years <- NULL                       # exact age not needed for the exercise
stopifnot(!"free_text" %in% names(teaching), !"age_years" %in% names(teaching), nrow(teaching) == 12)
teaching

## ---- w1-save ----
save_csv(raw, "w1-register-raw.csv")
save_csv(clean[, names(clean) != "free_text"], "w1-register-corrected.csv")
save_csv(teaching, "w1-teaching-table.csv")
save_csv(dictionary, "w1-data-dictionary.csv")
