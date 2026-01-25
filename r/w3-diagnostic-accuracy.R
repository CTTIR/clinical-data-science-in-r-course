# Lab W3 · Two-by-two tables, baseline and threshold
# Clinical Data Science in R · Course, workshop W3 (teaching units 09-11)
# All data are synthetic. Sensitivity, specificity and prevalence are teaching assumptions.
# Run in a fresh session from the repository root:  Rscript r/w3-diagnostic-accuracy.R
# Stand-alone runs write figures and tables to ./output (or to the folder in CDSR_OUTPUT).

## ---- w3-setup ----
# Locate the folder with style.R and course.R, whatever the working directory
script_dir <- local({
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  here <- if (length(arg) == 1) dirname(gsub("~+~", " ", sub("^--file=", "", arg), fixed = TRUE)) else "."
  candidates <- c(here, "r", ".", "../r")
  candidates[file.exists(file.path(candidates, "style.R"))][1]
})
source(file.path(script_dir, "style.R"))
source(file.path(script_dir, "course.R"))

## ---- w3-packages ----
library(ggplot2)
library(glmnet)

## ---- w3-two-by-two ----
# Part A (unit 09): 1,000 examinations in each of two referral settings
scenarios <- data.frame(
  setting     = c("Low prevalence (5%)", "Higher prevalence (20%)"),
  n           = c(1000, 1000),
  prevalence  = c(0.05, 0.20),
  sensitivity = c(0.90, 0.90),   # assumption, identical in both settings
  specificity = c(0.90, 0.90)    # assumption, identical in both settings
)
scenarios$fracture    <- scenarios$n * scenarios$prevalence
scenarios$no_fracture <- scenarios$n - scenarios$fracture
scenarios$tp <- scenarios$sensitivity * scenarios$fracture       # true alerts
scenarios$fn <- scenarios$fracture - scenarios$tp                # missed fractures
scenarios$tn <- scenarios$specificity * scenarios$no_fracture    # correct all-clear
scenarios$fp <- scenarios$no_fracture - scenarios$tn             # false alerts
scenarios$alerts <- scenarios$tp + scenarios$fp
scenarios$ppv <- scenarios$tp / scenarios$alerts                 # share of alerts that are fractures
scenarios$npv <- scenarios$tn / (scenarios$tn + scenarios$fn)    # share of all-clears without fracture
print(scenarios[, c("setting", "tp", "fp", "fn", "tn", "alerts", "ppv", "npv")], digits = 3)

# Target values of the teaching case: TP/FP/FN/TN = 45/95/5/855 and 180/80/20/720
stopifnot(
  isTRUE(all.equal(scenarios$tp, c(45, 180))),
  isTRUE(all.equal(scenarios$fp, c(95, 80))),
  isTRUE(all.equal(scenarios$fn, c(5, 20))),
  isTRUE(all.equal(scenarios$tn, c(855, 720))),
  isTRUE(all.equal(scenarios$alerts, c(140, 260))),
  identical(cdsr_percent(scenarios$ppv), c("32.1%", "69.2%"))
)

## ---- fig-w3-alerts ----
alerts_long <- data.frame(
  setting = factor(rep(scenarios$setting, 2), levels = scenarios$setting),
  type    = factor(rep(c("Fracture present (true alert)", "No fracture (false alert)"), each = 2),
                   levels = c("No fracture (false alert)", "Fracture present (true alert)")),
  count   = c(scenarios$tp, scenarios$fp)
)
p_alerts <- ggplot(alerts_long, aes(x = setting, y = count, fill = type)) +
  geom_col(width = 0.6, colour = "white") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), colour = "white", fontface = "bold") +
  scale_fill_manual(values = c("No fracture (false alert)" = unname(cdsr_colours["grey"]),
                               "Fracture present (true alert)" = unname(cdsr_colours["blue"]))) +
  labs(x = NULL, y = "Alerts per 1,000 examinations", fill = NULL,
       caption = "Synthetic teaching case; sensitivity and specificity set to 90% (assumption). n = 1,000 examinations per setting.") +
  theme_cdsr()
show_figure(p_alerts, "w3-alerts.png")

## ---- w3-countercheck ----
# Counter-check: predict first, then compute. Specificity rises from 90% to 95% at 5% prevalence.
fp_new  <- (1 - 0.95) * 950
ppv_new <- 45 / (45 + fp_new)
cat("False alerts:", fp_new, " PPV:", cdsr_percent(ppv_new), "\n")
stopifnot(isTRUE(all.equal(fp_new, 47.5)), identical(cdsr_percent(ppv_new), "48.6%"))

## ---- w3-cohort ----
# Part B (unit 10): synthetic emergency department admissions with suspected wrist fracture
set.seed(20260309)
n <- 2000
cohort <- data.frame(id = sprintf("ED%04d", seq_len(n)))
cohort$age <- round(runif(n, 18, 95))                                  # years
cohort$age_group <- factor(ifelse(cohort$age >= 75, "75 and older", "18 to 74"),
                           levels = c("18 to 74", "75 and older"))
cohort$fall_outstretched <- rbinom(n, 1, 0.55)                         # fall on outstretched hand (0/1)
# Reference standard (fracture yes/no) depends on age and mechanism
lp <- -2.6 + 0.02 * (cohort$age - 50) + 1.0 * cohort$fall_outstretched
cohort$fracture <- rbinom(n, 1, plogis(lp))
# Clinical signs: in older patients both signs are weaker indicators (synthetic assumption)
older <- cohort$age_group == "75 and older"
cohort$snuffbox_tender <- rbinom(n, 1, ifelse(cohort$fracture == 1, ifelse(older, 0.55, 0.80), 0.25))
cohort$swelling        <- rbinom(n, 1, ifelse(cohort$fracture == 1, ifelse(older, 0.45, 0.80), 0.20))
cohort$grip_kg <- round(rnorm(n, 32 - 0.15 * (cohort$age - 50) - 5 * cohort$fracture, 6), 1)
cohort$grip_kg[sample(n, 100)] <- NA                                   # 5% not measured
cohort$noise <- round(rnorm(n), 2)                                     # unrelated variable

# Fixed test set: 600 admissions drawn once and stored with the data
test_ids <- sort(sample(cohort$id, 600))
cohort$split <- ifelse(cohort$id %in% test_ids, "test", "train")
train <- cohort[cohort$split == "train", ]
test  <- cohort[cohort$split == "test", ]
stopifnot(nrow(train) == 1400, nrow(test) == 600,
          length(intersect(train$id, test$id)) == 0)
table(split = cohort$split, fracture = cohort$fracture)

## ---- w3-preprocess ----
# Pre-processing uses training data only: the training median replaces missing grip strength
grip_median_train <- median(train$grip_kg, na.rm = TRUE)
impute_grip <- function(d) {
  d$grip_missing <- as.integer(is.na(d$grip_kg))
  d$grip_kg[is.na(d$grip_kg)] <- grip_median_train
  d
}
train <- impute_grip(train)
test  <- impute_grip(test)
grip_median_train

## ---- w3-models ----
# Null model: everyone receives the training prevalence
p_null <- mean(train$fracture)
# Baseline: logistic regression with four pre-specified predictors
baseline <- glm(fracture ~ age + fall_outstretched + snuffbox_tender + swelling,
                family = binomial, data = train)
# Alternative: elastic net with all variables and their pairwise interactions;
# the penalty is tuned by 5-fold cross-validation inside the training data only
f_alt <- ~ (age + fall_outstretched + snuffbox_tender + swelling + grip_kg + grip_missing + noise)^2
x_train <- model.matrix(f_alt, train)[, -1]
x_test  <- model.matrix(f_alt, test)[, -1]
fold_id <- rep_len(1:5, nrow(train))[order(train$id)]      # fixed, reproducible folds
alt_fit <- cv.glmnet(x_train, train$fracture, family = "binomial", alpha = 0.5, foldid = fold_id)

test$p_null     <- p_null
test$p_baseline <- predict(baseline, newdata = test, type = "response")
test$p_alt      <- as.numeric(predict(alt_fit, newx = x_test, s = "lambda.1se", type = "response"))

# Performance on the fixed test set
brier <- function(p, y) mean((p - y)^2)
calib <- function(p, y) {                       # calibration intercept and slope
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  c(intercept = unname(coef(glm(y ~ offset(lp), family = binomial))[1]),
    slope = unname(coef(glm(y ~ lp, family = binomial))[2]))
}
results <- data.frame(
  model = c("Null model (training prevalence)", "Baseline: logistic regression, 4 predictors",
            "Alternative: elastic net with interactions"),
  n_test = nrow(test), fractures_test = sum(test$fracture),
  auc   = c(0.5, auc_rank(test$p_baseline, test$fracture), auc_rank(test$p_alt, test$fracture)),
  brier = c(brier(test$p_null, test$fracture), brier(test$p_baseline, test$fracture),
            brier(test$p_alt, test$fracture)),
  calib_slope = c(NA, calib(test$p_baseline, test$fracture)["slope"],
                  calib(test$p_alt, test$fracture)["slope"])
)
print(results, digits = 3)
stopifnot(
  results$n_test[1] == 600,
  all(results$auc[2:3] > 0.65 & results$auc[2:3] < 0.95),
  all(results$brier[2:3] < results$brier[1]),       # both models beat the null model
  abs(results$auc[3] - results$auc[2]) < 0.05        # small gain in ranking for considerably more effort
)

## ---- fig-w3-calibration ----
# Part C (unit 11): calibration by fifths of predicted risk in the test set
calib_bins <- function(p, y, model) {
  bin <- cut(p, quantile(p, probs = seq(0, 1, 0.2)), include.lowest = TRUE, labels = FALSE)
  do.call(rbind, lapply(split(data.frame(p, y), bin), function(d) {
    ci <- prop_ci(sum(d$y), nrow(d))
    data.frame(model = model, predicted = mean(d$p), observed = ci[["estimate"]],
               lower = ci[["lower"]], upper = ci[["upper"]], n = nrow(d))
  }))
}
calib_df <- rbind(calib_bins(test$p_baseline, test$fracture, "Baseline"),
                  calib_bins(test$p_alt, test$fracture, "Elastic net"))
p_calib <- ggplot(calib_df, aes(predicted, observed, colour = model, shape = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", colour = "grey50") +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_line(aes(linetype = model)) +
  scale_colour_manual(values = unname(cdsr_colours[c("blue", "orange")])) +
  scale_shape_manual(values = cdsr_shapes[1:2]) +
  scale_linetype_manual(values = cdsr_linetypes[1:2]) +
  scale_x_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(x = "Mean predicted risk in the group", y = "Observed share with fracture (95% CI)",
       colour = NULL, shape = NULL, linetype = NULL,
       caption = "Synthetic test set, n = 600 admissions in five groups of 120 per model; dashed line = perfect calibration.") +
  theme_cdsr()
show_figure(p_calib, "w3-calibration.png")

## ---- w3-thresholds ----
# Moving the threshold for the baseline model; 30 wrist examinations per shift (assumption)
per_shift <- 30
threshold_table <- do.call(rbind, lapply(c(0.10, 0.20, 0.30, 0.40), function(t) {
  alert <- test$p_baseline >= t
  y <- test$fracture == 1
  data.frame(threshold = t, alerts = sum(alert), tp = sum(alert & y), fp = sum(alert & !y),
             fn = sum(!alert & y), sensitivity = mean(alert[y]), specificity = mean(!alert[!y]),
             ppv = sum(alert & y) / sum(alert),
             false_alerts_per_shift = per_shift * sum(alert & !y) / nrow(test))
}))
print(threshold_table, digits = 3)
stopifnot(all(diff(threshold_table$alerts) <= 0),        # higher threshold, fewer alerts
          all(diff(threshold_table$sensitivity) <= 0))   # ... and fewer fractures detected

## ---- w3-subgroups ----
# Errors by age group at the example threshold of 20%, with denominators and exact 95% intervals
threshold <- 0.20
test$alert <- test$p_baseline >= threshold
subgroup_table <- do.call(rbind, lapply(split(test, test$age_group), function(d) {
  y <- d$fracture == 1
  data.frame(age_group = d$age_group[1], admissions = nrow(d), fractures = sum(y),
             detected = format_prop(sum(d$alert & y), sum(y)),
             missed = sum(!d$alert & y),
             false_alerts = format_prop(sum(d$alert & !y), sum(!y)))
}))
print(subgroup_table, row.names = FALSE)
sens_by_group <- tapply(test$alert[test$fracture == 1], test$age_group[test$fracture == 1], mean)
stopifnot(sens_by_group[["75 and older"]] < sens_by_group[["18 to 74"]])

## ---- w3-save ----
save_csv(cohort, "w3-admissions.csv")
save_csv(scenarios, "w3-two-by-two.csv")
