# Lab W5 · Collider, mixed model and clusters
# Clinical Data Science in R · Course, workshop W5 (teaching units 17-19)
# Fictitious spinal cord injury cohort. All data are synthetic; effect sizes, dropout mechanism and
# biomarker levels are teaching assumptions and describe no real patients or laboratory.
# Run in a fresh session from the repository root:  Rscript r/w5-causality-trajectories.R
# Stand-alone runs write figures and tables to ./output (or to the folder in CDSR_OUTPUT).

## ---- w5-setup ----
# Locate the folder with style.R and course.R, whatever the working directory
script_dir <- local({
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  here <- if (length(arg) == 1) dirname(gsub("~+~", " ", sub("^--file=", "", arg), fixed = TRUE)) else "."
  candidates <- c(here, "r", ".", "../r")
  candidates[file.exists(file.path(candidates, "style.R"))][1]
})
source(file.path(script_dir, "style.R"))
source(file.path(script_dir, "course.R"))

## ---- w5-packages ----
library(ggplot2)
library(dagitty)
library(lme4)
library(splines)

## ---- w5-dag ----
# Part A (unit 17): assumed causal structure. Severity and the biomarker both influence ICU admission;
# only severity influences the outcome. The biomarker has NO effect on the outcome by construction.
dag <- dagitty("dag {
  severity -> icu
  biomarker -> icu
  severity -> outcome
}")
coordinates(dag) <- list(x = c(severity = 0, biomarker = 2, icu = 1, outcome = 0),
                         y = c(severity = 0, biomarker = 0, icu = 1, outcome = 2))
# Which variables must be adjusted for to estimate the effect of the biomarker on the outcome?
adjustmentSets(dag, exposure = "biomarker", outcome = "outcome")
# Are biomarker and outcome independent ...
c(unconditional        = dseparated(dag, "biomarker", "outcome", list()),
  given_icu            = dseparated(dag, "biomarker", "outcome", "icu"),
  given_icu_and_severity = dseparated(dag, "biomarker", "outcome", c("icu", "severity")))
stopifnot(dseparated(dag, "biomarker", "outcome", list()),               # independent in everyone
          !dseparated(dag, "biomarker", "outcome", "icu"),               # selecting on ICU opens a path
          dseparated(dag, "biomarker", "outcome", c("icu", "severity")))  # adjusting for severity closes it

## ---- fig-w5-dag ----
nodes <- data.frame(name = c("severity", "biomarker", "icu", "outcome"),
                    label = c("Injury severity", "Serum biomarker", "ICU admission\n(selection)", "Motor score\nat 6 months"),
                    x = c(0, 2, 1, 0), y = c(2, 2, 1, 0))
edges <- data.frame(from = c("severity", "biomarker", "severity"), to = c("icu", "icu", "outcome"))
edges <- cbind(edges,
               setNames(nodes[match(edges$from, nodes$name), c("x", "y")], c("x", "y")),
               setNames(nodes[match(edges$to, nodes$name), c("x", "y")], c("xend", "yend")))
shorten <- function(a, b, by = 0.22) a + (b - a) * (1 - by / sqrt(sum((b - a)^2)))  # stop arrows before labels
for (i in seq_len(nrow(edges))) {
  end <- shorten(c(edges$x[i], edges$y[i]), c(edges$xend[i], edges$yend[i]))
  start <- shorten(c(edges$xend[i], edges$yend[i]), c(edges$x[i], edges$y[i]))
  edges[i, c("x", "y", "xend", "yend")] <- c(start, end)
}
p_dag <- ggplot() +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"), colour = cdsr_colours[["grey"]]) +
  geom_label(data = nodes, aes(x = x, y = y, label = label,
                               fill = ifelse(name == "icu", "selection", "variable")),
             size = 3.6, linewidth = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c(selection = "#F6D9A8", variable = "white")) +
  annotate("text", x = 1.05, y = 0.25, size = 3.3, colour = cdsr_colours[["red"]],
           label = "No arrow from biomarker to outcome:\nany association is not causal") +
  scale_x_continuous(limits = c(-0.5, 2.5)) + scale_y_continuous(limits = c(-0.3, 2.3)) +
  labs(caption = "Assumed causal structure for the teaching case (assumption, not a finding).") +
  theme_void(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, colour = "#4D4D4D", size = rel(0.8)))
show_figure(p_dag, "w5-dag.png", width = 7, height = 4.5)

## ---- w5-collider ----
# Simulate 5,000 patients according to the DAG
set.seed(20260517)
n <- 5000
cohort <- data.frame(id = sprintf("SCI%04d", seq_len(n)))
cohort$severity  <- rnorm(n)                                    # standardised severity score
cohort$biomarker <- round(rnorm(n, 80, 15), 1)                  # serum biomarker, µg/L (synthetic)
# Low biomarker values and high severity both prompt ICU admission
p_icu <- plogis(-1.2 + 1.6 * cohort$severity - 0.08 * (cohort$biomarker - 80))
cohort$icu <- rbinom(n, 1, p_icu)
# Outcome depends on severity only (points on a synthetic 0-100 motor scale)
cohort$motor_score <- round(pmin(100, pmax(0, 55 - 9 * cohort$severity + rnorm(n, 0, 8))), 1)

# Association of the biomarker with the outcome, per 10 µg/L
per10 <- function(fit) 10 * unname(coef(fit)["biomarker"])
icu_only <- cohort[cohort$icu == 1, ]
collider_table <- data.frame(
  analysis = c("All patients", "ICU patients only", "ICU patients only, adjusted for severity"),
  patients = c(nrow(cohort), nrow(icu_only), nrow(icu_only)),
  slope_per_10 = c(per10(lm(motor_score ~ biomarker, data = cohort)),
                   per10(lm(motor_score ~ biomarker, data = icu_only)),
                   per10(lm(motor_score ~ biomarker + severity, data = icu_only)))
)
print(collider_table, digits = 3)
stopifnot(
  abs(collider_table$slope_per_10[1]) < 0.3,   # no association in the whole population
  collider_table$slope_per_10[2] < -0.8,       # spurious negative association after selection on ICU
  abs(collider_table$slope_per_10[3]) < 0.3    # adjusting for severity closes the opened path
)

## ---- w5-collider-countercheck ----
# Counter-check: same patients, but ICU admission now depends on severity only
set.seed(20260520)
icu_severity_only <- rbinom(n, 1, plogis(-1.2 + 1.6 * cohort$severity))
slope_counter <- per10(lm(motor_score ~ biomarker, data = cohort[icu_severity_only == 1, ]))
slope_counter
stopifnot(abs(slope_counter) < 0.3)    # ICU is no longer a collider between biomarker and severity

## ---- fig-w5-collider ----
plot_df <- rbind(data.frame(panel = "All patients", cohort[, c("biomarker", "motor_score")]),
                 data.frame(panel = "ICU patients only", icu_only[, c("biomarker", "motor_score")]))
plot_df$panel <- factor(plot_df$panel, levels = c("All patients", "ICU patients only"))
p_collider <- ggplot(plot_df, aes(biomarker, motor_score)) +
  geom_point(alpha = 0.15, size = 0.8, colour = cdsr_colours[["grey"]]) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = cdsr_colours[["blue"]]) +
  facet_wrap(~ panel) +
  labs(x = "Serum biomarker (µg/L)", y = "Motor score at 6 months (points)",
       caption = sprintf("Synthetic cohort; n = %s patients in total, %s admitted to ICU. Line = linear fit with 95%% band.",
                         cdsr_number(nrow(cohort)), cdsr_number(nrow(icu_only)))) +
  theme_cdsr()
show_figure(p_collider, "w5-collider.png")

## ---- w5-trajectories ----
# Part B (unit 18): biomarker trajectories in 40 persons, measured on days 1, 3, 7, 14 and 28
set.seed(20260518)
days <- c(1, 3, 7, 14, 28)
persons <- data.frame(id = sprintf("P%02d", 1:40),
                      lesion = factor(rep(c("complete", "incomplete"), each = 20),
                                      levels = c("complete", "incomplete")))
persons$u0 <- rnorm(40, 0, 6)      # person-specific starting level, µg/L
persons$u1 <- rnorm(40, 0, 1.5)    # person-specific slope, µg/L per week
full <- merge(persons, data.frame(day = days))
full <- full[order(full$id, full$day), ]
full$week <- full$day / 7
mean_slope <- ifelse(full$lesion == "complete", 5, 8)            # initial rise, µg/L per week (assumption)
full$selenium <- round(60 + full$u0 + (mean_slope + full$u1) * full$week -
                         0.8 * full$week^2 + rnorm(nrow(full), 0, 3), 1)   # rise levels off (assumption)

# Informative dropout: the lower a person's (unobserved) day-28 value, the likelier the day-28 sample is missing
day28 <- full[full$day == 28, ]
p_missing <- plogis(-1.0 - 0.25 * (day28$selenium - median(day28$selenium)))
missing_ids <- day28$id[rbinom(nrow(day28), 1, p_missing) == 1]
full$observed <- !(full$day == 28 & full$id %in% missing_ids)
obs <- full[full$observed, c("id", "lesion", "day", "week", "selenium")]
c(persons = length(unique(obs$id)), observations = nrow(obs), missing_day28 = length(missing_ids))
table(lesion = persons$lesion, missing_day28 = persons$id %in% missing_ids)
stopifnot(length(unique(obs$id)) == 40, nrow(obs) == 200 - length(missing_ids),
          length(missing_ids) >= 8)

## ---- fig-w5-spaghetti ----
obs$dropped_out <- ifelse(obs$id %in% missing_ids, "day-28 sample missing", "complete series")
p_spaghetti <- ggplot(obs, aes(day, selenium, group = id, colour = dropped_out, linetype = dropped_out)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1) +
  facet_wrap(~ lesion, labeller = labeller(lesion = c(complete = "Complete lesion", incomplete = "Incomplete lesion"))) +
  scale_colour_manual(values = c("complete series" = cdsr_colours[["blue"]],
                                 "day-28 sample missing" = cdsr_colours[["red"]])) +
  scale_linetype_manual(values = c("complete series" = "solid", "day-28 sample missing" = "22")) +
  scale_x_continuous(breaks = days) +
  labs(x = "Day after injury", y = "Serum selenium (µg/L)", colour = NULL, linetype = NULL,
       caption = sprintf("Synthetic data; n = 40 persons (20 per group), %d observations; %d persons without a day-28 sample.",
                         nrow(obs), length(missing_ids))) +
  theme_cdsr()
show_figure(p_spaghetti, "w5-spaghetti.png")

## ---- w5-summary-measure ----
# Simple analysis: one least-squares slope per person (µg/L per week), then compare the groups
person_slopes <- do.call(rbind, lapply(split(obs, obs$id), function(d)
  data.frame(id = d$id[1], lesion = d$lesion[1], n_obs = nrow(d),
             slope = unname(coef(lm(selenium ~ week, data = d))["week"]))))
aggregate(slope ~ lesion, data = person_slopes, FUN = mean)
summary_test <- t.test(slope ~ lesion, data = person_slopes)
summary_diff <- unname(diff(summary_test$estimate))    # incomplete minus complete
summary_test

## ---- w5-mixed-model ----
# Linear mixed model: random intercept and slope per person, time-by-group interaction
fit_mixed <- lmer(selenium ~ week * lesion + (1 + week | id), data = obs)
stopifnot(!isSingular(fit_mixed))
summary(fit_mixed)$coefficients
interaction_est <- unname(fixef(fit_mixed)["week:lesionincomplete"])
interaction_ci  <- unname(confint(fit_mixed, parm = "week:lesionincomplete", method = "Wald")[1, ])
c(estimate = interaction_est, lower = interaction_ci[1], upper = interaction_ci[2])
stopifnot(interaction_ci[1] > 0)    # steeper rise with incomplete lesions in this synthetic cohort

## ---- w5-dropout ----
# What does the late dropout do to the day-28 mean? Because the data are synthetic, the values that
# would have been measured without dropout are known and serve as the reference.
# A straight line would distort the day-28 mean, so this model estimates one mean per day and group,
# with the same random intercept and slope per person.
fit_days <- lmer(selenium ~ factor(day) * lesion + (1 + week | id), data = obs)
stopifnot(!isSingular(fit_days))
grid28 <- data.frame(day = 28, week = 4,
                     lesion = factor(c("complete", "incomplete"), levels = levels(obs$lesion)))
day28_table <- data.frame(
  lesion = grid28$lesion,
  without_dropout = as.vector(tapply(full$selenium[full$day == 28], full$lesion[full$day == 28], mean)),
  completers_only = as.vector(tapply(obs$selenium[obs$day == 28], obs$lesion[obs$day == 28], mean)),
  mixed_model     = unname(predict(fit_days, newdata = grid28, re.form = NA))
)
print(day28_table, digits = 3, row.names = FALSE)
bias_completers <- with(day28_table, completers_only - without_dropout)
bias_mixed      <- with(day28_table, mixed_model - without_dropout)
stopifnot(all(bias_completers > 0),                       # completers look better than everyone
          all(abs(bias_mixed) < bias_completers))         # using all earlier measurements helps

## ---- w5-sensitivity ----
# Sensitivity analysis (delta adjustment): missing day-28 values are filled with each person's own
# prediction (including their random effects) minus delta, i.e. "those without a day-28 sample did
# delta µg/L worse than their earlier values suggest".
missing_rows <- full[!full$observed, c("id", "lesion", "day", "week")]
pred_missing <- predict(fit_days, newdata = missing_rows)
sensitivity_table <- do.call(rbind, lapply(c(0, 5, 10), function(delta) {
  filled <- rbind(obs[, c("id", "lesion", "day", "week", "selenium")],
                  cbind(missing_rows, selenium = pred_missing - delta))
  refit <- lmer(selenium ~ week * lesion + (1 + week | id), data = filled)
  data.frame(delta = delta,
             mean_day28_complete   = mean(filled$selenium[filled$day == 28 & filled$lesion == "complete"]),
             mean_day28_incomplete = mean(filled$selenium[filled$day == 28 & filled$lesion == "incomplete"]),
             interaction = unname(fixef(refit)["week:lesionincomplete"]))
}))
print(sensitivity_table, digits = 3, row.names = FALSE)
stopifnot(all(diff(sensitivity_table$mean_day28_complete) < 0),
          all(sensitivity_table$interaction > 0))

## ---- w5-cluster-data ----
# Part C, cluster station (unit 19): 300 patients lie on ONE continuous gradient (e.g. inflammatory
# activity) that drives three markers; a fourth marker is pure noise on a large numerical scale
set.seed(20260519)
m <- 300
gradient <- runif(m)
markers <- data.frame(
  marker_a = 10 + 30 * gradient + rnorm(m, 0, 3),      # mg/L
  marker_b = 200 - 80 * gradient + rnorm(m, 0, 10),    # U/L
  marker_c = 2 + 1.5 * gradient + rnorm(m, 0, 0.2),    # ratio
  marker_d = rnorm(m, 500, 120)                        # unrelated, large scale
)
round(sapply(markers, sd), 1)

## ---- w5-cluster-kmeans ----
# k-means with k = 3 on raw values and on standardised values
set.seed(1)
km_raw    <- kmeans(markers, centers = 3, nstart = 25)
set.seed(1)
km_scaled <- kmeans(scale(markers), centers = 3, nstart = 25)
ari_raw_scaled <- mclust::adjustedRandIndex(km_raw$cluster, km_scaled$cluster)
# Are the "clusters" just cuts of the gradient? Rank correlation of cluster (ordered by mean gradient) with the gradient
order_by_gradient <- function(cl) rank(tapply(gradient, cl, mean))[cl]
rho_scaled <- cor(order_by_gradient(km_scaled$cluster), gradient, method = "spearman")
rho_raw    <- cor(order_by_gradient(km_raw$cluster), gradient, method = "spearman")
c(adjusted_rand_raw_vs_scaled = ari_raw_scaled, rho_scaled = rho_scaled, rho_raw = rho_raw)
# Bootstrap stability of the standardised solution: recluster resamples, compare on the resampled patients
set.seed(2)
z <- scale(markers)
boot_ari <- replicate(50, {
  idx <- sample(m, replace = TRUE)
  km_b <- kmeans(z[idx, ], centers = 3, nstart = 10)
  mclust::adjustedRandIndex(km_b$cluster, km_scaled$cluster[idx])
})
summary(boot_ari)
# Mean silhouette width: values below about 0.5 do not indicate well-separated groups
sil <- cluster::silhouette(km_scaled$cluster, dist(z))
mean_silhouette <- mean(sil[, "sil_width"])
mean_silhouette
stopifnot(ari_raw_scaled < 0.2,      # the two scalings give unrelated partitions
          rho_raw < 0.3,             # raw-scale clusters follow the large-scale noise marker
          rho_scaled > 0.85,         # standardised clusters are essentially slices of the gradient
          median(boot_ari) > 0.5,    # moderately reproducible, which still would not prove distinct groups
          mean_silhouette < 0.4)     # weak separation

## ---- w5-cluster-countercheck ----
# Counter-check: drop the unrelated marker and cluster the three gradient markers only
set.seed(1)
km_three <- kmeans(scale(markers[, c("marker_a", "marker_b", "marker_c")]), centers = 3, nstart = 25)
rho_three <- cor(order_by_gradient(km_three$cluster), gradient, method = "spearman")
rho_three
stopifnot(rho_three > rho_scaled)      # without the noise marker the clusters follow the gradient even more closely

## ---- fig-w5-clusters ----
pca <- prcomp(z)
cl_df <- data.frame(pc1 = pca$x[, 1], gradient = gradient,
                    scaled = factor(order_by_gradient(km_scaled$cluster)),
                    raw = factor(order_by_gradient(km_raw$cluster)))
cl_long <- rbind(data.frame(scaling = "Standardised markers", cl_df[, c("pc1", "gradient")], cluster = cl_df$scaled),
                 data.frame(scaling = "Raw markers", cl_df[, c("pc1", "gradient")], cluster = cl_df$raw))
cl_long$scaling <- factor(cl_long$scaling, levels = c("Standardised markers", "Raw markers"))
p_clusters <- ggplot(cl_long, aes(gradient, pc1, colour = cluster, shape = cluster)) +
  geom_point(alpha = 0.8, size = 1.6) +
  facet_wrap(~ scaling) +
  scale_colour_manual(values = unname(cdsr_colours[c("blue", "orange", "green")])) +
  scale_shape_manual(values = cdsr_shapes[1:3]) +
  labs(x = "True position on the gradient (0 to 1, known only because the data are synthetic)",
       y = "First principal component", colour = "k-means cluster", shape = "k-means cluster",
       caption = "Synthetic data; n = 300 patients, four markers, k = 3; clusters numbered by mean gradient position.") +
  theme_cdsr()
show_figure(p_clusters, "w5-clusters.png")

## ---- w5-spline ----
# Part C, longitudinal station (unit 19): straight line versus natural spline for time, fitted by
# maximum likelihood on the observed data from Part B with the same random effects
fit_linear <- lmer(selenium ~ week * lesion + (1 + week | id), data = obs, REML = FALSE)
fit_spline <- lmer(selenium ~ ns(week, df = 3) * lesion + (1 + week | id), data = obs, REML = FALSE)
stopifnot(!isSingular(fit_linear), !isSingular(fit_spline))
aic_table <- data.frame(model = c("Linear time", "Natural spline, 3 df"),
                        parameters = c(attr(logLik(fit_linear), "df"), attr(logLik(fit_spline), "df")),
                        AIC = c(AIC(fit_linear), AIC(fit_spline)))
print(aic_table, digits = 4, row.names = FALSE)
stopifnot(aic_table$AIC[2] < aic_table$AIC[1])     # the curvature is worth its extra parameters

## ---- fig-w5-spline ----
grid <- expand.grid(week = seq(1, 28, by = 0.5) / 7, lesion = levels(obs$lesion))
grid$lesion <- factor(grid$lesion, levels = levels(obs$lesion))
curves <- rbind(data.frame(model = "Linear time", grid, fit = predict(fit_linear, grid, re.form = NA)),
                data.frame(model = "Natural spline, 3 df", grid, fit = predict(fit_spline, grid, re.form = NA)))
obs_means <- aggregate(selenium ~ lesion + day, data = obs, FUN = mean)
p_spline <- ggplot() +
  geom_line(data = curves, aes(week * 7, fit, colour = model, linetype = model), linewidth = 0.9) +
  geom_point(data = obs_means, aes(day, selenium), colour = "black", size = 2) +
  facet_wrap(~ lesion, labeller = labeller(lesion = c(complete = "Complete lesion", incomplete = "Incomplete lesion"))) +
  scale_colour_manual(values = unname(cdsr_colours[c("orange", "blue")])) +
  scale_linetype_manual(values = cdsr_linetypes[c(2, 1)]) +
  scale_x_continuous(breaks = days) +
  labs(x = "Day after injury", y = "Serum selenium (µg/L)", colour = NULL, linetype = NULL,
       caption = sprintf("Synthetic data; n = 40 persons, %d observations. Points = observed means (day 28: completers only).", nrow(obs))) +
  theme_cdsr()
show_figure(p_spline, "w5-spline.png")

## ---- w5-save ----
save_csv(cohort, "w5-collider-cohort.csv")
save_csv(obs[, c("id", "lesion", "day", "selenium")], "w5-trajectories.csv")
save_csv(cbind(id = sprintf("M%03d", seq_len(m)), round(markers, 2)), "w5-markers.csv")
