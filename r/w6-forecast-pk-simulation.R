# Lab W6 · Forecast, PK, queue and priors
# Clinical Data Science in R · Course, workshop W6 (teaching units 21-23)
# All data are synthetic. Pharmacokinetic parameters are virtual: no drug, no dosing recommendation.
# Run in a fresh session from the repository root:  Rscript r/w6-forecast-pk-simulation.R
# Stand-alone runs write figures and tables to ./output (or to the folder in CDSR_OUTPUT).

## ---- w6-setup ----
# Locate the folder with style.R and course.R, whatever the working directory
script_dir <- local({
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  here <- if (length(arg) == 1) dirname(gsub("~+~", " ", sub("^--file=", "", arg), fixed = TRUE)) else "."
  candidates <- c(here, "r", ".", "../r")
  candidates[file.exists(file.path(candidates, "style.R"))][1]
})
source(file.path(script_dir, "style.R"))
source(file.path(script_dir, "course.R"))

## ---- w6-packages ----
library(ggplot2)
library(deSolve)
library(simmer)

## ---- w6-arrivals ----
# Part A (unit 21): synthetic hourly arrivals at an emergency department over 16 weeks
set.seed(20260321)
n_weeks <- 16
hour_index <- 0:(n_weeks * 168 - 1)                       # 168 hours per week
ed <- data.frame(
  index        = hour_index + 1,
  week         = hour_index %/% 168 + 1,
  hour_of_week = hour_index %% 168 + 1,
  day          = hour_index %/% 24 + 1,
  hour         = hour_index %% 24                         # 0 = midnight to 1 a.m.
)
weekday_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
ed$weekday <- factor(weekday_names[(ed$day - 1) %% 7 + 1], levels = weekday_names)
# Expected arrivals per hour: daily profile x weekday factor x winter wave (all assumptions)
daily_profile  <- 2 + 7 * exp(-(ed$hour - 13)^2 / (2 * 4^2))          # low at night, peak early afternoon
weekday_factor <- c(1.15, 1.05, 1.00, 1.00, 1.05, 0.90, 0.85)[as.integer(ed$weekday)]
wave_factor    <- ifelse(ed$week >= 13, 1.25, 1)                       # +25% from week 13
ed$expected <- daily_profile * weekday_factor * wave_factor
ed$arrivals <- rpois(nrow(ed), ed$expected)
ed$shift <- factor(ifelse(ed$hour >= 22 | ed$hour < 6, "Night (22-06)",
                          ifelse(ed$hour < 14, "Early (06-14)", "Late (14-22)")),
                   levels = c("Night (22-06)", "Early (06-14)", "Late (14-22)"))
stopifnot(nrow(ed) == 2688, sum(ed$arrivals) > 0)
daily_totals <- aggregate(arrivals ~ day + week, data = ed, FUN = sum)
summary(daily_totals$arrivals)

## ---- fig-w6-arrivals ----
p_arrivals <- ggplot(daily_totals, aes(day, arrivals)) +
  geom_vline(xintercept = 12 * 7 + 0.5, linetype = "22", colour = cdsr_colours[["grey"]]) +
  annotate("text", x = 12 * 7 - 0.5, y = max(daily_totals$arrivals), hjust = 1, size = 3.5,
           label = "Winter wave from week 13 (assumption) →") +
  geom_line(colour = cdsr_colours[["blue"]]) +
  geom_point(colour = cdsr_colours[["blue"]], size = 1) +
  scale_x_continuous(breaks = seq(1, 113, by = 14), name = "Day of the observation period") +
  labs(y = "Arrivals per day",
       caption = "Synthetic emergency department; n = 112 days (2,688 hours) from one fictitious department.") +
  theme_cdsr()
show_figure(p_arrivals, "w6-arrivals.png")

## ---- w6-forecast ----
# Two forecasting rules for the next 168 hours; both see only the history before the origin
forecast_week <- function(history, method) {
  n <- length(history)
  if (method == "Same hour last week") {
    history[(n - 167):n]                                          # seasonal naive baseline
  } else {
    rowMeans(matrix(history[(n - 4 * 168 + 1):n], nrow = 168))    # mean of the same hour, last 4 weeks
  }
}
# Errors of the same rule in the four weeks before the origin give an empirical 80% interval
past_errors <- function(history, method) {
  n <- length(history)
  unlist(lapply(1:4, function(k) {
    cut <- n - k * 168
    history[(cut + 1):(cut + 168)] - forecast_week(history[seq_len(cut)], method)
  }))
}
methods <- c("Same hour last week", "Mean of last 4 weeks")
origins <- 9:16                                                    # forecast origin = Monday 00:00 of week 9 ... 16
forecasts <- do.call(rbind, lapply(origins, function(w) {
  origin <- (w - 1) * 168 + 1                                      # index of the first forecast hour
  history <- ed$arrivals[seq_len(origin - 1)]                      # nothing at or after the origin
  do.call(rbind, lapply(methods, function(m) {
    point <- forecast_week(history, m)
    q <- quantile(past_errors(history, m), c(0.10, 0.90), names = FALSE)
    data.frame(method = m, origin_week = w, horizon = 1:168,
               target_index = origin:(origin + 167), history_end = length(history),
               forecast = point, lower = pmax(point + q[1], 0), upper = point + q[2])
  }))
}))
forecasts$observed <- ed$arrivals[forecasts$target_index]
forecasts$shift    <- ed$shift[forecasts$target_index]
forecasts$period   <- ifelse(forecasts$origin_week >= 13, "Weeks 13-16 (wave)", "Weeks 9-12 (before wave)")
forecasts$error    <- forecasts$observed - forecasts$forecast      # positive = under-forecast
forecasts$covered  <- forecasts$observed >= forecasts$lower & forecasts$observed <= forecasts$upper
forecasts$horizon_band <- cut(forecasts$horizon, c(0, 24, 72, 168),
                              labels = c("1-24 h", "25-72 h", "73-168 h"))
# The forecast never uses data from the forecast window or later
stopifnot(all(forecasts$history_end < forecasts$target_index),
          nrow(forecasts) == length(origins) * length(methods) * 168)

## ---- w6-evaluation ----
evaluate <- function(d) data.frame(hours = nrow(d), mae = mean(abs(d$error)),
                                   mean_error = mean(d$error), coverage_80 = mean(d$covered))
eval_period <- do.call(rbind, lapply(split(forecasts, list(forecasts$method, forecasts$period)), function(d)
  cbind(method = d$method[1], period = d$period[1], evaluate(d))))
eval_horizon <- do.call(rbind, lapply(split(forecasts, list(forecasts$method, forecasts$horizon_band)), function(d)
  cbind(method = d$method[1], horizon = as.character(d$horizon_band[1]), evaluate(d))))
eval_period  <- eval_period[order(eval_period$method, eval_period$period), ]
eval_horizon <- eval_horizon[order(eval_horizon$method, eval_horizon$horizon), ]
rownames(eval_period) <- rownames(eval_horizon) <- NULL
print(eval_period, digits = 3)
print(eval_horizon, digits = 3)

# Under-forecast per 8-hour shift during the wave: missing arrivals per shift, averaged over shifts
wave <- forecasts[forecasts$period == "Weeks 13-16 (wave)", ]
shift_gap <- aggregate(error ~ method + shift, data = wave, FUN = function(e) sum(e) / (length(e) / 8))
names(shift_gap)[3] <- "under_forecast_per_shift"
print(shift_gap, digits = 3)

stopifnot(
  all(eval_period$hours == 4 * 168),                                   # each cell: 672 forecast hours
  sum(eval_horizon$hours) == 2 * 8 * 168,                              # 2 methods x 8 weeks x 168 hours
  # before the wave, averaging four weeks beats last week's value
  eval_period$mae[eval_period$method == "Mean of last 4 weeks" & eval_period$period == "Weeks 9-12 (before wave)"] <
    eval_period$mae[eval_period$method == "Same hour last week" & eval_period$period == "Weeks 9-12 (before wave)"],
  # during the wave, both rules under-forecast on average
  all(eval_period$mean_error[eval_period$period == "Weeks 13-16 (wave)"] > 0)
)
# The largest gap per shift: the late shift with the 4-week mean
gap_mean4 <- shift_gap[shift_gap$method == "Mean of last 4 weeks", ]
stopifnot(as.character(gap_mean4$shift[which.max(gap_mean4$under_forecast_per_shift)]) == "Late (14-22)")

## ---- w6-forecast-countercheck ----
# Counter-check: mean error (observed - forecast) per forecast week. Which rule recovers after the jump?
by_week <- aggregate(error ~ origin_week + method, data = forecasts, FUN = mean)
by_week <- reshape(by_week, idvar = "origin_week", timevar = "method", direction = "wide")
names(by_week) <- c("forecast_week", "mean_error_4_week_mean", "mean_error_last_week")
print(by_week[by_week$forecast_week >= 12, ], digits = 2, row.names = FALSE)
# From week 14 on, last week's value already contains the wave; the 4-week mean still lags
stopifnot(all(by_week$mean_error_last_week[by_week$forecast_week >= 14] <
              by_week$mean_error_4_week_mean[by_week$forecast_week >= 14]))

## ---- fig-w6-forecast-week ----
week13 <- forecasts[forecasts$origin_week == 13, ]
week13$hour_label <- week13$horizon
p_forecast <- ggplot(week13, aes(horizon)) +
  geom_ribbon(data = week13[week13$method == "Mean of last 4 weeks", ],
              aes(ymin = lower, ymax = upper), fill = cdsr_colours[["orange"]], alpha = 0.2) +
  geom_point(data = week13[week13$method == "Mean of last 4 weeks", ], aes(y = observed),
             colour = "grey30", size = 0.8) +
  geom_line(aes(y = forecast, colour = method, linetype = method)) +
  scale_colour_manual(values = unname(cdsr_colours[c("orange", "blue")])) +
  scale_linetype_manual(values = cdsr_linetypes[1:2]) +
  scale_x_continuous(breaks = seq(1, 169, by = 24), labels = weekday_names[c(1:7, 1)][1:8],
                     name = "Hour of the forecast week (week 13, first week of the wave)") +
  labs(y = "Arrivals per hour", colour = NULL, linetype = NULL,
       caption = "Points: observed arrivals; band: 80% interval of the 4-week mean. Synthetic data; n = 168 hours.") +
  theme_cdsr()
show_figure(p_forecast, "w6-forecast-week.png")

## ---- w6-pk ----
# Part B, station PK (unit 22): one-compartment model after an intravenous bolus.
# Virtual parameters for a teaching exercise only (no drug, no dosing recommendation).
dose  <- 100                          # mg, virtual amount
base  <- c(CL = 5, V = 50)            # clearance in L/h, volume of distribution in L
times <- seq(0, 24, by = 0.5)         # h, identical time points for both methods
tolerance <- 1e-6                     # maximal relative difference, fixed before computing

analytic_conc <- function(p) dose / p[["V"]] * exp(-p[["CL"]] / p[["V"]] * times)   # mg/L
one_compartment <- function(t, state, p) list(-p[["CL"]] / p[["V"]] * state[["A"]])  # dA/dt in mg/h
ode_conc <- function(p) {
  out <- ode(y = c(A = dose), times = times, func = one_compartment, parms = p,
             method = "lsoda", rtol = 1e-10, atol = 1e-12)
  out[, "A"] / p[["V"]]                                                              # amount / volume
}
c_ode <- ode_conc(base)
c_ana <- analytic_conc(base)
max_rel_diff <- max(abs(c_ode - c_ana) / c_ana)
half_life <- log(2) * base[["V"]] / base[["CL"]]                                     # h
auc_inf   <- dose / base[["CL"]]                                                     # mg*h/L
cat("Max. relative difference ODE vs analytic:", signif(max_rel_diff, 2),
    "\nHalf-life:", round(half_life, 2), "h   C(0):", dose / base[["V"]], "mg/L   AUC(0-inf):", auc_inf, "mg*h/L\n")
stopifnot(max_rel_diff < tolerance, round(half_life, 2) == 6.93,
          isTRUE(all.equal(c_ana[1], 2)), isTRUE(all.equal(auc_inf, 20)))

# Vary one parameter at a time
pk_scenarios <- data.frame(
  varied = c("Reference", "Clearance", "Clearance", "Volume", "Volume"),
  CL     = c(5, 2.5, 10, 5, 5),
  V      = c(50, 50, 50, 25, 100)
)
pk_scenarios$c0_mg_per_l   <- dose / pk_scenarios$V
pk_scenarios$half_life_h   <- log(2) * pk_scenarios$V / pk_scenarios$CL
pk_scenarios$c12_mg_per_l  <- dose / pk_scenarios$V * exp(-pk_scenarios$CL / pk_scenarios$V * 12)
pk_scenarios$auc_mg_h_per_l <- dose / pk_scenarios$CL
print(pk_scenarios, digits = 3)
pk_curves <- do.call(rbind, lapply(seq_len(nrow(pk_scenarios)), function(i) {
  p <- c(CL = pk_scenarios$CL[i], V = pk_scenarios$V[i])
  data.frame(scenario = sprintf("CL %s L/h, V %s L", p[["CL"]], p[["V"]]), time_h = times,
             conc_ode = ode_conc(p), conc_analytic = analytic_conc(p))
}))
stopifnot(max(abs(pk_curves$conc_ode - pk_curves$conc_analytic) / pk_curves$conc_analytic) < tolerance)

## ---- fig-w6-pk ----
curve_panels <- rbind(
  cbind(pk_curves[pk_curves$scenario %in% c("CL 2.5 L/h, V 50 L", "CL 5 L/h, V 50 L", "CL 10 L/h, V 50 L"), ],
        panel = "Clearance varied (V = 50 L)"),
  cbind(pk_curves[pk_curves$scenario %in% c("CL 5 L/h, V 25 L", "CL 5 L/h, V 50 L", "CL 5 L/h, V 100 L"), ],
        panel = "Volume varied (CL = 5 L/h)")
)
curve_panels$role <- ifelse(curve_panels$scenario == "CL 5 L/h, V 50 L", "Reference",
                     ifelse(grepl("CL 2.5|V 25 L", curve_panels$scenario), "Halved", "Doubled"))
curve_panels$role <- factor(curve_panels$role, levels = c("Halved", "Reference", "Doubled"))
p_pk <- ggplot(curve_panels, aes(time_h, conc_ode, colour = role, linetype = role)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = curve_panels[curve_panels$time_h %% 4 == 0, ], aes(y = conc_analytic, shape = role),
             size = 1.8) +
  facet_wrap(~ panel) +
  scale_y_log10() +
  scale_colour_manual(values = unname(cdsr_colours[c("orange", "blue", "green")])) +
  scale_linetype_manual(values = cdsr_linetypes[c(2, 1, 3)]) +
  scale_shape_manual(values = cdsr_shapes[c(2, 1, 3)]) +
  labs(x = "Time after the virtual bolus (h)", y = "Concentration (mg/L, log scale)",
       colour = "Parameter", linetype = "Parameter", shape = "Parameter",
       caption = paste("Lines: numerical solution (deSolve); points: analytic solution every 4 h.",
                       "Virtual parameters, one virtual individual; no dosing recommendation.", sep = "\n")) +
  theme_cdsr()
show_figure(p_pk, "w6-pk.png", width = 7.5)

## ---- w6-pk-countercheck ----
# Counter-check: double clearance AND volume. What happens to half-life and to C(0)?
double_both <- c(CL = 10, V = 100)
c(half_life_h = log(2) * double_both[["V"]] / double_both[["CL"]], c0_mg_per_l = dose / double_both[["V"]])
stopifnot(isTRUE(all.equal(log(2) * 100 / 10, half_life)), dose / 100 == 1)

## ---- w6-mci-events ----
# Part B, station mass casualty incident (unit 22): 12 casualties who need surgery,
# two capacity scenarios on the SAME pre-generated arrival and operating times (common random numbers)
set.seed(20260322)
n_rep <- 200
n_casualties <- 12
mci_events <- do.call(rbind, lapply(seq_len(n_rep), function(r) {
  data.frame(replication = r, casualty = seq_len(n_casualties),
             arrival_min = round(cumsum(rexp(n_casualties, rate = 1 / 15)), 2),          # mean gap 15 min
             surgery_min = round(rlnorm(n_casualties, log(90) - 0.35^2 / 2, 0.35), 2))  # mean 90 min
}))
head(mci_events, 4)

## ---- w6-mci-queue ----
# Transparent first-come-first-served queue: each casualty takes the theatre that becomes free first
fifo_wait <- function(arrival, surgery, theatres) {
  free_at <- rep(0, theatres)
  start <- numeric(length(arrival))
  for (i in seq_along(arrival)) {
    k <- which.min(free_at)
    start[i] <- max(arrival[i], free_at[k])
    free_at[k] <- start[i] + surgery[i]
  }
  start - arrival                                    # waiting time in minutes
}
mci_events$wait_2 <- NA_real_
mci_events$wait_3 <- NA_real_
for (r in seq_len(n_rep)) {
  rows <- mci_events$replication == r
  mci_events$wait_2[rows] <- fifo_wait(mci_events$arrival_min[rows], mci_events$surgery_min[rows], 2)
  mci_events$wait_3[rows] <- fifo_wait(mci_events$arrival_min[rows], mci_events$surgery_min[rows], 3)
}

# Cross-check with simmer for replication 1: the same events must give the same waiting times
simmer_wait <- function(ev, theatres) {
  casualty <- trajectory("casualty") |>
    seize("theatre", 1) |>
    timeout_from_attribute("surgery_min") |>
    release("theatre", 1)
  env <- simmer("mci") |>
    add_resource("theatre", capacity = theatres) |>
    add_dataframe("casualty", casualty, data = data.frame(time = ev$arrival_min, surgery_min = ev$surgery_min),
                  time = "absolute") |>
    run()
  arr <- get_mon_arrivals(env)
  arr <- arr[order(as.integer(sub("casualty", "", arr$name))), ]
  arr$end_time - arr$start_time - arr$activity_time
}
rep1 <- mci_events[mci_events$replication == 1, ]
check_simmer <- c(two_theatres   = max(abs(simmer_wait(rep1, 2) - rep1$wait_2)),
                  three_theatres = max(abs(simmer_wait(rep1, 3) - rep1$wait_3)))
check_simmer
stopifnot(all(check_simmer < 1e-8),
          all(mci_events$wait_3 <= mci_events$wait_2 + 1e-9))   # more capacity never lengthens a wait here

## ---- w6-mci-summary ----
per_rep <- do.call(rbind, lapply(split(mci_events, mci_events$replication), function(d)
  data.frame(replication = d$replication[1],
             mean_wait_2_h = mean(d$wait_2) / 60, mean_wait_3_h = mean(d$wait_3) / 60,
             over_2h_2 = mean(d$wait_2 > 120), over_2h_3 = mean(d$wait_3 > 120))))
per_rep$difference_h <- per_rep$mean_wait_2_h - per_rep$mean_wait_3_h
spread <- function(x) sprintf("%.2f (%.2f to %.2f)", median(x), quantile(x, 0.1), quantile(x, 0.9))
mci_summary <- data.frame(
  measure = c("Mean waiting time per casualty (h)", "Share of casualties waiting > 2 h"),
  two_theatres   = c(spread(per_rep$mean_wait_2_h), spread(per_rep$over_2h_2)),
  three_theatres = c(spread(per_rep$mean_wait_3_h), spread(per_rep$over_2h_3)),
  paired_difference = c(spread(per_rep$difference_h), spread(per_rep$over_2h_2 - per_rep$over_2h_3))
)
print(mci_summary, row.names = FALSE)
stopifnot(nrow(per_rep) == n_rep, all(per_rep$difference_h >= 0))

## ---- w6-mci-countercheck ----
# Counter-check: operations take 20% longer (same arrivals). How much of the third theatre's gain remains?
longer_3 <- sapply(split(mci_events, mci_events$replication), function(d)
  mean(fifo_wait(d$arrival_min, 1.2 * d$surgery_min, 3)) / 60)
c(median_3_theatres_h = median(per_rep$mean_wait_3_h), median_3_theatres_longer_h = median(longer_3),
  median_2_theatres_h = median(per_rep$mean_wait_2_h))
stopifnot(median(longer_3) > median(per_rep$mean_wait_3_h), median(longer_3) < median(per_rep$mean_wait_2_h))

## ---- fig-w6-mci ----
mci_long <- data.frame(
  scenario = factor(rep(c("2 operating theatres", "3 operating theatres"), each = n_rep),
                    levels = c("2 operating theatres", "3 operating theatres")),
  mean_wait_h = c(per_rep$mean_wait_2_h, per_rep$mean_wait_3_h)
)
p_mci <- ggplot(mci_long, aes(scenario, mean_wait_h, colour = scenario, shape = scenario)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, size = 1.4) +
  geom_boxplot(fill = NA, colour = "grey20", outlier.shape = NA, width = 0.45) +
  scale_colour_manual(values = unname(cdsr_colours[c("red", "blue")]), guide = "none") +
  scale_shape_manual(values = cdsr_shapes[1:2], guide = "none") +
  labs(x = NULL, y = "Mean waiting time for surgery per casualty (h)",
       caption = paste("Each point is one simulated incident with 12 casualties; n = 200 replications per scenario,",
                       "both scenarios on identical event sequences. Synthetic.", sep = "\n")) +
  theme_cdsr()
show_figure(p_mci, "w6-mci.png")

## ---- w6-bayes ----
# Part C (unit 23): 3 of 12 fictitious animals show the event; two plausible priors
events  <- 3
animals <- 12
priors <- data.frame(prior = c("Flat prior Beta(1, 1)", "Sceptical prior Beta(2, 8)"), a = c(1, 2), b = c(1, 8))
priors$a_post <- priors$a + events
priors$b_post <- priors$b + animals - events
priors$post_mean <- priors$a_post / (priors$a_post + priors$b_post)
priors$lower_95  <- qbeta(0.025, priors$a_post, priors$b_post)
priors$upper_95  <- qbeta(0.975, priors$a_post, priors$b_post)
print(priors, digits = 3)

# Independent numerical check: posterior on a grid (prior x likelihood) gives the same quantiles
grid <- seq(0, 1, length.out = 200001)
grid_quantiles <- t(sapply(seq_len(nrow(priors)), function(i) {
  post <- dbeta(grid, priors$a[i], priors$b[i]) * dbinom(events, animals, grid)
  cdf <- cumsum(post) / sum(post)
  c(grid[which(cdf >= 0.025)[1]], grid[which(cdf >= 0.975)[1]])
}))
stopifnot(
  all(priors$a_post == c(4, 5)), all(priors$b_post == c(10, 17)),
  max(abs(grid_quantiles - cbind(priors$lower_95, priors$upper_95))) < 1e-3
)

## ---- w6-bayes-countercheck ----
# Counter-check: the same rate, ten times more animals (30 of 120). Narrower intervals, closer priors?
big <- data.frame(prior = priors$prior, a_post = priors$a + 30, b_post = priors$b + 90)
big$lower_95 <- qbeta(0.025, big$a_post, big$b_post)
big$upper_95 <- qbeta(0.975, big$a_post, big$b_post)
print(big, digits = 3)
stopifnot(all(big$upper_95 - big$lower_95 < priors$upper_95 - priors$lower_95))

## ---- fig-w6-posterior ----
dens <- do.call(rbind, lapply(seq_len(nrow(priors)), function(i) {
  p <- seq(0, 1, by = 0.002)
  rbind(data.frame(prior = priors$prior[i], curve = "Prior", p = p, density = dbeta(p, priors$a[i], priors$b[i])),
        data.frame(prior = priors$prior[i], curve = "Posterior", p = p,
                   density = dbeta(p, priors$a_post[i], priors$b_post[i])))
}))
dens$curve <- factor(dens$curve, levels = c("Prior", "Posterior"))
p_post <- ggplot(dens, aes(p, density, colour = curve, linetype = curve)) +
  geom_vline(xintercept = events / animals, colour = cdsr_colours[["orange"]], linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ prior) +
  scale_colour_manual(values = unname(cdsr_colours[c("grey", "blue")])) +
  scale_linetype_manual(values = cdsr_linetypes[c(2, 1)]) +
  scale_x_continuous(labels = scales::percent, name = "Event probability per animal") +
  labs(y = "Density", colour = NULL, linetype = NULL,
       caption = "Orange line: observed 3 of 12 fictitious animals (25%); n = 12 animals, no control group. Synthetic.") +
  theme_cdsr()
show_figure(p_post, "w6-posterior.png", width = 7.5)

## ---- w6-save ----
save_csv(ed[, c("index", "week", "weekday", "hour", "shift", "arrivals")], "w6-ed-arrivals.csv")
save_csv(pk_curves, "w6-pk-concentrations.csv")
save_csv(mci_events[, c("replication", "casualty", "arrival_min", "surgery_min")], "w6-mci-events.csv")
