# Helpers for the course labs "Clinical Data Science in R · Course".
# They complement style.R (shared with the companion book) and let every lab script run
# both inside a rendered lab page and on its own in a fresh session (Rscript r/<script>.R).

# TRUE while knitr renders a lab page
course_in_report <- function() isTRUE(getOption("knitr.in.progress"))

# Folder for files written by stand-alone runs: ./output in the working directory,
# or the folder named in the environment variable CDSR_OUTPUT
course_output_dir <- function() {
  dir <- Sys.getenv("CDSR_OUTPUT", "output")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

# Show a ggplot on the lab page or in an interactive session; save it as PNG otherwise
show_figure <- function(plot, file, width = 7, height = 4.5) {
  if (course_in_report() || interactive()) {
    print(plot)
  } else {
    path <- file.path(course_output_dir(), file)
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = 150, bg = "white")
    message("Figure saved: ", path)
  }
  invisible(plot)
}

# Write a synthetic teaching table: into data/ when the lab page is rendered,
# into the output folder when the script runs on its own
save_csv <- function(table, file) {
  dir <- if (course_in_report()) "../data" else course_output_dir()
  path <- file.path(dir, file)
  utils::write.csv(table, path, row.names = FALSE, fileEncoding = "UTF-8")
  if (!course_in_report()) message("Table saved: ", path)
  invisible(path)
}

# Exact (Clopper-Pearson) 95% interval for a proportion x/n, returned as c(estimate, lower, upper)
prop_ci <- function(x, n) {
  if (n == 0) return(c(estimate = NA_real_, lower = NA_real_, upper = NA_real_))
  ci <- stats::binom.test(x, n)$conf.int
  c(estimate = x / n, lower = ci[1], upper = ci[2])
}

# Text for a proportion with its denominator and interval, e.g. "45/140 = 32.1% (24.5% to 40.5%)"
format_prop <- function(x, n, digits = 1) {
  ci <- prop_ci(x, n)
  sprintf("%s/%s = %s (%s to %s)", cdsr_number(x), cdsr_number(n), cdsr_percent(ci[[1]], digits),
          cdsr_percent(ci[[2]], digits), cdsr_percent(ci[[3]], digits))
}

# Area under the ROC curve via the rank (Mann-Whitney) formula; ties count one half
auc_rank <- function(score, outcome) {
  r <- rank(score)
  n1 <- sum(outcome == 1)
  n0 <- sum(outcome == 0)
  (sum(r[outcome == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
