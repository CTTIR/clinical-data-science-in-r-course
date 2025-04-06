# Shared figure and output style for the book and course "Clinical Data Science in R".
# Load at the start of a chapter with source("../R/style.R") (chapters live in chapters/).
# Colours: Okabe-Ito palette (safe for colour-vision deficiency); line types and point shapes
# are added so that every figure also works in black-and-white print.

cdsr_colours <- c(
  blue = "#0072B2", orange = "#E69F00", green = "#009E73", red = "#D55E00",
  purple = "#CC79A7", skyblue = "#56B4E9", yellow = "#F0E442", grey = "#6E6E6E"
)
cdsr_linetypes <- c("solid", "22", "42", "1343")
cdsr_shapes <- c(16, 17, 15, 18)

# ggplot2 theme: plain, legend at the bottom, source/denominator as left-aligned caption
theme_cdsr <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(hjust = 0, colour = "#4D4D4D", size = ggplot2::rel(0.8)),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold")
    )
}

# Base graphics with the same defaults
cdsr_par <- function() {
  graphics::par(mar = c(4.2, 4.4, 1.6, 1), las = 1, bty = "l", cex.axis = 0.9, family = "sans")
}

# Percentages, e.g. cdsr_percent(45 / 140) -> "32.1%"
cdsr_percent <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

# Numbers with thousands separator, e.g. cdsr_number(1234.5, 1) -> "1,234.5"
cdsr_number <- function(x, digits = 0) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}
