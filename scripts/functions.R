## Shared functions for the figure source-data repository
## ---------------------------------------------------------
## Only packages that are available from CRAN are used.  Each figure Rmd
## sources this file, reads the tidy CSV files in source_data/, performs the
## displayed statistical test, and writes the panel PDF to output/.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

## A compact, journal-like theme ------------------------------------------------
theme_pub <- function(base_size = 8, base_family = "sans") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.text       = element_text(colour = "black", size = base_size),
      axis.title      = element_text(colour = "black", size = base_size + 1),
      axis.line       = element_line(linewidth = 0.45, colour = "black"),
      axis.ticks      = element_line(linewidth = 0.45, colour = "black"),
      strip.background = element_blank(),
      strip.text      = element_text(face = "bold", size = base_size),
      legend.title    = element_blank(),
      legend.text     = element_text(size = base_size - 1),
      legend.key.size = unit(0.32, "cm"),
      plot.title      = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.margin     = margin(3, 3, 3, 3)
    )
}

## A stable colour palette for the groups in one panel -------------------------
group_palette <- function(x) {
  g <- unique(as.character(x))
  g <- g[!is.na(g)]
  setNames(hcl.colors(length(g), "Dark 3"), g)
}

## Save one panel as a vector PDF and a 300-dpi PNG ---------------------------
save_panel <- function(p, filename, width = 2.4, height = 2.2) {
  dir.create("output", showWarnings = FALSE)
  ggsave(file.path("output", paste0(filename, ".pdf")), p,
         width = width, height = height, units = "in", device = "pdf")
  ggsave(file.path("output", paste0(filename, ".png")), p,
         width = width, height = height, units = "in", dpi = 300)
  invisible(p)
}

## Box plot with all replicate points ----------------------------------------
box_plot <- function(d, ylab, x = "group", colour = "group",
                     order = NULL, colour_order = NULL,
                     facet = NULL, angle = 45, point_size = 1.0) {
  keep <- unique(c(x, colour, facet, "value"))
  d <- d[, keep, drop = FALSE]
  for (v in unique(c(x, colour))) d[[v]] <- as.character(d[[v]])
  if (!is.null(order))       d[[x]]      <- factor(d[[x]], levels = order)
  if (!is.null(colour_order)) d[[colour]] <- factor(d[[colour]], levels = colour_order)
  pal <- group_palette(d[[colour]])

  p <- ggplot(d, aes(x = .data[[x]], y = .data$value, colour = .data[[colour]])) +
    geom_boxplot(width = 0.58, outlier.shape = NA, fill = NA,
                 linewidth = 0.4, position = position_dodge(width = 0.75)) +
    geom_jitter(size = point_size, alpha = 0.75,
                position = position_jitterdodge(jitter.width = 0.12,
                                                dodge.width = 0.75)) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 1.7,
                 position = position_dodge(width = 0.75), show.legend = FALSE) +
    scale_colour_manual(values = pal) +
    labs(x = NULL, y = ylab) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = angle, hjust = 1, vjust = 1),
          legend.position = "none")

  if (!is.null(facet)) {
    p <- p + facet_wrap(stats::as.formula(paste("~", facet)), scales = "free_y")
  }
  p
}

## Mean +/- SEM line plot for time courses and dose-response data -------------
line_plot <- function(d, ylab, x = "time", colour = "group",
                      facet = NULL, order = NULL, colour_order = NULL,
                      xlab = NULL, log_x = FALSE) {
  vars <- c(x, colour, facet)
  s <- d %>%
    group_by(across(any_of(vars))) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
              .groups = "drop")
  for (v in unique(c(x, colour))) {
    d[[v]] <- as.character(d[[v]])
    s[[v]] <- as.character(s[[v]])
  }
  if (!is.null(order))        s[[x]]      <- factor(s[[x]], levels = order)
  if (!is.null(colour_order)) s[[colour]] <- factor(s[[colour]], levels = colour_order)
  pal <- group_palette(s[[colour]])

  p <- ggplot(s, aes(x = .data[[x]], y = .data$mean, colour = .data[[colour]],
                     group = .data[[colour]])) +
    geom_line(linewidth = 0.55) +
    geom_point(size = 1.4) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.14, linewidth = 0.35) +
    scale_colour_manual(values = pal) +
    labs(x = ifelse(is.null(xlab), x, xlab), y = ylab) +
    theme_pub() +
    theme(legend.position = "right")

  if (log_x) p <- p + scale_x_log10()
  if (!is.null(facet)) {
    p <- p + facet_wrap(stats::as.formula(paste("~", facet)), scales = "free_y")
  }
  p
}

## Mean +/- SEM bar plot with individual points -------------------------------
bar_plot <- function(d, ylab, x = "group", colour = "group",
                     facet = NULL, order = NULL, colour_order = NULL,
                     angle = 45, dodge = 0.75) {
  vars <- c(x, colour, facet)
  s <- d %>%
    group_by(across(any_of(vars))) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
              .groups = "drop")
  for (v in unique(c(x, colour))) {
    d[[v]] <- as.character(d[[v]])
    s[[v]] <- as.character(s[[v]])
  }
  if (!is.null(order))        { d[[x]] <- factor(d[[x]], levels = order); s[[x]] <- factor(s[[x]], levels = order) }
  if (!is.null(colour_order)) { d[[colour]] <- factor(d[[colour]], levels = colour_order); s[[colour]] <- factor(s[[colour]], levels = colour_order) }
  pal <- group_palette(s[[colour]])

  p <- ggplot(s, aes(x = .data[[x]], y = .data$mean, fill = .data[[colour]])) +
    geom_col(position = position_dodge(width = dodge), width = 0.68,
             colour = "black", linewidth = 0.3) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  position = position_dodge(width = dodge), width = 0.18,
                  linewidth = 0.35) +
    geom_point(data = d, aes(x = .data[[x]], y = .data$value, fill = .data[[colour]]),
               position = position_jitterdodge(jitter.width = 0.12,
                                               dodge.width = dodge),
               shape = 21, colour = "black", stroke = 0.25, size = 1.1,
               show.legend = FALSE) +
    scale_fill_manual(values = pal) +
    labs(x = NULL, y = ylab) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = angle, hjust = 1, vjust = 1))

  if (!is.null(facet)) {
    p <- p + facet_wrap(stats::as.formula(paste("~", facet)), scales = "free_y")
  }
  p
}

## One-way ANOVA + Tukey HSD --------------------------------------------------
stats_oneway <- function(d, panel, group = "group", value = "value") {
  dd <- d[!is.na(d[[value]]), , drop = FALSE]
  dd$.g <- factor(dd[[group]])
  if (nlevels(dd$.g) < 2 || nrow(dd) < 4) return(NULL)

  fit <- tryCatch(stats::aov(stats::as.formula(paste(value, "~ .g")), data = dd),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  a <- summary(fit)[[1]]
  df2 <- a[["Df"]][length(a[["Df"]])]
  a <- a[trimws(rownames(a)) != "Residuals", , drop = FALSE]
  anova <- data.frame(
    panel = panel,
    test  = "one-way ANOVA",
    term  = "group",
    df1   = a[["Df"]],
    df2   = df2,
    F     = a[["F value"]],
    p     = a[["Pr(>F)"]],
    row.names = NULL
  )

  tk <- tryCatch(stats::TukeyHSD(fit)$.g, error = function(e) NULL)
  tukey <- NULL
  if (!is.null(tk)) {
    tukey <- data.frame(
      panel      = panel,
      test       = "Tukey HSD",
      comparison = rownames(tk),
      difference = tk[, "diff"],
      lower      = tk[, "lwr"],
      upper      = tk[, "upr"],
      p          = tk[, "p adj"],
      row.names  = NULL
    )
  }
  list(anova = anova, tukey = tukey)
}

## Convenience wrapper: compute and write a stats table ----------------------
write_stats <- function(stats_list, filename) {
  dir.create("output", showWarnings = FALSE)
  out <- bind_rows(stats_list)
  if (nrow(out) == 0) return(invisible(out))
  write_csv(out, file.path("output", filename))
  out
}

## Correlation annotation -----------------------------------------------------
cor_annotation <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return("n < 3")
  ct <- stats::cor.test(x[ok], y[ok], method = "pearson")
  p <- ct$p.value
  p_txt <- if (p < 1e-4) "p < 0.0001" else sprintf("p = %.4f", p)
  sprintf("r = %.3f\n%s\nn = %d", unname(ct$estimate), p_txt, sum(ok))
}
