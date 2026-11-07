# Reproducible source-data previews for the six main figures
# All CSV data are read without editing, imputing, or converting missing cells to zero.
# Required R package: ggplot2. Rendering requires knitr and rmarkdown.

read_source_csv <- function(filename) {
  stopifnot(file.exists(filename))
  read.csv(filename, header = TRUE, check.names = FALSE,
           stringsAsFactors = FALSE, na.strings = c("", "NA"),
           fileEncoding = "UTF-8-BOM")
}

numeric_value <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

source_plot_type <- function(data, filename) {
  f <- basename(filename)
  if (grepl("_MST_", f, ignore.case = TRUE)) return("binding_curve")
  if (grepl("^Fig4A_IQGAP2_(IL1B|IL6|OCLN|TJP1)[.]csv$", f,
            ignore.case = TRUE)) return("scatter")
  if (tolower(names(data)[1]) == "days") return("timecourse")
  if (grepl("^Fig3C_|^Fig4C_|^Fig3D_", f, ignore.case = TRUE)) return("curve")
  if (is.na(names(data)[1]) || names(data)[1] == "") return("stratified")
  if (nrow(data) >= 40L && ncol(data) >= 3L) return("distribution")
  return("groups")
}

# Each CSV column is handled by its POSITION, not just its name: repeated names
# such as 'Ctrl', 'DSS' and 'LA' represent distinct replicate observations.
# Column- and row-level identity is preserved, including for longitudinal data.
wide_to_long <- function(data, first_col_is_label = FALSE) {
  cols <- if (first_col_is_label) seq.int(2L, ncol(data)) else seq_len(ncol(data))
  if (!length(cols)) stop("CSV has no measurement columns")
  raw_groups <- names(data)[cols]
  display_groups <- ifelse(is.na(raw_groups) | trimws(raw_groups) == "",
                           paste0("Column_", cols), raw_groups)
  values <- lapply(cols, function(j) {
    data.frame(row_id = seq_len(nrow(data)),
               source_column = j,
               group = display_groups[match(j, cols)],
               value = numeric_value(data[[j]]),
               stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, values)
  if (first_col_is_label) {
    result$stratum <- as.character(data[[1]][result$row_id])
  }
  result <- result[!is.na(result$value), , drop = FALSE]
  result$group <- factor(result$group, levels = unique(display_groups))
  rownames(result) <- NULL
  result
}

# SEM is a descriptive display only. No hypothesis-test p-values are calculated
# without a study-approved testing plan.
mean_sd <- function(data, by) {
  pieces <- split(data$value, interaction(data[by], drop = TRUE, lex.order = TRUE))
  keys <- lapply(split(data, interaction(data[by], drop = TRUE, lex.order = TRUE)),
                 function(d) d[1, by, drop = FALSE])
  out <- do.call(rbind, lapply(seq_along(pieces), function(i) {
    v <- pieces[[i]]
    cbind(keys[[i]], data.frame(n = length(v), mean = mean(v),
      sd = if (length(v) > 1) stats::sd(v) else NA_real_,
      sem = if (length(v) > 1) stats::sd(v)/sqrt(length(v)) else NA_real_))
  }))
  rownames(out) <- NULL
  out
}

source_summary <- function(data) {
  data.frame(
    column = ifelse(names(data) == "", "<unnamed first column>", names(data)),
    non_missing = vapply(data, function(x) sum(!is.na(x)), integer(1)),
    missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    check.names = FALSE)
}

plot_source_data <- function(data, filename) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install 'ggplot2' with install.packages('ggplot2')")
  kind <- source_plot_type(data, filename)
  title <- tools::file_path_sans_ext(basename(filename))
  base <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))

  if (kind == "scatter") {
    x <- numeric_value(data[[1]])
    y <- numeric_value(data[[2]])
    d <- data.frame(x = x, y = y)
    d <- d[stats::complete.cases(d), , drop = FALSE]
    return(ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = .48, size = 1.5, colour = "#3974A5") +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                           colour = "#D36D45", linewidth = .7) +
      ggplot2::labs(title = title, x = names(data)[1], y = names(data)[2],
        subtitle = "Exploratory linear fit; no correlation test or p-value inferred") + base)
  }

  if (kind == "binding_curve") {
    d <- data.frame(concentration = numeric_value(data[[1]]),
                    response = numeric_value(data[[2]]))
    d <- d[stats::complete.cases(d) & d$concentration > 0, , drop = FALSE]
    d <- d[order(d$concentration), , drop = FALSE]
    return(ggplot2::ggplot(d, ggplot2::aes(x = concentration, y = response)) +
      ggplot2::geom_line(colour = "#4976AE", linewidth = .7) +
      ggplot2::geom_point(colour = "#4976AE", size = 2) +
      ggplot2::scale_x_log10() +
      ggplot2::labs(title = title, x = names(data)[1], y = names(data)[2],
        subtitle = "Raw MST values connected; binding parameters are not fitted") + base)
  }

  if (kind == "timecourse" || kind == "curve") {
    x <- numeric_value(data[[1]])
    long <- wide_to_long(data, first_col_is_label = TRUE)
    long$x <- x[long$row_id]
    long <- long[!is.na(long$x), , drop = FALSE]
    if (!nrow(long)) stop("No valid numeric x-axis values in ", filename)
    s <- mean_sd(long, c("group", "x"))
    yl <- if (grepl("weight", filename, ignore.case = TRUE)) "Body weight (source units)" else
          if (grepl("dai", filename, ignore.case = TRUE)) "Disease activity index" else
          "Measured response (source units)"
    xl <- if (kind == "timecourse") "Day" else
          if (grepl("Fig3C|Fig4C", filename)) "Temperature (°C)" else
          "Treatment concentration (source units)"
    # For replicate columns at each x, the curves show mean ± SEM.
    return(ggplot2::ggplot(s, ggplot2::aes(x = x, y = mean, colour = group, group = group)) +
      ggplot2::geom_line(linewidth = .75) +
      ggplot2::geom_point(size = 1.7) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sem, ymax = mean + sem),
                             width = 0, linewidth = .35, na.rm = TRUE) +
      ggplot2::labs(title = title, x = xl, y = yl, colour = "Group",
        subtitle = "Observed mean ± SEM, missing observations excluded") + base)
  }

  long <- wide_to_long(data, first_col_is_label = (kind == "stratified"))
  if (!nrow(long)) stop("No numeric observations in ", filename)

  if (kind == "distribution") {
    return(ggplot2::ggplot(long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(width = .6, outlier.shape = NA, alpha = .7) +
      ggplot2::geom_jitter(width = .16, height = 0, alpha = .22, size = .8) +
      ggplot2::guides(fill = "none") +
      ggplot2::labs(title = title, x = NULL, y = "Measured value",
                    subtitle = "All non-missing observations; no significance test") + base)
  }

  s <- mean_sd(long, if (kind == "stratified") c("stratum", "group") else "group")
  p <- ggplot2::ggplot(s, ggplot2::aes(x = group, y = mean, fill = group)) +
    ggplot2::geom_col(width = .68, colour = "#FFFFFF") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
                           width = .2, na.rm = TRUE) +
    ggplot2::geom_point(data = long, ggplot2::aes(x = group, y = value),
                        inherit.aes = FALSE, position = ggplot2::position_jitter(width = .09),
                        size = 1.25, alpha = .75, colour = "#333333") +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(title = title, x = NULL, y = "Measured value",
      subtitle = "Observed mean ± SD and individual values; no significance test") + base
  if (kind == "stratified") {
    # Empty values are retained as NA in the CSV and excluded from calculations.
    p <- p + ggplot2::facet_wrap(~ stratum, scales = "free_y")
  }
  p
}
