# Base-R implementation of the weighting and histogram calculations used by the
# reference analysis. Keeping this code local avoids runtime dependencies on
# metafor, dplyr, ggplot2, and ggpubr in the Shinylive build.

clean_text <- function(x) {
  x[is.na(x)] <- ""
  tolower(enc2utf8(x))
}

filter_effects <- function(data, keyword, fields, conversion_bases,
                           preregistration, year_range, journals) {
  keep <- rep(TRUE, nrow(data))

  keyword <- trimws(keyword)
  terms <- trimws(strsplit(keyword, ",", fixed = TRUE)[[1]])
  terms <- unique(terms[nzchar(terms)])
  if (length(terms)) {
    searchable <- paste(
      clean_text(data$keywords),
      clean_text(data$claim),
      clean_text(data$title),
      clean_text(data$description),
      sep = "\n"
    )
    term_matches <- lapply(terms, function(term) {
      grepl(tolower(term), searchable, fixed = TRUE)
    })
    keep <- keep & Reduce(`|`, term_matches)
  }

  if (length(fields)) {
    keep <- keep & data$field %in% fields
  } else {
    keep <- rep(FALSE, nrow(data))
  }

  if (length(conversion_bases)) {
    keep <- keep & data$r_conversion_basis %in% conversion_bases
  } else {
    keep <- rep(FALSE, nrow(data))
  }

  if (length(journals)) {
    keep <- keep & data$journal %in% journals
  } else {
    keep <- rep(FALSE, nrow(data))
  }

  if (identical(preregistration, "preregistered")) {
    keep <- keep & !is.na(data$pre_registration) & data$pre_registration
  } else if (identical(preregistration, "not_preregistered")) {
    keep <- keep & !is.na(data$pre_registration) & !data$pre_registration
  }

  keep <- keep & !is.na(data$year) &
    data$year >= year_range[1] & data$year <= year_range[2]

  data[keep, , drop = FALSE]
}

# Evaluate the restricted log likelihood for
# V + tau_doi * Z_doi Z_doi' + tau_study * Z_study Z_study'. The nested block
# algebra below is equivalent to fitting random intercepts for DOI/study, but
# avoids constructing a potentially very large covariance matrix.
multilevel_reml_objective <- function(tau, groups, n) {
  tau_doi <- tau[1]
  tau_study <- tau[2]

  doi_q <- numeric(length(groups))
  doi_r <- numeric(length(groups))
  doi_u <- numeric(length(groups))
  doi_logdet <- numeric(length(groups))

  for (i in seq_along(groups)) {
    g <- groups[[i]]
    denominator <- 1 + tau_study * g$w
    q <- sum(g$w / denominator)
    r <- sum(g$wy / denominator)
    u <- sum(g$wy2 - tau_study * g$wy^2 / denominator)

    doi_denominator <- 1 + tau_doi * q
    doi_q[i] <- q / doi_denominator
    doi_r[i] <- r / doi_denominator
    doi_u[i] <- u - tau_doi * r^2 / doi_denominator
    doi_logdet[i] <- sum(g$log_v + log(denominator)) + log(doi_denominator)
  }

  xt_vinv_x <- sum(doi_q)
  if (!is.finite(xt_vinv_x) || xt_vinv_x <= 0) {
    return(.Machine$double.xmax / 100)
  }

  residual_quadratic <- sum(doi_u) - sum(doi_r)^2 / xt_vinv_x
  if (!is.finite(residual_quadratic) || residual_quadratic < 0) {
    return(.Machine$double.xmax / 100)
  }

  0.5 * (
    sum(doi_logdet) + log(xt_vinv_x) + residual_quadratic +
      (n - 1) * log(2 * pi)
  )
}

estimate_multilevel_tau <- function(y, sampling_variance, doi, study) {
  n <- length(y)
  if (n < 3L || length(unique(doi)) < 2L) {
    return(c(doi = 0, study = 0))
  }

  inv_v <- 1 / sampling_variance
  nested_study <- interaction(doi, study, drop = TRUE, lex.order = TRUE)
  study_rows <- split(seq_len(n), nested_study)

  study_summaries <- lapply(study_rows, function(index) {
    w <- inv_v[index]
    list(
      doi = doi[index[1]],
      w = sum(w),
      wy = sum(w * y[index]),
      wy2 = sum(w * y[index]^2),
      log_v = sum(log(sampling_variance[index]))
    )
  })

  groups <- lapply(split(study_summaries, vapply(study_summaries, `[[`, "", "doi")),
    function(studies) {
      list(
        w = vapply(studies, `[[`, 0, "w"),
        wy = vapply(studies, `[[`, 0, "wy"),
        wy2 = vapply(studies, `[[`, 0, "wy2"),
        log_v = vapply(studies, `[[`, 0, "log_v")
      )
    }
  )

  marginal_variance <- max(stats::var(y) - mean(sampling_variance), 0)
  starts <- list(
    c(marginal_variance / 2, marginal_variance / 2),
    c(marginal_variance, 0),
    c(0, marginal_variance),
    c(0, 0)
  )

  fits <- lapply(starts, function(start) {
    tryCatch(
      stats::optim(
        par = start,
        fn = multilevel_reml_objective,
        method = "L-BFGS-B",
        lower = c(0, 0),
        groups = groups,
        n = n,
        control = list(maxit = 120, factr = 1e8)
      ),
      error = function(e) NULL
    )
  })
  fits <- Filter(function(x) !is.null(x) && is.finite(x$value), fits)

  if (!length(fits)) {
    return(c(doi = 0, study = 0))
  }

  best <- fits[[which.min(vapply(fits, `[[`, 0, "value"))]]$par
  stats::setNames(pmax(best, 0), c("doi", "study"))
}

prepare_effect_distribution <- function(data, design, min_effects = 10L) {
  is_within <- identical(design, "within")
  subset <- data[!is.na(data$within_subjects) &
    data$within_subjects == is_within, , drop = FALSE]

  valid <- !is.na(subset$doi) & nzchar(subset$doi) &
    !is.na(subset$study) & nzchar(subset$study) &
    is.finite(subset$es_combined) &
    is.finite(subset$sample_size)

  if (is_within) {
    valid <- valid & subset$sample_size > 1
  } else {
    valid <- valid & subset$sample_size > 3 & abs(subset$es_combined) < 1
  }
  subset <- subset[valid, , drop = FALSE]

  result <- list(
    design = design,
    matching_effects = nrow(data[!is.na(data$within_subjects) &
      data$within_subjects == is_within, , drop = FALSE]),
    usable_effects = nrow(subset),
    minimum = min_effects,
    ready = nrow(subset) >= min_effects
  )

  if (!result$ready) {
    return(result)
  }

  if (is_within) {
    correction <- 1 - 3 / (4 * (subset$sample_size - 1) - 1)
    effect <- subset$es_combined * correction
    y <- effect
    sampling_variance <- (1 / subset$sample_size) +
      effect^2 * (1 - (subset$sample_size - 3) /
        ((subset$sample_size - 1) * correction^2))
    x_limits <- c(0, 2)
  } else {
    effect <- subset$es_combined
    y <- atanh(effect)
    sampling_variance <- 1 / (subset$sample_size - 3)
    x_limits <- c(0, 1)
  }

  finite <- is.finite(effect) & is.finite(y) &
    is.finite(sampling_variance) & sampling_variance > 0
  subset <- subset[finite, , drop = FALSE]
  effect <- effect[finite]
  y <- y[finite]
  sampling_variance <- sampling_variance[finite]

  result$usable_effects <- length(effect)
  result$ready <- length(effect) >= min_effects
  if (!result$ready) {
    return(result)
  }

  tau <- estimate_multilevel_tau(
    y,
    sampling_variance,
    subset$doi,
    subset$study
  )
  inverse_variance_weight <- 1 / (sampling_variance + sum(tau))
  doi_count <- ave(seq_along(subset$doi), subset$doi, FUN = length)
  density_weight <- inverse_variance_weight / doi_count

  quartiles <- bootstrap_weighted_quartiles(
    effect,
    inverse_variance_weight,
    subset$doi,
    subset$study,
    bootstraps = 5000L,
    seed = 912L
  )

  result$source_effect <- effect
  result$source_weight <- density_weight
  result$source_doi <- subset$doi
  result$source_quartiles <- quartiles

  in_range <- effect >= x_limits[1] & effect <= x_limits[2]
  result$effects_in_plot <- sum(in_range)
  result$usable_effects <- result$effects_in_plot
  if (result$effects_in_plot < min_effects) {
    result$ready <- FALSE
    result$reason <- sprintf(
      "%s effects fall inside the plotted range; at least %s are required.",
      result$effects_in_plot,
      min_effects
    )
    return(result)
  }

  result$effect <- effect[in_range]
  result$weight <- density_weight[in_range]
  result$doi <- subset$doi[in_range]
  result$tau <- tau
  result$quartiles <- quartiles
  result$x_limits <- x_limits
  result$studies <- length(unique(result$doi))
  result
}

between_metric_spec <- function(metric = "r") {
  specifications <- list(
    r = list(
      label = "r",
      subtitle = "Effect size measured as r",
      x_label = "Effect size (r)",
      x_limits = c(0, 1),
      drop_leading_zero = TRUE
    ),
    d = list(
      label = "d",
      subtitle = "Converted effect size measured as d",
      x_label = "Effect size (d)",
      x_limits = c(0, 4),
      x_ticks = 0:4,
      x_tick_labels = c("0", "1", "2", "3", "≥4"),
      drop_leading_zero = FALSE
    ),
    r_squared = list(
      label = "r² / partial η²",
      subtitle = "Variance explained (r² / partial η²)",
      x_label = "Effect size (r^2 / partial eta^2)",
      x_limits = c(0, 1),
      drop_leading_zero = TRUE
    ),
    f = list(
      label = "f",
      subtitle = "Converted effect size measured as Cohen's f",
      x_label = "Effect size (Cohen's f)",
      x_limits = c(0, 2),
      x_ticks = seq(0, 2, by = 0.5),
      x_tick_labels = c("0", "0.5", "1", "1.5", "≥2"),
      drop_leading_zero = FALSE
    ),
    odds_ratio = list(
      label = "Odds ratio",
      subtitle = "Converted effect size measured as an odds ratio",
      x_label = "Odds ratio (log-scaled axis)",
      x_limits = log10(c(1, 2000)),
      x_ticks = log10(c(1, 5, 20, 100, 500, 2000)),
      x_tick_labels = c("1", "5", "20", "100", "500", "≥2,000"),
      drop_leading_zero = FALSE
    )
  )

  if (!metric %in% names(specifications)) {
    stop("Unknown between-subjects display metric: ", metric, call. = FALSE)
  }
  c(list(key = metric), specifications[[metric]])
}

convert_r_metric <- function(r, metric = "r") {
  if (any(is.finite(r) & (r < 0 | r >= 1))) {
    stop("Between-subjects r values must be in the interval [0, 1).", call. = FALSE)
  }

  if (identical(metric, "r")) {
    return(r)
  }
  if (identical(metric, "r_squared")) {
    return(r^2)
  }

  d <- 2 * r / sqrt(1 - r^2)
  if (identical(metric, "d")) {
    return(d)
  }
  if (identical(metric, "f")) {
    return(sqrt(r^2 / (1 - r^2)))
  }
  if (identical(metric, "odds_ratio")) {
    return(exp(d * pi / sqrt(3)))
  }

  stop("Unknown between-subjects display metric: ", metric, call. = FALSE)
}

transform_between_distribution <- function(distribution, metric = "r") {
  specification <- between_metric_spec(metric)
  if (is.null(distribution) || !isTRUE(distribution$ready)) {
    return(distribution)
  }
  if (!identical(distribution$design, "between")) {
    stop("Only between-subjects distributions can use this conversion.", call. = FALSE)
  }

  display_effect <- convert_r_metric(distribution$source_effect, metric)
  display_quartiles <- convert_r_metric(distribution$source_quartiles, metric)

  if (identical(metric, "odds_ratio")) {
    plot_effect <- log10(display_effect)
    plot_quartiles <- log10(display_quartiles)
  } else {
    plot_effect <- display_effect
    plot_quartiles <- display_quartiles
  }

  plottable <- !is.na(plot_effect) &
    plot_effect >= specification$x_limits[1]
  clipped_plot_effect <- pmin(
    plot_effect[plottable],
    specification$x_limits[2]
  )

  distribution$effect <- clipped_plot_effect
  distribution$weight <- distribution$source_weight[plottable]
  distribution$doi <- distribution$source_doi[plottable]
  distribution$quartiles <- display_quartiles
  distribution$plot_quartiles <- plot_quartiles
  distribution$x_limits <- specification$x_limits
  distribution$x_ticks <- specification$x_ticks
  distribution$x_tick_labels <- specification$x_tick_labels
  distribution$drop_leading_zero <- specification$drop_leading_zero
  distribution$x_label <- specification$x_label
  distribution$metric <- specification$key
  distribution$metric_label <- specification$label
  distribution$effects_in_plot <- sum(plottable)
  distribution$usable_effects <- sum(plottable)
  distribution$studies <- length(unique(distribution$doi))
  distribution$ready <- sum(plottable) >= distribution$minimum

  if (!distribution$ready) {
    distribution$reason <- sprintf(
      "%s effects fall inside the plotted range; at least %s are required.",
      sum(plottable),
      distribution$minimum
    )
  }
  distribution
}

weighted_quantile_type7 <- function(x, weight,
                                    probabilities = c(0.25, 0.5, 0.75)) {
  keep <- is.finite(x) & is.finite(weight) & weight != 0
  x <- x[keep]
  weight <- weight[keep]

  if (length(x) <= 1L) {
    return(stats::quantile(
      x,
      probabilities,
      names = FALSE,
      type = 7,
      na.rm = TRUE
    ))
  }

  weight <- weight / sum(weight)
  order_x <- order(x)
  x <- x[order_x]
  weight <- weight[order_x]
  cumulative <- cumsum(weight)

  # This is ggdist::weighted_quantile(..., type = 7), vendored so ggdist is
  # not needed in the Shinylive runtime.
  probability_at_x <- (cumulative - weight) / (1 - weight)
  stats::approx(
    probability_at_x,
    x,
    xout = probabilities,
    rule = 2,
    ties = "ordered"
  )$y
}

bootstrap_weighted_quartiles <- function(x, weight, doi, study,
                                         bootstraps = 5000L, seed = 912L) {
  doi_levels <- unique(doi)
  nested_study <- interaction(doi, study, drop = TRUE, lex.order = TRUE)
  study_rows <- split(seq_along(x), nested_study)
  study_doi <- doi[vapply(study_rows, `[[`, 0L, 1L)]
  groups_by_doi <- split(seq_along(study_rows), match(study_doi, doi_levels))
  group_size <- lengths(study_rows)
  group_offset <- cumsum(c(0L, head(group_size, -1L)))
  flattened_rows <- unlist(study_rows, use.names = FALSE)
  n_doi <- length(doi_levels)

  if (!is.null(seed)) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_seed_exists) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit({
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  estimates <- matrix(NA_real_, nrow = bootstraps, ncol = 3L)
  for (bootstrap in seq_len(bootstraps)) {
    sampled_doi <- sample.int(n_doi, n_doi, replace = TRUE)
    selected_groups <- unlist(groups_by_doi[sampled_doi], use.names = FALSE)
    position <- group_offset[selected_groups] +
      floor(stats::runif(length(selected_groups)) * group_size[selected_groups]) + 1L
    selected <- flattened_rows[position]

    estimates[bootstrap, ] <- weighted_quantile_type7(
      x[selected],
      weight[selected]
    )
  }

  colMeans(estimates, na.rm = TRUE)
}

weighted_histogram_bins <- function(x, weight, limits, bins = 40L) {
  breaks <- seq(limits[1], limits[2], length.out = bins + 1L)
  bin <- findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
  weighted_count <- numeric(bins)
  sums <- rowsum(weight, bin, reorder = FALSE)
  weighted_count[as.integer(rownames(sums))] <- sums[, 1]
  list(
    breaks = breaks,
    proportion = weighted_count / sum(weighted_count)
  )
}

format_axis_number <- function(x, drop_leading_zero = FALSE) {
  label <- format(x, trim = TRUE, scientific = FALSE)
  if (drop_leading_zero) {
    label <- sub("^(-?)0\\.", "\\1.", label)
  }
  label
}

draw_effect_histogram <- function(distribution, colour, x_label) {
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(
    mar = c(4.8, 4.6, 1.2, 2.0),
    mgp = c(2.8, 0.75, 0),
    las = 1,
    family = "sans",
    col.axis = "#40505c",
    col.lab = "#17232b"
  )

  if (is.null(distribution) || !isTRUE(distribution$ready)) {
    graphics::par(mar = c(0.8, 0.8, 0.8, 0.8))
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    graphics::rect(0.04, 0.12, 0.96, 0.88, col = "#f5f7f8", border = "#d7dee2")

    heading <- if (is.null(distribution)) {
      "No distribution calculated"
    } else {
      "Not enough matching effects"
    }
    graphics::text(0.5, 0.62, heading, cex = 1.08,
      font = 2, col = "#263740")

    message <- if (is.null(distribution)) {
      "Click Calculate distributions to apply the current search and filters."
    } else if (!is.null(distribution$reason)) {
      distribution$reason
    } else {
      sprintf(
        "%s usable effects found; at least %s are required.",
        distribution$usable_effects,
        distribution$minimum
      )
    }
    message_lines <- strwrap(message, width = 48)
    message_y <- 0.49 - (seq_along(message_lines) - 1) * 0.065
    graphics::text(
      rep(0.5, length(message_lines)),
      message_y,
      message_lines,
      cex = 0.82,
      col = "#667680"
    )

    if (!is.null(distribution)) {
      graphics::text(
        0.5,
        min(message_y) - 0.12,
        "Try broadening the search or filters.",
        cex = 0.79,
        col = "#667680"
      )
    }
    return(invisible(NULL))
  }

  histogram <- weighted_histogram_bins(
    distribution$effect,
    distribution$weight,
    distribution$x_limits
  )
  y_max <- max(histogram$proportion) * 1.16
  graphics::plot(
    NA,
    xlim = distribution$x_limits,
    ylim = c(0, y_max),
    xaxs = "i",
    yaxs = "i",
    axes = FALSE,
    xlab = x_label,
    ylab = "Weighted proportion"
  )
  y_ticks <- pretty(c(0, y_max), n = 5)
  y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= y_max]
  graphics::abline(h = y_ticks, col = "#e8edef", lwd = 1)
  graphics::rect(
    histogram$breaks[-length(histogram$breaks)],
    0,
    histogram$breaks[-1],
    histogram$proportion,
    col = grDevices::adjustcolor(colour, alpha.f = 0.70),
    border = colour,
    lwd = 0.45
  )

  plot_quartiles <- if (is.null(distribution$plot_quartiles)) {
    distribution$quartiles
  } else {
    distribution$plot_quartiles
  }
  graphics::abline(v = plot_quartiles, lty = 2, lwd = 1.15, col = "#1d252a")

  if (!is.null(distribution$x_ticks)) {
    x_ticks <- distribution$x_ticks
    x_tick_labels <- distribution$x_tick_labels
  } else {
    x_ticks <- pretty(distribution$x_limits, n = 5)
    x_ticks <- x_ticks[x_ticks >= distribution$x_limits[1] &
      x_ticks <= distribution$x_limits[2]]
    drop_leading_zero <- if (is.null(distribution$drop_leading_zero)) {
      identical(distribution$design, "between")
    } else {
      distribution$drop_leading_zero
    }
    x_tick_labels <- format_axis_number(
      x_ticks,
      drop_leading_zero = drop_leading_zero
    )
  }
  graphics::axis(1, at = x_ticks, labels = x_tick_labels)
  graphics::axis(2, at = y_ticks, labels = format(y_ticks, digits = 2, trim = TRUE))
  graphics::box(bty = "l", col = "#71808a")

  graphics::legend(
    "topright",
    legend = c(
      sprintf("n = %s articles", distribution$studies),
      sprintf("k = %s effects", distribution$effects_in_plot)
    ),
    inset = c(0.012, 0.02),
    bty = "o",
    bg = grDevices::adjustcolor("white", alpha.f = 0.92),
    box.col = "#c7d2da",
    box.lwd = 0.8,
    cex = 0.76,
    text.col = "#40505c"
  )
  invisible(NULL)
}
