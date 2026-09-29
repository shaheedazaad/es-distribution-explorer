# Convert stored r and dz effect sizes to two-sided p-values, then let the
# zcurve package handle clustered fitting, intervals, and plotting.

expected_zcurve_version <- "2.4.2"

prepare_zcurve_effects <- function(data) {
  valid <- is.finite(data$es_combined) & is.finite(data$sample_size) &
    !is.na(data$within_subjects) & !is.na(data$doi) & nzchar(data$doi) &
    !is.na(data$study) & nzchar(data$study)
  valid <- valid & ifelse(data$within_subjects,
    data$sample_size > 1, data$sample_size > 2 & abs(data$es_combined) < 1)
  selected <- data[valid, , drop = FALSE]
  if (!nrow(selected)) return(transform(selected, p_value = numeric(), z = numeric()))

  statistic <- numeric(nrow(selected))
  df <- numeric(nrow(selected))
  within <- selected$within_subjects
  statistic[within] <- abs(selected$es_combined[within]) * sqrt(selected$sample_size[within])
  df[within] <- selected$sample_size[within] - 1
  r <- selected$es_combined[!within]
  n <- selected$sample_size[!within]
  statistic[!within] <- abs(r) * sqrt((n - 2) / (1 - r^2))
  df[!within] <- n - 2
  selected$p_value <- 2 * stats::pt(statistic, df = df, lower.tail = FALSE)
  selected$z <- stats::qnorm(selected$p_value / 2, lower.tail = FALSE)
  selected$z[is.infinite(selected$z) & selected$p_value == 0] <- 8
  selected[is.finite(selected$p_value) & is.finite(selected$z), , drop = FALSE]
}

fit_clustered_zcurve <- function(prepared, cluster_by = "doi", bootstraps = 500L,
                                 seed = 912L) {
  stopifnot(cluster_by %in% c("doi", "study"), bootstraps >= 1L)
  if (as.character(utils::packageVersion("zcurve")) != expected_zcurve_version) {
    stop("This app requires zcurve ", expected_zcurve_version,
         " so its fits match the Shinylive build.")
  }
  cutoff <- stats::qnorm(0.025, lower.tail = FALSE)
  significant <- prepared[prepared$z >= cutoff, , drop = FALSE]
  if (nrow(significant) < 10L) {
    return(list(ready = FALSE, usable = nrow(prepared), significant = nrow(significant),
                reason = "At least 10 significant effects are needed to fit a z-curve."))
  }
  cluster <- if (cluster_by == "doi") prepared$doi else
    paste(prepared$doi, prepared$study, sep = "\r")
  if (length(unique(cluster[prepared$z >= cutoff])) < 2L) {
    return(list(ready = FALSE, usable = nrow(prepared), significant = nrow(significant),
                reason = "At least two independent clusters are needed."))
  }

  # zcurve_data converts z back to p; for z near 38 that conversion underflows
  # to zero. The model's upper fitting bound is 6, so values above 8 all enter
  # the same upper tail. Preserve the original p-values in `prepared`.
  safe_z <- pmin(prepared$z, 8)
  # `rounded = FALSE` treats calculated test statistics as exact rather than
  # introducing censoring based on the number of displayed digits.
  input <- zcurve::zcurve_data(
    paste0("z=", format(safe_z, digits = 15, scientific = FALSE, trim = TRUE)),
    id = cluster, rounded = FALSE
  )
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  model <- suppressWarnings(zcurve::zcurve_clustered(
    input, method = "b", bootstrap = bootstraps, parallel = FALSE
  ))
  list(ready = TRUE, model = model, estimates = summary(model)$coefficients,
       usable = nrow(prepared), significant = nrow(significant),
       clusters = length(unique(cluster[prepared$z >= cutoff])),
       cluster_by = cluster_by, bootstraps = bootstraps)
}

draw_zcurve <- function(result) {
  if (is.null(result) || !isTRUE(result$ready)) {
    graphics::plot.new()
    graphics::text(.5, .5, if (is.null(result))
      "Search the dataset, then fit a z-curve." else result$reason)
    return(invisible(NULL))
  }
  graphics::plot(result$model, CI = TRUE, annotation = TRUE,
                 plot_type = "base", cex.anno = 0.75)
}
