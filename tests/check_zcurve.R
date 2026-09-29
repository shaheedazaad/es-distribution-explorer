source(file.path("app", "R", "zcurve.R"))
effects <- readRDS(file.path("app", "data", "effects.rds"))
stopifnot(as.character(utils::packageVersion("zcurve")) == expected_zcurve_version)

example <- effects[rep(1, 2), , drop = FALSE]
example$es_combined <- 0.5
example$sample_size <- 30
example$within_subjects <- c(FALSE, TRUE)
prepared <- prepare_zcurve_effects(example)
stopifnot(
  isTRUE(all.equal(prepared$p_value[1],
    2 * pt(0.5 * sqrt(28 / (1 - 0.5^2)), df = 28, lower.tail = FALSE))),
  isTRUE(all.equal(prepared$p_value[2],
    2 * pt(0.5 * sqrt(30), df = 29, lower.tail = FALSE)))
)

prepared <- prepare_zcurve_effects(effects[1:300, , drop = FALSE])
doi <- fit_clustered_zcurve(prepared, "doi", bootstraps = 3L)
study <- fit_clustered_zcurve(prepared, "study", bootstraps = 3L)
stopifnot(
  doi$ready, study$ready,
  study$clusters >= doi$clusters,
  inherits(doi$model, "zcurve"),
  max(doi$model$data) <= 8,
  all(is.finite(doi$estimates)),
  all(doi$estimates >= 0 & doi$estimates <= 1)
)

extreme <- prepared[1:100, , drop = FALSE]
extreme$p_value[1] <- 1e-320
extreme$z[1] <- stats::qnorm(extreme$p_value[1] / 2, lower.tail = FALSE)
extreme_fit <- fit_clustered_zcurve(extreme, "doi", bootstraps = 2L)
stopifnot(extreme_fit$ready, max(extreme_fit$model$data) <= 8,
  extreme_fit$model$N_obs == nrow(extreme),
  all(is.finite(extreme_fit$estimates)), extreme$p_value[1] == 1e-320)

app_environment <- new.env(parent = globalenv())
source(file.path("app", "app.R"), local = app_environment, chdir = TRUE)
archive <- tempfile(fileext = ".tar.gz")
directory <- tempfile("zcurve-check-")
dir.create(directory)
on.exit(unlink(c(archive, directory), recursive = TRUE), add = TRUE)
full <- fit_clustered_zcurve(prepare_zcurve_effects(effects), bootstraps = 2L)
full$prepared <- prepare_zcurve_effects(effects)
stopifnot(full$model$N_obs == nrow(full$prepared))
full$filters <- list(keyword = "", fields = app_environment$available_fields,
    conversion_bases = app_environment$available_bases,
    preregistration = "all", year_range = app_environment$year_limits,
    journals = app_environment$available_journals)
app_environment$write_zcurve_reproduction_package(full, archive)
utils::untar(archive, exdir = directory)
old_directory <- setwd(directory)
source("reproduce.R", local = TRUE)
stopifnot(isTRUE(all.equal(readRDS("zcurve_fit.rds")$coefficients,
  full$model$coefficients)))
source("refit.R", local = TRUE)
setwd(old_directory)
stopifnot(file.exists(file.path(directory, c(
  "zcurve_estimates.csv", "zcurve_effects.csv", "zcurve.pdf",
  "reproduced_zcurve.pdf", "refit_estimates.csv", "refit_zcurve.pdf"))))
cat("Z-curve checks passed.\n")
