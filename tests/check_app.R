source(file.path("app", "R", "effect_distribution.R"))
effects <- readRDS(file.path("app", "data", "effects.rds"))

stopifnot(
  nrow(effects) > 10000,
  all(c("keywords", "claim", "title", "description") %in% names(effects)),
  all(c("SP", "IDP", "CP") %in% unique(effects$field))
)

all_effects <- filter_effects(
  effects,
  keyword = "",
  fields = unique(effects$field),
  conversion_bases = unique(effects$r_conversion_basis),
  preregistration = "all",
  year_range = range(effects$year, na.rm = TRUE),
  journals = unique(effects$journal)
)
stopifnot(nrow(all_effects) == nrow(effects))

between <- prepare_effect_distribution(all_effects, "between")
within <- prepare_effect_distribution(all_effects, "within")
stopifnot(
  between$ready,
  within$ready,
  length(between$source_effect) == between$usable_effects,
  length(between$source_quartiles) == 3,
  length(between$effect) == between$effects_in_plot,
  length(within$effect) == within$effects_in_plot,
  abs(sum(weighted_histogram_bins(
    between$effect,
    between$weight,
    between$x_limits
  )$proportion) - 1) < 1e-12
)

reference_r <- c(0, 0.1, 0.3, 0.5, 0.8)
reference_d <- 2 * reference_r / sqrt(1 - reference_r^2)
stopifnot(
  isTRUE(all.equal(convert_r_metric(reference_r, "r"), reference_r)),
  isTRUE(all.equal(convert_r_metric(reference_r, "r_squared"), reference_r^2)),
  isTRUE(all.equal(convert_r_metric(reference_r, "d"), reference_d)),
  isTRUE(all.equal(
    convert_r_metric(reference_r, "f"),
    sqrt(reference_r^2 / (1 - reference_r^2))
  )),
  isTRUE(all.equal(
    convert_r_metric(reference_r, "odds_ratio"),
    exp(reference_d * pi / sqrt(3))
  ))
)

for (metric in c("r", "d", "r_squared", "f", "odds_ratio")) {
  displayed <- transform_between_distribution(between, metric)
  stopifnot(
    displayed$ready,
    displayed$metric == metric,
    displayed$effects_in_plot == between$effects_in_plot,
    displayed$studies == between$studies,
    length(displayed$effect) == displayed$effects_in_plot,
    isTRUE(all.equal(
      displayed$quartiles,
      convert_r_metric(between$source_quartiles, metric)
    )),
    abs(sum(weighted_histogram_bins(
      displayed$effect,
      displayed$weight,
      displayed$x_limits
    )$proportion) - 1) < 1e-12
  )
}

odds_ratio_display <- transform_between_distribution(between, "odds_ratio")
stopifnot(isTRUE(all.equal(
  odds_ratio_display$effect,
  pmin(
    log10(convert_r_metric(between$source_effect, "odds_ratio")),
    log10(2000)
  )
)))

rare <- all_effects[seq_len(5), , drop = FALSE]
rare_between <- prepare_effect_distribution(rare, "between")
rare_within <- prepare_effect_distribution(rare, "within")
stopifnot(!rare_between$ready, !rare_within$ready)

between_rows <- effects[!effects$within_subjects & effects$es_combined <= 1, , drop = FALSE]
within_rows <- effects[effects$within_subjects & effects$es_combined <= 2, , drop = FALSE]
mixed <- rbind(between_rows[seq_len(10), ], within_rows[seq_len(5), ])
stopifnot(
  prepare_effect_distribution(mixed, "between")$ready,
  !prepare_effect_distribution(mixed, "within")$ready
)

keyword_fields <- c("keywords", "claim", "title", "description")
tokens <- vapply(keyword_fields, function(column) {
  value <- effects[[column]][which(!is.na(effects[[column]]) & nzchar(effects[[column]]))[1]]
  strsplit(value, "[^[:alnum:]]+")[[1]][1]
}, character(1))
for (token in tokens[nzchar(tokens)]) {
  result <- filter_effects(
    effects,
    token,
    unique(effects$field),
    unique(effects$r_conversion_basis),
    "all",
    range(effects$year, na.rm = TRUE),
    unique(effects$journal)
  )
  stopifnot(nrow(result) > 0)
}

synthetic <- effects[rep(1, 4), , drop = FALSE]
synthetic$keywords <- c("alpha-only", "", "", "")
synthetic$claim <- c("", "bravo-only", "", "")
synthetic$title <- c("", "", "charlie-only", "")
synthetic$description <- c("", "", "", "delta-only")
for (token in c("alpha-only", "bravo-only", "charlie-only", "delta-only")) {
  result <- filter_effects(
    synthetic,
    token,
    unique(synthetic$field),
    unique(synthetic$r_conversion_basis),
    "all",
    range(synthetic$year),
    unique(synthetic$journal)
  )
  stopifnot(nrow(result) == 1)
}

comma_result <- filter_effects(
  synthetic,
  "alpha-only, delta-only",
  unique(synthetic$field),
  unique(synthetic$r_conversion_basis),
  "all",
  range(synthetic$year),
  unique(synthetic$journal)
)
stopifnot(nrow(comma_result) == 2)

cat("All app checks passed.\n")
