app_environment <- new.env(parent = globalenv())
source(file.path("app", "app.R"), local = app_environment, chdir = TRUE)
with(app_environment, {
  example <- data.frame(doi = c("b", "a", "c"), title = c("Alpha, quoted \"text\"", "beta", NA),
    es_combined = c(2, 10, NA), within_subjects = c(TRUE, FALSE, TRUE))
  stopifnot(
    nrow(explore_effects(example, "ALPHA,")) == 1L,
    nrow(explore_effects(example, "[")) == 0L,
    identical(explore_effects(example, column = "es_combined")$doi, c("b", "a", "c")),
    identical(explore_effects(example, column = "es_combined", descending = TRUE)$doi, c("a", "b", "c")),
    explorer_page(example, 99, 2)$page == 2,
    nrow(explorer_page(example, 2, 2)$data) == 1,
    explorer_page(example[FALSE, ], 1, 25)$first == 0
  )
})

shiny::testServer(app_environment$server, {
  session$setInputs(view = "zcurve", keyword = "", field = app_environment$available_fields,
    conversion_basis = app_environment$available_bases, preregistration = "all",
    year_range = app_environment$year_limits, journal = app_environment$available_journals,
    between_metric = "r")
  session$setInputs(calculate = 1)
  stopifnot(!is.null(zcurve_search()))
  session$setInputs(explore_zcurve = 1, explorer_search = "", explorer_sort = "sample_size",
    explorer_direction = "desc", explorer_size = "10")
  stopifnot(nrow(explorer_rows()) == nrow(app_environment$effects),
    nrow(explorer_current_page()$data) == 10)
  session$setInputs(explorer_next = 1)
  stopifnot(explorer_current_page()$page == 2)
  # Editing the sidebar does not change the completed search or its explorer.
  session$setInputs(keyword = "no-such-keyword-12345")
  stopifnot(nrow(explorer_rows()) == nrow(app_environment$effects))
  session$setInputs(explorer_search = "Cognition")
  stopifnot(explorer_current_page()$page == 1, nrow(explorer_rows()) > 10,
    nrow(explorer_rows()) < nrow(app_environment$effects))
  downloaded <- read.csv(output$download_effects, check.names = FALSE)
  stopifnot(nrow(downloaded) == nrow(explorer_rows()),
    identical(downloaded$doi, explorer_rows()$doi), nrow(downloaded) > 10)
  stopifnot(grepl("<table", output$explorer_table, fixed = TRUE))
  session$setInputs(explorer_search = "no-such-keyword-12345")
  stopifnot(nrow(explorer_rows()) == 0, explorer_current_page()$pages == 1)
  # A completed distribution calculation uses its own snapshot, even if too small to plot.
  session$setInputs(view = "distributions", calculate = 2)
  session$setInputs(explore_distribution = 1, explorer_search = "")
  stopifnot(nrow(explorer_snapshot()) == 0)
  # Opening z-curve again retrieves the previous z-curve search.
  session$setInputs(explore_zcurve = 2)
  stopifnot(nrow(explorer_snapshot()) == nrow(app_environment$effects))
})
cat("Explorer checks passed.\n")
