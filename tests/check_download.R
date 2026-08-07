run_download_checks <- function() {
  app_environment <- new.env(parent = globalenv())
  source(file.path("app", "app.R"), local = app_environment, chdir = TRUE)

  button <- app_environment$shinylive_download_button(
    "test-download",
    "Download"
  )
  stopifnot(is.null(button$attribs$download))

  archive <- tempfile(fileext = ".tar.gz")
  extraction_directory <- tempfile("reproduction-package-")
  dir.create(extraction_directory)
  on.exit(unlink(
    c(archive, extraction_directory),
    recursive = TRUE,
    force = TRUE
  ))

  result <- list(
    filters = list(
      keyword = "",
      fields = app_environment$available_fields,
      conversion_bases = app_environment$available_bases,
      preregistration = "all",
      year_range = app_environment$year_limits,
      journals = app_environment$available_journals
    ),
    between_metric = "odds_ratio"
  )
  app_environment$write_reproduction_package(result, archive)

  expected_files <- c("effects.csv", "README.txt", "reproduce.R")
  stopifnot(
    file.exists(archive),
    file.info(archive)$size > 0,
    setequal(utils::untar(archive, list = TRUE), expected_files)
  )

  utils::untar(archive, exdir = extraction_directory)
  reproduction_script <- file.path(extraction_directory, "reproduce.R")
  stopifnot(
    length(parse(reproduction_script)) > 0,
    identical(
      nrow(read.csv(file.path(extraction_directory, "effects.csv"))),
      nrow(app_environment$effects)
    ),
    any(grepl(
      'selected_between_metric <- "odds_ratio"',
      readLines(reproduction_script, warn = FALSE),
      fixed = TRUE
    ))
  )

  cat("Download checks passed.\n")
}

run_download_checks()
