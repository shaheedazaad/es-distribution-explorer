#!/usr/bin/env Rscript

# Convert the editable application CSV into the compressed RDS loaded by Shiny.
# Also strips the source CSV itself down to only the columns the app actually
# uses, so the checked-in CSV and the RDS built from it always stay in sync.
#
# Default usage, from the repository root:
#   Rscript scripts/csv_to_rds.R
#
# Optional custom paths:
#   Rscript scripts/csv_to_rds.R input.csv output.rds

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1L) args[[1]] else "data/effects.csv"
output_file <- if (length(args) >= 2L) args[[2]] else "app/data/effects.rds"

if (!file.exists(input_file)) {
  stop(
    "Input CSV not found: ", input_file, "\n",
    "Place the editable CSV at data/effects.csv or provide its path as the first argument.",
    call. = FALSE
  )
}

required_columns <- c(
  "doi", "title", "year", "journal", "keywords", "study", "description",
  "sample_size", "pre_registration", "r_conversion_basis",
  "es_combined", "within_subjects", "field"
)

effects <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA"),
  encoding = "UTF-8"
)

missing_columns <- setdiff(required_columns, names(effects))
if (length(missing_columns)) {
  stop(
    "The CSV is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(effects) == 0L) {
  stop("The CSV contains no effect rows.", call. = FALSE)
}

parse_numeric_column <- function(x, name) {
  original <- x
  parsed <- suppressWarnings(as.numeric(x))
  invalid <- !is.na(original) & nzchar(trimws(as.character(original))) & is.na(parsed)
  if (any(invalid)) {
    examples <- unique(as.character(original[invalid]))
    stop(
      "Column '", name, "' contains non-numeric values, including: ",
      paste(head(examples, 5L), collapse = ", "),
      call. = FALSE
    )
  }
  parsed
}

parse_logical_column <- function(x, name) {
  if (is.logical(x)) {
    return(x)
  }

  normalized <- tolower(trimws(as.character(x)))
  normalized[is.na(x)] <- NA_character_
  parsed <- rep(NA, length(normalized))
  parsed[normalized %in% c("true", "t", "1", "yes", "y")] <- TRUE
  parsed[normalized %in% c("false", "f", "0", "no", "n")] <- FALSE

  invalid <- !is.na(normalized) & is.na(parsed)
  if (any(invalid)) {
    examples <- unique(normalized[invalid])
    stop(
      "Column '", name, "' contains unrecognized logical values, including: ",
      paste(head(examples, 5L), collapse = ", "),
      call. = FALSE
    )
  }
  parsed
}

effects$year <- parse_numeric_column(effects$year, "year")
effects$sample_size <- parse_numeric_column(effects$sample_size, "sample_size")
effects$es_combined <- parse_numeric_column(effects$es_combined, "es_combined")
effects$pre_registration <- parse_logical_column(
  effects$pre_registration,
  "pre_registration"
)
effects$within_subjects <- parse_logical_column(
  effects$within_subjects,
  "within_subjects"
)

character_columns <- setdiff(
  required_columns,
  c("year", "sample_size", "es_combined", "pre_registration", "within_subjects")
)
effects[character_columns] <- lapply(effects[character_columns], function(x) {
  ifelse(is.na(x), NA_character_, enc2utf8(as.character(x)))
})

effects$study[is.na(effects$study) | trimws(effects$study) == ""] <- "Study 1"
effects$journal <- gsub("&amp;", "&", effects$journal, fixed = TRUE)

invalid_field <- !is.na(effects$field) & !effects$field %in% c("SP", "IDP", "CP")
if (any(invalid_field)) {
  stop(
    "Column 'field' must use SP, IDP, or CP. Invalid values: ",
    paste(unique(effects$field[invalid_field]), collapse = ", "),
    call. = FALSE
  )
}

effects <- effects[required_columns]

# Overwrite the source CSV with the stripped-down column set so the editable
# CSV never drifts from what the app can actually use.
dir.create(dirname(input_file), recursive = TRUE, showWarnings = FALSE)

temporary_csv <- tempfile("effects-", tmpdir = dirname(input_file), fileext = ".csv")
on.exit(unlink(temporary_csv), add = TRUE)
write.csv(effects, temporary_csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")

# Confirm that the stripped CSV round-trips before replacing the source file.
check_csv <- read.csv(
  temporary_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA"),
  encoding = "UTF-8"
)
stopifnot(nrow(check_csv) == nrow(effects), identical(names(check_csv), required_columns))

if (!file.copy(temporary_csv, input_file, overwrite = TRUE)) {
  stop("Could not write the stripped-down CSV: ", input_file, call. = FALSE)
}

# `on.exit()` is retained as failure cleanup, while this explicit removal is
# needed when the script is evaluated at top level by Rscript.
unlink(temporary_csv)

# Build the RDS from the same stripped-down data now saved to the CSV.
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

temporary_output <- tempfile("effects-", tmpdir = dirname(output_file), fileext = ".rds")
on.exit(unlink(temporary_output), add = TRUE)
saveRDS(effects, temporary_output, version = 2, compress = "gzip")

# Confirm that the serialized file is readable before replacing the app data.
check <- readRDS(temporary_output)
stopifnot(nrow(check) == nrow(effects), identical(names(check), required_columns))

if (!file.copy(temporary_output, output_file, overwrite = TRUE)) {
  stop("Could not write the RDS file: ", output_file, call. = FALSE)
}

# `on.exit()` is retained as failure cleanup, while this explicit removal is
# needed when the script is evaluated at top level by Rscript.
unlink(temporary_output)

cat(
  "Converted ", nrow(effects), " rows\n",
  "  CSV: ", normalizePath(input_file), " (stripped to ", length(required_columns), " columns)\n",
  "  RDS: ", normalizePath(output_file), "\n",
  sep = ""
)
