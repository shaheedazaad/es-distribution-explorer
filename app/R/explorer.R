# Base-R table operations also run inside Shinylive's webR runtime.
explore_effects <- function(data, search = "", column = names(data)[1], descending = FALSE) {
  search <- trimws(search)
  if (nzchar(search)) {
    matches <- lapply(data, function(values) {
      !is.na(values) & grepl(tolower(search), tolower(as.character(values)), fixed = TRUE)
    })
    data <- data[Reduce(`|`, matches), , drop = FALSE]
  }
  if (column %in% names(data)) {
    data <- data[order(data[[column]], decreasing = descending, na.last = TRUE), , drop = FALSE]
  }
  data
}

explorer_page <- function(data, page = 1L, size = 25L) {
  pages <- max(1L, ceiling(nrow(data) / size))
  page <- max(1L, min(page, pages))
  first <- (page - 1L) * size + 1L
  last <- min(page * size, nrow(data))
  list(data = data[if (nrow(data)) seq.int(first, last) else integer(), , drop = FALSE],
    page = page, pages = pages, first = if (nrow(data)) first else 0L, last = last)
}
