#' @title Summarize Search Results
#' @param x catalog data.frame
#' @return data.frame
#' @export

search_summary <- function(x) {
  res <- by(x, list(x$id, x$varname), function(i) {
    c(
      id = unique(i$id),
      long_name = unique(i$long_name),
      variable = unique(i$varname),
      count = nrow(i)
    )
  })

  data.frame(do.call(rbind, res))
}

#' @title Search Catalog
#' @description Search the data catalog to find an ideal source.
#' @param query a string to search for.
#' @param source If the source id you want is known, provide it to expedite search
#' @details query should be passed as a string or list of key word. If a single string is passed, it will be split at spaces.
#' @return data.frame
#' @export

query = "maca daily huss pr bnu-esm"

search <- function(query = NULL, source = NULL) {
  if (!is.null(source)) {
    x <- opendap.catalog::params[opendap.catalog::params$id == source, ]
  } else {
    x <- opendap.catalog::params
  }

  subs <- x[, !names(x) %in% c("grid.id", "URL", "tiled", "units", "T_name", "nT", "duration")]

  if (!is.null(query)) {
    .query(x, query, subs)
  } else {
    x
  }
}

#' @title Internal query
#' @param x catalog data.frame
#' @param query User query
#' @param subs subset of x by column (optional)
#' @return data.frame
#' @export
#' @importFrom utils adist

.query <- function(x, query, subs = NULL) {
  query <- gsub("daily", "day", query)
  query <- gsub("monthly", "month", query)
  query <- gsub("hourly", "hour", query)

  if (is.null(subs)) {
    subs <- x
  }

  splits <- split(subs, seq(nrow(x)))

  q <- strsplit(query, " ")[[1]]

  indices <- unlist(lapply(splits, function(y) {
    m <- adist(q,
      y,
      ignore.case = TRUE,
      partial     = TRUE
    )


    sum(apply(m, 1, min, na.rm = TRUE))
  }))

  x$rank <- indices

  if (min(indices) > length(q) * 3) {
    warning("No likely matches found.")
  }

  x <- x[x$rank == min(indices), ]

  x[order(x$rank), ]
}
