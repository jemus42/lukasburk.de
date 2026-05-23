# Talks & workshops helpers for talks.qmd.

month_names <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

read_talks <- function(path = "talks.yml") {
  talks <- yaml::read_yaml(path) |>
    lapply(as.data.frame) |>
    do.call(rbind, args = _)
  talks[order(talks$year, decreasing = TRUE), , drop = FALSE]
}

read_workshops <- function(path = "workshops.yml") {
  if (file.exists(path)) yaml::read_yaml(path) else NULL
}

workshop_date <- function(w) {
  if (!is.null(w$years)) {
    paste(as.integer(unlist(w$years)), collapse = ", ")
  } else if (!is.null(w$month)) {
    paste(month_names[as.integer(w$month)], w$year)
  } else {
    as.character(w$year)
  }
}

# Sort year: max(years) for recurring, else year. Year-only entries sort to
# top of their year; entries with explicit month sort by that month.
sort_year <- function(w) {
  if (!is.null(w$years)) max(as.integer(unlist(w$years))) else as.integer(w$year)
}

render_talks <- function(talks) {
  cat("## Talks\n\n")
  for (i in seq_len(nrow(talks))) {
    t <- talks[i, , drop = FALSE]
    cat(sprintf(
      "- [%s %s](%s)  \n  *%s* — [Slides](%s)\n\n",
      t$venue, t$year, t$url, t$title, t$slides
    ))
  }
}

render_workshops <- function(workshops) {
  if (length(workshops) == 0) {
    return(invisible())
  }
  cat("## Workshops\n\n")
  sort_keys <- vapply(workshops, function(w) {
    sprintf(
      "%04d-%02d",
      sort_year(w),
      if (!is.null(w$month)) as.integer(w$month) else 12L
    )
  }, character(1))
  workshops <- workshops[order(sort_keys, decreasing = TRUE)]

  for (w in workshops) {
    desc <- if (!is.null(w$description)) {
      sub("[[:space:].]+$", "", w$description)
    }
    meta <- c(
      paste0("With ", paste(w$people, collapse = ", ")),
      desc,
      if (!is.null(w$materials)) sprintf("[Materials](%s)", w$materials)
    )
    cat(sprintf(
      "- **%s**  \n  [%s %s](%s)  \n  %s\n\n",
      w$title,
      w$venue,
      workshop_date(w),
      w$url,
      paste(meta, collapse = ". ")
    ))
  }
}
