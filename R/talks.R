# Talks & workshops helpers for talks.qmd.

month_names <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

read_talks <- function(path = "talks.yml") {
  talks <- yaml::read_yaml(path)
  years <- vapply(talks, function(t) as.integer(t$year), integer(1))
  talks[order(years, decreasing = TRUE)]
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
  for (t in talks) {
    venue_long <- if (!is.null(t$venue_full)) {
      sprintf("  <span class=\"venue-full\">%s</span>  \n", t$venue_full)
    } else {
      ""
    }
    location <- if (!is.null(t$location)) {
      sprintf("  <span class=\"venue-location\">%s</span>  \n", t$location)
    } else {
      ""
    }
    cat(sprintf(
      "- [%s %s](%s)  \n%s%s  *%s* — [Slides](%s)\n\n",
      t$venue, t$year, t$url, venue_long, location, t$title, t$slides
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
    location <- if (!is.null(w$location)) {
      sprintf("  <span class=\"venue-location\">%s</span>  \n", w$location)
    } else {
      ""
    }
    cat(sprintf(
      "- **%s**  \n  [%s %s](%s)  \n%s  %s\n\n",
      w$title,
      w$venue,
      workshop_date(w),
      w$url,
      location,
      paste(meta, collapse = ". ")
    ))
  }
}
