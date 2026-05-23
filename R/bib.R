# Bibliography helpers for publications.qmd.
# Single source of truth: references.bib. Edit it then `make publications`.

`%||%` <- function(a, b) if (is.null(a)) b else a

strip_braces <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }
  gsub("[{}]", "", x)
}

clean_text <- function(s) {
  if (is.null(s) || is.na(s)) {
    return("")
  }
  gsub("\\s+", " ", trimws(strip_braces(s)))
}

# biblatex-flavour parser. Depth-tracks braces; assumes well-formed entries
# (the bib is hand-maintained and small).
parse_bib <- function(path) {
  text <- paste(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  entry_re <- "@(\\w+)\\s*\\{\\s*([^,\\s]+)\\s*,"
  matches <- gregexpr(entry_re, text, perl = TRUE)[[1]]
  starts <- as.integer(matches)
  if (starts[1] == -1L) {
    return(list())
  }
  match_lens <- attr(matches, "match.length")

  entries <- vector("list", length(starts))
  n <- nchar(text)

  for (i in seq_along(starts)) {
    header_end <- starts[i] + match_lens[i] - 1L
    depth <- 1L
    pos <- header_end + 1L
    while (pos <= n && depth > 0L) {
      ch <- substr(text, pos, pos)
      if (ch == "{") {
        depth <- depth + 1L
      } else if (ch == "}") {
        depth <- depth - 1L
      }
      pos <- pos + 1L
    }
    entry_text <- substr(text, starts[i], pos - 1L)
    hdr <- regmatches(entry_text, regexec(entry_re, entry_text, perl = TRUE))[[1]]
    type <- tolower(hdr[2])
    key <- trimws(hdr[3])

    body <- substr(entry_text, match_lens[i] + 1L, nchar(entry_text) - 1L)
    bn <- nchar(body)
    bp <- 1L
    fields <- list()
    while (bp <= bn) {
      rest <- substr(body, bp, bn)
      fm <- regexec("(\\w+)\\s*=\\s*\\{", rest, perl = TRUE)[[1]]
      if (fm[1] == -1L) {
        break
      }
      mm <- regmatches(rest, regexec("(\\w+)\\s*=\\s*\\{", rest, perl = TRUE))[[1]]
      field_name <- tolower(mm[2])
      header_len <- attr(fm, "match.length")[1]
      val_start <- bp + fm[1] + header_len - 1L
      d <- 1L
      p <- val_start
      while (p <= bn && d > 0L) {
        c <- substr(body, p, p)
        if (c == "{") {
          d <- d + 1L
        } else if (c == "}") {
          d <- d - 1L
        }
        if (d > 0L) p <- p + 1L
      }
      fields[[field_name]] <- substr(body, val_start, p - 1L)
      bp <- p + 1L
    }
    entries[[i]] <- list(type = type, key = key, fields = fields)
  }
  entries
}

# "Burk, Lukas and Bender, Andreas" -> c("L. Burk", "A. Bender")
parse_authors <- function(s) {
  if (is.null(s) || is.na(s)) {
    return(character())
  }
  parts <- strsplit(strip_braces(s), "\\s+and\\s+")[[1]]
  vapply(
    parts,
    function(p) {
      if (grepl(",", p)) {
        sp <- strsplit(p, "\\s*,\\s*")[[1]]
        surname <- sp[1]
        given <- sp[2]
      } else {
        sp <- strsplit(p, "\\s+")[[1]]
        surname <- sp[length(sp)]
        given <- paste(sp[-length(sp)], collapse = " ")
      }
      tokens <- strsplit(given, "\\s+")[[1]]
      initials <- vapply(
        tokens,
        function(t) {
          sub_tokens <- strsplit(t, "-")[[1]]
          paste(
            vapply(
              sub_tokens,
              function(st) paste0(substr(st, 1, 1), "."),
              character(1)
            ),
            collapse = "-"
          )
        },
        character(1)
      )
      paste(paste(initials, collapse = " "), surname)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

format_authors <- function(s, bold_self = TRUE) {
  a <- parse_authors(s)
  if (length(a) == 0) {
    return("")
  }
  if (bold_self) {
    a <- ifelse(grepl("Burk$", a), paste0("**", a, "**"), a)
  }
  if (length(a) == 1) {
    return(a)
  }
  if (length(a) == 2) {
    return(paste(a, collapse = " and "))
  }
  paste0(paste(a[-length(a)], collapse = ", "), ", and ", a[length(a)])
}

parse_date <- function(s, precision = c("month", "day")) {
  precision <- match.arg(precision)
  if (is.null(s) || is.na(s)) {
    return(list(display = NA, sort = "0000-00-00"))
  }
  parts <- strsplit(strip_braces(s), "-")[[1]]
  year <- parts[1]
  month <- if (length(parts) >= 2) parts[2] else NA_character_
  day <- if (length(parts) >= 3) parts[3] else NA_character_
  month_names <- c(
    "Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.",
    "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."
  )
  display <- if (precision == "day" && !is.na(month) && !is.na(day)) {
    sprintf("%s %s, %s", month_names[as.integer(month)], day, year)
  } else if (!is.na(month)) {
    paste0(month_names[as.integer(month)], " ", year)
  } else {
    year
  }
  sort_key <- paste(
    year,
    if (!is.na(month)) sprintf("%02d", as.integer(month)) else "01",
    if (!is.na(day)) sprintf("%02d", as.integer(day)) else "01",
    sep = "-"
  )
  list(display = display, sort = sort_key)
}

fix_pages <- function(p) {
  if (is.null(p)) {
    return(NULL)
  }
  gsub("--", "–", strip_braces(p))
}

format_doi <- function(d) {
  if (is.null(d) || is.na(d)) {
    return("")
  }
  d <- strip_braces(d)
  sprintf("doi: [%s](https://doi.org/%s)", d, d)
}

format_article <- function(e) {
  f <- e$fields
  bits <- c(
    if (!is.null(f$volume)) paste0("vol. ", f$volume),
    if (!is.null(f$number)) paste0("no. ", f$number),
    if (!is.null(f$pages)) paste0("p. ", fix_pages(f$pages)),
    parse_date(f$date %||% f$year)$display
  )
  sprintf(
    '- %s, "%s", %s, %s, %s.',
    format_authors(f$author),
    clean_text(f$title),
    clean_text(f$journaltitle %||% f$journal),
    paste(bits, collapse = ", "),
    format_doi(f$doi)
  )
}

format_online <- function(e) {
  f <- e$fields
  sprintf(
    '- %s, "%s." arXiv, %s. %s.',
    format_authors(f$author),
    clean_text(f$title),
    parse_date(f$date, "day")$display,
    format_doi(f$doi)
  )
}

format_incollection <- function(e) {
  f <- e$fields
  editors <- if (!is.null(f$editor)) {
    eds <- gsub("\\*\\*", "", format_authors(f$editor, bold_self = FALSE))
    paste0(eds, ", Eds., ")
  } else {
    ""
  }
  edition <- if (!is.null(f$edition)) {
    n <- as.integer(strip_braces(f$edition))
    suf <- switch(
      as.character(n %% 10),
      "1" = "st", "2" = "nd", "3" = "rd", "th"
    )
    if (!is.na(n) && n %% 100 %in% 11:13) {
      suf <- "th"
    }
    paste0(n, suf, " ed., ")
  } else {
    ""
  }
  publisher <- if (!is.null(f$publisher)) {
    paste0(clean_text(f$publisher), ", ")
  } else {
    ""
  }
  pages <- if (!is.null(f$pages)) {
    paste0("pp. ", fix_pages(f$pages), ", ")
  } else {
    ""
  }
  sprintf(
    '- %s, "%s" in *%s*, %s. %s.',
    format_authors(f$author),
    clean_text(f$title),
    clean_text(f$booktitle),
    paste0(editors, edition, publisher, pages,
           parse_date(f$date %||% f$year)$display),
    format_doi(f$doi)
  )
}

render_bibliography <- function(path = "references.bib") {
  entries <- parse_bib(path)
  buckets <- vapply(entries, function(e) {
    switch(e$type,
      article = "published",
      online = "preprint",
      incollection = "book",
      "other"
    )
  }, character(1))
  sort_keys <- vapply(
    entries,
    function(e) parse_date(e$fields$date %||% e$fields$year)$sort,
    character(1)
  )

  emit <- function(heading, formatter, ix) {
    if (!length(ix)) {
      return(invisible())
    }
    cat("## ", heading, "\n\n", sep = "")
    ix <- ix[order(sort_keys[ix], decreasing = TRUE)]
    for (j in ix) cat(formatter(entries[[j]]), "\n\n", sep = "")
  }

  emit("Published Articles", format_article, which(buckets == "published"))
  emit("Preprints", format_online, which(buckets == "preprint"))
  emit("Book Chapters", format_incollection, which(buckets == "book"))
}
