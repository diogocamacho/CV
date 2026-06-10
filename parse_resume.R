# parse_resume.R
# Reads resume.json and returns vitae-ready tibbles
# Source this file from your Rmd templates: source("parse_resume.R")

library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

# ── Load ──────────────────────────────────────────────────────
resume <- read_json("resume.json", simplifyVector = FALSE)

# ── LaTeX escape ──────────────────────────────────────────────
# Used when emitting raw LaTeX (\textbf{}) via .protect = FALSE in
# detailed_entries(): we bypass vitae's auto-escaping for the whole `why`
# field, so we must escape data ourselves.
latex_escape <- function(s) {
  if (is.null(s) || !nzchar(s)) return(s)
  s <- gsub("\\\\", "\\\\textbackslash ", s)
  s <- gsub("([&%$#_{}])", "\\\\\\1", s)
  s <- gsub("\\^", "\\\\^{}", s)
  s <- gsub("~", "\\\\~{}", s)
  s
}

# ── Basics ────────────────────────────────────────────────────
basics <- resume$basics

parse_summary <- function() resume$basics$summary

# Returns one markdown bullet per summary_highlight, joined by newlines.
# Emit via a results='asis' chunk so pandoc parses the bullets.
parse_summary_highlights <- function() {
  hl <- resume$basics$summary_highlights
  if (is.null(hl) || length(hl) == 0) return("")
  paste(vapply(hl, function(h) {
    paste0("- **", h$bold, "**: ", h$text)
  }, character(1)), collapse = "\n")
}

# ── Work experience ───────────────────────────────────────────
# Returns a tibble ready for vitae::detailed_entries()
# profile: character — key from resume$profiles (e.g. "executive", "az_cvrm")
#          if NULL, returns all included entries
parse_work <- function(profile = NULL) {

  work <- resume$work

  # Filter by profile work_filter if specified
  if (!is.null(profile) && !is.null(resume$profiles[[profile]])) {
    allowed_ids <- resume$profiles[[profile]]$work_filter
    work <- keep(work, ~ .x$id %in% allowed_ids)
  }

  # Only include entries with include:true
  work <- keep(work, ~ isTRUE(.x$include))

  collapsed_ids <- character(0)
  if (!is.null(profile) && !is.null(resume$profiles[[profile]]$earlier_roles_collapsed)) {
    collapsed_ids <- resume$profiles[[profile]]$earlier_roles_collapsed
  }

  map_dfr(work, function(job) {

    # Format dates
    start <- format(as.Date(paste0(job$startDate, "-01")), "%b %Y")
    end   <- if (is.null(job$endDate)) "Present" else
               format(as.Date(paste0(job$endDate, "-01")), "%b %Y")
    dates <- paste(start, "-", end)

    format_bullet <- function(h) {
      if (!is.null(h$bold) && nchar(h$bold) > 0)
        paste0("\\textbf{", latex_escape(h$bold), "}: ", latex_escape(h$text))
      else
        latex_escape(h$text)
    }

    # Collapsed (earlier roles) — one-line bullet per role
    if (job$id %in% collapsed_ids) {
      active_bullets <- keep(job$highlights, ~ isTRUE(.x$include))
      bullet_text <- if (length(active_bullets) == 0) NA_character_ else map_chr(active_bullets, format_bullet)
      return(tibble(
        what  = latex_escape(job$position),
        when  = dates,
        with  = latex_escape(job$company),
        where = latex_escape(job$location),
        why   = bullet_text
      ))
    }

    # Full entries — all active highlights as bullets
    active_bullets <- keep(job$highlights, ~ isTRUE(.x$include))

    if (length(active_bullets) == 0) {
      return(tibble(
        what  = latex_escape(job$position),
        when  = dates,
        with  = latex_escape(job$company),
        where = latex_escape(job$location),
        why   = NA_character_
      ))
    }

    bullet_text <- map_chr(active_bullets, format_bullet)

    tibble(
      what  = latex_escape(job$position),
      when  = dates,
      with  = latex_escape(job$company),
      where = latex_escape(job$location),
      why   = bullet_text
    )
  })
}

# ── Education ─────────────────────────────────────────────────
parse_education <- function() {
  edu <- keep(resume$education, ~ isTRUE(.x$include))
  map_dfr(edu, function(e) {
    tibble(
      what  = latex_escape(paste(e$degree, "in", e$field)),
      when  = "",
      with  = latex_escape(e$institution),
      where = latex_escape(e$location),
      why   = ""
    )
  })
}

# ── Publications ──────────────────────────────────────────────
# Returns one markdown bullet per publication, with the user's name bolded.
# Standard reference format: Authors (Year). Title. *Journal* Vol, pp.
parse_publications <- function() {
  pubs <- resume$publications$selected
  if (is.null(pubs) || length(pubs) == 0) return("")
  refs <- vapply(pubs, function(p) {
    authors <- gsub("Camacho DM", "**Camacho DM**", p$authors, fixed = TRUE)
    sprintf("- %s (%s). %s. *%s* %s, %s.",
            authors, p$year, p$title, p$journal, p$volume, p$pages)
  }, character(1))
  paste(refs, collapse = "\n")
}

parse_publications_footer <- function() resume$publications$footer

# ── Skills ────────────────────────────────────────────────────
parse_skills <- function() {
  paste(resume$skills$keywords, collapse = " | ")
}

# ── Patents & Grants (plain text) ─────────────────────────────
parse_patents <- function() resume$patents$description
parse_grants  <- function() {
  paste0("Raised ", resume$grants$total, " in competitive federal funding (",
         paste(resume$grants$sources, collapse = ", "), ") — ",
         resume$grants$description)
}
