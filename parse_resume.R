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

# ── Work experience: raw LaTeX emitter ────────────────────────
# emit_work_latex(profile) returns a complete LaTeX block for the
# Professional Experience section, including the cventries wrapper.
# Used via a results='asis' chunk because vitae::detailed_entries cannot
# express nested (parent → embedded) entries.

format_dates <- function(job) {
  start <- format(as.Date(paste0(job$startDate, "-01")), "%b %Y")
  end   <- if (is.null(job$endDate)) "Present" else
             format(as.Date(paste0(job$endDate, "-01")), "%b %Y")
  paste(start, "-", end)
}

format_bullet_tex <- function(h) {
  if (!is.null(h$bold) && nchar(h$bold) > 0)
    sprintf("\\item \\textbf{%s}: %s", latex_escape(h$bold), latex_escape(h$text))
  else
    sprintf("\\item %s", latex_escape(h$text))
}

format_cvitems <- function(highlights) {
  active <- keep(highlights, ~ isTRUE(.x$include))
  if (length(active) == 0) return("")
  paste0(
    "\\begin{cvitems}\n",
    paste(map_chr(active, format_bullet_tex), collapse = "\n"), "\n",
    "\\end{cvitems}"
  )
}

format_cventry <- function(job, is_collapsed) {
  description <- if (is_collapsed) "" else format_cvitems(job$highlights)
  sprintf("\\cventry{%s}{%s}{%s}{%s}{%s}",
          latex_escape(job$position),
          latex_escape(job$company),
          latex_escape(job$location),
          format_dates(job),
          description)
}

# Embedded entries render as a regular \cventry with the descriptor
# inlined on the company line, e.g. "Abiologics (Flagship Pioneering portfolio company)".
# (\cvsubentry breaks when its description contains an itemize/cvitems block.)
format_subentry <- function(sub) {
  company <- latex_escape(sub$company)
  if (!is.null(sub$descriptor) && nzchar(sub$descriptor)) {
    company <- sprintf("%s \\textit{(%s)}",
                       company, latex_escape(sub$descriptor))
  }
  sprintf("\\cventry{%s}{%s}{%s}{%s}{%s}",
          latex_escape(sub$position),
          company,
          latex_escape(sub$location),
          format_dates(sub),
          format_cvitems(sub$highlights))
}

emit_work_latex <- function(profile = NULL) {
  work <- resume$work

  if (!is.null(profile) && !is.null(resume$profiles[[profile]])) {
    allowed_ids <- resume$profiles[[profile]]$work_filter
    work <- keep(work, ~ .x$id %in% allowed_ids)
  }
  work <- keep(work, ~ isTRUE(.x$include))

  collapsed_ids <- character(0)
  if (!is.null(profile) && !is.null(resume$profiles[[profile]]$earlier_roles_collapsed)) {
    collapsed_ids <- resume$profiles[[profile]]$earlier_roles_collapsed
  }

  parts <- c("\\begin{cventries}")
  for (job in work) {
    parts <- c(parts, format_cventry(job, job$id %in% collapsed_ids))
    if (!is.null(job$embedded) && length(job$embedded) > 0) {
      for (sub in job$embedded) {
        if (isTRUE(sub$include)) {
          parts <- c(parts, format_subentry(sub))
        }
      }
    }
  }
  parts <- c(parts, "\\end{cventries}")
  paste(parts, collapse = "\n")
}

# ── Work experience: legacy tibble (kept for any other consumers) ──
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

    # job$context is intentionally unused. Field is preserved in the JSON
    # in case we want to render it differently in a future template.
    position_text <- latex_escape(job$position)

    # Collapsed (earlier roles) — one-line bullet per role
    if (job$id %in% collapsed_ids) {
      active_bullets <- keep(job$highlights, ~ isTRUE(.x$include))
      bullet_text <- if (length(active_bullets) == 0) NA_character_ else map_chr(active_bullets, format_bullet)
      return(tibble(
        what  = position_text,
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
        what  = position_text,
        when  = dates,
        with  = latex_escape(job$company),
        where = latex_escape(job$location),
        why   = NA_character_
      ))
    }

    bullet_text <- map_chr(active_bullets, format_bullet)

    tibble(
      what  = position_text,
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
