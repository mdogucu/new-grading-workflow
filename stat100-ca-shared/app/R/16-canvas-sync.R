# ---------------------------------------------------------------------------
# 16-canvas-sync.R — fill calendar.csv from the assignments already in Canvas
#
# Canvas knows every assignment's name, due date and id. "Check Canvas"
# (Admin tab) reads them and proposes changes to admin/config/calendar.csv;
# nothing is written until you click "Apply".
#
# Which Canvas assignments are used: names starting with hwNN, secNN or
# quizNNx, e.g. "hw09-statistical-inference", "sec09-...", "quiz11b".
#   item         from the name (hw09, sec09, quiz11b)
#   type, week   from the name (homework / section / quiz, 9)
#   title        from the name ("statistical-inference" -> "Statistical inference")
#   repo         homework/section: week09-statistical-inference
#   student_due  Canvas due date (Eastern)
#   canvas_id    Canvas assignment id
#   grading_due  only if empty: due date + course.yml grading_days_* (below)
# Items before course.yml first_week_homework / _section / _quiz are skipped
# (graded the old way).
# Everything else you typed (owners, ready-by dates, grading_due you set)
# is left alone. Canvas wins for due dates and ids — change those in Canvas.
# A copy of the old calendar.csv is kept in admin/config/old/.
# ---------------------------------------------------------------------------

canvas_get_all <- function(path, query = list()) {
  token <- canvas_token(); if (!nzchar(token)) stop("No Canvas token on this computer (docs/setup-canvas-token.md).")
  co <- read_course(); if (!nzchar(co$canvas_course_id %||% "")) stop("Add canvas_course_id to course.yml.")
  url <- paste0(co$canvas_url, "/api/v1/courses/", co$canvas_course_id, path)
  out <- list()
  repeat {
    resp <- httr2::request(url) |> httr2::req_auth_bearer_token(token) |>
      httr2::req_url_query(!!!c(query, list(per_page = 100))) |> httr2::req_perform()
    out <- c(out, httr2::resp_body_json(resp))
    nxt <- regmatches(httr2::resp_header(resp, "link") %||% "", regexpr('<[^>]+>; rel="next"', httr2::resp_header(resp, "link") %||% ""))
    if (!length(nxt)) break
    url <- sub('^<([^>]+)>.*$', "\\1", nxt); query <- list()
  }
  out
}

canvas_assignments <- function() {
  a <- canvas_get_all("/assignments")
  data.frame(canvas_id = vapply(a, function(x) as.character(x$id), ""),
             name = vapply(a, function(x) x$name %||% "", ""),
             due_at = vapply(a, function(x) x$due_at %||% NA_character_, ""),
             published = vapply(a, function(x) isTRUE(x$published), TRUE), stringsAsFactors = FALSE)
}

# Turn Canvas assignments into calendar rows (only hw / sec / quiz names).
canvas_to_calendar <- function(ca) {
  m <- regmatches(ca$name, regexec("^\\s*(hw|sec|quiz)\\s*0*(\\d+)([a-z]?)[-_ :]*(.*)$", ca$name, ignore.case = TRUE))
  ok <- lengths(m) > 0
  ca <- ca[ok, ]; m <- m[ok]
  if (!nrow(ca)) return(NULL)
  kind <- tolower(vapply(m, `[`, "", 2)); wk <- as.integer(vapply(m, `[`, "", 3))
  suf <- tolower(vapply(m, `[`, "", 4)); slug <- trimws(vapply(m, `[`, "", 5))
  item <- paste0(kind, sprintf("%02d", wk), suf)
  pretty <- function(x) { x <- gsub("[-_]+", " ", x); if (nzchar(x)) paste0(toupper(substr(x, 1, 1)), substring(x, 2)) else "" }
  due <- as.POSIXct(ca$due_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  data.frame(item = item,
             type = c(hw = "homework", sec = "section", quiz = "quiz")[kind],
             week = wk,
             title = vapply(slug, pretty, ""),
             repo = ifelse(kind %in% c("hw", "sec") & nzchar(slug), sprintf("week%02d-%s", wk, tolower(gsub("[ _]+", "-", slug))), ""),
             student_due = ifelse(is.na(due), "", format(due, "%Y-%m-%d %H:%M", tz = "America/New_York")),
             canvas_id = ca$canvas_id, canvas_name = ca$name, stringsAsFactors = FALSE, row.names = NULL)
}

# Compare with calendar.csv. Returns the new table and a list of changes.
calendar_sync_plan <- function(ca = canvas_assignments()) {
  cv <- canvas_to_calendar(ca)
  if (is.null(cv)) stop("No Canvas assignments named like hw09, sec09, quiz11b were found.")
  skipped <- cv$canvas_name[!in_dashboard_scope(cv$item)]       # before course.yml first_week_*
  cv <- cv[in_dashboard_scope(cv$item), ]
  cal <- read_csv_quiet(admin_path("config", "calendar.csv"))
  cols <- unique(c(names(cal), "item", "type", "week", "title", "repo", "student_due", "grading_due", "canvas_id"))
  for (k in setdiff(cols, names(cal))) cal[[k]] <- if (nrow(cal)) NA_character_ else character()
  co <- read_course()
  gdays <- c(homework = as.numeric(co$grading_days_homework %||% 3), section = as.numeric(co$grading_days_section %||% 3),
             quiz = as.numeric(co$grading_days_quiz %||% 3))
  changes <- list()
  note <- function(item, what, old, new) changes[[length(changes) + 1]] <<-
    data.frame(item = item, change = what, before = old %||% "", after = new %||% "")
  for (i in seq_len(nrow(cv))) {
    r <- cv[i, ]; k <- which(cal$item == r$item); is_new <- FALSE
    if (!length(k)) {
      is_new <- TRUE
      cal[nrow(cal) + 1, ] <- NA; k <- nrow(cal)
      cal$item[k] <- r$item; cal$type[k] <- r$type; cal$week[k] <- as.character(r$week)
      cal$title[k] <- r$title; cal$repo[k] <- r$repo
      if ("assignment_owner" %in% names(cal)) cal$assignment_owner[k] <- "mdogucu"
      if ("rubric_owner" %in% names(cal) && r$type == "homework") cal$rubric_owner[k] <- "JulieVu"
      note(r$item, "new item", "", r$canvas_name)
    }
    set <- function(col, val, label) {
      old <- cal[[col]][k]; if (is.na(old)) old <- ""
      same <- if (col == "student_due") isTRUE(parse_when(old) == parse_when(val)) else identical(old, val)
      if (nzchar(val) && !same) { cal[[col]][k] <<- val; if (!is_new) note(r$item, label, old, val) }
    }
    set("canvas_id", r$canvas_id, "Canvas id")
    set("student_due", r$student_due, "due date")
    if (is.na(cal$title[k]) || !nzchar(cal$title[k])) cal$title[k] <- r$title
    if ((is.na(cal$repo[k]) || !nzchar(cal$repo[k])) && nzchar(r$repo)) cal$repo[k] <- r$repo
    if ((is.na(cal$grading_due[k]) || !nzchar(cal$grading_due[k])) && nzchar(r$student_due)) {
      g <- format(parse_when(r$student_due) + gdays[[r$type]] * 86400, "%Y-%m-%d 23:59")
      cal$grading_due[k] <- g; if (!is_new) note(r$item, "grading due (default)", "", g)
    }
  }
  ord <- order(suppressWarnings(as.integer(cal$week)), match(cal$type, c("homework", "section", "quiz")), cal$item)
  list(calendar = cal[ord, cols], changes = if (length(changes)) do.call(rbind, changes) else NULL,
       unused = setdiff(ca$name[!ca$canvas_id %in% cv$canvas_id], skipped), skipped = skipped)
}

apply_calendar_sync <- function(plan, user = current_user()) {
  require_admin(user, "update calendar.csv from Canvas")
  f <- admin_path("config", "calendar.csv")
  old <- admin_path("config", "old"); dir.create(old, showWarnings = FALSE)
  file.copy(f, file.path(old, paste0("calendar-", format(Sys.time(), "%Y%m%d-%H%M"), ".csv")))
  readr::write_csv(plan$calendar, f, na = "")
  log_action(acting_username(user), "calendar synced from Canvas", sprintf("%d change(s)", NROW(plan$changes)))
  invisible(TRUE)
}
