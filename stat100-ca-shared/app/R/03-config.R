# ---------------------------------------------------------------------------
# 03-config.R — reading the course settings
#
# SOURCE OF TRUTH (admins edit these):     admin/config/
#     course.yml     course-wide settings (org, Canvas course id, windows)
#     staff.csv      everyone on the teaching team, their role and flags
#     sections.csv   section number, time, place, leader (CA username)
#     calendar.csv   every gradable item, due dates, grading due dates
#     coi.csv        conflicts of interest: student username, grader username
#     monitor-overrides.csv  (optional) hand-picked grading monitor for a week;
#                    otherwise picked at random (see "weekly grading monitor" below)
#
# CA-VISIBLE COPY (written by "Publish course info" on the Admin tab):
#     stat100-ca-shared/course-info/
#     same files minus anything private (no emails, no COI list).
# CAs' apps read the copy; admins' apps read the source.
# ---------------------------------------------------------------------------

read_csv_quiet <- function(path, ...) {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                  col_types = readr::cols(.default = readr::col_character()), ...)
}

as_flag <- function(x) if (!length(x)) logical() else toupper(trimws(x)) %in% c("TRUE", "T", "YES", "Y", "1")

config_path <- function(file) {
  if (has_admin_folder()) admin_path("config", file) else shared_path("course-info", file)
}

read_course <- function() {
  p <- config_path("course.yml")
  if (!file.exists(p)) return(list(course = "STAT 100", semester = "", github_org = ""))
  yaml::read_yaml(p)
}

clean_staff <- function(s) {
  if (is.null(s)) return(data.frame(username = character(), name = character(), role = character(),
                                    senior = logical(), monitor = logical(), section = character(),
                                    on_hold = logical()))
  s$username <- trimws(s$username)
  s$name <- trimws(s$name)
  s$role <- tolower(trimws(s$role))
  for (f in c("senior", "monitor", "on_hold")) s[[f]] <- as_flag(s[[f]])
  s$section <- toupper(trimws(s$section))
  s$section[s$section %in% c("", "NA")] <- NA
  s
}

# Staff list with this week's rotation applied:
#   monitor = TRUE if staff.csv says so (permanent monitor, e.g. a TF)
#             OR they are this week's monitor (monitor_for_week())
#   on_hold = TRUE if staff.csv says so OR they are this week's monitor
#   monitor_this_week = TRUE only for this week's rotation monitor
read_staff <- function(apply_rotation = TRUE) {
  s <- clean_staff(read_csv_quiet(config_path("staff.csv")))
  s$monitor_this_week <- FALSE
  if (!apply_rotation || !nrow(s)) return(s)
  m <- monitor_for_week(staff = s)
  if (!is.na(m)) {
    hit <- tolower(s$username) %in% tolower(m)
    s$monitor_this_week <- hit
    s$monitor <- s$monitor | hit
    s$on_hold <- s$on_hold | hit
  }
  s
}

# The public staff list (no emails). Rotation is NOT baked in: each app
# applies it when it reads the file, so it changes on its own every Monday.
read_staff_public <- function(apply_rotation = TRUE) {
  s <- read_staff(apply_rotation)
  # staff emails are shared with the team (for reminders); student emails never are
  s[, intersect(names(s), c("username", "name", "email", "role", "senior", "monitor", "monitor_this_week", "section", "on_hold"))]
}

# --- weekly grading monitor ------------------------------------------------
# Each week one senior CA (senior = TRUE in staff.csv) is the grading monitor
# and is on hold from grading. The pick is random but fair and reproducible:
#   * weeks are grouped in blocks, one week per senior CA (4 seniors -> 4 weeks)
#   * each block is a random shuffle of the seniors, using set.seed(block number)
#   * so everyone gets the same number of turns; if a shuffle would give the
#     same person two weeks in a row across blocks, it is rotated by one
# To change a week by hand, add a row to admin/config/monitor-overrides.csv:
#   week, monitor, note        e.g.  9, ysalerno, Virgil away for a conference
# Course weeks come from course.yml (week1_monday, weeks).

today_et <- function() as.Date(format(Sys.time(), tz = "America/New_York"))

course_weeks <- function() {
  co <- read_course()
  w1 <- as.Date(co$week1_monday %||% NA)
  n <- as.integer(co$weeks %||% 14)
  if (is.na(w1)) return(NULL)
  data.frame(week = seq_len(n), week_of = w1 + 7 * (seq_len(n) - 1))
}

current_week <- function(date = today_et()) {
  cw <- course_weeks()
  if (is.null(cw)) return(NA_integer_)
  k <- which(cw$week_of <= date & date < cw$week_of + 7)
  if (length(k)) cw$week[k] else NA_integer_
}

read_monitor_overrides <- function() {
  o <- read_csv_quiet(config_path("monitor-overrides.csv"))
  if (is.null(o) || !nrow(o)) return(data.frame(week = integer(), monitor = character(), note = character()))
  o$week <- as.integer(o$week); o$monitor <- trimws(o$monitor)
  o[!is.na(o$week) & !is.na(o$monitor) & o$monitor != "", ]
}

# Whole-semester schedule: week, week_of, monitor, source ("random" / "override")
monitor_schedule <- function(staff = clean_staff(read_csv_quiet(config_path("staff.csv")))) {
  cw <- course_weeks()
  if (is.null(cw)) return(NULL)
  seniors <- sort(staff$username[staff$senior & !is.na(staff$username) & staff$username != ""])
  cw$monitor <- NA_character_; cw$source <- "random"
  n <- length(seniors)
  if (n > 0) {
    old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
    on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv()))
    picks <- character(); last <- NA
    for (b in seq_len(ceiling(nrow(cw) / n))) {
      set.seed(b)
      perm <- sample(seniors)
      if (n > 1 && identical(perm[1], last)) perm <- c(perm[-1], perm[1])
      picks <- c(picks, perm); last <- perm[n]
    }
    cw$monitor <- picks[seq_len(nrow(cw))]
  }
  ov <- read_monitor_overrides()
  hit <- match(cw$week, ov$week)
  cw$monitor[!is.na(hit)] <- ov$monitor[hit[!is.na(hit)]]
  cw$source[!is.na(hit)] <- "override"
  cw$note <- ifelse(is.na(hit), "", ov$note[hit])
  cw
}

monitor_for_week <- function(date = today_et(), staff = clean_staff(read_csv_quiet(config_path("staff.csv")))) {
  ms <- monitor_schedule(staff)
  w <- current_week(date)
  if (is.null(ms) || is.na(w)) return(NA_character_)
  ms$monitor[ms$week == w]
}

read_sections <- function() {
  s <- read_csv_quiet(config_path("sections.csv"))
  if (is.null(s)) return(NULL)
  s$section <- toupper(trimws(s$section)); s$leader <- trimws(s$leader)
  s
}

read_calendar <- function() {
  cal <- read_csv_quiet(config_path("calendar.csv"))
  if (is.null(cal)) return(NULL)
  cal$week <- as.integer(cal$week)
  cal$student_due <- parse_when(cal$student_due)
  cal$grading_due <- parse_when(cal$grading_due)
  # Preparation (optional columns): who makes the assignment / rubric, by when.
  for (k in c("assignment_owner", "assignment_ready_by", "rubric_owner", "rubric_ready_by"))
    if (is.null(cal[[k]])) cal[[k]] <- NA_character_
  cal$assignment_ready_by <- parse_when(cal$assignment_ready_by)
  cal$rubric_ready_by <- parse_when(cal$rubric_ready_by)
  cal
}

# Accepts "2026-09-15 23:59", "2026-09-15" (= end of that day), and what
# spreadsheet apps write: "9/15/26 23:59", "9/15/2026 11:59 PM", "9/15/26".
# All times are Eastern.
parse_when <- function(x) {
  if (!length(x)) return(as.POSIXct(character(), tz = "America/New_York"))
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  tz <- "America/New_York"
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = tz)
  # Order matters: AM/PM before 24-hour (else 11:59 PM reads as AM), and
  # 2-digit year before 4-digit (else "26" becomes the year 26).
  fmts <- c("%m/%d/%y %I:%M %p", "%m/%d/%Y %I:%M %p",
            "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%m/%d/%y %H:%M", "%m/%d/%Y %H:%M")
  for (f in fmts) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    out[todo] <- as.POSIXct(x[todo], format = f, tz = tz)
  }
  # date only = 11:59 pm that day
  pats <- c("^\\d{4}-\\d{1,2}-\\d{1,2}$" = "%Y-%m-%d", "^\\d{1,2}/\\d{1,2}/\\d{2}$" = "%m/%d/%y",
            "^\\d{1,2}/\\d{1,2}/\\d{4}$" = "%m/%d/%Y")
  for (k in names(pats)) {
    todo <- is.na(out) & !is.na(x) & grepl(k, x)
    if (!any(todo)) next
    d <- as.Date(x[todo], format = pats[[k]])
    out[todo] <- as.POSIXct(paste(format(d), "23:59"), format = "%Y-%m-%d %H:%M", tz = tz)
  }
  out
}

# Admin only — never copied to the shared folder.
read_coi <- function() {
  if (!has_admin_folder()) return(NULL)
  coi <- read_csv_quiet(admin_path("config", "coi.csv"))
  if (is.null(coi)) return(data.frame(student = character(), grader = character()))
  names(coi) <- trimws(sub("^﻿", "", names(coi)))
  coi$student <- trimws(coi$student); coi$grader <- trimws(coi$grader)
  coi
}

# Copy the non-private config to the shared folder so CAs' apps can read it.
publish_course_info <- function(user = current_user()) {
  require_admin(user, "publish course info")
  out <- shared_path("course-info"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
  file.copy(admin_path("config", "course.yml"), file.path(out, "course.yml"), overwrite = TRUE)
  file.copy(admin_path("config", "calendar.csv"), file.path(out, "calendar.csv"), overwrite = TRUE)
  file.copy(admin_path("config", "sections.csv"), file.path(out, "sections.csv"), overwrite = TRUE)
  ovf <- admin_path("config", "monitor-overrides.csv")
  if (file.exists(ovf)) file.copy(ovf, file.path(out, "monitor-overrides.csv"), overwrite = TRUE)
  else unlink(file.path(out, "monitor-overrides.csv"))
  pub <- read_staff_public(apply_rotation = FALSE); pub$monitor_this_week <- NULL
  readr::write_csv(pub, file.path(out, "staff.csv"), na = "")
  writeLines(c("These files are written by the dashboard (Admin tab > Publish course info).",
               "Do not edit them here — edit admin/config/ and publish again."),
             file.path(out, "README.txt"))
  log_action(user$username, "publish course info", out)
  invisible(out)
}

# Append one line to the action log (admins: admin/logs; CAs: their own folder).
log_action <- function(user, action, detail = "") {
  dir <- if (has_admin_folder()) admin_path("logs") else grader_dir(user %||% "")
  if (is.na(dir) || !dir.exists(dirname(dir))) return(invisible(FALSE))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(dir, "action-log.csv")
  row <- data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), user = user %||% "",
                    action = action, detail = paste(detail, collapse = "; "))
  readr::write_csv(row, f, append = file.exists(f))
  invisible(TRUE)
}

# --- Canvas token ------------------------------------------------------------
# The token stays wherever you already keep it for R: ~/.Renviron (read
# automatically) or set in the session. Any of these names works:
#   CANVAS_API_TOKEN, CANVAS_API_KEY (what vvcanvas uses), CANVAS_TOKEN
canvas_token <- function() {
  if (file.exists("~/.Renviron")) try(readRenviron("~/.Renviron"), silent = TRUE)
  for (k in c("CANVAS_API_TOKEN", "CANVAS_API_KEY", "CANVAS_TOKEN")) {
    v <- Sys.getenv(k); if (nzchar(v)) return(v)
  }
  ""
}
