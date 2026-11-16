# ---------------------------------------------------------------------------
# 18-sections.R — section activities: no assigning, CA decides about late work
#
# WHO GRADES WHOM: every week the CA who teaches a section grades that
#   section's students. Nobody has to assign anything. The admin's dashboard
#   writes each CA's roster into their grader folder:
#     graders/stat100-grader-<user>/my-students.csv   github, name, email, section
#     graders/stat100-grader-<user>/my-exceptions.csv extensions / excused for
#                                                     section items (no notes)
#   (written whenever an admin opens the dashboard, refreshes the student list,
#   publishes course info, or adds/removes an extension or excuse).
#   A student with a conflict of interest with their section leader goes to
#   another CA (same CA every week: seed 1).
#
# WHEN: a section's deadline is the END of that section's meeting in the
#   item's week, from admin/config/sections.csv (day + time, e.g. "Friday",
#   "10:30 - 11:45 am"  ->  Friday 11:45 am of week 11 for sec11).
#   If sections.csv has no day/time for a section, the calendar's student_due
#   is used. An extension (Late & excused tab) replaces the deadline.
#
# THE CA'S LIST is built on the CA's own computer when they open the Grading
#   tab (and again with "Check for new commits"), from the GitHub commits that
#   touched section/:
#     grading/<item>/assignment.csv  github, repo, sha (= NEWEST version),
#       commit_time, deadline, deadline_sha (version at the end of section),
#       deadline_commit_time, late_commits (commits after the deadline),
#       wait_until (deadline still ahead: extension), status, built_at
#     status: "on time", "after section" (committed after the deadline — the
#       CA looks and decides), "no submission", "waiting" (deadline ahead).
#   Excused students aren't on the list at all.
#
# LATE IS THE CA'S CALL: a CA may cover only some questions in section, so a
#   commit after section isn't necessarily late. The CA grades the version at
#   the end of section by default, can switch to the newest version, and
#   marks the student "Late — warned" or "Not late".
#   (The Grading tab opens the version at the end of section by default;
#   the CA can switch to the newest version.) Marks are saved in
#   grading/<item>/late-marks.csv (github, decision, note, marked_by, marked_at).
#   "Late — warned" shows an email (templates/section-late.txt) and counts
#   toward the student's running total of late sections this semester.
# ---------------------------------------------------------------------------

# --- when does a section end? ------------------------------------------------

# "10:30 - 11:45 am" -> "11:45 am";  "12:00 - 1:15 pm" -> "1:15 pm"
section_end_clock <- function(time) {
  x <- trimws(sub("^.*[-–]\\s*", "", time %||% ""))
  if (!grepl("(am|pm)$", tolower(x)) && grepl("(am|pm)$", tolower(time %||% ""))) x <- paste(x, sub("^.*(am|pm)$", "\\1", tolower(time)))
  t <- as.POSIXct(paste("2000-01-01", toupper(x)), format = "%Y-%m-%d %I:%M %p", tz = "America/New_York")
  if (is.na(t)) t <- as.POSIXct(paste("2000-01-01", x), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  if (is.na(t)) NA_character_ else format(t, "%H:%M")
}

# End of a section's meeting in course week `week` (POSIXct, or NA).
section_end <- function(section, week, sec = read_sections()) {
  vapply(seq_along(section), function(i) {
    s <- sec[toupper(sec$section) == toupper(section[i] %||% ""), ]
    if (!nrow(s) || is.na(week)) return(NA_real_)
    off <- match(tolower(trimws(s$day[1] %||% "")), c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")) - 1
    clock <- section_end_clock(s$time[1])
    if (is.na(off) || is.na(clock)) return(NA_real_)
    monday <- as.Date(read_course()$week1_monday) + (week - 1) * 7
    as.numeric(as.POSIXct(paste(monday + off, clock), format = "%Y-%m-%d %H:%M", tz = "America/New_York"))
  }, 0) |> as.POSIXct(origin = "1970-01-01", tz = "America/New_York")
}

# --- admin side: rosters into the grader folders ------------------------------

# Who grades each student's section work (section leader; COI -> another CA).
section_graders <- function(st = read_students()) {
  st <- st[!is.na(st$github) & nzchar(st$github), ]
  sec <- read_sections(); coi <- read_coi(); staff <- read_staff()
  st$grader <- sec$leader[match(toupper(st$section), toupper(sec$section))]
  clash <- is.na(st$grader) | paste(tolower(st$github), tolower(st$grader)) %in% paste(tolower(coi$student), tolower(coi$grader))
  if (any(clash)) {
    redo <- deal_students(st$github[clash], active_graders(staff), coi, seed = 1)
    st$grader[match(redo$github, st$github)] <- redo$grader
  }
  st
}

publish_section_rosters <- function(user = current_user()) {
  if (!is_admin(user) || !has_admin_folder()) return(invisible(FALSE))
  st <- read_students(); if (is.null(st)) return(invisible(FALSE))
  st <- section_graders(st)
  exc <- read_exceptions(); exc <- exc[grepl("^sec", exc$item), ]
  for (g in unique(stats::na.omit(st$grader))) {
    d <- file.path(course_root(), "graders", grader_folder_name(g))
    if (!dir.exists(d)) next
    mine <- st[st$grader %in% g, c("github", "name", "email", "section")]
    readr::write_csv(mine, file.path(d, "my-students.csv"), na = "")
    x <- exc[tolower(exc$github) %in% tolower(mine$github), c("item", "github", "type", "new_due")]
    x$new_due <- ifelse(is.na(x$new_due), "", format(x$new_due, "%Y-%m-%d %H:%M", tz = "America/New_York"))
    readr::write_csv(x, file.path(d, "my-exceptions.csv"), na = "")
  }
  invisible(TRUE)
}

# --- CA side: build the list ---------------------------------------------------

my_section_students <- function(grader) {
  d <- grader_dir(grader); if (is.na(d)) return(NULL)
  read_csv_quiet(file.path(d, "my-students.csv"))
}
my_section_exceptions <- function(grader, item) {
  d <- grader_dir(grader); if (is.na(d)) return(NULL)
  x <- read_csv_quiet(file.path(d, "my-exceptions.csv"))
  if (is.null(x) || !nrow(x)) return(NULL)
  x$new_due <- parse_when(x$new_due)
  x[x$item == item, ]
}

# Section items this grader can open now: in the dashboard's range, and the
# earliest deadline among their students has passed.
section_items_for <- function(grader) {
  ros <- my_section_students(grader); cal <- read_calendar()
  if (is.null(ros) || !nrow(ros) || is.null(cal)) return(character())
  s <- cal[cal$type %in% "section" & in_dashboard_scope(cal$item), ]
  ok <- vapply(seq_len(nrow(s)), function(i) {
    e <- section_end(unique(ros$section), s$week[i]); e <- e[!is.na(e)]
    first <- if (length(e)) min(e) else s$student_due[i]
    !is.na(first) && first < now_et()
  }, TRUE)
  s$item[ok]
}

# Read commits and write grading/<item>/assignment.csv for one CA.
build_section_list <- function(grader, item, source = repo_source(), progress = NULL) {
  it <- item_info(item)
  ros <- my_section_students(grader)
  if (is.null(ros) || !nrow(ros)) stop("No section roster in your grading folder yet (my-students.csv). Ask the admin team to open the dashboard once.")
  exc <- my_section_exceptions(grader, item)
  staff <- tolower(staff_logins())
  ends <- section_end(ros$section, it$week)
  rows <- lapply(seq_len(nrow(ros)), function(i) {
    gh <- ros$github[i]
    if (!is.null(progress)) progress(i / nrow(ros), gh)
    x <- if (is.null(exc)) NULL else exc[tolower(exc$github) == tolower(gh), ]
    if (!is.null(x) && nrow(x) && x$type[1] == "excused") return(NULL)            # excused: not graded
    dl <- if (!is.na(ends[i])) ends[i] else it$student_due
    if (!is.null(x) && nrow(x) && x$type[1] == "extension" && !is.na(x$new_due[1])) dl <- x$new_due[1]
    repo <- student_repo(it$repo, gh)
    row <- data.frame(item = item, github = gh, repo = repo, sha = NA_character_, commit_time = "",
                      deadline = format(dl, "%Y-%m-%d %H:%M", tz = "America/New_York"),
                      deadline_sha = NA_character_, deadline_commit_time = "", late_commits = 0L,
                      wait_until = "", status = "", stringsAsFactors = FALSE)
    if (!is.na(dl) && dl > now_et()) { row$wait_until <- row$deadline; row$status <- "waiting"; return(row) }
    cm <- tryCatch(repo_commits(repo, source, path = it$folder), error = function(e) NULL)
    if (!is.null(cm)) cm <- cm[!tolower(cm$author) %in% staff & !grepl("\\[bot\\]$", cm$author), ]
    if (is.null(cm) || !nrow(cm)) { row$status <- "no submission"; return(row) }
    attr(cm$time, "tzone") <- "America/New_York"
    new <- cm[which.max(cm$time), ]
    row$sha <- new$sha; row$commit_time <- format(new$time, "%Y-%m-%d %H:%M", tz = "America/New_York")
    if (is.na(dl)) dl <- max(cm$time)                  # no deadline known: nothing counts as after
    before <- cm[cm$time <= dl, ]
    if (nrow(before)) { b <- before[which.max(before$time), ]; row$deadline_sha <- b$sha
      row$deadline_commit_time <- format(b$time, "%Y-%m-%d %H:%M", tz = "America/New_York") }
    row$late_commits <- sum(cm$time > dl)
    row$status <- if (row$late_commits > 0) "after section" else "on time"
    row
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) out <- data.frame(item = character(), github = character(), repo = character(), sha = character())
  out$built_at <- format(Sys.time(), "%Y-%m-%d %H:%M")
  d <- file.path(grader_dir(grader), "grading", item); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  tmp <- file.path(d, "assignment.csv.tmp"); readr::write_csv(out, tmp, na = ""); file.rename(tmp, file.path(d, "assignment.csv"))
  write_progress(grader)
  invisible(out)
}

# Rebuilt if it's older than `minutes` (new commits, extensions ending).
section_list_stale <- function(grader, item, minutes = 15) {
  a <- read_csv_quiet(file.path(grader_dir(grader), "grading", item, "assignment.csv"))
  if (is.null(a) || !nrow(a) || !"built_at" %in% names(a)) return(TRUE)
  t <- parse_when(a$built_at[1]); is.na(t) || difftime(now_et(), t, units = "mins") > minutes
}

# --- late marks ------------------------------------------------------------------

late_marks_path <- function(grader, item) file.path(grader_dir(grader), "grading", item, "late-marks.csv")
read_late_marks <- function(grader, item) {
  m <- read_csv_quiet(late_marks_path(grader, item))
  if (is.null(m)) m <- data.frame(github = character(), decision = character(), note = character(), marked_by = character(), marked_at = character())
  m
}
save_late_mark <- function(grader, item, github, decision, note = "", by) {
  m <- read_late_marks(grader, item); m <- m[m$github != github, ]
  if (nzchar(decision)) m <- rbind(m, data.frame(github = github, decision = decision, note = note %||% "", marked_by = by,
                                                 marked_at = format(Sys.time(), "%Y-%m-%d %H:%M")))
  readr::write_csv(m, late_marks_path(grader, item), na = "")
  invisible(m)
}

# "Late — warned" marks per student, across all section items.
#   graders = one CA's folder (what a CA sees) or everyone (admins).
section_late_counts <- function(graders) {
  rows <- list()
  for (g in graders) {
    d <- grader_dir(g); if (is.na(d)) next
    for (it in list.dirs(file.path(d, "grading"), full.names = FALSE, recursive = FALSE)) {
      m <- read_csv_quiet(file.path(d, "grading", it, "late-marks.csv"))
      if (!is.null(m) && nrow(m)) rows[[length(rows) + 1]] <- data.frame(item = it, grader = g, m[, c("github", "decision", "marked_at")])
    }
  }
  if (!length(rows)) return(data.frame(item = character(), grader = character(), github = character(), decision = character(), marked_at = character()))
  do.call(rbind, rows)
}

# --- admin side: all CAs' lists for one section item ------------------------------

# One row per student (like a homework snapshot): github, section, grader,
# repo, sha, status. status uses the homework words where it can:
#   on time / after section  -> graded;   waiting -> "extension (waiting)";
#   excused -> "excused (no grade)";  CA hasn't opened it -> "not loaded yet".
section_snapshot <- function(item) {
  st <- read_students(); if (is.null(st)) return(NULL)
  st <- section_graders(st)
  lists <- do.call(rbind, lapply(unique(stats::na.omit(st$grader)), function(g) {
    a <- read_csv_quiet(file.path(course_root(), "graders", grader_folder_name(g), "grading", item, "assignment.csv"))
    if (is.null(a) || !nrow(a)) return(NULL)
    data.frame(github = a$github, sha = a$sha, grader = g,
               status = if ("status" %in% names(a)) a$status else rep("not loaded yet", nrow(a)))   # list from before sections built themselves
  }))
  exc <- read_exceptions(item)
  m <- if (is.null(lists)) rep(NA, nrow(st)) else match(tolower(st$github), tolower(lists$github))
  out <- data.frame(github = st$github, section = st$section, grader = st$grader,
                    repo = student_repo(item_info(item)$repo, st$github),
                    sha = if (is.null(lists)) NA else lists$sha[m],
                    status = if (is.null(lists)) "not loaded yet" else lists$status[m], stringsAsFactors = FALSE)
  out$status[is.na(out$status)] <- "not loaded yet"
  out$status[out$status == "waiting"] <- "extension (waiting)"
  out$status[tolower(out$github) %in% tolower(exc$github[exc$type == "excused"])] <- "excused (no grade)"
  out
}

section_assignments <- function(item) {
  sn <- section_snapshot(item); if (is.null(sn)) return(NULL)
  sn[!sn$status %in% c("not loaded yet", "excused (no grade)", "no submission"), c("github", "grader", "section", "sha", "status")]
}

# The right helpers for any item (homework: snapshot + assignment files).
item_snapshot <- function(item) if (grepl("^sec", item)) section_snapshot(item) else read_snapshot(item)
item_assignments <- function(item) if (grepl("^sec", item)) section_assignments(item) else read_assignments(item)
