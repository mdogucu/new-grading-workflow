# ---------------------------------------------------------------------------
# 05-tasks.R — the "My tasks" list
#
# Each check_*() function looks at the course files and returns zero or more
# tasks. A task is one row:
#   who      : "admin", "ca", "senior", "monitor", or a username
#   title    : short sentence shown in bold
#   detail   : one line of explanation
#   due      : date-time or NA
#   tab      : which tab to open to do it ("" if none)
#   level    : "overdue", "todo", "info", "ok"
#
#   id       : what "Mark as done" remembers (default: the title). Most tasks
#              also clear themselves when the work is done (a quiz posted, a
#              rubric saved, grades entered). "Mark as done" is for work done
#              outside the dashboard; the task stays hidden unless its id
#              changes (e.g. the count in the title, or a new Canvas export).
#
# To add a new kind of task: write a check_something() function that returns
# task(...) rows and add it to the list in tasks_for().
# Marked-as-done tasks: admin/data/dismissed-tasks.csv (admins) or
# <your grader folder>/dismissed-tasks.csv (CAs), with who and when.
# Delete a row to bring the task back.
# ---------------------------------------------------------------------------

task <- function(title, detail = "", due = NA, tab = "", level = "todo", who = "", id = NA_character_) {
  data.frame(who = who, title = title, detail = detail,
             due = as.POSIXct(due, tz = "America/New_York"), tab = tab, level = level,
             id = id, stringsAsFactors = FALSE)
}

dismissed_path <- function(user) {
  if (is_admin(user)) admin_path("data", "dismissed-tasks.csv")
  else { d <- grader_dir(user$username %||% ""); if (is.na(d)) NA_character_ else file.path(d, "dismissed-tasks.csv") }
}
read_dismissed <- function(user) {
  f <- dismissed_path(user)
  d <- if (!is.na(f)) read_csv_quiet(f) else NULL
  if (is.null(d)) return(character())
  d$id[tolower(d$username) == tolower(user$username %||% "")]
}
dismiss_task <- function(user, id) {
  f <- dismissed_path(user); if (is.na(f)) return(invisible(FALSE))
  row <- data.frame(username = user$username, id = id, dismissed_at = format(Sys.time(), "%Y-%m-%d %H:%M"))
  readr::write_csv(row, f, append = file.exists(f))
  invisible(TRUE)
}
no_tasks <- function() task("")[0, ]

now_et <- function() as.POSIXct(format(Sys.time(), tz = "America/New_York"), tz = "America/New_York")

# --- admin checks ----------------------------------------------------------

check_setup_files <- function() {
  out <- list()
  need <- c("course.yml", "staff.csv", "sections.csv", "calendar.csv", "coi.csv")
  miss <- need[!file.exists(admin_path("config", need))]
  if (length(miss)) out[[1]] <- task(sprintf("Missing config file(s): %s", paste(miss, collapse = ", ")),
                                     "Put them in admin/config/. See admin/README.md.", tab = "admin", level = "overdue")
  course <- read_course()
  if (!nzchar(course$canvas_course_id %||% ""))
    out[[length(out) + 1]] <- task("Add the Canvas course ID to course.yml",
                                   "Needed before grades can be posted.", tab = "admin")
  dplyr::bind_rows(out)
}

check_staff <- function() {
  s <- read_staff(); out <- list()
  if (any(is.na(s$username) | s$username == ""))
    out[[1]] <- task(sprintf("%d staff member(s) without a GitHub username", sum(is.na(s$username) | s$username == "")),
                     paste("Fill in admin/config/staff.csv:", paste(s$name[is.na(s$username) | s$username == ""], collapse = ", ")),
                     tab = "staff")
  sec <- read_sections()
  if (!is.null(sec)) {
    bad <- sec$section[!sec$leader %in% s$username]
    if (length(bad)) out[[length(out) + 1]] <- task(
      sprintf("Section leader not found in staff list: %s", paste(bad, collapse = ", ")),
      "The leader column in sections.csv must be a username from staff.csv.", tab = "staff")
    cas <- s[s$role == "ca" & !is.na(s$section), ]
    wrong <- cas$name[!cas$section %in% sec$section]
    if (length(wrong)) out[[length(out) + 1]] <- task(
      sprintf("Staff with a section that doesn't exist: %s", paste(wrong, collapse = ", ")),
      "Check the section column in staff.csv.", tab = "staff")
  }
  ms <- monitor_schedule()
  if (is.null(ms)) {
    out[[length(out) + 1]] <- task("Add week1_monday to course.yml",
      "Needed to know the course week and pick the grading monitor.", tab = "staff")
  } else {
    if (!any(s$senior)) out[[length(out) + 1]] <- task("No senior CAs",
      "Set senior = TRUE for the senior CAs in staff.csv.", tab = "staff")
    bad <- setdiff(read_monitor_overrides()$monitor, s$username[s$senior])
    if (length(bad)) out[[length(out) + 1]] <- task(
      sprintf("monitor-overrides.csv lists someone who isn't a senior CA: %s", paste(bad, collapse = ", ")),
      "Fix the file, or set senior = TRUE in staff.csv.", tab = "staff")
    nw <- current_week(today_et() + 7)
    if (!is.na(nw)) {
      m <- ms$monitor[ms$week == nw]
      nm <- s$name[match(tolower(m), tolower(s$username))]
      out[[length(out) + 1]] <- task(sprintf("Next week's grading monitor: %s", nm %||% m),
        sprintf("Week %d starts %s. They'll be on hold from grading.", nw, format(ms$week_of[ms$week == nw], "%b %d")),
        tab = "staff", level = "info")
    }
  }
  dplyr::bind_rows(out)
}

check_coi <- function() {
  coi <- read_coi(); st <- read_students(); s <- read_staff()
  if (is.null(coi) || !nrow(coi)) return(no_tasks())
  out <- list()
  bad_g <- setdiff(coi$grader, s$username)
  if (length(bad_g)) out[[1]] <- task(sprintf("COI list names unknown grader(s): %s", paste(bad_g, collapse = ", ")),
                                      "Fix admin/config/coi.csv — grader must be a staff username.", tab = "staff")
  if (!is.null(st)) {
    bad_s <- setdiff(tolower(coi$student), tolower(st$github))
    if (length(bad_s)) out[[length(out) + 1]] <- task(
      sprintf("COI list names %d student username(s) not on the roster", length(bad_s)),
      paste(bad_s, collapse = ", "), tab = "staff")
  }
  dplyr::bind_rows(out)
}

check_roster <- function() {
  st <- read_students()
  if (is.null(st)) return(task("Build the student list", "Admin tab > Refresh student list.", tab = "admin"))
  out <- list()
  f <- admin_path("data", "students.csv")
  canvas <- newest_file(admin_path("inputs", "canvas-gradebook"))
  canvas_age <- if (is.na(canvas)) Inf else as.numeric(difftime(Sys.time(), file.mtime(canvas), units = "days"))
  if (!is.na(canvas) && file.mtime(canvas) > file.mtime(f))
    out[[1]] <- task("New downloads since the student list was built",
                     "Admin tab > Refresh student list.", tab = "admin")
  else if (canvas_age > 7)
    out[[1]] <- task(sprintf("Canvas export is %s days old", floor(canvas_age)), id = paste0("canvas-age:", basename(canvas)),
                     paste("Students who dropped are still on the list until you download a fresh Canvas gradebook",
                           "into admin/inputs/canvas-gradebook/ and refresh. Do this before finding submissions."),
                     tab = "admin", level = if (canvas_age > 14) "todo" else "info")
  for (iss in setdiff(unique(st$issue), "")) {
    n <- sum(st$issue == iss)
    out[[length(out) + 1]] <- task(sprintf("%s: %d student%s", iss, n, if (n == 1) "" else "s"),
                                   "See the Students tab (email template there).", tab = "students")
  }
  dplyr::bind_rows(out)
}

check_calendar <- function() {
  cal <- read_calendar()
  if (is.null(cal)) return(no_tasks())
  out <- list()
  # Only nag about items from this week on — past weeks are history.
  wk <- current_week(); if (is.na(wk)) wk <- 0
  blank <- cal$item[(is.na(cal$student_due) | is.na(cal$grading_due)) & cal$week >= wk]
  if (length(blank)) out[[1]] <- task(
    sprintf("%d calendar item(s) without due dates", length(blank)),
    paste("Fill student_due and grading_due in admin/config/calendar.csv:", paste(head(blank, 8), collapse = ", "),
          if (length(blank) > 8) "…" else ""), tab = "calendar", level = "info")
  dplyr::bind_rows(out)
}

# --- everyone --------------------------------------------------------------

check_my_setup <- function(user) {
  out <- list()
  if (!is.null(user$login_error))
    out[[1]] <- task("Set up your GitHub token", user$login_error, tab = "setup", level = "overdue")
  else if (user$role == "unknown")
    out[[1]] <- task(sprintf("GitHub user '%s' is not on the staff list", user$username),
                     "Ask Mine or Julie to add you.", tab = "setup", level = "overdue")
  else if (user$role == "admin-no-folder")
    out[[1]] <- task("Admin folder not found on this computer",
                     "Add the course folder to Google Drive for desktop. See Setup tab.", tab = "setup", level = "overdue")
  if (is_admin(user) && !nzchar(canvas_token()))
    out[[length(out) + 1]] <- task("Save your Canvas token on this computer",
                                   "Needed to post grades. See docs/setup-canvas-token.md.", tab = "setup")
  if (user$role == "ca" && is.na(grader_dir(user$username)))
    out[[length(out) + 1]] <- task("Your personal grading folder isn't on this computer",
                                   "Add it as a shortcut to My Drive. See Setup tab.", tab = "setup", level = "overdue")
  dplyr::bind_rows(out)
}

check_upcoming_grading <- function(user) {
  cal <- read_calendar()
  if (is.null(cal)) return(no_tasks())
  soon <- cal[!is.na(cal$grading_due) & cal$grading_due > now_et() - 3 * 86400 &
              cal$grading_due < now_et() + 10 * 86400, ]
  if (!nrow(soon)) return(no_tasks())
  dplyr::bind_rows(lapply(seq_len(nrow(soon)), function(i) task(
    sprintf("Grading for %s is due", soon$item[i]), soon$title[i], due = soon$grading_due[i],
    tab = if (soon$type[i] == "section") "section" else "grading",
    level = if (soon$grading_due[i] < now_et()) "overdue" else "todo")))
}

check_monitor_duty <- function(user) {
  if (!isTRUE(user$monitor_this_week) && !isTRUE(user$monitor)) return(no_tasks())
  wk <- current_week()
  out <- list(task(sprintf("You're the grading monitor%s", if (isTRUE(user$monitor_this_week)) sprintf(" for week %s", wk) else ""),
       paste("Check that everyone finishes their grading on time and remind anyone who's behind (Progress tab).",
             if (isTRUE(user$monitor_this_week)) "You have no grading of your own this week." else ""),
       tab = "progress", level = "info", id = paste0("monitor-week-", wk)))
  p <- read_progress()
  for (it in progress_items(p)) {
    b <- graders_behind(it, p); if (is.null(b)) next
    due <- read_calendar()$grading_due[match(it, read_calendar()$item)]
    out[[length(out) + 1]] <- task(sprintf("%d grader(s) not done with %s", nrow(b), it),
      paste(b$name, collapse = ", "), due = due, tab = "progress",
      level = if (!is.na(due) && due < now_et()) "overdue" else "todo")
  }
  dplyr::bind_rows(out)
}

# Grading assigned to me: one task per item with work left.
check_my_grading <- function(user) {
  its <- tryCatch(my_items(user$username), error = function(e) character())
  if (!length(its)) return(no_tasks())
  cal <- read_calendar()
  out <- lapply(its, function(it) {
    a <- my_assignment(user$username, it); g <- read_grades(user$username, it)
    w <- if (("wait_until" %in% names(a))) parse_when(a$wait_until) else rep(NA, nrow(a))
    a <- a[!((!is.na(w) & w > now_et()) | is.na(a$sha) | a$sha == ""), ]   # extensions: not yet
    left <- sum(!a$github %in% g$github)
    if (!left) return(NULL)
    due <- cal$grading_due[match(it, cal$item)]
    task(sprintf("Grade %s: %d of %d left", it, left, nrow(a)), cal$title[match(it, cal$item)] %||% "",
         due = due, tab = "grading", level = if (!is.na(due) && due < now_et()) "overdue" else "todo")
  })
  dplyr::bind_rows(out)
}

# Admin: items whose deadline + late window has passed but haven't been set up.
check_grading_setup <- function() {
  cal <- read_calendar(); if (is.null(cal)) return(no_tasks())
  late_h <- as.numeric(read_course()$late_window_hours %||% 48)   # wait this long after the deadline
  ready <- cal[cal$type %in% c("homework", "section") & !is.na(cal$student_due) & cal$student_due + late_h * 3600 < now_et() &
               cal$week >= (current_week() %||% 0) - 3, ]
  out <- lapply(seq_len(nrow(ready)), function(i) {
    it <- ready$item[i]
    sn <- read_snapshot(it)
    if (is.null(sn)) return(task(sprintf("Find submissions for %s", it), "Admin tab > Weekly grading, step 1.", tab = "admin"))
    t <- list()
    nr <- sum(sn$status == "needs review")
    if (nr) t[[length(t) + 1]] <- task(sprintf("Review %d late submission(s) for %s", nr, it),
                                        "Late work tab. Graders can't be assigned until this is done.", tab = "late")
    else if (is.null(read_assignments(it))) t[[length(t) + 1]] <- task(sprintf("Assign graders for %s", it),
                                        "Admin tab > Weekly grading, step 2.", tab = "admin")
    if (!file.exists(rubric_path(it))) t[[length(t) + 1]] <- task(sprintf("Add the rubric for %s", it),
                                        sprintf("stat100-ca-shared/rubrics/%s-rubric.csv", it), tab = "admin")
    dplyr::bind_rows(t)
  })
  dplyr::bind_rows(out)
}

# Preparing assignments and rubrics (calendar.csv: assignment_owner,
# assignment_ready_by, rubric_owner, rubric_ready_by). Shown from 3 weeks
# before the date. A rubric task disappears once the rubric file exists and
# passes the check; an assignment task once the week's repo exists on GitHub
# (or when dismissed).
check_prep <- function(user) {
  cal <- read_calendar(); if (is.null(cal)) return(no_tasks())
  me <- tolower(user$username %||% "")
  soon <- function(d) !is.na(d) & d < now_et() + 21 * 86400 & d > now_et() - 30 * 86400
  out <- list()
  a <- cal[tolower(cal$assignment_owner %||% "") == me & soon(cal$assignment_ready_by), ]
  for (i in seq_len(nrow(a))) {
    exists <- tryCatch({ gh::gh("GET /repos/{o}/{r}", o = read_course()$github_org, r = a$repo[i]); TRUE }, error = function(e) FALSE)
    if (exists && a$type[i] == "homework") {
      tl <- tryCatch(find_item_file(template_copy(a$repo[i]), a$item[i], "homework"), error = function(e) NA)
      exists <- !is.na(tl)
    }
    if (!exists) out[[length(out) + 1]] <- task(sprintf("Create %s (%s)", a$item[i], a$title[i]),
      sprintf("The week repo %s with its %s file.", a$repo[i], a$type[i]), due = a$assignment_ready_by[i],
      level = if (a$assignment_ready_by[i] < now_et()) "overdue" else "todo", id = paste0("make-", a$item[i]))
  }
  r <- cal[tolower(cal$rubric_owner %||% "") == me & cal$type == "homework", ]
  r$when <- r$rubric_ready_by
  r$when[is.na(r$when)] <- r$student_due[is.na(r$when)]
  r <- r[soon(r$when), ]
  for (i in seq_len(nrow(r))) {
    ok <- file.exists(shared_path("rubrics", paste0(r$item[i], "-rubric.csv"))) && !length(check_rubric(r$item[i]))
    if (!ok) out[[length(out) + 1]] <- task(sprintf("Make the rubric for %s", r$item[i]), r$title[i],
      due = r$when[i], tab = "rubrics", level = if (r$when[i] < now_et()) "overdue" else "todo")
  }
  dplyr::bind_rows(out)
}

# Quizzes: a Poll Everywhere file waiting to be posted, or a quiz in the
# calendar whose date has passed with no file yet.
check_quizzes <- function() {
  out <- list()
  cal <- read_calendar()
  due_of <- function(it) { d <- cal$grading_due[match(tolower(it), tolower(cal$item))]; if (length(d)) d else NA }
  for (f in unposted_quiz_files()) {
    it <- quiz_name_from_file(f); due <- due_of(it)
    out[[length(out) + 1]] <- task(sprintf("Grade %s", it), paste("Match any unmatched rows of", basename(f), "on the Quizzes tab, then post it on the Post to Canvas tab."), due = due,
                                   tab = "canvas", id = paste0("quiz-", basename(f)),
                                   level = if (!is.na(due) && due < now_et()) "overdue" else "todo")
  }
  # Quizzes in calendar.csv given in the last week with no Poll Everywhere file yet.
  q <- cal[cal$type %in% "quiz" & !is.na(cal$student_due) & cal$student_due < now_et() & cal$student_due > now_et() - 7 * 86400, ]
  have <- tolower(c(quiz_name_from_file(list.files(pollev_dir(), pattern = "\\.csv$")), unique(read_canvas_log()$item)))
  for (i in seq_len(nrow(q))) if (!tolower(q$item[i]) %in% have)
    out[[length(out) + 1]] <- task(sprintf("Grade %s", q$item[i]),
      "Download the Poll Everywhere gradebook into admin/inputs/pollev/, check matches on the Quizzes tab, then post it on the Post to Canvas tab. (No quiz that day? Dismiss, or delete the row from calendar.csv.)",
      due = q$grading_due[i], tab = "quizzes", id = paste0("quiz-cal-", q$item[i]),
      level = if (!is.na(q$grading_due[i]) && q$grading_due[i] < now_et()) "overdue" else "todo")
  dplyr::bind_rows(out)
}

# Post to Canvas: homework/section grades that are ready (everyone graded,
# or only extensions left), and changes after posting (regrades). Quizzes are
# covered by check_quizzes() for the quiz grader.
check_canvas_posting <- function(user) {
  s <- canvas_summary(user)
  its <- canvas_items(user); s$type <- its$type[match(s$item, its$item)]
  out <- list()
  for (i in seq_len(nrow(s))) {
    if (s$changed[i] > 0) out[[length(out) + 1]] <- task(sprintf("Post %d changed grade(s) for %s to Canvas", s$changed[i], s$item[i]),
      "Grades changed after they were posted (e.g. a regrade). Post to Canvas tab.", tab = "canvas")
    if (s$ready[i] > 0 && (s$type[i] != "quiz" || s$posted[i] > 0)) {
      first <- s$posted[i] == 0
      out[[length(out) + 1]] <- task(
        sprintf(if (first) "Post %s grades to Canvas (%d ready%s)" else "Post %2$d more grade(s) for %1$s to Canvas%3$s", s$item[i], s$ready[i],
                if (s$waiting[i] > 0) sprintf(", %d not ready yet", s$waiting[i]) else ""),
        if (first && s$waiting[i] > 0) "Some aren't graded yet; you can wait or post the ready ones now." else "Post to Canvas tab.",
        tab = "canvas", level = if (first && s$waiting[i] > 0) "info" else "todo")
    }
  }
  # EXCUSED for items not on the Post to Canvas list yet (e.g. a quiz with no file)
  e <- read_exceptions(); e <- e[e$type == "excused" & !e$canvas_excused & !e$item %in% s$item, ]
  if (nrow(e)) out[[length(out) + 1]] <- task(sprintf("%d excused student(s) not marked on Canvas yet", nrow(e)),
    sprintf("For %s. They're marked EXCUSED when you post that item on the Post to Canvas tab.", paste(unique(e$item), collapse = ", ")),
    tab = "canvas", level = "info")
  dplyr::bind_rows(out)
}

check_on_hold <- function(user) {
  if (!isTRUE(user$on_hold) || isTRUE(user$monitor_this_week)) return(no_tasks())
  task("You're on hold this week", "You won't be assigned new grading.", level = "info")
}

# --- put it together -------------------------------------------------------

tasks_for <- function(user = current_user()) {
  safe <- function(f, ...) tryCatch(f(...), error = function(e)
    task(paste("Couldn't check:", deparse(substitute(f))), conditionMessage(e), level = "info"))
  tl <- list(safe(check_my_setup, user), safe(check_monitor_duty, user), safe(check_on_hold, user))
  if (is_admin(user)) tl <- c(tl, list(safe(check_setup_files), safe(check_staff), safe(check_roster),
                                       safe(check_coi), safe(check_calendar), safe(check_grading_setup), safe(check_canvas_posting, user)))
  if (is_admin(user) && tolower(user$username %||% "") %in% tolower(trimws(strsplit(read_course()$quiz_grader %||% "", ",")[[1]])))
    tl <- c(tl, list(safe(check_quizzes)))
  if (user$role %in% c("ca", "admin")) tl <- c(tl, list(safe(check_my_grading, user), safe(check_prep, user)))
  out <- dplyr::bind_rows(tl)
  if (nrow(out)) {
    out$id[is.na(out$id)] <- out$title[is.na(out$id)]
    gone <- read_dismissed(user)
    hidden <- sum(out$id %in% gone)
    out <- out[!out$id %in% gone, ]
  } else hidden <- 0
  if (!nrow(out)) out <- task("Nothing pending", "You're all caught up.", level = "ok", id = "")
  attr(out, "hidden") <- hidden
  ord <- match(out$level, c("overdue", "todo", "info", "ok"))
  res <- out[order(ord, out$due, na.last = TRUE), ]
  attr(res, "hidden") <- hidden
  res
}
