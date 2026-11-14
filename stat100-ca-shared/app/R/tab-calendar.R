# ---------------------------------------------------------------------------
# tab-calendar.R — what's due when
#
# Everyone sees "My calendar": only what that person does —
#   CAs:     grading of homework and their section (done ones crossed out),
#            and, for senior CAs, their weeks as grading monitor.
#   admins:  the same, plus what they own in calendar.csv (making an
#            assignment or rubric) and, for the quiz grader, quizzes.
# Admins also get "Full calendar": every row of calendar.csv.
# Data: admin/config/calendar.csv (admins) or course-info/calendar.csv (CAs).
# ---------------------------------------------------------------------------

calendar_ui <- function(id, user) {
  ns <- shiny::NS(id)
  mine <- shiny::div(shiny::br(), shiny::uiOutput(ns("mine")))
  shiny::div(class = "container-narrow",
    shiny::h2("Calendar"),
    shiny::p(class = "text-muted", "Times are Eastern."),
    if (is_admin(user)) bslib::navset_underline(
      bslib::nav_panel("My calendar", mine),
      bslib::nav_panel("Full calendar", shiny::br(), DT::DTOutput(ns("cal"))))
    else mine)
}

# One row per thing this person has to do: date, what, status.
my_calendar <- function(user) {
  cal <- read_calendar(); if (is.null(cal)) return(NULL)
  me <- user$username %||% ""; now <- now_et(); rows <- list()
  add <- function(when, what, detail, status, kind) rows[[length(rows) + 1]] <<-
    data.frame(when = as.POSIXct(when, tz = "America/New_York"), what = what, detail = detail, status = status, kind = kind)

  # Grading (homework + section)
  g <- cal[cal$type %in% c("homework", "section") & !is.na(cal$grading_due), ]
  sec <- read_sections()
  my_sec <- if (!is.null(sec)) sec$section[tolower(sec$leader) == tolower(me)] else character()
  if (user$role %in% c("ca", "admin")) for (i in seq_len(nrow(g))) {
    it <- g$item[i]
    a <- tryCatch(my_assignment(me, it), error = function(e) NULL)
    label <- sprintf("Grade %s · %s", it, g$title[i])
    if (!is.null(a) && nrow(a)) {
      n <- nrow(a); k <- sum(a$github %in% read_grades(me, it)$github)
      st <- if (k >= n) "done" else if (g$grading_due[i] < now) "overdue" else "todo"
      add(g$grading_due[i], label, sprintf("%d of %d graded", k, n), st, g$type[i])
    } else if (g$grading_due[i] >= now - 7 * 86400 && user$role == "ca") {
      # not assigned (yet): coming up, unless it's long past
      will <- g$type[i] == "homework" || length(my_sec)
      past <- g$grading_due[i] < now
      if (will) add(g$grading_due[i], label,
                    if (past) "Nobody was assigned to you (e.g. on hold that week)"
                    else if (g$type[i] == "section") paste("Your section", paste(my_sec, collapse = ", "))
                    else sprintf("Students submit %s; your list appears after that", fmt_due(g$student_due[i])),
                    if (past) "not assigned" else "coming up", g$type[i])
    }
  }

  # Grading monitor weeks (senior CAs)
  ms <- tryCatch(monitor_schedule(), error = function(e) NULL)
  if (!is.null(ms)) {
    mine <- ms[tolower(ms$monitor) == tolower(me), ]
    for (i in seq_len(nrow(mine))) {
      end <- as.POSIXct(paste(mine$week_of[i] + 6, "23:59"), tz = "America/New_York")
      add(as.POSIXct(paste(mine$week_of[i], "00:00"), tz = "America/New_York"),
          sprintf("Grading monitor · week %d", mine$week[i]), "Check everyone's progress; no grading of your own that week",
          if (end < now) "done" else if (mine$week_of[i] <= as.Date(now)) "this week" else "coming up", "monitor")
    }
  }

  # Things I make (calendar.csv owners): assignments and rubrics
  mk <- cal[tolower(cal$assignment_owner %||% "") == tolower(me) & !is.na(cal$assignment_ready_by), ]
  for (i in seq_len(nrow(mk))) add(mk$assignment_ready_by[i], sprintf("Create %s · %s", mk$item[i], mk$title[i]), "",
                                   if (mk$assignment_ready_by[i] < now) "past" else "coming up", "make")
  rb <- cal[tolower(cal$rubric_owner %||% "") == tolower(me) & cal$type == "homework", ]
  rb$when <- rb$rubric_ready_by; rb$when[is.na(rb$when)] <- rb$student_due[is.na(rb$when)]
  rb <- rb[!is.na(rb$when), ]
  for (i in seq_len(nrow(rb))) {
    ok <- file.exists(shared_path("rubrics", paste0(rb$item[i], "-rubric.csv")))
    add(rb$when[i], sprintf("Rubric for %s", rb$item[i]), rb$title[i],
        if (ok) "done" else if (rb$when[i] < now) "overdue" else "coming up", "rubric")
  }

  # Quizzes: only for the quiz grader
  if (is_admin(user) && tolower(me) %in% tolower(trimws(strsplit(read_course()$quiz_grader %||% "", ",")[[1]]))) {
    q <- cal[cal$type %in% "quiz" & !is.na(cal$grading_due), ]
    posted <- tolower(unique(quiz_posted()$item))
    for (i in seq_len(nrow(q))) add(q$grading_due[i], sprintf("Grade %s", q$item[i]), q$title[i],
      if (tolower(q$item[i]) %in% posted) "done" else if (q$grading_due[i] < now) "overdue" else "coming up", "quiz")
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out[order(out$when), ]
}

calendar_server <- function(id, user) {
  shiny::moduleServer(id, function(input, output, session) {
    output$mine <- shiny::renderUI({
      d <- my_calendar(user)
      if (is.null(d)) return(empty_state("Nothing on your calendar yet."))
      now <- now_et()
      open <- c("todo", "overdue", "this week", "coming up")
      upcoming <- d[d$status %in% open, ]
      earlier <- d[!d$status %in% open, ]
      row_ui <- function(r) shiny::div(class = paste("cal-row", paste0("cal-", gsub(" ", "-", r$status))),
        shiny::div(class = "cal-date", format(r$when, "%a %b %d"), if (format(r$when, "%H:%M") != "00:00") shiny::div(class = "cal-time", format(r$when, "%l:%M %p"))),
        shiny::div(class = "cal-what", shiny::div(class = "cal-title", r$what), if (nzchar(r$detail)) shiny::div(class = "cal-detail", r$detail)),
        shiny::span(class = paste("pill", c(done = "pill-ok", overdue = "pill-overdue", todo = "pill-todo", `this week` = "pill-todo", `coming up` = "pill-info",
                                            `not assigned` = "pill-muted", past = "pill-muted")[[r$status]]),
                    c(done = "Done", overdue = "Overdue", todo = "In progress", `this week` = "This week", `coming up` = "Coming up",
                      `not assigned` = "Not yours", past = "Past")[[r$status]]))
      shiny::tagList(
        shiny::h5("Coming up"),
        if (nrow(upcoming)) lapply(seq_len(nrow(upcoming)), function(i) row_ui(upcoming[i, ])) else shiny::p(class = "text-muted", "Nothing coming up."),
        if (nrow(earlier)) shiny::tags$details(class = "mt-3", shiny::tags$summary(sprintf("Earlier (%d)", nrow(earlier))),
          lapply(rev(seq_len(nrow(earlier))), function(i) row_ui(earlier[i, ]))))
    })
    output$cal <- DT::renderDT({
      cal <- read_calendar()
      if (is.null(cal)) return(NULL)
      show <- data.frame(Week = cal$week, Item = cal$item, Type = cal$type, Title = cal$title,
                         `Students submit by` = vapply(cal$student_due, fmt_due, ""),
                         `Grading due` = vapply(cal$grading_due, fmt_due, ""),
                         `Made by` = paste(cal$assignment_owner %||% "", vapply(cal$assignment_ready_by, fmt_due, "")),
                         `Rubric by` = paste(cal$rubric_owner %||% "", vapply(cal$rubric_ready_by, fmt_due, "")),
                         check.names = FALSE)
      show$`Made by` <- trimws(gsub("^NA ?", "", show$`Made by`)); show$`Rubric by` <- trimws(gsub("^NA ?", "", show$`Rubric by`))
      DT::datatable(show, rownames = FALSE, filter = "top", options = list(dom = "ft", pageLength = 200))
    })
  })
}
