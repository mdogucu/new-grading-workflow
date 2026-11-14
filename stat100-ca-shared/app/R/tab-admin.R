# ---------------------------------------------------------------------------
# tab-admin.R — admin only: buttons that rebuild/publish course files
#
#   Refresh student list   inputs/ -> admin/data/students.csv (+ CA-safe copy)
#   Publish course info    admin/config/ -> stat100-ca-shared/course-info/
#   Create grader folders  one folder per CA under graders/ (you share them in Drive)
#   Weekly grading         1. Find submissions  2. Assign graders  + progress
#                          (the logic is in R/09-grading.R)
#
# Every button calls require_admin() first and writes to admin/logs/action-log.csv.
# ---------------------------------------------------------------------------

admin_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    shiny::h2("Admin"),
    bslib::layout_column_wrap(width = 1/3,
      bslib::card(bslib::card_header(icon_tag("people"), " Student list"),
        shiny::p("Rebuilds the student list from the newest files in admin/inputs/."),
        shiny::uiOutput(ns("inputs")),
        shiny::actionButton(ns("refresh"), "Refresh student list", class = "btn-primary")),
      bslib::card(bslib::card_header(icon_tag("share"), " Publish course info"),
        shiny::p("Copies calendar, sections and staff (no emails, no COI) to the shared folder so CAs see the latest."),
        shiny::uiOutput(ns("published")),
        shiny::actionButton(ns("publish"), "Publish course info", class = "btn-primary")),
      bslib::card(bslib::card_header(icon_tag("folder-plus"), " Grader folders"),
        shiny::p("Creates graders/stat100-grader-<username> for every CA who doesn't have one yet."),
        shiny::uiOutput(ns("folders")),
        shiny::actionButton(ns("make_folders"), "Create missing folders", class = "btn-primary"))
    ),
    bslib::card(bslib::card_header(icon_tag("pencil-square"), " Weekly grading"),
      bslib::layout_columns(col_widths = c(4, 8),
        shiny::div(
          shiny::selectInput(ns("g_item"), "Assignment", choices = NULL),
          shiny::uiOutput(ns("g_dates")),
          shiny::actionButton(ns("g_snap"), "1. Find submissions", class = "btn-primary w-100 mb-2"),
          shiny::tags$small(class = "text-muted d-block mb-3",
            "Reads each student's commit list on GitHub (nothing is downloaded) and picks the graded commit. Homework with commits after the deadline goes to the Late work tab for review."),
          shiny::actionButton(ns("g_assign"), "2. Assign graders", class = "btn-primary w-100 mb-2"),
          shiny::tags$small(class = "text-muted d-block",
            "Homework: shuffled with set.seed(week), split evenly among active CAs, skipping conflicts. Section: to the section leader. Running it again redoes it (the old file is kept).")),
        shiny::uiOutput(ns("g_status")))),
    bslib::card(bslib::card_header("Recent actions"), DT::DTOutput(ns("log")))
  )
}

admin_server <- function(id, user, refresh) {
  shiny::moduleServer(id, function(input, output, session) {
    tick <- shiny::reactiveVal(0)
    run <- function(label, expr) {
      res <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
      if (is.null(res)) shiny::showNotification(paste(label, "— done."), type = "message")
      else shiny::showNotification(paste(label, "failed:", res), type = "error", duration = NULL)
      tick(tick() + 1); refresh(refresh() + 1)
    }

    output$inputs <- shiny::renderUI({ tick()
      rows <- lapply(c("canvas-gradebook", "gradescope-survey", "section-roster"), function(d) {
        f <- newest_file(admin_path("inputs", d))
        shiny::tags$li(shiny::tags$code(d), ": ",
          if (is.na(f)) shiny::tags$em("no file") else
            sprintf("%s (%s)", basename(f), format(file.mtime(f), "%b %d")))
      })
      shiny::tags$ul(class = "small", rows)
    })
    output$published <- shiny::renderUI({ tick()
      f <- shared_path("course-info", "staff.csv")
      shiny::p(class = "small text-muted", if (file.exists(f))
        paste("Last published", format(file.mtime(f), "%b %d, %l:%M %p")) else "Never published.")
    })
    output$folders <- shiny::renderUI({ tick()
      s <- read_staff(); cas <- s$username[s$role == "ca" & !is.na(s$username) & s$username != ""]
      have <- dir.exists(file.path(course_root(), "graders", grader_folder_name(cas)))
      shiny::p(class = "small text-muted", sprintf("%d of %d CAs have a folder.", sum(have), length(cas)),
               if (any(!have)) paste("Missing:", paste(cas[!have], collapse = ", ")))
    })

    # --- weekly grading ---
    shiny::observe({ tick()
      cal <- read_calendar(); cal <- cal[!is.na(cal$student_due) & cal$type %in% c("homework", "section"), ]
      cal <- cal[order(cal$student_due, decreasing = TRUE), ]
      shiny::updateSelectInput(session, "g_item", choices = item_choices(cal$item, cal$title, "Add due dates in calendar.csv first"),
                               selected = { cur <- shiny::isolate(input$g_item); if (!nrow(cal)) "" else if (!is.null(cur) && cur %in% cal$item) cur else cal$item[1] })
    })
    output$g_dates <- shiny::renderUI({ shiny::req(input$g_item); it <- item_info(input$g_item)
      shiny::tags$ul(class = "small",
        shiny::tags$li("Students submit by ", shiny::strong(fmt_due(it$student_due))),
        if (it$type == "homework") shiny::tags$li("Late: reviewed on the Late work tab (", allowance_minutes() / 60, "-hour semester allowance)")
        else shiny::tags$li("Late section work: flagged on the Late work tab — you allow it or not"),
        shiny::tags$li("Grading due ", shiny::strong(fmt_due(it$grading_due))),
        shiny::tags$li("Rubric: ", if (file.exists(rubric_path(input$g_item))) basename(rubric_path(input$g_item)) else shiny::tags$em("missing!")),
        {
          cf <- newest_file(admin_path("inputs", "canvas-gradebook"))
          age <- if (is.na(cf)) NA else floor(as.numeric(difftime(Sys.time(), file.mtime(cf), units = "days")))
          shiny::tags$li("Student list from Canvas export of ", if (is.na(cf)) shiny::tags$em("none") else format(file.mtime(cf), "%b %d"),
            if (!is.na(age) && age > 7) shiny::span(class = "text-danger fw-bold",
              sprintf(" — %d days old; download a fresh one and Refresh student list first (dropped students are still on it)", age)))
        })
    })
    shiny::observeEvent(input$g_snap, {
      shiny::withProgress(message = "Checking repos", value = 0,
        run("Find submissions", snapshot_item(input$g_item, user, progress = function(v, d) shiny::setProgress(v, detail = d))))
    })
    shiny::observeEvent(input$g_assign, run("Assign graders", assign_item(input$g_item, user)))
    output$g_status <- shiny::renderUI({ tick(); shiny::req(input$g_item)
      sn <- read_snapshot(input$g_item)
      if (is.null(sn)) return(empty_state("Step 1 not done yet."))
      tab <- table(factor(sn$status, levels = unique(c("on time", "needs review", "late", "extension", "late (allowed)", "deadline version", "excused (no grade)", "over allowance (0)", "no submission", "no repo", sn$status))))
      asg <- read_assignments(input$g_item)
      prog <- if (!is.null(asg)) {
        st <- read_staff()
        do.call(rbind, lapply(sort(unique(stats::na.omit(asg$grader))), function(g) {
          done <- unique(read_grades(g, input$g_item)$github)
          n <- sum(asg$grader %in% g)
          data.frame(Grader = st$name[match(g, st$username)] %||% g, Assigned = n,
                     Graded = sum(asg$github[asg$grader %in% g] %in% done))
        }))
      }
      odd <- sn[!sn$status %in% c("on time"), c("github", "section", "status", "minutes_late", "commit_time")]
      shiny::tagList(
        shiny::div(class = "d-flex gap-2 flex-wrap mb-2", lapply(names(tab), function(k)
          if (tab[[k]] > 0) shiny::span(class = "badge bg-light text-dark border", sprintf("%s: %d", k, tab[[k]])))),
        if (!is.null(prog)) shiny::tagList(shiny::h6("Progress"),
          shiny::tags$table(class = "table table-sm", shiny::tags$thead(shiny::tags$tr(lapply(names(prog), shiny::tags$th))),
            shiny::tags$tbody(lapply(seq_len(nrow(prog)), function(i) shiny::tags$tr(
              lapply(prog[i, ], function(v) shiny::tags$td(v))))))) else shiny::p(class = "text-muted", "Step 2 not done yet."),
        if (any(is.na(asg$grader))) shiny::div(class = "alert alert-warning",
          "No grader could be found for: ", paste(asg$github[is.na(asg$grader)], collapse = ", ")),
        if (nrow(odd)) shiny::tags$details(shiny::tags$summary(sprintf("Late or missing (%d)", nrow(odd))),
          shiny::tags$table(class = "table table-sm small", shiny::tags$thead(shiny::tags$tr(lapply(names(odd), shiny::tags$th))),
            shiny::tags$tbody(lapply(seq_len(nrow(odd)), function(i) shiny::tags$tr(lapply(odd[i, ], function(v) shiny::tags$td(v %||% "")))))))
      )
    })

    shiny::observeEvent(input$refresh, run("Refresh student list", refresh_students(user)))
    shiny::observeEvent(input$publish, run("Publish course info", publish_course_info(user)))
    shiny::observeEvent(input$make_folders, run("Create grader folders", create_grader_folders(user)))

    output$log <- DT::renderDT({ tick()
      f <- admin_path("logs", "action-log.csv")
      d <- if (file.exists(f)) utils::tail(read_csv_quiet(f), 50) else data.frame(time = character())
      DT::datatable(d[rev(seq_len(nrow(d))), , drop = FALSE], rownames = FALSE, options = list(dom = "tp", pageLength = 10))
    })
  })
}

create_grader_folders <- function(user = current_user()) {
  require_admin(user, "create grader folders")
  s <- read_staff(); cas <- s$username[s$role == "ca" & !is.na(s$username) & s$username != ""]
  made <- character()
  for (u in cas) {
    d <- file.path(course_root(), "graders", grader_folder_name(u))
    if (dir.exists(d)) next
    dir.create(d, recursive = TRUE)
    writeLines(grader_readme(u, s$name[s$username == u][1]), file.path(d, "README.txt"))
    made <- c(made, u)
  }
  log_action(user$username, "create grader folders", if (length(made)) made else "none needed")
  made
}

grader_readme <- function(username, name) c(
  sprintf("PERSONAL GRADING FOLDER — %s (%s)", name, username),
  "",
  "Only you and the admin team can see this folder.",
  "The dashboard reads and writes here; you never need to edit these files by hand.",
  "",
  "What will appear here:",
  "  grading/    your grading assignments and the grades you've saved",
  "  regrades/   regrade requests sent to you",
  "  triage/     (senior CAs only) new regrade requests to sort",
  "",
  "Setup: in Google Drive, right-click this folder > Organize > Add shortcut > My Drive.",
  "Do the same for the stat100-ca-shared folder. Then open stat100-ca-shared/app/app.R in RStudio."
)
