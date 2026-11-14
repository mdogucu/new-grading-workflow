# ---------------------------------------------------------------------------
# tab-late.R — admin only: extensions & excused (top), review late work,
# post late notices. Logic: R/10-late.R and R/14-exceptions.R
# Logic and policy: R/10-late.R.  Decisions: admin/data/late-ledger.csv
# ---------------------------------------------------------------------------

gt_minutes <- function(v) suppressWarnings(as.numeric(sub("\\|.*$", "", v %||% "")))

late_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    bslib::card(bslib::card_header(icon_tag("calendar-plus"), " Extensions and excused"),
      shiny::p(class = "small text-muted",
        shiny::strong("Extension"), ": a new deadline for one student (enter it before or after the deadline). ",
        shiny::strong("Excused"), ": the student doesn't do this one — not graded, and marked EXCUSED on Canvas (not full points, not 0)."),
      bslib::layout_columns(col_widths = c(3, 3, 2, 4),
        shiny::selectInput(ns("x_item"), "Homework, section or quiz", choices = NULL),
        shiny::selectizeInput(ns("x_student"), "Student", choices = NULL, options = list(placeholder = "Type a name or username")),
        shiny::radioButtons(ns("x_type"), "Type", c("Extension" = "extension", "Excused" = "excused")),
        shiny::div(
          shiny::conditionalPanel(sprintf("input['%s'] == 'extension'", ns("x_type")),
            shiny::div(class = "d-flex gap-2",
              shiny::dateInput(ns("x_date"), "New deadline", width = "160px"),
              shiny::textInput(ns("x_time"), "Time", value = "23:59", width = "100px"))),
          shiny::textInput(ns("x_note"), "Note (private)", placeholder = "e.g. sick, DAO letter", width = "100%"))),
      shiny::actionButton(ns("x_add"), "Add", icon = icon_tag("plus"), class = "btn-primary btn-sm", width = "120px"),
      shiny::hr(),
      shiny::uiOutput(ns("x_list"))),
    shiny::div(class = "page-head", shiny::h2("Late work"),
      shiny::div(style = "min-width: 280px;", shiny::selectInput(ns("item"), NULL, choices = NULL, width = "100%"))),
    shiny::uiOutput(ns("intro")),
    shiny::uiOutput(ns("cases")),
    bslib::card(bslib::card_header(icon_tag("send"), " Late notices for Canvas"), shiny::uiOutput(ns("notices"))),
    bslib::card(bslib::card_header("Late minutes so far this semester"), DT::DTOutput(ns("totals")))
  )
}

late_server <- function(id, user, refresh) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tick <- shiny::reactiveVal(0)
    bump <- function() { tick(tick() + 1); refresh(refresh() + 1) }

    shiny::observe({ refresh()
      cal <- read_calendar(); cal <- cal[cal$type %in% c("homework", "section") & !is.na(cal$student_due), ]
      cal <- cal[order(cal$student_due, decreasing = TRUE), ]
      has <- file.exists(admin_path("data", "snapshots", paste0(cal$item, "-commits.csv")))
      cal <- cal[has, ]
      shiny::updateSelectInput(session, "item", choices = item_choices(cal$item, cal$title, "Run Find submissions for a homework first"),
                               selected = { cur <- shiny::isolate(input$item); if (!nrow(cal)) "" else if (!is.null(cur) && cur %in% cal$item) cur else cal$item[1] })
    })

    cases <- shiny::reactive({ tick(); refresh(); shiny::req(input$item); late_cases(input$item) })

    output$intro <- shiny::renderUI({
      shiny::req(input$item); it <- item_info(input$item); cs <- cases()
      n_todo <- if (is.null(cs)) 0 else sum(is.na(cs$decision))
      hw <- it$type == "homework"
      shiny::div(class = "late-intro",
        shiny::p("Deadline ", shiny::strong(fmt_due(it$student_due)), " · ",
                 if (hw) shiny::span("semester allowance ", shiny::strong(allowance_minutes() / 60, " hours"), " · "),
                 if (n_todo) shiny::span(class = "text-danger fw-bold", n_todo, " still to decide (graders can't be assigned until these are done)")
                 else shiny::span(class = "text-success fw-bold", "all decided")),
        if (hw) shiny::tags$small(class = "text-muted",
          shiny::strong("Late"), ": grade up to the chosen commit and count the minutes. ",
          shiny::strong("Extension"), ": grade up to the chosen commit, count 0 (special permission). ",
          shiny::strong("Not late"), ": grade the version at the deadline, count 0 (later commits weren't the homework).")
        else shiny::tags$small(class = "text-muted",
          "Section work is due by the deadline. ", shiny::strong("Allowed"), ": grade the late work (e.g. the student was sick). ",
          shiny::strong("Not allowed"), ": grade the version at the deadline. No minutes are counted for sections."))
    })

    gt_default <- new.env()   # the commit pre-selected for each card
    output$cases <- shiny::renderUI({
      cs <- cases(); shiny::req(input$item)
      if (is.null(cs)) return(empty_state("Run Find submissions for this homework first (Admin tab)."))
      if (!nrow(cs)) return(empty_state("Nobody committed after the deadline."))
      it <- item_info(input$item); cm <- read_commits(input$item); st <- read_students()
      shiny::tagList(lapply(seq_len(nrow(cs)), function(i) {
        c1 <- cs[i, ]
        after <- cm[tolower(cm$github) == tolower(c1$github) & cm$time > it$student_due, ]
        after <- after[order(after$time), ]
        after$mins <- ceiling(as.numeric(difftime(after$time, it$student_due, units = "mins")))
        # Each choice is one commit; its value is "<minutes after deadline>|<commit>".
        vals <- paste0(after$mins, "|", substr(after$sha, 1, 7))
        choices <- stats::setNames(vals, sprintf("+%s · %s · %s", fmt_minutes(after$mins),
                                                 format(after$time, "%a %b %d %l:%M %p"), substr(after$message, 1, 50)))
        gt <- if (!is.na(c1$grade_through_minutes)) {
          ok <- which(after$mins <= c1$grade_through_minutes)
          if (length(ok)) vals[max(ok)] else { v <- paste0(c1$grade_through_minutes, "|"); choices <- c(choices, stats::setNames(v, "the deadline version")); v }
        } else vals[length(vals)]
        gt_default[[as.character(i)]] <- gt
        hw <- it$type == "homework"
        dec <- c1$decision %||% (if (hw) "late" else "allowed")
        nm <- st$name[match(tolower(c1$github), tolower(st$github))]
        bslib::card(class = paste("late-card", if (is.na(c1$decision)) "late-todo" else "late-done"),
          bslib::card_header(shiny::div(class = "d-flex justify-content-between flex-wrap gap-2",
            shiny::span(shiny::strong(c1$github), " ", shiny::span(class = "text-muted", nm %||% ""),
                        " ", shiny::span(class = "badge bg-light text-dark border", c1$section %||% "")),
            shiny::span(if (is.na(c1$decision)) shiny::span(class = "pill pill-todo", "To review")
                        else shiny::span(class = "pill pill-ok", if (!hw && c1$decision == "not late") "not allowed" else c1$decision),
                        if (isTRUE(c1$posted)) shiny::span(class = "badge bg-secondary ms-1", "notice posted")))),
          bslib::layout_columns(col_widths = c(6, 6),
            shiny::div(
              shiny::p(class = "small mb-1", "Last commit ", shiny::strong(fmt_minutes(c1$suggested_minutes)), " after the deadline",
                       if (hw) shiny::span(" · late minutes in earlier weeks: ", shiny::strong(fmt_minutes(c1$earlier_minutes)))),
              shiny::tags$ul(class = "small commit-list", lapply(seq_len(nrow(after)), function(k)
                shiny::tags$li(shiny::span(class = "mono", substr(after$sha[k], 1, 7)), " +", fmt_minutes(after$mins[k]), " — ", after$message[k]))),
              shiny::tags$a(href = paste0(github_url(c1$repo), "/commits"), target = "_blank", class = "small",
                            icon_tag("github"), " See all commits on GitHub")),
            shiny::div(
              shiny::radioButtons(ns(paste0("dec_", i)), NULL, inline = TRUE, selected = dec,
                                  choices = if (hw) c("Late" = "late", "Extension (no minutes)" = "extension", "Not late" = "not late")
                                            else c("Allowed (grade the late work)" = "allowed", "Not allowed (grade the deadline version)" = "not late")),
              shiny::selectInput(ns(paste0("gt_", i)), "Grade the work up to", choices = choices, selected = gt, width = "100%"),
              shiny::div(class = "d-flex gap-2",
                if (hw) shiny::numericInput(ns(paste0("min_", i)), "Minutes to count", width = "160px",
                                    value = if (!is.na(c1$decision)) c1$minutes_late else gt_minutes(gt), min = 0),
                shiny::textInput(ns(paste0("note_", i)), "Note (private)", value = c1$note %||% "", width = "100%")),
              if (hw) shiny::uiOutput(ns(paste0("after_", i))),
              shiny::tags$button(class = "btn btn-primary btn-sm", type = "button",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("save"), i),
                if (is.na(c1$decision)) "Save decision" else "Update decision")))
        )
      }))
    })

    # Keep "minutes to count" in step with the choices.
    shiny::observe({
      cs <- cases(); shiny::req(cs, nrow(cs) > 0)
      lapply(seq_len(nrow(cs)), function(i) {
        output[[paste0("after_", i)]] <- shiny::renderUI({
          dec <- input[[paste0("dec_", i)]] %||% "late"
          m <- if (dec == "late") as.numeric(input[[paste0("min_", i)]] %||% 0) else 0
          tot <- cs$earlier_minutes[i] + (if (is.na(m)) 0 else m)
          over <- tot > allowance_minutes()
          shiny::p(class = if (over) "small text-danger fw-bold" else "small text-muted",
            sprintf("Semester total after this: %s%s", fmt_minutes(tot),
                    if (over) " — OVER the allowance: this homework gets 0 and isn't graded" else ""))
        })
      })
    })
    # Picking a different commit to grade up to fills in its minutes.
    gt_obs <- list()
    shiny::observe({
      cs <- cases(); shiny::req(cs, nrow(cs) > 0)
      for (o in gt_obs) o$destroy()
      gt_obs <<- lapply(seq_len(nrow(cs)), function(i) shiny::observeEvent(input[[paste0("gt_", i)]], {
        v <- input[[paste0("gt_", i)]]
        g <- gt_minutes(v); if (is.na(g)) return()
        if (identical(v, gt_default[[as.character(i)]])) return()   # just the initial value
        if (identical(input[[paste0("dec_", i)]], "late"))
          shiny::updateNumericInput(session, paste0("min_", i), value = g)
      }, ignoreInit = TRUE))
    })

    shiny::observeEvent(input$save, {
      i <- input$save; cs <- cases()
      dec <- input[[paste0("dec_", i)]]
      res <- tryCatch({
        record_late_decision(input$item, cs$github[i], dec,
                             grade_through = gt_minutes(input[[paste0("gt_", i)]]),
                             penalty = as.numeric(input[[paste0("min_", i)]] %||% 0),
                             note = input[[paste0("note_", i)]], user = user); NULL },
        error = function(e) conditionMessage(e))
      if (is.null(res)) shiny::showNotification(sprintf("Saved %s: %s.", cs$github[i], dec), type = "message")
      else shiny::showNotification(paste("Not saved:", res), type = "error", duration = NULL)
      bump()
    })

    output$notices <- shiny::renderUI({ tick(); shiny::req(input$item)
      n <- late_notices(input$item)
      if (!nrow(n)) return(shiny::p(class = "text-muted", "Nothing to post for this homework."))
      shiny::tagList(
        shiny::p(sprintf("%d student(s) will get a comment on the Canvas assignment%s.", nrow(n),
                         if (any(n$over)) sprintf(", and %d a grade of 0 (over the allowance)", sum(n$over)) else "")),
        shiny::tags$details(shiny::tags$summary("Preview"),
          lapply(seq_len(nrow(n)), function(i) shiny::div(class = "notice-preview",
            shiny::strong(n$username[i]), " ", shiny::span(class = "text-muted", n$name[i] %||% ""),
            if (n$over[i]) shiny::span(class = "badge bg-danger ms-1", "grade → 0"),
            shiny::tags$pre(n$comment[i])))),
        shiny::actionButton(ns("post"), sprintf("Post %d late notice(s) to Canvas", nrow(n)), class = "btn-primary mt-2"))
    })
    shiny::observeEvent(input$post, {
      res <- tryCatch(post_late_notices(input$item, user), error = function(e) e)
      if (inherits(res, "error")) shiny::showNotification(conditionMessage(res), type = "error", duration = NULL)
      else shiny::showNotification(sprintf("Posted %d notice(s).", res), type = "message")
      bump()
    })

    # --- extensions & excused ---
    shiny::observe({ refresh()
      cal <- read_calendar(); cal <- cal[cal$type %in% c("homework", "section"), ]
      cal <- cal[order(cal$week, decreasing = TRUE), ]
      qz <- quiz_catalog(); qz <- qz[order(qz$item, decreasing = TRUE), ]
      shiny::updateSelectInput(session, "x_item", choices = item_choices(c(cal$item, qz$item), c(cal$title, qz$title), "No items in calendar.csv"))
      st <- read_students(); st <- st[!is.na(st$github) & st$github != "", ]
      shiny::updateSelectizeInput(session, "x_student", server = TRUE,
        choices = stats::setNames(st$github, sprintf("%s (%s)", st$name, st$github)), selected = character(0))
    })
    shiny::observeEvent(input$x_add, {
      shiny::req(input$x_item, input$x_student)
      due <- if (input$x_type == "extension") parse_when(paste(format(input$x_date), input$x_time)) else NA
      if (input$x_type == "extension" && is.na(due)) {
        shiny::showNotification("Time must look like 23:59 or 17:00.", type = "warning"); return() }
      res <- tryCatch({ add_exception(input$x_item, input$x_student, input$x_type, due, input$x_note, user); NULL },
                      error = function(e) conditionMessage(e))
      if (is.null(res)) shiny::showNotification(sprintf("Saved: %s, %s for %s.", input$x_student, input$x_type, input$x_item), type = "message")
      else shiny::showNotification(paste("Not saved:", res), type = "error", duration = NULL)
      shiny::updateTextInput(session, "x_note", value = ""); bump()
    })
    output$x_list <- shiny::renderUI({ tick(); refresh()
      e <- read_exceptions()
      if (!nrow(e)) return(shiny::p(class = "text-muted small", "None yet."))
      st <- read_students(); cal <- read_calendar()
      e <- e[order(-match(e$item, cal$item)), ]
      todo <- sum(e$type == "excused" & !e$canvas_excused)
      shiny::tagList(
        shiny::tags$table(class = "table table-sm align-middle small",
          shiny::tags$thead(shiny::tags$tr(lapply(c("Item", "Student", "Type", "New deadline", "Note", "Canvas", "Added", ""), shiny::tags$th))),
          shiny::tags$tbody(lapply(seq_len(nrow(e)), function(i) shiny::tags$tr(
            shiny::tags$td(e$item[i]),
            shiny::tags$td(sprintf("%s (%s)", st$name[match(tolower(e$github[i]), tolower(st$github))] %||% "", e$github[i])),
            shiny::tags$td(e$type[i]), shiny::tags$td(if (is.na(e$new_due[i])) "" else fmt_due(e$new_due[i])),
            shiny::tags$td(e$note[i] %||% ""),
            shiny::tags$td(if (e$type[i] != "excused") "" else if (e$canvas_excused[i]) "marked excused" else shiny::span(class = "text-danger", "not yet")),
            shiny::tags$td(paste(e$added_by[i] %||% "", e$added_at[i] %||% "")),
            shiny::tags$td(shiny::tags$button(class = "btn btn-outline-secondary btn-xs", type = "button",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("x_remove"), paste(e$item[i], e$github[i], sep = "|")),
              "Remove")))))),
        if (todo) shiny::actionButton(ns("x_canvas"), sprintf("Mark %d excused student(s) on Canvas", todo), class = "btn-primary btn-sm"))
    })
    shiny::observeEvent(input$x_remove, {
      p <- strsplit(input$x_remove, "|", fixed = TRUE)[[1]]
      res <- tryCatch({ remove_exception(p[1], p[2], user); NULL }, error = function(e) conditionMessage(e))
      if (is.null(res)) shiny::showNotification("Removed.", type = "message") else shiny::showNotification(res, type = "error", duration = NULL)
      bump()
    })
    shiny::observeEvent(input$x_canvas, {
      res <- tryCatch(post_excused(user), error = function(e) e)
      if (inherits(res, "error")) shiny::showNotification(conditionMessage(res), type = "error", duration = NULL)
      else shiny::showNotification(sprintf("Marked %d as excused on Canvas.", res), type = "message")
      bump()
    })

    output$totals <- DT::renderDT({ tick()
      led <- read_ledger(); if (!nrow(led)) return(NULL)
      tot <- late_totals(Inf, led)
      by_week <- stats::aggregate(minutes_late ~ username, data = led[!is.na(led$minutes_late) & led$minutes_late > 0, ],
                                  FUN = function(x) length(x))
      tot$late_homeworks <- by_week$minutes_late[match(tot$username, by_week$username)]
      tot$total <- fmt_minutes(tot$total_minutes)
      tot$remaining <- fmt_minutes(pmax(0, allowance_minutes() - tot$total_minutes))
      tot <- tot[order(-tot$total_minutes), ]
      DT::datatable(tot[tot$total_minutes > 0, c("username", "late_homeworks", "total", "remaining")], rownames = FALSE,
                    options = list(dom = "ftp", pageLength = 15))
    })
  })
}
