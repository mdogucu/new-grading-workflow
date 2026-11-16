# ---------------------------------------------------------------------------
# tab-progress.R — "Grading progress": for the grading monitor and admins
# Who has graded how many of their assigned students, per homework/section,
# with a reminder email for anyone behind. Data: R/12-progress.R.
# ---------------------------------------------------------------------------

progress_ui <- function(id, user) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    shiny::div(class = "page-head", shiny::h2("Grading progress"),
      shiny::actionButton(ns("reload"), "Check again", icon = icon_tag("arrow-clockwise"), class = "btn-outline-secondary btn-sm"),
      if (is_admin(user)) shiny::actionButton(ns("rebuild"), "Rebuild from grader folders", class = "btn-outline-secondary btn-sm")),
    shiny::p(class = "text-muted", "Counts update whenever a grader opens the Grading tab or saves a grade, and when an admin opens the dashboard.",
             "Section work counts each CA's whole section, even before they open it."),
    shiny::uiOutput(ns("items")))
}

progress_server <- function(id, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tick <- shiny::reactiveVal(0)
    if (is_admin(user)) shiny::observeEvent(input$rebuild, { refresh_all_progress(user); tick(tick() + 1) })
    output$items <- shiny::renderUI({
      input$reload; tick()
      p <- read_progress(); its <- progress_items(p)
      if (!length(its)) return(empty_state("No grading has been assigned yet."))
      cal <- read_calendar(); s <- read_staff(FALSE)
      shiny::tagList(lapply(its, function(it) {
        d <- p[p$item == it, ]; d <- d[order(d$graded / pmax(d$assigned, 1)), ]
        due <- cal$grading_due[match(it, cal$item)]
        late <- !is.na(due) && due < now_et()
        behind <- graders_behind(it, p)
        bslib::card(
          bslib::card_header(shiny::div(class = "d-flex justify-content-between flex-wrap gap-2",
            shiny::span(shiny::strong(it), " · ", cal$title[match(it, cal$item)] %||% ""),
            shiny::span(sprintf("%d of %d graded", sum(d$graded), sum(d$assigned)), " · due ", fmt_due(due),
                        if (late && sum(d$graded) < sum(d$assigned)) shiny::span(class = "pill pill-overdue ms-2", "Overdue")))),
          shiny::tags$table(class = "table table-sm align-middle progress-table",
            shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Grader"), shiny::tags$th("Progress"), shiny::tags$th(""), shiny::tags$th("Last saved"))),
            shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i) {
              pct <- if (d$assigned[i]) round(100 * d$graded[i] / d$assigned[i]) else 100
              lvl <- if (d$graded[i] >= d$assigned[i]) "ok" else if (late) "overdue" else if (d$graded[i] == 0) "todo" else "info"
              shiny::tags$tr(
                shiny::tags$td(s$name[match(d$grader[i], s$username)] %||% d$grader[i]),
                shiny::tags$td(style = "width: 45%;", shiny::div(class = "progress", style = "height: 12px;",
                  shiny::div(class = paste0("progress-bar bar-", lvl), style = sprintf("width:%d%%;", pct)))),
                shiny::tags$td(sprintf("%d / %d", d$graded[i], d$assigned[i]), " ",
                  shiny::span(class = paste0("pill pill-", lvl), c(ok = "Done", overdue = "Overdue", todo = "Not started", info = "In progress")[[lvl]]),
                  if (!isTRUE(d$opened[i])) shiny::span(class = "small text-muted ms-1", "· hasn't opened it yet")),
                shiny::tags$td(class = "small text-muted", d$last_saved[i] %||% ""))
            }))),
          if (is_admin(user)) shiny::div(class = "small mb-2",
            icon_tag("table"), " All grades for this item: ",
            shiny::tags$code(sprintf("admin/data/grades/%s.csv", it)), " (every question) and ",
            shiny::tags$code(sprintf("%s-totals.csv", it)), " (one row per student). Updated whenever you open the dashboard or click Rebuild."),
          if (!is.null(behind)) shiny::tags$details(shiny::tags$summary(icon_tag("envelope"), sprintf(" Remind the %d grader(s) who aren't done", nrow(behind))),
            email_panel_ui(ns(paste0("mail_", it)), stats::na.omit(behind$email),
              fill_template("grading-reminder", list(sender = user$name, item = it,
                            title = cal$title[match(it, cal$item)] %||% "", due = fmt_due(due))))))
      }))
    })
  })
}
