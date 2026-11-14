# ---------------------------------------------------------------------------
# tab-rubrics.R — admin: make or edit a rubric without touching the CSV
# 1. Pick the homework. 2. "Load questions from the homework" reads them from
# the week's template. 3. Set points per question, add options. 4. Check, Save.
# Logic: R/13-rubrics.R.
# ---------------------------------------------------------------------------

rubrics_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    shiny::div(class = "page-head", shiny::h2("Rubrics"),
      shiny::div(style = "min-width: 300px;", shiny::selectInput(ns("item"), NULL, choices = NULL, width = "100%"))),
    shiny::uiOutput(ns("status")),
    shiny::div(class = "d-flex gap-2 flex-wrap mb-3",
      shiny::actionButton(ns("load_template"), "Load questions from the homework", icon = icon_tag("file-earmark-text"), class = "btn-outline-primary"),
      shiny::actionButton(ns("check"), "Check", icon = icon_tag("check2-square"), class = "btn-outline-primary"),
      shiny::actionButton(ns("save"), "Save rubric", icon = icon_tag("save"), class = "btn-primary")),
    bslib::layout_columns(col_widths = c(4, 8),
      bslib::card(bslib::card_header("Questions and points"),
        shiny::p(class = "small text-muted", "Double-click a number to change it."),
        shiny::uiOutput(ns("total")), DT::DTOutput(ns("questions"))),
      bslib::card(bslib::card_header("Options (what graders tick)"),
        shiny::div(class = "d-flex gap-2 align-items-end flex-wrap",
          shiny::selectInput(ns("show_q"), "Question", choices = NULL, width = "220px"),
          shiny::actionButton(ns("add"), "Add option", icon = icon_tag("plus"), class = "btn-outline-secondary btn-sm mb-3"),
          shiny::actionButton(ns("del"), "Delete selected", icon = icon_tag("trash"), class = "btn-outline-danger btn-sm mb-3")),
        shiny::p(class = "small text-muted",
          "Double-click a cell to edit. Message = what the grader sees; feedback = what the student sees; points off.",
          "For \"Every question\" options, points off is a percent of the question."),
        DT::DTOutput(ns("options"))))
  )
}

rubrics_server <- function(id, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(questions = NULL, options = NULL, expected = NULL, dirty = FALSE, problems = NULL)

    shiny::observe({
      cal <- read_calendar(); cal <- cal[cal$type == "homework", ]
      cal <- cal[order(cal$week, decreasing = TRUE), ]
      shiny::updateSelectInput(session, "item", choices = item_choices(cal$item, cal$title, "No homework in calendar.csv"))
    })

    # Load the saved rubric (or start empty) when the item changes.
    shiny::observeEvent(input$item, {
      shiny::req(input$item)
      r <- read_rubric(input$item)
      if (!is.null(r) && basename(rubric_path(input$item)) == paste0(input$item, "-rubric.csv")) {
        p <- rubric_parts(r); rv$questions <- p$questions; rv$options <- p$options
      } else {
        rv$questions <- data.frame(name = character(), total_points = numeric())
        rv$options <- default_all_questions()
      }
      rv$expected <- NULL; rv$dirty <- FALSE; rv$problems <- NULL
    })

    output$status <- shiny::renderUI({
      shiny::req(input$item); it <- item_info(input$item)
      f <- shared_path("rubrics", paste0(input$item, "-rubric.csv"))
      shiny::div(class = "mb-2",
        shiny::span(class = "text-muted", "File: ", shiny::tags$code(basename(f)), " · ",
          if (file.exists(f)) paste("saved", format(file.mtime(f), "%b %d %l:%M %p")) else "not created yet",
          " · students submit ", fmt_due(it$student_due)),
        if (rv$dirty) shiny::span(class = "badge bg-warning text-dark ms-2", "unsaved changes"),
        if (!is.null(rv$problems)) {
          if (length(rv$problems)) shiny::div(class = "alert alert-warning mt-2", shiny::strong("Check: "), shiny::tags$ul(lapply(rv$problems, shiny::tags$li)))
          else shiny::div(class = "alert alert-success mt-2", "Check passed: every question in the homework has points and options.")
        })
    })

    shiny::observeEvent(input$load_template, {
      qs <- tryCatch(template_questions(input$item), error = function(e) { shiny::showNotification(conditionMessage(e), type = "error", duration = NULL); NULL })
      shiny::req(qs)
      rv$expected <- qs
      have <- rv$questions
      new <- setdiff(qs, have$name)
      rv$questions <- rbind(have, data.frame(name = new, total_points = rep(NA_real_, length(new))))
      rv$questions <- rv$questions[order(match(question_key(rv$questions$name), question_key(qs))), ]
      if (!any(rv$options$name == "all_questions")) rv$options <- rbind(rv$options, default_all_questions())
      if (length(new)) rv$dirty <- TRUE
      shiny::showNotification(sprintf("Found %d question(s) in the homework; %d new.", length(qs), length(new)), type = "message")
    })

    output$questions <- DT::renderDT({
      q <- rv$questions; shiny::req(q)
      DT::datatable(q, rownames = FALSE, colnames = c("Question", "Points"), selection = "none",
                    editable = list(target = "cell", disable = list(columns = 0)),
                    options = list(dom = "t", pageLength = 100, ordering = FALSE))
    })
    shiny::observeEvent(input$questions_cell_edit, {
      e <- input$questions_cell_edit
      rv$questions$total_points[e$row] <- suppressWarnings(as.numeric(e$value)); rv$dirty <- TRUE
    })
    output$total <- shiny::renderUI({
      q <- rv$questions; shiny::req(q)
      shiny::p(shiny::strong(sprintf("Total: %s points", sum(q$total_points, na.rm = TRUE))),
               if (any(is.na(q$total_points))) shiny::span(class = "text-danger", sprintf(" · %d without points", sum(is.na(q$total_points)))))
    })

    shiny::observe({
      q <- rv$questions; shiny::req(q)
      ch <- c(stats::setNames(q$name, q$name), "Every question (all_questions)" = "all_questions")
      sel <- shiny::isolate(input$show_q)
      shiny::updateSelectInput(session, "show_q", choices = ch,
        selected = if (!is.null(sel) && sel %in% q$name) sel else if (nrow(q)) q$name[1] else "all_questions")
    })
    shown <- shiny::reactive({ shiny::req(input$show_q, rv$options); which(rv$options$name == input$show_q) })
    output$options <- DT::renderDT({
      o <- rv$options[shown(), c("prompt_code", "prompt_message", "feedback", "points_to_remove")]
      DT::datatable(o, rownames = FALSE, colnames = c("Code", "Message (grader sees)", "Feedback (student sees)", "Points off"),
                    editable = "cell", options = list(dom = "t", pageLength = 100, ordering = FALSE,
                    columnDefs = list(list(width = "45%", targets = 2))))
    })
    shiny::observeEvent(input$options_cell_edit, {
      e <- input$options_cell_edit; idx <- shown()[e$row]
      col <- c("prompt_code", "prompt_message", "feedback", "points_to_remove")[e$col + 1]
      rv$options[[col]][idx] <- if (col == "points_to_remove") suppressWarnings(as.numeric(e$value)) else e$value
      rv$dirty <- TRUE
    })
    shiny::observeEvent(input$add, {
      qn <- input$show_q; o <- rv$options[rv$options$name == qn, ]
      codes <- suppressWarnings(as.integer(o$prompt_code)); nxt <- if (any(!is.na(codes))) max(codes, na.rm = TRUE) + 1 else 1
      rv$options <- rbind(rv$options, data.frame(name = qn, prompt_code = as.character(nxt), prompt_message = "",
                                                 feedback = "", points_to_remove = 0))
      rv$dirty <- TRUE
    })
    shiny::observeEvent(input$del, {
      sel <- input$options_rows_selected; shiny::req(sel)
      rv$options <- rv$options[-shown()[sel], ]; rv$dirty <- TRUE
    })

    current <- function() rubric_table(rv$questions, rv$options)
    shiny::observeEvent(input$check, {
      exp <- rv$expected %||% tryCatch(template_questions(input$item), error = function(e) NULL)
      rv$problems <- check_rubric(input$item, current(), exp)
    })
    shiny::observeEvent(input$save, {
      res <- tryCatch({ save_rubric(input$item, rv$questions, rv$options, user); NULL }, error = function(e) conditionMessage(e))
      if (is.null(res)) { rv$dirty <- FALSE; shiny::showNotification("Rubric saved.", type = "message")
        rv$problems <- check_rubric(input$item, read_rubric(input$item), rv$expected) }
      else shiny::showNotification(paste("Not saved:", res), type = "error", duration = NULL)
    })
  })
}
