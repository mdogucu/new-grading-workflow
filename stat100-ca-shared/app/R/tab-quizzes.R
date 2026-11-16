# ---------------------------------------------------------------------------
# tab-quizzes.R — admin: match Poll Everywhere rows to students. Logic: R/15-quizzes.R
# Posting to Canvas is on the Post to Canvas tab (R/tab-canvas.R).
# ---------------------------------------------------------------------------

quizzes_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    shiny::div(class = "page-head", shiny::h2("Quizzes"),
      shiny::div(style = "min-width: 340px;", shiny::selectInput(ns("file"), NULL, choices = NULL, width = "100%"))),
    shiny::p(class = "text-muted small",
      "Put the Poll Everywhere gradebook download in admin/inputs/pollev/ (keep its name, e.g. quiz11b_gradebook_1773914.csv), then pick it here.",
      "Score = Participation. Students are matched by email, then name."),
    shiny::uiOutput(ns("body")))
}

quizzes_server <- function(id, user, refresh) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tick <- shiny::reactiveVal(0)
    shiny::observe({ refresh(); tick()
      f <- list.files(pollev_dir(), pattern = "\\.csv$", full.names = TRUE)
      f <- f[order(basename(f), decreasing = TRUE)]
      posted <- tolower(unique(read_canvas_log()$item))
      lab <- paste0(basename(f), ifelse(tolower(quiz_name_from_file(f)) %in% posted, "  ✓ posted", ""))
      cur <- shiny::isolate(input$file)
      shiny::updateSelectInput(session, "file", choices = if (length(f)) stats::setNames(f, lab) else c("No files in admin/inputs/pollev/" = ""),
                               selected = if (!is.null(cur) && cur %in% f) cur else f[1])
    })
    item <- shiny::reactive({ shiny::req(input$file); tolower(quiz_name_from_file(input$file)) })
    canvas_id_for <- function() { q <- quiz_catalog(); q$canvas_id[match(item(), q$item)] }
    res <- shiny::reactive({ tick(); shiny::req(input$file); quiz_scores(input$file, item()) })

    output$body <- shiny::renderUI({
      r <- res(); it <- item()
      cid <- canvas_id_for()
      shiny::tagList(
        bslib::layout_column_wrap(width = 1/4, class = "stat-row",
          stat_tile("Matched students", sum(r$matched$status != "excused")),
          stat_tile("Not matched", nrow(r$unmatched), warn = nrow(r$unmatched) > 0),
          stat_tile("Not in the file", nrow(r$absent)),
          stat_tile("Canvas assignment", if (is.na(cid)) "unknown" else cid, small = TRUE)),
        if (nrow(r$unmatched)) bslib::card(bslib::card_header("Who is this? (Poll Everywhere rows that didn't match a student)"),
          shiny::p(class = "small text-muted", "Unregistered people show the screen name they typed (or guestNNN) and everything they answered,",
                   "in case they wrote their name or email in an answer. A suggestion is filled in when there is one, with the reason.",
                   "Nothing is matched until you check it against their answers and click Confirm."),
          shiny::tags$table(class = "table table-sm align-middle", shiny::tags$tbody(lapply(seq_len(nrow(r$unmatched)), function(i) {
            u <- r$unmatched[i, ]
            st <- read_students()
            who <- trimws(paste(u$first, u$last))
            shiny::tags$tr(
              shiny::tags$td(shiny::strong(if (nzchar(u$screen)) u$screen else who),
                if (nzchar(u$screen) && who != u$screen) shiny::div(class = "small text-muted", who),
                if (nzchar(u$email)) shiny::div(class = "small", u$email)),
              shiny::tags$td(class = "small", style = "max-width: 420px;",
                if (nzchar(u$answers)) shiny::tags$details(open = if (!is.na(u$guess)) NA else NULL, shiny::tags$summary("Their answers"),
                  shiny::tags$pre(class = "small", style = "white-space: pre-wrap;", u$answers))),
              shiny::tags$td(u$score),
              shiny::tags$td(shiny::selectizeInput(ns(paste0("m_", i)), NULL, width = "320px",
                choices = c("Pick the student..." = "", stats::setNames(st$huid, paste0(st$name, " (", st$github %||% "", ")"))),
                selected = if (!is.na(u$guess)) u$guess else ""),
                if (!is.na(u$guess)) shiny::div(class = "small text-warning-emphasis", icon_tag("lightbulb"), " Suggested: ", u$guess_why, ". Please check.")),
              shiny::tags$td(shiny::tags$button(class = "btn btn-outline-primary btn-xs", type = "button",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("match"), i), "Confirm")))
          }))),
          shiny::p(class = "small text-muted", "A typed screen name is remembered for future quizzes; a guestNNN match is for this quiz only. Rows you leave unmatched are not posted.")),
        shiny::div(class = "alert alert-info small", icon_tag("send"),
          " When everyone is matched, post the scores on the ", shiny::strong("Post to Canvas"), " tab (pick ", it, ")."),
        bslib::card(bslib::card_header("Matched scores"),
          DT::renderDT(DT::datatable(r$matched[, c("name", "github", "score", "status")], rownames = FALSE,
                                     options = list(dom = "ftp", pageLength = 15)))),
        shiny::tags$details(shiny::tags$summary(sprintf("Students not in the file (%d)", nrow(r$absent))),
          shiny::p(class = "small", paste(r$absent$name, collapse = "; "))))
    })

    shiny::observeEvent(input$match, {
      r <- res(); u <- r$unmatched[input$match, ]; h <- input[[paste0("m_", input$match)]]
      shiny::req(nzchar(h %||% ""))
      save_pollev_match(u$email, u$name, h, user); tick(tick() + 1)
    })
  })
}
