# ---------------------------------------------------------------------------
# tab-quizzes.R — admin: Poll Everywhere quiz → Canvas. Logic: R/15-quizzes.R
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
      posted <- tolower(unique(quiz_posted()$item))
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
      p <- quiz_posted(); p <- p[p$item == it, ]
      n_post <- sum(r$matched$status == "post"); n_exc <- sum(r$matched$status == "excused")
      shiny::tagList(
        bslib::layout_column_wrap(width = 1/4, class = "stat-row",
          stat_tile("Matched students", sum(r$matched$status != "excused")),
          stat_tile("Not matched", nrow(r$unmatched), warn = nrow(r$unmatched) > 0),
          stat_tile("Not in the file", nrow(r$absent)),
          stat_tile("Canvas assignment", if (is.na(cid)) "unknown" else cid, small = TRUE)),
        if (nrow(p)) shiny::div(class = "alert alert-info", sprintf("Already posted %s by %s (%d students). Posting again overwrites.", p$posted_at[1], p$posted_by[1], nrow(p))),
        if (nrow(r$unmatched)) bslib::card(bslib::card_header("Who is this? (Poll Everywhere rows that didn't match a student)"),
          shiny::p(class = "small text-muted", "Unregistered people show the screen name they typed (or guestNNN) and everything they answered,",
                   "in case they wrote their name or email in an answer. A best guess is pre-selected when there is one; check it, then Save match."),
          shiny::tags$table(class = "table table-sm align-middle", shiny::tags$tbody(lapply(seq_len(nrow(r$unmatched)), function(i) {
            u <- r$unmatched[i, ]
            st <- read_students()
            who <- trimws(paste(u$first, u$last))
            shiny::tags$tr(
              shiny::tags$td(shiny::strong(if (nzchar(u$screen)) u$screen else who),
                if (nzchar(u$screen) && who != u$screen) shiny::div(class = "small text-muted", who),
                if (nzchar(u$email)) shiny::div(class = "small", u$email)),
              shiny::tags$td(class = "small", style = "max-width: 420px;",
                if (nzchar(u$answers)) shiny::tags$details(shiny::tags$summary("Their answers"),
                  shiny::tags$pre(class = "small", style = "white-space: pre-wrap;", u$answers))),
              shiny::tags$td(u$score),
              shiny::tags$td(shiny::selectizeInput(ns(paste0("m_", i)), NULL, width = "320px",
                choices = c("Pick the student..." = "", stats::setNames(st$huid, paste0(st$name, " (", st$github %||% "", ")"))),
                selected = if (!is.na(u$guess)) u$guess else "")),
              shiny::tags$td(shiny::tags$button(class = "btn btn-outline-primary btn-xs", type = "button",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("match"), i), "Save match")))
          }))),
          shiny::p(class = "small text-muted", "A typed screen name is remembered for future quizzes; a guestNNN match is for this quiz only. Rows you leave unmatched are not posted.")),
        if (is.na(cid)) shiny::div(class = "alert alert-warning",
          "Couldn't find this quiz's Canvas assignment in the Canvas export or calendar. Type its id (the number in the Canvas assignment's web address) below, or download a fresh Canvas gradebook."),
        shiny::textInput(ns("cid"), "Canvas assignment id", value = if (is.na(cid)) "" else cid, width = "220px"),
        shiny::div(class = "d-flex gap-3 align-items-center my-3 flex-wrap",
          shiny::actionButton(ns("post"), sprintf("Post %d score(s)%s to Canvas", n_post, if (n_exc) sprintf(" and %d excused", n_exc) else ""),
                              class = "btn-primary"),
          shiny::checkboxInput(ns("zero"), sprintf("Also give 0 to the %d student(s) not in the file", nrow(r$absent)), FALSE),
          if (!nrow(p)) shiny::actionLink(ns("mark"), "Already posted another way — mark as done", class = "small")),
        bslib::card(bslib::card_header("Scores to post"),
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
    shiny::observeEvent(input$mark, { mark_quiz_posted(item(), user); tick(tick() + 1) })
    shiny::observeEvent(input$post, {
      r <- res(); it <- item(); cid <- trimws(input$cid %||% "")
      out <- tryCatch(post_quiz(it, cid, r$matched, if (isTRUE(input$zero)) r$absent else NULL, user), error = function(e) e)
      if (inherits(out, "error")) shiny::showNotification(paste("Not posted:", conditionMessage(out)), type = "error", duration = NULL)
      else shiny::showNotification(sprintf("Posted %d to Canvas.", out), type = "message")
      tick(tick() + 1)
    })
  })
}
