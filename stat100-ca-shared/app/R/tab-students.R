# ---------------------------------------------------------------------------
# tab-students.R — admin only: the student list, its problems, and GitHub
#
#   Needs attention  problems found when building the list, with buttons:
#                    "Doesn't need GitHub" (tutors etc.) and "Dropped"
#   Everyone         the whole list; click a row for the same buttons
#   GitHub org       compare the GitHub organization with the list:
#                    people who may have dropped (remove them) and students
#                    who haven't joined (email them)
#   Dropped / no GitHub   students you've set aside (undo here)
#
# Data: admin/data/students.csv. Statuses and fixes are kept in
# admin/config/student-overrides.csv (huid, github, section, status, note).
# ---------------------------------------------------------------------------

students_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    shiny::h2("Students"),
    shiny::uiOutput(ns("summary")),
    bslib::navset_underline(id = ns("tabs"),
      bslib::nav_panel("Needs attention", shiny::br(), shiny::uiOutput(ns("issues"))),
      bslib::nav_panel("Everyone", shiny::br(),
        shiny::p(class = "small text-muted", "Click a student to mark them as dropped or as not needing GitHub."),
        DT::DTOutput(ns("all"))),
      bslib::nav_panel("GitHub organization", shiny::br(), shiny::uiOutput(ns("org"))),
      bslib::nav_panel("Dropped / no GitHub", shiny::br(), shiny::uiOutput(ns("setaside")))
    )
  )
}

students_server <- function(id, user, refresh) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tick <- shiny::reactiveVal(0)
    bump <- function() { tick(tick() + 1); refresh(refresh() + 1) }
    st <- shiny::reactive({ refresh(); tick(); read_students() })
    act <- function(label, expr) {
      res <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
      if (is.null(res)) shiny::showNotification(label, type = "message")
      else shiny::showNotification(paste("Didn't work:", res), type = "error", duration = NULL)
      bump()
    }

    output$summary <- shiny::renderUI({
      s <- st()
      if (is.null(s)) return(empty_state("No student list yet. Go to Admin > Refresh student list."))
      built <- format(file.mtime(admin_path("data", "students.csv")), "%b %d, %l:%M %p")
      bslib::layout_column_wrap(width = 1/4, class = "stat-row",
        stat_tile("Students", nrow(s)),
        stat_tile("With GitHub username", sum(!is.na(s$github))),
        stat_tile("Need attention", sum(s$issue != ""), warn = sum(s$issue != "") > 0),
        stat_tile("List built", built, small = TRUE))
    })

    # Two buttons used everywhere: they send the student's HUID to the server.
    row_buttons <- function(huid, github) shiny::span(class = "row-actions",
      shiny::tags$button(class = "btn btn-outline-secondary btn-xs", type = "button",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("nogh"), huid), "Doesn't need GitHub"),
      shiny::tags$button(class = "btn btn-outline-danger btn-xs", type = "button",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("drop"), huid), "Dropped…"))

    output$issues <- shiny::renderUI({
      s <- st(); if (is.null(s)) return(NULL)
      bad <- s[s$issue != "", ]
      if (!nrow(bad)) return(empty_state("No problems found."))
      groups <- split(bad, bad$issue)
      shiny::tagList(lapply(names(groups), function(g) {
        d <- groups[[g]]
        tmpl <- switch(g, "No GitHub username" = "github-no-username", NULL)
        bslib::card(
          bslib::card_header(shiny::span(g), shiny::span(class = "badge bg-secondary ms-2", nrow(d))),
          shiny::tags$table(class = "table table-sm align-middle",
            shiny::tags$thead(shiny::tags$tr(lapply(c("Name", "Email", "Section", "GitHub", "Canvas section", "Roster section", ""), shiny::tags$th))),
            shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i) shiny::tags$tr(
              shiny::tags$td(d$name[i]), shiny::tags$td(d$email[i] %||% ""), shiny::tags$td(d$section[i] %||% ""),
              shiny::tags$td(d$github[i] %||% ""), shiny::tags$td(d$canvas_section[i] %||% ""),
              shiny::tags$td(d$roster_section[i] %||% ""), shiny::tags$td(row_buttons(d$huid[i], d$github[i])))))),
          if (!is.null(tmpl)) shiny::tags$details(class = "mt-2",
            shiny::tags$summary(icon_tag("envelope"), " Email these students"),
            email_panel_ui(ns(paste0("mail_", tmpl)), stats::na.omit(d$email),
                           fill_template(tmpl, list(sender = (user$viewed_by %||% user)$name)))),
          if (is.null(tmpl)) shiny::tags$small(class = "text-muted",
            "To fix a username or section by hand: admin/config/student-overrides.csv (huid, github, section), then Refresh student list.")
        )
      }))
    })

    output$all <- DT::renderDT({
      s <- st(); if (is.null(s)) return(NULL)
      DT::datatable(s[, c("name", "email", "section", "github", "status", "issue", "huid", "canvas_id")],
                    rownames = FALSE, filter = "top", selection = "single",
                    options = list(pageLength = 25, dom = "ftip"))
    })
    shiny::observeEvent(input$all_rows_selected, {
      s <- st(); r <- s[input$all_rows_selected, ]
      shiny::showModal(shiny::modalDialog(title = r$name, easyClose = TRUE,
        shiny::p(sprintf("GitHub: %s · Section: %s · %s", r$github %||% "—", r$section %||% "—", r$email %||% "")),
        row_buttons(r$huid, r$github), footer = shiny::modalButton("Close")))
    })

    # "Doesn't need GitHub"
    shiny::observeEvent(input$nogh, {
      shiny::removeModal()
      act("Saved: doesn't need GitHub.", set_student_status(input$nogh, "no github needed", "", user))
    })
    # "Dropped…" asks first.
    shiny::observeEvent(input$drop, {
      s <- st(); r <- s[s$huid == input$drop, ][1, ]
      shiny::showModal(shiny::modalDialog(title = paste("Mark", r$name, "as dropped?"),
        shiny::p("They'll be taken off the student list: no repos, no grading, no reminders. You can undo this on the \"Dropped / no GitHub\" tab."),
        if (!is.na(r$github) && nzchar(r$github))
          shiny::checkboxInput(ns("drop_org"), sprintf("Also remove %s from the GitHub organization", r$github), TRUE),
        shiny::textInput(ns("drop_note"), "Note (optional)", placeholder = "e.g. dropped on Nov 10"),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("drop_ok"), "Mark as dropped", class = "btn-danger"))))
      pending_drop(r)
    })
    pending_drop <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$drop_ok, {
      r <- pending_drop(); shiny::removeModal()
      msg <- character()
      if (isTRUE(input$drop_org) && !is.na(r$github)) {
        e <- tryCatch({ remove_from_org(r$github, user); NULL }, error = function(e) conditionMessage(e))
        msg <- if (is.null(e)) "removed from GitHub" else paste("GitHub removal failed:", e)
      }
      act(paste(c(paste(r$name, "marked as dropped."), msg), collapse = " "),
          set_student_status(r$huid, "dropped", input$drop_note %||% "", user))
    })

    # --- GitHub organization ---
    output$org <- shiny::renderUI({ tick()
      g <- read_github_check()
      shiny::tagList(
        shiny::div(class = "d-flex gap-3 align-items-center mb-3",
          shiny::actionButton(ns("org_check"), "Check GitHub organization now", icon = icon_tag("github"), class = "btn-primary"),
          shiny::span(class = "text-muted small", if (is.null(g)) "Not checked yet." else paste("Last checked", g$checked_at[1]))),
        if (!is.null(g)) {
          gone <- g[!g$on_list, ]; notjoined <- g[g$on_list & g$in_org != "member", ]
          shiny::tagList(
            bslib::card(bslib::card_header(sprintf("In the GitHub organization but not on the student list (%d)", nrow(gone)),
                                           shiny::tags$small(class = "text-muted ms-2", "probably dropped — or the Canvas export is old")),
              if (!nrow(gone)) shiny::p(class = "text-muted", "None.") else
                shiny::tags$table(class = "table table-sm align-middle", shiny::tags$tbody(lapply(seq_len(nrow(gone)), function(i)
                  shiny::tags$tr(shiny::tags$td(shiny::tags$a(href = paste0("https://github.com/", gone$login[i]), target = "_blank", gone$login[i])),
                                 shiny::tags$td(gone$in_org[i]),
                                 shiny::tags$td(shiny::tags$button(class = "btn btn-outline-danger btn-xs", type = "button",
                                   onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("org_remove"), gone$login[i]),
                                   "Remove from organization"))))))),
            bslib::card(bslib::card_header(sprintf("On the student list but not a member yet (%d)", nrow(notjoined))),
              if (!nrow(notjoined)) shiny::p(class = "text-muted", "Everyone has joined.") else shiny::tagList(
                shiny::tags$table(class = "table table-sm", shiny::tags$tbody(lapply(seq_len(nrow(notjoined)), function(i)
                  shiny::tags$tr(shiny::tags$td(notjoined$login[i]), shiny::tags$td(notjoined$name[i] %||% ""),
                                 shiny::tags$td(if (notjoined$in_org[i] == "invited") "invited, not accepted" else "not invited"))))),
                shiny::tags$details(shiny::tags$summary(icon_tag("envelope"), " Email them"),
                  email_panel_ui(ns("mail_notjoined"), stats::na.omit(notjoined$email),
                                 fill_template("github-not-joined", list(sender = (user$viewed_by %||% user)$name)))))))
        })
    })
    shiny::observeEvent(input$org_check, shiny::withProgress(message = "Asking GitHub…", value = 0.5,
      act("GitHub organization checked.", check_github_org(user))))
    shiny::observeEvent(input$org_remove, {
      shiny::showModal(shiny::modalDialog(title = paste("Remove", input$org_remove, "from the GitHub organization?"),
        shiny::p("They lose access to the course repos. Their repos are not deleted."),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("org_remove_ok"), "Remove", class = "btn-danger"))))
    })
    shiny::observeEvent(input$org_remove_ok, { shiny::removeModal()
      act(paste(input$org_remove, "removed from the organization."), remove_from_org(input$org_remove, user)) })

    # --- set aside ---
    output$setaside <- shiny::renderUI({ tick(); refresh()
      ov <- read_overrides(); ov <- ov[ov$status %in% c("dropped", "no github needed"), ]
      if (!nrow(ov)) return(empty_state("Nobody set aside."))
      cv <- tryCatch(read_canvas_gradebook(newest_file(admin_path("inputs", "canvas-gradebook"))), error = function(e) NULL)
      nm <- if (!is.null(cv)) cv$canvas_name[match(ov$huid, cv$huid)] else rep(NA, nrow(ov))
      shiny::tags$table(class = "table table-sm align-middle",
        shiny::tags$thead(shiny::tags$tr(lapply(c("Name", "HUID", "Status", "Note", "Changed", ""), shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(ov)), function(i) shiny::tags$tr(
          shiny::tags$td(nm[i] %||% ""), shiny::tags$td(ov$huid[i]), shiny::tags$td(ov$status[i]),
          shiny::tags$td(ov$note[i] %||% ""), shiny::tags$td(paste(ov$changed_by[i] %||% "", ov$changed_at[i] %||% "")),
          shiny::tags$td(shiny::tags$button(class = "btn btn-outline-secondary btn-xs", type = "button",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("undo"), ov$huid[i]), "Undo"))))))
    })
    shiny::observeEvent(input$undo, act("Undone.", set_student_status(input$undo, "", "", user)))
  })
}

stat_tile <- function(label, value, warn = FALSE, small = FALSE) {
  shiny::div(class = paste("stat-tile", if (warn) "stat-warn"),
    shiny::div(class = if (small) "stat-value stat-small" else "stat-value", value),
    shiny::div(class = "stat-label", label))
}
