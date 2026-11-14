# ---------------------------------------------------------------------------
# tab-tasks.R — "My tasks": the first thing everyone sees
# The list itself comes from tasks_for() in 05-tasks.R.
# ---------------------------------------------------------------------------

tasks_ui <- function(id, user) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-narrow",
    shiny::div(class = "page-head",
      shiny::h2(sprintf("Hi %s", strsplit(user$name %||% "there", " ")[[1]][1])),
      shiny::p(class = "text-muted", sprintf("%s · %s", role_label(user), format(Sys.Date(), "%A, %B %d"))),
      shiny::actionButton(ns("reload"), "Check again", icon = icon_tag("arrow-clockwise"),
                          class = "btn-outline-secondary btn-sm")),
    shiny::uiOutput(ns("list"))
  )
}

tasks_server <- function(id, user, go_to) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reload <- shiny::reactiveVal(0)
    tl <- shiny::reactive({ input$reload; reload(); tasks_for(user) })
    output$list <- shiny::renderUI({
      t <- tl()
      hidden <- attr(t, "hidden") %||% 0
      shiny::tagList(
        if (hidden > 0) shiny::p(class = "small text-muted", sprintf("%d item(s) marked as done.", hidden)),
        lapply(seq_len(nrow(t)), function(i) {
        r <- t[i, ]
        shiny::div(class = paste("task-card", paste0("task-", r$level)),
          shiny::div(class = "task-main",
            status_pill(r$level),
            shiny::div(shiny::div(class = "task-title", r$title),
                       if (nzchar(r$detail)) shiny::div(class = "task-detail", r$detail))),
          shiny::div(class = "task-side",
            if (!is.na(r$due)) shiny::div(class = "task-due", icon_tag("clock"), " ", fmt_due(r$due)),
            if (nzchar(r$tab)) shiny::tags$a(href = "#", class = "task-open",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("goto"), r$tab),
              "Open ", icon_tag("arrow-right")),
            if (nzchar(r$id %||% "")) shiny::tags$a(href = "#", class = "task-dismiss",
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;", ns("dismiss"),
                                jsonlite::toJSON(r$id, auto_unbox = TRUE)),
              title = "Done (or doesn't apply). It comes back only if it changes.", icon_tag("check2"), " Mark as done"))
        )
      }))
    })
    shiny::observeEvent(input$dismiss, { dismiss_task(user, input$dismiss); reload(reload() + 1) })
    shiny::observeEvent(input$goto, go_to(input$goto))
  })
}
