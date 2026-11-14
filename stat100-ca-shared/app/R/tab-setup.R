# ---------------------------------------------------------------------------
# tab-setup.R — "is this computer ready?" checklist, for everyone
# Each check returns ok / problem + how to fix. Instructions live in
# stat100-ca-shared/docs/.
# ---------------------------------------------------------------------------

setup_checks <- function(user) {
  chk <- function(name, ok, fix, who = "everyone") data.frame(check = name, ok = isTRUE(ok), fix = fix, who = who)
  git <- nzchar(Sys.which("git"))
  rows <- list(
    chk("R packages installed", all(vapply(needed_packages, requireNamespace, logical(1), quietly = TRUE)),
        "Run the install.packages() line in docs/setup-computer.md."),
    chk("Git installed", git, "See docs/setup-computer.md, step 2."),
    chk(if (user$login_source == "STAT100_DEV_USER") "GitHub token (skipped: STAT100_DEV_USER test mode is on)"
        else "GitHub token saved on this computer", is.null(user$login_error),
        paste("See docs/setup-github-token.md.", user$login_error %||% "")),
    chk(sprintf("GitHub user '%s' is on the staff list", user$username %||% "?"),
        user$role %in% c("admin", "ca", "admin-no-folder"), "Ask Mine or Julie to add you to staff.csv."),
    chk("Shared folder found", dir.exists(shared_dir()), shared_dir())
  )
  if (user$role %in% c("ca")) rows <- c(rows, list(
    chk("Your personal grading folder found", !is.na(grader_dir(user$username)),
        sprintf("Add a shortcut to '%s' in My Drive (docs/setup-computer.md, step 4).", grader_folder_name(user$username)))))
  if (user$role %in% c("admin", "admin-no-folder")) rows <- c(rows, list(
    chk("Admin folder found", has_admin_folder(), paste("Expected at", admin_dir()), "admin"),
    chk("Canvas token found (CANVAS_API_TOKEN or CANVAS_API_KEY in ~/.Renviron)", nzchar(canvas_token()),
        "See docs/setup-canvas-token.md.", "admin")))
  do.call(rbind, rows)
}

setup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-narrow",
    shiny::div(class = "page-head", shiny::h2("Setup check"),
      shiny::actionButton(ns("recheck"), "Check again", icon = icon_tag("arrow-clockwise"), class = "btn-outline-secondary btn-sm")),
    shiny::p(class = "text-muted", "Everything here should be green before you start grading.",
             "Step-by-step instructions are in the docs/ folder of stat100-ca-shared."),
    shiny::uiOutput(ns("checks")),
    shiny::h4(class = "mt-4", "Where the app is looking"),
    shiny::uiOutput(ns("paths")))
}

setup_server <- function(id, user) {
  shiny::moduleServer(id, function(input, output, session) {
    output$checks <- shiny::renderUI({
      input$recheck
      if (isTRUE(input$recheck > 0)) .identity_cache$login <- NULL
      u <- if (isTRUE(input$recheck > 0)) current_user() else user
      ch <- setup_checks(u)
      shiny::tags$ul(class = "checklist", lapply(seq_len(nrow(ch)), function(i) shiny::tags$li(
        class = if (ch$ok[i]) "ok" else "bad",
        icon_tag(if (ch$ok[i]) "check-circle-fill" else "x-circle-fill"), " ", shiny::strong(ch$check[i]),
        if (!ch$ok[i]) shiny::div(class = "fix", ch$fix[i]))))
    })
    output$paths <- shiny::renderUI({
      shiny::tags$table(class = "table table-sm mono small",
        shiny::tags$tr(shiny::tags$td("App"), shiny::tags$td(app_dir())),
        shiny::tags$tr(shiny::tags$td("Shared folder"), shiny::tags$td(shared_dir())),
        shiny::tags$tr(shiny::tags$td("My folder"), shiny::tags$td(grader_dir(user$username %||% "") %||% "not found")),
        shiny::tags$tr(shiny::tags$td("Admin folder"), shiny::tags$td(if (has_admin_folder()) admin_dir() else "not available")),
        shiny::tags$tr(shiny::tags$td("Signed in as"), shiny::tags$td(sprintf("%s (from %s)", user$username %||% "?", user$login_source))))
    })
  })
}
