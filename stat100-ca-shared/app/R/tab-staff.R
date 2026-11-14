# ---------------------------------------------------------------------------
# tab-staff.R — admin only: teaching team, sections, conflicts of interest
# Edit the files in admin/config/ (staff.csv, sections.csv, coi.csv), then
# press "Reload" here and "Publish course info" on the Admin tab.
# ---------------------------------------------------------------------------

staff_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    shiny::div(class = "page-head", shiny::h2("Staff & sections"),
      shiny::actionButton(ns("reload"), "Reload", icon = icon_tag("arrow-clockwise"), class = "btn-outline-secondary btn-sm")),
    shiny::p(class = "text-muted", "Read-only view. Edit admin/config/staff.csv, sections.csv and coi.csv."),
    bslib::layout_columns(col_widths = c(7, 5),
      bslib::card(bslib::card_header("Teaching team"), DT::DTOutput(ns("staff"))),
      bslib::card(bslib::card_header("Sections"), DT::DTOutput(ns("sections")))),
    bslib::card(bslib::card_header("Grading monitor by week"),
      shiny::p(class = "small text-muted", "Each week one senior CA monitors grading and is automatically on hold from grading.",
               "Picked at random but evenly (seed = block of weeks). To change a week, add a row to admin/config/monitor-overrides.csv (week, monitor, note)."),
      DT::DTOutput(ns("rotation"))),
    bslib::card(bslib::card_header("Conflicts of interest (admins only)"), DT::DTOutput(ns("coi")))
  )
}

staff_server <- function(id, user, refresh) {
  shiny::moduleServer(id, function(input, output, session) {
    tick <- shiny::reactive({ input$reload; refresh() })
    dt <- function(d) DT::datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 50))
    output$staff <- DT::renderDT({ tick(); s <- read_staff()
      s$flags <- trimws(paste0(ifelse(s$senior, "senior · ", ""),
                               ifelse(s$monitor_this_week, "monitor this week · ", ifelse(s$monitor, "monitor · ", "")),
                               ifelse(s$on_hold, "on hold", "")), whitespace = "[ ·]")
      dt(s[, c("name", "username", "role", "section", "flags")]) })
    output$sections <- DT::renderDT({ tick(); dt(read_sections()) })
    output$coi <- DT::renderDT({ tick(); dt(read_coi()) })
    output$rotation <- DT::renderDT({ tick()
      r <- monitor_schedule(); if (is.null(r)) return(NULL)
      s <- read_staff(FALSE)
      r$name <- s$name[match(tolower(r$monitor), tolower(s$username))]
      r$`this week` <- ifelse(r$week_of <= today_et() & today_et() < r$week_of + 7, "◀ now", "")
      DT::datatable(r[, c("week", "week_of", "name", "monitor", "source", "this week", "note")], rownames = FALSE,
                    options = list(dom = "t", pageLength = 20)) |>
        DT::formatStyle("this week", target = "row", fontWeight = DT::styleEqual("◀ now", "bold"),
                        backgroundColor = DT::styleEqual("◀ now", "#FBECEE"))
    })
  })
}
