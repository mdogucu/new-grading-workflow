# ---------------------------------------------------------------------------
# tab-calendar.R — what's due when (students and graders)
# Data: admin/config/calendar.csv (admins) or course-info/calendar.csv (CAs).
# ---------------------------------------------------------------------------

calendar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    shiny::h2("Calendar"),
    shiny::p(class = "text-muted", "Student deadlines and grading deadlines for every item. Times are Eastern."),
    DT::DTOutput(ns("cal")))
}

calendar_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$cal <- DT::renderDT({
      cal <- read_calendar()
      if (is.null(cal)) return(NULL)
      show <- data.frame(Week = cal$week, Item = cal$item, Type = cal$type, Title = cal$title,
                         Repo = cal$repo,
                         `Students submit by` = vapply(cal$student_due, fmt_due, ""),
                         `Grading due` = vapply(cal$grading_due, fmt_due, ""),
                         `Made by` = paste(cal$assignment_owner %||% "", vapply(cal$assignment_ready_by, fmt_due, "")),
                         `Rubric by` = paste(cal$rubric_owner %||% "", vapply(cal$rubric_ready_by, fmt_due, "")),
                         check.names = FALSE)
      show$`Made by` <- trimws(gsub("^NA ?", "", show$`Made by`)); show$`Rubric by` <- trimws(gsub("^NA ?", "", show$`Rubric by`))
      DT::datatable(show, rownames = FALSE, options = list(dom = "ft", pageLength = 100))
    })
  })
}
