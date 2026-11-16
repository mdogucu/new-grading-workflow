# ===========================================================================
# STAT 100 course dashboard
#
# How to run: open this file in RStudio and click "Run App" (top right of the
# editor). Or in the R console:  shiny::runApp("<path to this app folder>")
#
# Code map (each file starts with a comment explaining it):
#   R/00-packages.R    which packages are needed
#   R/01-paths.R       where folders are
#   R/02-identity.R    who you are, what role you have
#   R/03-config.R      reading course settings (staff, sections, calendar, COI)
#   R/04-roster.R      building the student list from Canvas/Gradescope/roster
#   R/05-tasks.R       the "My tasks" list
#   R/06-templates.R   email templates
#   R/07-theme.R       colors, fonts, logo (matches stat-100.com)
#   R/08-github.R      GitHub: commits, graded version, local copies of repos
#   R/09-grading.R     weekly grading: snapshots, who grades whom, rubrics, saving
#   R/10-late.R        late homework/sections: review, 48-hour allowance, Canvas notices
#   R/11-github-org.R  GitHub organization vs. student list, removing people
#   R/12-progress.R    grading progress files for the grading monitor
#   R/13-rubrics.R     building and checking rubrics
#   R/14-exceptions.R  extensions and excused work (incl. EXCUSED on Canvas)
#   R/15-quizzes.R     Poll Everywhere quiz files -> Canvas
#   R/16-canvas-sync.R fill calendar.csv from the assignments in Canvas
#   R/17-canvas-post.R everything sent to Canvas: scores, feedback, late notes,
#                      EXCUSED, and regrade changes after posting
#   R/18-sections.R    section activities: each CA grades their own section,
#                      lists build themselves, the CA decides about late work
#   R/tab-*.R          one file per tab (UI + server)
# ===========================================================================

# Find this app's folder: the folder of app.R when it's sourced, else the
# working directory (Run App / runApp() set it to the app folder).
local({
  here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(here) || !file.exists(file.path(here, "app.R"))) here <- normalizePath(getwd())
  options(stat100.app_dir = here)
})
shiny::addResourcePath("stat100-www", file.path(getOption("stat100.app_dir"), "www"))
for (f in sort(list.files(file.path(getOption("stat100.app_dir"), "R"), pattern = "\\.R$", full.names = TRUE))) source(f, local = FALSE, encoding = "UTF-8")
check_packages()

ui <- function(req) {
  user <- user_for_request(req$QUERY_STRING)
  admin <- is_admin(user)
  tabs <- list(
    bslib::nav_panel("My tasks", value = "tasks", icon = icon_tag("list-check"), tasks_ui("tasks", user)),
    if (admin || isTRUE(user$monitor)) bslib::nav_panel("Progress", value = "progress", icon = icon_tag("bar-chart-line"), progress_ui("progress", user)),
    if (user$role %in% c("admin", "ca")) bslib::nav_panel("Grading", value = "grading", icon = icon_tag("pencil-square"), grading_ui("grading", user)),
    if (admin) bslib::nav_panel("Late & excused", value = "late", icon = icon_tag("hourglass-split"), late_ui("late")),
    if (admin) bslib::nav_panel("Quizzes", value = "quizzes", icon = icon_tag("patch-question"), quizzes_ui("quizzes")),
    if (admin) bslib::nav_panel("Post to Canvas", value = "canvas", icon = icon_tag("send"), canvas_ui("canvas")),
    if (admin) bslib::nav_panel("Rubrics", value = "rubrics", icon = icon_tag("ui-checks"), rubrics_ui("rubrics")),
    if (admin) bslib::nav_panel("Students", value = "students", icon = icon_tag("people"), students_ui("students")),
    if (admin) bslib::nav_panel("Staff", value = "staff", icon = icon_tag("person-badge"), staff_ui("staff")),
    bslib::nav_panel("Calendar", value = "calendar", icon = icon_tag("calendar3"), calendar_ui("calendar", user)),
    if (admin) bslib::nav_panel("Admin", value = "admin", icon = icon_tag("gear"), admin_ui("admin")),
    bslib::nav_spacer(),
    bslib::nav_panel("Setup", value = "setup", icon = icon_tag("tools"), setup_ui("setup")),
    if (can_view_as(user) || !is.null(user$viewed_by)) view_as_menu(user),
    bslib::nav_item(user_badge(user))
  )
  tabs <- Filter(Negate(is.null), tabs)
  do.call(bslib::page_navbar, c(
    list(id = "nav", title = navbar_title(), theme = stat100_theme(), window_title = "STAT 100 dashboard",
         header = shiny::tagList(stat100_head(), view_as_banner(user)), fillable = FALSE),
    navbar_bg("#F5F5F5"),
    tabs))
}

# Newer bslib (0.9+) wants navbar colors in navbar_options(); older wants bg =.
navbar_bg <- function(col) {
  if (utils::packageVersion("bslib") >= "0.9.0") list(navbar_options = bslib::navbar_options(bg = col))
  else list(bg = col)
}

server <- function(input, output, session) {
  user <- user_for_request(shiny::isolate(session$clientData$url_search))
  go_to <- function(tab) bslib::nav_select("nav", tab, session = session)
  # When an admin opens the dashboard (also in "View as"), rebuild every
  # grader's progress file from the grader folders, so the Progress tab is
  # complete even for graders who haven't opened the dashboard yet.
  real <- user$viewed_by %||% user
  if (is_admin(real)) {
    try(update_extension_students(real), silent = TRUE)   # extensions that just ended
    try(if (course_info_stale()) publish_course_info(real), silent = TRUE)   # CAs read calendar/sections/staff from course-info/
    try(publish_section_rosters(real), silent = TRUE)     # each CA's section students (R/18-sections.R)
    try(refresh_all_progress(real), silent = TRUE)
  }
  tasks_server("tasks", user, go_to)
  calendar_server("calendar", user)
  setup_server("setup", user)
  if (user$role %in% c("admin", "ca")) grading_server("grading", user)
  if (is_admin(user) || isTRUE(user$monitor)) progress_server("progress", user)
  if (is_admin(user)) {
    refresh <- shiny::reactiveVal(0)
    students_server("students", user, refresh)
    staff_server("staff", user, refresh)
    admin_server("admin", user, refresh)
    late_server("late", user, refresh)
    rubrics_server("rubrics", user)
    quizzes_server("quizzes", user, refresh)
    canvas_server("canvas", user, refresh)
  }
}

shiny::shinyApp(ui, server)
