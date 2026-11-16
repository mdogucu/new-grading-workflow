# ---------------------------------------------------------------------------
# 07-theme.R — look and feel, matching stat-100.com
#   crimson #A51C30, dark crimson #70101F, background #F9F9FB, text #1E1E1E,
#   fonts Atkinson Hyperlegible + Source Code Pro. Extra CSS: www/styles.css
# ---------------------------------------------------------------------------

stat100_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#A51C30", secondary = "#595959", success = "#2E7D32",
    warning = "#B26A00", danger = "#A51C30", info = "#1F5F8B",
    bg = "#F9F9FB", fg = "#1E1E1E",
    base_font = bslib::font_collection("Atkinson Hyperlegible", "Arial", "sans-serif"),
    code_font = bslib::font_collection("Source Code Pro", "monospace"),
    "navbar-bg" = "#F5F5F5", "border-radius" = "8px", "link-color" = "#A51C30",
    "link-hover-color" = "#70101F"
  )
}

# Files in app/www/ are served under the address "stat100-www/" (set up in
# app.R with addResourcePath), so they load however the app is started:
# Run App, runApp(), or running app.R line by line.
stat100_head <- function() {
  shiny::tags$head(
    # fonts and icons are bundled in www/vendor/ so nothing loads from the internet
    shiny::tags$link(rel = "stylesheet", href = "stat100-www/vendor/fonts/fonts.css"),
    shiny::tags$link(rel = "stylesheet", href = "stat100-www/vendor/bootstrap-icons/bootstrap-icons.min.css"),
    shiny::tags$link(rel = "stylesheet", href = "stat100-www/styles.css"),
    shiny::tags$link(rel = "icon", href = "stat100-www/stat100-logo-square.png"),
    shiny::tags$script(src = "stat100-www/app.js")
  )
}

navbar_title <- function() {
  shiny::tags$span(class = "brand",
    shiny::tags$img(src = "stat100-www/stat100-logo-rectangle.png", alt = "STAT 100", height = "34"),
    shiny::tags$span(class = "brand-text", "Course dashboard"))
}

icon_tag <- function(name) shiny::tags$i(class = paste0("bi bi-", name), `aria-hidden` = "true")

role_label <- function(user) {
  base <- switch(user$role, admin = "Admin team", ca = "Course assistant",
                 "admin-no-folder" = "Admin (folder missing)", "Not on staff list")
  extra <- c(if (isTRUE(user$senior)) "senior",
             if (isTRUE(user$monitor_this_week)) "grading monitor this week"
             else if (isTRUE(user$monitor)) "grading monitor",
             if (isTRUE(user$on_hold) && !isTRUE(user$monitor_this_week)) "on hold")
  if (length(extra)) paste0(base, " · ", paste(extra, collapse = " · ")) else base
}

user_badge <- function(user) {
  shiny::tags$span(class = "user-badge",
    icon_tag("person-circle"), " ", shiny::tags$strong(user$name %||% "unknown"),
    shiny::tags$span(class = "role-chip", role_label(user)))
}

status_pill <- function(level) {
  lab <- c(overdue = "Overdue", todo = "To do", info = "FYI", ok = "Done")[[level]]
  shiny::tags$span(class = paste("pill", paste0("pill-", level)), lab)
}

fmt_due <- function(x) {
  if (is.na(x)) return("")
  format(x, "%a %b %d, %l:%M %p", tz = "America/New_York")
}

fmt_short <- function(x) { t <- parse_when(x); ifelse(is.na(t), "", format(t, "%b %d, %l:%M %p", tz = "America/New_York")) }

empty_state <- function(msg) shiny::div(class = "empty-state", msg)

# Admins: a menu to open the dashboard as someone else (see user_for_request()).
view_as_menu <- function(user) {
  s <- read_staff_public()
  s <- s[!is.na(s$username) & s$username != "", ]
  s <- s[order(s$role != "ca", s$name), ]
  items <- c(list(bslib::nav_item(shiny::tags$a(class = "dropdown-item", href = "?", shiny::strong("Myself")))),
    lapply(seq_len(nrow(s)), function(i) bslib::nav_item(shiny::tags$a(class = "dropdown-item",
      href = paste0("?as=", utils::URLencode(s$username[i])), sprintf("%s (%s)", s$name[i], s$role[i])))))
  do.call(bslib::nav_menu, c(list(title = "View as", icon = icon_tag("eye"), align = "right"), items))
}

view_as_banner <- function(user) {
  if (is.null(user$viewed_by)) return(NULL)
  shiny::div(class = "view-as-banner", icon_tag("eye"),
    sprintf(" You're viewing the dashboard as %s. Anything you save here is recorded as done by you (%s). ",
            user$name, user$viewed_by$username),
    shiny::tags$a(href = "?", "Back to my view"))
}
