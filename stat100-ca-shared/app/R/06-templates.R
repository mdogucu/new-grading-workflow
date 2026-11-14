# ---------------------------------------------------------------------------
# 06-templates.R — email templates
#
# Templates are plain text files in  stat100-ca-shared/templates/ .
# The first line is the subject, starting with "Subject:". Everything after
# the first blank line is the body. Words in {curly braces} are filled in:
#   {course} {semester} {github_org} {website} {sender} {assignment} ...
# Edit the files freely — no code change needed. Unknown {words} are left as is.
# ---------------------------------------------------------------------------

template_dir <- function() shared_path("templates")

list_templates <- function() {
  f <- list.files(template_dir(), pattern = "\\.txt$")
  sub("\\.txt$", "", f)
}

read_template <- function(name) {
  p <- file.path(template_dir(), paste0(name, ".txt"))
  if (!file.exists(p)) return(list(subject = "", body = sprintf("[Template %s.txt not found]", name)))
  lines <- readLines(p, warn = FALSE, encoding = "UTF-8")
  subj_line <- grep("^Subject:", lines)[1]
  subject <- if (!is.na(subj_line)) trimws(sub("^Subject:", "", lines[subj_line])) else ""
  body_start <- if (!is.na(subj_line)) subj_line + 1 else 1
  body <- lines[body_start:length(lines)]
  while (length(body) && body[1] == "") body <- body[-1]
  list(subject = subject, body = paste(body, collapse = "\n"))
}

fill_template <- function(name, values = list()) {
  t <- read_template(name)
  course <- read_course()
  vals <- c(values, list(course = course$course, semester = course$semester,
                         github_org = course$github_org, website = course$website))
  fill <- function(x) {
    for (k in names(vals)) x <- gsub(paste0("{", k, "}"), as.character(vals[[k]] %||% ""), x, fixed = TRUE)
    x
  }
  list(subject = fill(t$subject), body = fill(t$body))
}

# A mailto: link that opens the person's email app with everything filled in.
mailto_link <- function(bcc, subject, body, to = "") {
  enc <- function(x) utils::URLencode(x, reserved = TRUE)
  sprintf("mailto:%s?bcc=%s&subject=%s&body=%s", enc(to), enc(paste(bcc, collapse = ",")),
          enc(subject), enc(body))
}

# UI block: recipients, editable subject/body, copy buttons, open-in-mail link.
email_panel_ui <- function(id, emails, tmpl) {
  ns <- shiny::NS(id)
  shiny::div(class = "email-panel",
    shiny::tags$label("Recipients (BCC)"),
    shiny::div(class = "copy-row",
      shiny::tags$textarea(id = ns("to"), class = "form-control mono", rows = 2, readonly = NA,
                           paste(emails, collapse = ", ")),
      copy_button(ns("to"), "Copy emails")),
    shiny::textInput(ns("subject"), "Subject", tmpl$subject, width = "100%"),
    shiny::textAreaInput(ns("body"), "Message (edit as you like)", tmpl$body, rows = 12, width = "100%"),
    shiny::div(class = "btn-row",
      copy_button(ns("body"), "Copy message"),
      shiny::tags$a(class = "btn btn-primary", id = ns("mail"), target = "_blank",
                    href = mailto_link(emails, tmpl$subject, tmpl$body), "Draft in my email app")),
    shiny::tags$small(class = "text-muted",
      "Edits above update the email-app link. To change the wording for good, edit the file in stat100-ca-shared/templates/.")
  )
}

copy_button <- function(target_id, label) {
  shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm copy-btn",
                     `data-target` = target_id, label)
}
