# ---------------------------------------------------------------------------
# 04-roster.R — building the student list from the three downloads
#
# Drop new downloads into admin/inputs/ (keep the old ones; newest file wins):
#   canvas-gradebook/    Canvas > Grades > Export (CSV)
#   gradescope-survey/   Gradescope > GitHub-username survey > Download submissions
#                        (submission_metadata.csv); rename with the date in front
#   section-roster/      the section roster from the registrar (HUID, Name, Section)
#
# They are joined on HUID (= Canvas "SIS User ID" = Gradescope "Student ID").
# Result: admin/data/students.csv — one row per student in Canvas, with
#   huid, canvas_id, name, email, section, github, and flags for problems.
# Then the CA-safe copy  stat100-ca-shared/course-info/students.csv  holds only
#   github username + section (no names, emails or IDs).
# ---------------------------------------------------------------------------

newest_file <- function(dir, pattern = "\\.csv$") {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(f)) return(NA_character_)
  # Sort by file name first (dates in names), then modification time.
  f[order(basename(f), file.mtime(f), decreasing = TRUE)][1]
}

# Registrar files are sometimes Latin-1 rather than UTF-8.
read_any_encoding <- function(path, ...) {
  raw <- readBin(path, "raw", file.info(path)$size)
  enc <- if (validUTF8(rawToChar(raw[raw != as.raw(0)]))) "UTF-8" else "latin1"
  readr::read_csv(path, locale = readr::locale(encoding = enc), show_col_types = FALSE,
                  progress = FALSE, col_types = readr::cols(.default = readr::col_character()), ...)
}

clean_username <- function(x) {
  x <- trimws(x)
  x <- sub("^https?://(www\\.)?github\\.com/", "", x, ignore.case = TRUE)
  x <- sub("^@", "", x)
  x <- sub("/.*$", "", x)
  x[x == ""] <- NA
  x
}

read_canvas_gradebook <- function(path) {
  g <- suppressWarnings(read_any_encoding(path))
  g <- g[!grepl("Points Possible", g$Student) & !is.na(g$`SIS User ID`), ]
  g <- g[!grepl("^Student, Test$", g$Student), ]
  # "COLGSAS STAT 100 001 and COLGSAS STAT 100 D010" -> "D010"
  sec <- stringr::str_extract_all(g$Section, "D\\d{3}")
  data.frame(
    huid = trimws(g$`SIS User ID`),
    canvas_id = trimws(g$ID),
    canvas_name = g$Student,
    canvas_section = vapply(sec, function(s) if (length(s) == 1) s else NA_character_, ""),
    stringsAsFactors = FALSE
  )
}

read_survey <- function(path) {
  s <- read_any_encoding(path)
  s <- s[s$Status != "Missing" & !is.na(s$`Question 1 Response`), ]
  data.frame(
    huid = trimws(s$`Student ID`),
    survey_name = paste(s$`First Name`, s$`Last Name`),
    email = trimws(s$Email),
    github = clean_username(s$`Question 1 Response`),
    survey_time = s$`Question 1 Submitted At`,
    stringsAsFactors = FALSE
  )
}

# Gradescope rosters include students who never answered; we keep their email
# so we can remind them.
read_survey_emails <- function(path) {
  s <- read_any_encoding(path)
  data.frame(huid = trimws(s$`Student ID`), email = trimws(s$Email),
             survey_name = paste(s$`First Name`, s$`Last Name`), stringsAsFactors = FALSE)
}

read_section_roster <- function(path) {
  r <- read_any_encoding(path)
  names(r) <- c("huid", "name", "section")[seq_along(names(r))]
  r$huid <- trimws(r$huid); r$section <- toupper(trimws(r$section))
  r[!is.na(r$huid), c("huid", "name", "section")]
}

build_students <- function() {
  inp <- function(sub) newest_file(admin_path("inputs", sub))
  files <- c(canvas = inp("canvas-gradebook"), survey = inp("gradescope-survey"),
             roster = inp("section-roster"))
  if (is.na(files[["canvas"]])) stop("No Canvas gradebook in admin/inputs/canvas-gradebook/.")
  cv <- read_canvas_gradebook(files[["canvas"]])
  sv <- if (!is.na(files[["survey"]])) read_survey(files[["survey"]]) else NULL
  se <- if (!is.na(files[["survey"]])) read_survey_emails(files[["survey"]]) else NULL
  rs <- if (!is.na(files[["roster"]])) read_section_roster(files[["roster"]]) else NULL

  st <- cv
  # Latest survey answer per student
  if (!is.null(sv)) {
    sv <- sv[order(sv$survey_time, decreasing = TRUE), ]
    sv <- sv[!duplicated(sv$huid), ]
    st <- dplyr::left_join(st, sv[, c("huid", "github")], by = "huid")
  } else st$github <- NA_character_
  if (!is.null(se)) {
    se <- se[!duplicated(se$huid), ]
    st <- dplyr::left_join(st, se[, c("huid", "email")], by = "huid")
  } else st$email <- NA_character_
  if (!is.null(rs)) {
    st <- dplyr::left_join(st, dplyr::rename(rs, roster_section = section, roster_name = name), by = "huid")
  } else { st$roster_section <- NA; st$roster_name <- NA }

  # Manual fixes win over everything (admin/config/student-overrides.csv):
  #   github / section : correct a username or section
  #   status           : "dropped" (left the course: removed from the list) or
  #                      "no github needed" (e.g. a tutor: never flagged)
  ov <- read_overrides()
  st$status <- ""
  for (i in seq_len(nrow(ov))) {
    k <- which(st$huid == ov$huid[i])
    if (!length(k)) next
    if (!is.na(ov$github[i]) && nzchar(ov$github[i])) st$github[k] <- trimws(ov$github[i])
    if (!is.na(ov$section[i]) && nzchar(ov$section[i])) st$roster_section[k] <- toupper(trimws(ov$section[i]))
    if (!is.na(ov$status[i])) st$status[k] <- ov$status[i]
  }
  st <- st[st$status != "dropped", ]

  st$name <- st$canvas_name
  st$section <- ifelse(!is.na(st$roster_section), st$roster_section, st$canvas_section)
  dup <- !is.na(st$github) & duplicated(tolower(st$github)) |
         !is.na(st$github) & duplicated(tolower(st$github), fromLast = TRUE)
  st$issue <- dplyr::case_when(
    st$status == "no github needed"                   ~ "",
    is.na(st$github)                                  ~ "No GitHub username",
    dup                                               ~ "Same GitHub username as another student",
    is.na(st$section)                                 ~ "No section",
    !is.na(st$roster_section) & !is.na(st$canvas_section) &
      st$roster_section != st$canvas_section          ~ "Canvas and section roster disagree",
    is.na(st$roster_section) & !is.null(rs)            ~ "Not on section roster",
    TRUE                                              ~ ""
  )
  st <- st[order(st$section, st$name), c("huid", "canvas_id", "name", "email", "section",
                                          "canvas_section", "roster_section", "github", "status", "issue")]
  attr(st, "files") <- files
  st
}

# Write admin/data/students.csv and the CA-safe copy.
refresh_students <- function(user = current_user()) {
  require_admin(user, "refresh the student list")
  st <- build_students()
  dir.create(admin_path("data"), showWarnings = FALSE)
  readr::write_csv(st, admin_path("data", "students.csv"), na = "")
  out <- shared_path("course-info"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
  pub <- st[!is.na(st$github), c("github", "section")]
  readr::write_csv(pub, file.path(out, "students.csv"), na = "")
  log_action(user$username, "refresh students",
             paste(basename(attr(st, "files")), collapse = ", "))
  invisible(st)
}

read_overrides <- function() {
  ov <- read_csv_quiet(admin_path("config", "student-overrides.csv"))
  cols <- c("huid", "github", "section", "status", "note", "changed_by", "changed_at")
  if (is.null(ov)) ov <- as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols))
  for (k in setdiff(cols, names(ov))) ov[[k]] <- if (nrow(ov)) NA_character_ else character()
  ov$huid <- trimws(ov$huid); ov$status <- tolower(trimws(ov$status))
  ov[, cols]
}

# Set a student's status (or clear it with status = "") and rebuild the list.
set_student_status <- function(huid, status, note = "", user = current_user()) {
  require_admin(user, "change a student's status")
  ov <- read_overrides()
  k <- which(ov$huid == huid)
  if (!length(k)) { ov[nrow(ov) + 1, ] <- NA; k <- nrow(ov); ov$huid[k] <- huid }
  ov$status[k] <- status; ov$note[k] <- note
  ov$changed_by[k] <- user$username; ov$changed_at[k] <- format(Sys.time(), "%Y-%m-%d %H:%M")
  readr::write_csv(ov, admin_path("config", "student-overrides.csv"), na = "")
  log_action(user$username, "student status", sprintf("%s -> %s %s", huid, if (nzchar(status)) status else "(cleared)", note))
  refresh_students(user)
}

read_students <- function() {
  if (has_admin_folder()) {
    st <- read_csv_quiet(admin_path("data", "students.csv"))
    if (is.null(st)) return(NULL)
    st$issue[is.na(st$issue)] <- ""
    if (is.null(st$status)) st$status <- ""
    st$status[is.na(st$status)] <- ""
    st
  } else read_csv_quiet(shared_path("course-info", "students.csv"))
}
