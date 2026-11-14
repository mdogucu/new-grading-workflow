# ---------------------------------------------------------------------------
# 12-progress.R — grading progress that the grading monitor can see
#
# CAs can't open each other's folders, so each grader's dashboard writes a
# tiny summary (counts only, no student names or usernames) to
#     stat100-ca-shared/progress/<username>.csv
#     item, assigned, graded, last_saved, updated_at
# whenever they open the Grading tab or save a grade. "Assign graders" also
# writes one for every grader, and so does opening the dashboard as an admin
# (refresh_all_progress), so the monitor sees everyone even before a grader
# has opened the dashboard.
#
# SHARING: the progress/ folder must be shared with all CAs as EDITOR
# (the rest of stat100-ca-shared stays Viewer). See docs/sharing-folders.md.
# ---------------------------------------------------------------------------

progress_dir <- function() shared_path("progress")

# Summarise one grader's folder and write their progress file.
write_progress <- function(grader) {
  its <- my_items(grader)
  if (!length(its)) return(invisible(FALSE))
  rows <- do.call(rbind, lapply(its, function(it) {
    a <- my_assignment(grader, it); g <- read_grades(grader, it)
    data.frame(item = it, assigned = nrow(a), graded = sum(unique(g$github) %in% a$github),
               last_saved = if (nrow(g)) max(g$saved_at, na.rm = TRUE) else "")
  }))
  rows$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M")
  tryCatch({
    dir.create(progress_dir(), showWarnings = FALSE)
    readr::write_csv(rows, file.path(progress_dir(), paste0(grader, ".csv")), na = "")
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

read_progress <- function() {
  f <- list.files(progress_dir(), pattern = "\\.csv$", full.names = TRUE)
  if (!length(f)) return(NULL)
  p <- do.call(rbind, lapply(f, function(x) {
    d <- read_csv_quiet(x); if (is.null(d) || !nrow(d)) return(NULL)
    d$grader <- sub("\\.csv$", "", basename(x)); d
  }))
  if (is.null(p)) return(NULL)
  p$assigned <- as.integer(p$assigned); p$graded <- as.integer(p$graded)
  p
}

# Admins: rebuild everyone's progress file straight from the grader folders.
refresh_all_progress <- function(user = current_user()) {
  require_admin(user, "refresh grading progress")
  s <- read_staff(FALSE)
  for (g in s$username[!is.na(s$username) & s$username != ""]) write_progress(g)
  invisible(TRUE)
}

# Items worth showing: grading due from 10 days ago to 14 days ahead,
# or (if none) the most recent items that have been assigned.
progress_items <- function(p = read_progress()) {
  if (is.null(p)) return(character())
  cal <- read_calendar()
  its <- unique(p$item)
  due <- cal$grading_due[match(its, cal$item)]
  now <- now_et()
  keep <- its[!is.na(due) & due > now - 10 * 86400 & due < now + 14 * 86400]
  if (!length(keep)) keep <- its
  keep[order(-match(keep, cal$item))]
}

# Graders behind on an item: list(name, username, email, left).
graders_behind <- function(item, p = read_progress()) {
  d <- p[p$item == item & p$graded < p$assigned, ]
  if (is.null(d) || !nrow(d)) return(NULL)
  s <- read_staff(FALSE)
  data.frame(username = d$grader, name = s$name[match(d$grader, s$username)],
             email = if ("email" %in% names(s)) s$email[match(d$grader, s$username)] else NA,
             left = d$assigned - d$graded)
}
