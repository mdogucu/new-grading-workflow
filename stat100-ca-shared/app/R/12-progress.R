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
  # Section items count even before the CA opens them (their list builds
  # itself on the Grading tab): until then, "assigned" = their section roster
  # (minus excused) and opened = FALSE.
  sec_its <- tryCatch(section_items_for(grader), error = function(e) character())
  its <- unique(c(my_items(grader), sec_its))
  if (!length(its)) return(invisible(FALSE))
  rows <- do.call(rbind, lapply(its, function(it) {
    g <- read_grades(grader, it)
    a0 <- if (it %in% sec_its) read_csv_quiet(file.path(grader_item_dir(grader, it), "assignment.csv")) else NULL
    if (!it %in% my_items(grader) || (it %in% sec_its && (is.null(a0) || !"built_at" %in% names(a0)))) {
      ros <- my_section_students(grader); x <- my_section_exceptions(grader, it)
      n <- sum(!tolower(ros$github) %in% tolower(x$github[x$type %in% "excused"]))
      return(data.frame(item = it, assigned = n, graded = length(unique(g$github)), waiting = 0L,
                        last_saved = if (nrow(g)) max(g$saved_at, na.rm = TRUE) else "", opened = FALSE))
    }
    a <- my_assignment(grader, it)
    w <- if (("wait_until" %in% names(a))) parse_when(a$wait_until) else rep(NA, nrow(a))
    wait <- (!is.na(w) & w > now_et()) | is.na(a$sha) | a$sha == ""
    a <- a[!wait, ]                                       # extensions not counted yet
    data.frame(item = it, assigned = nrow(a), graded = sum(unique(g$github) %in% a$github), waiting = sum(wait),
               last_saved = if (nrow(g)) max(g$saved_at, na.rm = TRUE) else "", opened = TRUE)
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
  p <- dplyr::bind_rows(lapply(f, function(x) {
    d <- read_csv_quiet(x); if (is.null(d) || !nrow(d)) return(NULL)
    d$grader <- sub("\\.csv$", "", basename(x)); d
  }))
  if (is.null(p)) return(NULL)
  p$assigned <- as.integer(p$assigned); p$graded <- as.integer(p$graded)
  p$opened <- if ("opened" %in% names(p)) !(toupper(as.character(p$opened)) %in% "FALSE") else TRUE
  p
}

# Admins: rebuild everyone's progress file straight from the grader folders,
# and the combined grade files (collect_grades) for every assigned item.
refresh_all_progress <- function(user = current_user()) {
  require_admin(user, "refresh grading progress")
  s <- read_staff(FALSE)
  for (g in s$username[!is.na(s$username) & s$username != ""]) write_progress(g)
  its <- sub("\\.csv$", "", list.files(admin_path("data", "assignments"), pattern = "^[a-z]+[0-9]+[a-z]?\\.csv$"))
  cal <- read_calendar()                                   # sections: no assignment file (R/18-sections.R)
  its <- unique(c(its, cal$item[cal$type %in% "section" & in_dashboard_scope(cal$item) & !is.na(cal$student_due) & cal$student_due < now_et()]))
  for (it in its) try(collect_grades(it), silent = TRUE)
  invisible(TRUE)
}

# All graders' grades for one item, in one place (admins only — has names):
#   admin/data/grades/<item>.csv         one row per student x question
#     (github, name, section, question, codes, rubric_points, adjustment,
#      points, max_points, feedback, grader, saved_at, sha)
#   admin/data/grades/<item>-totals.csv  one row per student
#     (github, name, section, grader, status, points, max_points, percent)
#     status: graded / not graded yet / extension (waiting) / excused / ...
collect_grades <- function(item) {
  asg <- item_assignments(item); if (is.null(asg) || !nrow(asg)) return(invisible(NULL))
  sn <- item_snapshot(item); st <- read_students()
  rows <- do.call(rbind, lapply(unique(stats::na.omit(asg$grader)), function(g) {
    x <- read_grades(g, item); x[x$github %in% asg$github[asg$grader %in% g], , drop = FALSE]
  }))
  if (is.null(rows)) rows <- read_grades("", item)[0, ]
  m <- match(tolower(rows$github), tolower(st$github))
  rows <- cbind(rows["github"], name = st$name[m], section = st$section[m], rows[setdiff(names(rows), "github")])
  out <- admin_path("data", "grades"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(rows, file.path(out, paste0(item, ".csv")), na = "")
  # totals: everyone in the snapshot, graded or not
  everyone <- if (!is.null(sn)) sn[, c("github", "status")] else data.frame(github = asg$github, status = "")
  tot <- if (nrow(rows)) stats::aggregate(cbind(points, max_points) ~ github, data = rows, FUN = sum) else
    data.frame(github = character(), points = numeric(), max_points = numeric())
  m2 <- match(tolower(everyone$github), tolower(st$github))
  t2 <- data.frame(github = everyone$github, name = st$name[m2], section = st$section[m2],
                   grader = asg$grader[match(everyone$github, asg$github)],
                   status = ifelse(everyone$github %in% tot$github, "graded",
                             ifelse(everyone$github %in% asg$github, ifelse(everyone$status == "extension (waiting)", "extension (waiting)", "not graded yet"),
                                    everyone$status)),
                   points = tot$points[match(everyone$github, tot$github)],
                   max_points = tot$max_points[match(everyone$github, tot$github)])
  t2$percent <- round(100 * t2$points / t2$max_points, 1)
  readr::write_csv(t2[order(t2$section, t2$name), ], file.path(out, paste0(item, "-totals.csv")), na = "")
  invisible(t2)
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
