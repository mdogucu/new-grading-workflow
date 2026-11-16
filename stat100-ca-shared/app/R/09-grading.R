# ---------------------------------------------------------------------------
# 09-grading.R — weekly grading: who grades whom, rubrics, saving grades
#
# ADMIN STEPS for one item (e.g. hw09), on the Admin tab > Weekly grading:
#   1. Find submissions   -> admin/data/snapshots/hw09.csv
#      (one row per student: repo, graded commit, minutes late, status)
#      plus hw09-commits.csv (every student commit) and hw09-students.csv.
#      Late homework is then reviewed on the Late work tab (R/10-late.R).
#   2. Assign graders     -> admin/data/assignments/hw09.csv
#                         -> graders/stat100-grader-<user>/grading/hw09/assignment.csv
#      Homework: students shuffled with set.seed(week); dealt out to the active
#        CAs (not on hold, not this week's monitor) so everyone gets the same
#        number (±1), never to a CA with a conflict of interest.
#      Section: each student goes to their section's leader (sections.csv);
#        if that's a conflict of interest, to another active CA.
#      Students with no submission are not assigned (they get 0 / "missing").
#
# GRADERS save to  graders/stat100-grader-<user>/grading/hw09/grades.csv
#   one row per student x question:
#     github, question, codes ("1---2", gradetools style), rubric_points
#     (from the rubric alone), adjustment (grader's +/- tweak, e.g. -0.001),
#     points (= rubric_points + adjustment, kept between 0 and the max),
#     max_points, feedback (the text the student will see — starts as the
#     rubric feedback for the ticked options, the grader may edit it),
#     grader, saved_at, sha.
#
# RUBRICS: stat100-ca-shared/rubrics/<item>-rubric.csv (gradetools columns:
#   name, total_points, prompt_code, prompt_message, feedback, points_to_remove).
#   Section items use section-rubric.csv if there's no sec09-rubric.csv.
#   Rows named "all_questions" can be used on any question; their
#   points_to_remove is a PERCENT of that question's points (404 = 100% = missing).
# ---------------------------------------------------------------------------

# Choices for an assignment drop-down: "hw09 · Statistical inference".
# (paste0() on an empty list returns one string, so handle "nothing yet" here.)
item_choices <- function(items, titles, empty_label = "Nothing here yet") {
  if (!length(items)) return(stats::setNames("", empty_label))
  stats::setNames(items, paste0(items, " · ", titles))
}

item_info <- function(item) {
  cal <- read_calendar()
  r <- cal[cal$item == item, ]
  if (!nrow(r) && grepl("^quiz", item)) {          # quizzes may only be in the Canvas export
    q <- quiz_catalog(); q <- q[q$item == item, ]
    if (nrow(q)) r <- data.frame(item = item, type = "quiz", week = NA_integer_, title = q$title[1], repo = NA,
                                 student_due = q$student_due[1], grading_due = as.POSIXct(NA), canvas_id = q$canvas_id[1])
  }
  if (!nrow(r)) stop("Item ", item, " is not in calendar.csv")
  r <- as.list(r[1, ])
  r$folder <- if (r$type == "section") "section" else "homework"
  r
}

staff_logins <- function() {
  s <- read_staff(FALSE)
  c(s$username, s$name, "github-classroom[bot]", "github-actions[bot]", "Mine Dogucu")
}

# --- 1. snapshots ----------------------------------------------------------

snapshot_item <- function(item, user = current_user(), source = repo_source(), progress = NULL) {
  require_admin(user, "find submissions")
  it <- item_info(item)
  if (is.na(it$student_due)) stop("Add student_due for ", item, " in calendar.csv first.")
  st <- read_students()
  st <- st[!is.na(st$github) & st$github != "", ]
  staff <- tolower(staff_logins())
  base <- list(); cms <- list()
  for (i in seq_len(nrow(st))) {
    if (!is.null(progress)) progress(i / nrow(st), st$github[i])
    repo <- student_repo(it$repo, st$github[i])
    cm <- tryCatch(repo_commits(repo, source, path = it$folder), error = function(e) e)   # only commits touching homework/ (or section/)
    if (!inherits(cm, "error") && is.null(cm)) {        # nothing in that folder: is there a repo at all?
      any_cm <- tryCatch(repo_commits(repo, source), error = function(e) NULL)
      cm <- if (is.null(any_cm)) NULL else any_cm[0, ]
    }
    status <- if (inherits(cm, "error")) paste("error:", conditionMessage(cm)) else if (is.null(cm)) "no repo" else "ok"
    base[[i]] <- data.frame(github = st$github[i], section = st$section[i], repo = repo, repo_status = status)
    if (status == "ok") {
      cm <- cm[!tolower(cm$author) %in% staff & !grepl("\\[bot\\]$", cm$author), ]   # student commits only
      if (nrow(cm)) cms[[length(cms) + 1]] <- data.frame(github = st$github[i], sha = cm$sha,
        time = format(cm$time, "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), author = cm$author, message = cm$message)
    }
  }
  base <- do.call(rbind, base)
  cms <- if (length(cms)) do.call(rbind, cms) else data.frame(github = character(), sha = character(), time = character(), author = character(), message = character())
  dir.create(admin_path("data", "snapshots"), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(base, admin_path("data", "snapshots", paste0(item, "-students.csv")), na = "")
  readr::write_csv(cms, admin_path("data", "snapshots", paste0(item, "-commits.csv")), na = "")
  sn <- compute_snapshot(item)
  log_action(user$username, paste("find submissions", item),
             paste(names(table(sn$status)), table(sn$status), sep = ": ", collapse = ", "))
  sn
}

read_snapshot <- function(item) {
  sn <- read_csv_quiet(admin_path("data", "snapshots", paste0(item, ".csv")))
  if (is.null(sn)) return(NULL)
  for (k in intersect(c("suggested_minutes", "minutes_late"), names(sn))) sn[[k]] <- suppressWarnings(as.numeric(sn[[k]]))
  sn
}

# --- 2. assignment ---------------------------------------------------------

active_graders <- function(staff = read_staff()) {
  sort(staff$username[staff$role == "ca" & !staff$on_hold & !is.na(staff$username) & staff$username != ""])
}

# Deal students to graders, evenly, skipping conflicts. Reproducible (seed).
deal_students <- function(students, graders, coi, seed) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  students <- sample(sort(students))
  graders <- sample(graders)
  load <- setNames(integer(length(graders)), graders)
  out <- character(length(students))
  for (i in seq_along(students)) {
    blocked <- tolower(coi$grader[tolower(coi$student) == tolower(students[i])])
    ok <- graders[!tolower(graders) %in% blocked]
    if (!length(ok)) { out[i] <- NA; next }
    g <- ok[which.min(load[ok])]           # least loaded; ties -> shuffled order
    out[i] <- g; load[g] <- load[g] + 1
  }
  data.frame(github = students, grader = out, stringsAsFactors = FALSE)
}

assign_item <- function(item, user = current_user()) {
  require_admin(user, "assign graders")
  it <- item_info(item)
  sn <- read_snapshot(item)
  if (is.null(sn)) stop("Find submissions for ", item, " first.")
  if (!file.exists(admin_path("data", "snapshots", paste0(item, "-commits.csv"))))
    stop("This snapshot was made by an older version of the dashboard. Click Find submissions again first.")
  if (any(sn$status == "needs review"))
    stop(sprintf("%d late submission(s) still need a decision — Late work tab first.", sum(sn$status == "needs review")))
  # graded: see graded_statuses in R/10-late.R.
  # not graded: no submission, no repo, over allowance (gets 0).
  sub <- sn[gradeable(sn), ]
  coi <- read_coi(); staff <- read_staff()
  if (it$type == "section") {
    sec <- read_sections()
    sub$grader <- sec$leader[match(sub$section, sec$section)]
    clash <- !is.na(sub$grader) & paste(tolower(sub$github), tolower(sub$grader)) %in%
      paste(tolower(coi$student), tolower(coi$grader))
    if (any(clash | is.na(sub$grader))) {
      redo <- deal_students(sub$github[clash | is.na(sub$grader)], active_graders(staff), coi, it$week)
      sub$grader[match(redo$github, sub$github)] <- redo$grader
    }
    asg <- sub
  } else {
    g <- active_graders(staff)
    if (!length(g)) stop("No active CAs (everyone is on hold?)")
    d <- deal_students(sub$github, g, coi, it$week)
    asg <- merge(sub, d, by = "github")
  }
  asg$item <- item
  asg <- asg[order(asg$grader, asg$github), c("item", "github", "section", "repo", "sha", "commit_time", "minutes_late", "status", "grader")]
  dir.create(admin_path("data", "assignments"), recursive = TRUE, showWarnings = FALSE)
  old <- read_csv_quiet(admin_path("data", "assignments", paste0(item, ".csv")))
  if (!is.null(old)) file.copy(admin_path("data", "assignments", paste0(item, ".csv")),
                               admin_path("data", "assignments", paste0(item, "-before-", format(Sys.time(), "%Y%m%d-%H%M"), ".csv")))
  readr::write_csv(asg, admin_path("data", "assignments", paste0(item, ".csv")), na = "")
  # Each grader's own list: GitHub usernames + commits only (no names, no sections).
  for (g in unique(stats::na.omit(asg$grader))) {
    d <- file.path(course_root(), "graders", grader_folder_name(g), "grading", item)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(grader_list(asg, g), file.path(d, "assignment.csv"), na = "")
  }
  for (g in unique(stats::na.omit(asg$grader))) write_progress(g)
  log_action(user$username, paste("assign graders", item),
             paste(names(table(asg$grader)), table(asg$grader), sep = ": ", collapse = ", "))
  asg
}

read_assignments <- function(item) read_csv_quiet(admin_path("data", "assignments", paste0(item, ".csv")))

# --- grader side -----------------------------------------------------------

grader_item_dir <- function(grader, item) {
  d <- grader_dir(grader)
  if (is.na(d)) d <- file.path(course_root(), "graders", grader_folder_name(grader))
  file.path(d, "grading", item)
}

# (Section lists also record students with no submission; they aren't graded.)
my_assignment <- function(grader, item) {
  a <- read_csv_quiet(file.path(grader_item_dir(grader, item), "assignment.csv"))
  if (!is.null(a) && "status" %in% names(a)) a <- a[is.na(a$status) | a$status != "no submission", ]
  a
}

# Items that have an assignment file for this grader.
my_items <- function(grader) {
  d <- file.path(grader_dir(grader) %||% "", "grading")
  if (is.na(d) || !dir.exists(d)) return(character())
  its <- list.dirs(d, full.names = FALSE, recursive = FALSE)
  its[file.exists(file.path(d, its, "assignment.csv"))]
}

read_grades <- function(grader, item) {
  g <- read_csv_quiet(file.path(grader_item_dir(grader, item), "grades.csv"))
  cols <- c("github", "question", "codes", "rubric_points", "adjustment", "points", "max_points",
            "feedback", "grader", "saved_at", "sha")
  if (is.null(g)) {
    g <- as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols))
  }
  for (k in setdiff(cols, names(g))) g[[k]] <- if (nrow(g)) NA_character_ else character()
  for (k in c("rubric_points", "adjustment", "points", "max_points")) g[[k]] <- suppressWarnings(as.numeric(g[[k]]))
  g[, cols]
}

# Replace one student's rows in the grader's grades.csv.
save_student_grades <- function(grader, item, github, rows, saved_by) {
  d <- grader_item_dir(grader, item); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(d, "grades.csv")
  g <- read_grades(grader, item)
  g <- g[g$github != github, ]
  rows$github <- github; rows$grader <- saved_by
  rows$saved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  g <- rbind(g, rows[, names(g)])
  tmp <- paste0(f, ".tmp")
  readr::write_csv(g, tmp, na = "")
  file.rename(tmp, f)
  invisible(g)
}

# --- rubrics ---------------------------------------------------------------

rubric_path <- function(item) {
  f <- shared_path("rubrics", paste0(item, "-rubric.csv"))
  if (!file.exists(f) && grepl("^sec", item)) f <- shared_path("rubrics", "section-rubric.csv")
  f
}

read_rubric <- function(item) {
  f <- rubric_path(item)
  if (!file.exists(f)) return(NULL)
  r <- read_any_encoding(f)
  r$name <- trimws(r$name)
  r$total_points <- suppressWarnings(as.numeric(r$total_points))
  r$points_to_remove <- suppressWarnings(as.numeric(r$points_to_remove))
  r$prompt_code <- trimws(r$prompt_code)
  r
}

# Questions in rubric order with their points.
rubric_questions <- function(r) {
  q <- r[r$name != "all_questions", ]
  qs <- unique(q$name)
  data.frame(question = qs,
             max_points = vapply(qs, function(x) { v <- q$total_points[q$name == x]; v <- v[!is.na(v)]; if (length(v)) v[1] else 0 }, 0),
             stringsAsFactors = FALSE)
}

# Points for one question given the chosen codes.
score_question <- function(r, question, codes) {
  maxp <- rubric_questions(r)$max_points[rubric_questions(r)$question == question]
  if (!length(codes)) return(NA_real_)
  spec <- r[r$name == question & r$prompt_code %in% codes, ]
  gen <- r[r$name == "all_questions" & r$prompt_code %in% codes, ]
  pts <- maxp - sum(spec$points_to_remove, na.rm = TRUE) - maxp * sum(gen$points_to_remove, na.rm = TRUE) / 100
  max(0, round(pts, 4))
}

# The feedback a student sees for the ticked options (one paragraph each).
feedback_text <- function(r, question, codes) {
  if (!length(codes)) return("")
  spec <- r[r$name == question & r$prompt_code %in% codes, ]
  gen <- r[r$name == "all_questions" & r$prompt_code %in% codes, ]
  fb <- c(spec$feedback, gen$feedback)
  paste(fb[!is.na(fb) & nzchar(fb)], collapse = "\n\n")
}

# Rubric points + the grader's adjustment, kept between 0 and the question's max.
final_points <- function(rubric_points, adjustment, max_points) {
  if (is.na(rubric_points)) return(NA_real_)
  adj <- suppressWarnings(as.numeric(adjustment)); if (!length(adj) || is.na(adj)) adj <- 0
  round(min(max_points, max(0, rubric_points + adj)), 4)
}

# --- reading a student's answers --------------------------------------------

# Key used to match rubric names to sections of the student's file:
# "Question 2 - Is 98.6°F Normal?" -> "question2";  part "vii." -> "question2vii"
question_key <- function(x) {
  m <- regmatches(x, regexec("[Qq]uestion\\s*(\\d+)\\s*([ivxlc]*)\\b", x))
  vapply(m, function(z) if (length(z)) tolower(paste0("question", z[2], z[3])) else NA_character_, "")
}

roman_part <- "^\\s*([ivxlc]+)\\.\\s"

# Split a .qmd into labelled chunks: question headings and roman-numeral parts.
# Returns data.frame(key, label, start, end) with line numbers.
split_questions <- function(lines) {
  qh <- grep("^#{1,4}\\s*Question\\s*\\d+", lines, ignore.case = TRUE)
  if (!length(qh)) return(NULL)
  anchors <- list()
  stop_at <- grep("^---\\s*$|^Before [Ss]ubmitting", lines)
  for (k in seq_along(qh)) {
    qnum <- sub(".*[Qq]uestion\\s*(\\d+).*", "\\1", lines[qh[k]])
    end_q <- if (k < length(qh)) qh[k + 1] - 1 else {
      s <- stop_at[stop_at > qh[k]]; if (length(s)) s[1] - 1 else length(lines) }
    anchors[[length(anchors) + 1]] <- data.frame(key = paste0("question", qnum), label = paste("Question", qnum),
                                                 start = qh[k], end = end_q, part = "")
    body <- seq(qh[k] + 1, end_q)
    parts <- body[grepl(roman_part, lines[body])]
    for (j in seq_along(parts)) {
      p <- sub(roman_part, "\\1", regmatches(lines[parts[j]], regexpr(roman_part, lines[parts[j]])))
      p <- trimws(sub("\\.$", "", p))
      e <- if (j < length(parts)) parts[j + 1] - 1 else end_q
      anchors[[length(anchors) + 1]] <- data.frame(key = paste0("question", qnum, p),
                                                   label = paste0("Question ", qnum, p), start = parts[j], end = e, part = p)
    }
  }
  do.call(rbind, anchors)
}

# For one rubric question, the lines of the student's file that belong to it,
# with a flag for lines the student wrote (not in the template).
answer_for <- function(lines, chunks, rubric_question, template_lines = NULL) {
  k <- question_key(rubric_question)
  hit <- chunks[chunks$key == k, ]
  if (!nrow(hit)) return(NULL)
  hit <- hit[1, ]
  # A question that has parts: show only the text before the first part.
  idx <- seq(hit$start, hit$end)
  if (hit$part == "") {
    sub_parts <- chunks[chunks$key != k & startsWith(chunks$key, k) & grepl("^[ivxlc]+$", chunks$part) &
                          chunks$start > hit$start & chunks$start <= hit$end, ]
    if (nrow(sub_parts)) idx <- seq(hit$start, min(sub_parts$start) - 1)
  }
  txt <- lines[idx]
  tl <- trimws(template_lines %||% character())
  data.frame(line = idx, text = txt,
             student = if (length(tl)) !(trimws(txt) %in% tl) & trimws(txt) != "" else FALSE,
             stringsAsFactors = FALSE)
}

# --- the rendered (.html) side ---------------------------------------------
# Same idea as split_questions(), on the student's rendered HTML: walk the
# page in order; a "Question N" heading starts a question; each item of a
# Roman-numeral list (<ol type="i">) starts a part. Everything until the next
# marker (their code, output, plots, text) belongs to that part.
# Returns data.frame(key, html). Images are pointed at `img_prefix`.
html_parts <- function(html_file, img_prefix = NULL) {
  doc <- xml2::read_html(html_file)
  for (n in xml2::xml_find_all(doc, "//script|//style")) xml2::xml_remove(n)
  main <- xml2::xml_find_first(doc, "//main")
  if (inherits(main, "xml_missing")) main <- xml2::xml_find_first(doc, "//body")
  if (!is.null(img_prefix)) for (im in xml2::xml_find_all(main, ".//img")) {
    src <- xml2::xml_attr(im, "src")
    if (!is.na(src) && !grepl("^(https?:|data:)", src)) xml2::xml_attr(im, "src") <- paste0(img_prefix, "/", src)
  }
  blocks <- list()
  flatten <- function(node) for (ch in xml2::xml_children(node)) {
    if (xml2::xml_name(ch) %in% c("section", "div") && length(xml2::xml_find_all(ch, "./h1|./h2|./h3|./h4|./section|./ol[@type='i']")))
      flatten(ch) else blocks[[length(blocks) + 1]] <<- ch
  }
  flatten(main)
  out <- list(); key <- NA; qn <- NA
  add <- function(k, html) out[[k]] <<- c(out[[k]], html)
  for (b in blocks) {
    nm <- xml2::xml_name(b); txt <- xml2::xml_text(b)
    if (nm %in% c("h1", "h2", "h3", "h4") && grepl("Question\\s*\\d+", txt, ignore.case = TRUE)) {
      qn <- sub(".*[Qq]uestion\\s*(\\d+).*", "\\1", txt); key <- paste0("question", qn); add(key, as.character(b)); next
    }
    if (nm == "hr" || grepl("^\\s*Before [Ss]ubmitting", txt)) { key <- NA; next }
    if (is.na(qn)) next
    if (nm == "ol" && identical(xml2::xml_attr(b, "type"), "i")) {
      start <- suppressWarnings(as.integer(xml2::xml_attr(b, "start"))); if (is.na(start)) start <- 1L
      lis <- xml2::xml_find_all(b, "./li")
      for (k in seq_along(lis)) {
        n <- start + k - 1L
        key <- paste0("question", qn, tolower(as.character(utils::as.roman(n))))
        add(key, sprintf('<ol type="i" start="%d">%s</ol>', n, as.character(lis[[k]])))
      }
      next
    }
    if (!is.na(key)) add(key, as.character(b))
  }
  if (!length(out)) return(NULL)
  data.frame(key = names(out), html = vapply(out, paste, "", collapse = "\n"), stringsAsFactors = FALSE)
}

find_item_file <- function(repo_dir, item, folder, ext = "qmd") {
  f <- list.files(file.path(repo_dir, folder), pattern = paste0("^", item, ".*\\.", ext, "$"), full.names = TRUE)
  if (length(f)) f[1] else NA_character_
}
