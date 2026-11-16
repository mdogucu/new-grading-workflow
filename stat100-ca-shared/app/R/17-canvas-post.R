# ---------------------------------------------------------------------------
# 17-canvas-post.R — everything that goes to Canvas, in one place
#                    (tab: "Post to Canvas", R/tab-canvas.R)
#
# For any item (homework, section, quiz) canvas_queue() works out, for each
# student, what Canvas should have:
#   action "grade"   a score (+ a comment for homework/sections)
#   action "excuse"  EXCUSED (Late & excused tab said so)
#   action "zero"    0 for no submission / not in the quiz file (only if you
#                    tick the box; the old scripts didn't post these)
#   action ""        nothing yet: not graded yet, extension still running,
#                    late work not reviewed, no Canvas id
#
# HOMEWORK / SECTION score = sum of the question points in the graders'
#   grades.csv files. The comment is built like 11-release-feedback.R:
#     ### Assignment Feedback
#     • **Question 1a** (2 / 3 pts)
#       - <feedback the student sees, one line per paragraph>
#     ...
#     **Total Score:** 17.5 / 20
#   Homework that was late also gets the late note from 12-late-submission-
#   penalty.R (minutes late, running total, what's left of the 48 hours) as a
#   SEPARATE comment. Homework that takes a student over the allowance
#   ("over allowance (0)") gets 0 and just the late note — it isn't graded.
#   Canvas comments never replace each other: each one is added below the
#   earlier ones.
# QUIZZES: the Participation score from the Poll Everywhere file (matches
#   are made on the Quizzes tab). No comment.
#
# WHAT WAS SENT: admin/data/canvas-posted.csv, one row per student per post:
#   item, canvas_user_id, github, name, action, score,
#   comment       the feedback as it stood at this post
#   late_comment  the late note as it stood at this post
#   questions     each question's points and feedback at this post (JSON),
#                 used to see what a regrade changed
#   sent          the comment(s) actually sent this time (separated by -----)
#   kind          "first", "update" (after a regrade), or "marked" (recorded
#                 as posted without sending anything)
#   posted_at, posted_by
#
# REGRADES: after posting, the queue keeps comparing what was last posted
#   with what the grades say now. If anyone changes a student's points or
#   feedback (a regrade in the Grading tab), that student shows up as
#   "changed since posted". Posting updates the score and ADDS one comment,
#   "### Regrade comment", listing only the questions that changed and the
#   new total. The original feedback comment stays on Canvas as it was.
#
# HOW IT SENDS: POST /api/v1/courses/<course>/assignments/<id>/submissions/
#   update_grades with grade_data[<canvas user id>][posted_grade],
#   [text_comment] and [excuse] — the same call as the old scripts, but up to
#   25 students per call. Canvas answers with a "progress" link that we check
#   every 2 seconds until it says completed (or failed).
# ---------------------------------------------------------------------------

canvas_log_path <- function() admin_path("data", "canvas-posted.csv")
canvas_log_cols <- c("item", "canvas_user_id", "github", "name", "action", "score", "comment", "late_comment",
                     "questions", "sent", "kind", "posted_at", "posted_by")

read_canvas_log <- function() {
  l <- read_csv_quiet(canvas_log_path())
  if (is.null(l)) l <- as.data.frame(stats::setNames(replicate(length(canvas_log_cols), character(), simplify = FALSE), canvas_log_cols))
  for (k in setdiff(canvas_log_cols, names(l))) l[[k]] <- if (nrow(l)) NA_character_ else character()
  l$canvas_user_id <- as.character(l$canvas_user_id)
  l$score <- suppressWarnings(as.numeric(l$score))
  for (k in c("comment", "late_comment", "questions", "sent")) l[[k]][is.na(l[[k]])] <- ""
  l[, canvas_log_cols]
}

append_canvas_log <- function(rows) {
  l <- rbind(read_canvas_log(), rows[, canvas_log_cols])
  dir.create(dirname(canvas_log_path()), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(canvas_log_path(), ".tmp")
  readr::write_csv(l, tmp, na = "")
  file.rename(tmp, canvas_log_path())
}

# The most recent post for each student of one item.
last_posted <- function(item, log = read_canvas_log()) {
  l <- log[tolower(log$item) == tolower(item), ]
  l <- l[order(l$posted_at, decreasing = TRUE), ]
  l[!duplicated(l$canvas_user_id), ]
}

is_quiz_grader <- function(user) {
  tolower(user$username %||% "") %in% tolower(trimws(strsplit(read_course()$quiz_grader %||% "", ",")[[1]]))
}

fmt_pts <- function(x) format(round(x, 3), trim = TRUE, scientific = FALSE, drop0trailing = TRUE)

# --- the comment -------------------------------------------------------------

# "  - paragraph" lines for one question's feedback ("All correct!" left out).
feedback_bullets <- function(fb) {
  if (is.na(fb)) fb <- ""
  paras <- trimws(strsplit(fb, "\n\\s*\n")[[1]])
  paras <- gsub("\\s*\n\\s*", " ", paras)
  paras <- paras[nzchar(paras) & paras != "All correct!"]
  if (length(paras)) paste0("\n  - ", paras, collapse = "") else ""
}

# g: one student's rows from grades.csv, in rubric order.
feedback_comment <- function(g) {
  lines <- vapply(seq_len(nrow(g)), function(i)
    paste0("• **", g$question[i], "** (", fmt_pts(g$points[i]), " / ", fmt_pts(g$max_points[i]), " pts)",
           feedback_bullets(g$feedback[i])), "")
  paste0("### Assignment Feedback\n\n", paste(lines, collapse = "\n\n"),
         "\n\n**Total Score:** ", fmt_pts(sum(g$points, na.rm = TRUE)), " / ", fmt_pts(sum(g$max_points, na.rm = TRUE)))
}

# The extra comment after a regrade: only the questions whose points or
# feedback changed since the last post, then the new total.
regrade_comment <- function(new_q, old_q, new_score, old_score) {
  parse <- function(x) if (is.na(x) || !nzchar(x)) NULL else jsonlite::fromJSON(x)
  n <- parse(new_q); o <- parse(old_q)
  lines <- character()
  if (!is.null(n) && !is.null(o)) for (i in seq_len(nrow(n))) {
    k <- match(n$question[i], o$question)
    op <- if (is.na(k)) NA else o$points[k]; of <- if (is.na(k)) "" else o$feedback[k]
    if (!is.na(op) && abs(op - n$points[i]) < 1e-9 && identical(of, n$feedback[i])) next
    pts <- if (is.na(op) || abs(op - n$points[i]) < 1e-9) sprintf("%s / %s pts", fmt_pts(n$points[i]), fmt_pts(n$max_points[i]))
           else sprintf("%s → %s / %s pts", fmt_pts(op), fmt_pts(n$points[i]), fmt_pts(n$max_points[i]))
    lines <- c(lines, paste0("• **", n$question[i], "** (", pts, ")", feedback_bullets(n$feedback[i])))
  }
  total <- if (!is.null(n)) fmt_pts(sum(n$max_points)) else NA
  paste0("### Regrade comment\n\n", if (length(lines)) paste0(paste(lines, collapse = "\n\n"), "\n\n") else "",
         "**Updated Total Score:** ", fmt_pts(new_score), if (!is.na(total)) paste0(" / ", total) else "",
         if (!is.na(old_score) && abs(old_score - new_score) > 1e-9) paste0(" (was ", fmt_pts(old_score), ")") else "")
}

# --- what each student should have on Canvas --------------------------------

canvas_items <- function(user) {
  cal <- read_calendar(); if (is.null(cal)) cal <- data.frame(item = character(), type = character(), week = integer(), title = character())
  hs <- cal[cal$type %in% c("homework", "section") & vapply(cal$item, in_dashboard_scope, TRUE), ]
  hs <- hs[file.exists(admin_path("data", "snapshots", paste0(hs$item, ".csv"))) |
           (hs$type == "section" & !is.na(hs$student_due) & hs$student_due < now_et()), ]
  out <- data.frame(item = hs$item, title = hs$title, type = hs$type, week = hs$week)
  if (is_quiz_grader(user)) {
    f <- list.files(pollev_dir(), pattern = "\\.csv$")
    q <- unique(tolower(quiz_name_from_file(f)))
    q <- q[vapply(q, in_dashboard_scope, TRUE)]
    if (length(q)) {
      qc <- quiz_catalog()
      out <- rbind(out, data.frame(item = q, title = qc$title[match(q, qc$item)] %||% q, type = "quiz",
                                   week = suppressWarnings(as.integer(sub("^quiz(\\d+).*$", "\\1", q)))))
    }
  }
  out$title[is.na(out$title)] <- out$item[is.na(out$title)]
  out[order(-out$week, out$item), ]
}

# Newest Poll Everywhere file for a quiz.
quiz_file_for <- function(item) {
  f <- list.files(pollev_dir(), pattern = "\\.csv$", full.names = TRUE)
  f <- f[tolower(quiz_name_from_file(f)) == tolower(item)]
  if (!length(f)) return(NA_character_)
  f[which.max(file.mtime(f))]
}

# One row per student. Columns: canvas_user_id, github, name, section, state,
# action, score, comment (feedback), late (late note), questions (JSON), detail,
# and last_* = what was posted last time.
# state: ready / changed since posted / posted / excused (posted) / not graded yet /
#        extension (waiting) / needs late review / no submission / no Canvas id
canvas_queue <- function(item, zero_missing = FALSE) {
  it <- item_info(item); st <- read_students()
  if (it$type == "quiz") q <- quiz_queue(item, zero_missing, st) else q <- hw_queue(item, it, zero_missing, st)
  if (is.null(q) || !nrow(q)) return(q)
  q$canvas_user_id <- as.character(q$canvas_user_id)
  # compare with what was sent last time
  lp <- last_posted(item)
  m <- match(q$canvas_user_id, lp$canvas_user_id)
  q$last_score <- lp$score[m]; q$last_action <- lp$action[m]; q$last_posted_at <- lp$posted_at[m]
  q$last_comment <- lp$comment[m]; q$last_late <- lp$late_comment[m]; q$last_questions <- lp$questions[m]
  has <- nzchar(q$action)
  same <- !is.na(m) & q$action == q$last_action &
          (q$action == "excuse" | (abs(q$score - q$last_score) < 1e-9 & q$comment == q$last_comment & q$late == q$last_late))
  same[is.na(same)] <- FALSE
  q$state[has] <- ifelse(same[has], "posted", ifelse(is.na(m[has]), "ready", "changed since posted"))
  q$state[has & same & q$action == "excuse"] <- "excused (posted)"
  noid <- has & (is.na(q$canvas_user_id) | q$canvas_user_id %in% c("", "NA"))
  q$state[noid] <- "no Canvas id"; q$action[noid] <- ""
  q[order(match(q$state, c("changed since posted", "ready", "no Canvas id", "needs late review", "not graded yet",
                            "extension (waiting)", "no submission", "posted", "excused (posted)")), q$name), ]
}

hw_queue <- function(item, it, zero_missing, st) {
  sn <- item_snapshot(item); if (is.null(sn)) return(NULL)
  asg <- item_assignments(item)
  # everyone's saved grades for this item
  g <- if (!is.null(asg)) do.call(rbind, lapply(unique(stats::na.omit(asg$grader)), function(gr) {
    x <- read_grades(gr, item); x[x$github %in% asg$github[asg$grader %in% gr], , drop = FALSE] })) else NULL
  r <- read_rubric(item); qs <- if (!is.null(r)) rubric_questions(r)$question else character()
  led <- read_ledger(); led_it <- led[led$item %in% item, ]
  tot <- if (it$type == "homework") late_totals(it$week, led) else NULL
  m <- match(tolower(sn$github), tolower(st$github))
  q <- data.frame(canvas_user_id = st$canvas_id[m], github = sn$github, name = st$name[m], section = sn$section,
                  state = "", action = "", score = NA_real_, comment = "", late = "", questions = "",
                  detail = sn$status, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(sn))) {
    s <- sn$status[i]; gh <- tolower(sn$github[i])
    late_note <- ""
    d <- led_it[led_it$username == gh, ]
    if (it$type == "homework" && nrow(d) && !is.na(d$minutes_late[1]) && d$minutes_late[1] > 0) {
      tm <- tot$total_minutes[match(gh, tot$username)]
      late_note <- late_comment(d$minutes_late[1], tm, allowance_minutes())
    }
    if (s == "excused (no grade)") { q$action[i] <- "excuse"; next }
    if (s == "extension (waiting)") { q$state[i] <- "extension (waiting)"; next }
    if (s == "needs review") { q$state[i] <- "needs late review"; next }
    if (s == "not loaded yet") { q$state[i] <- "not graded yet"
      q$detail[i] <- sprintf("%s hasn't opened it on the Grading tab yet", sn$grader[i] %||% "the CA"); next }
    if (s == "over allowance (0)") {
      q$action[i] <- "grade"; q$score[i] <- 0; q$late[i] <- late_note; next }
    if (s %in% graded_statuses && !is.na(sn$sha[i])) {
      mine <- if (is.null(g)) NULL else g[tolower(g$github) == gh, ]
      done <- !is.null(mine) && nrow(mine) && all(qs %in% mine$question) && !any(is.na(mine$points))
      if (!done) { q$state[i] <- "not graded yet"
        q$detail[i] <- sprintf("grader: %s", asg$grader[match(sn$github[i], asg$github)] %||% "not assigned"); next }
      mine <- mine[order(match(mine$question, qs)), ]
      q$action[i] <- "grade"; q$score[i] <- round(sum(mine$points), 3)
      q$comment[i] <- feedback_comment(mine); q$late[i] <- late_note
      q$questions[i] <- as.character(jsonlite::toJSON(mine[, c("question", "points", "max_points", "feedback")], digits = NA))
      next
    }
    # no submission / no repo
    q$state[i] <- "no submission"
    if (zero_missing) { q$action[i] <- "zero"; q$score[i] <- 0
      q$comment[i] <- "No submission was found for this assignment, so it receives a 0." }
  }
  q
}

quiz_queue <- function(item, zero_missing, st) {
  f <- quiz_file_for(item); if (is.na(f)) return(NULL)
  r <- quiz_scores(f, item)
  mt <- r$matched
  q <- data.frame(canvas_user_id = mt$canvas_id, github = mt$github, name = mt$name, section = st$section[match(mt$huid, st$huid)],
                  state = "", action = ifelse(mt$status == "post", "grade", ifelse(mt$status == "excused", "excuse", "")),
                  score = ifelse(mt$status == "post", mt$score, NA_real_), comment = "", late = "", questions = "",
                  detail = basename(f), stringsAsFactors = FALSE)
  q$state[mt$status == "extension (skip)"] <- "extension (waiting)"
  if (nrow(r$absent)) {
    a <- r$absent
    q <- rbind(q, data.frame(canvas_user_id = a$canvas_id, github = a$github, name = a$name, section = st$section[match(a$huid, st$huid)],
                             state = if (zero_missing) "" else "no submission", action = if (zero_missing) "zero" else "",
                             score = if (zero_missing) 0 else NA_real_, comment = "", late = "", questions = "",
                             detail = "not in the Poll Everywhere file"))
  }
  attr(q, "unmatched") <- nrow(r$unmatched)
  q
}

# --- sending -----------------------------------------------------------------

# The comments that go out, as two columns: c1 goes with the score, c2 is a
# second, separate comment (the late note). Canvas adds each comment below
# the earlier ones; nothing is overwritten.
#   first post:   c1 = the feedback, c2 = the late note (if late)
#   after that:   c1 = "### Regrade comment" (if points/feedback changed),
#                 c2 = the late note again only if it changed (a new late decision)
canvas_messages <- function(q) {
  c1 <- c2 <- character(nrow(q))
  for (i in seq_len(nrow(q))) {
    if (q$action[i] == "excuse") next
    again <- q$state[i] == "changed since posted" && identical(q$last_action[i], q$action[i])
    if (!again) { c1[i] <- q$comment[i]; c2[i] <- q$late[i]; next }
    fb_changed <- is.na(q$last_score[i]) || abs(q$score[i] - q$last_score[i]) > 1e-9 || q$comment[i] != q$last_comment[i]
    if (fb_changed && (nzchar(q$comment[i]) || nzchar(q$last_comment[i])))
      c1[i] <- regrade_comment(q$questions[i], q$last_questions[i], q$score[i], q$last_score[i])
    if (q$late[i] != q$last_late[i]) c2[i] <- q$late[i]
  }
  data.frame(c1 = c1, c2 = c2, stringsAsFactors = FALSE)
}

canvas_wait <- function(progress_url, token, timeout = 180) {
  t0 <- Sys.time()
  repeat {
    Sys.sleep(2)
    p <- httr2::request(progress_url) |> httr2::req_auth_bearer_token(token) |> httr2::req_perform() |> httr2::resp_body_json()
    if (p$workflow_state %in% c("completed", "failed")) return(p)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) stop("Canvas is still working on it after 3 minutes. Check Canvas, then use 'mark as posted' if it went through.")
  }
}

# Send the rows of a canvas_queue() that have an action and aren't "posted".
# mark_only = TRUE records them as posted without sending (posted another way).
# progress: optional function(done, total) for the progress bar.
# canvas_id: the Canvas assignment id, if it isn't in calendar.csv.
post_to_canvas <- function(item, q, user = current_user(), mark_only = FALSE, progress = NULL, canvas_id = NULL) {
  require_admin(user, "post to Canvas")
  it <- item_info(item)
  if (it$type == "quiz" && !is_quiz_grader(user)) stop("Only the quiz grader (course.yml quiz_grader) posts quizzes.")
  send <- q[nzchar(q$action) & q$state %in% c("ready", "changed since posted"), ]
  if (!nrow(send)) stop("Nothing to post.")
  cid <- if (!is.null(canvas_id) && nzchar(trimws(canvas_id))) trimws(canvas_id) else it$canvas_id
  if (!mark_only && (is.na(cid) || !nzchar(cid))) stop("Add the Canvas assignment id for ", item, " to calendar.csv (Calendar > Sync from Canvas).")
  msg <- canvas_messages(send); send$c1 <- msg$c1; send$c2 <- msg$c2
  posted <- rep(FALSE, nrow(send)); fails <- character()
  if (mark_only) posted[] <- TRUE else {
    token <- canvas_token(); if (!nzchar(token)) stop("No Canvas token on this computer (see docs/setup-canvas-token.md).")
    co <- read_course()
    chunks <- split(seq_len(nrow(send)), ceiling(seq_len(nrow(send)) / 25))
    call_canvas <- function(form) {
      resp <- httr2::request(co$canvas_url) |>
        httr2::req_url_path_append("api", "v1", "courses", co$canvas_course_id, "assignments", cid, "submissions", "update_grades") |>
        httr2::req_auth_bearer_token(token) |> httr2::req_method("POST") |> httr2::req_body_form(!!!form) |>
        httr2::req_perform() |> httr2::resp_body_json()
      if (is.null(resp$url)) stop("Canvas didn't return a progress link.")
      p <- canvas_wait(resp$url, token)
      if (p$workflow_state != "completed") stop(p$message %||% "Canvas said it failed.")
    }
    for (ch in chunks) {
      # call 1: score (or EXCUSED) + first comment
      form <- list()
      for (i in ch) {
        key <- sprintf("grade_data[%s]", send$canvas_user_id[i])
        if (send$action[i] == "excuse") form[[paste0(key, "[excuse]")]] <- "true"
        else form[[paste0(key, "[posted_grade]")]] <- fmt_pts(send$score[i])
        if (nzchar(send$c1[i])) form[[paste0(key, "[text_comment]")]] <- send$c1[i]
      }
      # call 2: the separate late-note comments
      form2 <- list()
      for (i in ch[nzchar(send$c2[ch])]) form2[[sprintf("grade_data[%s][text_comment]", send$canvas_user_id[i])]] <- send$c2[i]
      res <- tryCatch({ call_canvas(form); if (length(form2)) call_canvas(form2); TRUE },
        error = function(e) { fails <<- c(fails, sprintf("%s (%s)", paste(send$github[ch], collapse = ", "), conditionMessage(e))); FALSE })
      posted[ch] <- res
      if (!is.null(progress)) progress(max(ch), nrow(send))
    }
  }
  done <- send[posted, ]
  if (nrow(done)) {
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
    append_canvas_log(data.frame(item = item, canvas_user_id = done$canvas_user_id, github = done$github, name = done$name,
      action = done$action, score = done$score, comment = done$comment, late_comment = done$late,
      questions = done$questions,
      sent = if (mark_only) "" else ifelse(nzchar(done$c1) & nzchar(done$c2), paste0(done$c1, "\n\n-----\n\n", done$c2), paste0(done$c1, done$c2)),
      kind = if (mark_only) "marked" else ifelse(done$state == "changed since posted", "update", "first"),
      posted_at = now, posted_by = acting_username(user)))
    # keep the older records in step: late notices and EXCUSED
    if (it$type == "homework") {
      led <- read_ledger(); k <- led$item %in% item & led$username %in% tolower(done$github[nzchar(done$late)])
      if (any(k)) { led$posted[k] <- TRUE; led$posted_at[k] <- now; save_ledger(led) }
    }
    e <- read_exceptions(); k <- e$item == item & e$type == "excused" & tolower(e$github) %in% tolower(done$github[done$action == "excuse"])
    if (any(k)) { e$canvas_excused[k] <- TRUE; write_exceptions(e) }
  }
  log_action(acting_username(user), paste(if (mark_only) "marked as posted" else "posted to Canvas", item),
             sprintf("%d sent; problems: %s", nrow(done), paste(fails, collapse = "; ")))
  if (length(fails)) stop(sprintf("%d posted, but not: %s", nrow(done), paste(fails, collapse = "; ")))
  nrow(done)
}

# Counts per item, for My tasks and the overview.
canvas_summary <- function(user) {
  its <- canvas_items(user)
  if (!nrow(its)) return(data.frame(item = character(), ready = integer(), changed = integer(), waiting = integer(), posted = integer()))
  do.call(rbind, lapply(its$item, function(x) {
    q <- tryCatch(canvas_queue(x), error = function(e) NULL)
    if (is.null(q) || !nrow(q)) return(data.frame(item = x, ready = 0L, changed = 0L, waiting = 0L, posted = 0L))
    data.frame(item = x, ready = sum(q$state == "ready"), changed = sum(q$state == "changed since posted"),
               waiting = sum(q$state %in% c("not graded yet", "extension (waiting)", "needs late review")),
               posted = sum(q$state %in% c("posted", "excused (posted)")))
  }))
}
