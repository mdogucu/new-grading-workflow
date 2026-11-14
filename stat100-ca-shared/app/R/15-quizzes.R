# ---------------------------------------------------------------------------
# 15-quizzes.R — in-class quizzes (Poll Everywhere → Canvas), Mine only
#
# To-dos for quizzes go to the people in course.yml `quiz_grader` (mdogucu).
# 1. After class, download the Poll Everywhere gradebook for the quiz and put
#    it in admin/inputs/pollev/. Keep Poll Everywhere's file name, which starts
#    with the quiz name: quiz11b_gradebook_1773914.csv.
# 2. Quizzes tab: pick the file. Each Poll Everywhere row is matched to a
#    student by EMAIL (then by name). Rows that don't match (e.g. someone
#    "Unregistered", or a different first name) are listed; pick the student
#    once and the match is remembered in admin/config/pollev-matches.csv.
# 3. Score = the "Participation" column (a percent), as in the old script.
#    Excused students (Late & excused tab) are marked EXCUSED on Canvas
#    instead; students with an extension are skipped for now.
# 4. "Post to Canvas" sends everything in one go. The Canvas assignment is
#    found from the quiz name in the Canvas gradebook export ("quiz11b (1135126)")
#    or from calendar.csv (type quiz, canvas_id). Posted scores are recorded in
#    admin/data/quiz-posted.csv.
# ---------------------------------------------------------------------------

pollev_dir <- function() admin_path("inputs", "pollev")

quiz_name_from_file <- function(f) sub("^(quiz[0-9]+[a-z]?).*$", "\\1", basename(f), ignore.case = TRUE)

# Every quiz we know about: calendar rows of type "quiz" + quiz columns in the
# newest Canvas gradebook export. data.frame(item, title, canvas_id, student_due)
quiz_catalog <- function() {
  cal <- read_calendar()
  a <- if (!is.null(cal)) cal[cal$type %in% "quiz", c("item", "title", "canvas_id", "student_due")] else NULL
  cv <- newest_file(admin_path("inputs", "canvas-gradebook"))
  b <- NULL
  if (!is.na(cv)) {
    h <- names(suppressWarnings(read_any_encoding(cv, n_max = 1)))
    q <- h[grepl("^quiz", h, ignore.case = TRUE)]
    if (length(q)) b <- data.frame(item = tolower(sub("^(quiz[0-9]+[a-z]?).*$", "\\1", q, ignore.case = TRUE)),
                                   title = trimws(sub("\\s*\\(\\d+\\)\\s*$", "", q)),
                                   canvas_id = sub(".*\\((\\d+)\\)\\s*$", "\\1", q),
                                   student_due = as.POSIXct(NA))
  }
  out <- rbind(a, b[!b$item %in% a$item, ])
  if (is.null(out)) out <- data.frame(item = character(), title = character(), canvas_id = character(), student_due = as.POSIXct(character()))
  out[order(out$item), ]
}

read_pollev <- function(f) {
  d <- suppressWarnings(suppressMessages(read_any_encoding(f, name_repair = "unique_quiet")))
  names(d)[1] <- sub("^\\ufeff", "", names(d)[1])   # strip a byte-order mark
  out <- data.frame(first = trimws(d$`First name`), last = trimws(d$`Last name`),
                    email = tolower(trimws(d$Email)),
                    score = suppressWarnings(as.numeric(d$Participation)), stringsAsFactors = FALSE)
  out$row <- seq_len(nrow(out))
  out <- out[!grepl("^Average grade$", out$first %||% ""), ]          # Poll Everywhere's summary row
  out$last[is.na(out$last)] <- ""; out$email[is.na(out$email)] <- ""
  s <- read_staff(FALSE)                                              # the teaching team answering
  out[!(out$email %in% tolower(s$email %||% "")) & !(tolower(paste(out$first, out$last)) %in% tolower(s$name)), ]
}

read_pollev_matches <- function() {
  m <- read_csv_quiet(admin_path("config", "pollev-matches.csv"))
  if (is.null(m)) m <- data.frame(pollev_email = character(), pollev_name = character(), huid = character())
  m$pollev_email[is.na(m$pollev_email)] <- ""; m$pollev_name[is.na(m$pollev_name)] <- ""
  m
}

save_pollev_match <- function(pollev_email, pollev_name, huid, user = current_user()) {
  require_admin(user, "match a Poll Everywhere name")
  m <- read_pollev_matches()
  key <- if (nzchar(pollev_email %||% "")) tolower(pollev_email) else ""
  m <- m[!(tolower(m$pollev_email %||% "") == key & key != "") & !(m$pollev_name == pollev_name & key == ""), ]
  m <- rbind(m, data.frame(pollev_email = key, pollev_name = pollev_name, huid = huid))
  readr::write_csv(m, admin_path("config", "pollev-matches.csv"), na = "")
  invisible(TRUE)
}

# Match a Poll Everywhere file to the student list.
# Returns list(matched = data.frame(huid, canvas_id, name, github, score, status),
#              unmatched = data.frame(first, last, email, score),
#              absent = students not in the file)
quiz_scores <- function(f, item = quiz_name_from_file(f)) {
  pv <- read_pollev(f); st <- read_students()
  m <- read_pollev_matches()
  canvas_first_last <- function(n) {   # "Last, First Middle" -> "first last"
    last <- trimws(sub(",.*$", "", n)); first <- strsplit(trimws(sub("^[^,]*,", "", n)), " ")[[1]][1]
    tolower(paste(first, last))
  }
  st$fl <- vapply(st$name, canvas_first_last, "")
  # "Unregistered" rows have no name or email: a match for those is kept for
  # this quiz only (key "quiz11b#row 76"); real names/emails are remembered for good.
  pv$name <- ifelse(pv$email == "" & grepl("^unregistered", pv$first, ignore.case = TRUE),
                    paste0(item, "#row ", pv$row), trimws(paste(pv$first, pv$last)))
  pv$huid <- NA_character_
  for (i in seq_len(nrow(pv))) {
    h <- m$huid[(nzchar(m$pollev_email) & m$pollev_email == pv$email[i]) | (!nzchar(m$pollev_email) & m$pollev_name == pv$name[i])]
    h <- unique(h[!is.na(h)])
    if (!length(h) && nzchar(pv$email[i])) h <- st$huid[!is.na(st$email) & tolower(st$email) == pv$email[i]]
    if (!length(h)) h <- st$huid[!is.na(st$fl) & st$fl == tolower(pv$name[i])]
    if (length(h) == 1) pv$huid[i] <- h
  }
  pv$score[is.na(pv$score)] <- 0
  # someone answering twice (two accounts): keep the best score
  ok <- pv[!is.na(pv$huid), ]
  ok <- ok[order(-ok$score), , drop = FALSE]; ok <- ok[!duplicated(ok$huid), , drop = FALSE]
  ok <- ok[ok$huid %in% st$huid, , drop = FALSE]                     # e.g. matched to someone since dropped
  matched <- data.frame(huid = ok$huid, canvas_id = st$canvas_id[match(ok$huid, st$huid)],
                        name = st$name[match(ok$huid, st$huid)], github = st$github[match(ok$huid, st$huid)],
                        score = ok$score, status = "post")
  exc <- read_exceptions(item)
  ex_g <- tolower(exc$github[exc$type == "excused"]); ext_g <- tolower(exc$github[exc$type == "extension"])
  matched$status[tolower(matched$github) %in% ext_g] <- "extension (skip)"
  absent <- st[!st$huid %in% matched$huid, c("huid", "canvas_id", "name", "github")]
  # excused students are marked EXCUSED whether or not they're in the file
  exc_rows <- st[tolower(st$github) %in% ex_g, c("huid", "canvas_id", "name", "github")]
  matched <- matched[!matched$huid %in% exc_rows$huid, ]
  absent <- absent[!absent$huid %in% exc_rows$huid, ]
  if (nrow(exc_rows)) matched <- rbind(matched, data.frame(exc_rows, score = NA, status = "excused"))
  list(matched = matched, unmatched = pv[is.na(pv$huid), c("first", "last", "email", "score", "name")], absent = absent)
}

quiz_posted <- function() {
  p <- read_csv_quiet(admin_path("data", "quiz-posted.csv"))
  if (is.null(p)) p <- data.frame(item = character(), huid = character(), score = character(), excused = character(), posted_at = character(), posted_by = character())
  p
}

# Send scores (and EXCUSED) for one quiz to Canvas in one request.
post_quiz <- function(item, canvas_id, rows, zero_for_absent = NULL, user = current_user()) {
  require_admin(user, "post quiz grades to Canvas")
  token <- canvas_token(); if (!nzchar(token)) stop("No Canvas token on this computer.")
  co <- read_course(); if (!nzchar(co$canvas_course_id %||% "")) stop("Add canvas_course_id to course.yml.")
  if (is.na(canvas_id) || !nzchar(canvas_id)) stop("Don't know the Canvas assignment for ", item, ".")
  send <- rows[rows$status %in% c("post", "excused") & !is.na(rows$canvas_id), ]
  if (!is.null(zero_for_absent) && nrow(zero_for_absent))
    send <- rbind(send, data.frame(zero_for_absent[!is.na(zero_for_absent$canvas_id), ], score = 0, status = "post"))
  if (!nrow(send)) stop("Nothing to post.")
  form <- list()
  for (i in seq_len(nrow(send))) {
    key <- sprintf("grade_data[%s]", send$canvas_id[i])
    if (send$status[i] == "excused") form[[paste0(key, "[excuse]")]] <- "true"
    else form[[paste0(key, "[posted_grade]")]] <- as.character(send$score[i])
  }
  req <- httr2::request(co$canvas_url) |>
    httr2::req_url_path_append("api", "v1", "courses", co$canvas_course_id, "assignments", canvas_id, "submissions", "update_grades") |>
    httr2::req_auth_bearer_token(token) |> httr2::req_method("POST") |> httr2::req_body_form(!!!form)
  httr2::req_perform(req)
  p <- quiz_posted(); p <- p[p$item != item | !p$huid %in% send$huid, ]
  p <- rbind(p, data.frame(item = item, huid = send$huid, score = ifelse(send$status == "excused", "", send$score),
                           excused = send$status == "excused", posted_at = format(Sys.time(), "%Y-%m-%d %H:%M"),
                           posted_by = acting_username(user)))
  readr::write_csv(p, admin_path("data", "quiz-posted.csv"), na = "")
  e <- read_exceptions(); k <- e$item == item & e$type == "excused" & tolower(e$github) %in% tolower(send$github[send$status == "excused"])
  if (any(k)) { e$canvas_excused[k] <- TRUE; write_exceptions(e) }
  log_action(acting_username(user), paste("posted quiz", item), sprintf("%d scores, %d excused", sum(send$status == "post"), sum(send$status == "excused")))
  nrow(send)
}

# Record a quiz as posted without sending anything (it was posted another way).
mark_quiz_posted <- function(item, user = current_user()) {
  require_admin(user, "mark a quiz as posted")
  p <- quiz_posted()
  p <- rbind(p, data.frame(item = item, huid = "", score = "", excused = "", posted_at = format(Sys.time(), "%Y-%m-%d %H:%M"),
                           posted_by = paste(acting_username(user), "(marked, not sent)")))
  readr::write_csv(p, admin_path("data", "quiz-posted.csv"), na = "")
  invisible(TRUE)
}

# Quiz files that haven't been posted yet.
unposted_quiz_files <- function() {
  f <- list.files(pollev_dir(), pattern = "\\.csv$", full.names = TRUE)
  if (!length(f)) return(character())
  done <- unique(quiz_posted()$item)
  f[!tolower(quiz_name_from_file(f)) %in% tolower(done)]
}
