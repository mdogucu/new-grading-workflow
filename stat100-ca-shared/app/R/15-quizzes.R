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
# 4. Post to Canvas tab: pick the quiz and post (R/17-canvas-post.R). The
#    Canvas assignment comes from calendar.csv (type quiz, canvas_id) or the
#    quiz name in the Canvas gradebook export ("quiz11b (1135126)"). What was
#    sent is recorded in admin/data/canvas-posted.csv. A match made after
#    posting shows up there as a new score to post.
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
  if (!is.null(out)) out <- out[in_dashboard_scope(out$item), ]
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
  out$screen <- trimws(d$`Screen name` %||% rep("", nrow(d))); out$screen[is.na(out$screen)] <- ""
  # Everything the person typed or picked (all question columns), for clues
  # like "guest012 is Jane Doe" or an email typed into an answer.
  skip <- c("First name", "Last name", "Screen name", "Custom report ID", "Email", "Rank", "Attendance", "Grade",
            "Participation", "Total points possible", "Total points earned", "Total answered")
  qcols <- names(d)[!names(d) %in% skip & !grepl("^Points earned|^Responded at", names(d))]
  out$answers <- vapply(seq_len(nrow(d)), function(i) {
    v <- vapply(qcols, function(k) as.character(d[[k]][i] %||% NA), "")
    paste(sprintf("%s: %s", trimws(gsub('^"|"$', "", qcols))[!is.na(v) & v != ""], v[!is.na(v) & v != ""]), collapse = "\n")
  }, "")
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
  # "Unregistered" rows have no name or email, only a screen name the student
  # typed ("Valerie D") or one Poll Everywhere made up ("guest012").
  #  - a typed screen name is remembered for good (key "screen:Valerie D")
  #  - a guestNNN name changes every time, so that match is for this quiz only
  unreg <- pv$email == "" & grepl("^unregistered", pv$first, ignore.case = TRUE)
  guest <- grepl("^guest\\d*$", pv$screen, ignore.case = TRUE) | pv$screen == ""
  pv$name <- ifelse(!unreg, trimws(paste(pv$first, pv$last)),
                    ifelse(guest, paste0(item, "#", ifelse(pv$screen == "", paste("row", pv$row), pv$screen)),
                           paste0("screen:", pv$screen)))
  pv$huid <- NA_character_
  for (i in seq_len(nrow(pv))) {
    h <- m$huid[(nzchar(m$pollev_email) & m$pollev_email == pv$email[i]) | (!nzchar(m$pollev_email) & m$pollev_name == pv$name[i])]
    h <- unique(h[!is.na(h)])
    if (!length(h) && nzchar(pv$email[i])) h <- st$huid[!is.na(st$email) & tolower(st$email) == pv$email[i]]
    if (!length(h)) h <- st$huid[!is.na(st$fl) & st$fl == tolower(pv$name[i])]
    # an unregistered screen name that is exactly a student's full name
    if (!length(h) && unreg[i] && !guest[i]) h <- st$huid[!is.na(st$fl) & st$fl == tolower(pv$screen[i])]
    if (length(h) == 1) pv$huid[i] <- h
  }
  # Best guesses for the rows still unmatched (shown pre-selected, not saved
  # until you click "Save match"): "Valerie D" = first name + last initial,
  # "Ashi" = first name or GitHub username, or a student's email or name
  # appearing anywhere in their answers.
  guess_for <- function(i) {
    sc <- tolower(pv$screen[i]); txt <- tolower(pv$answers[i])
    first <- sub(" .*$", "", st$fl); last <- sub("^\\S+ ", "", st$fl)
    if (nzchar(sc) && !guest[i]) {
      p <- strsplit(sc, "\\s+")[[1]]
      if (length(p) >= 2) { c1 <- st$huid[first == p[1] & startsWith(last, sub("\\.$", "", p[length(p)]))]
        if (length(c1) == 1) return(c(c1, sprintf("screen name \"%s\" = first name + last initial", pv$screen[i]))) }
      c2 <- st$huid[first == p[1]]
      if (length(c2) == 1) return(c(c2, sprintf("screen name \"%s\" = first name", pv$screen[i])))
      c3 <- st$huid[!is.na(st$github) & startsWith(tolower(st$github), gsub("\\s+", "", sc))]
      if (length(c3) == 1) return(c(c3, sprintf("screen name \"%s\" matches a GitHub username", pv$screen[i])))
    }
    if (nzchar(txt)) {
      e <- !is.na(st$email) & nzchar(st$email) & vapply(tolower(st$email), function(x) grepl(x, txt, fixed = TRUE), TRUE)
      if (sum(e) == 1) return(c(st$huid[e], "their email appears in an answer"))
      n <- vapply(st$fl, function(x) nzchar(x) && grepl(x, txt, fixed = TRUE), TRUE)
      if (sum(n) == 1) return(c(st$huid[n], "their name appears in an answer"))
    }
    c(NA_character_, "")
  }
  pv$guess <- NA_character_; pv$guess_why <- ""
  for (i in which(is.na(pv$huid))) { g <- guess_for(i); pv$guess[i] <- g[1]; pv$guess_why[i] <- g[2] }
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
  list(matched = matched, unmatched = pv[is.na(pv$huid), c("first", "last", "email", "screen", "answers", "score", "name", "guess", "guess_why")], absent = absent)
}

# Quiz files with nothing posted yet (posting: R/17-canvas-post.R).
unposted_quiz_files <- function() {
  f <- list.files(pollev_dir(), pattern = "\\.csv$", full.names = TRUE)
  f <- f[in_dashboard_scope(tolower(quiz_name_from_file(f)))]
  if (!length(f)) return(character())
  done <- unique(tolower(read_canvas_log()$item))
  f[!tolower(quiz_name_from_file(f)) %in% done]
}
