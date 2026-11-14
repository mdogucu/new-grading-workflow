# ---------------------------------------------------------------------------
# 13-rubrics.R — making and checking rubrics
#
# A rubric is stat100-ca-shared/rubrics/<item>-rubric.csv in gradetools format:
#   name, total_points, prompt_code, prompt_message, feedback, points_to_remove
# One block of rows per question ("Question 1ii"); total_points on the first
# row of the block. Rows named "all_questions" can be used on every question
# (their points_to_remove is a PERCENT of the question).
#
# The Rubrics tab builds the list of questions from the week's template
# (homework/<item>-*.qmd in the template repo), so rubric names always match
# what the grading screen finds in students' files.
# ---------------------------------------------------------------------------

default_all_questions <- function() data.frame(
  name = "all_questions", prompt_code = c("0", "99", "404"),
  prompt_message = c("Correct", "Tidy style", "Answer missing"),
  feedback = c("All correct!", "Please be sure to follow the tidy style conventions for coding.",
               "No answer was provided for this question."),
  points_to_remove = c(0, 0.1, 100), stringsAsFactors = FALSE)

# Questions (and parts) in the week's template for this item.
template_questions <- function(item) {
  it <- item_info(item)
  t <- template_copy(it$repo)
  if (is.na(t)) stop("Couldn't download the template repo ", it$repo, " from GitHub.")
  f <- find_item_file(t, item, it$folder)
  if (is.na(f)) stop("No ", item, "*.qmd in the template's ", it$folder, "/ folder.")
  ch <- split_questions(readLines(f, warn = FALSE))
  if (is.null(ch)) stop("No \"## Question\" headings found in ", basename(f))
  # A question with parts is graded part by part; otherwise as a whole.
  has_parts <- unique(sub("^(question\\d+).*", "\\1", ch$key[ch$part != ""]))
  keep <- ch[ch$part != "" | !ch$key %in% has_parts, ]
  paste0("Question ", sub("^question", "", keep$key))
}

# Split a rubric file into questions (name, total_points) and options.
rubric_parts <- function(r) {
  q <- rubric_questions(r)
  opts <- r[, c("name", "prompt_code", "prompt_message", "feedback", "points_to_remove")]
  list(questions = data.frame(name = q$question, total_points = q$max_points, stringsAsFactors = FALSE),
       options = as.data.frame(opts, stringsAsFactors = FALSE))
}

# Back to one gradetools table (total_points only on each question's first row).
rubric_table <- function(questions, options) {
  out <- lapply(questions$name, function(qn) {
    o <- options[options$name == qn, ]
    if (!nrow(o)) o <- data.frame(name = qn, prompt_code = "1", prompt_message = "", feedback = "", points_to_remove = 0)
    o$total_points <- c(questions$total_points[questions$name == qn], rep(NA, nrow(o) - 1))
    o
  })
  gen <- options[options$name == "all_questions", ]
  if (nrow(gen)) { gen$total_points <- NA; out <- c(out, list(gen)) }
  r <- do.call(rbind, out)
  r[, c("name", "total_points", "prompt_code", "prompt_message", "feedback", "points_to_remove")]
}

# Problems with a rubric. Returns a character vector (empty = all good).
check_rubric <- function(item, r = read_rubric(item), expected = NULL) {
  if (is.null(r)) return(paste("No rubric file yet:", basename(rubric_path(item))))
  probs <- character()
  q <- rubric_questions(r)
  if (!is.null(expected)) {
    miss <- setdiff(question_key(expected), question_key(q$question))
    extra <- setdiff(question_key(q$question), question_key(expected))
    if (length(miss)) probs <- c(probs, paste("In the homework but not the rubric:", paste(expected[question_key(expected) %in% miss], collapse = ", ")))
    if (length(extra)) probs <- c(probs, paste("In the rubric but not found in the homework:", paste(q$question[question_key(q$question) %in% extra], collapse = ", ")))
  }
  if (any(is.na(q$max_points) | q$max_points <= 0)) probs <- c(probs, paste("No points for:", paste(q$question[is.na(q$max_points) | q$max_points <= 0], collapse = ", ")))
  o <- r[r$name != "all_questions", ]
  dup <- paste(o$name, o$prompt_code)[duplicated(paste(o$name, o$prompt_code))]
  if (length(dup)) probs <- c(probs, paste("Option code used twice:", paste(unique(dup), collapse = ", ")))
  blank <- o$name[is.na(o$prompt_message) | o$prompt_message == "" | is.na(o$feedback) | o$feedback == ""]
  if (length(blank)) probs <- c(probs, paste("Options missing a message or feedback in:", paste(unique(blank), collapse = ", ")))
  if (!any(r$name == "all_questions")) probs <- c(probs, "No all_questions rows (Correct / Tidy style / Answer missing).")
  probs
}

save_rubric <- function(item, questions, options, user = current_user()) {
  require_admin(user, "save a rubric")
  r <- rubric_table(questions, options)
  f <- rubric_path(item)
  if (grepl("^sec", item) && basename(f) == "section-rubric.csv") f <- shared_path("rubrics", paste0(item, "-rubric.csv"))
  if (file.exists(f)) {
    old <- shared_path("rubrics", "old"); dir.create(old, showWarnings = FALSE)
    file.copy(f, file.path(old, sub("\\.csv$", paste0("-", format(Sys.time(), "%Y%m%d-%H%M"), ".csv"), basename(f))))
  }
  readr::write_csv(r, f, na = "")
  log_action(user$username, "saved rubric", basename(f))
  invisible(f)
}
