# ---------------------------------------------------------------------------
# 14-exceptions.R — extensions and excused work, entered by an admin
#
# admin/config/exceptions.csv — one row per student per item:
#   item, github, type, new_due, note, added_by, added_at, canvas_excused
#   type = "extension": this student's deadline is new_due instead of the
#          calendar's (work up to then is on time; late minutes, if any,
#          count from new_due). Until new_due the student stays on their
#          grader's list marked "extension — don't grade yet"; after it,
#          their commits are read again (refreshed_at) and grading opens.
#   type = "excused":  the student doesn't do this item. Not graded, not
#          assigned, and on Canvas the submission is marked EXCUSED (neither
#          full points nor 0). canvas_excused = TRUE once that's been done.
# Can be entered before the deadline or after (the graded commit and the
# graders' lists are updated right away if submissions were already found).
# ---------------------------------------------------------------------------

exceptions_path <- function() admin_path("config", "exceptions.csv")
exception_cols <- c("item", "github", "type", "new_due", "note", "added_by", "added_at", "canvas_excused", "refreshed_at")

read_exceptions <- function(item = NULL) {
  e <- read_csv_quiet(exceptions_path())
  if (is.null(e)) e <- as.data.frame(stats::setNames(replicate(length(exception_cols), character(), simplify = FALSE), exception_cols))
  for (k in setdiff(exception_cols, names(e))) e[[k]] <- if (nrow(e)) NA_character_ else character()
  e$type <- tolower(trimws(e$type)); e$github <- trimws(e$github)
  e$new_due <- parse_when(e$new_due)
  e$canvas_excused <- as_flag(e$canvas_excused)
  if (!is.null(item)) e <- e[e$item %in% item, ]
  e[, exception_cols]
}

write_exceptions <- function(e) {
  e$new_due <- ifelse(is.na(e$new_due), "", format(e$new_due, "%Y-%m-%d %H:%M", tz = "America/New_York"))
  readr::write_csv(e[, exception_cols], exceptions_path(), na = "")
}

# Recompute the item's graded commits if submissions were already found.
# Sections: update the CA rosters (my-exceptions.csv) and that CA's list right
# away (excused: off the list; extension: "not yet"); the list is re-read
# from GitHub the next time the CA opens it.
refresh_item_after_exception <- function(item, github = NULL) {
  if (!is.null(read_csv_quiet(admin_path("data", "snapshots", paste0(item, "-students.csv"))))) compute_snapshot(item)
  if (grepl("^sec", item)) {
    try(publish_section_rosters(), silent = TRUE)
    if (!is.null(github)) try({
      g <- section_graders()$grader[tolower(section_graders()$github) == tolower(github)][1]
      f <- file.path(course_root(), "graders", grader_folder_name(g), "grading", item, "assignment.csv")
      a <- read_csv_quiet(f)
      if (!is.null(a) && nrow(a)) {
        x <- read_exceptions(item); x <- x[tolower(x$github) == tolower(github), ]
        k <- tolower(a$github) == tolower(github)
        if (nrow(x) && x$type[1] == "excused") a <- a[!k, ]
        else if (nrow(x) && x$type[1] == "extension" && !is.na(x$new_due[1]) && x$new_due[1] > now_et()) {
          a$sha[k] <- NA; a$status[k] <- "waiting"
          a$wait_until[k] <- format(x$new_due[1], "%Y-%m-%d %H:%M", tz = "America/New_York")
          a$deadline[k] <- a$wait_until[k]
        }
        a$built_at <- ""                                   # re-read on the CA's next visit
        readr::write_csv(a, f, na = "")
      }
    }, silent = TRUE)
  }
}

add_exception <- function(item, github, type, new_due = NA, note = "", user = current_user()) {
  require_admin(user, "add an extension or excuse")
  if (!type %in% c("extension", "excused")) stop("type must be extension or excused")
  if (type == "extension" && is.na(new_due)) stop("Pick the new deadline for the extension.")
  e <- read_exceptions()
  e <- e[!(e$item == item & tolower(e$github) == tolower(github)), ]
  e[nrow(e) + 1, ] <- NA
  k <- nrow(e)
  e$item[k] <- item; e$github[k] <- github; e$type[k] <- type
  e$new_due[k] <- if (type == "extension") as.POSIXct(new_due, tz = "America/New_York") else NA
  e$note[k] <- note; e$added_by[k] <- acting_username(user)
  e$added_at[k] <- format(Sys.time(), "%Y-%m-%d %H:%M"); e$canvas_excused[k] <- FALSE
  write_exceptions(e)
  log_action(acting_username(user), paste("exception", item), sprintf("%s: %s %s %s", github, type,
             if (type == "extension") format(new_due, "%Y-%m-%d %H:%M") else "", note))
  refresh_item_after_exception(item, github)
  invisible(TRUE)
}

remove_exception <- function(item, github, user = current_user()) {
  require_admin(user, "remove an extension or excuse")
  e <- read_exceptions()
  was <- e[e$item == item & tolower(e$github) == tolower(github), ]
  if (nrow(was) && was$type[1] == "excused" && isTRUE(was$canvas_excused[1]))
    tryCatch(canvas_excuse(item, github, excuse = FALSE), error = function(err) NULL)   # un-excuse on Canvas too
  write_exceptions(e[!(e$item == item & tolower(e$github) == tolower(github)), ])
  log_action(acting_username(user), paste("exception removed", item), github)
  refresh_item_after_exception(item, github)
  invisible(TRUE)
}

# Mark (or un-mark) one student's submission as excused on Canvas.
canvas_excuse <- function(item, github, excuse = TRUE) {
  it <- item_info(item)
  if (is.na(it$canvas_id)) stop("Add the Canvas assignment id for ", item, " to calendar.csv.")
  st <- read_students(); cid <- st$canvas_id[match(tolower(github), tolower(st$github))]
  if (is.na(cid)) stop("No Canvas id for ", github)
  token <- canvas_token(); if (!nzchar(token)) stop("No Canvas token on this computer.")
  co <- read_course()
  req <- httr2::request(co$canvas_url) |>
    httr2::req_url_path_append("api", "v1", "courses", co$canvas_course_id, "assignments", it$canvas_id, "submissions", cid) |>
    httr2::req_auth_bearer_token(token) |> httr2::req_method("PUT") |>
    httr2::req_body_form(`submission[excuse]` = if (excuse) "true" else "false")
  httr2::req_perform(req)
  invisible(TRUE)
}
