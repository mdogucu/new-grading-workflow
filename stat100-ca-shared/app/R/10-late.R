# ---------------------------------------------------------------------------
# 10-late.R — late homework: review, the 48-hour allowance, Canvas notices
#
# POLICY (course.yml late_allowance_hours, normally 48): every student has a
# semester-wide late allowance. Each late homework uses up minutes. A homework
# that takes the running total over the allowance gets a 0 (and isn't graded).
#
# SECTIONS: no minutes and no allowance. Late section work is flagged on the
# Late work tab and an admin decides: "Allowed" (grade the late work, e.g. the
# student was sick) or "Not allowed" (grade the version at the deadline).
#
# HOW IT WORKS for a homework item:
#   Find submissions  saves every student commit to
#                     admin/data/snapshots/<item>-commits.csv
#   Anyone with commits after the deadline shows up on the Late work tab as
#   "needs review". For each one, an admin decides:
#     * Late           grade the work up to a chosen commit, count N minutes
#     * Extension      grade up to a chosen commit, count 0 minutes
#                      (special permission given after the fact)
#     * Not late       grade the version at the deadline, count 0 minutes
#                      (the later commits weren't the homework)
#   Decisions go in admin/data/late-ledger.csv (one row per student per week).
#   The graded commit in the snapshot (and in graders' lists, if already
#   assigned) is updated right away.
#   Assign graders waits until every late student is reviewed.
#   Late notes (late_comment(): minutes late, running total, what's left) are
#   posted as their own Canvas comment, after the feedback comment, when grades are posted
#   on the Post to Canvas tab (R/17-canvas-post.R). Over the allowance = 0.
# ---------------------------------------------------------------------------

ledger_path <- function() admin_path("data", "late-ledger.csv")

ledger_cols <- c("week", "item", "repo_name", "username", "repo_url", "deadline", "last_commit_et",
                 "suggested_minutes", "grade_through_minutes", "minutes_late", "decision", "note",
                 "reviewed_by", "reviewed_at", "posted", "posted_at")

read_ledger <- function() {
  l <- read_csv_quiet(ledger_path())
  if (is.null(l)) l <- as.data.frame(stats::setNames(replicate(length(ledger_cols), character(), simplify = FALSE), ledger_cols))
  for (k in setdiff(ledger_cols, names(l))) l[[k]] <- if (nrow(l)) NA_character_ else character()
  l$week <- as.integer(l$week)
  for (k in c("suggested_minutes", "grade_through_minutes", "minutes_late")) l[[k]] <- suppressWarnings(as.numeric(l[[k]]))
  l$posted <- as_flag(l$posted)
  l$decision[l$decision %in% "excused"] <- "extension"   # older name for "late, no minutes counted"
  l$username <- tolower(trimws(l$username))
  l[, ledger_cols]
}

save_ledger <- function(l) {
  l <- l[order(l$week, l$username), ledger_cols]
  tmp <- paste0(ledger_path(), ".tmp")
  readr::write_csv(l, tmp, na = "")
  file.rename(tmp, ledger_path())
}

allowance_minutes <- function() as.numeric(read_course()$late_allowance_hours %||% 48) * 60

# Running late minutes per student, through `week`.
late_totals <- function(through_week = Inf, ledger = read_ledger()) {
  l <- ledger[!is.na(ledger$minutes_late) & ledger$week <= through_week, ]
  if (!nrow(l)) return(data.frame(username = character(), total_minutes = numeric()))
  stats::aggregate(minutes_late ~ username, data = l, FUN = sum) |>
    stats::setNames(c("username", "total_minutes"))
}

# "72 minutes (1 hour 12 minutes)" — same wording as the old script.
fmt_minutes <- function(m) {
  m <- round(m); h <- m %/% 60; r <- m %% 60
  pl <- function(n, u) paste0(n, " ", u, ifelse(n == 1, "", "s"))
  hm <- ifelse(h == 0, pl(r, "minute"), ifelse(r == 0, pl(h, "hour"), paste(pl(h, "hour"), pl(r, "minute"))))
  ifelse(h == 0, hm, paste0(pl(m, "minute"), " (", hm, ")"))
}

read_commits <- function(item) {
  c <- read_csv_quiet(admin_path("data", "snapshots", paste0(item, "-commits.csv")))
  if (is.null(c)) return(NULL)
  c$time <- as.POSIXct(c$time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  c
}

# --- recompute graded commits from commits + decisions ----------------------

compute_snapshot <- function(item) {
  it <- item_info(item)
  base <- read_csv_quiet(admin_path("data", "snapshots", paste0(item, "-students.csv")))
  cm <- read_commits(item)
  if (is.null(base) || is.null(cm)) return(NULL)
  dl <- it$student_due
  hw <- it$type == "homework"
  led <- read_ledger(); led <- led[led$item %in% item, ]
  exc <- read_exceptions(item)                  # extensions / excused (R/14-exceptions.R)
  if (hw) {
    tot_before <- late_totals(it$week - 1)
  }
  out <- lapply(seq_len(nrow(base)), function(i) {
    b <- base[i, ]
    mine <- cm[tolower(cm$github) == tolower(b$github), ]
    row <- data.frame(github = b$github, section = b$section, repo = b$repo, sha = NA_character_,
                      commit_time = NA_character_, suggested_minutes = NA_real_, minutes_late = NA_real_,
                      status = b$repo_status, stringsAsFactors = FALSE)
    x <- exc[tolower(exc$github) == tolower(b$github), ]
    if (nrow(x) && x$type[1] == "excused") { row$status <- "excused (no grade)"; return(row) }
    # An extension moves this student's deadline; everything else works the same.
    dl <- if (nrow(x) && x$type[1] == "extension" && !is.na(x$new_due[1])) x$new_due[1] else dl
    # Still inside the extension: stays on the grader's list, but marked "wait".
    # After the extension ends, update_extension_students() re-reads their
    # commits and the graded version is picked as usual.
    if (nrow(x) && x$type[1] == "extension" && !is.na(x$new_due[1]) && x$new_due[1] > now_et()) {
      row$status <- "extension (waiting)"; return(row)
    }
    if (b$repo_status != "ok") return(row)
    pick <- function(cutoff) { k <- mine[mine$time <= cutoff, ]; if (nrow(k)) k[which.max(k$time), ] else NULL }
    after <- mine[mine$time > dl, ]
    row$suggested_minutes <- if (nrow(after)) ceiling(as.numeric(difftime(max(after$time), dl, units = "mins"))) else 0
    set_pick <- function(p) if (!is.null(p)) { row$sha <<- p$sha; row$commit_time <<- format(p$time, "%Y-%m-%d %H:%M") }
    if (!nrow(after)) {
      set_pick(pick(dl)); row$minutes_late <- 0
      row$status <- if (is.na(row$sha)) "no submission" else "on time"
      return(row)
    }
    d <- led[led$username == tolower(b$github), ]
    if (!nrow(d) || is.na(d$minutes_late[1])) {
      set_pick(pick(max(mine$time)))           # provisional: newest commit
      row$status <- "needs review"
      return(row)
    }
    d <- d[1, ]
    gtm <- if (is.na(d$grade_through_minutes)) d$minutes_late else d$grade_through_minutes
    set_pick(pick(dl + gtm * 60))
    row$minutes_late <- d$minutes_late
    if (!hw) {                                  # sections: allowed or not, nothing counted
      row$status <- if (is.na(row$sha)) "no submission" else if (gtm > 0) "late (allowed)" else "deadline version"
      return(row)
    }
    prev <- tot_before$total_minutes[match(tolower(b$github), tot_before$username)]
    total <- (if (is.na(prev)) 0 else prev) + d$minutes_late
    row$status <- if (is.na(row$sha)) "no submission"
      else if (d$minutes_late > 0 && total > allowance_minutes()) "over allowance (0)"
      else if (d$minutes_late > 0) "late"
      else if (gtm > 0) "extension"
      else "deadline version"
    row
  })
  sn <- do.call(rbind, out)
  readr::write_csv(sn, admin_path("data", "snapshots", paste0(item, ".csv")), na = "")
  sync_assignment(item, sn)
  sn
}

graded_statuses <- c("on time", "late", "extension", "late (allowed)", "deadline version", "extension (waiting)")
gradeable <- function(sn) sn$status %in% graded_statuses & (!is.na(sn$sha) | sn$status == "extension (waiting)")

# The grader's own list: usernames, commits, and "wait_until" for students
# still inside an extension (graders can't see exceptions.csv).
grader_list <- function(asg, g) {
  mine <- asg[asg$grader %in% g, c("item", "github", "repo", "sha")]
  ex <- read_exceptions(unique(asg$item))
  ex <- ex[ex$type == "extension" & !is.na(ex$new_due) & ex$new_due > now_et(), ]
  w <- ex$new_due[match(tolower(mine$github), tolower(ex$github))]
  mine$wait_until <- ifelse(is.na(w), "", format(w, "%Y-%m-%d %H:%M", tz = "America/New_York"))
  mine$sha[nzchar(mine$wait_until)] <- NA        # nothing to grade yet
  mine
}

# If graders were already assigned, keep their lists right after a change:
#   * graded commit changed (late decision, extension)  -> update the commit
#   * no longer graded (excused, over allowance, ...)    -> take them off the list
#   * newly gradeable (e.g. extension, work now counts)  -> give them to the
#     least-loaded grader with no conflict (section: their section leader)
sync_assignment <- function(item, sn) {
  asg <- read_assignments(item)
  if (is.null(asg)) return(invisible())
  it <- item_info(item); changed <- character()
  ok <- sn[gradeable(sn), ]
  # commits
  new <- ok$sha[match(asg$github, ok$github)]
  k <- !is.na(new) & (is.na(asg$sha) | asg$sha == "" | new != asg$sha)
  if (any(k)) { asg$sha[k] <- new[k]; changed <- c(changed, asg$github[k]) }
  # removals
  gone <- !asg$github %in% ok$github
  if (any(gone)) { changed <- c(changed, asg$github[gone]); touched <- asg$grader[gone]; asg <- asg[!gone, ] } else touched <- character()
  # additions
  add <- ok[!ok$github %in% asg$github, ]
  if (nrow(add)) {
    coi <- read_coi(); staff <- read_staff()
    for (r in seq_len(nrow(add))) {
      g <- NA
      if (it$type == "section") { sec <- read_sections(); g <- sec$leader[match(add$section[r], sec$section)] }
      blocked <- tolower(coi$grader[tolower(coi$student) == tolower(add$github[r])])
      if (is.na(g) || tolower(g) %in% blocked) {
        cand <- setdiff(active_graders(staff), blocked)
        load <- table(factor(asg$grader, levels = cand))
        g <- if (length(cand)) names(load)[which.min(load)] else NA
      }
      asg <- rbind(asg, data.frame(item = item, github = add$github[r], section = add$section[r], repo = add$repo[r],
                                   sha = add$sha[r], commit_time = add$commit_time[r], minutes_late = add$minutes_late[r],
                                   status = add$status[r], grader = g))
      changed <- c(changed, add$github[r])
    }
  }
  asg$status <- sn$status[match(asg$github, sn$github)]
  readr::write_csv(asg, admin_path("data", "assignments", paste0(item, ".csv")), na = "")
  for (g in unique(stats::na.omit(c(asg$grader, touched)))) {
    d <- file.path(course_root(), "graders", grader_folder_name(g), "grading", item)
    if (!dir.exists(d)) next
    readr::write_csv(grader_list(asg, g), file.path(d, "assignment.csv"), na = "")
    write_progress(g)
  }
  if (length(changed)) log_action("system", paste("grader lists updated", item), paste(unique(changed), collapse = ", "))
}

# After an extension ends, read that student's commits again (they may have
# pushed during the extension) and recompute the graded version.
# Runs when an admin opens the dashboard; marks exceptions.csv refreshed_at.
update_extension_students <- function(user = current_user()) {
  if (!is_admin(user)) return(invisible(0))
  e <- read_exceptions()
  todo <- which(e$type == "extension" & !is.na(e$new_due) & e$new_due < now_et() &
                (is.na(e$refreshed_at) | e$refreshed_at == ""))
  n <- 0
  for (k in todo) {
    it <- e$item[k]
    base <- read_csv_quiet(admin_path("data", "snapshots", paste0(it, "-students.csv")))
    if (is.null(base)) next                         # submissions not found yet: nothing to update
    info <- item_info(it); repo <- student_repo(info$repo, e$github[k])
    cm <- tryCatch(repo_commits(repo, path = info$folder), error = function(err) NULL)
    if (is.null(cm)) next
    staff <- tolower(staff_logins())
    cm <- cm[!tolower(cm$author) %in% staff & !grepl("\\[bot\\]$", cm$author), ]
    all <- read_csv_quiet(admin_path("data", "snapshots", paste0(it, "-commits.csv")))
    all <- all[tolower(all$github) != tolower(e$github[k]), ]
    if (nrow(cm)) all <- rbind(all, data.frame(github = e$github[k], sha = cm$sha,
      time = format(cm$time, "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), author = cm$author, message = cm$message))
    readr::write_csv(all, admin_path("data", "snapshots", paste0(it, "-commits.csv")), na = "")
    e$refreshed_at[k] <- format(Sys.time(), "%Y-%m-%d %H:%M")
    write_exceptions(e)
    compute_snapshot(it)
    n <- n + 1
  }
  invisible(n)
}

# --- the review ---------------------------------------------------------------

# Everyone with commits after the deadline, with their decision (if any).
late_cases <- function(item) {
  it <- item_info(item); sn <- read_snapshot(item); cm <- read_commits(item)
  if (is.null(sn) || is.null(cm)) return(NULL)
  flagged <- sn[!is.na(sn$suggested_minutes) & sn$suggested_minutes > 0, ]
  if (!nrow(flagged)) return(flagged)
  led <- read_ledger(); led <- led[led$item %in% item, ]
  d <- led[match(tolower(flagged$github), led$username), ]
  tot <- late_totals(it$week - 1)
  flagged$decision <- d$decision; flagged$note <- d$note
  flagged$grade_through_minutes <- d$grade_through_minutes
  flagged$posted <- d$posted
  flagged$earlier_minutes <- tot$total_minutes[match(tolower(flagged$github), tot$username)]
  flagged$earlier_minutes[is.na(flagged$earlier_minutes)] <- 0
  flagged[order(is.na(flagged$decision) == FALSE, -flagged$suggested_minutes), ]
}

# decision: "late", "extension", "not late" (sections: "allowed"). grade_through: minutes after the
# deadline whose work is graded. penalty: minutes counted against the allowance.
record_late_decision <- function(item, github, decision, grade_through, penalty, note = "", user = current_user()) {
  require_admin(user, "review late work")
  it <- item_info(item); sn <- read_snapshot(item)
  s <- sn[sn$github == github, ][1, ]
  if (decision == "not late") { grade_through <- 0; penalty <- 0 }
  if (decision %in% c("extension", "allowed")) penalty <- 0
  if (it$type != "homework") penalty <- 0          # sections never count minutes
  led <- read_ledger()
  prev <- led[led$item %in% item & led$username == tolower(github), ]
  row <- data.frame(
    week = it$week, item = item, repo_name = it$repo, username = tolower(github),
    repo_url = github_url(s$repo),
    deadline = format(it$student_due, "%Y-%m-%d %H:%M:%S %Z"),
    last_commit_et = format(it$student_due + s$suggested_minutes * 60, "%Y-%m-%d %H:%M:%S %Z"),
    suggested_minutes = s$suggested_minutes, grade_through_minutes = grade_through,
    minutes_late = penalty, decision = decision, note = note %||% "",
    reviewed_by = user$username, reviewed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "America/New_York"),
    posted = if (nrow(prev) && isTRUE(prev$posted[1]) && identical(prev$minutes_late[1], penalty)) TRUE else FALSE,
    posted_at = if (nrow(prev)) prev$posted_at[1] else NA_character_)
  led <- rbind(led[!(led$item %in% item & led$username == tolower(github)), ], row[, ledger_cols])
  save_ledger(led)
  log_action(user$username, paste("late decision", item), sprintf("%s: %s, graded through +%s min, counted %s min", github, decision, grade_through, penalty))
  compute_snapshot(item)
}

# --- Canvas -----------------------------------------------------------------

late_comment <- function(minutes_late, total_minutes, allowance) {
  header <- paste0("### Late Submission\n\nThis homework was submitted **", fmt_minutes(minutes_late),
                   "** late.\n\nYour total late time this semester is **", fmt_minutes(total_minutes), "**.")
  if (total_minutes > allowance)
    paste0(header, " This exceeds the ", allowance / 60, "-hour late allowance for the semester, so this homework receives a grade of 0.")
  else paste0(header, " You have **", fmt_minutes(max(0, allowance - total_minutes)), "** remaining of your ",
              allowance / 60, "-hour late allowance.")
}

# Late notes not yet posted for this item (preview on the Late tab).
late_notices <- function(item) {
  it <- item_info(item)
  led <- read_ledger()
  if (it$type != "homework") return(led[0, ])
  todo <- led[led$item %in% item & !is.na(led$minutes_late) & led$minutes_late > 0 & !led$posted, ]
  if (!nrow(todo)) return(todo)
  tot <- late_totals(it$week, led)
  todo$total_minutes <- tot$total_minutes[match(todo$username, tot$username)]
  st <- read_students()
  todo$canvas_id <- st$canvas_id[match(todo$username, tolower(st$github))]
  todo$name <- st$name[match(todo$username, tolower(st$github))]
  todo$over <- todo$total_minutes > allowance_minutes()
  todo$comment <- mapply(late_comment, todo$minutes_late, todo$total_minutes, allowance_minutes())
  todo
}
