# ---------------------------------------------------------------------------
# 02-identity.R — who is using the app, and what they may do
#
# Identity = the GitHub account of the token saved on this computer
# (see docs/setup-github-token.md). That username is looked up in the staff
# list to find the role:
#
#   admin  : instructor, preceptor, TF — sees every tab, can post / create repos
#   ca     : course assistant — grading tabs only
#   extra flags on a CA (in staff.csv): senior (regrade triage), monitor
#   (permanent grading monitor), on_hold (gets no new grading).
#   Each week one senior CA is picked (seeded random, see 03-config.R) as
#   grading monitor; that week they count as monitor AND on hold automatically.
#
# Hiding a tab is only for convenience. The real protection is that CAs do not
# have the Canvas token, GitHub owner rights, or access to the admin folder.
# Every admin action also calls require_admin() before doing anything.
#
# Testing: set STAT100_DEV_USER=<username> in ~/.Renviron to view the app as
# that person (it cannot give anyone powers they don't have — see above).
# ---------------------------------------------------------------------------

.identity_cache <- new.env()

github_login <- function() {
  dev <- Sys.getenv("STAT100_DEV_USER")
  if (nzchar(dev)) return(list(login = dev, source = "STAT100_DEV_USER", error = NULL))
  if (!is.null(.identity_cache$login)) return(.identity_cache$login)
  res <- tryCatch({
    who <- gh::gh_whoami()
    if (is.null(who)) stop("No GitHub token found on this computer.")
    list(login = who$login, source = "GitHub token", error = NULL)
  }, error = function(e) list(login = NA_character_, source = "none", error = conditionMessage(e)))
  .identity_cache$login <- res
  res
}

current_user <- function() {
  gl <- github_login()
  profile_for(gl$login, gl$source, gl$error)
}

# The profile (role, flags) of any staff member.
profile_for <- function(login, source = "staff list", error = NULL) {
  staff <- read_staff_public()
  me <- staff[!is.na(staff$username) & tolower(staff$username) == tolower(login %||% ""), ]
  if (nrow(me) == 0) {
    return(list(username = login, name = login, role = "unknown",
                senior = FALSE, monitor = FALSE, monitor_this_week = FALSE, on_hold = FALSE, section = NA,
                login_source = source, login_error = error))
  }
  me <- me[1, ]
  role <- me$role
  # An "admin" in the staff list only acts as admin if this computer can see the admin folder.
  if (role == "admin" && !has_admin_folder()) role <- "admin-no-folder"
  list(username = me$username, name = me$name, role = role,
       senior = isTRUE(me$senior), monitor = isTRUE(me$monitor),
       monitor_this_week = isTRUE(me$monitor_this_week),
       on_hold = isTRUE(me$on_hold), section = me$section,
       login_source = source, login_error = error)
}

# "View as": the people listed in course.yml `can_view_as` (Mine and Julie)
# can open the dashboard as any staff member to see what they see (add
# ?as=<username> to the address, or use the View as menu). Whatever is saved
# in that view is recorded as done by them. Anyone else asking for ?as=
# simply gets their own view.
can_view_as <- function(user) {
  allowed <- trimws(strsplit(read_course()$can_view_as %||% "", ",")[[1]])
  is_admin(user) && tolower(user$username %||% "") %in% tolower(allowed)
}

user_for_request <- function(query_string) {
  real <- current_user()
  q <- shiny::parseQueryString(query_string %||% "")
  as <- q$as
  if (is.null(as) || !nzchar(as) || !can_view_as(real) || tolower(as) == tolower(real$username %||% "")) return(real)
  u <- profile_for(as, source = paste("viewed by", real$username))
  if (u$role == "unknown") return(real)
  u$viewed_by <- real
  u
}

# Who actually did something (the admin, when viewing as someone else).
acting_username <- function(user) (user$viewed_by %||% user)$username

is_admin <- function(user = current_user()) identical(user$role, "admin")

# Call at the top of every admin-only action.
require_admin <- function(user = current_user(), action = "this action") {
  if (!is_admin(user)) {
    log_action(user$username %||% "unknown", "blocked", action)
    stop(sprintf("Only the admin team can %s.", action), call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
