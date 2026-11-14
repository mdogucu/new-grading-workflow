# ---------------------------------------------------------------------------
# 11-github-org.R — the course GitHub organization vs. the student list
#
# check_github_org(): asks GitHub for the organization's members and pending
#   invitations and compares them with the student list (from Canvas).
#   Saves admin/data/github-check.csv:
#     login, in_org ("member" / "invited" / "no"), on_list (TRUE/FALSE), name, email, huid
#   - in the org but NOT on the list (and not staff) -> probably dropped
#   - on the list but not a member                  -> hasn't joined yet
# remove_from_org(): removes a member (or cancels their invitation).
# Needs an admin GitHub token with the admin:org permission.
# ---------------------------------------------------------------------------

org_people <- function() {
  org <- read_course()$github_org
  mem <- gh::gh("GET /orgs/{org}/members", org = org, per_page = 100, .limit = Inf)
  inv <- tryCatch(gh::gh("GET /orgs/{org}/invitations", org = org, per_page = 100, .limit = Inf), error = function(e) list())
  rbind(
    data.frame(login = vapply(mem, function(m) m$login, ""), in_org = rep("member", length(mem))),
    data.frame(login = vapply(inv, function(m) m$login %||% "", ""), in_org = rep("invited", length(inv)))
  )
}

check_github_org <- function(user = current_user()) {
  require_admin(user, "check the GitHub organization")
  op <- org_people(); op <- op[op$login != "", ]
  st <- read_students(); staff <- tolower(read_staff(FALSE)$username)
  st_g <- st[!is.na(st$github) & st$github != "", ]
  # everyone in the org who isn't staff
  a <- op[!tolower(op$login) %in% staff, ]
  a$on_list <- tolower(a$login) %in% tolower(st_g$github)
  # students not in the org at all
  b <- st_g[!tolower(st_g$github) %in% tolower(op$login), ]
  b <- if (nrow(b)) data.frame(login = b$github, in_org = "no", on_list = TRUE) else NULL
  res <- rbind(a, b)
  m <- match(tolower(res$login), tolower(st_g$github))
  res$name <- st_g$name[m]; res$email <- st_g$email[m]; res$huid <- st_g$huid[m]
  res$checked_at <- format(Sys.time(), "%Y-%m-%d %H:%M")
  readr::write_csv(res, admin_path("data", "github-check.csv"), na = "")
  log_action(user$username, "check GitHub organization",
             sprintf("%d not on the student list, %d not joined", sum(!res$on_list), sum(res$on_list & res$in_org != "member")))
  res
}

read_github_check <- function() {
  g <- read_csv_quiet(admin_path("data", "github-check.csv"))
  if (is.null(g)) return(NULL)
  g$on_list <- as_flag(g$on_list)
  g
}

remove_from_org <- function(login, user = current_user()) {
  require_admin(user, "remove someone from the GitHub organization")
  if (tolower(login) %in% tolower(read_staff(FALSE)$username)) stop(login, " is on the staff list — not removing.")
  gh::gh("DELETE /orgs/{org}/memberships/{username}", org = read_course()$github_org, username = login)
  log_action(user$username, "removed from GitHub organization", login)
  g <- read_github_check()
  if (!is.null(g)) readr::write_csv(g[tolower(g$login) != tolower(login), ], admin_path("data", "github-check.csv"), na = "")
  invisible(TRUE)
}
