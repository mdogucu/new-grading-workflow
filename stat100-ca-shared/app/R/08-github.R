# ---------------------------------------------------------------------------
# 08-github.R — talking to GitHub, and copies of student repos on this computer
#
# Student repo name = <week repo>-<GitHub username>,
#   e.g. week09-statistical-inference-aidanzagel   in the stat100-fa26 org.
#
# Which version do we grade? Commits are read here (no files downloaded);
# the rules are in R/09-grading.R (snapshot_item) and R/10-late.R
# (compute_snapshot): teaching-team and bot commits are ignored; on time =
# last commit before the deadline; late homework is reviewed by an admin.
#
# Copies of student repos live in GOOGLE DRIVE, not loose on the computer,
# so that removing someone's Drive access at the end of the term also removes
# their access to students' work:
#   CA:     <their grader folder>/repos/<week repo>/<homework|section>/<student repo>/
#   admins: admin/repos/<week repo>/<homework|section>/<student repo>/
# Each copy holds only the graded commit (a "shallow" copy, no history), to
# keep Drive light. Homework and section are separate copies because they're
# graded at different commits.
# ---------------------------------------------------------------------------

student_repo <- function(week_repo, github) paste0(week_repo, "-", github)

# Where repos come from: GitHub normally. For testing, set STAT100_REPO_SOURCE
# to a local folder that contains copies of the repos.
repo_source <- function() { s <- Sys.getenv("STAT100_REPO_SOURCE"); if (nzchar(s)) s else "github" }

local_repos_dir <- function(user = current_user()) {
  d <- Sys.getenv("STAT100_REPOS_DIR")
  if (nzchar(d)) return(d)
  if (has_admin_folder()) return(admin_path("repos"))
  g <- grader_dir(user$username %||% "")
  if (!is.na(g)) return(file.path(g, "repos"))
  stop("Can't find your grading folder in Google Drive, so student work can't be downloaded. See the Setup tab.")
}

github_url <- function(repo, sha = NULL, path = NULL) {
  org <- read_course()$github_org
  base <- sprintf("https://github.com/%s/%s", org, repo)
  if (is.null(sha)) return(base)
  if (is.null(path)) sprintf("%s/tree/%s", base, sha) else sprintf("%s/blob/%s/%s", base, sha, path)
}

# All commits on the default branch: data.frame(sha, time, author, message).
# `source` is normally GitHub; tests pass a local folder of git repos instead.
# `path` (e.g. "homework") keeps only commits that changed files in that
# folder — homework and section work live in the same repo, so a homework
# push after the section deadline must not look like late section work.
repo_commits <- function(repo, source = repo_source(), path = NULL) {
  if (!identical(source, "github")) return(local_commits(file.path(source, repo), path))
  org <- read_course()$github_org
  args <- list("GET /repos/{owner}/{repo}/commits", owner = org, repo = repo, per_page = 100, .limit = 500)
  if (!is.null(path)) args$path <- path
  res <- tryCatch(do.call(gh::gh, args),
                  error = function(e) e)
  if (inherits(res, "error")) {
    if (grepl("404|409", conditionMessage(res))) return(NULL)   # no repo / empty repo
    stop(res)
  }
  if (!length(res)) return(NULL)
  data.frame(
    sha = vapply(res, function(c) c$sha, ""),
    time = as.POSIXct(vapply(res, function(c) c$commit$committer$date, ""), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    author = vapply(res, function(c) c$author$login %||% c$commit$author$name %||% "", ""),
    message = vapply(res, function(c) strsplit(c$commit$message, "\n")[[1]][1] %||% "", ""),
    stringsAsFactors = FALSE)
}

local_commits <- function(dir, path = NULL) {
  if (!dir.exists(file.path(dir, ".git"))) return(NULL)
  out <- system2("git", c("-C", shQuote(dir), "log", "--format=%H%x09%cI%x09%an%x09%s",
                          if (!is.null(path)) c("--", shQuote(path))), stdout = TRUE)
  if (!length(out)) return(NULL)
  p <- strsplit(out, "\t")
  data.frame(sha = vapply(p, `[`, "", 1),
             time = as.POSIXct(sub("([+-]\\d\\d):(\\d\\d)$", "\\1\\2", vapply(p, `[`, "", 2)), format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
             author = vapply(p, `[`, "", 3), message = vapply(p, `[`, "", 4), stringsAsFactors = FALSE)
}

# --- local copies ----------------------------------------------------------

git <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0
  if (status != 0) stop(paste(out, collapse = "\n"), call. = FALSE)
  invisible(out)
}

# Make sure there's a copy of `repo` at `sha` in `dest`. Returns dest.
# `source` = "github" or a local folder of repos (tests).
checkout_copy <- function(repo, sha, dest, source = repo_source()) {
  url <- if (identical(source, "github"))
    sprintf("https://github.com/%s/%s.git", read_course()$github_org, repo) else file.path(source, repo)
  if (!dir.exists(file.path(dest, ".git"))) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    git("-C", shQuote(dest), "init", "--quiet")
    git("-C", shQuote(dest), "remote", "add", "origin", shQuote(url))
  }
  have <- tryCatch(git("-C", shQuote(dest), "cat-file", "-e", paste0(sha, "^{commit}")), error = function(e) NULL)
  if (is.null(have)) {
    # just the graded commit, no history (GitHub allows fetching a commit by its id)
    ok <- tryCatch({ git("-C", shQuote(dest), "fetch", "--quiet", "--depth", "1", "origin", sha); TRUE }, error = function(e) FALSE)
    if (!ok) git("-C", shQuote(dest), "fetch", "--quiet", "origin")       # fallback: everything
  }
  git("-C", shQuote(dest), "checkout", "--quiet", "--force", sha)
  dest
}

# The untouched week template (to tell student text from our text).
template_copy <- function(week_repo, source = repo_source()) {
  dest <- file.path(local_repos_dir(), week_repo, "_template")
  if (dir.exists(file.path(dest, ".git"))) {
    try(git("-C", shQuote(dest), "pull", "--quiet", "--ff-only"), silent = TRUE)   # pick up template edits
    return(dest)
  }
  url <- if (identical(source, "github"))
    sprintf("https://github.com/%s/%s.git", read_course()$github_org, week_repo) else file.path(source, week_repo)
  ok <- tryCatch({ dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
                   git("clone", "--quiet", "--depth", "1", shQuote(url), shQuote(dest)); TRUE },
                 error = function(e) FALSE)
  if (ok) dest else NA_character_
}
