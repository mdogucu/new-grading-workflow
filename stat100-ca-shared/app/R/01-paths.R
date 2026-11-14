# ---------------------------------------------------------------------------
# 01-paths.R — where things live
#
# The app sits in  <shared folder>/app/ . Everything else is found from there.
#
#   Admin's computer (owner of the course folder):
#     stat100-new-grading/                 <- course root
#       admin/                             <- admins only
#       stat100-ca-shared/app/             <- this app
#       graders/stat100-grader-<user>/     <- one per CA
#
#   CA's computer (folders shared with them, added as shortcuts to My Drive):
#     My Drive/stat100-ca-shared/app/      <- this app
#     My Drive/stat100-grader-<user>/      <- their own folder
#
# If the app ever can't find a folder, set it by hand in ~/.Renviron:
#     STAT100_SHARED_DIR = "/full/path/to/stat100-ca-shared"
#     STAT100_MY_DIR     = "/full/path/to/stat100-grader-<user>"
#     STAT100_ADMIN_DIR  = "/full/path/to/admin"          (admins only)
# then restart R.
# ---------------------------------------------------------------------------

app_dir <- function() {
  d <- getOption("stat100.app_dir")
  if (is.null(d)) d <- normalizePath(getwd(), mustWork = FALSE)
  d
}

shared_dir <- function() {
  env <- Sys.getenv("STAT100_SHARED_DIR")
  if (nzchar(env)) return(env)
  dirname(app_dir())
}

# Course root only exists on admin computers (the parent of the shared folder).
course_root <- function() dirname(shared_dir())

admin_dir <- function() {
  env <- Sys.getenv("STAT100_ADMIN_DIR")
  if (nzchar(env)) return(env)
  file.path(course_root(), "admin")
}

has_admin_folder <- function() dir.exists(admin_dir())

grader_folder_name <- function(user) paste0("stat100-grader-", user)

# A grader's personal folder. Admins can look up anyone's; CAs only find their own.
grader_dir <- function(user) {
  env <- Sys.getenv("STAT100_MY_DIR")
  candidates <- c(
    if (nzchar(env) && identical(user, current_user()$username)) env,
    file.path(course_root(), "graders", grader_folder_name(user)),  # admin layout
    file.path(course_root(), grader_folder_name(user))              # CA layout (sibling shortcut)
  )
  hit <- candidates[dir.exists(candidates)]
  if (length(hit)) hit[1] else NA_character_
}

# Paths inside the admin folder
admin_path <- function(...) file.path(admin_dir(), ...)
# Paths inside the shared folder
shared_path <- function(...) file.path(shared_dir(), ...)
