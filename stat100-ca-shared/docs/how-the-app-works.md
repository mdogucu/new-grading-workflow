# How the dashboard works (for whoever maintains it)

One Shiny app, `app/app.R`, used by everyone. When it starts it:

1. **Finds folders** (`R/01-paths.R`). The app lives in `stat100-ca-shared/app/`.
   On an admin's computer the parent of `stat100-ca-shared` contains `admin/`
   and `graders/`. On a CA's computer, their own folder is a sibling shortcut.
2. **Works out who you are** (`R/02-identity.R`): the GitHub username of the
   saved token, looked up in staff.csv to get role and flags.
3. **Builds the page for that role** (`app.R`): admin tabs are only added for
   admins. Every admin action calls `require_admin()` before doing anything.

## Files

| file | what it does |
|---|---|
| `R/00-packages.R` | list of needed packages |
| `R/01-paths.R` | where folders are |
| `R/02-identity.R` | who you are, your role, `require_admin()` |
| `R/03-config.R` | reads course.yml, staff, sections, calendar, COI; publishes the CA-safe copy; the action log |
| `R/04-roster.R` | joins Canvas + Gradescope survey + section roster into the student list |
| `R/05-tasks.R` | every "My tasks" check. Add a `check_*()` function to add a new kind of task. |
| `R/06-templates.R` | email templates: read, fill in {placeholders}, copy / mail buttons |
| `R/07-theme.R` | colors, fonts, logo, View as menu |
| `R/08-github.R` | GitHub commit lists, local copies of student repos |
| `R/09-grading.R` | find submissions, assign graders, rubrics, scoring, splitting a .qmd / .html by question |
| `R/10-late.R` | late homework/section review, 48-hour allowance, the late note wording |
| `R/11-github-org.R` | GitHub organization vs. the student list; removing people |
| `R/12-progress.R` | grading progress files (for the grading monitor's Progress tab) |
| `R/13-rubrics.R` | building and checking rubrics (Rubrics tab) |
| `R/14-exceptions.R` | extensions and excused work |
| `R/15-quizzes.R` | Poll Everywhere files: matching rows to students |
| `R/16-canvas-sync.R` | filling calendar.csv from Canvas assignments |
| `R/17-canvas-post.R` | everything sent to Canvas: scores, feedback comments, late notes, EXCUSED, regrade changes |
| `R/tab-*.R` | one file per tab: `*_ui()` draws it, `*_server()` makes it work |
| `www/` | logo, `styles.css`, `app.js` (copy buttons), bundled fonts/icons |

## Data rules

- Admin-only data stays in `admin/`. Only CA-safe copies go to
  `stat100-ca-shared/course-info/` (via "Publish course info" / "Refresh student list").
- The app never edits files in `admin/config/` — people do. The app writes to
  `admin/data/`, `admin/logs/`, `course-info/`, and grader folders.
- Students are joined on HUID: Canvas "SIS User ID" = Gradescope "Student ID" =
  roster "HUID".

## Making a change safely

1. Copy the `app/` folder somewhere outside Google Drive, make the change there,
   and run it (Run App) to check.
2. Copy the changed file back into `stat100-ca-shared/app/`. Everyone gets it
   the next time they start the app.
