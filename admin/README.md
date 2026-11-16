# admin/ — admin team only

Never share this folder with CAs. It holds student names, emails, HUIDs and
conflicts of interest.

## config/ — settings you edit

Edit these in RStudio, Excel, or any text editor. Keep the column names
exactly as they are. After editing, open the dashboard ▸ **Admin** ▸
**Publish course info** so CAs see the change.

### course.yml
Course-wide settings: GitHub organization, Canvas course ID, late window,
regrade window. Each line has a comment explaining it.

### staff.csv — one row per person on the teaching team
| column | meaning |
|---|---|
| username | GitHub username (this is how the app recognizes them) |
| name, email | for display and emails (email is never copied to CAs) |
| role | `admin` (instructor, preceptor, TF) or `ca` |
| senior | TRUE = senior CA: triages regrade requests, takes turns as grading monitor |
| monitor | TRUE = grading monitor **every** week (e.g. a TF). Leave FALSE for senior CAs — their turns are picked automatically |
| section | section they teach, e.g. D003 (blank if none) |
| on_hold | TRUE = gets no new grading this week (set back to FALSE later) |
| notes | anything; ignored by the app |

### sections.csv
`section, day, time, location, leader` — leader is the GitHub username of the
CA/TF who teaches it and grades its section work.

### Where the dashboard starts
Everything up to hw09/sec09/quiz11b was graded the old way. course.yml
`first_week_homework: 11`, `first_week_section: 11`, `first_week_quiz: 12`
tell the dashboard to ignore anything earlier (Calendar from Canvas skips it,
quizzes list skips it). Old quiz files are in inputs/pollev/before-dashboard/,
old rubrics in stat100-ca-shared/rubrics/old/. The late ledger keeps weeks
1–7 on purpose: the 48-hour allowance counts the whole semester.

### calendar.csv — one row per gradable item (homework, section, quiz)
**You don't have to type most of it.** Admin tab ▸ **Calendar from Canvas** ▸
Check Canvas reads the assignments you already made in Canvas (named like
hw09-statistical-inference, sec09-…, quiz11b) and fills in new items, titles,
week repo names, due dates and Canvas ids, plus a default grading due date
where it's empty (course.yml `grading_days_homework` / `_section` / `_quiz`, all 3:
days after the due date). You see the list of changes before anything is
written; the old file is kept in config/old/. Canvas wins for due dates and
ids, so change those in Canvas and check again. Owners, ready-by dates and
grading due dates you typed are never overwritten.

Quizzes are rows with `type = quiz` (quiz11b = week 11, lecture b). They were
filled in for every Monday (a) and Wednesday (b) lecture, skipping holidays,
with student_due = 1:15 pm that day and grading_due = 3 days later at 11:59 pm. Delete
rows for days without a quiz, add the Canvas id when you know it. After a
quiz's date it appears on your My tasks as "Grade quiz11b".
| column | meaning |
|---|---|
| item | short code: hw04, sec04, m1 … |
| type | homework / section / project |
| week | week number |
| title | shown in the dashboard |
| repo | the week's template repo name, e.g. week04-wrangle-variables |
| student_due | when students must push, `2026-09-29 23:59` (Eastern) |
| grading_due | when grading must be finished |
| canvas_id | Canvas assignment ID (the number in the assignment's URL) |

| assignment_owner | username of who creates the homework/section (e.g. mdogucu) |
| assignment_ready_by | when it must be ready → a to-do for that person from 3 weeks before (done when the week repo and file exist on GitHub, or Dismiss) |
| rubric_owner | username of who writes the rubric (JulieVu for homework) |
| rubric_ready_by | when the rubric must be ready (blank = the student deadline) → a to-do; done when the rubric passes the check on the Rubrics tab |

Dates can be `2026-09-29` (means 11:59 pm), `2026-09-29 17:00`, or what a
spreadsheet writes (`9/29/26 17:00`).

### Grading monitor (automatic) and monitor-overrides.csv (optional)
Each week one senior CA (senior = TRUE) is the **grading monitor** and is
automatically **on hold** from grading that week. The pick is random but fair:
weeks are grouped in blocks of 4 (one per senior CA), each block is a shuffle
using `set.seed(block number)`, so everyone gets the same number of turns and
the result never changes when the app is re-run. The Staff & sections tab shows
the whole semester.

To pick someone by hand for a week (illness, travel), create
`monitor-overrides.csv` with columns `week, monitor, note`, e.g.
`9, ysalerno, Virgil away`. Overrides win over the random pick.

Course weeks come from `week1_monday` and `weeks` in course.yml.
Adding or removing a senior CA reshuffles future weeks — use overrides to keep
past weeks as they were.

### exceptions.csv — extensions and excused (written by the Late & excused tab)
`item, github, type, new_due, note, added_by, added_at, canvas_excused`
- `extension`: that student's deadline for that item is `new_due`.
- `excused`: the student doesn't do it — not graded, not assigned, and marked
  EXCUSED on Canvas when you post that item on the **Post to Canvas** tab
  (`canvas_excused` becomes TRUE). Removing an excuse un-excuses it on Canvas
  too, and the student's grade is then posted like anyone else's.
Enter them before or after the deadline; if submissions were already found,
the graded commits and graders' lists update right away.

### coi.csv — conflicts of interest
`student, grader` — both GitHub usernames. That grader never grades, regrades,
or gets repo access for that student.

### student-overrides.csv (written by the Students tab; you can edit it too)
`huid, github, section, status, note, changed_by, changed_at` — fixes that win
over the downloads: a corrected GitHub username or section, and a status:
- `dropped` — taken off the student list (Students tab ▸ "Dropped…", which can
  also remove them from the GitHub organization)
- `no github needed` — e.g. a tutor; never flagged for a missing username
Undo on Students ▸ "Dropped / no GitHub". After editing by hand: Refresh student list.

### Other things in course.yml
- `can_view_as` — who may use **View as** (see the dashboard as someone else).
  Only these admins: "mdogucu, JulieVu".

## Weekly grading (dashboard ▸ Admin ▸ Weekly grading)

After the deadline **plus the late window** (course.yml `late_window_hours`):

1. **Find submissions** — reads every student's commit list on GitHub
   (nothing is downloaded). Saves `data/snapshots/<item>.csv` (graded commit
   and status per student), `<item>-commits.csv` (all student commits) and
   `<item>-students.csv`. On time = last commit before the deadline.
   Homework with commits after the deadline is marked **needs review**.
1b. **Late work tab** (homework) — for each student with commits after the
   deadline, choose:
   - **Late**: grade the work up to a chosen commit; count N minutes
     (pre-filled with how late that commit was; change it if you want).
   - **Extension**: grade up to a chosen commit, count 0 (special permission
     given after the fact). (For an extension known in advance, or to excuse a
     student completely, use "Extensions and excused" at the top of the tab.)
   - **Not late**: grade the deadline version, count 0 (later commits weren't the homework).
   Decisions are saved in `data/late-ledger.csv` (one row per student per
   week; weeks 1–7 were imported from your old late-submissions-ledger.csv).
   Each student has a semester-wide allowance (course.yml
   `late_allowance_hours`, 48). If a homework takes them over it, it gets 0
   and isn't graded. The late note (minutes late, running total, what's left
   of the 48 hours; same wording as the old script) is posted as its own
   Canvas comment, after the feedback comment, on **Post to Canvas**.
   A student over the allowance gets 0 and just the late note.
   **Sections** aren't part of the allowance and no minutes are counted.
   A section counts as late only if the section file itself changed after the
   section deadline (homework pushes don't count). You choose **Allowed**
   (grade the late work, e.g. the student was sick) or **Not allowed** (grade
   the version at the deadline).
2. **Assign graders** — saves `data/assignments/<item>.csv` and copies each
   CA's list (usernames + commits only) to their folder,
   `graders/stat100-grader-<user>/grading/<item>/assignment.csv`.
   - Homework: students shuffled with `set.seed(week)`, dealt evenly to CAs
     who are not on hold (this week's grading monitor is automatically on
     hold), never to someone with a conflict of interest.
   - Section: each student goes to their section leader (sections.csv); a
     conflict of interest sends them to another CA.
   - Running it again rebuilds it; the previous file is kept with a date.
   - It won't run while late homework still needs review. If you change a
     late decision after assigning, the grader's list updates to the new commit.
3. Watch progress in the same card. CAs' grades are in their folders:
   `grading/<item>/grades.csv` (one row per student × question: codes like
   `1---2`, rubric points, adjustment, final points, and the feedback text).
4. **All grades in one place** (admins): every time an admin opens the
   dashboard (or clicks Rebuild on Progress), the graders' files are combined into
   - `admin/data/grades/<item>.csv`: every student × question from every
     grader (codes, points, feedback, who graded it, when, which commit);
   - `admin/data/grades/<item>-totals.csv`: one row per student with
     total points, percent, and status (graded / not graded yet /
     extension (waiting) / excused / missing).
   These are rebuilt from the graders' files, so edit grades in the
   graders' files (or the dashboard), not here.

Extensions after assigning: if a student gets an extension (Late & excused)
after they were assigned, they stay on the same grader's list, shown as
"extension until …: not yet". The grader can't grade them and they don't
count as unfinished. Once the extension is over, the next time an admin opens
the dashboard the student's latest commit is fetched and they become gradeable
for that same grader, so nobody grades them twice.

Rubrics are made on the **Rubrics** tab: pick the homework, "Load questions
from the homework" (reads them from the week's template on GitHub), fill in
points and options, Check, Save. Older versions go to `rubrics/old/`.
Rubrics live in `stat100-ca-shared/rubrics/<item>-rubric.csv` (section items
fall back to `section-rubric.csv`). The rubric's question names ("Question 1ii")
must match the homework's headings ("## Question 1") and part labels ("ii.").

## Quizzes (dashboard ▸ Quizzes; Mine)

After class, download the Poll Everywhere gradebook for the quiz and save it in
`inputs/pollev/` keeping Poll Everywhere's name (it starts with the quiz name,
e.g. `quiz11b_gradebook_1773914.csv`). It then shows up on My tasks as
"Grade quiz11b". On the Quizzes tab:
- Score = the Participation column (a percent), as in the old quiz-grading.R.
- Students are matched by **email**, then by name. Anyone who doesn't match
  (e.g. "Unregistered") is listed; pick the student once and it's remembered
  (`config/pollev-matches.csv`; "Unregistered" matches are for that quiz only).
- Then post it on the **Post to Canvas** tab (below). Excused students are
  marked EXCUSED; students with an extension wait; optionally give 0 to
  students not in the file (the old script didn't).
- The Canvas assignment comes from calendar.csv or the Canvas gradebook
  export ("quiz11b (1135126)"); you can also type the id on Post to Canvas.
- Who gets the quiz to-dos (and sees quizzes on Post to Canvas): course.yml
  `quiz_grader`.

## Post to Canvas (dashboard ▸ Post to Canvas; admins)

Everything that goes to Canvas is sent from this one tab: homework and section
grades with their feedback comments, late notes, 0 for over the late
allowance, EXCUSED, and quiz scores. Pick the item, check, click Post.

- **Homework / section score** = sum of the question points the graders saved.
  **Comment** = same layout as the old 11-release-feedback.R / 14-release-
  section-feedback.R ("### Assignment Feedback", one bullet per question with
  points and the feedback text, "**Total Score:** x / y"). Late homework gets
  the late note as a second, separate comment. Canvas adds comments; it never
  overwrites an earlier one.
- Only students who are completely graded are posted. Students not graded
  yet, with an extension still running, or with late work not reviewed wait;
  you can post the others now and the rest show up later.
- Students with no submission aren't posted unless you tick "Give 0".
- Click a student in the table to see exactly what will be sent.
- **Regrades**: every post is recorded in `data/canvas-posted.csv` (score
  and the exact comment sent). If a grade or its feedback changes afterwards
  (someone regrades it on the Grading tab), the student shows up as
  **changed since posted**, and My tasks says so. Posting updates the score
  and adds one more comment, "### Regrade comment", listing only the
  questions that changed (old → new points, new feedback) and the updated
  total. The original feedback comment stays as it was.
- Click a student to see the new comment(s) and every comment already
  posted to them.
- "Already posted another way — mark these as posted" records them without
  sending anything.
- Canvas calls: `submissions/update_grades` (same as the old scripts), up to
  25 students per call, waiting for Canvas to say "completed".

## inputs/ — downloads you drop in

Keep old files; the newest one (by name, then date) is used. Put the date at the
start of the file name if the download doesn't include it.

| folder | where it comes from |
|---|---|
| canvas-gradebook/ | Canvas ▸ Grades ▸ Export ▸ Export entire gradebook (CSV) |
| gradescope-survey/ | Gradescope ▸ GitHub username survey ▸ Review grades ▸ Export submissions (`submission_metadata.csv`) |
| section-roster/ | the registrar section roster (HUID, Name, Section) |
| pollev/ | Poll Everywhere quiz gradebooks (see Quizzes above) |

Then dashboard ▸ **Admin** ▸ **Refresh student list**.

## data/ — built by the dashboard (don't edit by hand)
- `students.csv` — one row per student: huid, canvas_id, name, email, section,
  github, and an `issue` column (blank = fine).

If something looks wrong here, fix the input or add an override, then refresh.

### Other files in data/
- `github-check.csv` — last "Check GitHub organization" (Students tab):
  who's in the org but not on the list (probably dropped), who hasn't joined.
- `dismissed-tasks.csv` — My tasks items you dismissed. Delete a row to bring it back.
- `canvas-posted.csv` — everything sent to Canvas (Post to Canvas tab): item,
  canvas_user_id, github, name, action (grade / excuse / zero), score, the
  exact comment, kind (first / update / marked), posted_at, posted_by. The
  newest row per student is what Canvas has; deleting rows makes those
  students "ready to post" again.
- `grades/` — all graders' grades combined per item (see Weekly grading).

## repos/ — copies of student repos opened by admins
Graders' copies live in their own folders (`graders/stat100-grader-<user>/repos/`).
Each copy holds just the graded commit. Safe to delete after grades are
released; the dashboard downloads again if needed.
**End of term:** remove CAs from the Drive shares (docs/sharing-folders.md).
Note that files a CA creates in a folder you own are owned by *them* in Google
Drive, so also delete their `repos/` folders (or move grader folders to a
Shared Drive, where the Shared Drive owns everything).

## logs/
- `action-log.csv` — time, user, action, details. Includes blocked attempts.
