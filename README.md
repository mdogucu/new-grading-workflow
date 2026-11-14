# STAT 100 course management — START HERE (for Mine)

This folder holds everything for running the course: the dashboard app,
course settings, student lists, and every grader's work. This file is the map.

## The three parts

```
stat100-new-grading/                 ← this folder. Only you (and Julie, the TF) have it.
│
├── README.md                        ← you are here
│
├── admin/                           ← ADMIN TEAM ONLY. Private data lives here.
│   ├── README.md                       what each file is and how to edit it
│   ├── config/                         settings YOU edit: course.yml, staff.csv,
│   │                                   sections.csv, calendar.csv, coi.csv
│   ├── inputs/                         downloads you drop in (Canvas, Gradescope, rosters)
│   ├── data/                           files the dashboard builds (don't edit by hand)
│   └── logs/                           what was done, by whom, when
│
├── stat100-ca-shared/               ← SHARED WITH EVERY CA (folder #1 they see)
│   ├── README.md                       CA "start here"
│   ├── app/                            the dashboard (one app for everyone)
│   ├── docs/                           setup instructions, troubleshooting
│   ├── templates/                      email templates (plain text, edit freely)
│   ├── rubrics/                        homework rubrics (coming in phase 2)
│   └── course-info/                    CA-safe copy of the settings (written by the app)
│
└── graders/                         ← one folder per CA, each shared with that CA only
    └── stat100-grader-<username>/      (folder #2 they see)
```

**Rule of thumb:** a CA sees exactly two folders — `stat100-ca-shared` and their
own `stat100-grader-<username>`. Nothing private (names, emails, HUIDs, COIs)
is ever written to those two folders, except a student's email inside a regrade
that CA is handling.

## How information flows

```
 Canvas / Gradescope / registrar downloads
          │  you drop them in admin/inputs/
          ▼
 Admin tab ▸ "Refresh student list"  ──►  admin/data/students.csv   (full, private)
          │                          └──►  stat100-ca-shared/course-info/students.csv
          │                                (GitHub username + section only)
 admin/config/*  (you edit)
          │
 Admin tab ▸ "Publish course info"   ──►  stat100-ca-shared/course-info/
                                          (calendar, sections, staff without emails)
```

## Running the dashboard

Open `stat100-ca-shared/app/app.R` in RStudio and click **Run App**.
It opens on **My tasks** — everything that needs your attention, with a link
to the right tab. What you see depends on your role (from `admin/config/staff.csv`).

First time on a computer: follow `stat100-ca-shared/docs/setup-computer.md`
(R packages, Git, GitHub token) and, for admins, `docs/setup-canvas-token.md`.

## Who can do what

| | Admin team (Mine, Julie, TF) | CA | Senior CA | Grading monitor |
|---|---|---|---|---|
| My tasks, Calendar, Setup | ✓ | ✓ | ✓ | ✓ |
| Students, Staff, Admin tabs | ✓ | | | |
| Post grades to Canvas, create repos | ✓ | | | |
| Grade homework / own section | ✓ | ✓ (unless on hold) | ✓ | ✓ |
| Triage regrade requests | ✓ | | ✓ | |
| See everyone's grading progress | ✓ | | | ✓ |

Roles and flags are columns in `admin/config/staff.csv`. Change the file, then
press **Publish course info**.

**Senior CAs this semester:** Virgil Guo, Siri Panetta, Yasmin Salerno, Oliver
Song. Each week one of them is the **grading monitor** — picked at random but
evenly (seeded by week block) — and is automatically on hold from grading that
week. See admin/README.md for how to override a week.

**Why a CA can't post grades even if they tried:** admin buttons aren't shown
to them, every admin action checks the role again before running, and — the
real safeguard — they don't have the Canvas token, GitHub owner rights, or
access to `admin/`.

## Where to look when something goes wrong

1. **Setup tab** in the dashboard — shows what's missing on that computer and
   which folders the app is looking at.
2. `stat100-ca-shared/docs/troubleshooting.md` — common problems and fixes.
3. `admin/logs/action-log.csv` — every admin action (and blocked attempts).
4. The code: every file in `stat100-ca-shared/app/R/` starts with a comment
   saying what it does. `app/app.R` has the code map.

## Build status

- **Phase 1 (done):** folders, settings, student list from Canvas + Gradescope
  + section roster, staff/sections/COI checks, My tasks, Calendar, Setup check,
  email template for students without a GitHub username.
- **Phase 2 (done, first real run: hw09/sec09):** Admin ▸ Weekly grading
  (find submissions, assign graders, progress) and the **Grading** tab
  (question-by-question, clickable rubric, live points; section completion %).
- Phase 3: regrades (form, triage, routing), grading monitor, more templates.
- Phase 4: GitHub management (membership, weekly repos, CA access), projects
  and team changes, analytics (CAs, students, questions).
