# How to grade (course assistants)

1. Open the dashboard (`app/app.R` ▸ Run App). **My tasks** shows what you
   have to grade and when it's due. Click **Open**, or go to the **Grading** tab.
2. Pick the **assignment** (e.g. hw09). The list shows only GitHub usernames.
   Students you've graded drop off the list (tick "Include students I've already
   graded" to go back to one).
3. The first time you open a student, the dashboard downloads their repo to
   `~/stat100-grading-repos/` on your computer, at **exactly the version we
   grade** (the last push before the deadline + late window). Want them all at
   once? Click **Download all my students' work**.
4. For each question you see:
   - **Left:** two tabs. **Code (.qmd)**: the student's file for that question;
     lines the student wrote are highlighted in yellow, the grey text is our
     template. **Output (.html)**: the rendered result (text, output, plots) for
     that question. Students don't commit their .html, so the first time click
     **Render the student's file** (needs Quarto and the course packages).
   - **Right:** the rubric. Tick everything that applies. If it's all correct,
     tick **Correct**. The points update as you click.
   - **Adjust points (+/−)** (optional): a small tweak on top of the rubric,
     e.g. −0.001 for something minor. Explain it in the feedback.
   - **Feedback the student will see:** filled in automatically from the
     options you ticked. Edit it freely (add a note, reword). Once you've
     edited it, ticking more options won't overwrite your text — click
     **Rebuild from rubric** to start over from the rubric wording.
5. Need more context? **Open rendered file** (the student's HTML; if they
   didn't commit it, the dashboard renders it), **Open R project** (runs their
   code in a new RStudio window), or **Open on GitHub** (the graded version).
6. **Save & next student.** Every question needs at least one option ticked.
   "Mark unanswered questions correct" ticks *Correct* on every question you
   haven't touched — use it only after you've read them!

**Section work** is graded on completion: pick 100%, 90%, … 10%, or "Not submitted".

You can change a grade any time before grades are posted: tick "Include students
I've already graded", pick the student, change, Save.

Your grades are saved in your own folder (`stat100-grader-<you>/grading/<item>/grades.csv`).
Only you and the admin team can see them.

Never push anything to a student's repo, and don't share the downloaded repos.
