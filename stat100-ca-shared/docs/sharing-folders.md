# Sharing the folders (admin team)

Do this at the start of the semester, and when a CA joins or leaves.

## New CA
1. Add a row to `admin/config/staff.csv` (username, name, email, role = ca, …).
2. Dashboard ▸ **Admin** ▸ **Create missing folders** — makes
   `graders/stat100-grader-<username>/`.
3. Dashboard ▸ **Admin** ▸ **Publish course info**.
4. In Google Drive (browser):
   - Share `stat100-ca-shared` with the CA as **Viewer**.
   - Share the folder **`stat100-ca-shared/progress`** with the CA as **Editor**
     (only this one subfolder). Their dashboard writes a small file there with
     their grading counts, so the grading monitor can see who's done.
   - Share `graders/stat100-grader-<username>` with **only that CA** as **Editor**.
5. Send them `stat100-ca-shared/README.md` (or just tell them to open it).

## New admin (Julie, a TF)
Share the whole `stat100-new-grading` folder as **Editor**. Their app will see
`admin/` and act as admin once they're `role = admin` in staff.csv.

## CA leaves
Remove them from both shares in Google Drive. Keep their grader folder (it
holds their grading history). Set `on_hold = TRUE` or remove their row in
staff.csv, then Publish course info.

## Check
Never share `admin/` or the `graders/` parent folder with a CA — that would
give them every CA's folder.
