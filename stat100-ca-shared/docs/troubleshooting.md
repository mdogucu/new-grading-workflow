# Troubleshooting

Start with the dashboard's **Setup** tab: it shows each check and which folders
the app is using ("Where the app is looking").

| Problem | Fix |
|---|---|
| "Please install these R packages first" | Run the `install.packages()` line it prints. |
| Setup: GitHub token red | `docs/setup-github-token.md` |
| "GitHub user 'x' is not on the staff list" | Your token belongs to a different account than the one in staff.csv, or you haven't been added. Ask an admin. |
| "Your personal grading folder isn't on this computer" | Add the shortcut to My Drive (`setup-computer.md`, step 1). Make sure it sits next to `stat100-ca-shared`, not inside another folder. |
| Admin sees only CA tabs | The app can't see `admin/` from where it's running. Open the app from inside `stat100-new-grading/stat100-ca-shared/app/`, or set `STAT100_ADMIN_DIR` (below). |
| Data looks out of date | Admin: drop fresh downloads into `admin/inputs/`, then Admin ▸ Refresh student list and Publish course info. Google Drive can take a minute to sync. |
| A student shows up with the wrong GitHub username or section | Add a row to `admin/config/student-overrides.csv` (huid, github, section, note) and refresh. |

## Pointing the app at folders by hand

If your folders are somewhere unusual, run `usethis::edit_r_environ()` and add
any of these lines, then restart R:

```
STAT100_SHARED_DIR=/full/path/to/stat100-ca-shared
STAT100_MY_DIR=/full/path/to/stat100-grader-yourusername
STAT100_ADMIN_DIR=/full/path/to/stat100-new-grading/admin
```

## Viewing the app as someone else (admins)

Use the **View as** menu (top right) and pick a person: the dashboard shows
exactly what they see, with a yellow banner. "Myself" goes back. You can also
add `?as=<username>` to the address. Anything you save in that view is
recorded as done by you.

(For testing without a GitHub token: `STAT100_DEV_USER=<username>` in
`.Renviron` makes the app act as that person on this computer. Remove it when
done. It can't give anyone extra powers — posting and repo changes still need
real tokens.)
