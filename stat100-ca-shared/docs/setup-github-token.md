# GitHub token

The dashboard uses a GitHub token to (a) know who you are and (b) download the
student repositories you grade. The token is saved in your computer's password
store — never in a file, never in Google Drive.

## Create and save it

In the RStudio console:

```r
# 1. Opens GitHub in your browser with the right boxes ticked.
usethis::create_github_token(
  description = "STAT 100 dashboard",
  scopes = c("repo", "read:org")          # course assistants
  # scopes = c("repo", "admin:org", "workflow")   # admin team: use this line instead
)
```

On the GitHub page:
- **Expiration:** pick a date after the end of the semester.
- Click **Generate token** at the bottom, then **copy** the token (starts with `ghp_`).
  You won't be able to see it again.

```r
# 2. Paste the token when asked.
gitcreds::gitcreds_set()
```

```r
# 3. Check — should print your GitHub username.
gh::gh_whoami()
```

Restart R (Session ▸ Restart R), run the dashboard, and check the Setup tab.

## Why these permissions?

| scope | what for |
|---|---|
| repo | download (clone) the private student repos you've been given access to |
| read:org | see the course organization's teams |
| admin:org | admin team: invite students, manage teams and repo access |
| workflow | admin team: create repos from templates that contain GitHub Actions |

## Problems

- **"No GitHub token found"** — run step 2 again, then restart R.
- **Token expired / "Bad credentials"** — create a new one (step 1) and save it
  (step 2). `gitcreds_set()` will ask whether to replace the old one: choose replace.
- **Wrong username shown** — you saved a token from another GitHub account.
  Run `gitcreds::gitcreds_delete()` and start over.
