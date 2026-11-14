# Setting up your computer for the STAT 100 dashboard

You need: Google Drive for desktop, R + RStudio, a few R packages, Git, and a
GitHub token. Do this once per computer.

## 1. Put the two course folders on your computer

1. Install **Google Drive for desktop** (https://www.google.com/drive/download/)
   and sign in with your **Harvard Google account** (the one the folders were
   shared with).
2. In a web browser, open Google Drive ▸ **Shared with me**. You should see
   `stat100-ca-shared` and `stat100-grader-<your GitHub username>`.
3. For **each** of the two folders: right-click ▸ **Organize** ▸
   **Add shortcut** ▸ **My Drive** ▸ **Add**.
4. After a minute both folders appear on your computer:
   - Mac: Finder ▸ Google Drive ▸ My Drive
     (`~/Library/CloudStorage/GoogleDrive-<you>@.../My Drive/`)
   - Windows: `G:\My Drive\`

Keep the two shortcuts side by side in My Drive (don't put them inside other
folders) — that's how the dashboard finds your personal folder.

## 2. Install Git

- Mac: open Terminal and run `git --version`. If it isn't installed, macOS
  offers to install it — say yes.
- Windows: install from https://git-scm.com/download/win (all defaults).

## 3. Install the R packages

In the RStudio console:

```r
install.packages(c("shiny", "bslib", "DT", "dplyr", "readr", "stringr",
                   "yaml", "gh", "gitcreds", "httr2", "xml2", "usethis", "rstudioapi"))
```

## 4. GitHub token

Follow `setup-github-token.md`.

## 5. Admin team only: Canvas token

Follow `setup-canvas-token.md`.

## 6. Open the dashboard

Open `stat100-ca-shared/app/app.R` in RStudio ▸ click **Run App**.
Go to the **Setup** tab: all checks should be green.
