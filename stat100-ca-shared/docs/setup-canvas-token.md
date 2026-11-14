# Canvas token (admin team only)

**Already have one in R?** If your `~/.Renviron` already has `CANVAS_API_KEY=...`
(what the vvcanvas package uses) or `CANVAS_API_TOKEN=...`, you're done — the
dashboard reads it from there. Nothing else to save.

Needed to post grades and comments to Canvas. CAs never need this.

1. Go to https://canvas.harvard.edu ▸ **Account** (left bar) ▸ **Settings**.
2. Scroll to **Approved Integrations** ▸ **+ New Access Token**.
   - Purpose: `STAT 100 dashboard`
   - Expires: end of the semester
3. Click **Generate Token** and copy it.
4. In the RStudio console:

   ```r
   usethis::edit_r_environ()
   ```

   A file called `.Renviron` opens. Add this line (paste your token), save, close:

   ```
   CANVAS_API_TOKEN=paste-your-token-here
   ```

5. Restart R (Session ▸ Restart R). In the dashboard's Setup tab,
   "Canvas token saved" should be green.

**Keep it private.** The token can do anything you can do in Canvas. Never put
it in a file inside Google Drive, an R script, or GitHub. If it leaks, delete it
in Canvas ▸ Account ▸ Settings and make a new one.
