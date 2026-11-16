# ---------------------------------------------------------------------------
# tab-grading.R — the grading screen (homework AND section)
#
# Left: pick the item and the student (GitHub usernames only), progress,
#       buttons to open the work (GitHub / rendered file / R project).
# Right: one card per rubric question: the student's answer (lines they wrote
#       are highlighted), the rubric options to click, an optional points
#       adjustment (e.g. -0.001 for something minor; section work: in %), and the feedback text the
#       student will see — filled in from the rubric for the ticked options and
#       editable. Once edited by hand, ticking options no longer overwrites it
#       (click "Rebuild from rubric" to start over).
# Section items use the completion picker (100%, 90%, … 10%, not submitted).
# Section lists build themselves (R/18-sections.R): the version at the end of
# section is shown; if the student committed after section, a box lets the CA
# switch to the newest version, mark "Late — warned" / "Not late", and
# email the student (templates/section-late.txt).
#
# Grades are saved to the grader's own folder: grading/<item>/grades.csv.
# Copies of student repos are made on this computer in ~/stat100-grading-repos.
# Admins can open any grader's list ("Grading as").
# ---------------------------------------------------------------------------

grading_ui <- function(id, user) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(width = 330, open = "always",
      if (is_admin(user)) shiny::selectInput(ns("as"), "Grading as", choices = NULL),
      shiny::selectInput(ns("item"), "Assignment", choices = NULL),
      shiny::uiOutput(ns("progress")),
      shiny::uiOutput(ns("section_tools")),
      shiny::selectInput(ns("student"), "Student (GitHub username)", choices = NULL),
      shiny::checkboxInput(ns("show_done"), "Include students I've already graded", FALSE),
      shiny::uiOutput(ns("open_buttons")),
      shiny::hr(),
      shiny::actionButton(ns("get_all"), "Download all my students' work", icon = icon_tag("cloud-download"),
                          class = "btn-outline-secondary btn-sm w-100"),
      shiny::tags$small(class = "text-muted", "Copies are kept in your grading folder in Google Drive (repos/), not loose on this computer.")
    ),
    shiny::uiOutput(ns("main"))
  )
}

grading_server <- function(id, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    saved_tick <- shiny::reactiveVal(0)

    grader <- shiny::reactive({ if (is_admin(user)) input$as %||% user$username else user$username })
    shiny::observe({ g <- grader(); if (!is.null(g)) write_progress(g) })   # keep the monitor's view current

    # Admins: whose list?
    if (is_admin(user)) {
      s <- read_staff()
      have <- s$username[vapply(s$username, function(u) dir.exists(file.path(course_root(), "graders", grader_folder_name(u), "grading")), TRUE)]
      shiny::updateSelectInput(session, "as", choices = if (length(have)) have else user$username)
    }

    built <- shiny::reactiveVal(0)      # bumps when a section list is (re)built
    late_tick <- shiny::reactiveVal(0)
    shiny::observe({
      built()
      its <- unique(c(my_items(grader()), tryCatch(section_items_for(grader()), error = function(e) character())))
      cal <- read_calendar()
      if (length(its)) {
        its <- its[order(-match(its, cal$item))]   # newest first
        lab <- paste0(its, " · ", cal$title[match(its, cal$item)])
        cur <- shiny::isolate(input$item)
        shiny::updateSelectInput(session, "item", choices = stats::setNames(its, lab),
                                 selected = if (!is.null(cur) && cur %in% its) cur else its[1])
      } else shiny::updateSelectInput(session, "item", choices = c("Nothing assigned yet" = ""))
    })

    is_section <- shiny::reactive({ shiny::req(input$item); grepl("^sec", input$item) })
    build_now <- function() {
      res <- shiny::withProgress(message = sprintf("Checking your section's commits for %s…", input$item), value = 0,
        tryCatch(build_section_list(grader(), input$item, progress = function(v, d) shiny::setProgress(v, detail = d)),
                 error = function(e) e))
      if (inherits(res, "error")) shiny::showNotification(conditionMessage(res), type = "error", duration = NULL)
      built(built() + 1)
    }
    # Section: build (or refresh) the list when it's opened.
    shiny::observeEvent(list(input$item, grader()), {
      shiny::req(nzchar(input$item %||% ""))
      if (grepl("^sec", input$item) && input$item %in% tryCatch(section_items_for(grader()), error = function(e) character()) &&
          section_list_stale(grader(), input$item)) build_now()
    })
    shiny::observeEvent(input$recheck, build_now())
    output$section_tools <- shiny::renderUI({
      shiny::req(is_section()); a <- asg(); built()
      all <- read_csv_quiet(file.path(grader_item_dir(grader(), input$item), "assignment.csv"))
      nos <- if (!is.null(all) && "status" %in% names(all)) all$github[all$status %in% "no submission"] else character()
      after <- if (!is.null(a) && "status" %in% names(a)) sum(a$status %in% "after section") else 0
      shiny::div(class = "small mb-2",
        if (after) shiny::div(class = "text-warning-emphasis", icon_tag("clock-history"), sprintf(" %d committed after section — you decide if it's late", after)),
        if (length(nos)) shiny::tags$details(shiny::tags$summary(sprintf("%d with no submission", length(nos))), paste(nos, collapse = ", ")),
        shiny::actionLink(ns("recheck"), shiny::tagList(icon_tag("arrow-repeat"), " Check for new commits")),
        if (!is.null(all) && "built_at" %in% names(all) && nzchar(all$built_at[1] %||% "")) shiny::span(class = "text-muted", " · checked ", fmt_short(all$built_at[1])))
    })

    asg <- shiny::reactive({ built(); shiny::req(input$item); my_assignment(grader(), input$item) })
    grades <- shiny::reactive({ saved_tick(); shiny::req(input$item); read_grades(grader(), input$item) })
    rubric <- shiny::reactive({ shiny::req(input$item); read_rubric(input$item) })
    info <- shiny::reactive({ shiny::req(input$item); item_info(input$item) })
    done <- shiny::reactive(unique(grades()$github))
    # Students still inside an extension: on the list, but not to be graded yet.
    waiting <- shiny::reactive({
      a <- asg(); if (is.null(a) || !("wait_until" %in% names(a))) return(character())
      w <- parse_when(a$wait_until)
      a$github[(!is.na(w) & w > now_et()) | (is.na(a$sha) | a$sha == "")]
    })

    output$progress <- shiny::renderUI({
      a <- asg(); if (is.null(a)) return(NULL)
      wt <- waiting(); a <- a[!a$github %in% wt, ]
      n <- nrow(a); k <- sum(a$github %in% done())
      it <- info()
      shiny::div(class = "grading-progress",
        shiny::div(class = "d-flex justify-content-between",
          shiny::strong(sprintf("%d of %d graded", k, n)), shiny::span(class = "text-muted", sprintf("%d left", n - k))),
        if (length(wt)) shiny::div(class = "small text-muted", icon_tag("hourglass-split"),
          sprintf(" + %d with an extension (not yet)", length(wt))),
        shiny::div(class = "progress", style = "height: 10px;",
          shiny::div(class = "progress-bar", style = sprintf("width:%d%%;", if (n) round(100 * k / n) else 0))),
        if (!is.na(it$grading_due)) shiny::div(class = "small text-muted mt-1", icon_tag("clock"), " Due ", fmt_due(it$grading_due)))
    })

    shiny::observe({
      a <- asg(); if (is.null(a)) return()
      wt <- waiting()
      left <- a$github[!a$github %in% c(done(), wt)]
      ch <- c(left, if (length(wt)) wt[!wt %in% done()])
      if (isTRUE(input$show_done)) ch <- c(ch, setdiff(a$github, ch))
      wl <- if (("wait_until" %in% names(a))) a$wait_until[match(ch, a$github)] else rep("", length(ch))
      lab <- ifelse(ch %in% done(), paste(ch, "\u2713"),
               ifelse(ch %in% wt, paste0(ch, "  (", ifelse(!is.na(wl) & nzchar(wl), paste0("until ", fmt_short(wl)), "extension"), ": not yet)"),
                 ifelse(ch %in% after_ids(), paste(ch, "\u23f0"), ch)))
      sel <- shiny::isolate(input$student)
      shiny::updateSelectInput(session, "student", choices = stats::setNames(ch, lab),
                               selected = if (!is.null(sel) && sel %in% ch) sel else ch[1])
    })

    after_ids <- shiny::reactive({ a <- asg(); if (is.null(a) || !"status" %in% names(a)) character() else a$github[a$status %in% "after section"] })

    row <- shiny::reactive({
      a <- asg(); shiny::req(input$student, a, input$student %in% a$github)   # wait until the list has caught up
      shiny::req(!input$student %in% waiting())                               # nothing to download yet
      a[a$github == input$student, ][1, ] })

    # Which version: for section work committed after section, the version at
    # the end of section (default) or the newest if the CA switches to it.
    gsha <- shiny::reactive({
      r <- row()
      has_dl <- "deadline_sha" %in% names(r) && !is.na(r$deadline_sha) && nzchar(r$deadline_sha)
      if (has_dl && !identical(input$version, "newest")) r$deadline_sha else r$sha
    })

    # Local copy of the student's repo at the graded commit.
    repo_dir <- shiny::reactive({
      r <- row(); it <- info(); sha <- gsha()
      dest <- file.path(local_repos_dir(user), it$repo, it$folder, r$repo)
      shiny::withProgress(message = paste("Getting", r$github, "'s work…"), value = 0.5, {
        tryCatch(checkout_copy(r$repo, sha, dest), error = function(e) {
          shiny::showNotification(paste("Couldn't download the repo:", conditionMessage(e)), type = "error", duration = NULL)
          NA_character_ })
      })
    })
    template_lines <- shiny::reactive({
      it <- info(); t <- template_copy(it$repo)
      if (is.na(t)) return(NULL)
      f <- find_item_file(t, input$item, it$folder)
      if (is.na(f)) NULL else readLines(f, warn = FALSE)
    })

    # Rendered side: the student's .html, split by question (html_parts()).
    html_tick <- shiny::reactiveVal(0)
    html_view_parts <- shiny::reactive({
      html_tick(); d <- repo_dir(); it <- info()
      if (is.na(d)) return(NULL)
      f <- find_item_file(d, input$item, it$folder, "html")
      if (is.na(f)) return("missing")
      prefix <- paste0("student-", gsub("[^A-Za-z0-9]", "", basename(d)), "-", it$folder)
      shiny::addResourcePath(prefix, dirname(f))
      tryCatch(html_parts(f, prefix), error = function(e) NULL)
    })

    output$open_buttons <- shiny::renderUI({
      r <- row(); it <- info(); d <- repo_dir()
      qmd <- if (!is.na(d)) find_item_file(d, input$item, it$folder) else NA
      path <- if (!is.na(qmd)) paste0(it$folder, "/", basename(qmd)) else NULL
      shiny::div(class = "d-grid gap-2",
        shiny::tags$a(class = "btn btn-outline-primary btn-sm", target = "_blank",
                      href = github_url(r$repo, gsha(), path), icon_tag("github"), " Open on GitHub (this version)"),
        shiny::actionButton(ns("open_html"), "Open rendered file", icon = icon_tag("file-earmark-richtext"), class = "btn-outline-primary btn-sm"),
        shiny::actionButton(ns("open_proj"), "Open R project", icon = icon_tag("folder2-open"), class = "btn-outline-primary btn-sm"),
        shiny::tags$small(class = "text-muted mono", sprintf("commit %s", substr(gsha(), 1, 7))))
    })

    # Students don't commit their .html (it's in .gitignore), so we render
    # the graded .qmd on this computer. Needs Quarto and the course packages.
    render_student <- function() {
      d <- repo_dir(); it <- info(); shiny::req(!is.na(d))
      html <- find_item_file(d, input$item, it$folder, "html")
      if (!is.na(html)) return(html)
      qmd <- find_item_file(d, input$item, it$folder)
      if (is.na(qmd)) { shiny::showNotification("Couldn't find the student's .qmd file.", type = "error"); return(NA) }
      shiny::withProgress(message = sprintf("Rendering %s's file…", input$student), value = 0.5, {
        out <- tryCatch(system2("quarto", c("render", shQuote(qmd)), stdout = TRUE, stderr = TRUE),
                        error = function(e) conditionMessage(e))
      })
      html <- find_item_file(d, input$item, it$folder, "html")
      if (is.na(html)) {
        shiny::showNotification(shiny::tagList("Rendering failed (the student's code may have an error). Last lines:",
          shiny::tags$pre(paste(utils::tail(out, 6), collapse = "\n"))), type = "error", duration = NULL)
        return(NA)
      }
      html_tick(html_tick() + 1)
      html
    }
    shiny::observeEvent(input$open_html, { h <- render_student(); if (!is.na(h)) utils::browseURL(normalizePath(h)) })
    shiny::observeEvent(input$render_now, render_student())
    shiny::observeEvent(input$open_proj, {
      d <- repo_dir(); shiny::req(!is.na(d))
      rp <- list.files(d, pattern = "\\.Rproj$", full.names = TRUE)
      if (!length(rp)) { utils::browseURL(normalizePath(d)); return() }
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
        rstudioapi::openProject(rp[1], newSession = TRUE)
      else if (.Platform$OS.type == "windows") shell.exec(rp[1]) else system2("open", shQuote(rp[1]))
    })

    shiny::observeEvent(input$get_all, {
      a <- asg(); a <- a[!a$github %in% waiting(), ]; it <- info(); n <- nrow(a); fails <- character()
      shiny::withProgress(message = "Downloading", value = 0, {
        for (i in seq_len(n)) {
          shiny::incProgress(1 / n, detail = a$github[i])
          dest <- file.path(local_repos_dir(user), it$repo, it$folder, a$repo[i])
          sha <- if ("deadline_sha" %in% names(a) && !is.na(a$deadline_sha[i]) && nzchar(a$deadline_sha[i])) a$deadline_sha[i] else a$sha[i]
          ok <- tryCatch({ checkout_copy(a$repo[i], sha, dest); TRUE }, error = function(e) FALSE)
          if (!ok) fails <- c(fails, a$github[i])
        }
      })
      shiny::showNotification(if (length(fails)) paste("Couldn't download:", paste(fails, collapse = ", ")) else
        sprintf("All %d downloaded.", n), type = if (length(fails)) "warning" else "message", duration = 8)
    })

    # --- main panel: one card per question ---------------------------------
    qs <- shiny::reactive({ r <- rubric(); if (is.null(r)) NULL else rubric_questions(r) })
    is_completion <- shiny::reactive({ q <- qs(); !is.null(q) && nrow(q) == 1 && info()$type == "section" })

    output$main <- shiny::renderUI({
      if (is.null(asg())) return(empty_state("No grading assigned to you yet."))
      if (!nrow(asg())) return(empty_state(if (is_section()) "No submissions from your section yet. Click \"Check for new commits\" on the left to look again." else "Nobody to grade here."))
      if (isTRUE(input$student %in% waiting())) {
        a <- asg(); w <- if (("wait_until" %in% names(a))) a$wait_until[a$github == input$student][1] else ""
        return(shiny::div(class = "alert alert-info mt-3", icon_tag("hourglass-split"),
          shiny::strong(sprintf(" %s: not yet", input$student)),
          if (!is.na(w) && nzchar(w)) sprintf(" — their deadline is %s (extension, or their section hasn't ended).", fmt_short(w)) else " — they have an extension.",
          shiny::p(class = "mb-0 mt-2", "Don't grade them yet. They stay on your list; once the deadline has passed, the version",
                   "to grade is updated and they'll show up like everyone else.")))
      }
      r <- rubric(); q <- qs()
      if (is.null(r)) return(empty_state(sprintf("No rubric found. Add stat100-ca-shared/rubrics/%s-rubric.csv.", input$item)))
      d <- repo_dir()
      lines <- if (!is.na(d)) { f <- find_item_file(d, input$item, info()$folder); if (is.na(f)) NULL else readLines(f, warn = FALSE) }
      chunks <- if (!is.null(lines)) split_questions(lines)
      prev <- grades(); prev <- prev[prev$github == input$student, ]
      tl <- template_lines()
      hp <- html_view_parts()
      gen <- r[r$name == "all_questions", ]

      header <- shiny::div(class = "grade-head",
        shiny::h3(input$student),
        shiny::uiOutput(ns("total")),
        if (nrow(prev)) shiny::span(class = "badge bg-success", "graded — saving again replaces it"))

      header <- shiny::tagList(header, shiny::uiOutput(ns("late_box")))
      if (is.null(lines)) header <- shiny::tagList(header,
        shiny::div(class = "alert alert-warning", "Couldn't find this student's file. Use the buttons on the left to look at the repo."))

      cards <- lapply(seq_len(nrow(q)), function(i) {
        qn <- q$question[i]
        opts <- r[r$name == qn, ]
        p <- prev[prev$question == qn, ]
        sel <- if (nrow(p)) strsplit(p$codes[1] %||% "", "---", fixed = TRUE)[[1]] else character()
        ans <- if (is_completion()) {
          if (!is.null(lines)) data.frame(line = seq_along(lines), text = lines,
                                          student = if (length(tl)) !(trimws(lines) %in% trimws(tl)) & trimws(lines) != "" else FALSE)
        } else if (!is.null(chunks)) answer_for(lines, chunks, qn, tl)
        choice_ui <- if (is_completion()) {
          ch <- c("Complete (100%)" = "0", stats::setNames(opts$prompt_code, opts$prompt_message),
                  "Not submitted (0%)" = "404")
          shiny::radioButtons(ns(paste0("codes_", i)), NULL, choices = ch, selected = sel[1] %||% character(0))
        } else {
          spec <- stats::setNames(opts$prompt_code, sprintf("%s  (−%s)", opts$prompt_message, opts$points_to_remove))
          g <- stats::setNames(gen$prompt_code, sprintf("%s%s", gen$prompt_message,
                 ifelse(gen$points_to_remove > 0, sprintf("  (−%s%%)", gen$points_to_remove), "")))
          shiny::checkboxGroupInput(ns(paste0("codes_", i)), NULL, choices = c(spec, g), selected = sel)
        }
        fb0 <- if (nrow(p) && !is.na(p$feedback[1])) p$feedback[1] else feedback_text(r, qn, sel)
        auto[[as.character(i)]] <- feedback_text(r, qn, sel)
        adj0 <- if (nrow(p) && !is.na(p$adjustment[1])) p$adjustment[1] else 0
        if (is_completion() && q$max_points[i] > 0) adj0 <- round(adj0 * 100 / q$max_points[i], 4)   # shown as a percent
        bslib::card(class = "question-card",
          bslib::card_header(shiny::div(class = "d-flex justify-content-between",
            shiny::span(qn), shiny::uiOutput(ns(paste0("score_", i)), inline = TRUE))),
          bslib::layout_columns(col_widths = c(7, 5),
            bslib::navset_underline(
              bslib::nav_panel("Code (.qmd)", answer_view(ans)),
              bslib::nav_panel("Output (.html)", rendered_view(hp, qn, is_completion(), ns))),
            shiny::div(choice_ui,
              shiny::div(class = "adjust-row",
                if (is_completion()) shiny::numericInput(ns(paste0("adj_", i)), "Adjust (+/− %)", value = adj0, step = 5, width = "150px")
                else shiny::numericInput(ns(paste0("adj_", i)), "Adjust points (+/−)", value = adj0, step = 0.25, width = "150px"),
                shiny::tags$small(class = "text-muted", if (is_completion()) "Percentage points, e.g. −5 or +5; explain it in the feedback."
                                  else "e.g. −0.001 for something minor; explain it in the feedback.")),
              shiny::div(class = "d-flex justify-content-between align-items-end",
                shiny::tags$label(`for` = ns(paste0("fb_", i)), "Feedback the student will see"),
                shiny::actionLink(ns(paste0("rebuild_", i)), "Rebuild from rubric", class = "small")),
              shiny::textAreaInput(ns(paste0("fb_", i)), NULL, value = fb0, rows = 4, width = "100%"))))
      })
      shiny::tagList(header, cards,
        shiny::div(class = "save-bar",
          if (!is_completion()) shiny::actionButton(ns("all_correct"), "Mark unanswered questions correct", class = "btn-outline-secondary"),
          shiny::actionButton(ns("save"), "Save", icon = icon_tag("check2"), class = "btn-outline-primary"),
          shiny::actionButton(ns("save_next"), "Save & next student", icon = icon_tag("arrow-right"), class = "btn-primary")))
    })

    # live scores (rubric + adjustment)
    current_codes <- function(i) input[[paste0("codes_", i)]] %||% character()
    # The adjustment in points. Section (completion) work is adjusted in
    # percent of the total, so it's converted here; grades.csv stores points.
    adj_points <- function(q, i) {
      a <- suppressWarnings(as.numeric(input[[paste0("adj_", i)]])); if (!length(a) || is.na(a)) a <- 0
      if (is_completion()) a * q$max_points[i] / 100 else a
    }
    current_points <- function(r, q, i)
      final_points(score_question(r, q$question[i], current_codes(i)), adj_points(q, i), q$max_points[i])
    shiny::observe({
      q <- qs(); r <- rubric(); shiny::req(q)
      lapply(seq_len(nrow(q)), function(i) {
        output[[paste0("score_", i)]] <- shiny::renderUI({
          s <- current_points(r, q, i)
          shiny::span(class = if (is.na(s)) "score-pill score-empty" else "score-pill",
                      if (is.na(s)) sprintf("— / %s", q$max_points[i]) else sprintf("%s / %s", s, q$max_points[i]))
        })
      })
    })

    # Feedback text follows the ticked options until the grader edits it.
    auto <- shiny::reactiveValues()
    fb_observers <- list()
    shiny::observe({
      q <- qs(); r <- rubric(); shiny::req(q)
      for (o in fb_observers) o$destroy()
      fb_observers <<- unlist(lapply(seq_len(nrow(q)), function(i) {
        key <- as.character(i)
        list(
          shiny::observeEvent(input[[paste0("codes_", i)]], {
            new <- feedback_text(r, q$question[i], current_codes(i))
            cur <- input[[paste0("fb_", i)]] %||% ""
            if (!nzchar(trimws(cur)) || identical(cur, shiny::isolate(auto[[key]]) %||% ""))
              shiny::updateTextAreaInput(session, paste0("fb_", i), value = new)
            auto[[key]] <- new
          }, ignoreNULL = FALSE, ignoreInit = TRUE),
          shiny::observeEvent(input[[paste0("rebuild_", i)]], {
            new <- feedback_text(r, q$question[i], current_codes(i))
            shiny::updateTextAreaInput(session, paste0("fb_", i), value = new); auto[[key]] <- new
          }, ignoreInit = TRUE))
      }), recursive = FALSE)
    })
    output$total <- shiny::renderUI({
      q <- qs(); r <- rubric(); shiny::req(q)
      s <- vapply(seq_len(nrow(q)), function(i) current_points(r, q, i), 0)
      shiny::div(class = "total-pill", sprintf("Total %s / %s", round(sum(s, na.rm = TRUE), 2), sum(q$max_points)),
                 if (any(is.na(s))) shiny::span(class = "text-muted small", sprintf(" · %d not graded yet", sum(is.na(s)))))
    })

    shiny::observeEvent(input$all_correct, {
      q <- qs()
      for (i in seq_len(nrow(q))) if (!length(current_codes(i)))
        shiny::updateCheckboxGroupInput(session, paste0("codes_", i), selected = "0")
    })

    # --- section work committed after section: the CA's call ---------------
    late_box <- function() {
      r <- tryCatch(row(), error = function(e) NULL)
      if (is.null(r) || !"status" %in% names(r) || !identical(r$status, "after section")) return(NULL)
      late_tick()
      m <- read_late_marks(grader(), input$item); m <- m[m$github == r$github, ]
      prior <- section_late_counts(grader()); prior <- prior[tolower(prior$github) == tolower(r$github) & prior$decision == "late" & prior$item != input$item, ]
      has_dl <- !is.na(r$deadline_sha) && nzchar(r$deadline_sha %||% "")
      ver <- input$version %||% "deadline"   # not isolated: the box is redrawn with the current choice
      shiny::div(class = "alert alert-warning late-box",
        shiny::div(icon_tag("clock-history"), shiny::strong(sprintf(" Committed after section: %s commit%s after %s", r$late_commits,
          if (identical(as.character(r$late_commits), "1")) "" else "s", fmt_short(r$deadline))), sprintf(" (last one %s).", fmt_short(r$commit_time))),
        shiny::div(class = "small", if (nrow(prior)) shiny::strong(sprintf("Marked late before: %d time%s (%s). ", nrow(prior),
          if (nrow(prior) == 1) "" else "s", paste(prior$item, collapse = ", "))),
          "Only you know what was covered in section, so it's your call."),
        if (has_dl) shiny::radioButtons(ns("version"), "Grade which version?", inline = TRUE, selected = ver,
          choices = stats::setNames(c("deadline", "newest"), c(sprintf("At the end of section (%s)", fmt_short(r$deadline_commit_time)),
                                                               sprintf("Newest (%s)", fmt_short(r$commit_time)))))
        else shiny::div(class = "small", "There was no commit before the end of section, so only the newest version exists."),
        shiny::div(class = "d-flex gap-2 align-items-center flex-wrap",
          if (nrow(m)) shiny::span(class = if (m$decision[1] == "late") "badge bg-danger" else "badge bg-success",
                                   if (m$decision[1] == "late") "Marked: late — warned" else "Marked: not late"),
          shiny::actionButton(ns("mark_late"), "Late — warn the student", class = "btn-outline-danger btn-sm"),
          shiny::actionButton(ns("mark_ok"), "Not late (extra work after section)", class = "btn-outline-success btn-sm"),
          if (nrow(m)) shiny::actionLink(ns("mark_clear"), "undo", class = "small")),
        if (nrow(m) && m$decision[1] == "late") {
          ros <- my_section_students(grader()); s <- ros[tolower(ros$github) == tolower(r$github), ]
          nm <- s$name[1] %||% ""; first <- trimws(sub("^[^,]*,\\s*", "", nm)); first <- sub(" .*$", "", first)
          shiny::tags$details(class = "mt-2", open = NA, shiny::tags$summary("Email to the student"),
            email_panel_ui(ns("late_mail"), stats::na.omit(s$email[1]),
              fill_template("section-late", list(first_name = if (nzchar(first)) first else r$github, item = input$item,
                deadline = fmt_short(r$deadline), last_commit = fmt_short(r$commit_time), sender = user$name))))
        })
    }
    output$late_box <- shiny::renderUI(late_box())
    mark <- function(decision) {
      save_late_mark(grader(), input$item, input$student, decision, by = acting_username(user)); late_tick(late_tick() + 1)
    }
    shiny::observeEvent(input$mark_late, mark("late"))
    shiny::observeEvent(input$mark_ok, mark("not late"))
    shiny::observeEvent(input$mark_clear, mark(""))
    shiny::observeEvent(input$student, shiny::updateRadioButtons(session, "version", selected = "deadline"), ignoreInit = TRUE)

    do_save <- function() {
      q <- qs(); r <- rubric()
      codes <- lapply(seq_len(nrow(q)), current_codes)
      missing <- q$question[lengths(codes) == 0]
      if (length(missing)) {
        shiny::showNotification(paste("Pick at least one option for:", paste(missing, collapse = ", ")), type = "warning", duration = 8)
        return(FALSE)
      }
      rp <- vapply(seq_len(nrow(q)), function(i) score_question(r, q$question[i], codes[[i]]), 0)
      adj <- vapply(seq_len(nrow(q)), function(i) adj_points(q, i), 0)
      rows <- data.frame(
        question = q$question,
        codes = vapply(codes, paste, "", collapse = "---"),
        rubric_points = rp, adjustment = adj,
        points = vapply(seq_len(nrow(q)), function(i) final_points(rp[i], adj[i], q$max_points[i]), 0),
        max_points = q$max_points,
        feedback = vapply(seq_len(nrow(q)), function(i) trimws(input[[paste0("fb_", i)]] %||% ""), ""),
        sha = gsha(), stringsAsFactors = FALSE)
      ok <- tryCatch({ save_student_grades(grader(), input$item, input$student, rows, acting_username(user)); TRUE },
                     error = function(e) { shiny::showNotification(paste("Not saved:", conditionMessage(e)), type = "error", duration = NULL); FALSE })
      if (ok) { shiny::showNotification(sprintf("Saved %s.", input$student), type = "message"); write_progress(grader()) }
      ok
    }
    shiny::observeEvent(input$save, if (do_save()) saved_tick(saved_tick() + 1))
    shiny::observeEvent(input$save_next, {
      if (!do_save()) return()
      a <- asg(); cur <- input$student
      saved_tick(saved_tick() + 1)
      left <- a$github[!a$github %in% c(done(), cur, waiting())]
      if (length(left)) shiny::updateSelectInput(session, "student", selected = left[1])
      else shiny::showNotification("That was the last one — all done!", type = "message", duration = 8)
    })
  })
}

# The rendered output for one question (or the whole page for section work).
rendered_view <- function(hp, question, whole = FALSE, ns = identity) {
  if (identical(hp, "missing")) return(shiny::div(class = "answer empty-state",
    shiny::p("Not rendered yet on this computer."),
    shiny::tags$button(type = "button", class = "btn btn-outline-primary btn-sm",
      onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})", ns("render_now")),
      icon_tag("play"), " Render the student's file"),
    shiny::p(class = "small mt-2", "Takes a few seconds. Uses Quarto and the course R packages on your computer.")))
  if (is.null(hp)) return(shiny::div(class = "answer empty-state", "Couldn't read the rendered file."))
  h <- if (whole) paste(hp$html, collapse = "\n") else hp$html[hp$key == question_key(question)]
  if (!length(h) || !nzchar(h)) return(shiny::div(class = "answer empty-state", "Couldn't find this question in the rendered file."))
  shiny::div(class = "rendered-answer", shiny::HTML(h))
}

# Student's answer: their lines highlighted, our template text muted.
answer_view <- function(ans) {
  if (is.null(ans) || !nrow(ans)) return(shiny::div(class = "answer empty-state", "Couldn't find this question in the student's file — check the rendered file."))
  any_student <- any(ans$student)
  shiny::div(class = "answer",
    if (!any_student) shiny::div(class = "answer-flag", icon_tag("exclamation-triangle"), " No new lines here — the answer may be missing (or the template couldn't be compared)."),
    shiny::tags$pre(class = "answer-code", lapply(seq_len(nrow(ans)), function(k)
      shiny::tags$span(class = if (ans$student[k]) "ln mine" else "ln",
                       if (nzchar(ans$text[k])) ans$text[k] else "\u00a0"))))
}
