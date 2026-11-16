# ---------------------------------------------------------------------------
# tab-canvas.R — admin: "Post to Canvas". Every score, comment and EXCUSED
# that goes to Canvas is sent from here. Logic: R/17-canvas-post.R
# ---------------------------------------------------------------------------

canvas_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-wide",
    shiny::div(class = "page-head", shiny::h2("Post to Canvas"),
      shiny::div(style = "min-width: 340px;", shiny::selectInput(ns("item"), NULL, choices = NULL, width = "100%"))),
    shiny::p(class = "text-muted small",
      "Scores, feedback comments, late notes and EXCUSED for homework, sections and quizzes all go to Canvas from here.",
      "Every comment is added on Canvas as a new comment (nothing is overwritten). Each post is recorded in admin/data/canvas-posted.csv.",
      "If a grade changes after it was posted (a regrade), the student shows up again as \"changed since posted\" and gets a short Regrade comment."),
    shiny::uiOutput(ns("overview")),
    shiny::uiOutput(ns("body")))
}

canvas_server <- function(id, user, refresh) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tick <- shiny::reactiveVal(0)
    bump <- function() tick(tick() + 1)

    summ <- shiny::reactive({ tick(); refresh(); canvas_summary(user) })

    shiny::observe({
      s <- summ(); its <- canvas_items(user)
      lab <- paste0(its$item, " · ", its$title)
      k <- match(its$item, s$item)
      extra <- ifelse(!is.na(k) & s$changed[k] > 0, sprintf("  — %d changed", s$changed[k]), "")
      extra <- paste0(extra, ifelse(!is.na(k) & s$ready[k] > 0, sprintf("  — %d to post", s$ready[k]), ""))
      cur <- shiny::isolate(input$item)
      shiny::updateSelectInput(session, "item",
        choices = if (nrow(its)) stats::setNames(its$item, paste0(lab, extra)) else c("Nothing to post yet" = ""),
        selected = if (!is.null(cur) && cur %in% its$item) cur else its$item[1])
    })

    # Items with something waiting (new grades or regrade changes).
    output$overview <- shiny::renderUI({
      s <- summ(); s <- s[s$ready + s$changed > 0, ]
      if (!nrow(s)) return(NULL)
      shiny::div(class = "alert alert-info small",
        icon_tag("send"), " Waiting to be posted: ",
        paste(sprintf("%s (%s)", s$item, ifelse(s$changed > 0 & s$ready > 0, sprintf("%d new, %d changed", s$ready, s$changed),
                                         ifelse(s$changed > 0, sprintf("%d changed", s$changed), sprintf("%d new", s$ready)))),
              collapse = "; "))
    })

    q <- shiny::reactive({ tick(); refresh(); shiny::req(input$item)
      canvas_queue(input$item, zero_missing = isTRUE(input$zero)) })

    output$body <- shiny::renderUI({
      it <- item_info(input$item); x <- q()
      if (is.null(x)) return(shiny::div(class = "alert alert-warning",
        if (it$type == "quiz") "No Poll Everywhere file for this quiz in admin/inputs/pollev/." else "Run Find submissions for this item first (Admin tab)."))
      n <- function(s) sum(x$state %in% s)
      to_send <- n(c("ready", "changed since posted"))
      waiting <- n(c("not graded yet", "extension (waiting)", "needs late review"))
      unmatched <- attr(x, "unmatched") %||% 0
      cid <- it$canvas_id
      shiny::tagList(
        bslib::layout_column_wrap(width = 1/5, class = "stat-row",
          stat_tile("Ready to post", n("ready")),
          stat_tile("Changed since posted", n("changed since posted"), warn = n("changed since posted") > 0),
          stat_tile("Already posted", n(c("posted", "excused (posted)"))),
          stat_tile("Not ready yet", waiting, warn = waiting > 0),
          stat_tile("Canvas assignment", if (is.na(cid) || !nzchar(cid)) "unknown" else cid, small = TRUE)),
        if (is.na(cid) || !nzchar(cid)) shiny::div(class = "alert alert-warning",
          "No Canvas assignment id for this item in calendar.csv. Use Calendar > Sync from Canvas (or add it to the canvas_id column),",
          "or type it here: it's the number at the end of the Canvas assignment's web address.",
          shiny::textInput(ns("cid"), NULL, placeholder = "e.g. 1135126", width = "220px")),
        if (unmatched) shiny::div(class = "alert alert-warning",
          sprintf("%d Poll Everywhere row(s) aren't matched to a student yet. Match them on the Quizzes tab first, or they won't get a score.", unmatched)),
        if (n("needs late review")) shiny::div(class = "alert alert-warning",
          sprintf("%d late submission(s) still need a decision on the Late & excused tab.", n("needs late review"))),
        if (n("no Canvas id")) shiny::div(class = "alert alert-warning",
          sprintf("%d student(s) have no Canvas id (Students tab; download a fresh Canvas gradebook).", n("no Canvas id"))),
        if (waiting && to_send) shiny::p(class = "small text-muted",
          sprintf("%d student(s) aren't ready yet (not graded, extension still running, or late review). You can post the others now; they'll show up here when they're ready.", waiting)),
        shiny::div(class = "d-flex gap-3 align-items-center my-3 flex-wrap",
          shiny::actionButton(ns("post"), sprintf("Post %d to Canvas", to_send), class = "btn-primary",
                              disabled = if (!to_send) NA else NULL),
          shiny::checkboxInput(ns("zero"), if (it$type == "quiz") "Give 0 to students not in the file" else "Give 0 (with a comment) to students with no submission",
                               isTRUE(shiny::isolate(input$zero))),
          if (to_send) shiny::actionLink(ns("mark"), "Already posted another way — mark these as posted", class = "small")),
        bslib::layout_columns(col_widths = c(7, 5),
          bslib::card(bslib::card_header("Students — click a row to see exactly what will be sent"),
            bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table")))),
          bslib::card(bslib::card_header("What will be sent"),
            bslib::card_body(fillable = FALSE, shiny::uiOutput(ns("preview"))))))
    })

    shown <- shiny::reactive({
      x <- q(); shiny::req(x)
      data.frame(Student = x$name, GitHub = x$github, State = x$state,
                 Send = ifelse(x$action == "excuse", "EXCUSED", ifelse(nzchar(x$action), fmt_pts(x$score), "")),
                 `Last posted` = ifelse(is.na(x$last_posted_at), "",
                    paste0(ifelse(x$last_action %in% "excuse", "EXCUSED", fmt_pts(x$last_score)), "  (", substr(x$last_posted_at, 1, 16), ")")),
                 Detail = x$detail, check.names = FALSE)
    })
    output$table <- DT::renderDT(DT::datatable(shown(), rownames = FALSE, selection = "single",
                                               options = list(dom = "ftp", pageLength = 15)))

    output$preview <- shiny::renderUI({
      x <- q(); i <- input$table_rows_selected
      if (length(i) != 1 || i > nrow(x)) return(shiny::p(class = "text-muted small", "Click a student in the table."))
      r <- x[i, ]; m <- canvas_messages(r)
      box <- function(title, txt) shiny::tagList(shiny::div(class = "small fw-bold mt-2", title),
        shiny::tags$pre(class = "small", style = "white-space: pre-wrap;", txt))
      hist <- read_canvas_log(); hist <- hist[tolower(hist$item) == tolower(input$item) & hist$canvas_user_id %in% r$canvas_user_id & nzchar(hist$sent), ]
      shiny::div(
        shiny::strong(r$name), " ", shiny::span(class = "text-muted", r$state),
        if (r$action == "excuse") shiny::p("Will be marked EXCUSED on Canvas (no comment).")
        else if (!nzchar(r$action)) shiny::p(class = "text-muted", "Nothing will be sent for this student yet.")
        else if (r$state %in% c("posted", "excused (posted)")) shiny::p(class = "text-muted", "Already on Canvas; nothing new to send.")
        else shiny::tagList(shiny::p(sprintf("Score: %s%s", fmt_pts(r$score),
                                             if (!is.na(r$last_score)) sprintf(" (on Canvas now: %s)", fmt_pts(r$last_score)) else "")),
          if (nzchar(m$c1)) box("New comment", m$c1),
          if (nzchar(m$c2)) box(if (nzchar(m$c1)) "Second new comment" else "New comment", m$c2),
          if (!nzchar(m$c1) && !nzchar(m$c2)) shiny::p(class = "text-muted", "No comment.")),
        if (nrow(hist)) shiny::tags$details(class = "mt-2",
          shiny::tags$summary(sprintf("Comments already on Canvas (%d post%s)", nrow(hist), if (nrow(hist) > 1) "s" else "")),
          lapply(seq_len(nrow(hist)), function(k) {
            parts <- strsplit(hist$sent[k], "\n\n-----\n\n", fixed = TRUE)[[1]]
            lapply(seq_along(parts), function(j) box(sprintf("Comment posted %s by %s", substr(hist$posted_at[k], 1, 16), hist$posted_by[k]), parts[j]))
          })))
    })

    do_post <- function(mark_only) {
      x <- q()
      res <- shiny::withProgress(message = if (mark_only) "Recording..." else "Posting to Canvas...", value = 0, {
        tryCatch(post_to_canvas(input$item, x, user, mark_only = mark_only, canvas_id = input$cid,
                                progress = function(d, t) shiny::setProgress(d / t, detail = sprintf("%d of %d", d, t))),
                 error = function(e) e) })
      if (inherits(res, "error")) shiny::showNotification(conditionMessage(res), type = "error", duration = NULL)
      else shiny::showNotification(sprintf("%s %d student(s).", if (mark_only) "Recorded as posted:" else "Posted to Canvas:", res), type = "message")
      bump()
    }
    shiny::observeEvent(input$post, {
      x <- q(); k <- x$state %in% c("ready", "changed since posted") & nzchar(x$action)
      shiny::showModal(shiny::modalDialog(title = sprintf("Post %s to Canvas?", input$item),
        shiny::p(sprintf("%d new, %d changed since last post (%d EXCUSED). Students see grades and comments as Canvas's grade posting policy for this assignment allows.",
                         sum(k & x$state == "ready"), sum(k & x$state == "changed since posted"), sum(k & x$action == "excuse"))),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("confirm"), "Post", class = "btn-primary"))))
    })
    shiny::observeEvent(input$confirm, { shiny::removeModal(); do_post(FALSE) })
    shiny::observeEvent(input$mark, { do_post(TRUE) })
  })
}
