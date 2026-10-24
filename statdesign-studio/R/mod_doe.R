# One wizard for every designed experiment: data, roles, context and settings, results.

POSTHOC_NAMES <- c(tukey = "Tukey HSD", lsd = "Fisher LSD", bonferroni = "Bonferroni", scheffe = "Scheff\u00e9", dunnett = "Dunnett")
DOE_STEPS <- c("Data", "Map variables", "Context & settings", "Results")
LEVEL_COLOURS <- c("#008540", "#003E51", "#AA8A00", "#00573F", "#C9B643", "#6B6F7B", "#A42A1F", "#4B8BBE", "#7A4EAB", "#D9822B")

# Numeric codes sort as numbers; text levels keep the order they first appear in the data.
level_order <- function(x) {
  u <- unique(as.character(x[!is.na(x) & trimws(as.character(x)) != ""]))
  p <- suppressWarnings(as.numeric(u))
  if (length(u) && all(!is.na(p))) u[order(p)] else u
}

doe_ui <- function(id, design) {
  ns <- NS(id)
  tagList(
    page_head(design$name, uiOutput(ns("subtitle"), inline = TRUE),
              btn(ns("how"), sprintf("How %s works", design$short), "book", size = "bsm")),
    uiOutput(ns("stepper")),
    tabsetPanel(
      id = ns("steps"), type = "hidden",
      tabPanelBody("1", doe_step1_ui(ns, design)),
      tabPanelBody("2", doe_step2_ui(ns, design)),
      tabPanelBody("3", doe_step3_ui(ns, design)),
      tabPanelBody("4", doe_step4_ui(ns, design))
    )
  )
}

doe_step1_ui <- function(ns, design) {
  div(class = "split",
      kcard(div(class = "col", style = "gap:16px",
                div(h2(class = "ct", "Choose the data for this analysis"),
                    p(class = "small", style = "margin:0", "Pick a dataset you added in Data Studio, or start from the class example.")),
                uiOutput(ns("ds_pick")))),
      div(class = "col",
          kcard(div(class = "col", style = "gap:12px",
                    h3(class = "ct", "Need data?"),
                    p(class = "small", style = "margin:0", "Upload Excel or CSV, type it in, or paste it from a spreadsheet in Data Studio."),
                    tags$button(class = "btn-k bs", onclick = nav_js("data"), icon("database", 15), "Open Data Studio"),
                    btn(ns("load_example"), sprintf("Use the %s class example", design$short), "flask"))),
          div(class = "hrow", style = "justify-content:flex-end", btn(ns("to2"), "Continue", "arrowr", "bp"))))
}

role_card <- function(ns, r) {
  kcard(div(class = "role",
            ico_box(r$icon, if (r$required) "ig" else "ip"),
            div(class = "rb",
                div(div(class = "hrow", style = "gap:8px",
                        span(style = "font-size:14.5px;font-weight:600;color:var(--ink)", r$label),
                        pill(if (r$required) "Required" else "Optional", if (r$required) "inf" else "neu")),
                    div(class = "small", r$desc)),
                selectizeInput(ns(paste0("role_", r$key)), NULL, choices = NULL, width = "100%",
                               options = list(placeholder = if (r$required) "Choose a column" else "Choose a column (optional)")),
                uiOutput(ns(paste0("rnote_", r$key))))))
}

doe_step2_ui <- function(ns, design) {
  div(class = "split wide",
      div(class = "col", style = "gap:14px",
          div(h2(class = "ct", "Tell us what each column means"),
              p(class = "small", style = "margin:0", "Suitable columns are listed for each role. A column can have only one role.")),
          lapply(design$roles, function(r) role_card(ns, r))),
      div(class = "col",
          uiOutput(ns("check")),
          uiOutput(ns("layout")),
          div(class = "hrow", btn(ns("back1"), "Back", "arrowl"), div(class = "ml", btn(ns("to3"), "Continue", "arrowr", "bp")))))
}

doe_step3_ui <- function(ns, design) {
  froles <- factor_roles(design)
  div(class = "split wide",
      div(class = "col",
          kcard(div(class = "col", style = "gap:14px",
                    h2(class = "ct", "Experiment context"),
                    textInput(ns("title"), "Experiment title", ""),
                    div(class = "grid", style = "grid-template-columns:minmax(0,2fr) minmax(0,1fr)",
                        textInput(ns("resp_name"), "Response name", ""),
                        textInput(ns("unit"), "Unit", "", placeholder = "e.g. kg/plot")),
                    div(class = "grid g2", lapply(froles, function(r)
                      textInput(ns(paste0("disp_", r$key)), paste(r$label, "display name"), ""))))),
          kcard(div(class = "col", style = "gap:12px",
                    div(class = "hrow", h2(class = "ct", "Level labels"),
                        div(class = "ml", style = "width:220px",
                            selectInput(ns("lab_role"), NULL, setNames(vapply(froles, `[[`, "", "key"), vapply(froles, `[[`, "", "label"))))),
                    uiOutput(ns("lab_help")),
                    uiOutput(ns("labels")))),
          kcard(div(class = "col", style = "gap:12px",
                    div(h2(class = "ct", "Hypotheses"),
                        p(class = "small", style = "margin:0", "Written for you from the names above. Edit them if you like.")),
                    textAreaInput(ns("h0"), tagList(tags$b("H0"), " null hypothesis"), "", rows = 2, width = "100%"),
                    textAreaInput(ns("h1"), tagList(tags$b("H1"), " alternative hypothesis"), "", rows = 2, width = "100%")))),
      div(class = "col",
          kcard(div(class = "col", style = "gap:16px",
                    h2(class = "ct", "Settings"),
                    div(div(class = "klab", "Significance level (α)"),
                        seg_input(ns("alpha"), c("0.01" = "0.01", "0.05" = "0.05", "0.10" = "0.10", "Custom" = "custom"), "0.05"),
                        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("alpha")),
                                         div(style = "margin-top:8px;width:160px", numericInput(ns("alpha_custom"), NULL, 0.05, min = 0.001, max = 0.5, step = 0.005)))),
                    selectInput(ns("conf"), "Confidence level for intervals", c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"), "0.95"),
                    checkboxGroupInput(ns("posthoc"), "Post-hoc methods to run",
                                       choiceNames = list("Tukey HSD (default)", "Fisher LSD", "Bonferroni", "Scheffé", "Dunnett (vs control)"),
                                       choiceValues = list("tukey", "lsd", "bonferroni", "scheffe", "dunnett"),
                                       selected = c("tukey", "lsd", "bonferroni")),
                    div(class = "divider"),
                    radioButtons(ns("assump"), "Assumption handling",
                                 choiceNames = list(
                                   tagList(tags$b("Automatic (recommended)"), div(class = "small", sprintf("Switch to %s if normality or constant variance fails", design$nonpar))),
                                   "Always report ANOVA", "Always report the non-parametric test"),
                                 choiceValues = list("auto", "anova", "nonpar"), selected = "auto"),
                    tags$details(
                      tags$summary(style = "cursor:pointer;font-weight:600;font-size:13px;color:var(--ink)", "Advanced"),
                      div(class = "col", style = "gap:12px;margin-top:12px",
                          radioButtons(ns("sstype"), "Type of sums of squares",
                                       choiceNames = list("Type I, sequential (matches aov in class)", "Type II", "Type III"),
                                       choiceValues = list("I", "II", "III"), selected = "I"),
                          numericInput(ns("seed"), "Random seed (for randomised layouts and resampling)", 2025, min = 1, step = 1))))),
          btn(ns("run"), "Run analysis", "play", "bp", "blg"),
          uiOutput(ns("run_msg"))))
}

doe_step4_ui <- function(ns, design) {
  div(class = "col", uiOutput(ns("results")))
}

doe_server <- function(id, design, store, goto) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    roles <- design$roles
    rkeys <- role_keys(design)
    fkeys <- vapply(factor_roles(design), `[[`, "", "key")

    step <- reactiveVal(1); maxstep <- reactiveVal(1)
    ds_id <- reactiveVal(NULL)
    map <- reactiveVal(setNames(as.list(rep("", length(rkeys))), rkeys))
    labels <- reactiveVal(list())          # role -> data.frame(key, code, label, colour)
    labels_rev <- reactiveVal(0)
    control <- reactiveVal(NULL)
    auto <- reactiveValues()               # last auto-filled text per input, to respect user edits

    go <- function(i) { step(i); maxstep(max(maxstep(), i)); updateTabsetPanel(session, "steps", selected = as.character(i)) }
    observeEvent(input$goto_step, { if (input$goto_step <= maxstep()) go(input$goto_step) })

    output$stepper <- renderUI(stepper_ui(ns, DOE_STEPS, step(), maxstep()))

    ds <- reactive({ id <- ds_id(); if (is.null(id)) NULL else store$datasets[[id]] })
    dat <- reactive({ d <- ds(); req(d); drop_empty_rows(d$data) })

    output$subtitle <- renderUI({
      d <- ds()
      if (is.null(d)) return(span("Choose a dataset to begin."))
      span(sprintf("Dataset: %s · %d rows", d$name, nrow(drop_empty_rows(d$data))))
    })

    observeEvent(input$how, showModal(modalDialog(title = design$name, easyClose = TRUE, footer = modalButton("Close"),
                                                  how_text(design))))

    # ---------- Step 1 ----------
    observe({
      if (is.null(ds_id()) && !is.null(store$active) && store$active %in% names(store$datasets)) ds_id(store$active)
      if (!is.null(ds_id()) && !(ds_id() %in% names(store$datasets))) ds_id(NULL)
    })
    output$ds_pick <- renderUI({
      dss <- store$datasets
      if (!length(dss)) return(note(kind = "w", ic = "alert", "You have no datasets yet. Open Data Studio, or use the class example."))
      div(class = "optcards", radioButtons(ns("ds"), NULL, selected = isolate(ds_id()),
        choiceNames = unname(lapply(dss, function(d) span(span(class = "oc-t", d$name),
                                                   span(class = "oc-s", sprintf("%d rows · %d columns · %s", nrow(drop_empty_rows(d$data)), ncol(d$data), d$source))))),
        choiceValues = names(dss)))
    })
    observeEvent(input$ds, { if (!identical(input$ds, ds_id())) ds_id(input$ds) })
    observeEvent(input$load_example, {
      ex <- example_dataset(design$example)
      store$datasets[[ex$id]] <- ex; store$active <- ex$id
      ds_id(ex$id); go(2)
    })
    observeEvent(input$to2, {
      if (is.null(ds())) { showNotification("Choose a dataset first.", type = "warning"); return() }
      go(2)
    })

    # ---------- Step 2: role choices ----------
    col_info <- reactive({
      d <- dat(); ty <- ds()$types
      lapply(setNames(names(d), names(d)), function(n) {
        v <- d[[n]]; lv <- level_order(v)
        list(type = ty[[n]] %||% "category", nlev = length(lv), lv = lv, nval = sum(!is.na(v)),
             numeric_codes = length(lv) > 0 && all(!is.na(suppressWarnings(as.numeric(lv)))))
      })
    })
    observeEvent(list(ds_id(), names(dat()), ds()$types), {
      ci <- col_info(); d <- dat()
      m <- map()
      valid <- vapply(rkeys, function(k) !nzchar(m[[k]]) || m[[k]] %in% names(d), TRUE)
      need_guess <- !all(valid) || all(!nzchar(unlist(m[vapply(roles, `[[`, TRUE, "required")])))
      if (need_guess) { m <- guess_mapping(design, d, ds()$types); map(m) }
      for (r in roles) {
        cols <- names(ci)
        if (r$kind == "number") cols <- cols[vapply(ci[cols], function(x) x$type == "number", TRUE)]
        if (r$kind == "factor") cols <- cols[vapply(ci[cols], function(x) x$type != "id" || x$nlev < x$nval, TRUE)]
        lab <- vapply(cols, function(n) {
          x <- ci[[n]]
          if (r$kind == "number") sprintf("%s · number, %d values", n, x$nval)
          else if (r$kind == "factor") sprintf("%s · %d levels", n, x$nlev)
          else sprintf("%s · %s", n, names(TYPE_CHOICES)[match(x$type, TYPE_CHOICES)])
        }, "")
        ch <- setNames(cols, lab)
        if (!r$required) ch <- c("None" = "", ch)
        updateSelectizeInput(session, paste0("role_", r$key), choices = ch, selected = m[[r$key]] %||% "", server = FALSE)
      }
    }, ignoreNULL = FALSE)

    for (r in roles) local({
      k <- r$key
      observeEvent(input[[paste0("role_", k)]], {
        v <- input[[paste0("role_", k)]] %||% ""
        m <- map(); if (!identical(m[[k]], v)) { m[[k]] <- v; map(m) }
      }, ignoreInit = TRUE)
      output[[paste0("rnote_", k)]] <- renderUI({
        col <- map()[[k]]; req(nzchar(col %||% ""))
        ci <- col_info()[[col]]; req(ci)
        clash <- sum(unlist(map()) == col) > 1
        tagList(
          if (clash) note(kind = "e", ic = "alert", sprintf("%s is already used for another role.", col)),
          if (k %in% fkeys && ci$numeric_codes)
            note(sprintf("%s is stored as numbers (%s). It will be treated as %d categories, not as a measurement.",
                         col, fmt_levels(ci$lv, 5), ci$nlev)),
          if (k == "response" && ci$type != "number")
            note(kind = "w", ic = "alert", sprintf("%s is not marked as a number column. Change its type in Data Studio.", col))
        )
      })
    })

    check <- reactive({ d <- ds(); req(d); design_check(design, dat(), d$types, map()) })

    output$check <- renderUI({
      ch <- check()
      status <- if (ch$valid && !length(ch$warnings)) pill(paste("Valid", design$short), "pass", "check")
                else if (ch$valid) pill("Valid, with notes", "warn", "alert") else pill("Needs attention", "fail", "x")
      kcard(div(class = "hrow", h2(class = "ct", "Design check"), div(class = "ml", status)),
            div(style = "margin-top:8px", lapply(ch$items, function(it) div(class = "kv-row", span(it$label), span(it$value)))),
            if (length(ch$errors)) div(class = "col", style = "gap:8px;margin-top:12px", lapply(ch$errors, function(e) note(kind = "e", ic = "x", e))),
            if (length(ch$warnings)) div(class = "col", style = "gap:8px;margin-top:12px", lapply(ch$warnings, function(e) note(kind = "w", ic = "alert", e))),
            if (!is.null(ch$formula)) div(class = "hyp", style = "margin-top:14px", div(class = "small", "Model"), div(class = "mono", style = "color:var(--petrol)", ch$formula)))
    })

    output$layout <- renderUI({
      ch <- check(); m <- map()
      req(all(nzchar(unlist(m[c("response", fkeys)]))), !any(duplicated(unlist(m[c("response", fkeys)]))))
      d <- dat(); labs <- labels()
      hint <- switch(design$key, CRD = sprintf("Columns are %s levels.", m$treatment),
                     RCBD = sprintf("Rows are %s, columns are %s.", m$block, m$treatment),
                     sprintf("Rows are %s, columns are %s. Each cell shows the %s%s and the %s.", m$row, m$column, m$treatment,
                             if (design$key == "GLSD") paste(" and", m$greek) else "", m$response))
      kcard(h2(class = "ct", "Layout preview"), p(class = "small", style = "margin:2px 0 0", hint),
            div(class = "tscroll", style = "margin-top:10px", layout_table(design, d, m, labs)))
    })

    observeEvent(input$back1, go(1))
    observeEvent(input$to3, {
      ch <- check()
      if (!ch$valid) { showNotification("Fix the items in the design check before continuing.", type = "warning"); return() }
      go(3)
    })

    # ---------- Labels ----------
    observeEvent(list(map(), dat()), {
      m <- map(); d <- dat(); labs <- labels(); changed <- FALSE
      for (k in fkeys) {
        col <- m[[k]]
        if (!nzchar(col %||% "") || !(col %in% names(d))) next
        lv <- level_order(d[[col]])
        old <- labs[[k]]
        if (!is.null(old) && identical(attr(old, "col"), col) && setequal(old$code, lv)) next
        new <- data.frame(key = seq_along(lv), code = lv, stringsAsFactors = FALSE)
        new$label <- if (!is.null(old) && identical(attr(old, "col"), col)) ifelse(is.na(match(lv, old$code)), lv, old$label[match(lv, old$code)]) else lv
        new$colour <- LEVEL_COLOURS[(seq_along(lv) - 1) %% length(LEVEL_COLOURS) + 1]
        attr(new, "col") <- col
        labs[[k]] <- new; changed <- TRUE
      }
      if (changed) {
        labels(labs); labels_rev(labels_rev() + 1)
        tl <- labs[["treatment"]]
        if (!is.null(tl) && !(isTRUE(control() %in% tl$code))) control(tl$code[1])
      }
    })

    output$lab_help <- renderUI({
      k <- input$lab_role %||% fkeys[1]; col <- map()[[k]]; req(nzchar(col %||% ""))
      lv <- labels()[[k]]$code
      p(class = "small", style = "margin:0",
        if (all(!is.na(suppressWarnings(as.numeric(lv)))))
          sprintf("Your file codes %s as %s. Give each code its real name. The order here is used in charts and tables.", col, paste(lv, collapse = ", "))
        else "Rename levels as you want them to appear. The order here is used in charts and tables.")
    })

    output$labels <- renderUI({
      labels_rev(); k <- input$lab_role %||% fkeys[1]
      L <- isolate(labels())[[k]]; req(L)
      is_trt <- k == "treatment"
      ctrl <- isolate(control())
      n <- nrow(L)
      div(div(class = "lvl-row hd", span("Order"), span("Code in data"), span("Display label"), span("Colour"), span(if (is_trt) "Control (Dunnett)" else "")),
          lapply(seq_len(n), function(i) {
            row <- L[i, ]
            div(class = "lvl-row",
                div(class = "mv",
                    tags$button(disabled = if (i == 1) NA, title = "Move up",
                                onclick = sprintf("Shiny.setInputValue('%s', {role:'%s', key:%d, dir:-1, n:Math.random()})", ns("lvlmove"), k, row$key),
                                HTML('<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M18 15l-6-6-6 6"/></svg>')),
                    tags$button(disabled = if (i == n) NA, title = "Move down",
                                onclick = sprintf("Shiny.setInputValue('%s', {role:'%s', key:%d, dir:1, n:Math.random()})", ns("lvlmove"), k, row$key),
                                HTML('<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M6 9l6 6 6-6"/></svg>'))),
                span(class = "mono", row$code),
                textInput(ns(sprintf("lab_%s_%d", k, row$key)), NULL, row$label),
                tags$input(type = "color", class = "swatch", value = row$colour,
                           onchange = sprintf("Shiny.setInputValue('%s', {role:'%s', key:%d, value:this.value})", ns("lvlcol"), k, row$key)),
                if (is_trt) tags$input(type = "radio", name = ns("ctrl_radio"), checked = if (identical(ctrl, row$code)) NA,
                                       onchange = sprintf("Shiny.setInputValue('%s', '%s')", ns("lvlctrl"), gsub("'", "\\\\'", row$code))) else span())
          }))
    })
    observeEvent(input$lvlmove, {
      mv <- input$lvlmove; labs <- labels(); L <- labs[[mv$role]]; req(L)
      i <- match(mv$key, L$key); j <- i + mv$dir
      if (j >= 1 && j <= nrow(L)) { L[c(i, j), ] <- L[c(j, i), ]; rownames(L) <- NULL; labs[[mv$role]] <- L; labels(labs); labels_rev(labels_rev() + 1) }
    })
    observeEvent(input$lvlcol, {
      x <- input$lvlcol; labs <- labels(); L <- labs[[x$role]]; req(L)
      L$colour[L$key == x$key] <- x$value; labs[[x$role]] <- L; labels(labs)
    })
    observeEvent(input$lvlctrl, control(input$lvlctrl))
    observe({
      labs <- labels(); changed <- FALSE
      for (k in names(labs)) {
        L <- labs[[k]]
        for (i in seq_len(nrow(L))) {
          v <- input[[sprintf("lab_%s_%d", k, L$key[i])]]
          if (!is.null(v) && nzchar(trimws(v)) && !identical(v, L$label[i])) { L$label[i] <- trimws(v); changed <- TRUE }
        }
        labs[[k]] <- L
      }
      if (changed) isolate(labels(labs))
    })

    # Dunnett label names the control.
    observeEvent(list(control(), labels()), {
      tl <- labels()[["treatment"]]; ctrl <- control()
      lab <- if (!is.null(tl) && !is.null(ctrl) && ctrl %in% tl$code) tl$label[match(ctrl, tl$code)] else "control"
      updateCheckboxGroupInput(session, "posthoc",
                               choiceNames = list("Tukey HSD (default)", "Fisher LSD", "Bonferroni", "Scheffé", sprintf("Dunnett (vs %s)", lab)),
                               choiceValues = list("tukey", "lsd", "bonferroni", "scheffe", "dunnett"),
                               selected = isolate(input$posthoc))
    })

    # ---------- Context defaults that follow the mapping until the user edits ----------
    # A field still follows the automatic text while its value is one we filled in ourselves.
    untouched <- function(id) { cur <- isolate(input[[id]]) %||% ""; identical(cur, "") || cur %in% isolate(auto[[id]]) }
    remember <- function(id, value) auto[[id]] <- c(tail(isolate(auto[[id]]), 20), value)
    set_auto <- function(id, value) {
      if (untouched(id)) { updateTextInput(session, id, value = value); remember(id, value) }
    }
    observeEvent(map(), {
      m <- map(); req(nzchar(m$response %||% ""))
      set_auto("resp_name", m$response)
      for (k in fkeys) if (nzchar(m[[k]] %||% "")) set_auto(paste0("disp_", k), m[[k]])
      if (nzchar(m$treatment %||% "")) set_auto("title", sprintf("Effect of %s on %s", m$treatment, m$response))
    })
    names_now <- reactive({
      m <- map()
      list(resp = input$resp_name %||% m$response, trt = input$disp_treatment %||% m$treatment,
           block = if ("block" %in% fkeys) input$disp_block %||% m$block,
           row = if ("row" %in% fkeys) input$disp_row %||% m$row,
           column = if ("column" %in% fkeys) input$disp_column %||% m$column,
           greek = if ("greek" %in% fkeys) input$disp_greek %||% m$greek)
    })
    observe({
      nm <- names_now(); tl <- labels()[["treatment"]]
      req(nzchar(nm$resp %||% ""), nzchar(nm$trt %||% ""))
      lv <- if (!is.null(tl)) paste(tl$label, collapse = ", ") else ""
      adj <- switch(design$key,
                    RCBD = sprintf(", after accounting for %s", nm$block),
                    LSD = sprintf(", after accounting for %s and %s", nm$row, nm$column),
                    GLSD = sprintf(", after accounting for %s, %s and %s", nm$greek, nm$row, nm$column), "")
      h0 <- sprintf("Mean %s is the same for every %s (%s)%s.", nm$resp, nm$trt, lv, adj)
      h1 <- sprintf("At least one %s has a different mean %s.", nm$trt, nm$resp)
      isolate({
        for (p in list(c("h0", h0), c("h1", h1))) {
          if (untouched(p[1])) { updateTextAreaInput(session, p[1], value = p[2]); remember(p[1], p[2]) }
        }
      })
    })
    observeEvent(input$alpha, {
      if (input$alpha %in% c("0.01", "0.05", "0.10"))
        updateSelectInput(session, "conf", selected = sprintf("%.2f", 1 - as.numeric(input$alpha)))
    })
    alpha <- reactive(if (identical(input$alpha, "custom")) input$alpha_custom %||% 0.05 else as.numeric(input$alpha %||% "0.05"))

    # ---------- Run ----------
    settings <- reactive(list(
      design = design$key, dataset = ds()$name, map = map(), labels = labels(), control = control(),
      title = input$title, response_name = input$resp_name, unit = input$unit,
      display = setNames(lapply(fkeys, function(k) input[[paste0("disp_", k)]]), fkeys),
      alpha = alpha(), conf = as.numeric(input$conf), posthoc = input$posthoc, assump = input$assump,
      sstype = input$sstype, seed = input$seed, h0 = input$h0, h1 = input$h1,
      formula = check()$formula))
    res <- reactiveVal(NULL)
    rtab <- reactiveVal("summary")
    run_now <- function() {
      ch <- check()
      if (!ch$valid) { showNotification("The design check found problems. Go back to Map variables.", type = "error"); return() }
      a <- alpha()
      if (is.na(a) || a <= 0 || a >= 1) { showNotification("Choose a significance level between 0 and 1.", type = "warning"); return() }
      r <- withProgress(message = "Running the analysis", value = 0.4,
                        tryCatch(run_doe(design, dat(), map(), labels(), settings()), error = function(e) e))
      if (inherits(r, "error")) {
        showNotification(paste("The analysis could not run:", conditionMessage(r)), type = "error", duration = NULL); return()
      }
      res(r); rtab("summary"); go(4)
    }
    observeEvent(input$run, run_now())
    observeEvent(input$rerun, run_now())
    observeEvent(input$edit_settings, go(3))
    observeEvent(input$rtab, rtab(input$rtab))
    observeEvent(rtab(), updateTabsetPanel(session, "rtabs", selected = rtab()))

    output$results <- renderUI({
      r <- res(); req(r)
      s <- r$settings; d <- r$data
      sub <- sprintf("%s · %s%s · %d %s levels%s · n = %d", design$name, s$response_name %||% r$map$response,
                     if (nzchar(s$unit %||% "")) paste0(" (", s$unit, ")") else "", nlevels(d$treatment), r$names$treatment,
                     if (design$key == "RCBD") sprintf(" × %d %s", nlevels(d$block), r$names$block) else "", nrow(d))
      tagList(
        div(class = "page-head",
            div(class = "grow", h2(class = "ct", style = "font-size:20px", s$title %||% design$name), p(class = "small", style = "margin:2px 0 0", sub)),
            div(class = "hrow",
                btn(ns("edit_settings"), "Edit settings", "edit", size = "bsm"),
                pin_button(ns),
                tags$button(class = "btn-k bs bsm", onclick = sprintf("Shiny.setInputValue('%s', 'code', {priority: 'event'})", ns("rtab")), icon("code", 14), "R code"),
                downloadButton(ns("dl_tables"), label = tagList(icon("download", 14), "Download tables"), icon = NULL, class = "btn-k bp bsm"))),
        uiOutput(ns("stale")),
        uiOutput(ns("rtabbar")),
        tabsetPanel(id = ns("rtabs"), type = "hidden", selected = isolate(rtab()),
                    tabPanelBody("summary", uiOutput(ns("t_summary"), class = "rcol")),
                    tabPanelBody("explore", uiOutput(ns("t_explore"), class = "rcol")),
                    tabPanelBody("anova", uiOutput(ns("t_anova"), class = "rcol")),
                    tabPanelBody("assump", uiOutput(ns("t_assump"), class = "rcol")),
                    tabPanelBody("posthoc", uiOutput(ns("t_posthoc"), class = "rcol")),
                    tabPanelBody("nonpar", uiOutput(ns("t_nonpar"), class = "rcol")),
                    tabPanelBody("code", uiOutput(ns("t_code"), class = "rcol"))))
    })

    output$stale <- renderUI({
      r <- res(); req(r)
      now <- settings(); then <- r$settings
      keys <- c("map", "labels", "control", "title", "response_name", "unit", "display", "alpha", "conf", "posthoc", "assump", "sstype", "seed")
      same <- identical(now[keys], then[keys]) && identical(dat(), isolate(r$raw %||% dat()))
      if (same) return(NULL)
      div(class = "note w", icon("alert", 16), div(style = "flex:1", "Settings or data changed since this run. The results below use the previous settings."),
          btn(ns("rerun"), "Run again", "play", "bgld", "bsm"))
    })

    output$rtabbar <- renderUI({
      r <- res(); req(r)
      div(class = "ktabs", lapply(RESULT_TABS, function(t) {
        lab <- t$label
        tags$button(class = paste("ktab", if (rtab() == t$key) "on"),
                    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("rtab"), t$key),
                    icon(t$icon, 15), lab,
                    if (t$key == "nonpar" && r$recommend == "nonpar") span(class = "pill gold", style = "margin-left:4px;padding:1px 8px", "Recommended"))
      }))
    })

    plot_card_tools <- function(id) div(class = "ml", downloadLink(ns(paste0("dl_", id)), title = "Download PNG", class = "ib", style = "width:30px;height:30px", icon("download", 14)))
    reg_plot <- function(id, fun, h = 320) {
      output[[id]] <- renderPlot({ r <- res(); req(r); fun(r) }, res = 96, bg = "transparent")
      output[[paste0("dl_", id)]] <- downloadHandler(
        filename = function() paste0(id, ".png"),
        content = function(file) ggplot2::ggsave(file, fun(res()) + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA)),
                                                 width = 7, height = h / 96 * 1.1, dpi = 200, bg = "white"))
    }
    ph_method <- reactive({
      r <- res(); req(r)
      avail <- intersect(c("tukey", "lsd", "bonferroni", "scheffe", "dunnett"), r$settings$posthoc %||% character(0))
      if (!length(avail)) avail <- "tukey"
      m <- input$ph_method
      if (is.null(m) || !(m %in% avail)) avail[1] else m
    })
    ph_term <- reactive({ t <- input$ph_term; if (is.null(t) || !(t %in% compared_terms(design))) "treatment" else t })
    letter_method <- reactive({ m <- ph_method(); if (m == "dunnett") "tukey" else m })

    reg_plot("pl_means", function(r) plot_means(r, "treatment", letter_method()), 330)
    reg_plot("pl_means2", function(r) plot_means(r, "treatment", letter_method()), 330)
    reg_plot("pl_box", function(r) plot_box(r, "treatment"), 330)
    reg_plot("pl_box_greek", function(r) plot_box(r, "greek"), 330)
    reg_plot("pl_profile", function(r) if (design$key == "RCBD") plot_profile(r) else plot_square(r), 330)
    reg_plot("pl_qq", plot_qq, 280); reg_plot("pl_rvf", plot_rvf, 280); reg_plot("pl_order", plot_order, 280)
    reg_plot("pl_fdist", function(r) { tr <- trt_row(r); plot_dist("F", tr$F, tr$df, r$anova$dfe, r$alpha) }, 240)
    reg_plot("pl_ci", function(r) {
      tab <- r$posthoc[[ph_term()]]$methods[[ph_method()]]; req(tab)
      plot_ci(tab, r$conf, r$settings$unit %||% "")
    }, 260)
    reg_plot("pl_tukey_ci", function(r) plot_ci(r$posthoc$treatment$methods$tukey, r$conf, r$settings$unit %||% ""), 220)
    reg_plot("pl_chisq", function(r) { m <- r$nonpar$main; plot_dist("chisq", m$stat, m$df, NULL, r$alpha) }, 240)

    # ---------- Summary ----------
    output$t_summary <- renderUI({
      r <- res(); req(r)
      h <- headline(r); tr <- trt_row(r); a <- r$alpha; asm <- r$assumptions
      ph <- r$posthoc$treatment; best <- ph$means[which.max(ph$means$mean), ]
      unit <- r$settings$unit %||% ""
      kpi <- function(label, value, sub, kind = NULL) kcard(class = "soft", div(class = "small", style = "font-weight:500", label),
                                                              div(style = sprintf("font-size:21px;font-weight:600;line-height:30px;color:%s", if (identical(kind, "good")) "var(--green)" else "var(--ink)"), value),
                                                              div(class = "small", sub))
      third <- switch(design$key,
        RCBD = { b <- trt_row(r, "block"); kpi(sprintf("Block (%s) effect", r$names$block), p_txt(b$p),
                                               sprintf("%s · relative efficiency %.2f", if (b$p < a) "Significant" else "Not significant", r$re$value)) },
        LSD = { rw <- trt_row(r, "row"); cl <- trt_row(r, "column")
                kpi("Blocking factors", sprintf("%s / %s", fmt_p(rw$p, 3), fmt_p(cl$p, 3)), sprintf("p for %s / %s · RE %.2f", r$names$row, r$names$column, r$re$value)) },
        GLSD = { g <- trt_row(r, "greek"); kpi(sprintf("%s effect", r$names$greek), p_txt(g$p), sprintf("F(%d, %d) = %.2f", g$df, r$anova$dfe, g$F)) },
        kpi("Model fit", sprintf("R² = %.3f", r$anova$r2), sprintf("CV = %.2f%% · grand mean %.2f", r$anova$cv, r$anova$grand_mean)))
      npass <- sum(asm$status == "pass")
      tk <- ph$methods$tukey; n_ex <- sum(tk$lwr > 0 | tk$upr < 0)
      dec_row <- function(title, val, txt, ok) div(class = "kv-row", style = "align-items:center",
                                                   span(tags$b(style = "color:var(--ink)", title), br(), span(class = "small", txt)),
                                                   if (ok) pill("Reject H0", "pass") else pill("Do not reject", "neu"))
      np <- r$nonpar$main
      tagList(
        kcard(div(class = "hrow", style = "align-items:flex-start;flex-wrap:nowrap",
                  div(class = paste("ico", if (h$sig) "ig" else "ip"), style = "width:44px;height:44px;border-radius:22px", icon(if (h$sig) "check" else "info", 20, 2.5)),
                  div(style = "flex:1;min-width:0", h2(class = "ct", style = "font-size:19px", h$title), p(class = "sub", style = "margin-top:4px", h$why)),
                  div(class = "ml", pill(sprintf("Recommended: %s", if (r$recommend == "nonpar") np$name %||% "non-parametric test" else design_model_name(design)), "gold")))),
        if (isTRUE(r$dropped > 0)) note(kind = "w", ic = "alert", sprintf("%d row%s with a missing response or factor were left out.", r$dropped, if (r$dropped > 1) "s were" else " was")),
        div(class = "grid g4",
            kpi("Treatment effect", p_txt(tr$p), sprintf("F(%d, %d) = %.2f · partial η² = %.2f", tr$df, r$anova$dfe, tr$F, tr$eta2p), if (tr$p < a) "good"),
            kpi(sprintf("Highest mean %s", r$names$treatment), best$level, sprintf("Mean %.2f%s, highest of %d", best$mean, if (nzchar(unit)) paste0(" ", unit) else "", nrow(ph$means))),
            third,
            kpi("Assumptions", sprintf("%d of 3 pass", npass), "Normality, constant variance, independence", if (npass == 3) "good")),
        div(class = "split wide",
            kcard(div(class = "hrow", h2(class = "ct", sprintf("Mean %s by %s", r$settings$response_name %||% r$map$response, r$names$treatment)), plot_card_tools("pl_means")),
                  p(class = "small", style = "margin:2px 0 6px", sprintf("Squares show means with %g%% confidence intervals; dots are the observations. Different letters mean significantly different (%s, α = %s).",
                                                                     100 * r$conf, POSTHOC_NAMES[[letter_method()]], format(a))),
                  plotOutput(ns("pl_means"), height = "330px")),
            div(class = "col",
                kcard(div(class = "hrow", h2(class = "ct", "Assumption scorecard"),
                          tags$a(href = "#", class = "small ml", style = "color:var(--green);font-weight:600",
                                 onclick = sprintf("Shiny.setInputValue('%s', 'assump', {priority: 'event'}); return false;", ns("rtab")), "Details")),
                      div(class = "kv-row", style = "align-items:center", span("Normality", br(), span(class = "small", sprintf("Shapiro-Wilk %s", p_txt(asm$normal$p[1])))), status_pill(asm$status[["normality"]])),
                      div(class = "kv-row", style = "align-items:center", span("Constant variance", br(), span(class = "small", sprintf("Breusch-Pagan %s", p_txt(asm$variance$p[1])))), status_pill(asm$status[["variance"]])),
                      div(class = "kv-row", style = "align-items:center", span("Independence", br(), span(class = "small", sprintf("Durbin-Watson %s", p_txt(asm$indep$p[1])))), status_pill(asm$status[["independence"]]))),
                kcard(h2(class = "ct", "Decision, three ways"),
                      dec_row("P-value", NULL, sprintf("%s vs α = %s", p_txt(tr$p), format(a)), tr$p < a),
                      dec_row("Rejection region", NULL, sprintf("F = %.2f %s F critical %.2f", tr$F, if (tr$F > tr$Fcrit) ">" else "≤", tr$Fcrit), tr$F > tr$Fcrit),
                      dec_row("Confidence intervals", NULL, sprintf("%d of %d Tukey intervals exclude 0", n_ex, nrow(tk)), n_ex > 0),
                      if ((n_ex > 0) != (tr$p < a)) note(kind = "w", ic = "alert", "The Tukey intervals and the F-test disagree here, which can happen near the boundary. Report the F-test for the overall question."))
            )),
        kcard(div(class = "hrow", h2(class = "ct", "Interpretation"),
                  tags$button(class = "btn-k bs bsm ml", onclick = sprintf("navigator.clipboard.writeText(document.getElementById('%s').innerText); this.lastChild.textContent=' Copied'", ns("interp")),
                              icon("copy", 14), span(" Copy"))),
              p(id = ns("interp"), style = "margin:10px 0 0;font-size:14px;line-height:23px;color:var(--text)", interpretation(r))),
        if (!is.null(np) && r$recommend == "anova")
          note(tagList(tags$b(np$name), sprintf(" (non-parametric) gives %s: %s. It is shown in its own tab for comparison.",
                                               if ((np$p < a) == (tr$p < a)) "the same conclusion" else "a different conclusion", p_txt(np$p))))
      )
    })

    # ---------- Explore ----------
    output$t_explore <- renderUI({
      r <- res(); req(r)
      tagList(
        div(class = "grid g2",
            kcard(div(class = "hrow", h2(class = "ct", sprintf("Distribution by %s", r$names$treatment)), plot_card_tools("pl_box")),
                  p(class = "small", style = "margin:2px 0 6px", "Boxes show the middle half of the data; diamonds mark the means."),
                  plotOutput(ns("pl_box"), height = "330px")),
            if (design$key == "CRD") kcard(div(class = "hrow", h2(class = "ct", "Means and confidence intervals"), plot_card_tools("pl_means2")), plotOutput(ns("pl_means2"), height = "330px"))
            else kcard(div(class = "hrow", h2(class = "ct", if (design$key == "RCBD") sprintf("Each %s across %s", r$names$block, r$names$treatment) else "The square as laid out"),
                           plot_card_tools("pl_profile")),
                       p(class = "small", style = "margin:2px 0 6px", if (design$key == "RCBD") "Roughly parallel lines mean the treatment differences are similar in every block, as the additive model assumes."
                                                                     else "Darker cells have a higher response."),
                       plotOutput(ns("pl_profile"), height = "330px"))),
        kcard(h2(class = "ct", sprintf("Summary by %s", r$names$treatment)), div(class = "tscroll", style = "margin-top:10px", means_tbl_ui(r, "treatment"))),
        if (design$key == "GLSD") div(class = "grid g2",
            kcard(div(class = "hrow", h2(class = "ct", sprintf("Distribution by %s", r$names$greek)), plot_card_tools("pl_box_greek")), plotOutput(ns("pl_box_greek"), height = "330px")),
            kcard(h2(class = "ct", sprintf("Summary by %s", r$names$greek)), div(class = "tscroll", style = "margin-top:10px", means_tbl_ui(r, "greek"))))
      )
    })

    # ---------- ANOVA ----------
    output$t_anova <- renderUI({
      r <- res(); req(r)
      an <- r$anova; tr <- trt_row(r); a <- r$alpha
      tk <- r$posthoc$treatment$methods$tukey; n_ex <- sum(tk$lwr > 0 | tk$upr < 0)
      odds <- if (tr$p > 0) round(1 / tr$p) else Inf
      stat <- function(l, v) div(class = "chip", style = "padding:6px 12px;font-size:12.5px", span(class = "small", l), tags$b(style = "color:var(--ink)", v))
      blk_note <- if (design$key == "RCBD") {
        b <- trt_row(r, "block")
        if (b$p < a) sprintf("The %s effect is significant (%s), so blocking removed real variation from the error (relative efficiency %.2f: a CRD would need about %.0f%% more plots for the same precision).",
                             r$names$block, p_txt(b$p), r$re$value, 100 * (r$re$value - 1))
        else sprintf("The %s effect is small (%s), so blocking did not reduce error much in this trial. Keeping it in the model is still correct because the design used it.", r$names$block, p_txt(b$p))
      } else NULL
      tagList(
        kcard(div(class = "hrow", h2(class = "ct", "ANOVA table"), span(class = "mono small ml", r$settings$formula),
                  pill(sprintf("Type %s sums of squares", r$settings$sstype %||% "I"), "neu")),
              div(class = "tscroll", style = "margin-top:12px", anova_tbl_ui(r)),
              div(class = "hrow", style = "margin-top:14px",
                  stat("R² ", fmt_g(an$r2, 3)), stat("CV ", sprintf("%.2f%%", an$cv)), stat("Grand mean ", fmt_g(an$grand_mean)),
                  stat("Root MSE ", fmt_g(an$rmse, 3)),
                  if (!is.null(r$re)) stat(sprintf("Relative efficiency vs %s ", r$re$vs), fmt_g(r$re$value))),
              p(class = "small", style = "margin:10px 0 0", "Signif. codes: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1. Partial η² is the share of variation explained by the factor after removing the other factors."),
              if (!is.null(blk_note)) div(style = "margin-top:12px", note(blk_note))),
        div(h2(class = "ct", "Decide three ways"), p(class = "small", style = "margin:2px 0 0", sprintf("Three ways taught in class to test H0 for %s. They normally agree.", r$names$treatment))),
        div(class = "grid g3",
            kcard(div(class = "hrow", span(class = "pill neu", "1"), h3(class = "ct", "P-value method"), div(class = "ml", if (tr$p < a) pill("Reject H0", "pass") else pill("Do not reject", "neu"))),
                  div(style = "font-size:30px;font-weight:600;color:var(--ink);margin:14px 0 4px", fmt_p(tr$p)),
                  div(class = "small", sprintf("%s α = %s", if (tr$p < a) "<" else "≥", format(a))),
                  p(class = "small", style = "margin-top:12px", if (tr$p < a) sprintf("If %s had no effect, a difference this large would appear in about 1 in %s experiments.", r$names$treatment, format(odds, big.mark = ","))
                                                                else sprintf("A difference this large is quite likely even if %s has no effect.", r$names$treatment))),
            kcard(div(class = "hrow", span(class = "pill neu", "2"), h3(class = "ct", "Rejection region"), div(class = "ml", if (tr$F > tr$Fcrit) pill("Reject H0", "pass") else pill("Do not reject", "neu"))),
                  plotOutput(ns("pl_fdist"), height = "200px"),
                  p(class = "small", style = "margin:6px 0 0", sprintf("Reject H0 when F is beyond F critical = %.2f (the shaded %g%% tail). Observed F = %.2f.", tr$Fcrit, 100 * a, tr$F))),
            kcard(div(class = "hrow", span(class = "pill neu", "3"), h3(class = "ct", "Confidence intervals"), div(class = "ml", if (n_ex > 0) pill("Reject H0", "pass") else pill("Do not reject", "neu"))),
                  plotOutput(ns("pl_tukey_ci"), height = "200px"),
                  p(class = "small", style = "margin:6px 0 0", sprintf("%g%% Tukey intervals for each pair. %s", 100 * r$conf,
                                                                       if (n_ex > 0) sprintf("%d of %d do not contain 0, so the means are not all equal.", n_ex, nrow(tk)) else "All contain 0, so we cannot say the means differ."))))
      )
    })

    # ---------- Assumptions ----------
    output$t_assump <- renderUI({
      r <- res(); req(r)
      asm <- r$assumptions; a <- r$alpha; st <- asm$status
      npname <- r$nonpar$main$name %||% design$nonpar
      n_ok <- sum(st == "pass")
      head_txt <- if (n_ok == 3) "All three assumptions hold, so the parametric ANOVA is reported."
                  else if (any(st[c("normality", "variance")] == "fail")) sprintf("%s fails, so %s is recommended.",
                       paste(c("Normality", "Constant variance")[st[c("normality", "variance")] == "fail"], collapse = " and "), npname)
                  else if (st["independence"] == "fail") "Independence fails. A non-parametric test does not fix this; check how the runs were ordered and randomised."
                  else "Some checks are borderline. ANOVA is kept; compare with the non-parametric result."
      card <- function(title, q, ic, status, tab, plot_id, pass_txt, fail_txt, extra = NULL) {
        kind <- switch(status, pass = "g", fail = "e", borderline = "w", "i")
        kcard(div(class = "col", style = "gap:12px;height:100%",
                  div(class = "hrow", style = "flex-wrap:nowrap;align-items:flex-start", ico_box(ic), div(style = "flex:1;min-width:0", h2(class = "ct", title), div(class = "small", q)), status_pill(status)),
                  div(class = "klab", "Formal tests"), test_tbl_ui(tab),
                  div(class = "hrow", div(class = "klab", "Informal check"), plot_card_tools(plot_id)),
                  plotOutput(ns(plot_id), height = "280px"),
                  div(class = paste("note", kind), icon(if (status == "pass") "check" else "alert", 16),
                      div(if (status == "pass") pass_txt else if (status == "fail") fail_txt else sprintf("Borderline (α ≤ p < 0.10). %s", pass_txt))),
                  extra))
      }
      tagList(
        kcard(div(class = "hrow", style = "flex-wrap:nowrap", div(class = paste("ico", if (n_ok == 3) "ig" else "io"), style = "border-radius:19px", icon(if (n_ok == 3) "check" else "alert", 18, 3)),
                  div(style = "flex:1", h3(class = "ct", head_txt),
                      p(class = "small", style = "margin:2px 0 0", sprintf("Checks use the model residuals, α = %s. Assumptions are checked whether or not the ANOVA is significant.%s", format(a),
                                                                     if (r$anova$dfe < 6) sprintf(" With only %d error degrees of freedom these checks have little power.", r$anova$dfe) else ""))))),
        div(class = "grid g3",
            card("Normality", "Are the residuals normally distributed?", "bar", st[["normality"]], asm$normal, "pl_qq",
                 "Points follow the line and stay near the band, and the formal tests do not reject normality.",
                 sprintf("The residuals depart from normality. %s does not need this assumption.", npname)),
            card("Constant variance", "Is the spread of the residuals constant? (Breusch-Pagan)", "scale", st[["variance"]], asm$variance, "pl_rvf",
                 "Residuals scatter evenly around zero with no funnel shape, and the Breusch-Pagan test does not reject constant variance.",
                 sprintf("The spread of the residuals changes across fitted values. Consider %s or a transformation such as log(y).", npname),
                 if (!is.null(asm$groups)) div(class = "col", style = "gap:6px",
                   div(class = "klab", "Equal variances of the groups (separate check)"), test_tbl_ui(asm$groups),
                   p(class = "small", style = "margin:0", sprintf("Levene and Bartlett compare the spread of the %s groups directly. If they differ, the Welch ANOVA (k samples page) does not assume equal variances.", r$names$treatment)))),
            card("Independence", "Are the residuals independent of run order?", "clock", st[["independence"]], asm$indep, "pl_order",
                 "No trend or long runs above or below zero in run order, so the errors look independent.",
                 "The residuals show a pattern over run order. Rank tests also need independent errors, so the fix is the run order or randomisation, not a different test.",
                 if (!nzchar(r$map$run %||% "")) p(class = "small", style = "margin:0", "No run-order column was mapped, so the order of rows in the data is used."))),
        kcard(class = "soft", h3(class = "ct", "What happens when a check fails"),
              div(class = "grid g3", style = "margin-top:10px",
                  note(kind = "e", ic = "x", tags$b("Normality or constant variance fails (p < α). "), sprintf("In automatic mode the results switch to %s, marked Recommended, and the Summary explains why.", npname)),
                  note(kind = "w", ic = "alert", tags$b("Borderline (α ≤ p < 0.10). "), "ANOVA is kept, with a note to inspect the plot and compare with the non-parametric result."),
                  note(kind = "e", ic = "x", tags$b("Independence fails. "), "Both tests are shown with a warning; rank tests also need independent errors.")))
      )
    })

    # ---------- Post-hoc ----------
    output$t_posthoc <- renderUI({
      r <- res(); req(r)
      avail <- intersect(c("tukey", "lsd", "bonferroni", "scheffe", "dunnett"), r$settings$posthoc %||% character(0))
      if (!length(avail)) avail <- "tukey"
      terms <- compared_terms(design)
      tagList(
        div(class = "hrow",
            if (length(terms) > 1) div(style = "width:240px", selectInput(ns("ph_term"), NULL, setNames(terms, vapply(terms, function(t) r$names[[t]], "")), isolate(ph_term()))),
            seg_input(ns("ph_method"), setNames(avail, POSTHOC_NAMES[avail]), isolate(ph_method()))),
        uiOutput(ns("ph_body"), class = "rcol"))
    })
    output$ph_body <- renderUI({
      r <- res(); req(r)
      t <- ph_term(); m <- ph_method(); ph <- r$posthoc[[t]]
      tab <- ph$methods[[m]]
      if (is.null(tab)) return(note(kind = "w", ic = "alert", "This method could not be computed for these data."))
      info <- POSTHOC_INFO[[m]]; inf <- ph$info
      extra <- switch(m,
        tukey = if (!is.na(inf$hsd)) sprintf("Studentized range q(%g; %d, %d) = %.2f, HSD = %.2f.", r$conf, inf$k, inf$dfe, inf$tukey_q, inf$hsd) else sprintf("Studentized range q(%g; %d, %d) = %.2f (Tukey-Kramer for unequal sizes).", r$conf, inf$k, inf$dfe, inf$tukey_q),
        lsd = if (!is.na(inf$lsd_value)) sprintf("LSD = %.2f with %d error df.", inf$lsd_value, inf$dfe) else "",
        bonferroni = sprintf("%d pairs, so each p-value is multiplied by %d.", inf$m, inf$m),
        scheffe = sprintf("Critical value √((k−1)F) with k = %d.", inf$k),
        dunnett = sprintf("Control: %s.", inf$control))
      all_tab <- {
        mm <- intersect(c("lsd", "tukey", "bonferroni", "scheffe"), names(ph$methods))
        tags$table(class = "kt", tags$thead(tags$tr(tags$th("Pair"), lapply(mm, function(x) tags$th(class = "n", POSTHOC_NAMES[[x]])))),
                   tags$tbody(lapply(seq_len(nrow(ph$methods$tukey)), function(i) tags$tr(tags$td(ph$methods$tukey$pair[i]),
                     lapply(mm, function(x) { p <- ph$methods[[x]]$p[i]
                       tags$td(class = "n", style = if (p < r$alpha) "color:var(--green);font-weight:600", fmt_p(p)) })))))
      }
      dis <- {
        sig <- sapply(c("lsd", "tukey", "bonferroni", "scheffe"), function(x) ph$methods[[x]]$p < r$alpha)
        if (is.null(dim(sig))) sig <- matrix(sig, nrow = 1)
        w <- which(apply(sig, 1, function(z) length(unique(z)) > 1))
        if (length(w)) sprintf("Methods disagree on %s. Fisher LSD is the least conservative; Bonferroni and Scheffé are the most. Tukey is recommended for all pairwise comparisons.",
                               paste(ph$methods$tukey$pair[w], collapse = ", ")) else "All methods agree on every pair."
      }
      tagList(
        div(class = "split wide",
            div(class = "col",
                kcard(h2(class = "ct", info$title), p(class = "small", style = "margin:4px 0 12px", paste(info$desc, extra)),
                      div(class = "tscroll", ph_table_ui(tab, r$conf, r$alpha))),
                kcard(div(class = "hrow", h2(class = "ct", "Confidence intervals for each difference"), plot_card_tools("pl_ci")),
                      plotOutput(ns("pl_ci"), height = "260px"))),
            div(class = "col",
                if (m != "dunnett") kcard(h2(class = "ct", "Grouping letters"),
                      tags$table(class = "kt", style = "margin-top:10px",
                                 tags$thead(tags$tr(tags$th(r$names[[t]]), tags$th(class = "n", "Mean"), tags$th("Group"))),
                                 tags$tbody(lapply(order(-ph$means$mean), function(i) {
                                   lv <- ph$means$level[i]; L <- r$settings$labels[[t]]
                                   colr <- if (!is.null(L)) L$colour[match(lv, make.unique(L$label))] else "#008540"
                                   tags$tr(tags$td(span(style = sprintf("display:inline-block;width:10px;height:10px;border-radius:3px;background:%s;margin-right:8px", colr)), lv),
                                           tags$td(class = "n", fmt_g(ph$means$mean[i])), tags$td(span(class = "pill gold", ph$letters[[m]][[lv]])))
                                 }))),
                      p(class = "small", style = "margin:10px 0 0", "Means that share a letter are not significantly different.")),
                kcard(h2(class = "ct", "Same pairs, all methods"), p(class = "small", style = "margin:2px 0 10px", "Adjusted p-values. Green means significant at α."),
                      div(class = "tscroll", all_tab), p(class = "small", style = "margin:10px 0 0", dis))))
      )
    })

    # ---------- Non-parametric ----------
    np_ph_sel <- reactive(input$np_ph %||% "1")
    output$t_nonpar <- renderUI({
      r <- res(); req(r)
      np <- r$nonpar
      if (!is.null(np$error)) return(note(kind = "e", ic = "alert", paste("The non-parametric test could not run:", np$error)))
      a <- r$alpha; tr <- trt_row(r)
      section <- function(x, idx, title_name) {
        m <- x$main
        kcard(div(class = "col", style = "gap:14px",
                  div(class = "hrow", h2(class = "ct", m$name), div(class = "ml", if (m$p < a) pill("Reject H0", "pass") else pill("Do not reject", "neu"))),
                  div(class = "grid g4",
                      div(div(class = "small", m$stat_name), div(style = "font-size:22px;font-weight:600;color:var(--ink)", fmt_g(m$stat))),
                      div(div(class = "small", "df"), div(style = "font-size:22px;font-weight:600;color:var(--ink)", if (is.na(m$df %||% NA)) "—" else m$df)),
                      div(div(class = "small", "p-value"), div(style = "font-size:22px;font-weight:600;color:var(--ink)", fmt_p(m$p, 4))),
                      if (!is.null(m$effect)) div(div(class = "small", m$effect_name), div(style = "font-size:22px;font-weight:600;color:var(--ink)", fmt_g(m$effect))) else div()),
                  p(class = "small", style = "margin:0", m$about, sprintf(" H0: the distribution of %s is the same for every %s.", r$settings$response_name %||% r$map$response, title_name)),
                  if (!is.null(m$asym)) p(class = "small", style = "margin:0", sprintf("Asymptotic version (Kruskal-Wallis on the ranks): χ²(%d) = %.2f, %s.", m$asym$df, m$asym$stat, p_txt(m$asym$p))),
                  if (!is.null(m$note)) note(kind = "w", ic = "alert", m$note),
                  div(class = "klab", "Mean ranks"),
                  tags$table(class = "kt", tags$thead(tags$tr(tags$th(title_name), tags$th(class = "n", "n"), tags$th(class = "n", "Mean rank"), tags$th(class = "n", "Median"))),
                             tags$tbody(lapply(seq_len(nrow(x$ranks)), function(i) tags$tr(tags$td(x$ranks$level[i]), tags$td(class = "n", x$ranks$n[i]),
                                                                                         tags$td(class = "n", fmt_g(x$ranks$mean_rank[i])), tags$td(class = "n", fmt_g(x$ranks$median[i])))))),
                  div(class = "klab", "Post-hoc"),
                  seg_input(ns(paste0("np_ph", idx)), setNames(as.character(seq_along(x$posthoc)), vapply(x$posthoc, `[[`, "", "name")), "1"),
                  uiOutput(ns(paste0("np_ph_tab", idx)))))
      }
      tagList(
        if (r$recommend == "nonpar") note(kind = "g", ic = "check", tags$b("Recommended. "), if (r$failed) "An ANOVA assumption failed, so this test is the one to report." else "You chose to report the non-parametric test.")
        else note(tags$b("Shown for comparison. "), sprintf("The ANOVA assumptions hold, so %s is recommended. When normality or constant variance fails, this tab is marked Recommended.", design_model_name(design))),
        div(class = "split wide",
            section(np, "", r$names$treatment),
            div(class = "col",
                if (identical(np$main$dist, "chisq")) kcard(div(class = "hrow", h3(class = "ct", sprintf("Rejection region, χ²(%d)", np$main$df)), plot_card_tools("pl_chisq")), plotOutput(ns("pl_chisq"), height = "220px")),
                if (!is.null(np$rank_grid)) kcard(h3(class = "ct", sprintf("Ranks within each %s", r$names$block)),
                  tags$table(class = "kt", style = "margin-top:10px", tags$thead(tags$tr(tags$th(""), lapply(colnames(np$rank_grid), function(c) tags$th(class = "n", c)))),
                             tags$tbody(lapply(rownames(np$rank_grid), function(b) tags$tr(tags$td(tags$b(b)), lapply(colnames(np$rank_grid), function(c) tags$td(class = "n", fmt_g(np$rank_grid[b, c], 1)))))))),
                kcard(h3(class = "ct", sprintf("ANOVA vs %s", if (design$key %in% c("LSD", "GLSD")) "rank test" else np$main$name)),
                      tags$table(class = "kt", style = "margin-top:10px",
                                 tags$thead(tags$tr(tags$th(""), tags$th(class = "n", "ANOVA"), tags$th(class = "n", "Non-parametric"))),
                                 tags$tbody(tags$tr(tags$td("Statistic"), tags$td(class = "n", sprintf("F = %.2f", tr$F)), tags$td(class = "n", sprintf("%s = %.2f", np$main$stat_name, np$main$stat))),
                                            tags$tr(tags$td("p-value"), tags$td(class = "n", fmt_p(tr$p)), tags$td(class = "n", fmt_p(np$main$p))),
                                            tags$tr(tags$td("Decision"), tags$td(class = "n", if (tr$p < a) "Reject" else "Do not reject"), tags$td(class = "n", if (np$main$p < a) "Reject" else "Do not reject")))),
                      p(class = "small", style = "margin:10px 0 0", if ((tr$p < a) == (np$main$p < a)) "Both agree." else "They disagree. Trust the test whose assumptions hold; see the Assumptions tab.",
                        if (np$main$p > tr$p) " Rank tests use less information, so with few observations their p-values are larger." else "")),
                if (!is.null(np$alt)) kcard(h3(class = "ct", "Also available"),
                  div(class = "kv-row", span(tags$b(np$alt$name), br(), span(class = "small", np$alt$about)), span(sprintf("%s = %.2f, %s", np$alt$stat_name, np$alt$stat, p_txt(np$alt$p))))))),
        if (!is.null(np$greek)) tagList(h2(class = "ct", style = "margin-top:6px", sprintf("Greek factor: %s", r$names$greek)), section(np$greek, "_g", r$names$greek))
      )
    })
    np_tab_out <- function(idx, getx) {
      output[[paste0("np_ph_tab", idx)]] <- renderUI({
        r <- res(); req(r); x <- getx(r); req(x)
        k <- as.integer(input[[paste0("np_ph", idx)]] %||% "1"); ph <- x$posthoc[[min(k, length(x$posthoc))]]
        np_ph_table_ui(ph$table, r$alpha, ph$diff_name)
      })
    }
    np_tab_out("", function(r) r$nonpar)
    np_tab_out("_g", function(r) r$nonpar$greek)

    # ---------- R code ----------
    code_txt <- reactive({ r <- res(); req(r); d <- ds(); make_r_code(r, d$name, d$file, d$sheet, dat()) })
    output$t_code <- renderUI({
      txt <- code_txt()
      div(class = "split wide",
          kcard(div(class = "hrow", ico_box("code", "ip"), div(h2(class = "ct", sprintf("analysis_%s.R", tolower(design$key))), div(class = "small", "Reproduces every number on the results tabs")),
                    div(class = "ml hrow",
                        tags$button(class = "btn-k bs bsm", onclick = sprintf("navigator.clipboard.writeText(document.getElementById('%s').innerText); this.lastChild.textContent=' Copied'", ns("code_pre")), icon("copy", 14), span(" Copy")),
                        downloadButton(ns("dl_code"), label = tagList(icon("download", 14), "Download .R"), icon = NULL, class = "btn-k bp bsm"))),
                tags$pre(id = ns("code_pre"), class = "mono", style = "margin-top:14px;background:var(--surface2);border:1px solid var(--line);border-radius:10px;padding:16px;max-height:640px;overflow:auto;white-space:pre;color:var(--text)", txt)),
          div(class = "col",
              kcard(h3(class = "ct", "Why show the code?"),
                    p(class = "small", style = "margin:6px 0 0", "Your instructor can rerun the analysis in R and get the same numbers. The script uses your names, labels and order, and the same tests as the course scripts."),
                    p(class = "small", style = "margin:6px 0 0", "Fisher LSD and Scheffé are computed directly from the ANOVA error, which gives the same results as agricolae's LSD.test and scheffe.test.")),
              kcard(h3(class = "ct", "Session info"),
                    div(class = "kv-row", span("R"), span(as.character(getRversion()))),
                    lapply(c("car", "lmtest", "nortest", "multcomp", if (design$key %in% c("LSD", "GLSD")) "coin"), function(p)
                      div(class = "kv-row", span(p), span(as.character(utils::packageVersion(p))))),
                    div(class = "kv-row", span("Random seed"), span(format(res()$settings$seed %||% 1))))))
    })
    output$dl_code <- downloadHandler(filename = function() sprintf("analysis_%s.R", tolower(design$key)),
                                      content = function(file) writeLines(code_txt(), file, useBytes = TRUE))
    output$dl_tables <- downloadHandler(filename = function() sprintf("results_%s_%s.xlsx", tolower(design$key), format(Sys.Date())),
                                        content = function(file) writexl::write_xlsx(results_tables(res()), file))
    observeEvent(input$back3, go(3))

    pin_observers(input, session, store, items = reactive({ r <- res(); req(r); doe_pin_items(r, ph_method(), letter_method()) }),
      src = reactive({
        r <- res(); s <- r$settings; d <- r$data; dd <- ds()
        list(key = tolower(design$key), label = sprintf("%s \u00b7 %s", design$short, s$response_name %||% r$map$response),
             title = s$title %||% design$name,
             subtitle = sprintf("%s \u00b7 %d %s levels%s \u00b7 n = %d \u00b7 \u03b1 = %s", design$name, nlevels(d$treatment), r$names$treatment,
                                if (design$key == "RCBD") sprintf(" \u00d7 %d %s", nlevels(d$block), r$names$block) else "", nrow(d), format(r$alpha)),
             dataset = dd$name, notes = dd$notes, code = tryCatch(code_txt(), error = function(e) ""))
      }))

    # ---------- API for the app ----------
    list(
      ai = function(full) {
        d <- ds(); if (is.null(d)) return(NULL)
        r <- res()
        if (!is.null(r) && step() == 4)
          return(list(label = sprintf("your %s results (%s)", design$short, r$settings$response_name %||% r$map$response), kind = "results",
                      text = if (full) paste0(ctx_doe(r, tryCatch(code_txt(), error = function(e) "")), "\n\n", ctx_dataset(d))))
        list(label = sprintf("dataset %s and your %s set-up", d$name, design$short), kind = "setup",
             text = if (full) paste0(ctx_setup(design$name, step(), map(), tryCatch(check(), error = function(e) NULL)), "\n", ctx_dataset(d)))
      },
      use_dataset = function(id) { ds_id(id); map(setNames(as.list(rep("", length(rkeys))), rkeys)); go(2) },
      get_state = function() isolate(list(
        ds_id = ds_id(), step = step(), maxstep = maxstep(), map = map(), labels = lapply(labels(), function(L) { L$col <- attr(L, "col"); L }),
        control = control(),
        inputs = list(title = input$title, resp_name = input$resp_name, unit = input$unit,
                      disp = setNames(lapply(fkeys, function(k) input[[paste0("disp_", k)]]), fkeys),
                      alpha = input$alpha, alpha_custom = input$alpha_custom, conf = input$conf, posthoc = input$posthoc,
                      assump = input$assump, sstype = input$sstype, seed = input$seed, h0 = input$h0, h1 = input$h1))),
      set_state = function(s) {
        if (is.null(s$ds_id) || !(s$ds_id %in% names(isolate(store$datasets)))) return(invisible())
        m <- setNames(as.list(rep("", length(rkeys))), rkeys); for (k in intersect(names(s$map), rkeys)) m[[k]] <- s$map[[k]] %||% ""
        labs <- lapply(s$labels, function(L) { L <- as.data.frame(L, stringsAsFactors = FALSE); attr(L, "col") <- L$col[1]; L$col <- NULL; L })
        map(m); labels(labs); labels_rev(isolate(labels_rev()) + 1); control(s$control)
        ds_id(s$ds_id)
        i <- s$inputs
        for (nm in c("title", "resp_name", "unit")) if (!is.null(i[[nm]])) { updateTextInput(session, nm, value = i[[nm]]); auto[[nm]] <- NULL }
        for (k in names(i$disp)) if (!is.null(i$disp[[k]])) updateTextInput(session, paste0("disp_", k), value = i$disp[[k]])
        for (nm in c("h0", "h1")) if (!is.null(i[[nm]])) updateTextAreaInput(session, nm, value = i[[nm]])
        if (!is.null(i$alpha)) updateRadioButtons(session, "alpha", selected = i$alpha)
        if (!is.null(i$alpha_custom)) updateNumericInput(session, "alpha_custom", value = i$alpha_custom)
        if (!is.null(i$conf)) updateSelectInput(session, "conf", selected = i$conf)
        updateCheckboxGroupInput(session, "posthoc", selected = i$posthoc %||% character(0))
        if (!is.null(i$assump)) updateRadioButtons(session, "assump", selected = i$assump)
        if (!is.null(i$sstype)) updateRadioButtons(session, "sstype", selected = i$sstype)
        if (!is.null(i$seed)) updateNumericInput(session, "seed", value = i$seed)
        maxstep(max(1, min(3, s$maxstep %||% 1))); go(max(1, min(3, s$step %||% 1)))
      }
    )
  })
}

# Grid showing where each observation sits in the design.
layout_table <- function(design, d, m, labs) {
  y <- d[[m$response]]
  lab_of <- function(k, code) { L <- labs[[k]]; if (is.null(L)) code else ifelse(is.na(match(code, L$code)), code, L$label[match(code, L$code)]) }
  col_of <- function(k, code) { L <- labs[[k]]; if (is.null(L)) "#6B6F7B" else { c <- L$colour[match(code, L$code)]; ifelse(is.na(c), "#6B6F7B", c) } }
  ord <- function(k) { L <- labs[[k]]; if (is.null(L)) level_order(d[[m[[k]]]]) else L$code }
  cellv <- function(val, colr) span(class = "cellv", style = sprintf("background:%s22;color:var(--ink)", colr), val)
  tr <- as.character(d[[m$treatment]])
  if (design$key == "CRD") {
    lv <- ord("treatment")
    vals <- lapply(lv, function(l) y[tr == l])
    nr <- max(lengths(vals))
    return(tags$table(class = "kt lay",
      tags$thead(tags$tr(lapply(lv, function(l) tags$th(class = "n", lab_of("treatment", l))))),
      tags$tbody(lapply(seq_len(nr), function(i) tags$tr(lapply(seq_along(lv), function(j) {
        v <- vals[[j]][i]; tags$td(class = "n", if (!is.na(v)) cellv(v, col_of("treatment", lv[j])))
      }))))))
  }
  if (design$key == "RCBD") {
    lv <- ord("treatment"); bl <- ord("block"); b <- as.character(d[[m$block]])
    return(tags$table(class = "kt lay",
      tags$thead(tags$tr(tags$th(""), lapply(lv, function(l) tags$th(class = "n", lab_of("treatment", l))))),
      tags$tbody(lapply(bl, function(bb) tags$tr(tags$td(tags$b(lab_of("block", bb))), lapply(lv, function(l) {
        v <- y[b == bb & tr == l]
        tags$td(class = "n", if (length(v)) cellv(paste(v, collapse = ", "), col_of("treatment", l)) else span(class = "pill fail", "missing"))
      }))))))
  }
  rw <- as.character(d[[m$row]]); cl <- as.character(d[[m$column]])
  rl <- ord("row"); cc <- ord("column")
  gr <- if (design$key == "GLSD") as.character(d[[m$greek]]) else NULL
  tags$table(class = "kt lay",
    tags$thead(tags$tr(tags$th(""), lapply(cc, function(c) tags$th(style = "text-align:center", lab_of("column", c))))),
    tags$tbody(lapply(rl, function(r) tags$tr(tags$td(tags$b(lab_of("row", r))), lapply(cc, function(c) {
      i <- which(rw == r & cl == c)
      tags$td(style = "text-align:center",
              if (length(i)) div(cellv(lab_of("treatment", tr[i[1]]), col_of("treatment", tr[i[1]])),
                                 if (!is.null(gr)) div(class = "small", lab_of("greek", gr[i[1]])),
                                 div(class = "small", style = "font-variant-numeric:tabular-nums", y[i[1]]))
              else span(class = "pill fail", "empty"))
    })))))
}

how_text <- function(design) {
  switch(design$key,
    CRD = tagList(p("Every experimental unit is assigned to a treatment completely at random. Use it when units are alike, such as plots on uniform land or identical samples in a lab."),
                  p(class = "mono", "Response ~ Treatment"),
                  p("If normality or constant variance fails, the Kruskal-Wallis test compares the treatments using ranks.")),
    RCBD = tagList(p("Units are grouped into blocks that are alike (farms, days, batches). Every treatment appears once in every block, and the order is randomised inside each block. Blocking removes the block-to-block variation from the error."),
                   p(class = "mono", "Response ~ Treatment + Block"),
                   p("If normality or constant variance fails, the Friedman test ranks the treatments within each block.")),
    LSD = tagList(p("Two blocking factors, such as rows and columns of a field. Each treatment appears exactly once in every row and every column, so the number of rows, columns and treatments is the same."),
                  p(class = "mono", "Response ~ Treatment + Row + Column"),
                  p("A 3 × 3 square leaves only 2 error degrees of freedom, so tests have little power.")),
    GLSD = tagList(p("A Latin square with a second treatment factor laid over it using Greek letters. Each Greek letter appears once per row and column, and each Latin–Greek pair appears exactly once."),
                   p(class = "mono", "Response ~ Latin + Greek + Row + Column"),
                   p("Error degrees of freedom are (p − 1)(p − 3), so a 3 × 3 square has none and needs p ≥ 4.")))
}
