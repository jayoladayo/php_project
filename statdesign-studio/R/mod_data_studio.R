# Data Studio: upload files, pick sheets, type or paste data, check data health.

data_studio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Data Studio", "Bring your data in once and use it in any analysis.",
              tags$button(class = "btn-k bs bsm", onclick = sprintf("Shiny.setInputValue('%s', 'examples', {priority: 'event'})", ns("tab")),
                          icon("flask", 15), "Load a class example")),
    uiOutput(ns("tabbar")),
    tabsetPanel(
      id = ns("tabs"), type = "hidden",
      tabPanelBody("upload", upload_tab_ui(ns)),
      tabPanelBody("edit", edit_tab_ui(ns)),
      tabPanelBody("paste", paste_tab_ui(ns)),
      tabPanelBody("examples", examples_tab_ui(ns)),
      tabPanelBody("open", open_tab_ui(ns))
    )
  )
}

DS_TABS <- list(
  list(key = "upload", label = "Upload file", icon = "upload"),
  list(key = "edit", label = "Type & edit data", icon = "edit"),
  list(key = "paste", label = "Paste from Excel", icon = "copy"),
  list(key = "examples", label = "Class examples", icon = "flask"),
  list(key = "open", label = "Open project", icon = "save")
)

upload_tab_ui <- function(ns) {
  div(class = "split", style = "margin-top:20px",
      div(class = "col",
          div(class = "dropzone", id = ns("dz"),
              ico_box("upload", "ig", 20),
              div(class = "dz-txt",
                  div(class = "dz-t", "Drop an Excel or CSV file here, or browse"),
                  div(class = "small", ".xlsx, .xls, .csv, .txt · up to 50 MB · delimiter and decimal mark detected automatically")),
              fileInput(ns("file"), NULL, accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"),
                        buttonLabel = "Browse files", placeholder = NULL)),
          uiOutput(ns("fileinfo")),
          uiOutput(ns("sheetpick"))),
      div(class = "col",
          uiOutput(ns("preview")),
          uiOutput(ns("upload_actions"))))
}

edit_tab_ui <- function(ns) {
  div(style = "margin-top:20px",
      uiOutput(ns("edit_body")))
}

paste_tab_ui <- function(ns) {
  div(class = "split", style = "margin-top:20px",
      kcard(div(class = "col", style = "gap:14px",
                div(h2(class = "ct", "Paste cells from Excel or Google Sheets"),
                    p(class = "small", "Select the block in your spreadsheet including the header row, copy it, and paste it below.")),
                textAreaInput(ns("paste_txt"), NULL, rows = 12, width = "100%",
                              placeholder = "Farm\tFertilizer\tYield\n1\t1\t50\n1\t2\t48"),
                checkboxInput(ns("paste_header"), "The first row contains column names", TRUE))),
      div(class = "col", uiOutput(ns("paste_preview"))))
}

examples_tab_ui <- function(ns) {
  div(style = "margin-top:20px", class = "col",
      note("These datasets come from the course R scripts. Add one to your data, or open it straight in its analysis."),
      div(class = "grid g3",
          lapply(names(CLASS_EXAMPLES), function(k) {
            ex <- CLASS_EXAMPLES[[k]]
            d <- DESIGNS[[ex$design]]
            kcard(div(class = "col", style = "gap:10px;height:100%",
                      div(class = "hrow", ico_box(d$icon, "ig"), pill(d$short, "inf")),
                      h3(class = "ct", ex$title),
                      p(class = "small", style = "margin:0", ex$desc),
                      div(class = "small", sprintf("%d rows · %d columns", nrow(ex$data), ncol(ex$data))),
                      div(class = "hrow", style = "margin-top:auto",
                          tags$button(class = "btn-k bp bsm",
                                      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("ex_open"), k),
                                      icon("play", 13), sprintf("Open in %s", d$short)),
                          tags$button(class = "btn-k bs bsm",
                                      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("ex_add"), k),
                                      icon("plus", 13), "Add to my data"))))
          })))
}

open_tab_ui <- function(ns) {
  div(class = "split", style = "margin-top:20px",
      kcard(div(class = "col", style = "gap:14px",
                h2(class = "ct", "Open a saved project"),
                p(class = "small", style = "margin:0", "Projects are saved with the Save project button at the top. They hold your datasets, column types, names, labels and analysis settings."),
                div(class = "dropzone",
                    ico_box("save", "ig", 20),
                    div(class = "dz-txt", div(class = "dz-t", "Choose a .sdproj file"),
                        div(class = "small", "Opening a project adds its datasets to the ones you already have.")),
                    fileInput(ns("proj_file"), NULL, accept = c(".sdproj", ".json"), buttonLabel = "Browse files", placeholder = NULL)))),
      kcard(div(class = "col", style = "gap:10px",
                h3(class = "ct", "Your datasets in this session"),
                uiOutput(ns("ds_list")))))
}

data_studio_server <- function(id, store, goto, open_project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("upload")
    observeEvent(input$tab, {
      tab(input$tab)
      updateTabsetPanel(session, "tabs", selected = input$tab)
    })
    observeEvent(store$ds_tab, { if (!is.null(store$ds_tab)) { tab(store$ds_tab); updateTabsetPanel(session, "tabs", selected = store$ds_tab); store$ds_tab <- NULL } })

    output$tabbar <- renderUI({
      div(class = "ktabs", lapply(DS_TABS, function(t)
        tags$button(class = paste("ktab", if (tab() == t$key) "on"),
                    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("tab"), t$key),
                    icon(t$icon, 15), t$label)))
    })

    add_dataset <- function(ds, open_edit = TRUE) {
      store$datasets[[ds$id]] <- ds
      store$active <- ds$id
      store$grid_rev <- (store$grid_rev %||% 0) + 1
      if (open_edit) { tab("edit"); updateTabsetPanel(session, "tabs", selected = "edit") }
      showNotification(sprintf("Added “%s” (%d rows, %d columns).", ds$name, nrow(ds$data), ncol(ds$data)), type = "message")
      ds$id
    }

    # ---------- Upload ----------
    up <- reactiveValues(path = NULL, name = NULL, ext = NULL, size = NULL, sheets = NULL)
    observeEvent(input$file, {
      f <- input$file
      ext <- tolower(tools::file_ext(f$name))
      up$path <- f$datapath; up$name <- f$name; up$ext <- ext; up$size <- f$size; up$sheets <- NULL
      if (ext %in% c("xlsx", "xls")) {
        sh <- tryCatch(sheet_summaries(f$datapath), error = function(e) { showNotification(paste("Could not read the workbook:", conditionMessage(e)), type = "error"); NULL })
        up$sheets <- sh
      }
    })
    read_opts <- reactive(list(header = !identical(input$header, "no"), skip = max(0, as.integer(input$skip %||% 0)),
                               na = unique(c("", input$na_codes %||% "NA"))))
    uploaded <- reactive({
      req(up$path)
      o <- read_opts()
      tryCatch({
        if (up$ext %in% c("xlsx", "xls")) {
          req(input$sheet)
          read_excel_sheet(up$path, input$sheet, o$header, o$skip, o$na)
        } else {
          read_text_table(up$path, o$header, o$skip, o$na)$data
        }
      }, error = function(e) structure(list(msg = conditionMessage(e)), class = "readerr"))
    })

    output$fileinfo <- renderUI({
      req(up$name)
      nsheet <- length(up$sheets)
      kcard(div(class = "fileinfo",
                ico_box("sheet", "ig"),
                div(style = "flex:1;min-width:0",
                    div(style = "font-weight:600;color:var(--ink);font-size:14px", up$name),
                    div(class = "small", sprintf("%s%s", format_bytes(up$size),
                                                 if (nsheet) sprintf(" · %d sheet%s found", nsheet, if (nsheet > 1) "s" else "") else ""))),
                if (inherits(uploaded(), "readerr")) pill("Could not read", "fail", "x") else pill("Uploaded", "pass", "check")))
    })

    output$sheetpick <- renderUI({
      req(up$path)
      sh <- up$sheets
      sel <- NULL
      if (length(sh)) {
        ready <- which(vapply(sh, `[[`, "", "status") == "Ready")
        sel <- isolate(input$sheet) %||% (if (length(ready)) sh[[ready[1]]]$name else sh[[1]]$name)
      }
      kcard(div(class = "col", style = "gap:16px",
                if (length(sh)) div(
                  h2(class = "ct", "Which sheet should we use?"),
                  p(class = "small", sprintf("We found %d sheet%s in this workbook. Pick the one that holds your measurements.", length(sh), if (length(sh) > 1) "s" else "")),
                  div(class = "optcards", radioButtons(ns("sheet"), NULL, selected = sel,
                    choiceNames = lapply(sh, function(s) tagList(
                      span(span(class = "oc-t", s$name),
                           span(class = "oc-s", sprintf("%d rows · %d columns", s$rows, s$cols)),
                           span(class = "oc-p", pill(s$status, switch(s$status, Ready = "pass", Empty = "neu", "warn"))))
                    )),
                    choiceValues = vapply(sh, `[[`, "", "name")))
                ) else div(h2(class = "ct", "Read options"), p(class = "small", "Adjust these if the preview does not look right.")),
                div(class = "grid g3",
                    selectInput(ns("header"), "Header row", c("Row 1 contains column names" = "yes", "No header row" = "no"), isolate(input$header) %||% "yes"),
                    numericInput(ns("skip"), "Skip rows at top", isolate(input$skip) %||% 0, min = 0, max = 100, step = 1),
                    selectizeInput(ns("na_codes"), "Missing value code", choices = c("NA", "-", ".", "N/A", "999", "-99"),
                                   selected = isolate(input$na_codes) %||% "NA", multiple = TRUE,
                                   options = list(create = TRUE, placeholder = "Blank cells are always missing")))))
    })

    output$preview <- renderUI({
      req(up$path)
      d <- uploaded()
      if (inherits(d, "readerr")) return(kcard(note(kind = "e", ic = "alert", d$msg)))
      if (!nrow(d)) return(kcard(note(kind = "w", ic = "alert", "This sheet has no data rows. Pick another sheet or change the read options.")))
      ty <- guess_types(d)
      coded <- names(ty)[ty == "category" & vapply(d, function(c) all(!is.na(suppressWarnings(as.numeric(c[!is.na(c)])))), TRUE)]
      tagList(
        kcard(div(class = "col", style = "gap:12px",
                  div(class = "hrow", h2(class = "ct", paste("Preview:", if (length(up$sheets)) input$sheet %||% up$name else up$name)),
                      span(class = "small ml", sprintf("first %d of %d rows", min(6, nrow(d)), nrow(d)))),
                  div(class = "tscroll", html_table(d, types = ty, max_rows = 6)))),
        if (length(coded)) note(sprintf("%s contain%s numbers. When you map %s as factors in an analysis, we treat %s as categories automatically.",
                                        paste(coded, collapse = " and "), if (length(coded) == 1) "s" else "",
                                        if (length(coded) == 1) "it" else "them", if (length(coded) == 1) "it" else "them"))
      )
    })

    output$upload_actions <- renderUI({
      req(up$path)
      d <- uploaded()
      if (inherits(d, "readerr") || !nrow(d)) return(NULL)
      base <- tools::file_path_sans_ext(up$name)
      nm <- if (length(up$sheets) > 1 && !is.null(input$sheet)) paste(base, "–", input$sheet) else base
      kcard(div(class = "col", style = "gap:14px",
                textInput(ns("up_name"), "Dataset name", nm),
                div(class = "hrow", style = "justify-content:flex-end",
                    btn(ns("up_cancel"), "Cancel"),
                    btn(ns("up_use"), if (length(up$sheets)) "Use this sheet" else "Use this file", "arrowr", "bp"))))
    })
    observeEvent(input$up_cancel, { up$path <- NULL; up$name <- NULL; up$sheets <- NULL })
    observeEvent(input$up_use, {
      d <- uploaded(); req(!inherits(d, "readerr"), nrow(d) > 0)
      ds <- new_dataset(trimws(input$up_name) %||% up$name, d, source = "Upload",
                        sheet = if (length(up$sheets)) input$sheet, file = up$name)
      add_dataset(ds)
    })

    # ---------- Paste ----------
    pasted <- reactive({
      txt <- input$paste_txt %||% ""
      if (!nzchar(trimws(txt))) return(NULL)
      tryCatch(parse_pasted(txt, isTRUE(input$paste_header)), error = function(e) structure(list(msg = conditionMessage(e)), class = "readerr"))
    })
    output$paste_preview <- renderUI({
      d <- pasted()
      if (is.null(d)) return(kcard(div(class = "empty", ico_box("copy", "ip", 24), h3(class = "ct", "Nothing pasted yet"),
                                        p(class = "small", "A preview of your table appears here."))))
      if (inherits(d, "readerr")) return(kcard(note(kind = "e", ic = "alert", d$msg)))
      kcard(div(class = "col", style = "gap:12px",
                div(class = "hrow", h2(class = "ct", "Preview"), span(class = "small ml", sprintf("%d rows · %d columns", nrow(d), ncol(d)))),
                div(class = "tscroll", html_table(d, types = guess_types(d), max_rows = 8)),
                textInput(ns("paste_name"), "Dataset name", "Pasted data"),
                div(class = "hrow", style = "justify-content:flex-end", btn(ns("paste_use"), "Use this data", "arrowr", "bp"))))
    })
    observeEvent(input$paste_use, {
      d <- pasted(); req(!is.null(d), !inherits(d, "readerr"))
      add_dataset(new_dataset(input$paste_name %||% "Pasted data", d, source = "Pasted"))
      updateTextAreaInput(session, "paste_txt", value = "")
    })

    # ---------- Examples ----------
    observeEvent(input$ex_add, add_dataset(example_dataset(input$ex_add)))
    observeEvent(input$ex_open, {
      k <- input$ex_open
      idd <- add_dataset(example_dataset(k), open_edit = FALSE)
      goto(list(page = DESIGNS[[CLASS_EXAMPLES[[k]]$design]]$page, dataset = idd, t = Sys.time()))
    })

    # ---------- Open project ----------
    observeEvent(input$proj_file, open_project(input$proj_file$datapath))
    output$ds_list <- renderUI({
      dss <- store$datasets
      if (!length(dss)) return(p(class = "small", "No datasets yet."))
      tagList(lapply(dss, function(d) div(class = "list-row", icon("table", 16),
                                           div(style = "flex:1;min-width:0", div(style = "font-weight:600", d$name),
                                               div(class = "small", sprintf("%d rows · %d columns · %s", nrow(d$data), ncol(d$data), d$source))))))
    })

    # ---------- Edit ----------
    cur <- reactive({ id <- store$active; if (is.null(id)) NULL else store$datasets[[id]] })
    set_cur <- function(ds, rerender = FALSE) {
      store$datasets[[ds$id]] <- ds
      if (rerender) store$grid_rev <- (store$grid_rev %||% 0) + 1
    }

    output$edit_body <- renderUI({
      dss <- store$datasets
      if (!length(dss)) {
        return(kcard(div(class = "empty", ico_box("edit", "ig", 24),
                         h2(class = "ct", "Start a new table"),
                         p(class = "sub", style = "max-width:480px", "Type your data straight into a grid. You can also upload a file, paste from Excel, or load a class example."),
                         div(class = "hrow", style = "justify-content:center",
                             btn(ns("new_blank"), "New blank table", "plus", "bp"),
                             tags$button(class = "btn-k bs", onclick = sprintf("Shiny.setInputValue('%s', 'examples', {priority: 'event'})", ns("tab")), icon("flask", 15), "Class examples")))))
      }
      choices <- setNames(names(dss), vapply(dss, `[[`, "", "name"))
      div(class = "split", style = "grid-template-columns:minmax(0,1fr) 360px",
          kcard(div(class = "col", style = "gap:16px",
                    div(class = "hrow", style = "align-items:flex-end",
                        div(style = "flex:1;min-width:200px", selectInput(ns("edit_ds"), "Dataset", choices, isolate(store$active))),
                        div(style = "flex:1;min-width:200px", textInput(ns("ds_name"), "Dataset name", isolate(cur()$name)))),
                    div(class = "hrow",
                        btn(ns("add_row"), "Add row", "plus", size = "bsm"),
                        btn(ns("add_col"), "Add column", "plus", size = "bsm"),
                        btn(ns("edit_col"), "Rename or delete column", "edit", size = "bsm"),
                        btn(ns("new_blank"), "New blank table", "file", size = "bsm"),
                        div(class = "ml hrow",
                            downloadButton(ns("dl_xlsx"), label = tagList(icon("download", 14), "Excel"), icon = NULL, class = "btn-k bs bsm"),
                            downloadButton(ns("dl_csv"), label = tagList(icon("download", 14), "CSV"), icon = NULL, class = "btn-k bs bsm"),
                            btn(ns("del_ds"), "Remove", "x", "bdel", "bsm"))),
                    div(class = "gridbox", rhandsontable::rHandsontableOutput(ns("grid"))),
                    p(class = "small", style = "margin:0", "Tip: copy a block of cells in Excel and press Ctrl V in the grid. Right-click the grid to insert or remove rows."))),
          div(class = "col", uiOutput(ns("health")), uiOutput(ns("types")), uiOutput(ns("notes"))))
    })

    observeEvent(input$edit_ds, {
      if (!identical(input$edit_ds, store$active) && input$edit_ds %in% names(store$datasets)) {
        store$active <- input$edit_ds
        store$grid_rev <- (store$grid_rev %||% 0) + 1
        updateTextInput(session, "ds_name", value = store$datasets[[input$edit_ds]]$name)
      }
    })
    observeEvent(store$active, {
      if (!identical(input$edit_ds, store$active)) {
        updateSelectInput(session, "edit_ds", choices = setNames(names(store$datasets), vapply(store$datasets, `[[`, "", "name")), selected = store$active)
        updateTextInput(session, "ds_name", value = cur()$name)
      }
    })
    observeEvent(input$ds_name, {
      ds <- cur(); req(ds, nzchar(trimws(input$ds_name)))
      if (!identical(ds$name, input$ds_name)) { ds$name <- input$ds_name; set_cur(ds) }
    }, ignoreInit = TRUE)

    output$grid <- rhandsontable::renderRHandsontable({
      store$grid_rev
      ds <- isolate(cur()); req(ds)
      d <- ds$data
      h <- min(520, 60 + 31 * max(nrow(d), 8))
      rhandsontable::rhandsontable(d, useTypes = FALSE, stretchH = "all", height = h, rowHeaders = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    observeEvent(input$grid, {
      ds <- cur(); req(ds)
      df <- tryCatch(rhandsontable::hot_to_r(input$grid), error = function(e) NULL)
      req(!is.null(df), identical(names(df), names(ds$data)))
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      df[] <- lapply(df, function(c) { c <- as.character(c); c[!is.na(c) & trimws(c) == ""] <- NA; c })
      rownames(df) <- NULL
      if (!identical(df, ds$data)) { ds$data <- df; set_cur(ds) }
    })

    observeEvent(input$add_row, {
      ds <- cur(); req(ds)
      blank <- as.data.frame(setNames(rep(list(NA_character_), ncol(ds$data)), names(ds$data)), check.names = FALSE)
      ds$data <- rbind(ds$data, blank[rep(1, 5), , drop = FALSE]); rownames(ds$data) <- NULL
      set_cur(ds, TRUE)
    })
    observeEvent(input$add_col, {
      showModal(modalDialog(title = "Add a column", easyClose = TRUE,
                            textInput(ns("new_col_name"), "Column name", ""),
                            selectInput(ns("new_col_type"), "Type", TYPE_CHOICES, "number"),
                            footer = tagList(modalButton("Cancel"), btn(ns("add_col_ok"), "Add column", "plus", "bp"))))
    })
    observeEvent(input$add_col_ok, {
      ds <- cur(); req(ds)
      nm <- clean_names(c(names(ds$data), trimws(input$new_col_name)))[ncol(ds$data) + 1]
      ds$data[[nm]] <- NA_character_
      ds$types[[nm]] <- input$new_col_type
      removeModal(); set_cur(ds, TRUE)
    })
    observeEvent(input$edit_col, {
      ds <- cur(); req(ds)
      showModal(modalDialog(title = "Rename or delete a column", easyClose = TRUE,
                            selectInput(ns("ec_col"), "Column", names(ds$data)),
                            textInput(ns("ec_name"), "New name", names(ds$data)[1]),
                            footer = tagList(btn(ns("ec_del"), "Delete column", "x", "bdel"), modalButton("Cancel"),
                                             btn(ns("ec_ok"), "Rename", "check", "bp"))))
    })
    observeEvent(input$ec_col, updateTextInput(session, "ec_name", value = input$ec_col))
    observeEvent(input$ec_ok, {
      ds <- cur(); req(ds, input$ec_col %in% names(ds$data), nzchar(trimws(input$ec_name)))
      i <- match(input$ec_col, names(ds$data))
      nn <- names(ds$data); nn[i] <- trimws(input$ec_name); nn <- clean_names(nn)
      names(ds$data) <- nn
      names(ds$types)[match(input$ec_col, names(ds$types))] <- nn[i]
      ds$notes <- c(ds$notes, sprintf("Renamed column %s to %s.", input$ec_col, nn[i]))
      removeModal(); set_cur(ds, TRUE)
    })
    observeEvent(input$ec_del, {
      ds <- cur(); req(ds, input$ec_col %in% names(ds$data), ncol(ds$data) > 1)
      ds$data[[input$ec_col]] <- NULL; ds$types <- ds$types[names(ds$types) != input$ec_col]
      ds$notes <- c(ds$notes, sprintf("Deleted column %s.", input$ec_col))
      removeModal(); set_cur(ds, TRUE)
    })
    observeEvent(input$new_blank, {
      showModal(modalDialog(title = "New blank table", easyClose = TRUE,
                            textInput(ns("nb_name"), "Dataset name", "My experiment"),
                            textInput(ns("nb_cols"), "Column names, separated by commas", "Treatment, Block, Response"),
                            numericInput(ns("nb_rows"), "Number of rows", 12, min = 1, max = 1000),
                            footer = tagList(modalButton("Cancel"), btn(ns("nb_ok"), "Create table", "plus", "bp"))))
    })
    observeEvent(input$nb_ok, {
      cols <- clean_names(strsplit(input$nb_cols, ",")[[1]])
      n <- max(1, min(1000, as.integer(input$nb_rows %||% 12)))
      df <- as.data.frame(setNames(rep(list(rep(NA_character_, n)), length(cols)), cols), check.names = FALSE, stringsAsFactors = FALSE)
      ds <- new_dataset(input$nb_name %||% "My experiment", df, source = "Typed")
      ds$types <- setNames(ifelse(seq_along(cols) == length(cols), "number", "category"), cols)
      removeModal(); add_dataset(ds)
    })
    observeEvent(input$del_ds, {
      ds <- cur(); req(ds)
      showModal(modalDialog(title = "Remove this dataset?", easyClose = TRUE,
                            p(sprintf("“%s” will be removed from this session. Analyses that use it will need another dataset.", ds$name)),
                            footer = tagList(modalButton("Cancel"), btn(ns("del_ok"), "Remove", "x", "bdel"))))
    })
    observeEvent(input$del_ok, {
      id <- store$active; req(id)
      dss <- store$datasets; dss[[id]] <- NULL; store$datasets <- dss
      store$active <- if (length(dss)) names(dss)[length(dss)] else NULL
      store$grid_rev <- (store$grid_rev %||% 0) + 1
      removeModal()
    })

    output$dl_xlsx <- downloadHandler(
      filename = function() paste0(safe_file(cur()$name), ".xlsx"),
      content = function(file) writexl::write_xlsx(typed_data(cur()), file))
    output$dl_csv <- downloadHandler(
      filename = function() paste0(safe_file(cur()$name), ".csv"),
      content = function(file) utils::write.csv(typed_data(cur()), file, row.names = FALSE, na = ""))

    # Health
    health <- reactive({
      ds <- cur(); req(ds)
      d <- ds$data
      before <- nrow(d)
      d2 <- drop_empty_rows(d)
      h <- data_health(d2, ds$types, before - nrow(d2))
      ign <- ds$ignored %||% character(0)
      lapply(h, function(x) { if (!is.null(x$fix) && x$text %in% ign) x$fix <- NULL; x })
    })
    output$health <- renderUI({
      h <- health()
      fixes <- Filter(function(x) !is.null(x$fix), h)
      badge <- function(level) switch(level,
        ok = div(class = "hb pass", icon("check", 12, 3)), warn = div(class = "hb warn", icon("alert", 12, 2.5)),
        error = div(class = "hb fail", icon("x", 12, 2.5)), div(class = "hb inf", icon("info", 12, 2.5)))
      kcard(div(class = "hrow", h2(class = "ct", "Data health"),
                div(class = "ml", if (length(fixes)) pill(sprintf("%d fix%s suggested", length(fixes), if (length(fixes) > 1) "es" else ""), "gold")
                    else pill("Looks good", "pass", "check"))),
            div(style = "margin-top:6px", lapply(h, function(x) div(class = "hl", badge(x$level), div(x$text)))),
            if (length(fixes)) div(class = "fixbox",
                                   div(style = "display:flex;gap:8px", icon("wand", 16), div(tags$b("Suggested fix: "), fixes[[1]]$fix$label,
                                                                                             " We will record this change in your data notes.")),
                                   div(class = "hrow", style = "margin-top:10px",
                                       btn(ns("fix_apply"), "Apply fix", kind = "bgld", size = "bsm"),
                                       btn(ns("fix_ignore"), "Ignore", size = "bsm"))))
    })
    observeEvent(input$fix_apply, {
      ds <- cur(); h <- health(); fixes <- Filter(function(x) !is.null(x$fix), h); req(length(fixes))
      f <- fixes[[1]]
      ds$data <- apply_fix(ds$data, f$fix)
      ds$notes <- c(ds$notes, sprintf("%s Fixed: %s", format(Sys.time(), "%H:%M"), f$text))
      set_cur(ds, TRUE)
    })
    observeEvent(input$fix_ignore, {
      ds <- cur(); h <- health(); fixes <- Filter(function(x) !is.null(x$fix), h); req(length(fixes))
      ds$ignored <- c(ds$ignored, fixes[[1]]$text); set_cur(ds)
    })

    # Column types
    col_key <- reactiveVal(NULL)
    observe({ ds <- cur(); col_key(if (is.null(ds)) NULL else paste(ds$id, paste(names(ds$data), collapse = "\r"))) })
    output$types <- renderUI({
      col_key(); ds <- isolate(cur()); req(ds)
      kcard(h2(class = "ct", "Column types"),
            p(class = "small", "Numbers are measurements. Categories are groups, even when they are stored as codes like 1, 2, 3."),
            lapply(seq_along(ds$data), function(j) {
              nm <- names(ds$data)[j]
              div(class = "kv-row", style = "align-items:center", span(nm),
                  div(style = "width:150px", selectInput(ns(paste0("type_", j)), NULL, TYPE_CHOICES, ds$types[[nm]] %||% "category")))
            }))
    })
    observe({
      ds <- cur(); req(ds)
      nms <- names(ds$data)
      vals <- lapply(seq_along(nms), function(j) input[[paste0("type_", j)]])
      if (any(vapply(vals, is.null, TRUE))) return()
      newt <- setNames(unlist(vals), nms)
      old <- ds$types[nms]
      if (!identical(unname(unlist(old)), unname(newt))) {
        isolate({ ds <- cur(); ds$types <- newt; set_cur(ds) })
      }
    })

    output$notes <- renderUI({
      ds <- cur(); req(ds)
      kcard(h3(class = "ct", "About this dataset"),
            div(class = "kv-row", span("Source"), span(paste(c(ds$source, ds$file, ds$sheet), collapse = " · "))),
            div(class = "kv-row", span("Size"), span(sprintf("%d rows · %d columns", nrow(drop_empty_rows(ds$data)), ncol(ds$data)))),
            if (length(ds$notes)) div(style = "margin-top:10px", div(class = "klab", "Change log"),
                                      tags$ul(class = "small", style = "padding-left:18px;margin:0", lapply(ds$notes, tags$li))))
    })
  })
}

format_bytes <- function(b) {
  if (is.null(b) || is.na(b)) return("")
  if (b < 1024) paste(b, "B") else if (b < 1024^2) sprintf("%.0f KB", b / 1024) else sprintf("%.1f MB", b / 1024^2)
}
safe_file <- function(x) gsub("[^A-Za-z0-9_ -]", "", x %||% "data")
