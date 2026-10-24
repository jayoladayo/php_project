# Report builder: pinned items, the live preview and the Word, PDF, HTML and Excel exports.

REPORT_TEMPLATES <- c("Lab report" = "lab", "Thesis" = "thesis", "Brief" = "brief")
REPORT_FORMATS <- list(docx = c("Word", "docx"), pdf = c("PDF", "pdf"), html = c("HTML", "html"), xlsx = c("Excel (all tables)", "xlsx"))

# ---------- Building pins ----------

# A pin is a snapshot: its numbers and picture stay the same even if the analysis is run again.
mk_pin <- function(kind, title, icon = NULL, df = NULL, plot = NULL, text = NULL, h = 3.4) {
  pin <- list(kind = kind, title = title, icon = icon %||% switch(kind, table = "table", plot = "bar", text = "file", heading = "tag", "edit"),
              df = df, text = text, h = h)
  if (!is.null(plot)) {
    f <- tempfile(fileext = ".png")
    ggplot2::ggsave(f, plot + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA)),
                    width = 7, height = h, dpi = 170, bg = "white")
    pin$png <- readBin(f, "raw", file.info(f)$size); unlink(f)
  }
  if (!is.null(df)) {
    df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
    pin$num <- vapply(df, function(x) { x <- gsub("[<,%−—\\[\\] ]", "", x); x <- x[nzchar(x)]; length(x) > 0 && all(!is.na(suppressWarnings(as.numeric(x)))) }, TRUE)
    pin$df <- df
  }
  pin
}

# Format any results table for a report: numbers rounded, p-values in the usual style.
fmt_df <- function(df) {
  out <- df
  for (n in names(df)) {
    x <- df[[n]]
    if (is.logical(x)) out[[n]] <- ifelse(is.na(x), "", ifelse(x, "Yes", "No"))
    else if (is.numeric(x)) out[[n]] <- if (tolower(n) %in% c("p", "p_holm", "p-value", "p_adj")) fmt_p(x) else if (all(is.na(x) | x == round(x))) ifelse(is.na(x), "", format(x)) else fmt_g(x, 3)
  }
  out
}

# What a CRD, RCBD, Latin or Graeco-Latin result can add to the report.
doe_pin_items <- function(r, ph_m, letter_m) {
  an <- r$anova; tab <- an$table; ph <- r$posthoc$treatment; asm <- r$assumptions
  trt <- r$names$treatment
  it <- list(
    anova = list(label = "ANOVA table", default = TRUE, build = function() mk_pin("table", "ANOVA table", "table",
      df = data.frame(Source = c(tab$source, "Residuals", "Total"), df = c(tab$df, an$dfe, an$dft),
                      `Sum of squares` = fmt_num(c(tab$ss, an$sse, an$sst)), `Mean square` = c(fmt_num(tab$ms), fmt_num(an$mse), ""),
                      F = c(fmt_g(tab$F), "", ""), `p-value` = c(fmt_p(tab$p), "", ""), check.names = FALSE))),
    means_plot = list(label = "Mean plot with letters", default = TRUE, build = function()
      mk_pin("plot", "Mean plot with letters", "bar", plot = plot_means(r, "treatment", letter_m), h = 3.6)),
    assump = list(label = "Assumption checks", default = TRUE, build = function() {
      parts <- list(Normality = asm$normal, `Constant variance` = asm$variance, Independence = asm$indep)
      st <- c(Normality = "normality", `Constant variance` = "variance", Independence = "independence")
      df <- do.call(rbind, lapply(names(parts), function(k) { x <- parts[[k]]
        data.frame(Check = c(k, rep("", nrow(x) - 1)), Test = x$test,
                   Statistic = ifelse(is.na(x$value), "—", sprintf("%s = %s", x$statistic, fmt_g(x$value, 3))), `p-value` = fmt_p(x$p),
                   Result = c(c(pass = "Pass", borderline = "Borderline", fail = "Fail", na = "Not available")[[asm$status[[st[[k]]]]]], rep("", nrow(x) - 1)),
                   check.names = FALSE) }))
      mk_pin("table", "Assumption checks", "scale", df = df) }),
    posthoc = list(label = sprintf("%s table", POSTHOC_NAMES[[ph_m]]), default = TRUE, build = function() {
      x <- ph$methods[[ph_m]]
      mk_pin("table", sprintf("%s table", POSTHOC_NAMES[[ph_m]]), "columns",
             df = data.frame(Comparison = x$pair, Difference = fmt_g(x$diff), CI = sprintf("[%s, %s]", fmt_g(x$lwr), fmt_g(x$upr)),
                             `Adjusted p` = fmt_p(x$p), Result = ifelse(x$p < r$alpha, "Different", "Not different"), check.names = FALSE)) }),
    means = list(label = sprintf("Means by %s with letters", trt), default = FALSE, build = function() {
      m <- ph$means; lt <- ph$letters[[letter_m]]
      mk_pin("table", sprintf("Means by %s", trt), "table",
             df = setNames(data.frame(m$level, m$n, fmt_g(m$mean), fmt_g(m$sd), sprintf("[%s, %s]", fmt_g(m$lwr), fmt_g(m$upr)), unname(lt[as.character(m$level)] %||% "")),
                           c(trt, "n", "Mean", "SD", sprintf("%g%% CI", 100 * r$conf), "Group"))) }),
    box = list(label = "Box plot", default = FALSE, build = function() mk_pin("plot", sprintf("Box plot by %s", trt), "bar", plot = plot_box(r, "treatment"), h = 3.4)),
    qq = list(label = "Normal QQ plot of residuals", default = FALSE, build = function() mk_pin("plot", "Normal QQ plot of residuals", "trend", plot = plot_qq(r), h = 3)),
    rvf = list(label = "Residuals vs fitted", default = FALSE, build = function() mk_pin("plot", "Residuals vs fitted", "trend", plot = plot_rvf(r), h = 3)))
  if (!is.null(r$nonpar$main)) it$nonpar <- list(label = sprintf("Non-parametric test (%s)", r$nonpar$main$name), default = r$recommend == "nonpar",
    build = function() { m <- r$nonpar$main
      mk_pin("table", "Non-parametric test", "shuffle", df = data.frame(Test = m$name, Statistic = fmt_g(m$stat, 3), df = format(m$df %||% ""), `p-value` = fmt_p(m$p), check.names = FALSE)) })
  it$interp <- list(label = "Interpretation", default = TRUE, build = function() mk_pin("text", "Interpretation", "file", text = interpretation(r)))
  it
}

# ---------- Pin to report: button and modal shared by every analysis ----------

pin_button <- function(ns) btn(ns("pin"), "Pin to report", "pin", size = "bsm")

pin_observers <- function(input, session, store, items, src) {
  ns <- session$ns
  observeEvent(input$pin, {
    it <- tryCatch(items(), error = function(e) NULL)
    if (!length(it)) { showNotification("Run the analysis first.", type = "warning"); return() }
    showModal(modalDialog(
      title = "Pin to report", easyClose = TRUE,
      p(class = "small", "Choose what to add to your report. Pinned items keep these numbers even if you run the analysis again."),
      div(class = "pinpick", checkboxGroupInput(ns("pin_pick"), NULL, choices = setNames(names(it), vapply(it, `[[`, "", "label")),
                                                selected = names(it)[vapply(it, function(x) isTRUE(x$default), TRUE)])),
      footer = tagList(modalButton("Cancel"), btn(ns("pin_ok"), "Pin selected", "pin", "bp"))))
  })
  observeEvent(input$pin_ok, {
    it <- items(); pick <- intersect(input$pin_pick %||% character(0), names(it)); s <- src()
    if (!length(pick)) { removeModal(); return() }
    n <- 0
    withProgress(message = "Pinning to the report", value = 0.3, {
      for (k in pick) {
        pin <- tryCatch(it[[k]]$build(), error = function(e) NULL)
        if (is.null(pin)) next
        store$pin_seq <- (isolate(store$pin_seq) %||% 0) + 1
        pin$id <- paste0("pin", isolate(store$pin_seq)); pin$source <- s$label; pin$src <- s$key
        store$pins <- c(isolate(store$pins), list(pin)); n <- n + 1
      }
    })
    src_all <- isolate(store$report_src) %||% list(); src_all[[s$key]] <- s; store$report_src <- src_all
    removeModal()
    showNotification(sprintf("%d item%s pinned. Open Report to arrange and export.", n, if (n == 1) "" else "s"), type = "message")
  })
}

# ---------- Shared report content ----------

report_opts_default <- function(pins, srcs) {
  keys <- vapply(pins, function(p) p$src %||% "", "")
  keys <- keys[nzchar(keys) & keys %in% names(srcs)]
  first <- if (length(keys)) srcs[[keys[1]]] else NULL
  list(title = first$title %||% "Results report", subtitle = first$subtitle %||% "", author = "", template = "lab", appendix = TRUE)
}

# Number the items: section numbers for the lab report, table and figure numbers for the thesis.
report_numbering <- function(pins, template) {
  sec <- 0; tb <- 0; fg <- 0
  lapply(pins, function(p) {
    if (p$kind %in% c("heading", "note")) return(list(head = NULL, caption = NULL))
    if (template == "thesis") {
      if (p$kind == "table") { tb <<- tb + 1; return(list(caption = sprintf("Table %d. %s", tb, p$title))) }
      if (p$kind == "plot") { fg <<- fg + 1; return(list(caption = sprintf("Figure %d. %s", fg, p$title))) }
      return(list(head = p$title))
    }
    if (template == "brief") return(list(head = p$title))
    sec <<- sec + 1; list(head = sprintf("%d. %s", sec, p$title))
  })
}

report_sources_used <- function(pins, srcs) {
  keys <- unique(vapply(pins, function(p) p$src %||% "", ""))
  Filter(Negate(is.null), lapply(keys[nzchar(keys)], function(k) srcs[[k]]))
}

# The report as HTML; the same markup drives the preview and the HTML download.
report_html_body <- function(pins, srcs, o) {
  num <- report_numbering(pins, o$template)
  esc <- htmltools::htmlEscape
  img <- function(p) sprintf('<img alt="%s" src="data:image/png;base64,%s">', esc(p$title), base64enc::base64encode(p$png))
  table <- function(p) {
    df <- p$df
    paste0('<table class="rt"><thead><tr>', paste0(sprintf('<th%s>%s</th>', ifelse(p$num, ' class="n"', ""), esc(names(df))), collapse = ""), "</tr></thead><tbody>",
           paste0(vapply(seq_len(nrow(df)), function(i) paste0("<tr>", paste0(sprintf("<td%s>%s</td>", ifelse(p$num, ' class="n"', ""), esc(unlist(df[i, ]))), collapse = ""), "</tr>"), ""), collapse = ""),
           "</tbody></table>")
  }
  items <- vapply(seq_along(pins), function(i) {
    p <- pins[[i]]; n <- num[[i]]
    head <- if (!is.null(n$head)) sprintf("<h3>%s</h3>", esc(n$head)) else ""
    switch(p$kind,
      heading = sprintf("<h2>%s</h2>", esc(p$text)),
      note = sprintf('<p class="rnote">%s</p>', gsub("\n", "<br>", esc(p$text))),
      text = paste0(head, sprintf("<p>%s</p>", esc(p$text))),
      table = paste0(head, if (!is.null(n$caption)) sprintf('<p class="cap">%s</p>', esc(n$caption)) else "", table(p)),
      plot = paste0(head, sprintf('<figure class="%s">%s%s</figure>', if (o$template == "brief") "sm" else "", img(p),
                                  if (!is.null(n$caption)) sprintf("<figcaption>%s</figcaption>", esc(n$caption)) else "")),
      "")
  }, "")
  appendix <- ""
  if (isTRUE(o$appendix) && o$template != "brief") {
    used <- report_sources_used(pins, srcs)
    if (length(used)) appendix <- paste0('<div class="rapp"><h2>Appendix</h2>', paste0(vapply(seq_along(used), function(i) { s <- used[[i]]
      paste0(sprintf("<h3>%s. %s</h3>", LETTERS[i], esc(s$label)),
             sprintf('<p class="meta">Dataset: %s</p>', esc(s$dataset %||% "")),
             if (length(s$notes)) paste0("<p><b>Data notes</b></p><ul>", paste0(sprintf("<li>%s</li>", esc(s$notes)), collapse = ""), "</ul>") else "",
             if (nzchar(s$code %||% "")) sprintf("<p><b>R code</b></p><pre>%s</pre>", esc(s$code)) else "") }, ""), collapse = ""), "</div>")
  }
  kind <- names(REPORT_TEMPLATES)[REPORT_TEMPLATES == o$template]
  paste0('<div class="rdoc t-', o$template, '">',
         '<div class="rhead"><div class="rlogo"><svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="#00573F" stroke-width="2.6" stroke-linecap="round"><path d="M12 20V10"/><path d="M18 20V4"/><path d="M6 20v-4"/></svg></div>',
         sprintf("<div><div>StatDesign Studio · %s</div><div>%s%s</div></div></div>", esc(kind), format(Sys.Date(), "%e %B %Y"),
                 if (nzchar(o$author %||% "")) paste0(" · ", esc(o$author)) else ""),
         sprintf("<h1>%s</h1>", esc(o$title)), if (nzchar(o$subtitle %||% "")) sprintf('<p class="meta">%s</p>', esc(o$subtitle)) else "",
         paste0(items, collapse = "\n"), appendix, "</div>")
}

REPORT_CSS <- "
.rdoc{font-family:Poppins,'Segoe UI',Arial,sans-serif;color:#1F2321;font-size:13px;line-height:1.6}
.rdoc .rhead{display:flex;gap:12px;align-items:center;font-size:11px;color:#6B6F7B;padding-bottom:14px;border-bottom:2px solid #008540;margin-bottom:22px}
.rdoc .rlogo{width:36px;height:36px;border-radius:8px;background:#DAC961;display:flex;align-items:center;justify-content:center;flex-shrink:0}
.rdoc h1{color:#00573F;font-size:24px;line-height:1.25;margin:0 0 6px;font-weight:700}
.rdoc h2{color:#00573F;font-size:17px;margin:26px 0 8px}
.rdoc h3{font-size:14px;margin:22px 0 8px;font-weight:600}
.rdoc p{margin:0 0 10px}
.rdoc .meta{color:#6B6F7B;font-size:12px}
.rdoc .rnote{background:#F7F1D2;border-left:3px solid #AA8A00;padding:8px 12px;border-radius:4px}
.rdoc .rt{border-collapse:collapse;width:100%;font-size:12px;margin:4px 0 12px}
.rdoc .rt th{background:#EEF0F4;text-align:left;font-weight:600;padding:7px 10px;font-size:11px;text-transform:uppercase;letter-spacing:.03em;color:#4B4F5A}
.rdoc .rt td{padding:7px 10px;border-bottom:1px solid #E4E6EC}
.rdoc .rt .n{text-align:right;font-variant-numeric:tabular-nums}
.rdoc figure{margin:6px 0 14px}.rdoc figure img{max-width:100%;height:auto;display:block}
.rdoc figure.sm img{max-width:68%}
.rdoc .cap,.rdoc figcaption{font-size:12px;font-weight:600;color:#373938;margin:6px 0}
.rdoc pre{background:#F7F8FA;border:1px solid #E4E6EC;border-radius:6px;padding:10px;font-size:10.5px;white-space:pre-wrap;font-family:'Roboto Mono',Consolas,monospace}
.rdoc .rapp{margin-top:34px;border-top:1px solid #E4E6EC;padding-top:6px}
.rdoc.t-thesis{font-family:'Times New Roman',Georgia,serif;font-size:14px}
.rdoc.t-thesis h1,.rdoc.t-thesis h2{color:#1F2321}
.rdoc.t-thesis .rt th{background:none;border-top:1.5px solid #1F2321;border-bottom:1px solid #1F2321;text-transform:none;letter-spacing:0;font-size:12.5px;color:#1F2321}
.rdoc.t-thesis .rt td{border:none}.rdoc.t-thesis .rt tr:last-child td{border-bottom:1.5px solid #1F2321}
.rdoc.t-thesis figcaption{font-weight:400;font-style:italic;text-align:center}
.rdoc.t-brief{font-size:12.5px}.rdoc.t-brief h3{margin-top:14px}
"

report_html_doc <- function(pins, srcs, o) {
  paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
         sprintf("<title>%s</title>", htmltools::htmlEscape(o$title)),
         '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Poppins:wght@400;600;700&family=Roboto+Mono&display=swap">',
         "<style>body{margin:0;background:#F7F8FA}.page{max-width:820px;margin:32px auto;background:#fff;padding:48px 56px;box-shadow:0 1px 4px rgba(0,0,0,.08)}",
         "@media print{body{background:#fff}.page{margin:0;box-shadow:none;padding:0}}", REPORT_CSS, "</style></head><body><div class=\"page\">",
         report_html_body(pins, srcs, o), "</div></body></html>")
}

# ---------- Word ----------

report_docx <- function(pins, srcs, o, file) {
  fam <- if (o$template == "thesis") "Times New Roman" else "Calibri"
  green <- if (o$template == "thesis") "#1F2321" else "#00573F"
  tx <- function(size = 11, bold = FALSE, color = "#1F2321", italic = FALSE, f = fam) officer::fp_text(font.size = size, bold = bold, color = color, italic = italic, font.family = f)
  par <- function(doc, txt, prop = tx(), before = 0, after = 6, align = "left", keep = FALSE)
    officer::body_add_fpar(doc, officer::fpar(officer::ftext(txt, prop), fp_p = officer::fp_par(padding.top = before, padding.bottom = after, text.align = align, keep_with_next = keep)))
  num <- report_numbering(pins, o$template)
  kind <- names(REPORT_TEMPLATES)[REPORT_TEMPLATES == o$template]
  doc <- officer::read_docx()
  doc <- par(doc, sprintf("StatDesign Studio · %s · %s%s", kind, format(Sys.Date(), "%e %B %Y"), if (nzchar(o$author %||% "")) paste0(" · ", o$author) else ""), tx(9, color = "#6B6F7B"))
  doc <- par(doc, o$title, tx(20, TRUE, green), before = 6, after = 4)
  if (nzchar(o$subtitle %||% "")) doc <- par(doc, o$subtitle, tx(10, color = "#6B6F7B"), after = 12)
  for (i in seq_along(pins)) {
    p <- pins[[i]]; n <- num[[i]]
    if (!is.null(n$head)) doc <- par(doc, n$head, tx(if (o$template == "brief") 11.5 else 13, TRUE), before = 10, after = 4, keep = TRUE)
    if (p$kind == "heading") doc <- par(doc, p$text, tx(15, TRUE, green), before = 14, after = 4, keep = TRUE)
    if (p$kind %in% c("note", "text")) doc <- par(doc, p$text, tx(10.5, italic = p$kind == "note"), after = 8)
    if (p$kind == "table") {
      if (!is.null(n$caption)) doc <- par(doc, n$caption, tx(10, TRUE), before = 6, after = 2, keep = TRUE)
      ft <- flextable::flextable(p$df)
      ft <- if (o$template == "thesis") flextable::theme_booktabs(ft) else flextable::theme_vanilla(ft)
      ft <- flextable::font(ft, fontname = fam, part = "all")
      ft <- flextable::fontsize(ft, size = 9, part = "all")
      if (o$template != "thesis") ft <- flextable::bg(ft, bg = "#EEF0F4", part = "header")
      for (j in which(p$num)) ft <- flextable::align(ft, j = j, align = "right", part = "all")
      ft <- flextable::autofit(ft)
      ft <- flextable::fit_to_width(ft, 6.5)
      doc <- flextable::body_add_flextable(doc, ft, align = "left")
      doc <- par(doc, "", after = 4)
    }
    if (p$kind == "plot") {
      f <- tempfile(fileext = ".png"); writeBin(p$png, f)
      w <- if (o$template == "brief") 4.4 else 6.2
      doc <- officer::body_add_img(doc, src = f, width = w, height = w * p$h / 7)
      if (!is.null(n$caption)) doc <- par(doc, n$caption, tx(10, italic = TRUE), after = 8, align = "center")
    }
  }
  if (isTRUE(o$appendix) && o$template != "brief") {
    used <- report_sources_used(pins, srcs)
    if (length(used)) {
      doc <- officer::body_add_break(doc)
      doc <- par(doc, "Appendix", tx(15, TRUE, green), after = 6)
      for (i in seq_along(used)) { s <- used[[i]]
        doc <- par(doc, sprintf("%s. %s", LETTERS[i], s$label), tx(12, TRUE), before = 8, after = 2)
        doc <- par(doc, sprintf("Dataset: %s", s$dataset %||% ""), tx(9.5, color = "#6B6F7B"))
        if (length(s$notes)) { doc <- par(doc, "Data notes", tx(10.5, TRUE)); for (nt in s$notes) doc <- par(doc, paste("•", nt), tx(10), after = 2) }
        if (nzchar(s$code %||% "")) {
          doc <- par(doc, "R code", tx(10.5, TRUE), before = 6)
          for (ln in strsplit(s$code, "\n", fixed = TRUE)[[1]]) doc <- par(doc, if (nzchar(ln)) ln else " ", tx(8, f = "Courier New"), after = 0)
        }
      }
    }
  }
  # officer zips from the working directory and needs to read it back; on some network drives it can vanish.
  if (is.null(tryCatch(getwd(), error = function(e) NULL))) setwd(tempdir())
  print(doc, target = file)
}

# ---------- PDF, drawn with grid so it needs no LaTeX ----------

report_pdf <- function(pins, srcs, o, file) {
  serif <- o$template == "thesis"
  fam <- if (serif) "serif" else "sans"
  W <- 8.27; H <- 11.69; mg <- 0.85; cw <- W - 2 * mg
  green <- if (serif) "#1F2321" else "#00573F"
  # Open the device first so text is measured with the fonts it will be drawn in.
  grDevices::cairo_pdf(file, width = W, height = H, onefile = TRUE)
  on.exit(grDevices::dev.off())
  blocks <- list()
  keep <- FALSE
  add <- function(h, draw) { blocks[[length(blocks) + 1]] <<- list(h = h, draw = draw, keep = keep) }
  # Wrapped text: about 0.5 em per character.
  text_block <- function(txt, size = 10.5, bold = FALSE, col = "#1F2321", f = fam, after = 0.08, italic = FALSE, mono = FALSE, hjust = 0) {
    width <- floor(cw * 72 / (size * if (mono) 0.6 else 0.5))
    lines <- unlist(lapply(strsplit(txt, "\n", fixed = TRUE)[[1]], function(l) if (!nzchar(l)) "" else strwrap(l, width)))
    lh <- size * 1.38 / 72
    for (k in seq_along(lines)) local({ l <- lines[k]; last <- k == length(lines)
      add(lh + if (last) after else 0, function(y) grid::grid.text(l, x = grid::unit(if (hjust == 0.5) W / 2 else mg, "in"), y = grid::unit(y - lh * 0.82, "in"), just = c(if (hjust == 0.5) "centre" else "left", "bottom"),
                                                              gp = grid::gpar(fontsize = size, col = col, fontfamily = if (mono) "mono" else f, fontface = if (bold) "bold" else if (italic) "italic" else "plain"))) })
  }
  table_block <- function(p) {
    df <- p$df
    hj <- ifelse(p$num, 1, 0); xx <- ifelse(p$num, 0.95, 0.05)
    chunks <- split(seq_len(nrow(df)), ceiling(seq_len(nrow(df)) / 28))
    for (rows in chunks) local({
      nr <- length(rows)
      th <- gridExtra::ttheme_minimal(base_size = 8.5, base_family = fam,
        core = list(fg_params = list(hjust = rep(hj, each = nr), x = rep(xx, each = nr)), bg_params = list(fill = "white", col = "#E4E6EC", lwd = 0.5)),
        colhead = list(fg_params = list(hjust = hj, x = xx, fontface = "bold", col = "#373938"),
                       bg_params = list(fill = if (serif) "white" else "#EEF0F4", col = NA)))
      g <- gridExtra::tableGrob(df[rows, , drop = FALSE], rows = NULL, theme = th)
      wid <- sum(grid::convertWidth(g$widths, "in", valueOnly = TRUE))
      if (wid > cw) g$widths <- g$widths * (cw / wid)
      hh <- sum(grid::convertHeight(g$heights, "in", valueOnly = TRUE))
      add(hh + 0.18, function(y) { grid::pushViewport(grid::viewport(x = grid::unit(mg, "in"), y = grid::unit(y, "in"), width = grid::unit(min(wid, cw), "in"),
                                                                     height = grid::unit(hh, "in"), just = c("left", "top")))
        grid::grid.draw(g); grid::popViewport() })
    })
  }
  plot_block <- function(p) {
    w <- if (o$template == "brief") cw * 0.68 else cw; h <- w * p$h / 7
    img <- png::readPNG(p$png)
    add(h + 0.12, function(y) grid::grid.raster(img, x = grid::unit(mg, "in"), y = grid::unit(y, "in"), width = grid::unit(w, "in"), height = grid::unit(h, "in"), just = c("left", "top")))
  }
  kind <- names(REPORT_TEMPLATES)[REPORT_TEMPLATES == o$template]
  text_block(sprintf("StatDesign Studio · %s · %s%s", kind, format(Sys.Date(), "%e %B %Y"), if (nzchar(o$author %||% "")) paste0(" · ", o$author) else ""), 8.5, col = "#6B6F7B", after = 0.05)
  add(0.22, function(y) grid::grid.lines(grid::unit(c(mg, W - mg), "in"), grid::unit(c(y - 0.05, y - 0.05), "in"), gp = grid::gpar(col = "#008540", lwd = 2)))
  text_block(o$title, 19, TRUE, green, after = 0.04)
  if (nzchar(o$subtitle %||% "")) text_block(o$subtitle, 9.5, col = "#6B6F7B", after = 0.12)
  num <- report_numbering(pins, o$template)
  for (i in seq_along(pins)) {
    p <- pins[[i]]; n <- num[[i]]
    keep <- TRUE
    if (!is.null(n$head)) { add(0.1, function(y) NULL); text_block(n$head, if (o$template == "brief") 11 else 12.5, TRUE, after = 0.06) }
    if (p$kind == "table" && !is.null(n$caption)) text_block(n$caption, 10, TRUE, after = 0.04)
    if (p$kind == "heading") { add(0.14, function(y) NULL); text_block(p$text, 14, TRUE, green, after = 0.06) }
    keep <- FALSE
    switch(p$kind,
      heading = NULL,
      note = text_block(p$text, 10, italic = TRUE, after = 0.12),
      text = text_block(p$text, 10.5, after = 0.12),
      table = table_block(p),
      plot = { plot_block(p); if (!is.null(n$caption)) text_block(n$caption, 9.5, italic = TRUE, after = 0.12, hjust = 0.5) })
  }
  if (isTRUE(o$appendix) && o$template != "brief") {
    used <- report_sources_used(pins, srcs)
    if (length(used)) {
      add(-1, NULL)  # page break
      text_block("Appendix", 14, TRUE, green, after = 0.08)
      for (i in seq_along(used)) { s <- used[[i]]
        text_block(sprintf("%s. %s", LETTERS[i], s$label), 12, TRUE, after = 0.02)
        text_block(sprintf("Dataset: %s", s$dataset %||% ""), 9, col = "#6B6F7B", after = 0.06)
        if (length(s$notes)) { text_block("Data notes", 10.5, TRUE); for (nt in s$notes) text_block(paste("•", nt), 9.5, after = 0.02) }
        if (nzchar(s$code %||% "")) { text_block("R code", 10.5, TRUE, after = 0.04); text_block(s$code, 7.5, mono = TRUE, after = 0.15) }
      }
    }
  }
  y <- H - mg; page <- 1; first <- TRUE
  footer <- function() grid::grid.text(sprintf("%s · page %d", o$title, page), x = grid::unit(W / 2, "in"), y = grid::unit(0.45, "in"),
                                       gp = grid::gpar(fontsize = 8, col = "#8A8F9C", fontfamily = fam))
  new_page <- function() { footer(); grid::grid.newpage(); page <<- page + 1; y <<- H - mg }
  grid::grid.newpage()
  for (i in seq_along(blocks)) {
    b <- blocks[[i]]
    if (b$h < 0) { new_page(); next }
    need <- b$h
    if (isTRUE(b$keep)) { j <- i; while (j < length(blocks) && isTRUE(blocks[[j]]$keep)) j <- j + 1; need <- sum(vapply(blocks[i:j], function(x) max(x$h, 0), 0)) }
    if (y - need < mg && y < H - mg - 0.01) new_page()
    if (!is.null(b$draw)) b$draw(y)
    y <- y - b$h
  }
  footer()
}

# ---------- Excel ----------

report_xlsx <- function(pins, file) {
  out <- list(); used <- character(0)
  for (p in pins) if (p$kind == "table") {
    nm <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "", p$title), 1, 28); base <- nm; k <- 1
    while (nm %in% used) { k <- k + 1; nm <- sprintf("%s %d", substr(base, 1, 26), k) }
    used <- c(used, nm); out[[nm]] <- p$df
  }
  txt <- Filter(function(p) p$kind %in% c("text", "note", "heading"), pins)
  if (length(txt)) out$Text <- data.frame(Item = vapply(txt, function(p) p$title %||% p$kind, ""), Text = vapply(txt, function(p) p$text, ""))
  if (!length(out)) out$Report <- data.frame(Note = "No tables are pinned yet.")
  writexl::write_xlsx(out, file)
}

# ---------- Page ----------

report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Report", "Pinned results become a report you can export and submit.",
              div(class = "hrow", btn(ns("preview"), "Preview", "eye", size = "bsm"),
                  downloadButton(ns("dl_top"), label = tagList(icon("download", 14), "Export"), icon = NULL, class = "btn-k bp bsm"))),
    div(class = "split rep",
        div(class = "col",
            kcard(div(class = "hrow", h2(class = "ct", "Pinned items"), uiOutput(ns("count"), inline = TRUE, class = "ml")),
                  uiOutput(ns("pins")),
                  btn(ns("add_note"), "Add a note or heading", "plus", size = "bsm", style = "width:100%;margin-top:10px")),
            kcard(div(class = "col", style = "gap:12px",
                      h2(class = "ct", "Export"),
                      textInput(ns("title"), "Report title", ""),
                      div(class = "grid g2", style = "gap:12px",
                          textInput(ns("subtitle"), "Subtitle", ""),
                          textInput(ns("author"), "Author or course", "")),
                      div(tags$label(class = "control-label", "Template"), seg_input(ns("template"), REPORT_TEMPLATES, "lab")),
                      div(class = "fmtlist", radioButtons(ns("format"), NULL, selected = "docx",
                          choiceNames = unname(lapply(REPORT_FORMATS, function(f) span(class = "fmt", span(f[1]), span(class = "ext", f[2])))),
                          choiceValues = names(REPORT_FORMATS))),
                      checkboxInput(ns("appendix"), "Include data notes and R code appendix", TRUE),
                      uiOutput(ns("dl_btn"))))),
        div(class = "rep-stage", uiOutput(ns("preview_body")))))
}

report_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pins <- reactive(store$pins %||% list())
    srcs <- reactive(store$report_src %||% list())

    # Fill the title and subtitle from the first analysis until the user types their own.
    touched <- reactiveValues(title = FALSE, subtitle = FALSE)
    last_auto <- reactiveValues(title = "", subtitle = "")
    observe({
      d <- report_opts_default(pins(), srcs())
      for (k in c("title", "subtitle")) {
        cur <- isolate(input[[k]]) %||% ""
        if (!nzchar(cur) || identical(cur, isolate(last_auto[[k]]))) { last_auto[[k]] <- d[[k]]; updateTextInput(session, k, value = d[[k]]) }
      }
    })

    opts <- reactive({
      d <- report_opts_default(pins(), srcs())
      list(title = if (nzchar(trimws(input$title %||% ""))) input$title else d$title,
           subtitle = input$subtitle %||% d$subtitle, author = input$author %||% "",
           template = input$template %||% "lab", appendix = isTRUE(input$appendix))
    })

    output$count <- renderUI(span(class = "pill gold", length(pins())))
    output$pins <- renderUI({
      ps <- pins()
      if (!length(ps)) return(div(class = "empty", style = "padding:22px 8px",
                                  p(class = "small", style = "margin:0", "Nothing pinned yet. Run any analysis and press ", tags$b("Pin to report"), ".")))
      div(class = "pinlist", id = ns("pinlist"), `data-input` = ns("order"),
          lapply(ps, function(p) div(class = "pinrow", draggable = "true", `data-id` = p$id,
            span(class = "drag", title = "Drag to reorder", icon("drag", 14)),
            ico_box(p$icon %||% "file", if (p$kind %in% c("heading", "note")) "ip" else "ig", 15),
            div(class = "pt", div(class = "t", if (p$kind %in% c("heading", "note")) (if (p$kind == "heading") "Heading" else "Note") else p$title),
                div(class = "s", if (p$kind %in% c("heading", "note")) substr(p$text, 1, 60) else p$source %||% "")),
            tags$button(class = "ib x", title = "Move up", onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("up"), p$id), icon("chev", 14, class = "rot-up")),
            tags$button(class = "ib x", title = "Remove", onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("rm"), p$id), icon("x", 14)))))
    })
    ids <- function() vapply(pins(), `[[`, "", "id")
    observeEvent(input$rm, store$pins <- Filter(function(p) p$id != input$rm, pins()))
    observeEvent(input$up, { i <- match(input$up, ids()); if (!is.na(i) && i > 1) { ps <- pins(); ps[c(i - 1, i)] <- ps[c(i, i - 1)]; store$pins <- ps } })
    observeEvent(input$order, { o <- unlist(input$order); ps <- pins(); i <- match(o, ids()); if (length(i) == length(ps) && !anyNA(i)) store$pins <- ps[i] })

    observeEvent(input$add_note, showModal(modalDialog(
      title = "Add a note or heading", easyClose = TRUE,
      seg_input(ns("note_kind"), c("Heading" = "heading", "Note" = "note"), "heading"),
      textAreaInput(ns("note_text"), NULL, "", width = "100%", rows = 4, placeholder = "For example: Discussion, or a sentence about how the data were collected"),
      footer = tagList(modalButton("Cancel"), btn(ns("note_ok"), "Add", "plus", "bp")))))
    observeEvent(input$note_ok, {
      txt <- trimws(input$note_text %||% ""); if (!nzchar(txt)) { removeModal(); return() }
      store$pin_seq <- (store$pin_seq %||% 0) + 1
      k <- input$note_kind %||% "note"
      pin <- mk_pin(k, if (k == "heading") "Heading" else "Note", if (k == "heading") "tag" else "edit", text = txt)
      pin$id <- paste0("pin", store$pin_seq); pin$source <- ""
      store$pins <- c(pins(), list(pin)); removeModal()
    })

    preview_html <- reactive({
      ps <- pins()
      if (!length(ps)) return(div(class = "rpaper empty-paper", div(class = "empty", icon("file", 30), h3(class = "ct", "Your report appears here"),
        p(class = "small", "Run an analysis, press Pin to report, and the tables, charts and interpretation you choose are laid out here."))))
      div(class = "rpaper", tags$style(HTML(REPORT_CSS)), HTML(report_html_body(ps, srcs(), opts())))
    })
    output$preview_body <- renderUI(preview_html())
    observeEvent(input$preview, showModal(modalDialog(size = "xl", easyClose = TRUE, footer = modalButton("Close"), title = "Report preview",
                                                      div(class = "rep-stage", style = "padding:16px", preview_html()))))

    fmt <- reactive(input$format %||% "docx")
    output$dl_btn <- renderUI(downloadButton(ns("dl"), label = tagList(icon("download", 15), sprintf("Download %s", switch(fmt(), docx = "Word report", pdf = "PDF report", html = "HTML report", xlsx = "Excel tables"))),
                                             icon = NULL, class = "btn-k bp", style = "width:100%"))
    export <- function(file) {
      ps <- pins(); o <- opts()
      if (!length(ps)) ps <- list(mk_pin("note", "Note", text = "Nothing is pinned yet. Run an analysis and press Pin to report."))
      switch(fmt(),
        docx = report_docx(ps, srcs(), o, file),
        pdf = report_pdf(ps, srcs(), o, file),
        html = writeLines(report_html_doc(ps, srcs(), o), file, useBytes = TRUE),
        xlsx = report_xlsx(ps, file))
    }
    fname <- function() sprintf("%s.%s", gsub("[^A-Za-z0-9]+", "_", substr(opts()$title, 1, 50)), REPORT_FORMATS[[fmt()]][2])
    output$dl <- downloadHandler(filename = fname, content = export)
    output$dl_top <- downloadHandler(filename = fname, content = export)
  })
}
