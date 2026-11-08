# Worked-solution PDF for a Problem Sets exercise: the problem, the data and every step of the
# process (hypotheses, α, information, test, decision, post-hoc, assumption checks and, when an
# assumption fails, the non-parametric restart), with the numbers, tables and plots.
# Plain R (grid, gridExtra, ggplot2) drawn with cairo_pdf, so it can be tested without Shiny.

# ---------- Content: a list of blocks ----------
# Block types: h1 (step heading), h2 (sub heading), p (paragraph), mono (lines), tbl (data frame),
# plot (ggplot and height in mm), note (highlighted paragraph), rule.

ps_solution_blocks <- function(m, s = NULL, test = NULL, ph = NULL, lang = "en") {
  tr <- function(x) x   # the blocks are written in English; ps_solution_pdf translates them when lang is "ar"
  st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
  if (is.na(kind)) stop(ps_kind_problem(kind, st))
  s <- s %||% ps_default_settings(m, kind)
  info <- ps_info(kind, st, m, s)
  multi <- ps_multi(kind); fact <- ps_is_fact(kind)
  test <- test %||% info$suggest
  rank_first <- multi && test %in% PS_NONPAR_TESTS
  B <- list(); add <- function(type, ...) B[[length(B) + 1]] <<- list(type = type, ...)
  step <- function(n, title) add("h1", text = sprintf("%s %d · %s", tr("Step"), n, tr(title)))
  para <- function(x) for (y in x[!is.na(x) & nzchar(x)]) add("p", text = y)
  fam_now <- if (rank_first) "nonpar" else if (multi) "param" else info$family

  # ---- Step 1 ----
  add("title", text = sprintf("%s %s", if (identical(m$id, "custom")) tr("Custom problem") else tr("Problem"), if (identical(m$id, "custom")) "" else m$id),
      sub = m$title, meta = sprintf("%s · %s", ps_type_of(kind, st), format(Sys.Date(), "%Y-%m-%d")))
  step(1, "Problem & data")
  para(m$statement); for (x in m$parts) add("p", text = x, indent = TRUE)
  if (!is.null(m$note)) add("note", text = m$note)
  if (isTRUE(m$edited)) add("note", text = tr("The data were edited by the student: this solution uses the edited values, not the printed ones."), kind = "w")
  disp <- m$display %||% m$data
  add("tbl", df = disp, max_rows = 45, caption = sprintf("%s (%d)", tr("Data"), sum(!is.na(st$d$y)) + (if (!is.null(st$missing)) 0 else 0)))
  add("plot", plot = ps_plot_data(kind, st, m), h = 62, caption = tr("The data"))

  # ---- Step 2 ----
  step(2, "Hypotheses")
  if (fact) {
    fh <- ps_fact_hypotheses(st, m, if (rank_first) "nonpar" else "param")
    add("tbl", df = data.frame(Effect = vapply(names(fh), function(cd) sprintf("%s (%s)", cd, fh[[cd]]$label %||% ""), ""), H0 = vapply(fh, function(e) e$h0, ""), H1 = vapply(fh, function(e) e$h1, ""),
                               check.names = FALSE, row.names = NULL), widths = c(.2, .4, .4))
  } else {
    h <- ps_hypotheses(kind, st, m, s, fam_now)
    add("p", text = sprintf("H0: %s", h$h0)); add("p", text = sprintf("H1: %s", h$h1)); add("mono", lines = c(h$h0s, h$h1s))
  }

  # ---- Step 3 ----
  step(3, "Significance level")
  add("p", text = sprintf("α = %s. %s", format(s$alpha), sprintf(tr("H0 is rejected when the p-value is less than %s."), format(s$alpha))))

  # ---- Step 4 ----
  step(4, "Information available")
  add("tbl", df = info$facts, widths = c(.28, .72))
  if (!is.null(info$family_reason)) add("p", text = info$family_reason)
  if (!is.null(info$suggest_reason)) add("p", text = info$suggest_reason)
  if (fact && isTRUE(st$cpool)) { st2 <- ps_fact_set_pool(st, s); add("p", text = sprintf("%s %s (%s %d).", tr("Effects put into the error:"), paste(st2$pool_codes, collapse = ", "), tr("error df ="), length(st2$pool_codes))) }
  if (fact) {
    add("plot", plot = ps_plot_fact_main(st, m), h = 60, caption = tr("Main-effects plot"))
    np <- choose(length(st$ffacs), 2)
    add("plot", plot = ps_plot_fact_inter(st, m), h = if (np <= 1) 60 else if (np <= 3) 70 else 40 + 28 * ceiling(np / 3), caption = tr("Interaction plots"))
    if (ps_fact_2level(st)) {
      ce <- ps_fact_contrasts(st, m)
      add("tbl", df = data.frame(Effect = sprintf("%s (%s)", ce$label, ce$code), `Effect estimate` = ps_fmt(ce$effect, 5), `Sum of squares` = ps_fmt(ce$ss, 6), `% contribution` = sprintf("%.2f", ce$pct), check.names = FALSE),
          caption = tr("Effect estimates of the 2^k design"), widths = c(.4, .2, .2, .2))
      add("plot", plot = ps_plot_fact_normal(st, m), h = 70, caption = tr("Normal probability plot of the effects"))
      add("plot", plot = ps_plot_fact_qq(st, m), h = 70, caption = tr("Normal Q-Q plot of the effects (as in class)"))
    }
  }

  # ---- Step 5 ----
  step(5, "Test")
  res <- ps_run_test(kind, test, st, m, s)
  add("p", text = sprintf("%s: %s", tr("Test"), res$t$name %||% test))
  if (!is.null(res$formula)) add("mono", lines = res$formula)
  if (!is.null(res$anova_df)) add("tbl", df = res$anova_df, caption = tr("ANOVA table"))
  add("tbl", df = ps_test_table(res), caption = tr("Result"))
  if (!is.null(res[["rank_table"]])) add("tbl", df = res[["rank_table"]], caption = res[["rank_title"]])

  # ---- Step 6 ----
  step(6, "Decision")
  concl <- character(0)
  if (fact) {
    add("tbl", df = ps_test_table(res), caption = tr("All the effects"))
    fh <- ps_fact_hypotheses(st, m, if (rank_first) "nonpar" else "param")
    for (cd in res$effects$code) {
      d <- ps_decision(ps_fact_effect_res(res, cd), fh[[cd]]$h1 %||% "")
      add("p", text = sprintf("%s: %s", cd, d$conclusion)); concl <- c(concl, d$conclusion)
    }
  } else {
    d <- ps_decision(res, if (rank_first) ps_hypotheses(kind, st, m, s, "nonpar")$h1 else ps_hypotheses(kind, st, m, s, info$family)$h1)
    add("note", text = d$conclusion, kind = if (d$reject) "r" else "k")
    add("p", text = sprintf("%s: %s", tr("Rejection region"), d$rr$text)); add("p", text = sprintf("%s: %s", tr("Confidence interval"), d$ci$text)); add("p", text = sprintf("%s: %s", tr("P-value"), d$pv$text))
    add("plot", plot = ps_plot_rr(d$ref, res$alpha), h = 55, caption = tr("Rejection region")); concl <- d$conclusion
  }

  # ---- Step 7 ----
  if (multi && isTRUE(res$reject)) {
    step(7, "Post-hoc")
    ch <- ps_posthoc_choices(res$test, kind, res$control); mth <- if (!is.null(ph) && ph %in% names(ch)) ph else ps_default_ph(s, ch)
    add("p", text = sprintf("%s: %s", tr("Method"), ch[[mth]]))
    if (fact) {
      for (cd in sig_codes(res)) { fp <- ps_fact_posthoc(res, cd, mth); add("h2", text = sprintf("%s · %s", cd, fp$label))
        for (b in fp$blocks) { add("p", text = b$title); add("tbl", df = b$table); add("tbl", df = b$groups) } }
    } else {
      pp <- ps_posthoc(res, mth); add("tbl", df = pp$table, caption = tr("Pairwise comparisons")); add("tbl", df = pp$groups, caption = tr("Grouping"))
    }
  } else if (multi) { step(7, "Post-hoc"); add("p", text = tr("H0 was not rejected, so no post-hoc analysis is needed.")) }

  # ---- Step 8 ----
  if (multi) {
    step(8, "Assumption checks")
    a <- ps_assumptions(kind, st, s, if (rank_first) info$suggest else test)
    add("tbl", df = a$normality, caption = tr("Normality")); add("tbl", df = a$variance %||% data.frame(Note = "Not required (Welch ANOVA)"), caption = tr("Constant variance"))
    add("tbl", df = a$independence, caption = tr("Independence"))
    add("plot", plot = plot_qq_vec(a$targets[[1]], "Residuals"), h = 58, caption = tr("Normal QQ plot of the residuals"))
    add("plot", plot = ps_plot_rvf(a$ind_fit), h = 58, caption = tr("Residuals versus fitted values"))
    if (a$all_ok) { add("note", kind = "k", text = tr("All the assumptions hold: the analysis stops here.")) }
    else if (rank_first) add("note", kind = "w", text = sprintf(tr("%s failed, so the rank test chosen in step 5 is the appropriate analysis."), paste(a$failed, collapse = " and ")))
    else {
      add("note", kind = "w", text = sprintf(tr("%s failed: the process starts again with the non-parametric test."), paste(a$failed, collapse = " and ")))
      # ---- Steps 9 to 14 ----
      npt <- ps_np_test(kind)
      step(9, "Non-parametric hypotheses")
      if (fact) { fhn <- ps_fact_hypotheses(st, m, "nonpar")
        add("tbl", df = data.frame(Effect = names(fhn), H0 = vapply(fhn, function(e) e$h0, ""), H1 = vapply(fhn, function(e) e$h1, ""), check.names = FALSE, row.names = NULL), widths = c(.12, .44, .44))
      } else { hn <- ps_hypotheses(kind, st, m, s, "nonpar"); add("p", text = sprintf("H0: %s", hn$h0)); add("p", text = sprintf("H1: %s", hn$h1)) }
      step(10, "Significance level (non-parametric)"); add("p", text = sprintf("α = %s (%s).", format(s$alpha), tr("kept from step 3")))
      step(11, "Information available (non-parametric)")
      add("p", text = sprintf("%s %s.", ps_np_name(kind), np_about_txt(kind)))
      step(12, "Rank test")
      rn <- ps_run_test(kind, npt, st, m, s)
      if (!is.null(rn$formula)) add("mono", lines = rn$formula)
      add("tbl", df = ps_test_table(rn), caption = rn$t$name)
      if (!is.null(rn[["rank_table"]])) add("tbl", df = rn[["rank_table"]], caption = rn[["rank_title"]])
      step(13, "Rank test decision")
      if (fact) { fhn <- ps_fact_hypotheses(st, m, "nonpar")
        for (cd in rn$effects$code) { d <- ps_decision(ps_fact_effect_res(rn, cd), fhn[[cd]]$h1 %||% ""); add("p", text = sprintf("%s: %s", cd, d$conclusion)); concl <- c(concl, d$conclusion) }
      } else { d <- ps_decision(rn, ps_hypotheses(kind, st, m, s, "nonpar")$h1); add("note", text = d$conclusion, kind = if (d$reject) "r" else "k"); add("p", text = d$pv$text); concl <- d$conclusion }
      if (isTRUE(rn$reject)) {
        step(14, "Rank test post-hoc")
        chn <- ps_posthoc_choices(npt, kind)
        for (mth in names(chn)) {
          add("p", text = sprintf("%s: %s", tr("Method"), chn[[mth]]))
          if (fact) { for (cd in sig_codes(rn)) { fp <- ps_fact_posthoc(rn, cd, mth); add("h2", text = sprintf("%s · %s", cd, fp$label)); for (b in fp$blocks) { add("p", text = b$title); add("tbl", df = b$table) } } }
          else { pp <- ps_posthoc(rn, mth); add("tbl", df = pp$table); add("tbl", df = pp$groups) }
        }
      } else { step(14, "Rank test post-hoc"); add("p", text = tr("H0 was not rejected, so no post-hoc analysis is needed.")) }
    }
  }
  add("h1", text = tr("Conclusion")); for (x in concl) add("p", text = x)
  B
}

sig_codes <- function(res) { sg <- res$effects$code[res$effects$p < res$alpha]; sg[order(-nchar(sg), match(sg, res$effects$code))] }
np_about_txt <- function(kind) switch(kind, k = "compares the groups through the ranks of all the observations",
  rcbd = "ranks the responses within each block", fact = "ranks all the observations together and tests each effect on the ranks",
  factb = "ranks the combinations within each block and tests each effect on the rank sums",
  factl = , factg = "removes the blocking factors, ranks the residuals and tests each effect on the ranks",
  "removes the blocking factors and compares the treatments through the ranks of the residuals")
sol_tr <- function(x, lang = "en") if (identical(lang, "ar")) ar_text(x) else x

# The text of a block in Arabic (names and numbers stay as they are; the formula lines stay in English).
sol_ar_block <- function(b) {
  if (b$type == "p" && grepl("^(Rejection region|Confidence interval|P-value|Method|Test): ", b$text)) {
    b$text <- paste0(ar_text(sub(": .*$", "", b$text)), ": ", ar_text(sub("^[^:]*: ", "", b$text)))
    for (f in c("sub", "caption")) if (!is.null(b[[f]])) b[[f]] <- ar_text(b[[f]])
    return(b)
  }
  for (f in c("text", "sub", "caption")) if (!is.null(b[[f]])) b[[f]] <- ar_text(b[[f]])
  if (b$type == "title") b$meta <- paste(ar_text(sub(" · [0-9-]+$", "", b$meta)), sub("^.* · ", "", b$meta), sep = " · ")
  if (b$type == "tbl" && !is.null(b$df)) {
    df <- b$df; df[] <- lapply(df, function(x) ar_text(as.character(x))); names(df) <- ar_text(names(df)); b$df <- df
    if (!is.null(b$widths)) b$widths <- b$widths
  }
  b
}

# ---------- Layout and drawing ----------

SOL_W <- 180; SOL_H <- 262   # usable width and height of an A4 page, in mm

sol_mm <- function(g) grid::convertHeight(grid::grobHeight(g), "mm", valueOnly = TRUE)
sol_wrap <- function(x, width) unlist(lapply(strsplit(x, "\n", fixed = TRUE)[[1]], function(l) if (!nzchar(l)) "" else strwrap(l, width = width)))

sol_grob <- function(b, fam = "sans", rtl = FALSE) {
  fs <- 9.5; col <- "#1F2933"; hx <- if (rtl) 1 else 0
  switch(b$type,
    title = { g <- grid::gTree(children = grid::gList(
        grid::textGrob(b$text, x = hx, y = 1, hjust = hx, vjust = 1, gp = grid::gpar(fontsize = 20, fontface = "bold", col = "#0B6B3A", fontfamily = fam)),
        grid::textGrob(b$sub, x = hx, y = grid::unit(1, "npc") - grid::unit(9, "mm"), hjust = hx, vjust = 1, gp = grid::gpar(fontsize = 12, col = col, fontfamily = fam)),
        grid::textGrob(b$meta, x = hx, y = grid::unit(1, "npc") - grid::unit(15, "mm"), hjust = hx, vjust = 1, gp = grid::gpar(fontsize = 8.5, col = "#6B7280", fontfamily = fam)))); list(g = g, h = 22) },
    h1 = { g <- grid::gTree(children = grid::gList(
        grid::rectGrob(x = 0, y = 0, width = grid::unit(1, "npc"), height = grid::unit(0.45, "mm"), just = c("left", "bottom"), gp = grid::gpar(fill = "#0B6B3A", col = NA)),
        grid::textGrob(b$text, x = hx, y = grid::unit(2, "mm"), hjust = hx, vjust = 0, gp = grid::gpar(fontsize = 12.5, fontface = "bold", col = "#0B6B3A", fontfamily = fam)))); list(g = g, h = 11, before = 4) },
    h2 = list(g = grid::textGrob(b$text, x = hx, hjust = hx, gp = grid::gpar(fontsize = 10.5, fontface = "bold", col = col, fontfamily = fam)), h = 7),
    p = { w <- if (isTRUE(b$indent)) 100 else 104; lines <- sol_wrap(b$text, w); if (isTRUE(b$indent) && !rtl) lines <- paste0("   ", lines)
      list(g = grid::textGrob(paste(lines, collapse = "\n"), x = hx, y = 1, hjust = hx, vjust = 1, gp = grid::gpar(fontsize = fs, col = col, lineheight = 1.25, fontfamily = fam)), h = length(lines) * 4.75 + 2) },
    note = { lines <- sol_wrap(b$text, 96); h <- length(lines) * 4.75 + 6
      fillc <- switch(b$kind %||% "i", r = "#E3F4EA", k = "#EEF2F5", w = "#FFF4DB", "#EAF2F7")
      g <- grid::gTree(children = grid::gList(grid::roundrectGrob(gp = grid::gpar(fill = fillc, col = NA), r = grid::unit(1.5, "mm")),
        grid::textGrob(paste(lines, collapse = "\n"), x = if (rtl) grid::unit(1, "npc") - grid::unit(3, "mm") else grid::unit(3, "mm"), y = grid::unit(1, "npc") - grid::unit(3, "mm"), hjust = hx, vjust = 1, gp = grid::gpar(fontsize = fs, col = col, lineheight = 1.25, fontfamily = fam)) ))
      list(g = g, h = h) },
    mono = { lines <- unlist(lapply(b$lines, sol_wrap, width = 98)); list(g = grid::textGrob(paste(lines, collapse = "\n"), x = 0, y = 1, hjust = 0, vjust = 1, gp = grid::gpar(fontsize = 8, col = col, fontfamily = "mono", lineheight = 1.3)), h = length(lines) * 4.1 + 3) },
    plot = { g <- ggplot2::ggplotGrob(b$plot + ggplot2::theme(plot.margin = ggplot2::margin(2, 6, 2, 2))); list(g = g, h = b$h %||% 60) },
    NULL)
}

sol_table_chunks <- function(b, fam = "sans", rtl = FALSE) {
  df <- b$df; if (is.null(df) || !ncol(df)) return(list())
  if (rtl) { df <- df[, rev(seq_along(df)), drop = FALSE]; if (!is.null(b$widths)) b$widths <- rev(b$widths) }
  df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- "—"; x })
  if (!is.null(b$max_rows) && nrow(df) > b$max_rows) df <- df[seq_len(b$max_rows), , drop = FALSE]
  n <- nrow(df); per <- 24
  idx <- split(seq_len(n), ceiling(seq_len(n) / per))
  lapply(seq_along(idx), function(k) {
    d <- df[idx[[k]], , drop = FALSE]
    # wrap long cells to the column's share of the page width
    fs <- if (ncol(d) >= 7) 6.8 else if (ncol(d) >= 5) 7.4 else 8
    wid <- b$widths %||% { nc <- pmax(nchar(names(d)), vapply(d, function(x) max(nchar(x), 0), 1)); pmin(pmax(nc, 6), 60) / sum(pmin(pmax(nc, 6), 60)) }
    cw <- pmax(8, floor(wid * 112 / (fs / 8)))
    for (j in seq_along(d)) d[[j]] <- vapply(d[[j]], function(x) paste(strwrap(x, width = max(6, cw[j])), collapse = "\n"), "")
    names(d) <- vapply(names(d), function(x) paste(strwrap(x, width = max(6, cw[match(x, names(d))] %||% 12)), collapse = "\n"), "")
    th <- gridExtra::ttheme_minimal(base_size = fs, base_family = fam,
      core = list(fg_params = list(hjust = if (rtl) 1 else 0, x = if (rtl) 0.96 else 0.04, col = "#1F2933"), bg_params = list(fill = c("white", "#F3F6F8"), col = NA), padding = grid::unit(c(2.2, 1.6), "mm")),
      colhead = list(fg_params = list(hjust = if (rtl) 1 else 0, x = if (rtl) 0.96 else 0.04, fontface = "bold", col = "white"), bg_params = list(fill = "#0B6B3A", col = NA), padding = grid::unit(c(2.2, 2), "mm")))
    g <- gridExtra::tableGrob(d, rows = NULL, theme = th)
    tw <- sum(grid::convertWidth(g$widths, "mm", valueOnly = TRUE))
    if (tw > SOL_W) g$widths <- g$widths * (SOL_W / tw)
    tw <- min(tw, SOL_W); h <- sum(grid::convertHeight(g$heights, "mm", valueOnly = TRUE))
    cap <- if (k == 1 && !is.null(b$caption)) b$caption else if (k > 1 && !is.null(b$caption)) paste(b$caption, "(cont.)")
    list(g = g, h = h + 1.5, caption = cap, w = tw)
  })
}

ps_solution_pdf <- function(m, file, s = NULL, test = NULL, ph = NULL, lang = "en") {
  blocks <- ps_solution_blocks(m, s, test, ph, lang)
  rtl <- identical(lang, "ar"); if (rtl) blocks <- lapply(blocks, sol_ar_block)
  fam <- sol_font(lang)
  grDevices::cairo_pdf(file, width = 8.27, height = 11.69, onefile = TRUE, family = fam)
  on.exit(grDevices::dev.off(), add = TRUE)
  page <- 0L; y <- 0; cap_h <- 5.5
  title <- if (identical(m$id, "custom")) (m$title %||% "Custom problem") else sprintf("Problem %s", m$id)
  new_page <- function() {
    page <<- page + 1L; grid::grid.newpage(); y <<- 0
    grid::pushViewport(grid::viewport(x = grid::unit(15, "mm"), y = grid::unit(1, "npc") - grid::unit(14, "mm"), width = grid::unit(SOL_W, "mm"), height = grid::unit(SOL_H, "mm"), just = c("left", "top"), name = "body"))
    grid::grid.text(sprintf("%s · %s", sol_tr("StatDesign Studio", lang), sol_tr(title, lang)), x = if (rtl) 1 else 0, y = grid::unit(SOL_H + 6, "mm"), just = c(if (rtl) "right" else "left", "bottom"), gp = grid::gpar(fontsize = 7.5, col = "#6B7280", fontfamily = fam))
    grid::grid.text(sprintf("%d", page), x = if (rtl) 0 else 1, y = grid::unit(-9, "mm"), just = c(if (rtl) "left" else "right", "top"), gp = grid::gpar(fontsize = 8, col = "#6B7280", fontfamily = fam))
    grid::grid.text(sol_tr("KFUPM · Experimental Design · Worked solution", lang), x = if (rtl) 1 else 0, y = grid::unit(-9, "mm"), just = c(if (rtl) "right" else "left", "top"), gp = grid::gpar(fontsize = 7.5, col = "#6B7280", fontfamily = fam))
    grid::upViewport(0)
  }
  place <- function(g, h, caption = NULL, before = 0, w = SOL_W, keep = 0, right = FALSE) {
    need <- h + (if (!is.null(caption)) cap_h else 0) + before + 1.5 + keep
    if (page == 0L || y + need > SOL_H) new_page()
    y <<- y + before
    grid::downViewport("body")
    if (!is.null(caption)) {
      grid::pushViewport(grid::viewport(x = 0, y = grid::unit(SOL_H - y, "mm"), width = grid::unit(SOL_W, "mm"), height = grid::unit(cap_h, "mm"), just = c("left", "top")))
      grid::grid.text(caption, x = if (right) 1 else 0, hjust = if (right) 1 else 0, gp = grid::gpar(fontsize = 9.5, fontface = "bold", col = "#374151", fontfamily = fam)); grid::popViewport(); y <<- y + cap_h
    }
    grid::pushViewport(grid::viewport(x = if (right) 1 else 0, y = grid::unit(SOL_H - y, "mm"), width = grid::unit(w, "mm"), height = grid::unit(h, "mm"), just = c(if (right) "right" else "left", "top")))
    grid::grid.draw(g); grid::popViewport(); grid::upViewport(0)
    y <<- y + h + 1.5
  }
  # a heading is kept with what follows it (the height of the next block, up to 45 mm)
  nexth <- function(k) { if (k > length(blocks)) return(0); nb <- blocks[[k]]
    if (nb$type == "tbl") { ch <- sol_table_chunks(nb, fam, rtl); return(if (length(ch)) min(100, ch[[1]]$h + 6) else 0) }
    o <- sol_grob(nb, fam, rtl && nb$type != "mono"); if (is.null(o)) 0 else min(100, o$h + (if (!is.null(nb$caption)) cap_h else 0)) }
  for (k in seq_along(blocks)) {
    b <- blocks[[k]]
    if (b$type == "tbl") { for (ch in sol_table_chunks(b, fam, rtl)) place(ch$g, ch$h, ch$caption, w = ch$w, right = rtl); y <- y + 1.5; next }
    o <- sol_grob(b, fam, rtl && b$type != "mono"); if (is.null(o)) next
    place(o$g, o$h, b$caption, o$before %||% 0, right = rtl && b$type == "plot", keep = if (b$type %in% c("h1", "h2")) nexth(k + 1) else 0)
  }
  invisible(file)
}

# A font that has the Arabic letters when the Arabic interface is on.
sol_font <- function(lang = "en") "sans"
