/* Guided tour: draws a box around the parts of the screen a newcomer needs, one at a time,
   with Back and Next. It works on every page and knows where the student is: the page, the
   stage of a wizard (Data, Map variables, Context & settings, Results) or the step of a
   Problem Sets problem. Everything is client-side; it only reads the page and, in the
   walk-throughs, clicks the same buttons a student would. */
(function () {
  'use strict';
  var T = window.StatTour = {};
  var S = { steps: [], i: 0, on: false, token: 0, menuOpen: false };
  var WIZ = ['one', 'two', 'k', 'crd', 'rcbd', 'lsd', 'glsd'];
  var PAGE_NAME = { home: 'Home', advisor: 'Test Advisor', data: 'Data Studio', desc: 'Descriptive statistics', one: 'One sample', two: 'Two samples', k: 'k samples',
                    problems: 'Problem Sets', crd: 'CRD', rcbd: 'RCBD', lsd: 'Latin Square', glsd: 'Graeco-Latin Square', planner: 'Design Planner',
                    report: 'Report', learn: 'Learn', team: 'About the team' };

  // ---------- helpers ----------
  // offsetParent is null for fixed-position elements (the assistant button), so test the box and the style instead.
  function vis(e) { if (!e) return false; var r = e.getBoundingClientRect(); if (r.width <= 0 || r.height <= 0) return false; var cs = window.getComputedStyle(e); return cs.visibility !== 'hidden' && cs.display !== 'none'; }
  function find(sel) {
    if (!sel) return null;
    var list = Array.isArray(sel) ? sel : [sel];
    for (var k = 0; k < list.length; k++) {
      var els; try { els = document.querySelectorAll(list[k]); } catch (e) { continue; }
      for (var j = 0; j < els.length; j++) if (vis(els[j])) return els[j];
    }
    return null;
  }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function page() {
    var p = document.getElementById('pages'), pane = p && p.parentElement.querySelector(':scope > .tab-content > .tab-pane.active');
    return pane ? pane.getAttribute('data-value') : 'home';
  }
  function shinySet(id, v) { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(id, v, { priority: 'event' }); }
  function nav(pg) { shinySet('nav', pg); return wait(900); }
  function click(sel) { var e = find(sel); if (e) e.click(); return wait(1700); }
  // the problems page has nested hidden tab sets, so look for the active pane by name rather than by position
  function psView() {
    var all = document.querySelectorAll('div.tab-pane[data-value="problems"] .tab-pane.active');
    for (var k = 0; k < all.length; k++) { var v = all[k].getAttribute('data-value'); if (v === 'list' || v === 'detail' || v === 'edit') return v; }
    return 'list';
  }
  function psStep() {
    var d = document.querySelector('div.tab-pane[data-value="problems"] div.tab-pane[data-value="detail"]');
    var all = d ? d.querySelectorAll('.tab-pane.active') : [];
    for (var k = 0; k < all.length; k++) { var n = parseInt(all[k].getAttribute('data-value'), 10); if (n >= 1 && n <= 14) return n; }
    return 1;
  }
  function psMulti() { return document.querySelectorAll('.ps-stepbar .st').length > 6; }
  function psGo(n) { shinySet('problems-go', n); return wait(2200); }
  function wizStage(pg) {
    var all = document.querySelectorAll('#' + pg + '-stepper .st'), k = -1;
    all.forEach(function (e, i) { if (e.classList.contains('cur')) k = i; });
    return k < 0 ? 0 : k;
  }
  var ACT = 'div.tab-pane[data-value="problems"] div.tab-pane[data-value="detail"] .tab-content > .tab-pane.active ';

  // ---------- the overview of the whole app ----------
  function overview() {
    return [
      { title: 'Welcome to StatDesign Studio', text: 'This short tour shows you around. Each step draws a box around the part of the screen it explains. Press <b>Next</b> (or the → key) to continue, <b>Back</b> (←) to go back, and <b>Esc</b> to leave. You can start a tour again at any time from the <b>Guided tour</b> button at the top of every page.' },
      { t: '#side', place: 'right', title: 'The sidebar: everything is here', text: 'Home, the Test Advisor and Data Studio are at the top. Below are <b>Explore</b>, <b>Compare</b> (one, two and k sample tests), <b>Design of experiments</b> (Problem Sets, CRD, RCBD, Latin and Graeco-Latin squares) and <b>Tools</b> (Design Planner, Report, Learn). Click any item to open that page.' },
      { t: '.side a.nav-item[data-page="advisor"]', place: 'right', title: 'Not sure which test? Ask the Advisor', text: 'Answer three short questions and the Test Advisor tells you which analysis fits your data and opens it for you.' },
      { t: '.side a.nav-item[data-page="data"]', place: 'right', title: 'Bring your data in once', text: '<b>Data Studio</b> is where data enters the app: upload an Excel or CSV file, type or paste it, or load a class example. Every analysis page then lets you pick from your datasets.' },
      { t: '.side a.nav-item[data-page="problems"]', place: 'right', title: 'Problem Sets: solve the course exercises', text: 'The textbook exercises of Chapters 2 to 5 with their full data, solved step by step, plus your own problems. This is the best place to learn the hypothesis-testing process.' },
      { t: '.side a.nav-item[data-page="crd"]', place: 'right', title: 'Design pages: CRD, RCBD, Latin and Graeco-Latin', text: 'Upload data from a designed experiment and get the ANOVA, assumption checks, post-hoc tests and non-parametric fallbacks, with R code and a written interpretation.' },
      { t: '.side a.nav-item[data-page="report"]', place: 'right', title: 'Build your report as you go', text: 'Every result has a <b>Pin to report</b> button. Pinned tables, plots and conclusions collect on the <b>Report</b> page, which exports to Word, PDF, HTML or Excel.' },
      { t: '.search-wrap', place: 'bottom', title: 'Search from anywhere', text: 'Type a test, a design or a page name (for example “Tukey”, “RCBD” or “problems”) to jump straight to it.' },
      { t: '#tour-btn', place: 'bottom', title: 'This button: your guide on every page', text: 'Open it whenever you are lost. It offers a tour of the page you are on, a hands-on walk-through where the app does the steps for you, and this overview.' },
      { t: ['.top .ib[title="Light or dark"]'], place: 'bottom', title: 'Light or dark', text: 'Switch the colour theme to suit your room and your eyes.' },
      { t: ['.top .ib[title="Help"]'], place: 'bottom', title: 'Help and About', text: 'Short explanations of the app, its stages and what each test needs.' },
      { t: ['#save_project'], place: 'bottom', title: 'Save your project', text: 'Download everything (datasets, settings, pinned results) in one file and open it again later, so you never lose your work.' },
      { t: ['.riaz-fab'], place: 'left', title: 'Ask Professor Riaz', text: 'The AI course assistant sees the page you are on. Ask it to explain a result, check an assumption or walk through the R code.' },
      { title: 'You are ready', text: 'A good way to begin: open <b>Problem Sets</b>, pick a problem, and press <b>Guided tour</b> there for the step-by-step walk-through. Or open <b>Data Studio</b> and bring in your own data.' }
    ];
  }

  // ---------- one page at a time ----------
  var PAGE_TOURS = {
    home: function () { return [
      { t: '.hero', place: 'bottom', title: 'Home', text: 'The two big buttons start the most common jobs: <b>Start with your data</b> opens Data Studio, and <b>Try the RCBD example</b> opens a ready-made analysis so you can see what the app produces.' },
      { t: '.tm-aims', place: 'right', title: 'Aim and objectives', text: 'What the app is for: planning, analysing and reporting designed experiments correctly, step by step, without writing code.' },
      { t: '.tm-home', place: 'left', title: 'The project team', text: 'Click any name to open the <b>About the team</b> page with the photos and departments of Group 4.' },
      { t: '.tab-pane.active .grid.g2 a.acard', place: 'bottom', title: 'Analysis cards', text: 'Each card opens an analysis: the four designs under <b>Design of experiments</b>, and descriptive statistics and the one, two and k sample tests under <b>Explore and compare</b>.' },
      { t: '#home_ds', place: 'left', title: 'Your datasets', text: 'Datasets you upload or open appear here. They are kept for the session; use <b>Save project</b> to keep them.' },
      { t: '.tab-pane.active .btn-k.bgh.bsm', place: 'left', title: 'Class examples', text: 'Datasets from the course R scripts. <b>Open</b> loads one and takes you to the matching analysis.' }
    ]; },
    advisor: function () { return [
      { t: '.page-head', place: 'bottom', title: 'Test Advisor', text: 'Not sure which analysis to use? Answer a few questions about your data and it recommends one.' },
      { t: '#advisor-body', place: 'right', title: 'Answer the question', text: 'Click the option that describes your situation. The advisor asks up to three questions, then shows its recommendation with the reasons.' },
      { t: '.tab-pane.active .kcard:last-child', place: 'left', title: 'Your answers so far', text: 'Your earlier answers are listed here so you can see how the recommendation was reached.' }
    ]; },
    data: function () { return [
      { t: '.page-head', place: 'bottom', title: 'Data Studio', text: 'All your data enters the app here. Once a dataset is added, every analysis page can use it.' },
      { t: '#data-tabbar', place: 'bottom', title: 'Four ways to add data', text: '<b>Upload file</b> (Excel or CSV), <b>Type & edit</b> in a spreadsheet grid, <b>Paste from Excel</b>, or <b>Examples</b> from the course. Pick the tab you need.' },
      { t: ['#data-dz', '.tab-pane.active .kcard'], place: 'right', title: 'Drop or browse', text: 'Drag a file here or click to browse. The first sheet of an Excel file is used; you can choose another sheet afterwards.' },
      { t: ['#data-preview', '#data-fileinfo', '#data-dz'], place: 'bottom', title: 'Check the preview', text: 'Once a file is added, a preview of its columns and first rows appears under the drop area. Look at it: the app detects number and text columns and flags problems such as missing values.' },
      { t: '.page-head .btn-k', place: 'bottom', title: 'No data yet? Load an example', text: 'This opens a class dataset so you can try any analysis immediately.' }
    ]; },
    desc: function () { return [
      { t: '.page-head', place: 'bottom', title: 'Descriptive statistics', text: 'Summaries and charts for any column: the first thing to do with new data.' },
      { t: '.tab-pane.active .kcard', place: 'right', title: 'Choose what to describe', text: 'Pick the <b>dataset</b>, the <b>variables</b> to summarise, an optional <b>group by</b> column, and an extra percentile if you want one.' },
      { t: '#desc-body', place: 'left', title: 'The summary', text: 'Mean, standard deviation, quartiles, charts and outliers appear here once a dataset is selected.' },
      { t: ['#desc-pin'], place: 'bottom', title: 'Pin and export', text: '<b>Pin to report</b> adds the table to your report, and <b>Export table</b> downloads it.' }
    ]; },
    planner: function () { return [
      { t: '.tab-pane.active .kcard', place: 'right', title: 'Plan your experiment', text: 'Choose the design (CRD, RCBD, Latin or Graeco-Latin), name your factor and response, and list the treatments.' },
      { t: '#planner-go', place: 'top', title: 'Randomise the layout', text: 'Press this to generate a random layout. The seed makes it repeatable; press the dice button next to it for a new one.' },
      { t: '#planner-power', place: 'right', title: 'Power and sample size', text: 'Enter the difference worth detecting and the expected standard deviation to see how many replicates you need.' },
      { t: '#planner-grid', place: 'top', title: 'Your field layout', text: 'Every plot with its treatment. Download it as a PDF field map, or as an Excel data sheet to fill in during the experiment.' }
    ]; },
    report: function () { return [
      { t: '#report-pins', place: 'right', title: 'Pinned items', text: 'Everything you pin with <b>Pin to report</b> collects here. Reorder it, remove items, or add your own notes and headings.' },
      { t: '#report-add_note', place: 'top', title: 'Add a note or heading', text: 'Write your own text between results: an introduction, a heading or a comment.' },
      { t: ['#report-title', '.tab-pane.active .kcard:nth-of-type(2)'], place: 'left', title: 'Title and author', text: 'Set the report title, subtitle and your name or course.' },
      { t: ['#report-template'], place: 'left', title: 'Template and format', text: 'Choose a template (lab report, thesis or brief) and the format: Word, PDF, HTML or Excel.' },
      { t: ['#report-preview'], place: 'bottom', title: 'Preview', text: 'See the report before you download it.' },
      { t: ['#report-dl_top', '#report-dl'], place: 'bottom', title: 'Export', text: 'Download the finished report.' }
    ]; },
    learn: function () { return [
      { t: '.lnav', place: 'right', title: 'Method guides', text: 'Pick a method from the list: descriptive statistics, the sample tests, the four designs, assumption checks, post-hoc tests and the three ways to decide.' },
      { t: '#learn-body', place: 'left', title: 'What each guide gives you', text: 'When to use the method, the model, the assumptions and what to do when they fail, and a worked class example.' },
      { t: '#learn-open', place: 'bottom', title: 'Open the class example', text: 'Loads that example into the matching analysis page so you can run it yourself.' }
    ]; },
    team: function () { return [
      { t: '.tm-intro', place: 'bottom', title: 'About the team', text: 'StatDesign Studio was built by Group 4 for the KFUPM Experimental Design course.' },
      { t: '.tm-grid', place: 'top', title: 'The team', text: 'Each card shows a member’s photo, department and role. The team leader is marked with a gold badge.' },
      { t: '.tm-aims', place: 'top', title: 'Aim and objectives', text: 'The purpose of the app and what it helps students do.' }
    ]; }
  };

  // ---------- wizard pages: one tour per stage ----------
  function wizardTour(pg) {
    var P = '#' + pg + '-', st = wizStage(pg), name = PAGE_NAME[pg];
    var head = [{ t: P + 'stepper', place: 'bottom', title: name + ': four stages', text: 'Every analysis here has the same four stages: <b>Data</b>, <b>Map variables</b>, <b>Context & settings</b> and <b>Results</b>. The current one is highlighted; completed ones show a tick, and you can click a finished stage to go back to it.' }];
    var how = { t: P + 'how', place: 'bottom', title: 'A short explanation', text: 'This button explains how the method works: when to use it, its assumptions and what the app does for you.' };
    var stages = [
      [ { t: [P + 'ds_pick', '.tab-pane.active .kcard'], place: 'right', title: 'Stage 1 · Choose the data', text: 'Pick one of your datasets. If you have none, upload one in Data Studio first.' },
        { t: P + 'load_example', place: 'top', title: 'Or use the class example', text: 'Loads the course example for this analysis and moves on, so you can see a complete result straight away.' },
        { t: P + 'to2', place: 'top', title: 'Continue', text: 'When a dataset is selected, <b>Continue</b> takes you to the next stage.' } ],
      [ { t: '.tab-pane.active .kcard h2.ct', place: 'right', title: 'Stage 2 · Tell us what each column means', text: 'The app needs to know which column is the <b>response</b> (the number you measured) and which are the <b>treatments, blocks, rows or groups</b>. It guesses from the column names; check each choice.' },
        { t: [P + 'check', P + 'layout'], place: 'left', title: 'The design check', text: 'The app checks the layout against the design: balanced, complete, and the right number of levels. Fix anything it flags before you continue. If a column is stored as numbers (1, 2, 3) it is treated as a label.' },
        { t: ['button[id$="-to3"]'], place: 'top', title: 'Continue', text: 'When the check is green, continue to the context and settings.' } ],
      [ { t: [P + 'title', '.tab-pane.active .kcard'], place: 'right', title: 'Stage 3 · Describe the experiment', text: 'Give the analysis a title and name the response, its unit and the factors in your own words. These names appear in every table, plot and sentence of the result.' },
        { t: [P + 'h0', P + 'hyp'], place: 'right', title: 'Hypotheses', text: 'H0 says there is no effect or difference; H1 says there is. The app writes both from your names. You can edit them.' },
        { t: [P + 'alpha'], place: 'left', title: 'Significance level α', text: 'The chance of a false alarm you will accept: 0.05 is the usual choice. A smaller α makes rejecting H0 harder.' },
        { t: [P + 'posthoc', P + 'method', P + 'assump', P + 'var_mode', P + 'alt'], place: 'left', title: 'Settings', text: 'Choose post-hoc methods, how to treat the assumptions (automatic is recommended: it switches to the non-parametric test when an assumption fails), or the type of alternative.' },
        { t: P + 'run', place: 'top', title: 'Run analysis', text: 'Press this to run everything: the test, the assumption checks, the post-hoc comparisons and the written interpretation.' } ],
      [ { t: [P + 'results h2.ct', '.tab-pane.active h2.ct'], place: 'bottom', title: 'Stage 4 · Your results', text: 'The headline states the conclusion in plain words, with the test, its statistic, the p-value and the decision.' },
        { t: [P + 'rtabbar', P + 'test'], place: 'bottom', title: 'Tabs and tests', text: 'Move between <b>Summary</b>, <b>Explore</b>, <b>ANOVA</b>, <b>Assumptions</b>, <b>Post-hoc</b>, <b>Non-parametric</b> and <b>R code</b> (or choose another test on the sample-test pages). Check <b>Assumptions</b> before you trust the result.' },
        { t: [P + 'edit_settings'], place: 'bottom', title: 'Change the settings', text: 'Go back to stage 3, change α or the test, and run again.' },
        { t: [P + 'pin'], place: 'bottom', title: 'Pin to report', text: 'Adds this result to your report. Pin the parts you want, then export on the Report page.' },
        { t: [P + 'dl_tables'], place: 'bottom', title: 'Download the tables', text: 'All tables in one Excel file.' } ]
    ];
    var out = head.concat(how.t ? [how] : []);
    return out.concat(stages[Math.min(st, 3)]);
  }
  // a hands-on walk-through of a wizard page: loads the class example and runs it
  function wizardDemo(pg) {
    var P = '#' + pg + '-', name = PAGE_NAME[pg];
    var s = [
      { title: 'Walk-through: ' + name, text: 'I will load the class example and take you through every stage, doing the clicks for you. Watch the highlighted box; press <b>Next</b> when you are ready for the following part.' },
      { t: P + 'stepper', place: 'bottom', title: 'The four stages', text: 'Data → Map variables → Context & settings → Results.', before: function () { return nav(pg).then(function () { shinySet(pg + '-goto_step', 1); return wait(1000); }); } },
      { t: [P + 'ds_pick', P + 'load_example'], place: 'top', title: 'Stage 1 · Data', text: 'Here you would pick a dataset. I am loading the class example now.' },
      { t: '.tab-pane.active .kcard h2.ct', place: 'right', title: 'Stage 2 · What each column means', text: 'The app has guessed the response and the factors from the column names. Check them, and read the design check on the right.', before: function () { return click([P + 'load_example']); } },
      { t: ['button[id$="-to3"]'], place: 'top', title: 'Continue', text: 'The design check is green, so we can continue.' },
      { t: [P + 'title', '.tab-pane.active .kcard'], place: 'right', title: 'Stage 3 · Context and hypotheses', text: 'Name the experiment, the response and the factors; H0 and H1 are written for you.', before: function () { return click(['button[id$="-to3"]']); } },
      { t: [P + 'alpha'], place: 'left', title: 'Significance level', text: 'α = 0.05 is the usual choice.' },
      { t: P + 'run', place: 'top', title: 'Run analysis', text: 'This runs the test, the assumption checks and the post-hoc comparisons together.' },
      { t: [P + 'results h2.ct', '.tab-pane.active h2.ct'], place: 'bottom', title: 'Stage 4 · The result', text: 'The headline gives the conclusion in plain words. Below it are the evidence and a written interpretation you can copy.', before: function () { return click([P + 'run']).then(function () { return wait(2200); }); } },
      { t: [P + 'rtabbar', P + 'test'], place: 'bottom', title: 'Explore the tabs', text: 'Open <b>Assumptions</b> to see whether the test can be trusted, and <b>Post-hoc</b> to see which groups differ.' },
      { t: [P + 'pin'], place: 'bottom', title: 'Pin it', text: 'Pin the result to build your report. That is the whole workflow. Try it with your own data next.' }
    ];
    return s;
  }

  // ---------- Problem Sets ----------
  function psListTour() { return [
    { t: '.page-head', place: 'bottom', title: 'Problem Sets', text: 'Exercises from the course problem sheets with their full data, each solved with the step-by-step hypothesis-testing process.' },
    { t: '#problems-cat', place: 'bottom', title: 'Choose a chapter', text: '<b>Chapter 2</b> one- and two-sample problems, <b>Chapter 3</b> one-way ANOVA, <b>Chapter 4</b> blocked designs, <b>Chapter 5</b> factorial designs, or <b>Custom Problem</b> for your own.' },
    { t: '.ps-search', place: 'bottom', title: 'Search', text: 'Find a problem by its number, topic or test.' },
    { t: '.ps-filters', place: 'bottom', title: 'Filters and grouping', text: 'Narrow the list by <b>problem type</b>, <b>test type</b> (parametric or non-parametric) and <b>post-hoc</b> method, and choose how the problems are grouped. Click a group heading to collapse it.' },
    { t: '.ps-card', place: 'right', title: 'A problem card', text: 'The card shows the problem number, its title, the design, and tags for the test and post-hoc the steps will use. <b>Click a card to open the problem.</b>' },
    { t: '#problems-new_custom', place: 'bottom', title: 'Your own problem', text: 'Write or paste any problem, bring in its data and say what each column is. It follows the same steps.' },
    { title: 'Next: open a problem', text: 'Click a problem card, then press <b>Guided tour</b> again: it will walk you through the step you are on. Or choose <b>Step-by-step demo</b> from the Guided tour menu and I will do it for you.' }
  ]; }
  function psEditTour() { return [
    { t: '#problems-c_title', place: 'right', title: 'Custom problem', text: 'Give your problem a title.' },
    { t: '#problems-c_statement', place: 'right', title: 'The statement', text: 'Paste the full problem. Writing α (for example α = 0.01), μ0 = … or σ = … lets the steps fill those values in.' },
    { t: ['#problems-c_paste', '.gridbox', '#problems-c_src'], place: 'right', title: 'The data', text: 'Paste from Excel, upload a file, load a class example, or edit the grid. Wide tables (one column per group) can be stacked.' },
    { t: ['.tab-pane.active .kcard:has(#problems-c_response)', '#problems-c_response'], place: 'left', title: 'What each column is', text: 'Choose the <b>Response</b> (the number measured) and the <b>Group / Factor A</b>. Add <b>Factor B</b> (and C) for a factorial experiment, a <b>Block</b> for blocks or pairs, or <b>Row / Column / Greek letter</b> for Latin squares.' },
    { t: '#problems-c_check', place: 'left', title: 'Data checks', text: 'The app tells you which layout it found (one sample, two samples, k samples, blocks, squares or factorial) and flags problems.' },
    { t: '#problems-c_go', place: 'top', title: 'Continue to the steps', text: 'Opens the step-by-step solution for your problem.' }
  ]; }
  // the parts of one step, scoped to the step on screen
  function psStepTour(n) {
    var A = ACT, multi = psMulti();
    var head = { t: A + '.ps-step-head', place: 'bottom', title: 'You are on this step', text: '' };
    var next = { t: A + '.ps-nav', place: 'top', title: 'Back and Next', text: 'Use <b>Next</b> to move on and <b>Back</b> to return. The last step shows “End of the analysis”. You can also click any dot in the progress bar.' };
    var bar = { t: '.ps-stepbar', place: 'bottom', title: 'Your progress', text: multi ? 'Each dot is a step. Phase 1 (parametric) runs first; Phase 2 (non-parametric) opens only if an assumption fails in step 8. Click a dot to jump to a step you have already seen.' : 'Each dot is a step. A tick means you have been there; click any dot to jump to it.' };
    var T_ = {
      1: [ { t: A + '.ps-statement', place: 'right', title: 'Step 1 · Problem and data', text: 'Read the question exactly as it appears in the book, including the parts (a), (b), (c).' },
           { t: A + '.ps-data', place: 'left', title: 'The data', text: 'The complete dataset as printed. <b>CSV</b> downloads it, and <b>Copy to Custom Problem</b> lets you change it and solve a variation.' }, bar, next ],
      2: [ { t: [A + '.ps-hblock', A + '.ps-sym'], place: 'right', title: 'Step 2 · Hypotheses', text: 'H0 is the “nothing happens” claim; H1 is what the question wants to show. The chips show them in symbols. For a factorial design there is a pair for every factor and every interaction.' },
           { t: [A + '.ps-hyp'], place: 'right', title: 'Edit the wording', text: 'The app writes H0 and H1 from the problem; you can edit them in your own words. <b>Restore the suggested wording</b> undoes your changes.' },
           { t: [A + 'select[id^="problems-alt_"]'], opt: true, place: 'bottom', title: 'Type of alternative', text: 'For one- and two-sample problems choose two-sided (≠), right-tailed (>) or left-tailed (<) and the hypothesised value.' }, next ],
      3: [ { t: [A + '.ps-alpha', A + '.ps-step-body'], place: 'right', title: 'Step 3 · Significance level α', text: 'α is the chance of wrongly rejecting H0 that you accept. It is filled in from the problem when it states one, otherwise 0.05. The confidence level is 1 − α.' },
           { t: [A + '.btn-k.bsm', A + '.ps-alpha .col'], place: 'left', title: 'Quick choices', text: 'Click 0.01, 0.05 or 0.10, or type your own value.' }, next ],
      4: [ { t: [A + '.grid.g3', A + '.ps-info'], place: 'bottom', title: 'Step 4 · Information available', text: 'Collect what is known: the number and size of samples, whether they are independent or paired, whether σ is known, and whether the population is normal. For one- and two-sample problems you can set the test type and σ here.' },
           { t: [A + '.ps-card-s', A + '.ps-info .kv-row'], place: 'right', title: 'Checks and facts', text: 'The facts are listed, and the checks the app runs at this stage (normality, independence, equal variances) are shown with their p-values.' },
           { t: [A + '.ps-data', A + '.tscroll'], place: 'top', title: 'The data again', text: 'Shown here so you can look at it while you decide.' },
           { t: [A + '.ps-verdict', A + '.ps-plan'], place: 'top', title: 'The conclusion of this step', text: multi ? 'The app states which parametric test it will run first and the plan that follows: post-hoc if H0 is rejected, then the assumption checks, then a rank test only if one fails.' : 'The app decides between a parametric and a non-parametric test from the information, and names the test.' }, next ],
      5: [ { t: [A + '.ps-fixed', A + 'select', A + '.hrow'], place: 'bottom', title: 'Step 5 · The test', text: multi ? 'The test is fixed: the parametric test chosen in step 4 always runs first.' : 'The test suggested in step 4 is selected (marked ★). You can choose another if you have a reason to.' },
           { t: [A + 'button[id^="problems-run_"]'], place: 'left', title: 'Run test', text: 'Press this to compute the test statistic and p-value. Going to the next step runs it for you if you forget.' },
           { t: [A + '.ps-tile'], place: 'bottom', title: 'The result at a glance', text: 'The test, its statistic, the p-value and an effect size or α.' },
           { t: [A + '.am-formula'], place: 'top', title: 'How the statistic is computed', text: 'The calculation worked out with your numbers, so you can follow it by hand.' },
           { t: [A + '.tscroll'], place: 'top', title: 'Tables', text: 'The ANOVA table (for ANOVA tests) and the result table with the statistic, degrees of freedom and p-value.' }, next ],
      6: [ { t: [A + '.ps-decision'], place: 'bottom', title: 'Step 6 · Decision', text: 'The verdict in one sentence: reject H0 or fail to reject it, in the words of the problem.' },
           { t: [A + '.ps-method'], place: 'bottom', title: 'Three ways to decide', text: '<b>Rejection region</b> (the statistic against the critical value), <b>confidence interval</b> (does it exclude the H0 value?) and <b>p-value</b> (below α?). They normally agree.' },
           { t: [A + 'select[id^="problems-eff6_"]'], opt: true, place: 'bottom', title: 'Factorial designs: one effect at a time', text: 'With several effects, the table lists them all and you choose one to see its decision, its plots and an effect plot (parallel lines mean no interaction).' }, next ],
      7: [ { t: [A + 'select[id^="problems-ph_"]', A + '.ps-step-body'], place: 'bottom', title: 'Step 7 · Post-hoc', text: 'Only when H0 is rejected: find which groups differ. Choose a method (Tukey HSD, Fisher LSD, Bonferroni, Scheffé; Dunnett only if the problem names a control).' },
           { t: [A + 'select[id^="problems-eff7_"]'], opt: true, place: 'bottom', title: 'Which effect', text: 'In a factorial design choose the significant effect. A significant interaction is compared within each level of the other factor (simple effects).' },
           { t: [A + '.tscroll', A + '.grid.g2'], place: 'top', title: 'Pairwise comparisons and grouping', text: 'Each pair with its difference, interval and adjusted p-value. Groups that share a letter are not significantly different.' }, next ],
      8: [ { t: [A + '.ps-asm'], place: 'bottom', title: 'Step 8 · Assumption checks', text: 'The ANOVA is trustworthy only if the residuals are <b>normal</b> (Shapiro-Wilk), have <b>constant variance</b> (Breusch-Pagan) and are <b>independent</b> (Breusch-Godfrey). Each check shows its test, its p-value and a plot.' },
           { t: [A + '.ps-final', A + '.ps-switch'], place: 'top', title: 'What happens next', text: 'If all three hold, the analysis ends here with the conclusion. If one fails, the app explains why the ANOVA cannot be trusted and starts the process again with the rank test.' }, next ],
      9: [ { t: [A + '.ps-process'], place: 'bottom', title: 'Step 2 again · Non-parametric hypotheses', text: 'An assumption failed, so we start again with a test that needs fewer assumptions. The hypotheses are now about <b>medians (η)</b> instead of means.' },
           { t: [A + '.ps-hyp', A + '.ps-hblock'], place: 'right', title: 'Edit if needed', text: 'The wording is suggested for you; change it if you wish.' }, next ],
      10: [ { t: [A + '.ps-alpha'], place: 'bottom', title: 'Step 3 again · Significance level', text: 'The same α is used for both analyses. Change it in step 3 if you need to.' }, next ],
      11: [ { t: [A + '.ps-info'], place: 'bottom', title: 'Step 4 again · Information', text: 'The facts, the data, and a summary of the ANOVA assumption checks that failed.' },
            { t: [A + '.ps-verdict'], place: 'top', title: 'Why this rank test', text: 'The app names the rank test and why it is the right one for this design.' }, next ],
      12: [ { t: [A + 'button[id^="problems-run_np_"]'], place: 'left', title: 'Step 5 again · The rank test', text: 'Run the rank test: Kruskal-Wallis for groups, Friedman for blocks, or the rank test on residuals for squares. Its statistic and p-value appear below with the ranks.' },
            { t: [A + '.tscroll'], place: 'top', title: 'Ranks', text: 'The mean ranks show which groups tend to be higher.' }, next ],
      13: [ { t: [A + '.ps-decision'], place: 'bottom', title: 'Step 6 again · Decision', text: 'The decision for the rank test, by the rejection region and the p-value (a rank test has no confidence interval).' },
            { t: [A + '.ps-final'], opt: true, place: 'top', title: 'Final conclusion', text: 'If H0 is not rejected, the analysis ends here with the final conclusion.' }, next ],
      14: [ { t: [A + '.tscroll', A + '.ps-step-body'], place: 'top', title: 'Step 7 again · Post-hoc', text: 'Dunn, Nemenyi or Wilcoxon comparisons show which groups differ.' },
            { t: [A + '.ps-final'], opt: true, place: 'top', title: 'Final conclusion', text: 'The conclusion of the whole analysis: the rank test result, with a note that the ANOVA was set aside and why.' } ]
    };
    var s = (T_[n] || []).slice();
    return s;
  }
  function psDetailTour() { return [bar0()].concat(psStepTour(psStep())); function bar0() { return { t: '.ps-step-head', place: 'bottom', title: 'Step ' + psStep() + ' of the analysis', text: 'This tour explains the step you are on. Use the Guided tour button on any other step to see that one.' }; } }
  function psDemo() {
    var s = [{ title: 'Step-by-step demo', text: 'I will open <b>Problem 3.11</b> (the tensile strength of Portland cement) and walk through every step of its solution, moving the steps for you. Press <b>Next</b> after reading each box.' }];
    for (var n = 1; n <= 8; n++) (function (n) {
      // the demo problem is a one-way ANOVA, so the steps that belong to other problem types are left out
      var parts = psStepTour(n).filter(function (x) { return x.title !== 'Back and Next' && !x.opt; });
      if (parts.length) { var f = parts[0]; f.before = function () { return (n === 1 ? nav('problems').then(function () { shinySet('problems-pick', '3.11'); return wait(2800); }) : Promise.resolve()).then(function () { return psGo(n); }); }; }
      if (n === 5) parts.forEach(function (x) { if (x.title === 'The result at a glance') {
        x.title = 'The test has run'; x.text = 'The demo pressed <b>Run test</b> for you. Here F = 12.73 and p = 0.0005: a small p-value, so the mixing techniques seem to differ. The next boxes show how the statistic is built and the tables.';
        x.before = function () { return click([ACT + 'button[id^="problems-run_"]']); }; } });
      s = s.concat(parts);
    })(n);
    s.push({ t: '.ps-stepbar', place: 'bottom', title: 'If an assumption fails', text: 'Here all three assumptions held, so the analysis ended at step 8. When one fails (try Problem 3.10 or 3.32), the dimmed <b>Non-parametric</b> phase lights up and the process restarts with medians (η), a rank test, its decision and a post-hoc.' });
    s.push({ title: 'Your turn', text: 'Pick any problem from the list. Press <b>Guided tour</b> on any step to get an explanation of that step.', before: function () { shinySet('problems-back', Math.random()); return wait(600); } });
    return s;
  }

  // ---------- choose the tour for where the student is ----------
  function pageTour() {
    var pg = page();
    if (pg === 'problems') { var v = psView(); return v === 'detail' ? psDetailTour() : v === 'edit' ? psEditTour() : psListTour(); }
    if (WIZ.indexOf(pg) >= 0) return wizardTour(pg);
    return (PAGE_TOURS[pg] || function () { return overview().slice(0, 1); })();
  }
  function demoTour() { var pg = page(); return pg === 'problems' ? psDemo() : WIZ.indexOf(pg) >= 0 ? wizardDemo(pg) : null; }

  // ---------- the overlay ----------
  var el = {};
  function build() {
    if (el.layer) return;
    var d = document.createElement('div'); d.id = 'tour-layer'; d.className = 'tour-layer'; d.setAttribute('aria-live', 'polite');
    d.innerHTML = '<div class="tour-block"></div><div class="tour-hl"></div>' +
      '<div class="tour-card" role="dialog" aria-label="Guided tour"><button class="tour-x" title="Close the tour (Esc)" aria-label="Close the tour">×</button>' +
      '<div class="tour-count"></div><h3 class="tour-title"></h3><div class="tour-text"></div>' +
      '<div class="tour-foot"><div class="tour-dots"></div><div class="tour-btns"><button class="btn-k bs bsm tour-back">Back</button><button class="btn-k bp bsm tour-next">Next</button></div></div></div>';
    document.body.appendChild(d);
    el.layer = d; el.hl = d.querySelector('.tour-hl'); el.card = d.querySelector('.tour-card'); el.title = d.querySelector('.tour-title');
    el.text = d.querySelector('.tour-text'); el.count = d.querySelector('.tour-count'); el.dots = d.querySelector('.tour-dots');
    el.back = d.querySelector('.tour-back'); el.next = d.querySelector('.tour-next');
    d.querySelector('.tour-x').onclick = T.end; d.querySelector('.tour-block').onclick = function () {};
    el.back.onclick = function () { T.go(S.i - 1, -1); }; el.next.onclick = function () { S.i >= S.steps.length - 1 ? T.end(true) : T.go(S.i + 1, 1); };
  }
  function place(target, pref) {
    var vw = window.innerWidth, vh = window.innerHeight, cw = el.card.offsetWidth, ch = el.card.offsetHeight, m = 14, x, y;
    if (!target) { el.hl.style.display = 'none'; el.card.style.left = Math.max(12, (vw - cw) / 2) + 'px'; el.card.style.top = Math.max(12, (vh - ch) / 2.4) + 'px'; el.card.classList.add('center'); return; }
    el.card.classList.remove('center');
    var r = target.getBoundingClientRect(), pad = 6;
    el.hl.style.display = 'block';
    el.hl.style.left = (r.left - pad) + 'px'; el.hl.style.top = (r.top - pad) + 'px'; el.hl.style.width = (r.width + 2 * pad) + 'px'; el.hl.style.height = (r.height + 2 * pad) + 'px';
    var order = { bottom: ['bottom', 'top', 'right', 'left'], top: ['top', 'bottom', 'right', 'left'], right: ['right', 'left', 'bottom', 'top'], left: ['left', 'right', 'bottom', 'top'] }[pref || 'bottom'];
    var spots = {
      bottom: function () { return { x: r.left + r.width / 2 - cw / 2, y: r.bottom + pad + m, ok: r.bottom + pad + m + ch < vh }; },
      top: function () { return { x: r.left + r.width / 2 - cw / 2, y: r.top - pad - m - ch, ok: r.top - pad - m - ch > 8 }; },
      right: function () { return { x: r.right + pad + m, y: r.top + r.height / 2 - ch / 2, ok: r.right + pad + m + cw < vw }; },
      left: function () { return { x: r.left - pad - m - cw, y: r.top + r.height / 2 - ch / 2, ok: r.left - pad - m - cw > 8 }; } };
    var pick = null; for (var k = 0; k < order.length; k++) { var c = spots[order[k]](); if (c.ok) { pick = c; break; } }
    if (!pick) { pick = { x: r.left + r.width / 2 - cw / 2, y: Math.min(vh - ch - 12, r.bottom + 12) }; if (pick.y < r.top && r.height > vh * .5) pick.y = vh - ch - 12; }
    x = Math.max(12, Math.min(vw - cw - 12, pick.x)); y = Math.max(12, Math.min(vh - ch - 12, pick.y));
    el.card.style.left = x + 'px'; el.card.style.top = y + 'px';
  }
  function layout() {
    if (!S.on) return; var s = S.steps[S.i]; var t = s && s.t ? find(s.t) : null; place(t, s && s.place);
  }
  T.go = function (i, dir) {
    if (!S.on || i < 0 || i >= S.steps.length) return;
    var token = ++S.token, s = S.steps[i], prev = S.i; S.i = i; dir = dir || (i >= prev ? 1 : -1);
    el.card.classList.add('busy');
    Promise.resolve(s.before ? s.before() : null).then(function () { return wait(120); }).then(function () {
      if (token !== S.token) return;
      var t = s.t ? find(s.t) : null;
      // a step marked optional only applies to some problems: skip it when its box is not on screen
      if (s.opt && !t) { var nx = i + dir; if (nx >= 0 && nx < S.steps.length) return T.go(nx, dir); if (dir > 0) return T.end(); return T.go(Math.max(0, i - 1), 1); }
      el.count.textContent = 'Guided tour · step ' + (i + 1) + ' of ' + S.steps.length;
      el.title.textContent = s.title || '';
      el.text.innerHTML = (s.text || '') + (s.t && !t ? '<div class="tour-miss">This part is not on screen yet. It appears once you reach that point.</div>' : '');
      el.back.style.visibility = i === 0 ? 'hidden' : 'visible'; el.next.textContent = i === S.steps.length - 1 ? 'Finish' : 'Next';
      el.dots.innerHTML = ''; var n = S.steps.length, lo = Math.max(0, Math.min(i - 5, n - 11)), hi = Math.min(n, lo + 11);
      for (var k = lo; k < hi; k++) { var b = document.createElement('i'); if (k === i) b.className = 'on'; else if (k < i) b.className = 'done'; el.dots.appendChild(b); }
      el.card.classList.remove('busy');
      if (t) {
        var r0 = t.getBoundingClientRect();
        if (r0.top < 70 || r0.bottom > window.innerHeight - 20) { t.scrollIntoView({ block: r0.height > window.innerHeight * .7 ? 'start' : 'center', behavior: 'auto' }); if (r0.height > window.innerHeight * .7) window.scrollBy(0, -90); }
      }
      return wait(60).then(function () { if (token === S.token) place(t, s.place); el.next.focus({ preventScroll: true }); });
    });
  };
  T.start = function (steps) {
    if (!steps || !steps.length) return;
    // optional steps that do not apply to what is on screen (and need no action first) are left out, so the count is right
    steps = steps.filter(function (x) { return !(x.opt && x.t && !x.before && !find(x.t)); });
    if (!steps.length) return;
    T.closeMenu(); build(); S.steps = steps; S.on = true; S.i = 0; S.token++;
    el.layer.classList.add('on'); document.body.classList.add('touring');
    try { localStorage.setItem('sds_tour_seen', '1'); } catch (e) { /* private window */ }
    T.nudgeOff(); T.go(0);
  };
  T.end = function () { S.on = false; S.token++; if (el.layer) el.layer.classList.remove('on'); document.body.classList.remove('touring'); };
  T.page = function () { T.start(pageTour()); };
  T.overview = function () { T.start(overview()); };
  T.demo = function () { var d = demoTour(); if (d) T.start(d); };

  // ---------- the launcher menu under the button ----------
  T.menu = function () { S.menuOpen ? T.closeMenu() : openMenu(); };
  T.closeMenu = function () { var m = document.getElementById('tour-menu'); if (m) m.remove(); S.menuOpen = false; };
  function openMenu() {
    T.nudgeOff(); var b = document.getElementById('tour-btn'); if (!b) return;
    var pg = page(), nm = PAGE_NAME[pg] || 'this page', demo = demoTour !== null && (pg === 'problems' || WIZ.indexOf(pg) >= 0);
    var m = document.createElement('div'); m.id = 'tour-menu'; m.className = 'tour-menu';
    var detail = pg === 'problems' && psView() === 'detail';
    m.innerHTML = '<div class="tour-menu-h">Guided tour</div>' +
      '<button data-a="page"><b>' + (detail ? 'Explain this step' : 'Tour of ' + nm) + '</b><span>' + (detail ? 'What to look at and do on step ' + psStep() + ' of the analysis' : 'Boxes around every part of this page, one at a time') + '</span></button>' +
      (demo ? '<button data-a="demo"><b>' + (pg === 'problems' ? 'Step-by-step demo' : 'Hands-on walk-through') + '</b><span>' + (pg === 'problems' ? 'Opens Problem 3.11 and walks through every step for you' : 'Loads the class example and goes through all four stages for you') + '</span></button>' : '') +
      '<button data-a="overview"><b>Overview of the whole app</b><span>The sidebar, search, reports and the assistant</span></button>' +
      '<div class="tour-menu-f">Tip: use the ← → keys, and Esc to close.</div>';
    document.body.appendChild(m); S.menuOpen = true;
    var r = b.getBoundingClientRect(); m.style.top = (r.bottom + 8) + 'px'; m.style.left = Math.max(10, Math.min(window.innerWidth - m.offsetWidth - 10, r.right - m.offsetWidth)) + 'px';
    m.addEventListener('click', function (e) { var a = e.target.closest('button[data-a]'); if (!a) return; var k = a.getAttribute('data-a'); k === 'page' ? T.page() : k === 'demo' ? T.demo() : T.overview(); });
  }
  document.addEventListener('click', function (e) { if (S.menuOpen && !e.target.closest('#tour-menu') && !e.target.closest('#tour-btn')) T.closeMenu(); });

  // ---------- first-visit nudge ----------
  T.nudgeOff = function () { var n = document.getElementById('tour-nudge'); if (n) n.remove(); var b = document.getElementById('tour-btn'); if (b) b.classList.remove('pulse'); };
  function nudge() {
    var seen = false; try { seen = localStorage.getItem('sds_tour_seen') === '1'; } catch (e) { /* ignore */ }
    var b = document.getElementById('tour-btn'); if (seen || !b) return;
    b.classList.add('pulse');
    var n = document.createElement('div'); n.id = 'tour-nudge'; n.className = 'tour-nudge';
    n.innerHTML = '<b>New here?</b> Take a quick guided tour: boxes show you each part of the app, one step at a time.' +
      '<div><button class="btn-k bp bsm" data-a="go">Start the tour</button><button class="btn-k bs bsm" data-a="no">Not now</button></div>';
    document.body.appendChild(n);
    var r = b.getBoundingClientRect(); n.style.top = (r.bottom + 10) + 'px'; n.style.left = Math.max(10, Math.min(window.innerWidth - n.offsetWidth - 10, r.right - n.offsetWidth)) + 'px';
    n.addEventListener('click', function (e) { var a = e.target.closest('button[data-a]'); if (!a) return;
      if (a.getAttribute('data-a') === 'go') T.overview(); else { try { localStorage.setItem('sds_tour_seen', '1'); } catch (x) { /* ignore */ } T.nudgeOff(); } });
  }

  // ---------- keyboard and layout ----------
  document.addEventListener('keydown', function (e) {
    if (!S.on) return;
    if (e.key === 'Escape') { T.end(); e.preventDefault(); }
    else if (e.key === 'ArrowRight' || e.key === 'Enter') { if (e.target && /INPUT|TEXTAREA|SELECT/.test(e.target.tagName)) return; S.i >= S.steps.length - 1 ? T.end() : T.go(S.i + 1, 1); e.preventDefault(); }
    else if (e.key === 'ArrowLeft') { T.go(S.i - 1, -1); e.preventDefault(); }
  });
  var tick = 0;
  function relayout() { if (tick) return; tick = requestAnimationFrame(function () { tick = 0; layout(); }); }
  window.addEventListener('resize', relayout); window.addEventListener('scroll', relayout, true);
  window.addEventListener('load', function () { setTimeout(nudge, 2500); });
})();
