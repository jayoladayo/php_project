# Arabic: assorted labels, the assistant context labels, page help and the test-selector suffix.

ar_part_misc <- function() {
ar_add(
  "KFUPM · Experimental Design · Worked solution" = "جامعة الملك فهد للبترول والمعادن · التصميم التجريبي · حل مفصَّل",
  "What" = "ماذا", "Conclusion" = "الاستنتاج", "Step" = "الخطوة",
  "home" = "الرئيسية", "descriptive statistics" = "الإحصاء الوصفي", "test advisor" = "مرشد الاختبارات", "data studio" = "استوديو البيانات", "design planner" = "مخطط التصميم", "learn" = "تعلّم",
  "pairs" = "أزواج", "treatments" = "معالجات", "comparisons" = "مقارنات", "levels" = "مستويات", "(none)" = "(لا شيء)",
  "Search analyses, tests, help" = "ابحث في التحليلات والاختبارات والمساعدة", "Close" = "إغلاق", "About StatDesign Studio" = "حول ستات ديزاين ستوديو",
  "A no-code tool for the KFUPM Experimental Design course. Bring your data into Data Studio, choose a design, map your columns, and run the analysis." = "أداة بلا برمجة لمقرر التصميم التجريبي في جامعة الملك فهد للبترول والمعادن. أدخل بياناتك إلى استوديو البيانات، واختر تصميمًا، وحدّد أعمدتك، وشغّل التحليل.",
  "Data Studio: upload Excel or CSV, pick the sheet, or type and paste data." = "استوديو البيانات: ارفع ملف Excel أو CSV، واختر الورقة، أو اكتب البيانات أو الصقها.",
  "Choose an analysis from the sidebar (One sample, Two samples, k samples, CRD, RCBD, Latin Square, Graeco-Latin Square or Factorial)." = "اختر تحليلًا من الشريط الجانبي (عيّنة واحدة، أو عيّنتان، أو k عيّنات، أو CRD، أو RCBD، أو المربع اللاتيني، أو المربع اللاتيني اليوناني، أو التصميم العاملي).",
  "Pick the dataset and say which column is the response, the treatment and the blocks." = "اختر مجموعة البيانات وحدّد أي عمود هو الاستجابة وأيها المعالجة وأيها القطاعات.",
  "Follow the five steps: hypotheses, significance level, information available, test and decision (post-hoc and assumption checks for designs with three or more treatments)." = "اتبع الخطوات الخمس: الفرضيات، ومستوى الدلالة، والمعلومات المتاحة، والاختبار والقرار (مع الاختبارات البعدية وفحص الافتراضات للتصاميم ذات ثلاث معالجات أو أكثر).",
  "If an ANOVA assumption fails, the process restarts with the rank test: Kruskal-Wallis, Friedman or Quade's test." = "إذا فشل أحد افتراضات تحليل التباين تبدأ العملية من جديد باختبار الرتب: كروسكال-واليس أو فريدمان أو كويد.",
  "Every page has a Download solution button: a worked PDF of the steps." = "في كل صفحة زر «تنزيل الحل»: ملف PDF مفصَّل بالخطوات.",
  "Not sure which test? The Test Advisor asks a few questions and opens the right page." = "لا تعرف أي اختبار تختار؟ يطرح مرشد الاختبارات بضعة أسئلة ويفتح الصفحة المناسبة.",
  "Save project downloads a file with your datasets and settings. Open it later from Data Studio, Open project." = "ينزّل «حفظ المشروع» ملفًا فيه مجموعات بياناتك وإعداداتك. افتحه لاحقًا من استوديو البيانات ← فتح مشروع.",
  "Value (value)" = "القيمة (value)", "Google Gemini" = "Google Gemini", "(none)" = "(لا شيء)", "Greek" = "اليوناني", "Factors" = "العوامل",
  "Variety A" = "الصنف A", "Variety B" = "الصنف B", "Variety C" = "الصنف C", "Variety D" = "الصنف D",
  "2 per combination (balanced)" = "2 لكل توليفة (متوازن)", "A, B, C, AB, AC, BC, ABC" = "A وB وC وAB وAC وBC وABC", "ABC:" = "ABC:",
  "Mean difference" = "فرق المتوسطين"
)
ar_tpl(
  "Step %d · %s" = "الخطوة $1 · $2",
  "%s ★ suggested" = "$1 ★ مقترح",
  "Can see: %s" = "يرى: $1", "Can see: Problem %s, %s" = "يرى: المسألة $1، $2", "Can see: %s, %s" = "يرى: $1، $2", "Can see: the %s page" = "يرى: صفحة $1",
  "%s · your own data" = "$1 · بياناتك الخاصة",
  "Parametric test, so the hypotheses are about the mean μ. %s" = "اختبار معلمي، لذا تتعلق الفرضيات بالمتوسط μ. $1",
  "Non-parametric test, so the hypotheses are about the median η. %s" = "اختبار لامعلمي، لذا تتعلق الفرضيات بالوسيط η. $1",
  "With three or more treatments H1 is always “at least one differs”. %s" = "مع ثلاث معالجات أو أكثر تكون H1 دائمًا «تختلف إحداها على الأقل». $1",
  "%s (%s)" = "$1 ($2)",
  "Plot %s · %s" = "القطعة $1 · $2",
  "Mean = %s" = "المتوسط = $1", "Partial η² = %s" = "η² الجزئي = $1", "Mean difference = %s" = "فرق المتوسطين = $1",
  "%s: %d of %d %s differ at α = %s. %s" = "$1: تختلف $2 من $3 $4 عند α = $5. $6",
  "%d of %d comparisons differ at α = %s.%s" = "تختلف $1 من $2 مقارنات عند α = $3.$4",
  "%d of %d comparisons differ at α = %s." = "تختلف $1 من $2 مقارنات عند α = $3.",
  "%s %d of %d pairs different." = "$1 وجد أن $2 من $3 أزواج مختلفة.",
  "%s = %s" = "$1 = $2",
  "%s: %d levels (%s)" = "$1: $2 مستويات ($3)",
  "%s: %d levels (%s); %s: %d levels (%s)" = "$1: $2 مستويات ($3)؛ $4: $5 مستويات ($6)",
  "%d per combination (balanced)" = "$1 لكل توليفة (متوازن)",
  "%s compared within each combination of %s" = "مقارنة $1 داخل كل توليفة من $2",
  "%s is significant, so its main effects cannot be read alone. Each factor is compared within every level of the other factor (simple effects)." = "$1 دالّ، فلا يمكن قراءة آثاره الرئيسية منفردة. يُقارن كل عامل داخل كل مستوى من العامل الآخر (الآثار البسيطة).",
  "%s affects the median %s: at least one level of %s gives a different median." = "$1 يؤثر في الوسيط $2: مستوى واحد على الأقل من $3 يعطي وسيطًا مختلفًا.",
  "%s affects the mean %s: at least one level of %s gives a different mean." = "$1 يؤثر في المتوسط $2: مستوى واحد على الأقل من $3 يعطي متوسطًا مختلفًا.",
  "%s has no effect: the median %s is the same at every level of %s (%s)." = "ليس لـ$1 أثر: الوسيط $2 هو نفسه عند كل مستوى من $3 ($4).",
  "%s has no effect: the mean %s is the same at every level of %s (%s)." = "ليس لـ$1 أثر: المتوسط $2 هو نفسه عند كل مستوى من $3 ($4).",
  "%s and %s interact: the effect of %s on the median %s depends on the level of %s." = "$1 و$2 يتفاعلان: يعتمد أثر $3 في الوسيط $4 على مستوى $5.",
  "%s and %s interact: the effect of %s on the mean %s depends on the level of %s." = "$1 و$2 يتفاعلان: يعتمد أثر $3 في المتوسط $4 على مستوى $5.",
  "The mean %s differs between %s and %s." = "يختلف المتوسط $1 بين $2 و$3.",
  "The median %s differs between %s and %s." = "يختلف الوسيط $1 بين $2 و$3.",
  "The %s %s is the same for all %d %s." = "$1 $2 هو نفسه في جميع الـ$3 ($4).",
  "The %s of %s is %s." = "$1 لـ$2 هو $3.",
  "%s %s (%s)" = "$1 $2 ($3)"
)
# "Test name: stat = value, p = value." (the message after a test is run)
ar_rx("([^:]+): ([^:=,]+) = (-?[0-9.]+(?:e-?[0-9]+)?), (p [=<>≥] [-0-9.e ]+)\\.", "$1: $2 = $3، $4.")
# The rejection-region sentence of the decision step.
ar_rx("Reject H0 if (.+?)\\. (The test statistic is|The .+? gives the standard normal equivalent) (\\S+) = (-?[0-9.e]+), which (falls in|is not in) the rejection region\\.", "نرفض H0 إذا $1. $2 $3 = $4، وهي $5 منطقة الرفض.")
}
