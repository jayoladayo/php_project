# Arabic: assorted labels, the assistant context labels, page help and the test-selector suffix.

ar_part_misc <- function() {
ar_add("Skip to the content" = "تخطَّ إلى المحتوى", "Open the menu" = "افتح القائمة", "Main menu" = "القائمة الرئيسية")

ar_add(
  "No data yet? Start here" = "لا توجد بيانات بعد؟ ابدأ من هنا", "Your data stay in this session and are shared by every analysis." = "تبقى بياناتك في هذه الجلسة وتُستخدم في كل تحليل.",
  "Try a class example" = "جرّب مثالًا من الحصة", "One click loads a real dataset from the course." = "نقرة واحدة تحمّل مجموعة بيانات حقيقية من المقرر.",
  "Paste from Excel" = "الصق من إكسل", "Copy the cells, including the header row." = "انسخ الخلايا مع صف العناوين.",
  "Type the values" = "اكتب القيم", "A small table you fill in by hand." = "جدول صغير تملؤه يدويًا.",
  "After that, pick an analysis in the menu. It reads your columns and walks you through the 5 steps." = "بعد ذلك اختر تحليلًا من القائمة. يقرأ أعمدتك ويرشدك عبر الخطوات الخمس."
)

ar_add(
  "Your data" = "بياناتك", "Choose where the data come from, then check the table." = "اختر مصدر البيانات ثم راجع الجدول.",
  "Your question (optional)" = "سؤالك (اختياري)", "A title and the wording of the problem. Writing α = 0.01, μ0 = … or σ = … fills those settings in." = "عنوان وصياغة المسألة. وكتابة α = 0.01 أو μ0 = … أو σ = … تملأ تلك الإعدادات.",
  "What happens next" = "ما يحدث بعد ذلك", "You are guided through the 5 steps, then the post-hoc comparisons and the assumption checks." = "ستُرشَد عبر الخطوات الخمس، ثم المقارنات البعدية وفحص الافتراضات."
)

ar_add(
  "5-step process" = "عملية الخطوات الخمس", "show the calculation" = "اعرض الحساب"
)

ar_add(
  "Hypotheses" = "الفرضيات", "Significance level" = "مستوى المعنوية", "Information available" = "المعلومات المتاحة", "Test" = "الاختبار", "Decision" = "القرار",
  "State what you want to show" = "حدّد ما تريد إثباته", "How sure you need to be (α)" = "درجة التأكد المطلوبة (α)", "What the data and the design tell us" = "ما تخبرنا به البيانات والتصميم",
  "The right test, run for you" = "الاختبار المناسب، يُجرى لك", "Reject H0 or not, three ways" = "رفض H0 أو عدمه، بثلاث طرق",
  "Every analysis follows the same 5 steps" = "كل تحليل يتبع الخطوات الخمس نفسها", "The 5-step process" = "عملية الخطوات الخمس",
  "Bring your data, follow five clear steps, and finish with a decision you can explain and a report you can submit." = "أحضر بياناتك، واتبع خمس خطوات واضحة، وانتهِ بقرار تستطيع شرحه وتقرير تستطيع تسليمه.",
  "Practise with course problems" = "تدرّب على مسائل المقرر", "What would you like to do?" = "ماذا تريد أن تفعل؟", "Pick the closest match. You can always change your mind." = "اختر الأقرب. ويمكنك تغيير رأيك في أي وقت.",
  "Analyse my own data" = "حلّل بياناتي", "Upload or paste a table, say what each column is, and get the five steps on your data." = "ارفع جدولًا أو الصقه، وحدّد معنى كل عمود، لتحصل على الخطوات الخمس على بياناتك.", "Open Data Studio" = "افتح استوديو البيانات",
  "I am not sure which test" = "لست متأكدًا من الاختبار", "Answer three short questions and we open the right analysis with sensible settings." = "أجب عن ثلاثة أسئلة قصيرة لنفتح لك التحليل المناسب بإعدادات معقولة.", "Ask the Test Advisor" = "اسأل مرشد الاختبارات",
  "Practise with course problems" = "تدرّب على مسائل المقرر", "Open Problem Sets" = "افتح مجموعات المسائل",
  "Learn the methods" = "تعلّم الطرق", "When to use each design, the model, the assumptions and a worked example from class." = "متى تستخدم كل تصميم، والنموذج، والافتراضات، ومثال محلول من الحصة.", "Open Learn" = "افتح تعلّم",
  "Designed experiments" = "التجارب المصمَّمة", "ANOVA, assumption checks, post-hoc comparisons and the rank-test fallback, in five steps" = "تحليل التباين وفحص الافتراضات والمقارنات البعدية واختبار الرتب البديل، في خمس خطوات",
  "About this project" = "حول هذا المشروع", "The aim of the app and the team behind it" = "هدف التطبيق والفريق الذي يقف وراءه",
  "Completely Randomised (CRD)" = "العشوائي الكامل (CRD)", "Randomised Complete Block (RCBD)" = "القطاعات العشوائية الكاملة (RCBD)"
)
ar_tpl("Chapters 2 to 6 with their complete data, solved step by step (%d problems)." = "الفصول من 2 إلى 6 ببياناتها الكاملة، محلولة خطوة بخطوة ($1 مسألة).")

ar_add(
  "Start" = "ابدأ", "Practice" = "تدرّب", "5-step" = "5 خطوات", "Guided tour" = "جولة إرشادية",
  "Open a shared analysis" = "افتح تحليلًا مشاركًا", "Light or dark" = "فاتح أو داكن", "Light or dark theme" = "السمة الفاتحة أو الداكنة", "Help" = "مساعدة", "Language" = "اللغة",
  "Save your datasets and settings" = "احفظ بياناتك وإعداداتك"
)

ar_add(
  "Which effects go into the error?" = "أي الآثار تذهب إلى الخطأ؟",
  "With no replicate the error has to come from the effects that behave like noise. Tick the effects to put into the error: the ones sitting with the small points on the Q-Q plot. They are not tested in step 5; the others are tested against them. The ticks start from the plots' suggestion (Lenth's rule)." = "عند غياب التكرار يجب أن يأتي الخطأ من الآثار التي تسلك سلوك الضجيج. علّم الآثار التي تريد وضعها في الخطأ: تلك التي تقع مع النقاط الصغيرة في رسم Q-Q. لا تُختبر هذه في الخطوة 5؛ وتُختبر الآثار الأخرى مقابلها. تبدأ العلامات من اقتراح الرسمين (قاعدة لِنث).",
  "Use the plot's suggestion" = "استخدم اقتراح الرسم", "Pool the interactions of three or more factors" = "ادمج التفاعلات بين ثلاثة عوامل فأكثر",
  "There is one observation per combination, so there is no pure error. Every main effect and interaction is stated here; in step 4 you choose which effects go into the error (the plots show which ones stand out), and only the others are tested in step 5." = "توجد مشاهدة واحدة لكل توليفة فلا يوجد خطأ خالص. يُصاغ هنا كل أثر رئيسي وكل تفاعل؛ وفي الخطوة 4 تختار الآثار التي تذهب إلى الخطأ (تُظهر الرسوم أيها بارز)، ولا تُختبر في الخطوة 5 إلا الآثار الأخرى.",
  "With one observation per combination there is no pure error. In both plots the effects that stand away from the small ones are real, and the small ones behave like noise: those are the effects to put into the error. The first plot shows the signed effects with the line through the small ones; the Q-Q plot is the one used in class (absolute effects, every point labelled). Choose the effects for the error just below." = "عند وجود مشاهدة واحدة لكل توليفة لا يوجد خطأ خالص. في الرسمين تكون الآثار التي تبتعد عن الصغيرة حقيقية، وتسلك الصغيرة سلوك الضجيج: وهي الآثار التي توضع في الخطأ. يعرض الرسم الأول الآثار بإشاراتها مع الخط المار بالصغيرة؛ ورسم Q-Q هو المستخدم في الحصة (القيم المطلقة للآثار، وكل نقطة مسمّاة). اختر الآثار الخاصة بالخطأ أدناه مباشرة.",
  "1 observation per combination (unreplicated): there is no pure error, so some effects must be put into the error. Choose them below, from the plots." = "مشاهدة واحدة لكل توليفة (دون تكرار): لا يوجد خطأ خالص، لذلك يجب وضع بعض الآثار في الخطأ. اخترها أدناه من الرسوم.",
  "Effects put into the error:" = "الآثار الموضوعة في الخطأ:", "error df =" = "درجات حرية الخطأ ="
)
ar_tpl(
  "Into the error: %s (error df = %d). Tested in step 5: %s." = "في الخطأ: $1 (درجات حرية الخطأ = $2). المختبَرة في الخطوة 5: $3.",
  "There is one observation per combination, so there is no pure error. The error line pools the %d effects you put into the error in step 4 (%s), which are taken to be noise; every other effect is tested against it." = "توجد مشاهدة واحدة لكل توليفة فلا يوجد خطأ خالص. يدمج سطر الخطأ الآثار الـ$1 التي وضعتها في الخطأ في الخطوة 4 ($2) على أنها ضجيج؛ ويُختبر كل أثر آخر مقابله.",
  "y = μ + %s + (interactions) + ε: one observation per combination, so the effects put into the error (chosen below) form ε" = "y = μ + $1 + (التفاعلات) + ε: مشاهدة واحدة لكل توليفة، لذلك تشكّل الآثار الموضوعة في الخطأ (المختارة أدناه) ε",
  "One observation per combination: SS_E is the sum of squares of the %d effects put into the error (%s), on %d df." = "مشاهدة واحدة لكل توليفة: SS_E هو مجموع مربعات الآثار الـ$1 الموضوعة في الخطأ ($2)، على $3 درجة حرية."
)

ar_add(
  "Normal Q-Q plot of the effects (as in class)" = "الرسم الاحتمالي الطبيعي Q-Q للآثار (كما في الحصة)", "Theoretical quantiles" = "الكميات النظرية", "Sample quantiles (absolute effects)" = "كميات العيّنة (القيم المطلقة للآثار)",
  "Active effects stand away from the small ones · the rest go into the error" = "الآثار النشطة تبتعد عن الصغيرة · والباقي يذهب إلى الخطأ",
  "With one observation per combination there is no pure error. In both plots the effects that stand away from the small ones are real, and the small ones behave like noise: those are the interactions pooled into the error in step 5. The first plot shows the signed effects with the line through the small ones; the Q-Q plot below is the one used in class (absolute effects, every point labelled)." = "عند وجود مشاهدة واحدة لكل توليفة لا يوجد خطأ خالص. في الرسمين تكون الآثار التي تبتعد عن الصغيرة حقيقية، وتسلك الصغيرة سلوك الضجيج: وهي التفاعلات التي تُدمج في الخطأ في الخطوة 5. يعرض الرسم الأول الآثار بإشاراتها مع الخط المار بالصغيرة؛ أما رسم Q-Q أدناه فهو المستخدم في الحصة (القيم المطلقة للآثار، وكل نقطة مسمّاة).",
  "2^k Factorial" = "العاملي ‎2^k‎", "2² to 2⁵" = "من 2² إلى 2⁵", "Pooled error if unreplicated" = "خطأ مدمج عند غياب التكرار",
  "Two-level designs with two to five factors, replicated or with one observation per combination." = "تصاميم بمستويين وبعاملين إلى خمسة عوامل، مكرَّرة أو بمشاهدة واحدة لكل توليفة.",
  "2^k Factorial: two-level designs with two to five factors" = "العاملي 2^k: تصاميم بمستويين وبعاملين إلى خمسة عوامل",
  "Two-level factorial designs (2² to 2⁵): every factor and every interaction is tested, replicated or with one observation per combination, then the Kruskal-Wallis test if an assumption fails." = "التصاميم العاملية بمستويين (من 2² إلى 2⁵): يُختبر كل عامل وكل تفاعل، بتكرار أو بمشاهدة واحدة لكل توليفة، ثم اختبار كروسكال-واليس إذا فشل أحد الافتراضات.",
  "A 2^k design needs a Response and two to five factor columns, each with exactly two levels (low and high), with every combination of levels observed: once for an unreplicated design, or more than once for a replicated one." = "يحتاج تصميم 2^k إلى عمود استجابة وإلى عمودين حتى خمسة أعمدة للعوامل، لكل منها مستويان بالضبط (منخفض ومرتفع)، مع مشاهدة كل توليفة من المستويات: مرة واحدة في التصميم غير المكرَّر، أو أكثر من مرة في المكرَّر.",
  "Factor A (two levels)" = "العامل A (مستويان)", "Factor B (two levels)" = "العامل B (مستويان)", "Factor C (two levels, optional)" = "العامل C (مستويان، اختياري)",
  "Factor D (two levels, optional)" = "العامل D (مستويان، اختياري)", "Factor E (two levels, optional)" = "العامل E (مستويان، اختياري)",
  "Replication" = "التكرار", "All problems" = "كل المسائل", "Replicated" = "مكرَّرة", "No replicate" = "بلا تكرار", "Not a factorial design" = "ليس تصميمًا عامليًا",
  "No replicate (one observation per combination)" = "بلا تكرار (مشاهدة واحدة لكل توليفة)",
  "Every treatment combination is observed more than once, so every effect is tested against the pure error of the replicates." = "تُشاهَد كل توليفة من المعالجات أكثر من مرة، فيُختبر كل أثر مقابل الخطأ الخالص للمكررات.",
  "One observation per treatment combination: there is no pure error. With two factors the interaction is checked with Tukey's test for nonadditivity; with three or more factors the higher-order interactions are pooled into the error." = "مشاهدة واحدة لكل توليفة من المعالجات: لا يوجد خطأ خالص. ومع عاملين يُفحص التفاعل باختبار توكي لعدم الجمعية؛ ومع ثلاثة عوامل فأكثر تُدمج التفاعلات العليا في الخطأ.",
  "Filter by problem type, test type, post-hoc and (for factorial designs) replication, and choose how the problems are grouped. Click a heading to collapse it." = "رشّح حسب نوع المسألة ونوع الاختبار والاختبار البعدي و(للتصاميم العاملية) التكرار، واختر طريقة تجميع المسائل. انقر على عنوان لطيّه.",
  "This page analyses 2^k factorial designs, but at least one of the factors you chose has more than two levels." = "تحلل هذه الصفحة التصاميم العاملية 2^k، لكن لأحد العوامل التي اخترتها أكثر من مستويين.",
  "Use the Factorial page for factors with three or more levels." = "استخدم صفحة العاملي للعوامل ذات ثلاثة مستويات فأكثر."
)

ar_add(
  "Each sum of squares comes from the ANOVA decomposition of the effect means (the main effects and the interactions listed below); SS_E = SS_T − (the other sums of squares)" = "ينتج كل مجموع مربعات من تحليل التباين لمتوسطات الآثار (الآثار الرئيسية والتفاعلات المدرجة أدناه)؛ و SS_E = SS_T − (مجاميع المربعات الأخرى)",
  "Chapter 6" = "الفصل 6", "Four-factor factorial (CRD)" = "عاملي بأربعة عوامل (CRD)", "Five-factor factorial (CRD)" = "عاملي بخمسة عوامل (CRD)",
  "Four-factor ANOVA" = "تحليل تباين بأربعة عوامل", "Five-factor ANOVA" = "تحليل تباين بخمسة عوامل",
  "Four crossed factors (usually a 2⁴ design) in a completely randomized layout: four main effects and their interactions. With one observation per combination, the three- and four-factor interactions form the error." = "أربعة عوامل متقاطعة (عادةً تصميم 2⁴) بتوزيع عشوائي كامل: أربعة آثار رئيسية وتفاعلاتها. وعند وجود مشاهدة واحدة لكل توليفة تشكّل التفاعلات الثلاثية والرباعية الخطأ.",
  "Five crossed factors (a 2⁵ design) in a completely randomized layout. With one observation per combination, the three-factor and higher interactions form the error." = "خمسة عوامل متقاطعة (تصميم 2⁵) بتوزيع عشوائي كامل. وعند وجود مشاهدة واحدة لكل توليفة تشكّل التفاعلات الثلاثية فما فوق الخطأ.",
  "Factor C (three or more factors)" = "العامل C (ثلاثة عوامل أو أكثر)", "Factor D (four or five factors)" = "العامل D (أربعة أو خمسة عوامل)", "Factor E (five factors)" = "العامل E (خمسة عوامل)",
  "Factor D (four-factor)" = "العامل D (أربعة عوامل)", "Factor E (five-factor)" = "العامل E (خمسة عوامل)", "Factor D" = "العامل D", "Factor E" = "العامل E",
  "Effect estimates of the 2² design" = "تقديرات الآثار في التصميم 2²", "Effect estimates of the 2³ design" = "تقديرات الآثار في التصميم 2³", "Effect estimates of the 2⁴ design" = "تقديرات الآثار في التصميم 2⁴", "Effect estimates of the 2⁵ design" = "تقديرات الآثار في التصميم 2⁵",
  "Effect estimates of the 2^k design" = "تقديرات الآثار في التصميم 2^k", "Normal probability plot of the effects" = "الرسم الاحتمالي الطبيعي للآثار", "Effect estimate" = "تقدير الأثر", "% contribution" = "النسبة المئوية للمساهمة",
  "Effect estimate (coded −1, +1)" = "تقدير الأثر (بالترميز −1، +1)", "Normal score" = "الدرجة الطبيعية",
  "Dashed line: noise · Labelled points: effects that stand out (Lenth)" = "الخط المتقطع: الضجيج · النقاط المسمّاة: آثار بارزة (لِنث)",
  "Each effect is the average response at the high level minus the average at the low level (for an interaction, the same contrast of the product of the coded levels, −1 and +1). The sum of squares of an effect is N × (effect/2)², and the % contribution is its share of the total of all effects." = "الأثر هو متوسط الاستجابة عند المستوى المرتفع مطروحًا منه المتوسط عند المستوى المنخفض (وللتفاعل التباين نفسه لحاصل ضرب المستويات المرمَّزة −1 و+1). ومجموع مربعات الأثر هو N × (الأثر/2)²، والنسبة المئوية للمساهمة هي حصته من مجموع الآثار كلها.",
  "With one observation per combination there is no pure error. Effects that fall off the line through the small ones are real; the points near the line behave like noise, which is why the higher-order interactions can be pooled into the error in step 5." = "عند وجود مشاهدة واحدة لكل توليفة لا يوجد خطأ خالص. الآثار التي تبتعد عن الخط المار بالآثار الصغيرة حقيقية؛ أما النقاط القريبة من الخط فتسلك سلوك الضجيج، ولهذا يمكن دمج التفاعلات العليا في الخطأ في الخطوة 5.",
  "A quick look before the tests: effects far from the line stand out. Step 5 tests every effect against the pure error of the replicates." = "نظرة سريعة قبل الاختبارات: الآثار البعيدة عن الخط بارزة. وتختبر الخطوة 5 كل أثر مقابل الخطأ الخالص للمكررات.",
  "There is one observation per combination, so there is no pure error: the main effects and the two-factor interactions are tested, and the three-factor and higher interactions are pooled to form the error (step 5)." = "توجد مشاهدة واحدة لكل توليفة فلا يوجد خطأ خالص: تُختبر الآثار الرئيسية والتفاعلات الثنائية، وتُدمج التفاعلات الثلاثية فما فوق لتكوين الخطأ (الخطوة 5).",
  "1 observation per combination (unreplicated): there is no pure error, so the three-factor and higher interactions are pooled to estimate the error." = "مشاهدة واحدة لكل توليفة (دون تكرار): لا يوجد خطأ خالص، لذلك تُدمج التفاعلات الثلاثية فما فوق لتقدير الخطأ."
)
ar_tpl(
  "Factorial design in a completely randomized layout: the %d factors are crossed, so every combination of their levels is a treatment (a 2^%d design: every factor has a low and a high level)." = "تصميم عاملي بتوزيع عشوائي كامل: العوامل الـ$1 متقاطعة، فكل توليفة من مستوياتها معالجة (تصميم 2^$2: لكل عامل مستوى منخفض ومستوى مرتفع).",
  "y = μ + %s + (two-factor interactions) + ε: one observation per combination, so the three-factor and higher interactions are pooled into the error" = "y = μ + $1 + (التفاعلات الثنائية) + ε: مشاهدة واحدة لكل توليفة، لذلك تُدمج التفاعلات الثلاثية فما فوق في الخطأ",
  "y = μ + %s + all the interactions (two-factor up to the %d-factor interaction) + ε" = "y = μ + $1 + جميع التفاعلات (من الثنائي حتى التفاعل بين $2 عوامل) + ε",
  "One observation per combination: SS_E is the pooled sum of squares of the three-factor and higher interactions (%d df), the usual error for an unreplicated 2^k design." = "مشاهدة واحدة لكل توليفة: SS_E هو مجموع المربعات المدموج للتفاعلات الثلاثية فما فوق ($1 درجة حرية)، وهو الخطأ المعتاد في تصميم 2^k غير المكرَّر.",
  "There is one observation per combination, so there is no pure error. The error line pools the three-factor and higher interactions (%d df), which are assumed negligible; every main effect and two-factor interaction is tested against it." = "توجد مشاهدة واحدة لكل توليفة فلا يوجد خطأ خالص. يدمج سطر الخطأ التفاعلات الثلاثية فما فوق ($1 درجة حرية) المفترض أنها مهملة؛ ويُختبر كل أثر رئيسي وكل تفاعل ثنائي مقابله."
)

ar_add(
  "At the end of an analysis you can download the worked solution (PDF) and an R script." = "في نهاية التحليل يمكنك تنزيل الحل المفصَّل (PDF) وسكربت R.",
  "The worked solution (PDF) and the R script appear here once you have been through every step, from the hypotheses to the decision." = "يظهر الحل المفصَّل (PDF) وسكربت R هنا بعد أن تمرّ بكل الخطوات، من الفرضيات حتى القرار.",
  "Copy link" = "نسخ الرابط", "Press Ctrl+C" = "اضغط Ctrl+C", "End of the analysis" = "نهاية التحليل",
  "Keep your work: the worked solution as a PDF, or an R script you can run to check the numbers yourself." = "احتفظ بعملك: الحل المفصَّل بصيغة PDF، أو سكربت R يمكنك تشغيله للتحقق من الأرقام بنفسك.",
  "Send the link, the file or the code. The other person opens the link in their browser, or uses Open shared analysis (at the top) with the file or the code." = "أرسل الرابط أو الملف أو الرمز. يفتح الطرف الآخر الرابط في متصفحه، أو يستخدم «افتح تحليلًا مشاركًا» (في الأعلى) مع الملف أو الرمز.",
  "Whoever opens it sees exactly this analysis: your data, settings and wording, the test and post-hoc you chose, and the step you are on. The results are recomputed and checked against yours." = "من يفتحه يرى هذا التحليل نفسه: بياناتك وإعداداتك وصياغتك، والاختبار والمقارنات البعدية التي اخترتها، والخطوة التي وصلت إليها. تُعاد حساب النتائج وتُقارَن بنتائجك.",
  "KFUPM · Experimental Design · Worked solution" = "جامعة الملك فهد للبترول والمعادن · التصميم التجريبي · حل مفصَّل",
  "What" = "ماذا", "Share analysis" = "شارِك التحليل", "Open shared analysis" = "افتح تحليلًا مشاركًا", "Download file" = "تنزيل الملف", "Copy code" = "نسخ الرمز", "Copied" = "تم النسخ",
  "Share this analysis" = "مشاركة هذا التحليل", "Your name (optional)" = "اسمك (اختياري)", "Note for the lecturer (optional)" = "ملاحظة للمحاضر (اختيارية)",
  "Or send this code (paste it under Open shared analysis):" = "أو أرسل هذا الرمز (يُلصق ضمن «افتح تحليلًا مشاركًا»):",
  "Whoever opens it (Open shared analysis, at the top) sees exactly this analysis: your data, settings and wording, the test and post-hoc you chose, and the step you are on. The results are recomputed and checked against yours." = "من يفتحه (عبر «افتح تحليلًا مشاركًا» في الأعلى) يرى هذا التحليل نفسه تمامًا: بياناتك وإعداداتك وصياغتك، والاختبار والاختبار البعدي اللذين اخترتهما، والخطوة التي أنت فيها. وتُعاد حساب النتائج وتُقارن بنتائجك.",
  "Open a shared analysis" = "افتح تحليلًا مشاركًا", "Open" = "افتح", "Shared analysis opened" = "فُتح التحليل المشارَك", "Choose file" = "اختر ملفًا", "A .sdsa file" = "ملف ‎.sdsa",
  "Choose the file a student sent you (it ends in .sdsa), or paste the code they sent. You will see their exact data, settings, wording and results, at the step they reached." = "اختر الملف الذي أرسله لك طالب (ينتهي بـ ‎.sdsa)، أو الصق الرمز الذي أرسله. سترى بياناته وإعداداته وصياغته ونتائجه نفسها، عند الخطوة التي وصل إليها.",
  "Or paste the code here (it starts with SDS1:)" = "أو الصق الرمز هنا (يبدأ بـ SDS1:)",
  "The numbers computed here are identical to those in the shared file." = "الأرقام المحسوبة هنا مطابقة لتلك الواردة في الملف المشارَك.",
  "The numbers computed here differ from those in the shared file. The file may come from another version of the app." = "الأرقام المحسوبة هنا تختلف عن تلك الواردة في الملف المشارَك. قد يكون الملف من إصدار آخر للتطبيق.", "R script" = "شيفرة R", "Download solution (PDF)" = "تنزيل الحل (PDF)", "Theoretical quantiles" = "الكميات النظرية", "Sample quantiles" = "كميات العيّنة", "Standardised residuals" = "البواقي المعيارية", "Run order" = "ترتيب التشغيل", "Standard normal" = "الطبيعي المعياري", "Value" = "القيمة", "Count" = "العدد", "Density" = "الكثافة", "Blocks" = "القطاعات",
  "Two factors (3 temperatures × 3 pressures), two replicates of every combination, completely randomised (Montgomery, Problem 5.x)." = "عاملان (3 درجات حرارة × 3 ضغوط)، ومكرَّران لكل توليفة، بتوزيع عشوائي كامل (مونتغمري، المسألة 5.x).", "Conclusion" = "الاستنتاج", "Step" = "الخطوة",
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
  "It is open at step %d, with the student's data, settings and wording." = "فُتح عند الخطوة $1، ببيانات الطالب وإعداداته وصياغته.",
  "Difference in means (%g%%)" = "الفرق بين المتوسطات ($1%)", "critical %.2f" = "الحرجة $1", "One line per %s" = "خط لكل $1",
  "Difference in means%s, %g%% interval" = "الفرق بين المتوسطات$1، فترة $2%", "One line per pair; green where %s is higher" = "خط لكل زوج؛ أخضر حيث يكون $1 أعلى",
  "Mean %s" = "متوسط $1",
  "1 · Parametric: %s" = "1 · معلمي: $1", "2 · Non-parametric: %s" = "2 · لامعلمي: $1",
  "%s, so the hypotheses are about the population medians η. H1 is “at least one median differs”." = "$1، لذا تتعلق الفرضيات بوسيطات المجتمع η. وH1 هي «يختلف وسيط واحد على الأقل».",
  "%s decision" = "قرار $1", "%s post-hoc" = "الاختبارات البعدية لـ$1", "%s · %d rows" = "$1 · $2 صفوف",
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
