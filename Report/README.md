# Condensed research report

- [Compiled report PDF](report.pdf)
- [Main LaTeX source](report.tex)
- [Complete section sources](parts/)
- [Single reference list](bibliography.tex)
- [Applied cuts and checks](CUT_SUMMARY.md)

The report incorporates the instructions in `Report_Cuts_6_pages.docx`. It is
18 pages, reduced from 24, with the existing HSG template, 2 cm side margins,
body fonts, sizes and spacing retained. Only the specifically requested table
and figure arrangements were compacted. Numerical table values are unchanged.

The standalone cover follows the supplied University of St. Gallen example:
a centred HSG symbol and university heading, a ruled title block, and aligned
course, instructor, submission-date and group-member details. The table of
contents occupies the following page. Cover metadata is set in `report.tex`.

## Structure

| Report content | Source |
| --- | --- |
| 1 Introduction and related literature | `parts/01_literature.tex` |
| 2 Research design | `parts/02_study_design.tex` |
| 3 Data, including Tables 1 and 2 on one page | `parts/03_data.tex` |
| 4 Random forest and paired Figures 1–2 | `parts/04_random_forest.tex` |
| 5.1 Logistic regression | `parts/05_logistic_regression.tex` |
| 5.2–5.4 Neural network and model comparison | `parts/06_neural_network.tex` |
| 6 Discussion and conclusion | `parts/07_conclusion.tex` |
| References | `bibliography.tex` |
| AI-use declaration and list of aids | `parts/08_list_of_aids.tex` |
| Authorship declaration | `parts/09_declaration_of_authorship.tex` |

Tables 1–5 and Figures 1–2 are numbered automatically. Figure 3 was removed as
requested; its tuning results remain in the text. The variable dictionary has
four columns, and the paired figures use existing repository images. Research
displays use `[htbp]` placement. The professor's parallel-programme and
moral-hazard points are retained in the shortened conclusion.

The single bibliography contains 37 cited works. The list of aids declares
EndNote (use confirmed by the authors), ChatGPT (OpenAI) and Claude (Anthropic),
including the AI tools' assistance with code and drafts. It and the authorship
declaration follow the references. Unresolved `[verify]` markers are retained
for author checking. Citation page checks are recorded in
[REVISION_MARKERS.md](REVISION_MARKERS.md); the Christodoulou citation explicitly
uses the accepted manuscript's pagination. The IMF WEO retrieval date is
October 1, 2026.

## Build

From the repository root:

```bash
latexmk -xelatex -interaction=nonstopmode -halt-on-error -file-line-error -cd Report/report.tex
```

Required files are `report.tex`, `olplainarticle.cls`, `parts/*.tex`,
`bibliography.tex` and the images below. The Word drafts, cut-instruction document,
template ZIP, data files, Python and R are not compilation dependencies.

Use XeLaTeX with TeX Live or MiKTeX, TeX Gyre fonts and the standard packages
loaded by the main document and bundled class. The class's mathematical font
support includes RSFS font metrics. XeLaTeX may also be run repeatedly from
inside `Report/` until the contents, cross-references and page totals resolve.

## Image dependencies

| Use | Existing repository file |
| --- | --- |
| University logo | `Presentation/figures/hsg_logo.png` |
| Figure 1: permutation importance | `Presentation/figures/rf_importance.png` |
| Figure 2: logistic coefficients | `Presentation/figures/logit_coefficients.png` |

Keep these relative paths when moving the source or using Overleaf. Select
`Report/report.tex` as the main document and XeLaTeX as the compiler.
