# Revised research report

## Revision outputs

- **Output A:** [complete report PDF](report.pdf), [main LaTeX source](report.tex),
  [complete section sources](parts/) and [downloadable source bundle](revision_sources.zip).
- **Output B:** [item-by-item change log and self-check](REVISION_CHANGELOG.md).
- **Output C:** [unresolved markers](REVISION_MARKERS.md).
- [For the authors](FOR_AUTHORS.md): the brief's Part 9 checklist, reproduced unchanged.

The current report is revised under the supplied editorial brief. The class and
preamble formatting settings are preserved under Rule A; authored title,
subtitle, date and metadata strings are updated. Numbered sections and contents
use the existing class. Its uppercase heading display, caption presentation,
green URL links, fonts, spacing and page-count footer are retained. A subsequent
author request sets both left and right margins to 2 cm in `report.tex`.
The new contents uses native template styling, without a custom Brief column.
Detailed precedence exceptions are recorded in Output B.

## Complete section sources

| Report section | LaTeX source |
| --- | --- |
| 1 Introduction and related literature | `parts/01_literature.tex` |
| 2 Research design | `parts/02_study_design.tex` |
| 3 Data | `parts/03_data.tex` |
| 4 Random forest | `parts/04_random_forest.tex` |
| 5.1 Logistic regression | `parts/05_logistic_regression.tex` |
| 5.2–5.4 Neural network and model comparison | `parts/06_neural_network.tex` |
| 6 Discussion and conclusion | `parts/07_conclusion.tex` |
| References | `bibliography.tex` |
| Declaration of AI use and supplied aids disclosure | `parts/08_list_of_aids.tex` |
| Existing authorship declaration and signature fields | `parts/09_declaration_of_authorship.tex` |

These are complete sections, including retained sentences, rather than a list
of replacement passages. The existing Remarks were merged into section 6.
The already-supplied aids and authorship statements are retained instead of an
invented AI-use declaration. The report contains 40 distinct reference entries,
including the three authorised additions in Part 6h of the brief.

All five tables and three figures have automatic numbers and labels. The
variable dictionary retains its continuation block. Selected-threshold and
common-threshold model comparisons are identified explicitly. Existing plots
remain linked to repository images; author re-export and source/data checks are
listed in `FOR_AUTHORS.md`.

## Build

From the repository root:

```bash
latexmk -xelatex -interaction=nonstopmode -halt-on-error -file-line-error -cd Report/report.tex
```

Required project files: `report.tex`, `olplainarticle.cls`, `parts/*.tex`,
`bibliography.tex` and the images below. The Word drafts, template ZIP, data
files, Python and R are not compilation dependencies.

Use XeLaTeX with TeX Live or MiKTeX, TeX Gyre fonts and the standard packages
loaded by the main document and bundled class. The class's mathematical font
support includes the RSFS font metrics. From inside `Report/`, XeLaTeX can also
be run repeatedly until the contents, cross-references and page total resolve.

## Existing image dependencies

| Use | Repository file |
| --- | --- |
| University logo | `Presentation/figures/hsg_logo.png` |
| Figure 1: permutation importance | `Presentation/figures/rf_importance.png` |
| Figure 2: logistic coefficients | `Presentation/figures/logit_coefficients.png` |
| Figure 3: neural-network tuning | `Neural_Network/fig_nn_tuning.png` |

Keep these relative paths when moving the source or uploading it to Overleaf.
Select `Report/report.tex` as the main document and XeLaTeX as the compiler.
The source bundle includes the required directory structure and existing images.
