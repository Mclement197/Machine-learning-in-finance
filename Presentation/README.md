# Reading IMF Programs on Day One

The presentation uses the repository's **Metropolis Beamer template**, with a
16:9 layout, Fira Sans typography, St. Gallen green accents, section footers and
a slide-progress indicator.

## Open or build

- **Compiled slides:** [presentation.pdf](presentation.pdf)
- **Editable PowerPoint:** [presentation.pptx](presentation.pptx)
- **Editable source:** [presentation.tex](presentation.tex)
- **References:** [references.bib](references.bib)

From the repository root:

```bash
latexmk -xelatex -interaction=nonstopmode -halt-on-error -file-line-error -cd Presentation/presentation.tex
```

This produces `Presentation/presentation.pdf`. `latexmk` runs XeLaTeX and BibTeX
as needed. Use a TeX Live or MiKTeX installation with Beamer, Metropolis, Fira
Sans, PGFPlots, `appendixnumberbeamer`, `booktabs`, `tabularx` and `bibentry`
(part of `natbib`).

For an editor without `latexmk`, run these commands **inside `Presentation/`**:

```bash
xelatex -interaction=nonstopmode -halt-on-error presentation.tex
bibtex presentation
xelatex -interaction=nonstopmode -halt-on-error presentation.tex
xelatex -interaction=nonstopmode -halt-on-error presentation.tex
```

For Overleaf, upload `presentation.tex`, `references.bib` and `figures/`, set
`presentation.tex` as the main document, and select **XeLaTeX** as the compiler.
All figures needed for compilation are included in this folder.

## PowerPoint version

[presentation.pptx](presentation.pptx) contains the same 16-slide presentation,
adapted to native PowerPoint objects with the green Metropolis-inspired styling
and widescreen layout. It uses Arial for portable editing and rendering.

- Text, tables, layout shapes and the facility-rate chart are editable.
- The original model plots, disbursement diagram and university logo are
  embedded images that can be resized or replaced.
- Speaker notes on the forest-tuning and logistic-regression slides include
  the country-CV explanation and the coefficient-scale note.

The file was opened and rendered in Microsoft PowerPoint to check compatibility
and slide layout. To regenerate it from the repository root:

```bash
python -m pip install python-pptx Pillow
python Presentation/build_powerpoint.py
```

This regenerates `presentation.pptx` from the content and layout in
`build_powerpoint.py`. Update that builder to carry content changes into a new
PowerPoint export, or edit the PPTX directly in PowerPoint.

## Slide structure

The original 13 PDF pages have been laid out as **16 slides**: a title slide,
13 numbered content slides and two reference slides. The densest material is
split across slides to keep the type and charts readable.

| Original PDF | LaTeX PDF | Content |
| --- | --- | --- |
| 1 | 1 | Title, authors and date |
| 2 | 2 | Executive summary and road map |
| 3 | 3 | Literature, research question and disbursement diagram |
| 4–6 | 4–7 | Study goal, data construction, features and exploration |
| 7 | 8 | Random-forest tuning |
| 8 | 9–10 | Random-forest performance and importance |
| 9 | 11 | Logistic regression |
| 10 | 12 | Neural network |
| 11 | 13 | Model comparison |
| 12 | 14 | Limitations and conclusion |
| 13 | 15–16 | All 13 references |

## Figures and provenance

The logo and all seven original illustrations are extracted directly from
`Machine_Learning_Project.pdf` at their embedded resolution, including
transparency. They are individual image assets, rather than screenshots of
whole slides. Text, tables, the feature layout and the descriptive facility
chart are editable LaTeX.

| Asset in `figures/` | Original PDF page |
| --- | --- |
| `hsg_logo.png` | 1 |
| `disbursement_timeline.png` | 3 |
| `rf_tuning.png` | 7 |
| `rf_importance.png` | 8 |
| `logit_roc.png`, `logit_coefficients.png` | 9 |
| `nn_tuning.png` | 10 |
| `model_comparison.png` | 11 |

The extraction can optionally be repeated from the repository root:

```bash
python -m pip install pymupdf
python Presentation/prepare_assets.py
```

The extraction script requires the original 13-page PDF. Normal LaTeX builds
use the included assets and require neither Python nor the original PDF.
The facility-rate chart uses the original slide's rounded rates (SBA 64%, EFF
56%, ECF 38%); it explicitly shows selected facilities.

## Content checks and editorial corrections

The reported model scores and tuning choices are transcribed from the supplied
presentation. Data definitions and counts were cross-checked against
`Data_Prep/01_data.R` and the existing summary CSVs. The transfer includes these
corrections and clarifications:

- **Outcome notation:** corrected “X = 0 otherwise” to “Y = 0 otherwise”. The
  prediction target is written as a conditional probability for the binary
  classification task.
- **Baseline arithmetic:** the full-sample interruption rate is 82/173 = 47.4%.
  In the test set, always predicting interruption gives 17/28 = 60.7% accuracy,
  **39.3% error** and 50% balanced accuracy. This replaces the inconsistent
  original “37% error” statement.
- **Data provenance:** the final 13 predictors use MONA approval-snapshot macro
  data. The preparation code excludes revised April 2026 WEO fiscal data and
  does not use the WDI fallback. Both supplementary sources and their original
  references are retained, with their role clarified.
- **Feature exclusions:** the exclusion audit has 12 entries, some representing
  groups of fields. They are described as candidate entries rather than 12
  individual columns.
- **Facility coverage:** the preparation code includes predecessor facilities,
  including ESF under the SCF grouping.
- **Neural-network regularisation:** the supplied chart shows decay-1 scores
  around 0.51–0.53, rather than exactly 0.50 at every size.
- **Small-sample sensitivity:** one extra correct prediction changes balanced
  accuracy by 1/(2 × 17) = 0.029 for an interrupted programme or 1/(2 × 11) =
  0.045 for a completed one; the slide reports approximately 0.03–0.05.
- **Interpretation:** “macro variables add nothing” is qualified as little
  predictive importance in this forest. The comparison table consolidates the
  original logistic, forest and neural-network scores; the constant-score
  baseline's AUC of 0.500 follows from all predictions being tied. The original
  comparison chart's 0.50 line is distinguished from the 60.7% test accuracy of
  the always-interrupted rule.

Spelling, decimal separators and programme terminology are made consistent.
The title, six authors, university and presentation date are retained.
