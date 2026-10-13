# Combined research report

- [Compiled PDF](report.pdf)
- [Main LaTeX document](report.tex)
- [Editable report parts](parts/)
- [Single final bibliography](bibliography.tex)
- [List of aids](parts/08_list_of_aids.tex)
- [Declaration of authorship and signatures](parts/09_declaration_of_authorship.tex)
- [Bundled journal-template class](olplainarticle.cls)

## Design

The report uses `olplainarticle`, from the supplied
`template_hsg/Basic_Academic_Journal_Article_Template.zip` (Overleaf's Basic
Academic Journal Article Template, based on SelfArx). It follows the template's
single-column layout, asymmetric journal margins, Times-style body text,
uppercase sans-serif section headings, compact title/author block, and
page/total-pages footer.

The document is A4 with 11-point, 1.5-spaced body text; tables, figure notes and
references are single-spaced, with additional separation between paragraphs,
headings, captions and display blocks. TeX Gyre Termes and Heros supply
Unicode-compatible Times and Helvetica-style fonts. The title block includes the existing HSG
logo, all seven authors, their shared affiliation without repeated superscripts,
course and term. It is adapted to the supplied material rather than the
template's sample abstract and keywords.
The article starts directly below the title block, as in the template.

The bundled class retains its original attribution. Small compatibility changes
make input encoding engine-aware and use `zref` for the template's total-page
footer. Project-specific title and font settings are in `report.tex`.

## Content and sources

The seven research parts use the current Word sources, including the reviewed study
design, revised logistic-regression section and conclusion. Prose, numerical
results, captions, citations and the footnote are transcribed without
rephrasing. Exercise identifiers have been removed from headings (for example,
`b.`, `3A.` and `(Task 1g)`). Table numbers and in-text table references are
generated automatically across the report. Uppercase main headings are a
typesetting feature of the journal template.

| Word document | LaTeX part |
| --- | --- |
| `1.a) updated.docx` | `parts/01_literature.tex` |
| `1b 1c reviewed.docx` | `parts/02_study_design.tex` |
| `ML in Finance paper – Data sections (1d, 1e, 1g) (1).docx` | `parts/03_data.tex` |
| `ML paper (4).docx` | `parts/04_random_forest.tex` |
| `Logistic Regression with References.docx` | `parts/05_logistic_regression.tex` |
| `3b_Neural_Network_full.docx` | `parts/06_neural_network.tex` |
| `conclusion.docx` | `parts/07_conclusion.tex` |
| `Declaration_of_Authorship_We_With_Aids_and_Signatures.docx` — aids table | `parts/08_list_of_aids.tex` |
| Same document — declaration and signature fields | `parts/09_declaration_of_authorship.tex` |

The conclusion appears under its original heading, **Remarks**. The bibliography
starts on a new page, followed by the **List of Aids** on a new page and finally
the **Declaration of Authorship**, also starting on a new page. The declaration
retains its supplied wording and ends with a separate signature page containing
blank fields for all seven authors. The original random-forest explanatory
footnote is retained.

The six source reference lists are consolidated into
one alphabetised bibliography: 51 entries represent 37 distinct works. Each
retained entry uses a source version with fuller publication/link details.
The IMF 2026a/b/c labels follow `1b 1c reviewed.docx`; the Breiman 2001a/b labels
follow the revised logistic-regression document.

The `\src{...}` and `\srcnote{...}` wrappers identify original text for fidelity
checks and print their contents directly. Table captions use
`\wordtablecaption{tab:label}{...\thetable...}`; the variable dictionary continues
in a second block with the same automatic table number and repeated column headings.
Cross-references use
`Table~\ref{tab:label}`. Heading cleanup and automatic table numbering are the
deliberate exceptions to the original wording.

## Build

From the repository root:

```bash
latexmk -xelatex -interaction=nonstopmode -halt-on-error -file-line-error -cd Report/report.tex
```

This produces `Report/report.pdf`. The required project files are `report.tex`,
`olplainarticle.cls`, `parts/*.tex`, `bibliography.tex` and the existing images
listed below. Word documents, the template ZIP, generated data files, Python
and R are not build dependencies. Local Word drafts and LaTeX intermediates
are ignored by Git.

Use XeLaTeX with TeX Live or MiKTeX, TeX Gyre fonts and the standard packages
loaded by the bundled class and main document: `fontspec`, `babel`, `microtype`,
`setspace`, `graphicx`, `xcolor`, `booktabs`, `tabularx`, `array`, `authblk`,
`caption`, `natbib`, `fancyhdr`, `titlesec`, `titletoc`, `zref`, `xurl` and
`hyperref`, plus the template's supporting packages.

Without `latexmk`, run XeLaTeX twice inside `Report/`:

```bash
xelatex -interaction=nonstopmode -halt-on-error report.tex
xelatex -interaction=nonstopmode -halt-on-error report.tex
```

## Existing image dependencies

| Use | Repository file |
| --- | --- |
| University logo | `Presentation/figures/hsg_logo.png` |
| Forest importance figure | `Presentation/figures/rf_importance.png` |
| Logistic-regression coefficients | `Presentation/figures/logit_coefficients.png` |
| Neural-network tuning figure | `Neural_Network/fig_nn_tuning.png` |

Keep these relative paths alongside `Report/` when moving the source or using
Overleaf. Select `Report/report.tex` as the main document and XeLaTeX as the
compiler.
