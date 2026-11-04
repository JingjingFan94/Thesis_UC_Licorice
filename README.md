# Source data and R Markdown code for Figures 1–6

This repository organizes the **79 source CSV files** supplied for six manuscript figures into one folder per figure and provides six corresponding R Markdown (`.Rmd`) files.

## Structure

- `fig1/` — 24 CSV files and `fig1.Rmd`
- `fig2/` — 18 CSV files and `fig2.Rmd`
- `fig3/` — 8 CSV files and `fig3.Rmd`
- `fig4/` — 9 CSV files and `fig4.Rmd`
- `fig5/` — 7 CSV files and `fig5.Rmd`
- `fig6/` — 13 CSV files and `fig6.Rmd`
- `script/source_data_helpers.R` — shared CSV readers, robust reshaping and descriptive plotting routines.
- `source_data_index.csv` — complete inventory, original data-file SHA-256 checksums, dimensions and data checks.
- `render_all.R` — batch-render six R Markdown documents to HTML in RStudio or R.
- `QUALITY_CHECKS.md` — issues requiring human verification before submission.

## Quick start

1. Download or clone the entire repository (keep the directory structure).
2. Open the project in RStudio (R >= 4.1 recommended).
3. Run `install.packages(c("ggplot2", "knitr", "rmarkdown"))` once. A local Pandoc installation (bundled with RStudio) is needed for HTML output.
4. Open `fig1/fig1.Rmd` and select **Knit**, or run `source("render_all.R")` from the repository root.
5. Repeat or inspect the other figures as needed.

Each `.Rmd` executes from its own `figN/` folder and sources the common functions using a relative path (`../script/source_data_helpers.R`). No personal local paths are used.

## What is reproduced

- Input data reading and completeness checks for **each of the 79 CSV files**.
- Descriptive graphics chosen according to the data layout: group means ± SD with observations, longitudinal or thermal-response curves as mean ± SEM, patient-group distributions, correlation scatter plots, MST measurement curves, and stratified results.
- Duplicate group-name headers are kept as **independent replicate columns** instead of being silently renamed or collapsed; empty cells are treated as missing (NA), not zero.

## Important limitations

**These Rmd scripts are reproducible numeric-data previews, not certified reproductions of the published composite figures.** The provided archive did not contain the original statistical-analysis scripts, the annotation rules, individual source microscopy/blot/image assets, or a full analysis plan. Therefore the scripts do **not** invent p-values, star annotations, tests, normalization, or binding parameters, and they do not recreate graphical panels for which only embedded PDF images are provided.

The six original PDF figures are present in the optional full archive, but omitted from the lightweight source-data upload archive. Use the relevant manuscript PDF to confirm titles, display units, panel composition, and layout before manuscript submission.

Review `QUALITY_CHECKS.md` before sharing publicly and make sure publication permissions, ethics approval, image policies and data availability requirements are met.

## Suggested Data availability wording (only after public release)

> The numerical source data supporting the main figures and the corresponding R Markdown scripts are available at https://github.com/USERNAME/REPOSITORY.

The URL above is a placeholder. Replace with the publicly accessible repository URL only after uploading and verifying all required files.
