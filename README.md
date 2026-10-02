# hri-stats-tutorial

An introductory statistics tutorial for human–robot interaction (HRI) studies,
written for undergraduates. It goes from comparing two groups (mean, variance,
the normal distribution, p and alpha, the t-test) to linear mixed models and
ordinal mixed models.

The tutorial is a [Quarto](https://quarto.org) website. Its R code runs in the
reader's browser through [Quarto Live](https://r-wasm.github.io/quarto-live/)
and [webR](https://docs.r-wasm.org/webr/latest/), so it is hosted for free on
GitHub Pages with no server.

**Live site:** https://pourya-shahverdi.github.io/hri-stats-tutorial/

All data in this repository are simulated. No real participants are involved.

## Chapters

| File | Chapter |
|------|---------|
| `01-basics.qmd` | Comparing two groups: mean, SD, normal distribution, p and alpha, t-test |
| `02-repeated-measures.qmd` | Same people, many measurements: independence, pseudo-replication, paired t-test |
| `03-mixed-models.qmd` | Linear mixed models: fixed and random effects, random intercepts and slopes |
| `04-ordinal.qmd` | Rating-scale outcomes: ordinal regression (`clm`) and ordinal mixed models (`clmm`) |

## Publishing

Every push to `main` renders and publishes the site
(see `.github/workflows/publish.yml`). Progress shows in the **Actions** tab.
GitHub Pages must be set to **Settings → Pages → Source → GitHub Actions**.

## Preview on your computer

You need R with `knitr` and `rmarkdown`, and Quarto (RStudio includes Quarto).
In RStudio, open the project and click **Build → Preview Website**, or run:

```bash
quarto preview
```

Open the address it prints in Chrome or Edge. Opening the HTML file directly
from disk does not work, because webR must be served over HTTP.

## Layout

| Path | What it is |
|------|------------|
| `_quarto.yml` | Site settings and sidebar navigation |
| `index.qmd` | Home page |
| `01-...qmd` to `04-...qmd` | The four chapters |
| `webr-check.qmd` | Checks that the browser can install the packages and fit the models |
| `data/robot_one_visit.csv` | Chapter 1 data: 40 people, one rating each |
| `data/robot_sessions.csv` | Chapters 2–4 data: 30 people × 5 sessions |
| `R/simulate_tutorial_data.R` | Code that creates the two files above |
| `data/affect_sim.csv`, `R/simulate_affect.R` | Simulated affect-study data, kept for the later study chapters |
| `_extensions/r-wasm/live/` | Quarto Live extension (v0.2.0) |
| `.github/workflows/publish.yml` | Renders and deploys the site |

## Regenerate the data

```r
source("R/simulate_tutorial_data.R")
write_tutorial_data()
```

## Writing a new page

Start a page with this header, then use `{webr}` code blocks. List any R
packages the page needs under `webr: packages:`.

````markdown
---
title: "Page title"
format:
  live-html:
    webr:
      packages:
        - ggplot2
      cell-options:
        autorun: true
engine: knitr
resources:
  - data
---

{{< include ./_extensions/r-wasm/live/_knitr.qmd >}}
````

Add the new file to `project: render:` and the sidebar in `_quarto.yml`.
Avoid regular `{r}` chunks: they run on GitHub during the build, where only
`knitr` and `rmarkdown` are installed.
