# hri-stats-tutorial

Teaching HRI statistics in R, from paired t-tests to ordinal mixed models.
The tutorial is a [Quarto](https://quarto.org) website. Its R code runs in the
reader's browser through [Quarto Live](https://r-wasm.github.io/quarto-live/)
and [webR](https://docs.r-wasm.org/webr/latest/), so it is hosted for free on
GitHub Pages with no server.

**Live site:** https://pourya-shahverdi.github.io/hri-stats-tutorial/

All data in this repository are simulated. They copy the design of the N66
affect study but contain no participant data.

## One-time setup on GitHub

1. Go to **Settings → Pages**.
2. Under **Build and deployment → Source**, choose **GitHub Actions**.

After that, every push to `main` renders and publishes the site
(see `.github/workflows/publish.yml`). Progress shows in the **Actions** tab.

## Preview on your computer

You need R with `knitr` and `rmarkdown`, and Quarto (RStudio includes Quarto).

```bash
quarto preview
```

Open the page from the address `quarto preview` prints. Opening the HTML file
directly from disk does not work, because webR must be served over HTTP.

## Layout

| Path | What it is |
|------|------------|
| `_quarto.yml` | Site settings and navigation |
| `index.qmd` | Home page and module list |
| `webr-check.qmd` | Checks that the browser can install the packages and fit the models |
| `data/affect_sim.csv` | Simulated data (1,188 rows) |
| `R/simulate_affect.R` | Code that generates the simulated data |
| `_extensions/r-wasm/live/` | Quarto Live extension (v0.2.0) |
| `.github/workflows/publish.yml` | Renders and deploys the site |

## Regenerate the data

```r
source("R/simulate_affect.R")
write.csv(simulate_affect(), "data/affect_sim.csv", row.names = FALSE)
```

## Writing a new module

Start a page with this header, then use `{webr}` code blocks:

````markdown
---
title: "Module title"
format: live-html
engine: knitr
resources:
  - data
---

{{< include ./_extensions/r-wasm/live/_knitr.qmd >}}
````

Add the new file to `project: render:` and the navbar in `_quarto.yml`.
