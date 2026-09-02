# BA Thesis

This repository contains the source files and the rendered PDF for my Bachelor's thesis.

## Requirements

- R 4.5 or newer
- RStudio Desktop (includes Quarto)
- Git

## Setup

1. Open `BA_Thesis.Rproj` in RStudio.
2. In the R console run:

```r
source("setup.R")
```

3. Restart R to activate the local `renv` environment.

On a new machine you can also install the locked package versions with `renv::restore()`.

## Render the thesis

In RStudio open `Thesis/Thesis.qmd` and click "Render", or run:

```r
quarto::quarto_render("Thesis/Thesis.qmd")
```

The PDF will be created at `Thesis/Thesis.pdf`.

## Repository structure

- `Thesis/`: Quarto source and rendered PDF
- `data/datasets/`: raw data by canton
- `data/preparation_and_training/`: R scripts for data preparation and analysis
- `sources/`: literature and references
- `formalities/`: university guidelines and deadlines
- `proposal/`: proposal and planning
- `thesis_colloqium/`: colloquium materials
