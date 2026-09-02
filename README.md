# BA Thesis

This repository contains the source files and the rendered PDF for my Bachelor's thesis.

## Requirements

- R 4.5 or newer
- Visual Studio Code (with the R extension) or another editor
- Git
- Quarto (for rendering)

## Setup

1. Open the project folder in Visual Studio Code.
2. Start an R session (e.g. using the VS Code R extension) and run:

```r
source("setup.R")
```

Or from a terminal run:

```bash
Rscript -e "source('setup.R')"
```

3. Restart the R session to activate the local `renv` environment.

On a new machine you can also install the locked package versions with `renv::restore()`.

## Render the thesis

From the terminal run:

```bash
quarto render Thesis/Thesis.qmd
```

Or render from R:

```r
quarto::quarto_render("Thesis/Thesis.qmd")
```

The PDF will be created at `Thesis/Thesis.pdf`.

## Repository structure

- `Thesis/`: Quarto source and rendered PDF
- `data/datasets/`: raw data by canton
- `data/preparation_and_training/`: R scripts for data preparation and analysis
- `data_cleaning_canton/` : The R Code for the cleaning of all elections
- `sources/`: literature and references
- `formalities/`: university guidelines and deadlines
- `proposal/`: proposal and planning
- `thesis_colloqium/`: colloquium materials
