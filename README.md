# BA Thesis

Das eigenständige Repository für meine Bachelorarbeit.

## Voraussetzungen

- R 4.5 oder neuer
- RStudio Desktop (enthält Quarto)
- Git

R 4.5.1, RStudio 2026.04 und Quarto 1.9.36 sind auf diesem Rechner bereits
installiert. Python und `uv` werden nicht benötigt.

## Ersteinrichtung

1. `BA_Thesis.Rproj` in RStudio öffnen.
2. In der R-Konsole einmal `source("setup.R")` ausführen.
3. R anschließend neu starten, damit die lokale `renv`-Umgebung aktiv ist.

Auf einem neuen Rechner können die festgeschriebenen Paketversionen auch direkt
mit `renv::restore()` installiert werden.

## Thesis rendern

In RStudio `Thesis/Thesis.qmd` öffnen und auf **Render** klicken. Alternativ in
der R-Konsole:

```r
quarto::quarto_render("Thesis/Thesis.qmd")
```

Die PDF wird als `Thesis/Thesis.pdf` gespeichert.

## Struktur

- `Thesis/`: Quarto-Quelldatei und gerenderte PDF der Bachelorarbeit
- `data/datasets/`: Rohdaten nach Kanton
- `data/preparation_and_training/`: R-Skripte für Vorbereitung und Analysen
- `sources/`: Literatur und Quellen
- `formalities/`: universitäre Vorgaben und Fristen
- `proposal/`: Exposé und Planung
- `thesis_colloqium/`: Kolloquiumsunterlagen
