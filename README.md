# Bachelor's Thesis: 20 Years of Double Pukelsheim in Switzerland

This repository contains the thesis, data, and R code for my Bachelor's thesis in Economics at the University of St. Gallen. It examines how the transition from the Hagenbach-Bischoff method to the Double Pukelsheim electoral system affects the parliamentary representation of smaller parties in Swiss cantons.

## What is analyzed?

The central question is whether, and by how much, smaller parties receive a larger share of parliamentary seats under Double Pukelsheim than under the former Hagenbach-Bischoff system.

For each election, the thesis compares the official Double Pukelsheim result with a **counterfactual allocation**: a calculation of how the same observed votes would translate into seats under the specified former cantonal rules. The votes, constituencies, number of seats, and reported electoral lists remain the same in both calculations. The reconstruction accounts for canton-specific allocation rules, but does not assume hypothetical alliances between separately reported lists.

**Smaller parties** are defined as electoral lists or list groups receiving less than 10% of the vote in the area analyzed. This classification is determined separately for each election, and the same group is compared under both allocation systems. Independently reported youth or affiliated lists remain separate units. The analysis adds up the seats won by these smaller lists under each system and compares their combined share of the seats covered by the analysis. For example, a combined seat share of 20% under Double Pukelsheim versus 15% under Hagenbach-Bischoff represents a gain of five percentage points. A collective gain does not mean that every smaller list benefits.

Beyond the overall seat-share difference, the thesis explores how the results relate to:

- **Constituency size and variation:** whether gains differ according to the number of seats per constituency and how unevenly those seats are distributed across constituencies.
- **Electoral thresholds:** whether gains differ between elections with and without explicit minimum vote-share requirements for representation.
- **Successive elections:** whether the advantage for smaller parties persists over time within each canton, taking changes in the composition of the below-10% group into account.

The comparison measures the mechanical effect of applying different allocation rules to the observed election results. Voters and parties might have behaved differently under the former system, so it does not estimate the reform's full causal effect. 

## Data

The main analysis covers 31 cantonal parliamentary elections between 2007 and 2026 in Aargau, Nidwalden, Schaffhausen, Schwyz, Uri, Valais, Zug, and Zurich. Two elections in Graubünden provide additional context, they are excluded from the main transition comparison because the canton previously used a majority voting system. These elections were till the completion, all available elections under Double Pukelsheim for regional parliaments in Switzerland.

The data come from official cantonal election results published by statistical offices and state chancelleries, supplemented by files supplied directly by cantonal authorities. They include party and list votes, constituency seat totals, and official seat allocations, collected in CSV, Excel, and PDF formats and harmonized in R.

## Repository contents

- [Thesis](Thesis/Thesis.pdf): the thesis PDF; its source is in `Thesis/`.
- `data/datasets/`: election data organized by canton.
- `data_cleaning_cantons/`: R scripts for cleaning data and calculating counterfactual seat allocations.
- `sources/`: some of the literature and reference material.

## Contact

If you have any questions about the thesis, data, or analysis, feel free to contact me at [beynmoritz@gmail.com](mailto:beynmoritz@gmail.com).

Moritz
