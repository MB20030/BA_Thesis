library(tidyverse)
library(ggrepel)
library(ggplot2)

dummy_data <- tribble(
  ~election,   ~canton, ~year, ~system,               ~seats_small_parties, ~vote_share_small_parties,
  "ZH_07",   "ZH",    2007,  "Hagenbach-Bischoff",  8,                    14.2,
  "ZH_07",   "ZH",    2007,  "Pukelsheim",          10,                   14.2,
  "ZH_11",   "ZH",    2011,  "Hagenbach-Bischoff",  7,                    13.8,
  "ZH_11",   "ZH",    2011,  "Pukelsheim",          9,                    13.8,
  "ZH_15",   "ZH",    2015,  "Hagenbach-Bischoff",  9,                    15.1,
  "ZH_15",   "ZH",    2015,  "Pukelsheim",          11,                   15.1,
  "ZH_19",   "ZH",    2019,  "Hagenbach-Bischoff",  10,                   16.0,
  "ZH_19",   "ZH",    2019,  "Pukelsheim",          12,                   16.0,
  "ZH_23",   "ZH",    2023,  "Hagenbach-Bischoff",  9,                    15.4,
  "ZH_23",   "ZH",    2023,  "Pukelsheim",          11,                   15.4,
  "AG_09",   "AG",    2009,  "Hagenbach-Bischoff",  5,                    11.3,
  "AG_09",   "AG",    2009,  "Pukelsheim",          7,                    11.3,
  "AG_12",   "AG",    2012,  "Hagenbach-Bischoff",  6,                    12.1,
  "AG_12",   "AG",    2012,  "Pukelsheim",          7,                    12.1,
  "AG_16",   "AG",    2016,  "Hagenbach-Bischoff",  6,                    12.5,
  "AG_16",   "AG",    2016,  "Pukelsheim",          8,                    12.5,
  "AG_20",   "AG",    2020,  "Hagenbach-Bischoff",  7,                    13.0,
  "AG_20",   "AG",    2020,  "Pukelsheim",          9,                    13.0,
  "AG_24",   "AG",    2024,  "Hagenbach-Bischoff",  7,                    13.4,
  "AG_24",   "AG",    2024,  "Pukelsheim",          9,                    13.4,
  "SH_08",   "SH",    2008,  "Hagenbach-Bischoff",  3,                    10.4,
  "SH_08",   "SH",    2008,  "Pukelsheim",          4,                    10.4,
  "SH_12",   "SH",    2012,  "Hagenbach-Bischoff",  3,                    10.8,
  "SH_12",   "SH",    2012,  "Pukelsheim",          5,                    10.8,
  "SH_16",   "SH",    2016,  "Hagenbach-Bischoff",  4,                    11.6,
  "SH_16",   "SH",    2016,  "Pukelsheim",          5,                    11.6,
  "SH_20",   "SH",    2020,  "Hagenbach-Bischoff",  4,                    11.9,
  "SH_20",   "SH",    2020,  "Pukelsheim",          6,                    11.9,
  "SH_24",   "SH",    2024,  "Hagenbach-Bischoff",  4,                    12.2,
  "SH_24",   "SH",    2024,  "Pukelsheim",          6,                    12.2,
  "SZ_16",   "SZ",    2016,  "Hagenbach-Bischoff",  1,                    10.9,
  "SZ_16",   "SZ",    2016,  "Pukelsheim",          4,                    10.9,
  "SZ_20",   "SZ",    2020,  "Hagenbach-Bischoff",  3,                    11.7,
  "SZ_20",   "SZ",    2020,  "Pukelsheim",          5,                    11.7,
  "SZ_24",   "SZ",    2024,  "Hagenbach-Bischoff",  3,                    12.4,
  "SZ_24",   "SZ",    2024,  "Pukelsheim",          5,                    12.4,
  "GR_22",   "GR",    2022,  "Hagenbach-Bischoff",  2,                    10.6,
  "GR_22",   "GR",    2022,  "Pukelsheim",          4,                    10.6
)


ggplot(dummy_data, aes(x = seats_small_parties,
                       y = vote_share_small_parties,
                       color = system,
                       shape = system)) +
  geom_line(aes(group = election), color = "grey75", linewidth = 0.6) +
  geom_point(size = 3.2) +
  geom_text_repel(aes(label = election),
                  size = 3.2,
                  show.legend = FALSE,
                  max.overlaps = Inf,
                  box.padding = 0.3,
                  point.padding = 0.2,
                  segment.color = "grey80",
                  segment.linewidth = 0.4) +
  scale_color_manual(values = c("Hagenbach-Bischoff" = "#D55E00",
                                "Pukelsheim" = "#0072B2")) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.08))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  labs(
    title = "Smaller Parties Across Elections",
    subtitle = "Dummy data: election-level comparison of HB and Pukelsheim",
    x = "Number of seats for smaller parties",
    y = "Aggregated vote share of smaller parties (%)",
    color = "Electoral system",
    shape = "Electoral system"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

