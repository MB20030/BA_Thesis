library(tidyverse)

# Dummy example for one election: ZH_15
# Vote shares sum to 15.1
# Seats sum to 9 under Hagenbach-Bischoff and 11 under Pukelsheim

party_data <- tribble(
  ~election, ~party, ~system,               ~seats, ~vote_share,
  "ZH_15",   "GLP",  "Hagenbach-Bischoff",  4,      5.8,
  "ZH_15",   "GLP",  "Pukelsheim",          4,      5.8,
  "ZH_15",   "EVP",  "Hagenbach-Bischoff",  2,      3.6,
  "ZH_15",   "EVP",  "Pukelsheim",          3,      3.6,
  "ZH_15",   "EDU",  "Hagenbach-Bischoff",  1,      2.7,
  "ZH_15",   "EDU",  "Pukelsheim",          2,      2.7,
  "ZH_15",   "BDP",  "Hagenbach-Bischoff",  2,      3.0,
  "ZH_15",   "BDP",  "Pukelsheim",          2,      3.0
)

library(tidyverse)
library(ggrepel)

ggplot(party_data, aes(x = seats, y = vote_share)) +
  geom_line(aes(group = party), color = "grey75", linewidth = 0.7) +
  geom_point(aes(color = system, shape = system), size = 3.5) +
  geom_text_repel(
    aes(label = party),
    color = "black",
    size = 3.5,
    box.padding = 0.3,
    point.padding = 0.25,
    segment.color = "grey80",
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = 1:4, expand = expansion(mult = c(0.05, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    title = "Smaller Parties in ZH_15",
    subtitle = "Comparison of seat allocation under the old and new system",
    x = "Number of seats",
    y = "Vote share (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )