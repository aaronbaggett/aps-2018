# --------------------------------------------------------------
# FIGURE: Umpire accuracy dotplot, 2015-2017
#
# TITLE: Modeling Ability and Decision Difficulty
# Among Expert Baseball Umpires
#
# APS 2018
# Aaron R. Baggett, Ph.D.
# May 26, 2018
# --------------------------------------------------------------

# Load package libraries
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

# Load PITCHf/x data
load("data/pfx_15-17.rda")

# Sorted list of most accurate umpires
ind_acc <- pfx_dat %>%
  group_by(umpire) %>%
  summarize(N = length(call),
    mean = mean(u_test),
    se = sqrt(mean*((1 - mean) / N)),
    ll = mean - (2*se),
    ul = mean + (2*se)) %>%
  filter(N >= 4000) %>% 
  arrange(mean)

# Relevel umpire names to prevent ggplot resorting
ind_acc$umpire <- factor(ind_acc$umpire, levels = ind_acc$umpire)

# Dot plot of individual umpire accuracy for season
(ind_acc_plot <- ggplot(data = ind_acc, aes(x = mean, y = umpire, group = 1)) + 
  geom_vline(xintercept = 0.8921817, color = "tomato") +
    geom_errorbarh(aes(x = mean, xmin = ll, xmax = ul, y = umpire), 
      color = "grey55", height = 0) +
    geom_point(color = "dodgerblue3", size = 2) + 
    geom_line(color = "dodgerblue3") + 
  scale_x_continuous(breaks = seq(.80, 1, .010), 
    limits = c(.80, .93),
    name = "\nMean Accuracy Rate (2015-2017)") +
  scale_y_discrete(name = "Umpire\n") +
  theme_bw() + theme(axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size= 13, face = "bold"),
    axis.text.x = element_text(size = 11), 
    axis.text.y = element_text(size = 7)) + 
  theme(panel.grid.minor = element_blank())
)

# ggsave("ind_acc_plot.pdf", device = "pdf", width = 7.5, height = 13, 
#   units = "in")
