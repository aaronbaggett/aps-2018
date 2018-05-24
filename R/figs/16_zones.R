# --------------------------------------------------------------
# FIGURE: Recallibrated strike zone regions 1-16
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

# Create region boundaries
xmin <- rep(seq(-2, 1, 1), each = 4)
xmax <- rep(seq(-1, 2, 1), each = 4)

ymin <- rep(seq(0.5, 3.5, 1), times = 4)
ymax <- rep(seq(1.5, 4.5, 1), times = 4)

# Create region labels
reg_lab <- list()
for(i in 1:4){
  reg_lab[[i]] <- seq(i, 16, 4)
}

# Reverse order for accurate plotting
region <- c(rev(reg_lab[[1]]),
rev(reg_lab[[2]]),
rev(reg_lab[[3]]),
rev(reg_lab[[4]])
  )

# Regions data frame for plotting
regions <- data_frame(region, xmin, xmax, ymin, ymax)

# Make zone label coordinates
regions$x_lab <- regions[, 2] + 0.50
regions$y_lab <- abs(regions[, 5]) - 0.50

# Plot all 16 regions
ggplot() + 
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 0.50), 
    name = "\nWidth of Strike Zone Area (in ft.)") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1.00), 
    name = "Height of Strike Zone Area (in ft.)\n") + 
  geom_rect(aes(xmin = -2, xmax = 2, ymin = 0.5, ymax = 4.5),
    fill = "gray98", color = "dodgerblue") +
  geom_rect(data = regions, aes(xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax), 
    color = "dodgerblue", fill = "NA", linetype = 1) +
  geom_rect(aes(xmin = -0.811, xmax = 0.811, ymin = 1.54, ymax = 3.56),
    color = "orangered", fill = "NA", linetype = 1, size = 0.5) + 
  geom_label(data = regions, aes(x = x_lab, y = y_lab, 
    label = region), size = 6) + theme_light() + 
  theme(panel.grid.minor = element_line(linetype = "blank"))
