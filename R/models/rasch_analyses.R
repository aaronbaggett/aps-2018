# --------------------------------------------------------------
# MODEL: Umpire accuracy dotplot, 2015-2017
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

if(!require(ltm)){
  install.packages("ltm")
  library(ltm)
}

if(!require(psych)){
  install.packages("psych")
  library(psych)
}

if(!require(viridis)){
  install.packages("viridis")
  library(viridis)
}

if(!require(xtable)){
  install.packages("xtable")
  library(xtable)
}

# Load PITCHf/x data
load("data/pfx_items.rda")
load("data/pfx_items_rhb.rda")
load("data/pfx_items_lhb.rda")

# Rasch Model 1: All batters (RHB/LHB)
fit1 <- rasch(pfx_items, constraint = cbind(length(pfx_items) + 1, 1))
summary(fit1)
coef(fit1, prob = TRUE)
print(xtable(coef(fit1, prob = TRUE)), include.rownames = FALSE)

fit1_preds <- data.frame(region = 1:16, coef(fit1, prob = TRUE, order = FALSE), 
  row.names = NULL)
names(fit1_preds) <- c("region", "difficulty", "discrimination", "prob")

print(xtable(fit1_preds %>% 
    mutate(prob_diff = prob - mean(prob)) %>% 
    arrange(prob), digits = 3), include.rownames = FALSE)

# prob <- list()
# for (i in 1:17){
#   prob[[i]] <- plot(fit1)[, i]
#   prob <- data.frame(prob)
# }

Ability <- rep(prob[, 1], 16)
Region <- rep(paste0("Region ", 1:16), each = 100)
Probability <- reshape2::melt(prob[, 2:17])[, 2]
df <- data.frame(Ability, Region, Probability)
df$Region <- ordered(df$Region, levels = c("Region 1", "Region 2", "Region 3", 
  "Region 4", "Region 5", "Region 6", "Region 7", "Region 8", "Region 9", 
  "Region 10", "Region 11", "Region 12", "Region 13", "Region 14", 
  "Region 15", "Region 16"))

# Check discriminability for Model 1
df %>% 
  group_by(Region) %>% 
  summarize(mean = mean(Probability),
    sd = sd(Probability)) %>% 
  arrange(desc(mean))

# Remove select regions
df <- df %>% 
  filter(!Region %in% c("Region 1", "Region 4", "Region 13",
    "Region 14", "Region 16"))

# Plot Model 1 ICCs
ggplot(data = df, aes(x = Ability, y = Probability, color = Region)) + 
  geom_line(size = 1.25) + scale_color_viridis(discrete = TRUE) + 
  scale_x_continuous(name = "\nAbility") +
  scale_y_continuous(breaks = seq(0, 1, 0.10), name = "Probabity\n") +
  theme_bw() + theme(axis.title = element_text(size = 16, 
    face = "bold"), axis.text = element_text(size = 13), 
    plot.title = element_text(size = 16, 
        face = "bold"), legend.text = element_text(size = 13), 
    legend.title = element_text(size = 16, 
        face = "bold"), legend.position = "right", 
    legend.direction = "vertical")# + labs(title = "Item Characteristic Curves")

# Rasch Model 2 (RHB)
fit2 <- rasch(pfx_items_rhb, constraint = cbind(length(pfx_items_rhb) + 1, 1))
summary(fit2)
coef(fit2, prob = TRUE)
print(xtable(coef(fit2, prob = TRUE)), include.rownames = FALSE)

fit2_preds <- data.frame(region = 1:16, coef(fit2, prob = TRUE, order = FALSE), 
  row.names = NULL)
names(fit2_preds) <- c("region", "difficulty", "discrimination", "prob")

print(xtable(fit2_preds %>% 
    mutate(prob_diff = prob - mean(prob)) %>% 
    arrange(prob), digits = 3), include.rownames = FALSE)

# Rasch Model 3 (LHB)
fit3 <- rasch(pfx_items_lhb, constraint = cbind(length(pfx_items_lhb) + 1, 1))
summary(fit3)
coef(fit3, prob = TRUE)
print(xtable(coef(fit2, prob = TRUE)), include.rownames = FALSE)

fit3_preds <- data.frame(region = 1:16, coef(fit3, prob = TRUE, order = FALSE), 
  row.names = NULL)
names(fit3_preds) <- c("region", "difficulty", "discrimination", "prob")

print(xtable(fit3_preds %>% 
    mutate(prob_diff = prob - mean(prob)) %>% 
    arrange(prob), digits = 3), include.rownames = FALSE)
