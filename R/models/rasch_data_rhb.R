# IRT Data Setup --------------------------------------------------------------
# RHB ---------------------------------------------------------------------

# Load package libraries (if necessary)
library(tidyverse)

# Load PITCHf/x data
load("~/Dropbox/PITCHfx_IRT/data/pfx_15-17.rda")

# Take sample of decisions from each zone (item)
# 
# Note: This is super redundant. The following needs debugging.
# There is somehing wrong here. The sample mean accuracy rates are not 
# comparable to the population accuracy rates
# samp_zones <- list()
# for(i in c(1:9, 11:14)){
#   samp_zones[[i]] <- sample(pfx_dat$u_test[pfx_dat$region == i], 10000, 
#     replace = TRUE)
# }
# 
# irt_zones <- as_data_frame(matrix(unlist(samp_zones), nrow = 10000, 
#   byrow = TRUE), stringsAsFactors = FALSE)
# 
# names(irt_zones) <- paste0("zone", sep = "_", c(1:9, 11:14))

# Take sample of n = 10000 from each zone
reg_1 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 1 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_2 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 2 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_3 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 3 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_4 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 4 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_5 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 5 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_6 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 6 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_7 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 7 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_8 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 8 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_9 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 9 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_10 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 10 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_11 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 11 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_12 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 12 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_13 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 13 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_14 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 14 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_15 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 15 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

reg_16 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 16 & stand == "R") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

# Build data.frame (This is our data. I hope.)
(pfx_items_rhb <- data.frame(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6,
  reg_7, reg_8, reg_9, reg_10, reg_11, reg_12, reg_13, reg_14, reg_15, reg_16))

names(pfx_items_rhb) <- paste0("region", sep = "_", c(1:16))

save(pfx_items_rhb, file = "/Users/AB/Dropbox/PITCHfx_IRT/data/pfx_items_rhb.rda")

# Calculate sample means by zone for comparison
samp_means_rhb <- list()
for(i in 1:13){
  samp_means_rhb[[i]] <- colMeans(pfx_items_rhb)[[i]]
}

samp_means_rhb <- unlist(samp_means_rhb)

# Calculate population means by zone
pop_means_rhb <- pfx_dat %>% 
  group_by(region) %>% 
  filter(stand == "R") %>% 
  summarize(pop = mean(u_test))

# Add zone column
zone <- factor(rep(c(1:9, 11:14)))

# Mean comparison df
(mean_comp_rhb <- data.frame(zone, sample = samp_means_rhb, 
  pop = pop_means_rhb$pop))

# Compare means by sample/pop.
ggplot(data = mean_comp_rhb, aes(zone, pop, group = 1)) + 
  geom_point(color = "tomato", alpha = .75, size = 2.5) + 
  geom_point(aes(zone, sample), color = "dodgerblue3", 
    alpha = .5, size = 2.5) + theme_classic()
