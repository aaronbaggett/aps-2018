# IRT Data Setup --------------------------------------------------------------
# LHB ---------------------------------------------------------------------

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
region_1 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 1 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_2 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 2 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_3 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 3 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_4 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 4 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_5 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 5 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_6 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 6 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_7 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 7 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_8 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 8 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_9 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 9 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_10 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 10 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_11 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 11 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_12 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 12 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_13 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 13 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_14 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 14 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_15 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 15 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

region_16 <- pfx_dat %>% 
  ungroup() %>% 
  filter(region == 16 & stand == "L") %>% 
  sample_n(10000, replace = TRUE) %>% 
  select(u_test)

# Build data.frame (This is our data. I hope.)
(pfx_items_lhb <- data.frame(region_1, region_2, region_3, region_4, region_5, region_6,
  region_7, region_8, region_9, region_10, region_11, region_12, region_13, region_14, region_15, region_16))

names(pfx_items_lhb) <- paste0("region", sep = "_", c(1:16))

save(pfx_items_lhb, file = "/Users/AB/Dropbox/PITCHfx_IRT/data/pfx_items_lhb.rda")

# Calculate sample means by zone for comparison
samp_means_lhb <- list()
for(i in 1:13){
  samp_means_lhb[[i]] <- colMeans(pfx_items_lhb)[[i]]
}

samp_means_lhb <- unlist(samp_means_lhb)

# Calculate population means by zone
pop_means_lhb <- pfx_dat %>% 
  group_by(region) %>% 
  filter(stand == "L") %>% 
  summarize(pop = mean(u_test))

# Add zone column
zone <- factor(rep(c(1:9, 11:14)))

# Mean comparison df
(mean_comp_lhb <- data.frame(zone, sample = samp_means_lhb, 
  pop = pop_means_lhb$pop))

# Compare means by sample/pop.
ggplot(data = mean_comp_lhb, aes(zone, pop, group = 1)) + 
  geom_point(color = "tomato", alpha = .75, size = 2.5) + 
  geom_point(aes(zone, sample), color = "dodgerblue3", 
    alpha = .5, size = 2.5) + theme_classic()
