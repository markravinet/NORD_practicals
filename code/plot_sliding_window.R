## Plot the sliding window analysis
# Mark Ravinet - 26/11/2021

# clear environment
rm(list = ls())

# load packages (uncomment to install)
#install.packages("tidyverse")
library(tidyverse)

# set variable
infile <- "~/genome_scan/house_bac_div_stats.csv"

# read data
house_bac <- read_csv(infile)

# plot fst
ggplot(house_bac, aes(mid, fst_bac_house)) + geom_line()

# plot dxy
ggplot(house_bac, aes(mid, dxy_bac_house)) + geom_line()

# plot pi
# first gather the data
pi <- house_bac %>%
  select(mid, contains("pi")) %>%
  pivot_longer(cols = contains("pi"), names_to = "species", values_to = "pi")
# then plot it
ggplot(pi, aes(mid, pi, colour = species)) + geom_line()
