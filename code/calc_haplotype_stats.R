## calculate haplotype homozygosity statistics
# Mark Ravinet - 26/11/2021

# clear environment
rm(list = ls())

# load packages (uncomment to install)
#install.packages("rehh")
library(rehh)
#install.packages("tidyverse")
library(tidyverse)

# read in data for each species
# house
house_hh <- data2haplohh(hap_file = "./rehh/house_chr8.vcf.gz",
                         polarize_vcf = FALSE)
# bactrianus
bac_hh <- data2haplohh(hap_file = "./rehh/bac_chr8.vcf.gz",
                       polarize_vcf = FALSE)

# filter on MAF - here 0.05
house_hh_f <- subset(house_hh, min_maf = 0.05)
bac_hh_f <- subset(bac_hh, min_maf = 0.05)

# perform scans
house_scan <- scan_hh(house_hh_f, polarized = FALSE)
bac_scan <- scan_hh(bac_hh_f, polarized = FALSE)

# perform xp-ehh
house_bac <- ies2xpehh(bac_scan, house_scan,
                       popname1 = "bactrianus", popname2 = "house",
                       include_freq = T)

# plot xpEHH
ggplot(house_bac, aes(POSITION, XPEHH_bactrianus_house)) + geom_point()

# plot log-pvalue
ggplot(house_bac, aes(POSITION, LOGPVALUE)) + geom_point()

# find the highest hit
hit <- house_bac %>% arrange(desc(LOGPVALUE)) %>% top_n(1)
# get SNP position
x <- hit$position

# identify markers
marker_id_h <- which(house_hh_f@positions == x)

# calculate haplotype bifurcation
house_furcation <- calc_furcation(house_hh_f, mrk = marker_id_h)
bac_furcation <- calc_furcation(bac_hh_f, mrk = marker_id_b)
marker_id_b <- which(bac_hh_f@positions == x)

# plot bifurcation at this point
plot(house_furcation, xlim = c(19.18E+6, 19.22E+6))
plot(bac_furcation, xlim = c(19.18E+6, 19.22E+6))

# calculate haplotype length
house_haplen <- calc_haplen(house_furcation)
bac_haplen <- calc_haplen(bac_furcation)

# plot haplotype length
plot(house_haplen)
plot(bac_haplen)

# write out house bactrianus xpEHH
house_bac <- tbl_df(house_bac)
colnames(house_bac) <- tolower(colnames(house_bac))
write_tsv(house_bac, "./house_bac_xpEHH.tsv")
