## Identify candidate genes
# Mark Ravinet - 26/11/2021

# clear environment
rm(list = ls())

# load packages (uncomment to install)
#install.packages("tidyverse")
library(tidyverse)

# read in the selection scan data
house_bac <- read_tsv("./house_bac_xpEHH.tsv")

# read in the gff
gff <-  read_tsv("./house_sparrow_chr8.gff", col_names = FALSE)
# subset and clear up the gff - add names
colnames(gff) <- c("chr", "source", "feature", "start", "end", "score",
                   "strand", "frame", "attribute")

# select genes only
new_gff <- gff %>% filter(feature == "gene")
# arrange the gff
new_gff <- new_gff %>% arrange(start, end)
# make a gene mid point variable
new_gff <- new_gff %>% mutate(mid = start + (end-start)/2)

# plot selection scan again
ggplot(house_bac, aes(position, logpvalue)) + geom_point()

# identify the highest peak of selection
hits <- house_bac %>% arrange(desc(logpvalue)) %>% top_n(3)

# find the nearest genes to our highest hit
x <- hits$position[1]

# find hits closest to genes
new_gff %>% mutate(hit_dist = abs(mid - x)) %>% arrange(hit_dist)

# find hits within 250 Kb
gene_hits <- new_gff %>% mutate(hit_dist = abs(mid - x)) %>% arrange(hit_dist) %>% filter(hit_dist < 250000)

# what are these genes?
gene_hits <- gene_hits %>% select(chr, start,end, attribute,hit_dist)
# separate out the attribute column
gene_hits %>% pull(attribute)
