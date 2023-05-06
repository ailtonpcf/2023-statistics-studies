library(tidyverse)

bac_abundace <- read_csv("raw/bacteria.species.raw.csv")

metadata <- read_tsv("raw/biosamples.tsv")

long_bac_abund <- bac_abundace %>% 
  pivot_longer(-sample_id, names_to = "genus", values_to = "abundance")

sub_meta <- metadata %>% 
  select(sample_id, bioproject_id, environment_group)

long_bac_abund %>% 
  left_join(sub_meta) %>% 
  relocate(sample_id, bioproject_id, genus, environment_group, abundance)
