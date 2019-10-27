# dplyr lessons

library(dplyr)

genes <- read.csv("Genetic_dataset_Coenonympha.txt", header = TRUE, sep = " ")

sum(genes$X6_46[genes$Population == "ACQ"] == 2, na.rm = TRUE)

for (pop_name in unique(genes$Population)){
  print(sum(genes$X6_46[genes$Population == pop_name] == 2, na.rm = TRUE))
}


# select()
sub_genes <- select(genes, Ind, Population, Species, X6_46)
all_genes <- select(genes, starts_with("X"))

# filter()
genes_sp4 <- filter(genes, Species == 4)

# group_by()
genes_df_grouped <- group_by(genes, Species)

# %>% piping
counts_by_species <- genes %>%
  select(Ind, Population, Species,X6_46)

# piping piper
counts_geno_by_species <- genes %>%
  select(Ind, Population, Species, X6_46) %>%
  filter(!is.na(X6_46)) %>%
  group_by(Species) %>%
  summarize(number_AA = sum(X6_46 == 0),
            number_Aa = sum(X6_46 == 1),
            number_aa = sum(X6_46 == 2))

# piping piper
counts_geno_by_pop <- genes %>%
  select(Ind, Population, Species, X6_46) %>%
  filter(!is.na(X6_46)) %>%
  group_by(Population) %>%
  summarize(number_AA = sum(X6_46 == 0),
            number_Aa = sum(X6_46 == 1),
            number_aa = sum(X6_46 == 2))

# with out explicitly stating the three genotypes, 
counts_geno_by_species <- genes %>%
  select(Ind, Population, Species, X6_46) %>%
  filter(!is.na(X6_46)) %>%
  group_by(Species, X6_46) %>%
  count()

# 
counts_geno_by_pop <- genes %>%
  select(Ind, Population, Species, X6_46) %>%
  filter(!is.na(X6_46)) %>%
  group_by(Population) %>%
  summarize(number_AA = sum(X6_46 == 0),
            number_Aa = sum(X6_46 == 1),
            number_aa = sum(X6_46 == 2)) %>%
  mutate(freq_p = (2*number_AA + 1* number_Aa) / 
           sum(2*number_AA + 2 * number_Aa + 2 * number_aa))



