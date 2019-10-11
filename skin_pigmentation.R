# human skin pigmentation 

allele_freq <- read.csv("allele_freq_data_human_pigmentation_genes.csv")

fst_vals <- read.csv("fst_values_human_skin_pigmentation.csv")

library(dplyr)


jazz <- read.csv("http://emptymind.org/r/datasets/jazz.csv", sep=",")
jazz <- jazz[order(jazz$G),]
row.names(jazz) <- jazz$Player
jazz1=subset(jazz,select=-Player)
jazz_matrix <- data.matrix(jazz1)
jazz_heatmap <- heatmap(jazz_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(3,3))
jazz_heatmap <- heatmap(jazz_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(3,3))

colnames(allele_freq)[c(2, 4:length(colnames(allele_freq)))]

allele_freq[-c(54:55), ]

alleles <- allele_freq %>% select(., colnames(allele_freq)[c(4:length(colnames(allele_freq)))])

allele_mat <- data.matrix(alleles)

row.names(allele_mat) <- allele_freq$population_name




heatmap(allele_mat, Rowv=NA, Colv=NA, col = cm.colors(256), scale = "column")


library("pheatmap")
pheatmap(allele_mat)



