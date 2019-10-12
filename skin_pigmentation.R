# human skin pigmentation 

allele_freq <- read.csv("allele_freq_data_human_pigmentation_genes.csv")
allele_freq <- allele_freq[-c(54:55), ]
fst_vals <- read.csv("fst_values_human_skin_pigmentation.csv")

library(dplyr)

# BiocManager::install("ComplexHeatmap")
library("ComplexHeatmap")

# Example online --------------
# jazz <- read.csv("http://emptymind.org/r/datasets/jazz.csv", sep=",")
# jazz <- jazz[order(jazz$G),]
# row.names(jazz) <- jazz$Player
# jazz1=subset(jazz,select=-Player)
# jazz_matrix <- data.matrix(jazz1)
# jazz_heatmap <- heatmap(jazz_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), 
#                         scale="column", margins=c(3,3))
# jazz_heatmap <- heatmap(jazz_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), 
#                         scale="column", margins=c(3,3))

# 

allele_mat <- allele_freq %>% 
  select(., colnames(allele_freq)[c(4:length(colnames(allele_freq)))]) %>% 
  data.matrix(.)

row.names(allele_mat) <- allele_freq$population_name

Heatmap(allele_mat, row_names_side = "left", row_names_gp = gpar(cex = 0.6), 
        row_names_centered = TRUE, 
        row_dend_width = unit(3, "cm"),
        show_heatmap_legend = TRUE,
        heatmap_legend_param = list(title = "Allele Freq"))

# try scaling the columns to make them more similar in color
allele_mat_scaled <- scale(allele_mat)

x <- (allele_mat_scaled + abs(min(allele_mat_scaled)))
y <- x/max(x)

Heatmap(y, row_names_side = "left", row_names_gp = gpar(cex = 0.6), 
        row_names_centered = TRUE, 
        row_dend_width = unit(3, "cm"),
        show_heatmap_legend = TRUE,
        heatmap_legend_param = list(title = "Scaled\nAllele Freq"))
