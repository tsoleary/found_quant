# population genetics ----------------------------------------------------------


snps <- read.table("Genetic_dataset_Coenonympha.txt")

prop_na <- sum(is.na(snps[4:length(snps)]))/(nrow(snps)*length(4:length(snps)))

prop_hom_min <- sum(snps[4] == 0, na.rm = TRUE)/(nrow(snps[4]) - sum(is.na(snps[4])))

prop_het <- sum(snps[4] == 1, na.rm = TRUE)/(nrow(snps[4]) - sum(is.na(snps[4])))

prop_hom_maj <- sum(snps[4] == 2, na.rm = TRUE)/(nrow(snps[4]) - sum(is.na(snps[4])))

##

total_count <- as.numeric(nrow(snps[4]) - sum(is.na(snps[4])))

c_hom_min <- as.numeric(sum(snps[4] == 0, na.rm = TRUE))

c_het <- as.numeric(sum(snps[4] == 1, na.rm = TRUE))

c_hom_maj <- as.numeric(sum(snps[4] == 2, na.rm = TRUE))


p <- (2 * c_hom_maj + c_het) / (total_count * 2)
q <- 1 - p

# code from Tibault ------------------------------------------------------------

library(ade4)
library(adegenet)
#BiocManager::install("LEA")
library(LEA)
library(pcadapt)

#############
#### PCA ####

GenData <- snps

TAB <- GenData[,-c(1:3)]
TAB <- apply(TAB, 2, as.numeric)
for (i in 1:nrow(TAB)) {
  TAB[i, which(is.na(TAB[i,]))]<-median(TAB[i,-which(is.na(TAB[i,]))], na.rm=TRUE)
}

pca <- dudi.pca(TAB, scannf = F, nf = 4)
s.class(pca$li[,c(1,2)], fac = as.factor(GenData$Species), col = c("black", "#F9D017", "#377EB8", "#4DAF4A"))


##############
#### sNMF ####

# Genetic dataset
GenData <- snps
# Running the program with a certain number of K
write.geno(GenData[,-c(1:3)], "genotypes.geno")
object <- snmf("genotypes.geno", K = 1:5, entropy = T, ploidy = 2, alpha = 100, project = "new", repetitions = 10)

# Best runs
ce <- unlist(lapply(1:5, function(x) which.min(cross.entropy(object, K = x))))

# Proba
proba <- lapply(1:5, function(x) Q(object, K = x, run = ce[x]))

# Sort by species
groups <- GenData$Species 
proba <- lapply(proba, function(x) x[order(groups, decreasing = T),])
groups <- groups[order(groups, decreasing = T)]

# Plot
seps = (as.integer(as.factor(groups)) - c(1, as.integer(as.factor(groups[-length(groups)]))))
opar <- par(no.readonly = T)
layout(mat = matrix(c(rep(c(1:2),4)),byrow = F, ncol = 4), heights = c(rep(12,17), 8))
par(mar=c(0,3,1,4))
cols=c("black", "#F9D017", "#377EB8", "#4DAF4A")
barplot(t(proba[[4]]), las=2, names = rep("", ncol(t(proba[[4]]))), axes = F, border = NA, space=0, col=cols)
abline(v=(which(seps!=0)-1), lty=1, lwd=1)
mtext(text = paste0('K = ', as.character(4)), side = 4, las=1)
axis(side=2,pos=0, las=2, at = pretty(c(0,1), n=5), labels=c('0%', rep("",4), '100%'))
coord <- 1:nrow(proba[[2]])
coord <- unlist(lapply(as.character(unique(groups)), function(x) mean(coord[as.character(groups)==x])))
axis(side=1, at = coord, labels=as.character(unique(groups)), cex.axis=1, las=2)
#dev.off()

###########################
#### Genetic diversity ####

GenData_sp <- split(GenData[,-c(1:3)], f = GenData$Species)

# Frequency of heterozygote genotypes for each site and each species
Hobs <- lapply(GenData_sp, function(x) apply(x, 2, function(y) sum(y==1, na.rm = T)/(2*sum(!is.na(y)))))

# Allele frequency for each site within each species
freq_pop <- lapply(GenData_sp, function(x) apply(x, 2, function(y) sum(y, na.rm = T)/(2*sum(!is.na(y)))))

# Hexpected heterozygosity
Hexp <- lapply(freq_pop, function(x) 2*x*(1-x))

# Mean genetic diversity in each species
Hexp_mean <- unlist(lapply(Hexp, function(x) mean(x,na.rm = T)))

# Comparison Hexp and Hobs
plot(Hobs[[1]], Hexp[[1]], pch = 16, col = "gray35")
abline(0,1, lwd = 3, lty = 3, col="red")





