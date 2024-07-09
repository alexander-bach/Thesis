###############################################################

# Grassy field margins revisited: Long term development of 
# taxonomic and functional diversity of spiders in intensively 
# used agricultural landscapes in Western Germany

#Bach 2023

###############################################################

# R code to reproduce the analysis of chapter 5 in the dissertation
# "Empowering ecological research by improving access and usability
# of biodiversity data : An arachnological perspective"

# for more informations: 10.18154/RWTH-2023-08502

# Author: Alexander Bach
# Last update: 23.02.2023

###############################################################

library(here)
library(reshape2)
library(reshape)
library(vegan)
library(adespatial)
library(FD)
library(indicspecies)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggpubr)

#### import data ####

# import spider data
A2002 <- read.csv2(here("A2002.csv"), fileEncoding = "UTF-8-BOM") 
A2018 <- read.csv2(here("A2018.csv"), fileEncoding = "UTF-8-BOM")
J2018 <- read.csv2(here("J2018.csv"), fileEncoding = "UTF-8-BOM")
J2001 <- read.csv2(here("J2001.csv"), fileEncoding = "UTF-8-BOM")

# import spider traitdata
TraitData <- read.csv2(here("TraitData.csv"), fileEncoding = "UTF-8-BOM", row.names = 1, sep = "\t")
rownames(TraitData) <- make.cepnames(rownames(TraitData), seconditem = TRUE)

# import SiteData
Sites_A1 <- read.csv2(here("Sites_t0.csv"), fileEncoding = "UTF-8-BOM", sep ="\t")
Sites_A2 <- read.csv2(here("Sites_t1.csv"), fileEncoding = "UTF-8-BOM", sep ="\t")

# import vegetation data
VegData <- read.csv2(here("VegData.csv"), fileEncoding = "UTF-8-BOM", sep =";", row.names = 1)
rownames(VegData) <- make.cepnames(rownames(VegData))

#### Analysing vegetational data ####


# cut and transpose vegetational data 
Veg_t0 <- VegData[,1:10]
Veg_t1 <- VegData[,11:20]

Veg_t0_t <- t(Veg_t0) %>% as.data.frame()
Veg_t1_t <- t(Veg_t1) %>% as.data.frame()

# calculate shifting species between t0 and t1
res <- tpaired.krandtest(Veg_t0_t, Veg_t1_t, nperm = 999, list.all = TRUE)
sig_veg <- res$t.tests %>% filter(`p<=0.05` == "*")
sig_pos_veg <- rownames(sig_veg)[sig_veg$`Sign(T1-T2)` == -1]
sig_neg_veg <- rownames(sig_veg)[sig_veg$`Sign(T1-T2)` == 1]

# NMDS Analysis
# Define common parameters
colvec <- c("lightgrey", "darkgrey")
cex <- 1.3

# Function to plot NMDS with hulls and indicator species
plot_nmds <- function(nmds, grp, sig_pos, sig_neg, x_range, y_range, xlab, ylab, pos_adj, neg_adj) {
  plot(x_range, y_range, xlab = xlab, ylab = ylab, type = "n") 
  ordihull(nmds, grp, col = colvec, lwd = 1, lty = 0, draw = "polygon")
  
  species_score <- scores(nmds)
  species_score <- as.data.frame(species_score$species)
  
  subset_spec_score <- subset(species_score, rownames(species_score) %in% sig_pos)
  subset_spec_score$spec <- row.names(subset_spec_score)
  row.names(subset_spec_score) <- subset_spec_score$spec
  subset_spec_score$spec <- NULL
  
  subset_spec_score1 <- subset(species_score, rownames(species_score) %in% sig_neg)
  subset_spec_score1$spec <- row.names(subset_spec_score1)
  row.names(subset_spec_score1) <- subset_spec_score1$spec
  subset_spec_score1$spec <- NULL
  
  with(subset_spec_score, points(subset_spec_score, col = "purple", pch = 19, cex = 1.3))
  text(subset_spec_score, labels = rownames(subset_spec_score), col = "purple", cex = 1, adj = pos_adj)
  
  with(subset_spec_score1, points(subset_spec_score1, col = "red", pch = 19, cex = 1.3))
  text(subset_spec_score1, labels = rownames(subset_spec_score1), col = "red", cex = 1, adj = neg_adj)
}

# delete empty species from both datasets
Veg_t0_ts <- Veg_t0_t[, colSums(Veg_t0_t) != 0]
Veg_t1_ts <- Veg_t1_t[, colSums(Veg_t1_t) != 0]

# perfom NMDS
V1.nmds <- metaMDS(Veg_t0_ts, k = 4, try = 20, trymax = 500)
V2.nmds <- metaMDS(Veg_t1_ts, k = 4, try = 20, trymax = 500)

# plot NMDS
par(cex.lab = cex, cex.axis = cex, cex.main = cex, mfrow = c(1, 2))

plot_nmds(V1.nmds, Sites_A1$Grp, sig_pos_veg, sig_neg_veg, c(-1.5, 1.5), c(-1.2, 1), "NMDS1", "NMDS2", c(-0.2, 0), c(1.1, 0))
plot_nmds(V2.nmds, Sites_A1$Grp, sig_pos_veg, sig_neg_veg, c(-0.5, 0.5), c(-0.5, 0.6), "NMDS1", "", c(1.1, 0.5), c(-0.1, 0.1))

# perform procrustes analysis
summary(procrustes(V1.nmds, V2.nmds))
protest(V1.nmds, V2.nmds, symmetric = TRUE)

#### Analysing spider data ####

# standardise spider data according to Saska et al (2021)
A2002$NumberOfUnits <- (A2002$NumberOfUnits / 176 / 4) 
A2018$NumberOfUnits <- (A2018$NumberOfUnits / 196 / 5) 
J2001$NumberOfUnits <- (J2001$NumberOfUnits / 208 / 4) 
J2018$NumberOfUnits <- (J2018$NumberOfUnits / 196 / 5)

spider_t0 <- rbind(A2002, J2001)
spider_t1 <- rbind(A2018, J2018)

# create crosstables for t0 and t1
spider_t0 <- cast(spider_t0, TaxonomicName~LocalityDescription, 
           value = "NumberOfUnits", sum)
row.names(spider_t0) <- spider_t0$TaxonomicName
spider_t0$TaxonomicName <- NULL
spider_t0 <- spider_t0[ order(row.names(spider_t0)), ]

spider_t1 <- cast(spider_t1, TaxonomicName~LocalityDescription, 
           value = "NumberOfUnits", sum)
row.names(spider_t1) <- spider_t1$TaxonomicName
spider_t1$TaxonomicName <- NULL
spider_t1 <- spider_t1[ order(row.names(spider_t1)), ]

rownames(spider_t0) <- make.cepnames(rownames(spider_t0), seconditem = TRUE)
spider_t0 <- as.data.frame(spider_t0)
rownames(spider_t1) <- make.cepnames(rownames(spider_t1), seconditem = TRUE)
spider_t1 <- as.data.frame(spider_t1)

# calcualte biomass
spider_t0_BM <- spider_t0
spider_t0_BM[spider_t0_BM > 0] <- NA

for(i in 1:nrow(spider_t0)){
  spider_t0_BMs <- TraitData[i, 5] * spider_t0[i, ]
  spider_t0_BM[i,] <- spider_t0_BMs
}

spider_t1_BM <- spider_t1
spider_t1_BM[spider_t1_BM > 0] <- NA

for(i in 1:nrow(spider_t1)){
  spider_t1_BMs <- TraitData[i, 5] * spider_t1[i, ]
  spider_t1_BM[i,] <- spider_t1_BMs
}

# transpose spider table
spider_t0_t <- t(spider_t0) %>% as.matrix() %>% as.data.frame()
spider_t1_t <- t(spider_t1) %>% as.matrix() %>% as.data.frame()

# calculate biodiversity parameters
col_sums_t0 <- colSums(spider_t0, na.rm = TRUE)
col_sums_t1 <- colSums(spider_t1, na.rm = TRUE)
biomass_sum_t0 <- colSums(spider_t0_BM, na.rm = TRUE)
biomass_sum_t1 <- colSums(spider_t1_BM, na.rm = TRUE)
non_zero_rows_t0 <- apply(spider_t0, 2, function(x) sum(x != 0))
non_zero_rows_t1 <- apply(spider_t1, 2, function(x) sum(x != 0))
BioDivParam_Spider_t0 <- data.frame(ActivityDensity = col_sums_t0, Biomass = biomass_sum_t0, SpeciesRichness = non_zero_rows_t0)
BioDivParam_Spider_t1 <- data.frame(ActivityDensity = col_sums_t1, Biomass = biomass_sum_t1, SpeciesRichness = non_zero_rows_t1)

BioDivParam_Spider <- rbind(BioDivParam_Spider_t0, BioDivParam_Spider_t1)
rm(BioDivParam_Spider_t0, BioDivParam_Spider_t1, col_sums_t0, col_sums_t1, biomass_sum_t0, biomass_sum_t1, non_zero_rows_t0, non_zero_rows_t1)

#calculate shifting spider species between t0 and t1
res1 <- tpaired.krandtest(spider_t0_t, spider_t1_t, nperm = 999, list.all = TRUE)
sig_spi <- res1$t.tests %>% filter(`p<=0.05` == "*")
sig_pos_spi <- rownames(sig_spi)[sig_spi$`Sign(T1-T2)` == -1] 
sig_neg_spi <- rownames(sig_spi)[sig_spi$`Sign(T1-T2)` == 1] 

#NMDS analysis
#transform data
spider_t0_ts <- spider_t0_t[, colSums(spider_t0_t) != 0]
spider_t1_ts <- spider_t1_t[, colSums(spider_t1_t) != 0]

# perfom NMDS
U1.nmds <- metaMDS(spider_t0_ts, k = 3, try = 20, trymax = 500)
U2.nmds <- metaMDS(spider_t1_ts, k = 3, try = 20, trymax = 500)

# plot NMDS
par(cex.lab = cex, cex.axis = cex, cex.main = cex, mfrow = c(1, 2))

plot_nmds(U1.nmds, Sites_A1$Grp, sig_pos_spi, sig_neg_spi, c(-1.5, 1.5), c(-1, 1.4), "NMDS1", "NMDS2", c(-0.2, 0), c(1.1, 0))
plot_nmds(U2.nmds, Sites_A1$Grp, sig_pos_spi, sig_neg_spi, c(-1.5, 1.5), c(-1, 1.4), "NMDS1", "", c(1.1, 0.5), c(-0.1, 0.1))

# perform procrustes analysis
summary(procrustes(V1.nmds, V2.nmds))
protest(V1.nmds, V2.nmds, symmetric = TRUE)

# BC Plot
# create data.frame for species richness BC Plot
spider_t0_tp <- decostand(spider_t0_t, "pa")
spider_t1_tp <- decostand(spider_t1_t, "pa")

BCPlot1 <- TBI(spider_t0_t, spider_t1_t, nperm = 999, test.t.perm = TRUE, test.BC = TRUE, method = "%difference", pa.tr = FALSE)
BCPlot2 <- TBI(spider_t0_tp, spider_t1_tp, nperm = 999, test.t.perm = TRUE, test.BC = TRUE, method = "sorensen", pa.tr = FALSE)
BCPlot3 <- TBI(t(spider_t0_BM), t(spider_t1_BM), nperm = 999, test.t.perm = TRUE, test.BC = TRUE, method = "%difference", pa.tr = FALSE)

# build plots
plot(BCPlot1, pch.loss = 21, s.names = c("JU2", "JU3", "JU4", "JU5", "JU1","AC1", "AC2", "AC3", "AC4", "AC5"),
     pch.gain = 22, cx.symb = 2, main = "B-C Plot, Activitiy density", xlim = c(0,0.9), ylim = c(0, 0.6))
plot(BCPlot2, pch.loss = 21, s.names = c("JU2", "JU3", "JU4", "JU5", "JU1","AC1", "AC2", "AC3", "AC4", "AC5"),
     pch.gain = 22, cx.symb = 2, main = "B-C Plot, Species richness", xlim = c(0,0.9), ylim = c(0, 0.6))
plot(BCPlot3, pch.loss = 21, s.names = c("JU2", "JU3", "JU4", "JU5", "JU1","AC1", "AC2", "AC3", "AC4", "AC5"),
     pch.gain = 22, cx.symb = 2, main = "B-C Plot, Biomass", xlim = c(0,0.9), ylim = c(0, 0.6))

# analyze functional diversity
# data transformation
spider_t0_t_nonzero = spider_t0_t[,colSums(spider_t0_t) != 0]
spider_t1_t_nonzero = spider_t1_t[,colSums(spider_t1_t) != 0]

TraitFDIS <- TraitData[,c(4,6,8:10)]
row.names(TraitFDIS) <- colnames(spider_t0_t)

i1 <- (colSums(spider_t0_t, na.rm=T) != 0) # T if colSum is not 0, F otherwise
spider_t0_t_nonzero <- spider_t0_t[, i1]
Traits_t0 <- TraitFDIS[i1,]

i2 <- (colSums(spider_t1_t, na.rm=T) != 0) # T if colSum is not 0, F otherwise
spider_t1_t_nonzero <- spider_t1_t[, i2]
Traits_t1 <- TraitFDIS[i2,]

# calculate functional diversity
FDiv_1 <- dbFD(Traits_t0, spider_t0_t_nonzero, corr = "lingoes", m = "min", stand.FRic = TRUE, CWM.type = "all")
FDiv_2 <- dbFD(Traits_t1, spider_t1_t_nonzero, corr = "lingoes", m = "min", stand.FRic = TRUE, CWM.type = "all")

# build results table
FDiv1 <- FDiv_1$FDiv
FDis1 <- FDiv_1$FDis
FEve1 <- FDiv_1$FEve
FDiv2 <- FDiv_2$FDiv
FDis2 <- FDiv_2$FDis
FEve2 <- FDiv_2$FEve

func_div1 <- data.frame(FDiv1, FDis1, FEve1)
func_div2 <- data.frame(FDiv2, FDis2, FEve2)
colnames(func_div2) <- c("FDiv1", "FDis1", "FEve1")
func_div <- rbind(func_div1, func_div2)
grp <- c(rep(1, 5), rep(3, 5), rep(2, 5), rep(4, 5))
site <- c(rep(1, 5), rep(2, 5), rep(1, 5), rep(2, 5))
func_div$grp <- grp
func_div$site <- site

# build plots
p1 <- ggplot(func_div, aes(x=factor(site), y=FDiv1)) +
  geom_boxplot(aes(fill = factor(grp)), show.legend = FALSE) +
  theme_pubr() +
  scale_x_discrete(labels=c("1" = "Aachen", "2" = "Jülich"), name = "") +
  scale_fill_manual(values = c("lightgrey", "darkgrey", "lightgrey", "darkgrey")) +
  font("x.text", size = 15, color = "black", face = "bold") +
  font("y.title", size = 15, color = "black", face = "bold") +
  font("y.text", size = 15, color = "black", face = "bold") +
  geom_vline(xintercept = 1.5, linetype="dotted") +
  scale_y_continuous(name="FDiv")

p2 <- ggplot(func_div, aes(x=factor(site), y=FDis1)) +
  geom_boxplot(aes(fill = factor(grp)), show.legend = FALSE) +
  theme_pubr() +
  scale_x_discrete(labels=c("1" = "Aachen", "2" = "Jülich"), name = "") +
  scale_fill_manual(values = c("lightgrey", "darkgrey", "lightgrey", "darkgrey")) +
  font("x.text", size = 15, color = "black", face = "bold") +
  font("y.title", size = 15, color = "black", face = "bold") +
  font("y.text", size = 15, color = "black", face = "bold") +
  geom_vline(xintercept = 1.5, linetype="dotted") +
  scale_y_continuous(name="FDis")

p3 <- ggplot(func_div, aes(x=factor(site), y=FEve1)) +
  geom_boxplot(aes(fill = factor(grp))) +
  theme_pubr() +
  scale_x_discrete(labels=c("1" = "Aachen", "2" = "Jülich"), name = "") +
  scale_fill_manual(values = c("lightgrey", "darkgrey", "lightgrey", "darkgrey")) +
  font("x.text", size = 15, color = "black", face = "bold") +
  font("y.title", size = 15, color = "black", face = "bold") +
  font("y.text", size = 15, color = "black", face = "bold") +
  font("legend.text", face = "bold") +
  geom_vline(xintercept = 1.5, linetype="dotted") +
  scale_y_continuous(name="FEve") 


ggarrange(p2, p3, p1, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 2)

# analyzse community weighted means of significant traits
CWM_traits_1 <- functcomp(TraitData, as.matrix(spider_t0_t), CWM.type = "all")
CWM_traits_2 <- functcomp(TraitData, as.matrix(spider_t1_t), CWM.type = "all")

CWM_traits <- rbind(CWM_traits_1, CWM_traits_2)
CWM_traits$grp <- grp
CWM_traits$site <- site

comp <- list(c("1", "2"), c("3", "4"))
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))

t.test(CWM_traits_1[c(1:5),25], CWM_traits_2[c(1:5),25], paired = TRUE, alternative = "two.sided")

# build plots
c1 <- ggplot(CWM_traits, aes(x=factor(site), y=KL.Median)) +
  geom_boxplot(aes(fill = factor(grp)), show.legend = FALSE) +
  theme_pubr() +
  scale_x_discrete(labels=c("1" = "Aachen", "2" = "Jülich"), name = "") +
  scale_fill_manual(values = c("green4", "green4", "dodgerblue", "dodgerblue")) +
  font("x.text", size = 15, color = "black", face = "bold") +
  font("y.title", size = 15, color = "black", face = "bold") +
  font("y.text", size = 15, color = "black", face = "bold") +
  geom_vline(xintercept = 1.5, linetype="dotted") +
  scale_y_continuous(name=" Body Size (mm)") +
  font("title", size = 15, color = "black", face = "bold")

c2 <- ggplot(CWM_traits, aes(x=factor(site), y=M_Std)) +
  geom_boxplot(aes(fill = factor(grp)), show.legend = FALSE) +
  theme_pubr() +
  scale_x_discrete(labels=c("1" = "Aachen", "2" = "Jülich"), name = "") +
  scale_fill_manual(values = c("green4", "green4", "dodgerblue", "dodgerblue")) +
  font("x.text", size = 15, color = "black", face = "bold") +
  font("y.title", size = 15, color = "black", face = "bold") +
  font("y.text", size = 15, color = "black", face = "bold") +
  geom_vline(xintercept = 1.5, linetype="dotted") +
  scale_y_continuous(name="Moisture") +
  font("title", size = 15, color = "black", face = "bold")

ggarrange(c1, c2, 
          labels = c("CWM Body Size", "   CWM Moisture"),
          ncol = 2, nrow = 2)

# significance tests
t.test(CWM_traits[c(1:5),25], CWM_traits[c(11:15),25], paired = TRUE, alternative = "two.sided") #t.test body size aachen
t.test(CWM_traits[c(6:10),25], CWM_traits[c(16:20),25], paired = TRUE, alternative = "two.sided") #t.test body size jülich
t.test(CWM_traits[c(1:5),43], CWM_traits[c(11:15),43], paired = TRUE, alternative = "two.sided") #t.test moisture aachen
t.test(CWM_traits[c(6:10),43], CWM_traits[c(16:20),43], paired = TRUE, alternative = "two.sided") #t.test moisture jülich
