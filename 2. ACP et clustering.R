#' ---
#' title: "Formation R - ACP & clustering"
#' author: "Frédéric Baudron"
#' date: "19-20 Juin 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# INSTALLING NECESSARY PACKAGES-------------------------------------------------

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('ade4')) install.packages("ade4")
if (!require('factoextra')) install.packages("factoextra")
if (!require('NbClust')) install.packages("NbClust")
if (!require('dendextend')) install.packages("dendextend")
if (!require('statip')) install.packages("statip")


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(ade4)
library(factoextra)
library(NbClust)
library(dendextend)
library(statip)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

# Set your directory (where your input files are located, and where your output
# files will be save)

setwd("D:\\Mes Donnees\\1. Cirad\\Cameroun\\Formation\\")

# Load input files
data = read.xlsx("Soybean agronomiques et nodulation.xlsx", sheet = 1)

# dataframe of averages per variety
data_mean = aggregate(cbind(DFF, PV, `Yd.(t/ha)`, DM, S_W, SMV, BB, CS) ~ Var, FUN = mean, na.rm = TRUE, data = data)


# PCA---------------------------------------------------------------------------

# scale variables
data_mean[, 2:9] = scale(data_mean[, 2:9])

# pca
delta.pca = dudi.pca(data_mean[, -c(1)])

delta.pca = dudi.pca(df = data_mean[, -c(1)], scannf = FALSE, nf = 3)

# eigen values of the different PCs
delta.pca$eig

# cumulated percentage of variability explained by the different PCs
cumsum(delta.pca$eig) / sum(delta.pca$eig)

summary(delta.pca)

# correlation coefficients between the PCs and the variables
delta.pca$co

# biplots
fviz_pca_var(delta.pca, col.var = "contrib") + 
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 12) +
  theme_minimal()

fviz_pca_ind(delta.pca) +
  theme_minimal()

fviz_pca_biplot(delta.pca, label = "all", col.ind = "purple", pointsize = 3, pointshape = 16,
                col.var = "black", ) +
  theme_minimal()


# HIERCHICAL CLUSTERING ANALYSIS------------------------------------------------

# applying the Hierarchical Clustering (HC) on the PCA results
delta.cah = hclust(dist(delta.pca$li), method = "ward.D")

hcd = as.dendrogram(delta.cah)

plot(hcd, type = "rectangle", ylab = "Height", leaflab = "none")

# getting clusters
nc = NbClust(delta.pca$li, diss = dist(delta.pca$li), distance = NULL, method = "ward.D")

nc$Best.nc[1, ]
mfv(nc$Best.nc[1, ])

delta.type = cutree(delta.cah, k = 2)

par(mfrow = c(1, 1))
hcd = color_branches(hcd, k = 2)
hcd = color_labels(hcd, k = 2)
plot(hcd, type = "rectangle", ylab = "Height", leaflab = "none")

# Visualising and interpreting the clusters in the PCi-PCj
s.class(delta.pca$li, fac = as.factor(delta.type), col = c("royalblue", "orangered"))

data_mean$cluster = delta.type
data_mean$cluster = as.factor(data_mean$cluster)

fviz_pca_biplot(delta.pca, label = "all", habillage = data_mean$cluster,
                addEllipses = TRUE, ellipse.level = 0.95,
                col.var = "black") +
  theme_minimal()


