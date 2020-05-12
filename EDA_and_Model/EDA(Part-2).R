library(dplyr)
library(ggplot2)
library(pander)
library(ggcorrplot)
library(factoextra)
library("FactoMineR")

rownames(Test_year) <- Test_year$Year
Test_year$Year <- NULL

Test_year.active <- Test_year[1:10, 1:21]
head(Test_year.active[, 1:10])


res.pca <- PCA(Test_year.active, graph = FALSE)
eig.val <- get_eigenvalue(res.pca)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


ind.sup <- Test_year[9:10, 1:21]
ind.sup[, 1:21]

ind.sup.coord <- predict(res.pca, newdata = ind.sup)

# Plot of active individuals
p <- fviz_pca_ind(res.pca, repel = TRUE)
# Add supplementary individuals
fviz_add(p, ind.sup.coord, color ="blue")
# Centering and scaling the supplementary individuals
ind.scaled <- scale(ind.sup, 
                    center = res.pca$center,
                    scale = res.pca$scale)
# Coordinates of the individividuals
coord_func <- function(ind, loadings){
  r <- loadings*ind
  apply(r, 2, sum)
}
pca.loadings <- res.pca$rotation
ind.sup.coord <- t(apply(ind.scaled, 1, coord_func, pca.loadings ))

# Wages of Employees

Wages.active <- Wages[1:9, 2:3]
head(Test_year.active[, 1:10])

res1.pca <- PCA(Wages.active, graph = FALSE)
eig.val1 <- get_eigenvalue(res1.pca)

fviz_eig(res1.pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_ind(res1.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res1.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res1.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)




# Eigenvalues
eig.val1 <- get_eigenvalue(res1.pca)
eig.val1

# Results for Variables
res.var1 <- get_pca_var(res1.pca)
res.var1$coord          # Coordinates
res.var1$contrib        # Contributions to the PCs
res.var1$cos2           # Quality of representation 
# Results for individuals
res.ind1 <- get_pca_ind(res1.pca)
res.ind1$coord          # Coordinates
res.ind1$contrib        # Contributions to the PCs
res.ind1$cos2           # Quality of representation 


ind.sup1 <- Wages[9:10, 2:4]
ind.sup1[, 1:2]

ind.sup.coord1 <- predict(res1.pca, newdata = ind.sup1)


