# PCA
pca_trainset = m
pca = prcomp( pca_trainset, scale = T )
# variance
pr_var = ( pca$sdev )^2 
# % of variance
prop_varex = pr_var / sum( pr_var )
# Plot
plot( prop_varex, xlab = "Principal Component", 
      ylab = "Proportion of Variance Explained", type = "b" )
# Scree Plot
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" )

