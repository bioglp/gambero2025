example(PCA_RDA_graphics)


## Example of GABB package pipeline with the base data.set "mtcars" 
my.data <- mtcars

## Data preparation for RDA and PCA : tranformation and scaling of numeric/quantitative variables

prep_data(data = my.data, quantitative_columns = c(1:4), 
          transform_data_method = "log", 
          scale_data = T)

## Create PCA
library(FactoMineR)
my.pca <- FactoMineR::PCA(X = data_quant) 


## Create, display and save graphic output of individual and variable PCA

#Basic output with minimum required parameters
PCA_RDA_graphics(complete.data.set = initial_data_with_quant_transformed, 
                 PCA.object = my.pca, 
                 factor.names = c("vs", "am", "gear", "carb"))

#Advanced outputs (image below)
PCA_RDA_graphics(complete.data.set = initial_data_with_quant_transformed, 
                 PCA.object = my.pca, 
                 factor.names = c("vs", "am", "gear", "carb"), 
                 Biplot.PCA = TRUE,col.arrow.var.PCA = "grey",
                 Barycenter = TRUE, Segments = TRUE, 
                 Barycenter.Ellipse.Fac1 = "vs", Barycenter.Ellipse.Fac2 = "am",
                 factor.colors = "vs", factor.shapes = "am",
                 Barycenter.factor.col = "vs", Barycenter.factor.shape = "am")
