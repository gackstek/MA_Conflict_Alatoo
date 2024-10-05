library(corrplot)

setwd("~/Desktop/MA/code/adjust grid to new study area/adjusted grids/")
Kristina_Sitecov<-read.csv("GridCovariates_clean.csv")[,6:12]

# We first estimate a correlation matrix from the covariates 
# We use Spearman rank correlation coefficient, as we do not know 
# whether all variables are normally distributed.
colnames(Kristina_Sitecov) <- c('Distance to water', 'Distance to road','Distance to PA', 'NDVI', 'Slope','Elevation','TRI')

cor_mat <- cor(Kristina_Sitecov, method='spearman')

# we plot the correlation coefficients as percentages.
corrplot.mixed(cor_mat, tl.pos='lt', tl.cex=1.25, number.cex=1.25,cl.cex = 1.25, addCoefasPercent=T)
