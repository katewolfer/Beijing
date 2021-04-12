[![DOI](https://zenodo.org/badge/353720036.svg)](https://zenodo.org/badge/latestdoi/353720036)
# Beijing dataset analysis and visualisations
This repository holds the functions and data visualisations developed for our ACP paper, implemented in R.  

The functions cover a range of standard univariate and multivariate data analysis, with the data plotting as used for all paper figures.

Most of the functions available here can be used for similar datasets; some hard coding is present, and we will make the package fully generalisable as soon as we can.

Package dependencies can be found [here](https://github.com/katewolfer/Beijing/blob/main/R/beijingPackages.R), our credit and thanks to the authors of these!

Some of the functions include:  
- univariate statistics: Spearman correlations and p-values, in a handy text output  
  
- multivariate statistics: principal components analysis (currently without cross-validation), plus scores and loadings plots. These plots can also be used for PLSR models, which can be constructed using the [pls](https://cran.r-project.org/web/packages/pls/index.html) package (also in R).  
  
![scores example](https://github.com/katewolfer/Beijing/blob/main/examples/PCA%20scores.png)  
![loadings example](https://github.com/katewolfer/Beijing/blob/main/examples/PCA%20loadings.png) 
  
- correlation heatmaps per assay and per season  
  
![heatmap example](https://github.com/katewolfer/Beijing/blob/main/examples/seasonal%20heatmap.png)  

- and the MLR optimisation and model visualisations

![MLR example](https://github.com/katewolfer/Beijing/blob/main/examples/vehicle%20AA%20winter%20linear%20regression%20MASS%2C%2028%20Sept%202020.png)
  
If you find any of the code useful or have suggestions for improvement, please let us know!  


# Paper reference
Atmospheric conditions and composition that influence PM2.5 oxidative potential in Beijing, China  
Campbell, Wolfer et al.  
Atmos. Chem. Phys., 21, 5549–5573, 2021  
https://doi.org/10.5194/acp-21-5549-2021  
https://acp.copernicus.org/articles/21/5549/2021/acp-21-5549-2021.html  
