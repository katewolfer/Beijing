plotPCA <- function(df, pc1, pc2, logTr, startCol, colorCol, loadingsCol) {

  #############################################
  ## Beijing, v0.0.2                         ##
  ## Analysis of Beijing campaign total data ##
  ## Kate Wolfer, Universitaet Basel         ##
  ## April 2021                              ##
  #############################################

  ## simple PCA, produces scores and loadings plots
  ## R package does not produce Q2 values based on 7-fold model
  ## cross-validation; we used SIMCA SIMCA+ 16.0 (Umetrics, Umea, Sweden)
  ## to produce the actual PCA, then used the ggplot2 code at the bottom
  ## of this function to produce the plots in the paper
  ## we will implement cross-validation at a later date

  ## INPUTS

  ## df: dataframe of collated atmospheric measurement data
  ## with columns as observations and rows as samples

  ## pc1, pc2: select which two principal components are plotted, this may need
  ## to be changed if PCs 1 and 2 do not show all of the interesting trends

  ## logTr: log10 transform data - mostly only suitable for LC-MS data

  ## startCol: integer of beginning of numeric observation data
  ## which needs to be normalised to ensure all values are actual numerical
  ## format, have standardised units no major outliers

  ## pick up only the numerical data required to produce the model
  ds <- df[,c(startCol:ncol(df))]

  ## select column data used for coloring scores
  colorData <- df[,colorCol]

  ## need to add here some automated way to eliminate disproportionately
  ## influential features on models
  #ds <- ds[,-27]

  ## remove columns which are more than 50% missing values
  pcmv <- NULL
  for (cl in 1:ncol(ds)){
    pcmv[cl] <- length(which(is.na(ds[,cl])))/nrow(df)
  }
  forRemoval <- which(pcmv > 0.55)
  ds <- ds[,-forRemoval]

  ## unit variance scaling
  getSD <- apply(ds, 2, sd, na.rm = TRUE)
  getMeans <- apply(ds, 1, median, na.rm = TRUE)
  for (cl in 1:ncol(ds)){
    ds[,cl] <- ds[,cl]/getSD[cl]
    ds[,cl] <- ds[,cl]-getMeans[cl]
  }

  ## PCA needs zero NA
  ds[is.na(ds)] <- 1e-19

  ## log10 transform data if required, produce PCA model
  set.seed(20)
  if (logTr == 1) {
    pcaSamples <- prcomp(log10(ds+10))
    cat("data log10 transformed \n")
  } else if (logTr == 0) {
    pcaSamples <- prcomp(ds)
    cat("data not transformed \n")
  } else (error("please specify if transformation to be applied: 0 for none, 1 for log(10)"))

  ## get PCA scores
  pcaDF <- as.data.frame(pcaSamples$x)
  pcaDF$group <- colorData

  ## get PCA loadings
  pcaLoadings <- as.data.frame(pcaSamples$rotation)
  loadingsCol <- loadingsCol[c(startCol:ncol(df))]
  loadingsCol <- loadingsCol[-forRemoval]
  pcaLoadings$category <- loadingsCol
  pcaLoadings$category <- as.factor(pcaLoadings$category)

  ## R2X value
  expl.var <- round(pcaSamples$sdev^2/sum(pcaSamples$sdev^2)*100,2)

  ## get Hotellings T2 value for plot
  getHT2 <- as.data.frame(pcaMethods:::simpleEllipse(pcaDF[,"PC1"],
                                                     pcaDF[,"PC2"],
                                                     alfa = 0.95,
                                                     len = 500))

  ## set up scores plot colours
  nb.cols <- length(unique(colorData))
  mycolors <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols))
  names(mycolors) <- levels(df$type)


  getBreaks = round(seq(floor(min(colorData, na.rm = TRUE)),
                        ceiling(max(colorData, na.rm = TRUE)),
                        ceiling(max(colorData, na.rm = TRUE))/15))

  ## obtain plot
  scoresPlot <- ggplot(pcaDF, aes(x = pcaDF[,pc1], y = pcaDF[,pc2], color = group))
  scoresPlot <- scoresPlot + geom_point(size = 5)
  scoresPlot <- scoresPlot + ggtitle(paste0("PCA scores plot, points coloured by ",
                                            colnames(df)[colorCol]))
  scoresPlot <- scoresPlot + scale_colour_gradientn(colors = mycolors,
                                                    breaks = getBreaks)
  scoresPlot <- scoresPlot + geom_point(data = getHT2, aes(x = V1, y = V2),
                                        colour = '#888888', size = 0.2)
  scoresPlot <- scoresPlot + geom_hline(yintercept = 0)
  scoresPlot <- scoresPlot + geom_vline(xintercept = 0) + theme_bw()
  scoresPlot <- scoresPlot + theme(text = element_text(size = 16))
  #plotlabel = bquote('AA (counts '*mu~g^-1*')')
  #scoresPlot <- scoresPlot + labs(colour=plotlabel)
  scoresPlot <- scoresPlot + xlab(paste0("PC", pc1, "\n R2X ", expl.var[pc1], "%"))
  scoresPlot <- scoresPlot + ylab(paste0("PC", pc2, "\n R2X ", expl.var[pc2], "%"))
  scoresPlot <- scoresPlot + theme(legend.position = "bottom",
                                   legend.key.height = unit(0.3, "cm"),
                                   legend.key.width = unit(4, "cm"),
                                   legend.title = element_text(size = 16),
                                   legend.text=element_text(size=14))
  scoresPlot <- scoresPlot + theme(plot.title = element_text(hjust = 0.5),
                                   legend.title=element_blank())

  ## set up loadings plot colours
  ## needs replacing with better discrete color scale
  # mycolors <- c("#ffa500", # AMS
  #               "#b1f2ff", # biomass
  #               "#b11226", # cooking
  #               "#C0C0C0", # gas radical
  #               "#ffbeb1", # gases
  #               "#ffef00", # metal
  #               "#0dd9f4", # meteo
  #               "#6bafc3", # n-alkane
  #               "#ff721a", # PAH
  #               "#000000", # photolysis
  #               "#6091e3", # small ions
  #               "#008141", # SOA
  #               "#a2df35", # total EC
  #               "#ccb2e5", # total OC
  #               "#7f20b4", # vehicle
  #               "#f66f6f") # VOC
  #
  # names(mycolors) <- levels(as.factor(pcaLoadings$category))

  ## obtain loadings plot
  loadingsPlot <- ggplot(pcaLoadings, aes(x = pcaLoadings[,pc1],
                                          y = pcaLoadings[,pc2],
                                          color = category))
  loadingsPlot <- loadingsPlot + geom_point(size = 5.5)
  loadingsPlot <- loadingsPlot + ggtitle(paste0("PCA loadings plot"))
  loadingsPlot <- loadingsPlot + geom_text_repel(aes(label=colnames(ds)),
                                                 vjust=-2, color = "#888888",
                                                 size = 3.5)

  loadingsPlot <- loadingsPlot + geom_hline(yintercept = 0)
  loadingsPlot <- loadingsPlot + geom_vline(xintercept = 0) + theme_bw()
  loadingsPlot <- loadingsPlot + xlab(paste0("PC", pc1, "\n R2X ",
                                             expl.var[pc1], "%"))
  loadingsPlot <- loadingsPlot + ylab(paste0("PC", pc2, "\n R2X ",
                                             expl.var[pc2], "%"))
  loadingsPlot <- loadingsPlot + theme(text = element_text(size = 16))
  loadingsPlot <- loadingsPlot + theme(legend.title=element_blank())
  loadingsPlot <- loadingsPlot + theme(legend.position="bottom")
  loadingsPlot <- loadingsPlot + theme(plot.title = element_text(hjust = 0.5))
  loadingsPlot

  return(list(scoresPlot, loadingsPlot))

}
