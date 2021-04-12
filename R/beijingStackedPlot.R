beijingStackedPlot <- function(dfComp, renameCategories, removeComposites, hazeEvents){

  #############################################
  ## Beijing, v0.1.3                         ##
  ## Analysis of Beijing campaign total data ##
  ## Kate Wolfer, Universitaet Basel         ##
  ## April 2021                              ##
  #############################################

  library(ggpubr)

  cat("Is data mass or volume normalised \n")
  normalised <- readline("Enter 'm' for mass (ug/ug) or 'v' for volume (ug/m3): \n")

  ## INPUTS

  ## df: dataframe of collated atmospheric measurement data
  ## with columns as observations and rows as samples

  ## data cleanup to filter categories
  ds <- dfComp[,-removeComposites]
  renameCategories <- renameCategories[-removeComposites]

  ## get list of all the measurement categories
  singleCategories <- unique(renameCategories)
  #singleCategories <- singleCategories[-which(singleCategories == "")]
  #singleCategories <- singleCategories[-which(singleCategories == "AMS factors")]

  ## pull the data into small/large datasets
  largeCategories <- c("total OC", "total EC", "small ions", "metals")

  smallCategories <- c("biomass burning", "PAH", "n-alkane",
                       "cooking markers", "vehicle markers", "SOA" )

  #singleCategories <- c(largeCategories, smallCategories)

  sumCategories <- as.data.frame(matrix(ncol = length(singleCategories)+3,
                                        nrow = nrow(ds), NA))

  colnames(sumCategories) <- c("Date","Season", "PMmass", singleCategories)
  sumCategories$Date <- df$Date
  sumCategories$Season <- df$Season
  sumCategories$'PMmass' <- df$`PM mass`
  #sumCategories$'PMmass' <- as.numeric(paste(sumCategories$'PMmass'))


  for (sc in 1:length(singleCategories)){
    findCols <- which(renameCategories == singleCategories[sc])
    print(singleCategories[sc])
    print(findCols)

    if(length(findCols) == 1){
      sumCategories[,sc+3] <- ds[,findCols]
    } else {
      getCols <- ds[,findCols]
      sumCol <- apply(getCols, 1, sum, na.rm = TRUE)
      sumCategories[,sc+3] <- sumCol
    }
  }

  selectSmall <- sumCategories[,c(1,2,3,which(is.element(colnames(sumCategories),
                                                         smallCategories)))]

  selectLarge <- sumCategories[,c(1,2,3,which(is.element(colnames(sumCategories),
                                                         largeCategories)))]


  ##############
  ## Plotting ##
  ##############

  ## colours and parameters for plots
  small <- c("#4751bd", "#4b80b7", "#36bfc0",
             "#f58945", "#ffca35","#fdf202")
  large <- c("#7f29ce", "#dce853", "#d13d3d", "#31cbdd")
  colList <- c("large", "small")
  baseSize <- 16

  ## produce one plot per season - currently hardcoded
  for(c in 1:length(colList)){

    if(c == 1){
      pullData <- selectSmall
      colPal <- small
      p = list()

    } else {pullData <- selectLarge
    colPal <- large
    }

    ## reshape for long format preferred by ggplot2
    lmMelt1  <- melt(pullData[which(pullData$Season == "summer"),-2],
                     id.vars = c("Date", "PMmass"))

    lmMelt2  <- melt(pullData[which(pullData$Season == "winter"),-2],
                     id.vars = c("Date", "PMmass"))

    maxY <- max(apply(pullData[,c(4:ncol(pullData))],1, sum))
    maxY <- maxY + 0.05*maxY

    ## y-axis labels, for mass or volume
    vol <- bquote("concentration (" *mu~ "g/" ~m^3 ~")")
    mass <- bquote("concentration (" *mu~ "g/" *mu~ "g)")

    ###########
    ## PLOTS ##
    ###########

    ## summer plot
    stak1 <- ggplot(lmMelt1, aes(x = Date, y = value, fill = variable)) + theme_bw(base_size = baseSize)
    stak1 <- stak1 + geom_bar(position="stack", stat="identity")
    stak1 <- stak1 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
    stak1 <- stak1 + coord_cartesian(ylim=c(0,maxY)) + labs(fill='category')
    if (normalised == "m"){
      stak1 <- stak1 + labs(x = "summer (date)", y = mass)
    } else if (normalised == "v"){
      stak1 <- stak1 + labs(x = "summer (date)", y = vol)
    }
    stak1 <- stak1 + scale_y_continuous(expand = c(0,0))
    stak1 <- stak1 + theme(legend.position = "none", legend.title = element_blank())
    stak1 <- stak1 + scale_x_date(expand = c(0,0), date_breaks = "1 day")
    stak1 <- stak1 + scale_fill_manual(values = colPal)

    ## winter plot
    stak2 <- ggplot(lmMelt2, aes(x = Date, y = value, fill = variable)) + theme_bw(base_size = baseSize)
    stak2 <- stak2 + geom_bar(position="stack", stat="identity")
    stak2 <- stak2 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
    stak2 <- stak2 + coord_cartesian(ylim=c(0,maxY))
    if (normalised == "m"){
      stak2 <- stak2 + labs(x = "winter (date)", y = mass)
    } else if (normalised == "v"){
      stak2 <- stak2 + labs(x = "winter (date)", y = vol)
    }
    stak2 <- stak2 + theme(legend.position = "none", legend.title = element_blank())
    stak2 <- stak2 + scale_y_continuous(expand = c(0, 0))
    stak2 <- stak2 + scale_x_date(expand = c(0,0), date_breaks = "1 day")
    stak2 <- stak2 + scale_fill_manual(values = colPal)

    if(exists("hazeEvents")){
      stak1 <- stak1 + theme(axis.text.x = element_text(colour = as.factor(hazeEvents$haze[which(hazeEvents$Season == "summer")])))
      stak2 <- stak2 + theme(axis.text.x = element_text(colour = as.factor(hazeEvents$haze[which(hazeEvents$Season == "winter")])))
    }

    getP <- ggarrange(stak2, stak1, ncol=2, common.legend = TRUE, legend="bottom")

    p[[c]] <- getP + rremove("legend.title")

  }


  return(p)

}
