beijingLRoptimisation <- function(getSeason, selectPredictor, selectSeason,
                           selectedFeatures, refine){

  #############################################
  ## Beijing, v0.1.3                         ##
  ## Analysis of Beijing campaign total data ##
  ## Kate Wolfer, Universitaet Basel         ##
  ## April 2021                              ##
  #############################################

  ###############################
  ## Linear model construction ##
  ###############################

  ## subset the data for the selected prdictor and rename columns
  modelSubset <- data.frame(getSeason[,which(colnames(getSeason) == selectPredictor)],
                            getSeason[,which(is.element(colnames(getSeason),selectedFeatures) == TRUE)])
  colnames(modelSubset)[1] <- selectPredictor
  colnames(modelSubset)[c(2:ncol(modelSubset))] <- colnames(getSeason)[which(is.element(colnames(getSeason),selectedFeatures) == TRUE)]

  ## replace missing data with median
  for(y in 1:ncol(modelSubset)){
    findThem <- is.na(modelSubset[,y])
    subMed <- median(modelSubset[,y], na.rm = TRUE)
    modelSubset[findThem,y] <- subMed
  }

  # remove features which have substantial number which are median (5 here)
  checkMedian <- apply(modelSubset,2 , median)
  collectRemovals <- NULL
  for (g in 1:ncol(modelSubset)){
    isMed <- modelSubset[,g] == checkMedian[g]
    if (sum(is.na(isMed)) != length(isMed)){
      if(sum(isMed) > 5){
        collectRemovals[g] <- TRUE
      } else {collectRemovals[g] <- FALSE}
    } else {collectRemovals[g] <- TRUE}
  }

  if (sum(collectRemovals) > 0){
    if (sum(collectRemovals) > ncol(modelSubset)){
      modelSubset <- modelSubset[,-which(collectRemovals == TRUE)]
    }
  }

  selectModel <- modelSubset
  #cat(selectedFeatures)

  ################################
  ## get optimised linear model ##
  ################################

  selectedFeatures <- colnames(selectModel)
  lmModelSubset <- selectModel[,which(is.element(colnames(selectModel), selectedFeatures) == TRUE)]
  selectedFeatures <- colnames(selectModel)[-1]

  ###############################
  ## Stepwise model refinement ##
  ###############################

  ## variable selection for optimal linear regression model
  predName <- colnames(lmModelSubset)[1]

  if(ncol(lmModelSubset) < 5){
    refine = 0
  }

  if (refine == 1){
    retainPredictor <- lmModelSubset[,1]
    tmp <- regsubsets(lmModelSubset[,1] ~ ., data = lmModelSubset[,-1], nbest=1000, really.big=T)#, intercept=T)
    all.mods <- summary(tmp)[[1]]
    all.mods[,1] <- FALSE
    collectAIC <- NULL
    collectSum <- NULL

    matrixSubset <- as.matrix(lmModelSubset)

    for(t in 1:nrow(all.mods)){
      findCols <- which(all.mods[t,] == TRUE)
      all.lm <- glm(matrixSubset[,1] ~ matrixSubset[,findCols])
      collectAIC[t] <- extractAIC(all.lm)[2]
      collectSum[t] <- sum(all.mods[t,])
    }

    selectModel <- which(collectAIC == min(collectAIC,na.rm = TRUE))
    if (collectSum[selectModel] < 4){
      reduceSet <- which(collectSum >= 4)
      selectModel <- which(collectAIC == min(collectAIC[reduceSet],na.rm = TRUE))
    }

    lmModelSubset <- cbind(lmModelSubset[,1],
                           lmModelSubset[,which(all.mods[selectModel,] == TRUE)])
  }

  colnames(lmModelSubset)[1] <- predName

  # produce LR model
  realModel <- glm(lmModelSubset[,1] ~ ., data = lmModelSubset[,-1],
                   family = gaussian(link="identity"))
  #realModel <- lm(lmModelSubset[,1] ~ ., data = lmModelSubset[,-1])
  getSummary <- summary(realModel)
  getSumm <- summ(realModel,digits = 10)


  #############################
  ## once model is optimised ##
  #############################
  
  ## collect the model information
  collectEstimate <- getSummary$coefficients[,1]
  getResiduals <- resid(realModel)
  getResiduals <- data.frame(getSeason$Date, getResiduals)
  colnames(getResiduals) <- c("id", "mr")

  ## actually calculate the model for visualisation
  linearModel <- lmModelSubset
  linearModel2 <- lmModelSubset
  for (m in 2:ncol(lmModelSubset)){
    linearModel[,m] <- lmModelSubset[,m]*collectEstimate[m]
  }
  linearModel$summed <- apply(linearModel[,c(2:ncol(linearModel))],
                              1, sum) + unlist(collectEstimate[1])
  linearModel$id <- getSeason$Date

  ## plot both together for each sample
  linearModel$medLM <- apply(cbind(linearModel[,1], linearModel$sum),1,median)
  meltLM <- melt(linearModel[,c(1,(ncol(linearModel)-2):ncol(linearModel))],id.vars = "id")

  ## looking at cross-validation to check stability of features in the model
  source("pmRLMcrossValidation.R")
  nIter <- 500
  nFolds <- 5

  getCV <- pmRLMcrossValidation(lmModelSubset,
                                selectPredictor,
                                selectSeason,
                                nFolds,
                                nIter)
  pullPreds <- getCV[[4]]
  getPredSD <- apply(pullPreds,1,sd)
  c <- getCV[[1]]

  ## colour palettes
  cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
  warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
  cols = c(rev(cool), rev(warm))
  getPalette <- colorRampPalette(cols)(nrow(linearModel))

  # predicted vs. actual correlation plot
  q <- ggplot(linearModel, aes(linearModel[,1], summed))
  q <- q + geom_point(size = 3, aes(colour = factor(id)), alpha = 0.7)
  q <- q + ggtitle("Linear regression performance 1, real vs. predicted (PLSR)") + theme_bw()
  q <- q + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
  q <- q + theme(axis.text.x = element_text(hjust=0.95,vjust=0.2))
  q <- q + labs(x = "actual response values",
                y = "predicted response values")
  q <- q + geom_errorbar(aes(ymin=summed-getPredSD, ymax=summed+getPredSD))
  q <- q + geom_smooth(color = "blue", method='lm', se = FALSE)
  q <- q + scale_color_manual(values = getPalette)

  # residuals plot
  res <- ggplot(getResiduals) + geom_point(aes(id, mr, colour = factor(id)),
                                           size = 3, alpha = 0.7)
  res <- res + geom_smooth(aes(id, mr),color = "red", method='lm', se = FALSE)
  res <- res + ggtitle("Linear regression residuals (PLSR)") + theme_bw()
  res <- res + theme(plot.title = element_text(hjust = 0.5)) + labs(colour='Date')
  res <- res + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  res <- res + labs(y = "residual values") + guides(col = guide_legend(ncol = 1))
  res <- res + theme(axis.title.x=element_blank())
  res <- res + scale_x_date(date_breaks = "1 day")
  res <- res + scale_color_manual(values = getPalette)
  res <- res + theme(legend.text=element_text(size=4))

  # residuals density plot
  resDens <- ggplot(getResiduals) + geom_density(aes(mr, size = 1, alpha = 0.7, colour = "red"))
  resDens <- resDens + ggtitle("Linear regression residuals (PLSR), distribution") + theme_bw()
  resDens <- resDens + theme(plot.title = element_text(hjust = 0.5))
  resDens <- resDens + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  resDens <- resDens + labs(x = "residuals") + theme(legend.position="none")

  #fn <- paste0("Density ", fn)
  #ggsave(fn, plot = res, width=30, height=30, units="cm", dpi = 600)

  ## plot time series
  lmModelSubset$Date <- getSeason$Date
  lmMelt  <- melt(lmModelSubset[,c(2:(ncol(lmModelSubset)))],id.vars = "Date")
  stak <- ggplot(lmMelt, aes(x = Date, y = value, fill = variable))
  stak <- stak + geom_area(position="stack", stat="identity")
  stak <- stak + scale_y_continuous(expand = c(0, 0))
  stak <- stak + scale_x_date(date_breaks = "1 day", expand = c(0, 0)) + theme_bw()
  stak <- stak + theme(axis.text.x = element_text(hjust = 0.95,
                                                  vjust = 0.2,
                                                  angle = 90))
  stak <- stak + theme(legend.text = element_text(size = 6))
  #stak <- stak + coord_cartesian(ylim=c(0,10)) + labs(fill='category')
  stak <- stak + ggtitle("Measurement values time series data")
  stak <- stak + theme(plot.title = element_text(hjust = 0.5))
  stak <- stak + theme(axis.title.x=element_blank())

  savePlot <- grid.arrange(q, res, getCV[[1]], stak, resDens, ncol = 2)

  return(list(getSummary, savePlot, getSumm, linearModel, getCV, getResiduals))
}


