beijingMLRmodelling <- function(sourceModels, selectSource, predictors, seasons){

  #############################################
  ## Beijing, v0.1.3                         ##
  ## Analysis of Beijing campaign total data ##
  ## Kate Wolfer, Universitaet Basel         ##
  ## April 2021                              ##
  #############################################

  source("beijingLRoptimisation.R")

  for (x in 1:length(predictors)){
    selectPredictor <- predictors[x]

    for (z in 1:length(seasons)){

      # pull season data
      selectSeason <- seasons[z]
      getSeason <-  df[which(df$Season == selectSeason),]

      # for collecting model data
      list1 = list()
      list2 = list()
      collectVariability <- data.frame(matrix(ncol = 6, nrow = 11, "NA"))

      # run automated model selection
      for(n in 1:length(selectSource)){
        i <- selectSource[n]

        selectedFeatures <- colnames(df)[is.element(colnames(df),
                                                    sourceModels$measurement[which(sourceModels[,i] == 1)])]

        fn <- paste0(colnames(sourceModels)[i]," ", selectPredictor, " ", selectSeason,
                     " linear regression MASS, 28 Sept 2020.png")

        refine <- 1
        if (length(selectedFeatures) < 5){
          refine = 0
        }
        p <- pmRLRselected4(getSeason, selectPredictor, selectSeason,
                            selectedFeatures, refine)
        ggsave(fn, plot = p[[2]], width=40, height=51, units="cm", dpi = 600)

        dev.off()
        list1[[n]] <- p[[1]]
        list2[[n]] <- p[[3]]
        # variability in coefficient predictions
        # pullCoeff <- p[[5]][[3]][,c(1:500)]
        # getMinCoeff <- apply(pullCoeff, 1, min)
        # getMaxCoeff <- apply(pullCoeff, 1, max)
        # coeffVar <- cbind(getMinCoeff, getMaxCoeff)
        getVars <- as.data.frame(t(rbind(p[[5]][[2]][8,],
                                         p[[5]][[2]][1,],
                                         p[[5]][[2]][5,])))
        getVars[,1] <- formatC(getVars[,1], format = 'e', digits = 2)
        getVars[,2] <- formatC(getVars[,2], format = 'e', digits = 2)
        getVars[,3] <- formatC(getVars[,3], format = 'e', digits = 2)

        cvVars <- paste(getVars[,1], " (", getVars[,2],",", getVars[,3],")", sep="")
        collectVariability[c(1:length(cvVars)),n] <- cvVars

        # # variability in day predictions
        # getMinPredict <- apply(p[[5]][[4]],1, min)
        # getMaxPredict <- apply(p[[5]][[4]],1, max)

      }

      collateModels <- as.data.frame(matrix('', ncol = length(selectSource), nrow = 59))
      getValsLength <- NULL

      for (s in 1:ncol(collateModels)){
        selectRow <- s
        collectCoef <- as.data.frame(coef(list1[[selectRow]]))
        getValsLength[s] <- nrow(collectCoef)

        collateModels[1,s] <- paste0(list1[[selectRow ]]$terms)[3] # model terms
        collateModels[c(2:7),s] <- summary(list1[[selectRow ]]$deviance.resid) # deviance residuals
        collateModels[8,s] <- list1[[selectRow]]$null.deviance # null deviance
        collateModels[9,s] <- list1[[selectRow]]$df.null
        collateModels[10,s] <- list1[[selectRow]]$deviance
        collateModels[11,s] <- list1[[selectRow]]$df.residual
        collateModels[12,s] <- list1[[selectRow]]$dispersion # Gaussian dispersion parameter
        collateModels[13,s] <- round(list1[[selectRow]]$aic,4) # AIC value
        collateModels[14,s] <- list1[[selectRow]]$iter # fisher scoring iterations
        collateModels[15,s] <- (1 - list1[[selectRow]]$deviance/list1[[selectRow]]$null.deviance)

        collateModels[c(16:(15+getValsLength[s])),s]  <- collectCoef[,1]
        collateModels[c(27:(26+getValsLength[s])),s]  <- collectCoef[,2]
        collateModels[c(38:(37+getValsLength[s])),s]  <- collectCoef[,3]
        collateModels[c(49:(48+getValsLength[s])),s]  <- collectCoef[,4]
      }
      colnames(collectVariability) <- colnames(collateModels)
      collateModels <- rbind(collateModels, collectVariability[c(1:10),])
      summariseModels <- colnames(sourceModels)[selectSource]
      colnames(collateModels) <- paste(selectSeason, selectPredictor, summariseModels)

      rownames(collateModels) <- c("model terms", # 1
                                   "deviance residuals min", # 2
                                   "deviance residuals 1Q", # 3
                                   "deviance residuals median", # 4
                                   "deviance residuals mean", # 5
                                   "deviance residuals 3Q", # 6
                                   "deviance residuals max", # 7
                                   "null deviance", # 8
                                   "deg freedom null dev", # 9
                                   "residual deviance", # 10
                                   "deg freedom resid dev", # 11
                                   "Gaussian dispersion parameter", # 12
                                   "AIC value", # 13
                                   "Fisher scoring iterations", # 14
                                   "R2", # 15
                                   "Intercept coefficient",
                                   paste("Coefficient", c(1:10)),
                                   "Intercept std error",
                                   paste("std error", c(1:10)),
                                   "Intercept t-value",
                                   paste("t-value", c(1:10)),
                                   "Intercept p-value",
                                   paste("p-value", c(1:10)),
                                   "Intercept coeff var",
                                   paste("coeff var", c(1:9)))

      fn <- paste0(selectPredictor, " ", selectSeason,
                   " Beijing source apportionment models.csv")


      write.csv(collateModels, fn)
    }
  }

return(collateModels)

}
