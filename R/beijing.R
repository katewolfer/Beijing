#############################################
## Analysis of Beijing campaign total data ##
## Kate Wolfer, Universitaet Basel         ##
## March 2021                              ##
#############################################

#######################
## Required packages ##
#######################

require(ggplot2)
require(ggsignif)
require(reshape2)
require(psych)
require(leaps)
require(gridExtra)
require(RColorBrewer)
require(pls)
require(psych)
require(ggplot2)
require(Hotelling)
require(ggrepel)
library(grid)
library(GGally)

source("beijingCleanup.R")
source("beijingUnivariateList.R")
source("plotPCA.R")
source("beijingHeatmaps.R")

##################
## Data cleanup ##
##################

df <- read.csv("Offline_ROS_updated_May2020_7.csv",
               check.names = FALSE,
               stringsAsFactors=FALSE)

# df <- read.csv("Offline_ROS_VOLUME_3.csv",
#                check.names = FALSE,
#                stringsAsFactors=FALSE)

## import data and do initial cleanup
PMmass <- df[,4]
PMmass <- PMmass[-c(1:2)]
saveUnits <- paste(df[1,])
categories <- as.matrix(df[2,])
categories <- as.character(categories[1,])
df <- df[-c(1,2),]
rownames(df) <- NULL

## tidy up date and season columns
df$Season <- factor(df$Season)
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")

## data cleanup - standardise units, remove outliers
source("beijingCleanup.R")
startCol <- 4
df <- beijingCleanup(df, startCol)


###############################################
## Univariate statistics                     ##
## Pairwise correlations of all observations ##
## Performed by season                       ##
###############################################

source("beijingUnivariateList.R")
univariateStats <- beijingUnivariateList(df, startCol)
write.csv(univariateStats,"univariate_stats.csv")

# source("statsSummary.R")
# fullList <- statsSummary(df)
#
# source("runBeijingUnivariate.R")
# #df[is.na(df)] <- 0
# runBeijingUnivariate(df, 5, categories)


#############################
## Multivariate statistics ##
#############################

source("plotPCA.R")
getPCA <- plotPCA(df,
                  pc1 = 1,
                  pc2 = 2,
                  logTr = 0,
                  startCol = 9,
                  colorCol = 6,
                  loadingsCol = categories)

## make plots
plot(getPCA[[1]])
plot(getPCA[[2]])
#plotPCA <- grid.arrange(getPCA[[1]],getPCA[[2]],ncol = 2)

ggsave("PCA loadings.eps",
       plot = getPCA[[1]],
       device = "eps",
       width = 25,
       height = 19,
       units = "cm",
       dpi = 350)

ggsave("PCA scores.eps",
       plot = getPCA[[2]],
       device = "eps",
       width = 25,
       height = 25,
       units = "cm",
       dpi = 350)


#############
## Heatmap ##
#############

source("beijingHeatmaps.R")
heatMap <- beijingHeatmaps(df = df,
                           startCol = 9,
                           assayCols = c(5:8),
                           group = categories)
#plot(heatMap)

ggsave("seasonal heatmap.eps",
       plot = heatMap,
       device = "eps",
       width = 40,
       height = 30,
       units = "cm",
       dpi = 500)

###################
## Stacked plots ##
###################

## required data tidy
df$`palmitic acid`[47] <- median(df$`palmitic acid`[which(df$Season == "winter")],na.rm = TRUE)
findConcs <- grep("/ug",  saveUnits)
dfComp <- df[,findConcs]
renameCategories <- categories[findConcs]
removeComposites <- c(which(renameCategories == "AMS factors"),69,70)
hazeEvents <- read.csv("Beijing haze events.csv")

## run function
stackedPlot <- beijingStackedPlot(dfComp,
                                  categories,
                                  removeComposites,
                                  hazeEvents)

## MLR code

summer <- df[which(df$Season == "summer"),]
winter <- df[which(df$Season == "winter"),]
summer[is.na(summer)] <- 0
winter[is.na(winter)] <- 0

