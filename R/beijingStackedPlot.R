beijingStackedPlot <- function(df, categories, removeComposites, hazeEvents){

  #############################################
  ## Analysis of Beijing campaign total data ##
  ## Kate Wolfer, Universitaet Basel         ##
  ## March 2021                              ##
  #############################################

  ## INPUTS

  ## df: dataframe of collated atmospheric measurement data
  ## with columns as observations and rows as samples

  ## data cleanup to filter categories
  ds <- df[,-removeComposites]
  renameCategories <- categories[-removeComposites]

  ## get list of all the measurement categories
  singleCategories <- unique(renameCategories)

  ## pull the data into small/large datasets
  largeCategories <- c("total OC", "total EC", "small ions", "metals")

  smallCategories <- c("biomass burning", "AMS factors", "PAH", "n-alkane",
                       "cooking markers", "vehicle markers", "SOA" )

  sumCategories <- as.data.frame(matrix(ncol = length(singleCategories)+3,
                                        nrow = nrow(df), NA))

  colnames(sumCategories) <- c("Date","Season", "PMmass", singleCategories)
  sumCategories$Date <- df$Date
  sumCategories$Season <- df$Season
  sumCategories$'PMmass' <- PMmass
  sumCategories$'PMmass' <- as.numeric(paste(sumCategories$'PMmass'))


  for (i in 1:length(singleCategories)){
    if(length(which(renameCategories == singleCategories[i])) == 1){
      sumCategories[,i+3] <- dfComp[,which(renameCategories == singleCategories[i])]
    } else {
      sumCol <- apply(dfComp[,which(renameCategories == singleCategories[i])], 1,
                      sum, na.rm = TRUE)
      sumCategories[,i+3] <- sumCol
    }
  }


  ##############
  ## Plotting ##
  ##############

  ## colours for stacks
  small <- c("#4751bd", "#4b80b7", "#36bfc0", "#f58945", "#ffca35","#fdf202")
  large <- c("#7f29ce", "#dce853", "#d13d3d", "#31cbdd")

  colList <- c("large", "small")

  colPal <- small

  ## base text size
  baseSize <- 16


  lmMelt1  <- melt(sumCategories[which(sumCategories$Season == "summer"),
                                 c(1, 3:ncol(sumCategories))],id.vars = c("Date", "PMmass"))
  colnames(lmMelt1)[2] <- "PMmass"
  lmMelt1$'PMmass' <- as.numeric(paste(lmMelt1$'PMmass'))

  lmMelt2  <- melt(sumCategories[which(sumCategories$Season == "winter"),
                                 c(1, 3:ncol(sumCategories))],id.vars = c("Date", "PMmass"))

  colnames(lmMelt2)[2] <- "PMmass"
  lmMelt2$'PMmass' <- as.numeric(paste(lmMelt2$'PMmass'))

  ## construct plots
  stak1 <- ggplot(lmMelt1, aes(x = Date, y = value, fill = variable)) + theme_bw(base_size = baseSize)
  stak1 <- stak1 + geom_bar(position="stack", stat="identity")
  stak1 <- stak1 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  stak1 <- stak1 + coord_cartesian(ylim=c(0,3)) + labs(fill='category')
  #stak1 <- stak1 + labs(x = "summer (date)", y = bquote("concentration (" *mu~ "g/" ~m^3 ~")"))
  stak1 <- stak1 + labs(x = "summer (date)", y = bquote("concentration (ug/" ~m^3 ~")"))
  stak1 <- stak1 + scale_y_continuous(expand = c(0,0))
  stak1 <- stak1 + theme(legend.position = "none", legend.title = element_blank())
  stak1 <- stak1 + scale_x_date(expand = c(0,0), date_breaks = "1 day")
  stak1 <- stak1 + scale_fill_manual(values = colPal)
  stak1 <- stak1 + theme(axis.text.x = element_text(colour = as.factor(hazeEvents$haze[which(hazeEvents$Season == "summer")])))

  stak2 <- ggplot(lmMelt2, aes(x = Date, y = value, fill = variable)) + theme_bw(base_size = baseSize)
  stak2 <- stak2 + geom_bar(position="stack", stat="identity")
  stak2 <- stak2 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  stak2 <- stak2 + coord_cartesian(ylim=c(0,3))
  #stak2 <- stak2 + labs(x = "winter (date)", y = bquote("concentration (" *mu~ "g/" ~m^3 ~")"))
  stak2 <- stak2 + labs(x = "winter (date)", y = bquote("concentration (ug/" ~m^3 ~")"))
  stak2 <- stak2 + theme(legend.position = "none", legend.title = element_blank())
  stak2 <- stak2 + scale_y_continuous(expand = c(0, 0))
  stak2 <- stak2 + scale_x_date(expand = c(0,0), date_breaks = "1 day")
  stak2 <- stak2 + scale_fill_manual(values = colPal)
  stak2 <- stak2 + theme(axis.text.x = element_text(colour = as.factor(hazeEvents$haze[which(hazeEvents$Season == "winter")])))


  # stak3 <- ggplot(lmMelt1, aes(x = Date, y = value, fill = variable))
  # stak3 <- stak3 + geom_bar(position="stack", stat="identity") + theme_bw(base_size = baseSize)
  # stak3 <- stak3 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  # stak3 <- stak3 + coord_cartesian(ylim=c(0,0.075)) + labs(fill='category')
  # stak3 <- stak3 + labs(x = "summer (date)", y = "concentration (ug/ug)")
  # stak3 <- stak3 + scale_y_continuous(expand = c(0, 0))
  # stak3 <- stak3 + scale_x_date(expand = c(0,0), date_breaks = "2 day")
  #
  # stak4 <- ggplot(lmMelt2, aes(x = Date, y = value, fill = variable))
  # stak4 <- stak4 + geom_bar(position="stack", stat="identity") + theme_bw(base_size = baseSize)
  # stak4 <- stak4 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  # stak4 <- stak4 + coord_cartesian(ylim=c(0,0.075))
  # stak4 <- stak4 + labs(x = "winter (date)", y = "concentration (ug/ug)")
  # stak4 <- stak4 + theme(legend.position = "none")
  # stak4 <- stak4 + scale_y_continuous(expand = c(0, 0))
  # stak4 <- stak4 + scale_x_date(expand = c(0,0), date_breaks = "2 day")

  # p1 <- grid.arrange(stak2, stak4, ncol = 1)
  # p2 <- grid.arrange(stak1, stak3, ncol = 1)
  #
  # ggsave("stacked plot 1 29 July 2020.png", plot = p1, width=18, height=24, units="cm", dpi = 500)
  # ggsave("stacked plot 2 29 July 2020.png", plot = p2, width=24, height=24, units="cm", dpi = 500)
  #
  # ggsave("stacked plot 3 29 July 2020.png", plot = p1, width=30, height=40, units="cm", dpi = 500)
  # ggsave("stacked plot 4 29 July 2020.png", plot = p2, width=40, height=40, units="cm", dpi = 500)

  library(ggpubr)
  #p3 <- grid.arrange(stak2, stak1, ncol = 2)
  p3 <- ggarrange(stak2, stak1, ncol=2, common.legend = TRUE, legend="bottom")
  p3 <- p3 + rremove("legend.title")

  ggsave("redone stacked plot small VOL 06 Aug 2020.png", plot = p3, width=35, height=18, units="cm", dpi = 500)
  ggsave("stak2.png", plot = stak2, width=17.5, height=36, units="cm", dpi = 500)



  ## stacked plot with PM mass line/100

  # stak1 <- ggplot(lmMelt1, aes(x = Date, y = value, fill = variable)) + theme_bw(base_size = baseSize)
  # stak1 <- stak1 + geom_bar(position="stack", stat="identity")
  # stak1 <- stak1 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  # stak1 <- stak1 + coord_cartesian(ylim=c(0,1)) + labs(fill='category')
  # stak1 <- stak1 + labs(x = "summer (date)", y = "concentration (ug/ug)")
  # stak1 <- stak1 + scale_y_continuous(expand = c(0, 0))
  # stak1 <- stak1 + scale_x_date(expand = c(0,0), date_breaks = "3 day")
  # stak1 <- stak1 + geom_line(lmMelt1, mapping = aes(x = Date,
  #                                                   y = (lmMelt1$'PM mass'/100)),
  #                            size = 1.25, color = "blue")
  # stak1 <- stak1 +  scale_y_continuous(~./100, sec.axis = sec_axis(name = "PM mass"), labels = "PM mass (ug)")
  # stak1

  ## facet plots: stacked with trends

  sumCategories$EPR <- df$EPR
  sumCategories$AA <- df$AA
  sumCategories$DTT <- df$DTTm
  sumCategories$DCFH <- df$DCFH

  extraMelt1  <- melt(sumCategories[which(sumCategories$Season == "summer"),
                                    c(1,3,12:14)],id.vars = "Date")

  corrAssay <- corr.test(sumCategories$PMmass[which(sumCategories$Season == "summer")],
                         sumCategories[which(sumCategories$Season == "summer"),
                                       c(3,12:15)])

  plot(sumCategories[which(sumCategories$Season == "summer"),c(12:15)],
       sumCategories$PMmass[which(sumCategories$Season == "summer")],
  )


  line1 <- ggplot(extraMelt1, aes(x = Date, y = value, colour = variable))
  line1 <- line1 + geom_line(position="stack", stat="identity") + theme_bw(base_size = baseSize)
  line1 <- line1 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  #line1 <- line1 + labs(x = "summer (date)", y = "concentration (ug/ug)")
  #line1 <- line1 + scale_y_continuous(expand = c(0, 0))
  line1 <- line1 + scale_x_date(expand = c(0,0), date_breaks = "3 day")
  line1

  extraMelt2  <- melt(sumCategories[which(sumCategories$Season == "winter"),
                                    c(1,3,12:15)],id.vars = "Date")

  line2 <- ggplot(extraMelt2, aes(x = Date, y = value, colour = variable))
  line2 <- line2 + geom_line(position="stack", stat="identity") + theme_bw(base_size = baseSize)
  line2 <- line2 + theme(axis.text.x=element_text(hjust=0.95,vjust=0.2, angle = 90))
  #line2 <- line2 + labs(x = "summer (date)", y = "concentration (ug/ug)")
  #line2 <- line2 + scale_y_continuous(expand = c(0, 0))
  line2 <- line2 + scale_x_date(expand = c(0,0), date_breaks = "3 day")
  line2

  ## correlation plots

  d  <- sumCategories[which(sumCategories$Season == "summer"), c(1,3,12:14)]
  ggpairs(d)



}
