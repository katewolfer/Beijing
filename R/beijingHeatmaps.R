beijingHeatmaps <- function(df,startCol, assayCols, group){

  #############################################
  ## Beijing, v0.0.2                         ##
  ## Analysis of Beijing campaign total data ##
  ## Kate Wolfer, Universitaet Basel         ##
  ## April 2021                              ##
  #############################################

  getSeasons <- levels(df$Season)
  corr <- NULL

  for(s in 1:length(getSeasons)){

    ds <- df[which(df$Season == getSeasons[s]),]
    ds[is.na(ds)] <- 0

    ## get correlations between all assays and all observations
    corrTest <- corr.test(x = ds[,c(startCol:ncol(ds))], y = ds[,assayCols],
                          use="pairwise.complete.obs", method="spearman",
                          adjust = "BH")

    ## pull out just the assays correlation values and p-values
    pullCorr <- corrTest$r[,c(1:length(assayCols))]
    pullPval <- corrTest$p[,c(1:length(assayCols))]

    ## adjust the header names for season
    makeColNames <- paste(colnames(pullCorr), getSeasons[s], sep = " ")
    colnames(pullCorr) <- makeColNames
    colnames(pullPval) <- makeColNames

    ## correlations
    if(s == 1){
      corr <- as.data.frame(pullCorr)
    } else {
      corr <- cbind(corr, as.data.frame(pullCorr))
    }
  }

  corr <- corr[,order(colnames(corr))]

  corr$features <- rownames(corr)
  corr$group <- group[c(startCol:ncol(df))]
  rownames(corr) <- NULL
  corrMat <- as.matrix(corr)
  meltCorr <- melt(corr, id.vars = c("features","group"))
  meltCorr$value <- as.numeric(paste(meltCorr$value))

  # p-values
  #changeP <- as.matrix(pval)
  #meltP <- melt(changeP)


  ##################
  ## Produce plot ##
  ##################

  fontSize = 17

  w <- ggplot(meltCorr, aes(features, variable)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "#0F1AA3", mid = "white", high = "#9C0000", limits=c(-1,1))

  w <- w + facet_grid(variable~group, scales = "free", switch = "both", space = "free_x") +
    theme(strip.placement = "outside")

  #w <- w + theme(strip.background.y = element_rect(fill="purple"))
  w <- w + theme(strip.text.y.left = element_text(size = fontSize,
                                             colour = "white",
                                             angle = 0,
                                             face = "bold"))

  w <- w + theme(strip.text.x = element_text(size = fontSize,
                                             colour = "white",
                                             angle = 90,
                                             face = "bold"))

  w <- w + theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
  w <- w + theme(axis.title = element_blank())
  w <- w + theme(axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank())


  w <- w + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_blank())

  w <- w + theme(legend.position = "right",
                 legend.key.height = unit(4, "cm"),
                 legend.key.width = unit(1, "cm"),
                 legend.title = element_text(size = fontSize),
                 legend.text=element_text(size=fontSize)) + theme(legend.title=element_blank())

  ## adjust facets for improved interpretability
  g <- ggplot_gtable(ggplot_build(w))

  ## replace left side facet colours
  stripl <- which(grepl('strip-l', g$layout$name))
  fills <- c("dark blue","dark blue",
             "purple","purple",
             "red","red",
             "dark green","dark green")
  k <- 1
  for (i in stripl) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  n_cols <- length(levels(factor(meltCorr$group)))
  txt_colors <- gray(0:n_cols/n_cols)
  fills <- rainbow(n_cols)

  ## replace bottom facet colours
  stripb <- which(grepl('strip-b', g$layout$name))

  k <- 1
  for (i in stripb) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }

  return(g)

}





