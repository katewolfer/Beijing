beijingUnivariateList <- function(df, startCol) {

  #############################################
  ## Beijing, v0.0.1                         ##
  ## Analysis of Beijing campaign total data ##
  ## Kate Wolfer, Universitaet Basel         ##
  ## April 2021                              ##
  #############################################

  ## outputs a dataframe containing all Mann-Whitney-U and Spearman correlations
  ## for the Beijing data
  ## should also work for other data providing there are two seasons
  ## seasons can be renamed here in the function
  ## the start column implies all data in this and subsequent columns
  ## is numerical and continuous

  ## INPUTS

  ## df: dataframe of collated atmospheric measurement data
  ## with columns as observations and rows as samples
  ## which has already had the data standardised and cleaned up with
  ## the beijingCleanup function and must contain season-based data
  ## in a column headed "Season". Currently only works for summer/winter!

  ## assayCols: list of integers representing column indices of assay data

  ## startCol: integer of beginning of numeric observation data

  ## check Season column is present
  if (length(which(grepl("Season", names(df))) > 0)){
    getSeasons <- unique(df$Season)
    summer <- df[which(df$Season == "summer"),]
    winter <- df[which(df$Season == "winter"),]
  } else {stop("\n This function only works with season data!")}


  ## create empty worksheet to populate with correlations
  colNames <- c("feature name", "seasonal Mann-Whitney p-value")
  pasteColumns <- NULL
  colCounter <- 1

  for(h in (startCol:ncol(df))){
    pasteColumns[colCounter] <- paste0(colnames(df)[h], " correlation summer")
    colCounter <- colCounter + 1
    pasteColumns[colCounter] <- paste0(colnames(df)[h], " p-value summer")
    colCounter <- colCounter + 1
    pasteColumns[colCounter] <- paste0(colnames(df)[h], " correlation winter")
    colCounter <- colCounter + 1
    pasteColumns[colCounter] <- paste0(colnames(df)[h], " p-value winter")
    colCounter <- colCounter + 1
  }

  colNames <- c(colNames, pasteColumns)
  output <- matrix(NA, nrow = ncol(df)-3, ncol = length(colNames))
  colnames(output) <- colNames
  output[,1] <- colnames(df)[c(4:ncol(df))]
  output <- as.data.frame(output, stringsAsFactors = FALSE)

  # produce the univariate statistical analyses and fill table
  for (i in 1:nrow(output)){
    if ((sum(is.na(df[,i]))/length(df[,i])) > 0.5){

      output[i,2] <- NA

    } else {

      # seasonal Mann-Whitney U p-value
      mwuTest <- wilcox.test(x = df[which(df$Season == "summer"),which(colnames(df) == output[i,1])],
                             y = df[which(df$Season == "winter"),which(colnames(df) == output[i,1])],
                             paired = FALSE, alternative = "two.sided",
                             conf.level = 0.95)
      output[i,2] <- as.numeric(mwuTest$p.value)
    }
  }


  # get all correlations and p-values
  if ((sum(is.na(df[,i]))/length(df[,i])) > 0.5){

  } else {

    sCorrTest <- corr.test(x = summer[,c(startCol:ncol(df))], y = summer[,c(startCol:ncol(df))],
                           use="pairwise.complete.obs", method="spearman", adjust = "BH")
    wCorrTest <- corr.test(x = winter[,c(startCol:ncol(df))], y = winter[,c(startCol:ncol(df))],
                           use="pairwise.complete.obs", method="spearman", adjust = "BH")
    sCorr <- sCorrTest$r
    wCorr <- wCorrTest$r
    sP <- sCorrTest$p
    wP <- wCorrTest$p

    colCounter <- 3

    for (i in 1:ncol(sCorr)){
      output[,colCounter] <- sCorr[,i]
      colCounter <- colCounter + 1
      output[,colCounter] <- sP[,i]
      colCounter <- colCounter + 1
      output[,colCounter] <- wCorr[,i]
      colCounter <- colCounter + 1
      output[,colCounter] <- wP[,i]
      colCounter <- colCounter + 1
    }
  }

  return(output)

}
