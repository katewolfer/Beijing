beijingCleanup <- function(df, startCol){

  #############################################
  ## Analysis of Beijing campaign total data ##
  ## Kate Wolfer, Universitaet Basel         ##
  ## March 2021                              ##
  #############################################

  ## Simple data cleanup to fully standardise data
  ## You may need to check your input for mixed column types
  ## i.e. columns should contain either only numbers, or text and numbers,
  ## which will all be interpreted as text

  ## INPUTS

  ## df: dataframe of collated atmospheric measurement data
  ## with columns as observations and rows as samples

  ## startCol: integer of beginning of numeric observation data
  ## which needs to be normalised to ensure all values are actual numerical
  ## format, have standardised units no major outliers

  cat("Is data mass (ug/ug) or volume (ug/m3) normalised? \n")
  massOrVol <- readline("Enter 'mass' or 'volume': \n")

  # ensure all numeric values are really numeric
  for (c in startCol:ncol(df)){
    df[,c] <- as.numeric(paste(df[,c]))
  }

  if(massOrVol == "mass"){
    ## convert units MASS
    findConcs <- grep("/ug",  saveUnits)
    findng <- grep("ng",  saveUnits)

    for (mc in 1:length(findng)){
      df[,findng[mc]] <- as.numeric(df[,findng[mc]])/1000}
  } else if (massOrVol == "volume"){
    ## convert units VOLUME
    findConcs <- grep("/m3",  saveUnits)
    findng <- grep("ng",  saveUnits)
    for (i in 1:length(findng)){
      df[,findng[mc]] <- as.numeric(df[,findng[mc]])/1000}
  }


  ## remove outlier features
  for (c in startCol:ncol(df)){
    findOutliers <- which(df[,c] > 5*sd(df[,c], na.rm = TRUE))
    print(paste0("Column ", c, ", outliers include ", length(findOutliers)," observations"))
    df[findOutliers,c] <- NA
  }

  return(df)

}



