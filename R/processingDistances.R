#' True and False matrix
#'
#' @param row An array of rowanames from the matrix
#' @param col An array of colnames from the matrix
#' @return Returns a matrix of rows and columns. Value is either TRUE or
#' FALSE depending on column and row match
#' @examples
#' matrix <- rowAndCwol(c("cg00000000", "cg00000000"), c("Headache","Smoking"))
#' matrix <- rowAndCol(dataFrame$Probe, dataFrame$Trait)
#' @export
rowAndCol <-function(row ,col){
  mat <- matrix(nrow = length(unique(row)), ncol = length(unique(col)),
                dimnames = list(unique(row),  unique(col)))

  for (i in 1:length(row)){
    row1 <- row[i]
    col1 <- col[i]
    mat[row1,col1] <- TRUE
  }
  mat <- !is.na(mat)

  return(mat)
}

#' Append matrix rows
#'
#' @param col An array that has more positions that the matrix
#' @param data A dataframe which will be appended
#' @return Return appended dataframe
#' @examples
#' data <- appendMatrixSize(col, dataFrame)
#' @export
appendMatrixSize <- function(col, data){
  toAdd <- unique(col) %in% unique(rownames(data))

  # Check if there are new positions
  if (length(which(!toAdd)) == 0){
    return(FALSE)
  }
  toAdd <- unique(col)[which(!toAdd)]
  for(i in 1:length(toAdd)){
    row <- c(rep(FALSE, times = ncol(data)))
    data <- rbind(data, row)
    rownames(data)[nrow(data)] <- toAdd[i]
  }
  return(data)
}

#' Make an array of TRUE/FALSE for given positions
#'
#' @param change An araay with names that will be changed into
#' @param col An array with names which will have same names
#' with same order as change array as well as TRUE/FALSE values
#' @return Rearranged array
#' @examples
#' row <- rearrange(c("a", "b", "c"), c("c", "b", "t"))
makeArray <- function(change,col){
  col <- change %in% col
  names(col) <- change
  return(col)
}

#' Estimated values of odd ands pval
#'
#' @param data Matrix that has numerical values of True/False ratio
#' @param col An array which will be used as a base to test to
#' @return Returns two matrices: first one is odds and the other - pvalues
#' @examples
#' list_of_datas <- getEstimatedVals(matrix, colnames)
#' @export
getEstimatedVals <- function(data, col){
  names <- rownames(data)

  col <- makeArray(names, col)
  odds <- c()
  pvals <- odds
  for (i in 1:ncol(data)){
    colf <- factor(col, levels = c(TRUE,FALSE))
    dataf <- factor(data[,i], levels = c(TRUE,FALSE))
    tab <- table(colf, dataf)
    fisher <- fisher.test(tab)
    odds[i] <- fisher$estimate
    pvals[i] <- fisher$p.value
  }
  df <- data.frame(odds = odds, pvals = pvals, stringsAsFactors = FALSE)
  rownames(df) <- colnames(data)
  return(df)
}

#' Get similar studies
#'
#' @param data Matrix that has numerical values of True/False ratio
#' @param col An array with TRUE/FALSE which will be for base row
#' Must have name for reach value.
#' @return Returns an array of studies which are ordered based on similarity
#' @examples
#' odds_and_pvals <- getSimilarCols(data, row)
#' @export
getSimilarCols <- function(data, col){

  vals <- getEstimatedVals(data,col)
  odds <- vals[vals$odds > 1 & vals$pvals <= 0.05,]
  odds <- odds[order(odds$odds, decreasing = TRUE),]
  return(odds)
}

#' Append all missing cg positions from annotation
#'
#' @param data Matrix of positions and studies with TRUE/FALSE
#' @return Data matrix with new positions at the end
#' @examples
#' data <- maxMatrix(dataMatrix)
#' @export
maxMatrix <- function(data){
  # All cg positions from anno
  probes <- readRDS("mockdata/allProbes.rds")
  # Get only cg positions
  print("Appending new positions to a matrix")
  probes <- probes[which(grepl("cg\\d{8}",unique(probes)))]
  # Add new positions to a matrix of TRUE/FALSE
  data <- appendMatrixSize(probes, data)
  return(data)
}

#' Filter data according to given array
#'
#' @param data A dataframe of merged data
#' @param colName A column name which will be filtered by
#' @param array An array that determins which rows will be left.
#' @return A dataframe of filtered data
#' @examples
#' data <- filterBy(data, "Probe.id", c("cg02609880", "cg06874172"))
#' data <- filterBy(data, "Study.id", c("ES00034, "ES00035))
#' data <- filterBy(data, "Trait", c("aging", "air pollution (PM2.5)"))
#' @export
filterBy <- function(data, colName, array){
   data <- data[which(data[,colName] %in% array),]
   return(data)
}

####

#' Process data into a dataframe for connections
#'
#' @param data A dataframe of general data
#' @param positions An array of cg positions in the new study
#' that wants to be checked, 0 by default - all positions included
#' @param studies An array of study names that wants to be checked,
#' 0 by default - all study names included
#' @param studByTraits A list of trait names to be filtered by. 0 by default
#' @param maxPositions Set to FALSE by Default. If set to TRUE,
#' 900k positions will be used in general
#' @param minSampleSize Minimal sample size to be used. Set to 100 by default
#' @return A matrix of positions x studies with TRUE/FALSE values. If a cg
#' position is found in specific study - valuse set to TRUE
#' @examples
#' data1 <- processData(data, col)
#' data1 <- processData(data, col, positions, studies)
#' data1 <- processData(data, col, positions, minSampleSize = 500)
#' @export
processData <- function(data, positions = 0, studies = 0,
              studByTraits = 0,
              maxPositions = FALSE, minSampleSize = 100){

  data <- data[data$Sample.size > minSampleSize,]

  if(typeof(positions) == "character"){
    data<- filterBy(data, "Probe.id", positions)
  }
  if (typeof(studies) == "character"){
    data <- filterBy(data, "Study.id", studies)
  }
  if(typeof(studByTraits) == "character"){
    data <- filterBy(data, "Trait", studByTraits)
  }



  print("Processing data into positions x studies matrix...")
  data <- rowAndCol(data$Probe.id, data$Study.id)
  if (maxPositions){
    data <- maxMatrix(data)
  }

  if(typeof(positions) == "character"){
    mat <- appendMatrixSize(positions,data)
    if(typeof(mat) != "logical"){
      data <- mat
    }
  }
  return(data)
}

#' Convert studyNames to ids for connections
#'
#' @param df A dataframe that contains 'from' and 'to' as numeric values
#' @param cln Either 'from' or 'to' colnames that are used for ID converion
#' @return A dataframe with converted 'from' and 'to' columns
#' @examples
#' df <-ConvertIDS(df, "from")
#' data <-ConvertIDS(df, "to")
#' @export
convertIDS <- function(df, cln){
  names <- unique(df[,cln])
  for(i in 1:length(names)){
    ids <- which(df[,cln] %in% names[i])
    df[ids, cln] <- df$id[df$Study.id == names[i]][1]
  }
  return(df)
}

#' Get new row
#'
#' @param df A dataframe which will be appended
#' @param data A data matrix of TRUE/FALSE
#' @param col An array of cg positions that will be compared
#' @param name A name of the study
#' @param shape Shape for connection interpreting
#' @param color Color that is used in connections
#' @return Returns a dataframe of unique positions and their traits
#' @examples
#' df <- getNewRow(df, data, positions, "new_study", "star", "purple")
#' @export
getNewRow <- function(df, data, col, name, shape, color){
  if (length(col) == 0){
    return(df)
  }
  vals <- getSimilarCols(data, col)
  id <- nrow(df)+1
  if(nrow(vals) == 0){
    return(df)
  }
  for (i in 1:nrow(vals)){
    arr <- makeArray(rownames(data), col)
    names <- rownames(vals)[i]
    tab <- table(factor(arr, levels = c(TRUE,FALSE)),
                 factor(data[,names], levels = c(TRUE,FALSE)))
    df[nrow(df)+1,] <- c(nrow(df)+1, "", vals$odds[i], shape, name,color,
                        name, rownames(vals)[i],
                        round(tab[1]/sum(arr | data[,names]),digits = 2), sum(arr))
  }
  return(df)
}

#' Trait table
#'
#' @param dara Primary dataframe that is used for identifying
#' traits according to study names
#' @return A dataframe with trait for each study name
#' @example
#' traits <- traitTable(data)
#' @export
traitTable <- function(data){
  names <- unique(data$Study.id)
  df <- data[1,c("Study.id", "Trait", "PMID", "Ancestry")]
  for(i in 1:length(names)){
    df[i,] <- c(names[i],
                unique(data$Trait[data$Study.id == names[i]]),
                unique(data$PMID[data$Study.id ==names[i]]),
                unique(data$Ancestry[data$Study.id ==names[i]]) )
  }
  return(df)
}

#' Add a new study with a trait name
#'
#' @param traits A trait table that is used for conversion
#' @param df A dataframe of unique cg positions with traits
#' @param name Set empty by default. New study name
#' @param traitName Set empty by default. New trait name
#' @return A dataframe with a new study appended
#' @example
#' data<- addTrait(traits, df, "new_Study", "aging")
#' @export
addTrait <- function(traits, df, name="", traitName = ""){
  names <- unique(df$Study.id)
  for (i in 1:length(names)){
    ids <- which(df$Study.id %in% names[i])
    df[ids,"trait"] <- traits$Trait[traits$Study.id == names[i]][1]
    df[ids, "PMID"] <- traits$PMID[traits$Study.id == names[i]][1]
    df[ids, "Ancestry"] <- traits$Ancestry[traits$Study.id == names[i]][1]
  }
  df[which(is.na(df$trait)),"trait"] <- traitName
  return(df)
}

#' Set trait color
#'
#' @param df A dataframe that shows connections for visNetwork
#' @param colors An array of colors that defines trait color in graph
#' @param traits An array of traits that needs to be colored
#' @return A dataframe with changed colors
#' @examples
#' df <- setTraitColors(df, c("red", "green"), c("smoking", "aging"))
#' @export
setTraitColors <- function(df,colors, traits){
  for(i in 1:length(colors)){
    df[which(df$trait %in% traits[i]), "color"] <- colors[i]
  }
  return(df)
}

#############

#' Make a dataframe of linked studies
#'
#' @param data A data matrix with TRUE/FALSE values
#' @param minSize A number that defines minimum sample size: > X. 50 by default
#' @param col An array of cg positions in the new study
#' @param name A name of the study
#' @param type Disease type
#' @param traitMatrix A dataframe of studies and their traits
#' @param traits A list of trait names to filter by
#' @param color An array of colors to be used. First is for new and the
#' second if for other studies. Red and darcyan by default
#' @param shape An array of shapes for plot. Circle and elipsis by default
#' @return A dataframe that is used in visNetwork
#' @example
#' data2 <- makeConnections(data, positions, traits,
#'      c("green", "yellow"), c("star", "circle"))
#' @export
makeConnections <- function(data, minSize = 50, col = 15 ,name,
                            type ,traitMatrix,
                            traits, color = c("red","darkcyan"),
                            shape = c("circle", "elipsis")){

  if(typeof(col) == "character"){
    mat <- appendMatrixSize(col,data)
    if(typeof(mat) != "logical"){
      data <- mat
    }
  }
  sums <-apply(data, 2, sum)
  morethanX <- names(sums[sums > minSize])
  length(morethanX)
  data<- data[,colnames(data)%in% morethanX]

  study_names <- c(name,colnames(data))

  if(typeof(col) == "double"){
    col <- names(which(data[,col] == TRUE))
  }

  df <- data.frame(id = integer(), Ancestry = character(),
                   odds = double(), shape = character(),
                   Study.id = character(), color = character(),
                   from = character(), to = character(),
                   coef = double(), count = integer(),
                   stringsAsFactors = FALSE)
  df <- getNewRow(df,data,col, name, shape[1], color[1])
  for(i in 1:length(study_names)-1){
    temp_col <- names(which(data[,i] == TRUE))
    df <- getNewRow(df, data[,-c(i), drop = FALSE], temp_col, colnames(data)[i],
                    shape[2], color[2])
  }

  df$id<- as.numeric(df$id)
  df$odds <- as.numeric(df$odds)
  df$count <- as.numeric(df$count)

  df <- convertIDS(df, "from")
  df <- convertIDS(df, "to")

  df$from <- as.numeric(df$from)
  df$to <- as.numeric(df$to)

  df$coef <- as.numeric(df$coef)

  df <- addTrait(traitMatrix, df, name, type)
  if((length(color) > 2)){
    df <- setTraitColors(df, colors[-c(1:2)], traits)
  }

  df$edgeCol <- "grey"
  df$edgeCol[df$from == 1] <- color[1]
  df$color[df$from == 1] <- color[1]
  df$trait[df$from == 1] <- type
  df$PMID[df$from == 1] <- "NA"
  return(df)
}


#' Assign color by group
#'
#' @param data A dataframe from makeConnections
#' @param colorData A dataframe with group names and study.id
#' @param colors A list of colors that should be assigned for each group
#' @return A dataframe with colors by group name
#' @examples
#' colored_data <- groupColor(data, colorData, c("yellow", "green"))
#' colored_data <- groupColor(data, colorData, c("#22105", "#00FF4E"))
#' @export
groupColor <- function(data, colorData, colors){
  library(RColorBrewer)
  #names <- unique(data$Study.id)
  groups <- unique(colorData$Group)
  #ncol <- length(groups)

  data$group <- ""
  for(i in 1:length(groups)){
    ids <- colorData$Study.id[colorData$Group == groups[i]]
    data$color[data$Study.id %in% ids] <- colors[i]
    data$group[data$Study.id %in% ids] <- groups[i]
  }
  data$group[data$from == 1] <- "Your study"
  #data$color[df$from == 1] <- unique(data$edgeCol[data$from == 1])
  return(data)
}


