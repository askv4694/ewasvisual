
#' True and False matrix
#'
#' @param row An array of rowanames from the matrix.
#' @param col An array of colnames from the matrix.
#' @return Returns a matrix of rows and columns. Value is either TRUE or
#' FALSE depending on column and row match.
#' @examples
#' matrix <- rowAndCol(c("cg00000000", "cg00000000"), c("Headache","Ache"))
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
#' @param col An array that has more positions that the matrix.
#' @param data A dataframe which will be appended.
#' @return Return appended dataframe.
#' @examples
#' data <- appendMatrixSize(col, dataFrame)
#' @export
appendMatrixSize <- function(col, data){
  toAdd <- unique(col) %in% unique(rownames(data))

  # Check if there are new positions
  if (length(which(!toAdd)) == 0){
    return(0)
  }
  toAdd <- unique(col)[which(!toAdd)]
  for(i in 1:length(toAdd)){
    row <- c(rep(FALSE, times = ncol(data)))
    data <- rbind(data, row)
    rownames(data)[nrow(data)] <- toAdd[i]
  }
  return(data)
}

#' Make an array of tue/false for given positions
#'
#' @param change An araay with names that will be changed to.
#' @param col An array with names which will have same names
#' with same order as change array.
#' @return Rearranged array. May be extended accordingly.
#' @examples
#' row <- rearrange(c("a", "b", "c"), c("c", "b", "t"))
makeArray <- function(change,col){
  col <- change %in% col
  names(col) <- change
  return(col)
}



#' Estimated values of odd ands pval
#'
#' @param data Matrix that has numerical values of True/False ratio.
#' @param col An array which will be used as a base to test to.
#' @return Returns two matrices: first one is odds and the other - pvalues.
#' @examples
#' list_of_datas <- getEstimatedVals(matrix, colnames)
#' @export
getEstimatedVals <- function(data, col){
  #odds  <- matrix(nrow = nrow(data), ncol = ncol(data),
  #      dimnames = list(rownames(data),  colnames(data)))
  col <- makeArray(rownames(data), col)
  odds <- c()
  pvals <- odds

  for (i in 1:ncol(data)){
    tab <- table(col, data[,i])
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
#' @param data Matrix that has numerical values of True/False ratio.
#' @param row An array with TRUE/FALSE which will be for base row.
#' Must have name for reach value.
#' @return Returns an array of studies which are ordered based on similarity.
#' @examples
#' odds_and_pvals <- getSimilarCols(data, row)
#' @export
getSimilarCols <- function(data, col = data[,13]){

  vals <- getEstimatedVals(data,col)
  #pvals
  odds <- vals[vals$odds > 1 & vals$pvals < 0.05,]
  odds <- odds[order(odds$odds, decreasing = TRUE),]
  #similar <- data[,names(odds)]
  return(rownames(odds))
}

#' Append all missing cg positions from annotation
#'
#' @param data Matrix of positions and studies with TRUE/FALSE
#' @return Data matrix with new positions at the end.
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

