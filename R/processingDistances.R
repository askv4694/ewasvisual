
#' True and False matrix
#'
#' @param row An array of rowanames from the matrix.
#' @param col An array of colnames from the matrix.
#' @return Returns a matrix of rows and columns. Value is either TRUE or
#' FALSE depending on column and row match.
#' @examples
#' matrix <- rowAndCol(2, c("cg00000000", "cg00000000"), c("Headache","Ache"))
#' matrix <- rowAndCol(length(dataFrame$Probe), dataFrame$Probe, dataFrame$Trait)
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

#' Rearrange given row names
#'
#' @param change An araay with names that will be changed to.
#' @param row An array with names which will have same names
#' with same order as change array.
#' @return Rearranged array. May be extended accordingly.
#' @examples
#' row <- rearrange(c("a", "b", "c"), c("c", "b", "t"))
rearrange <- function(change,col){
  col <- names(change)%in% names(col)
  names(col) <- names(change)
  return(col)
}

#' Sorted matrix compatibility with given row
#'
#' @param row An array of positions with TRUE/FALSE values.
#' Order does not matter.
#' @param data Data matrix which will be compared with given row.
#' @return Sorted matrix depending on compatibility with given row.
#' @examples
#' data <- getCompatibility(arrayOfPositions,dataMatrix)
#' @export
getCompatibility <- function(row, data){
  row <- rearrange(data[,1], row)
  row <- as.vector(row)
  arr <- c()
  for(i in 1:length(data[1,])){
    sum <- 0
    arr[i] <- sum(row & data[,i])
  }
  names(arr) <- c(1:length(data[1,]))
  arr <- sort(arr, decreasing = TRUE)
  index <- as.numeric(names(arr))
  return(data[,index])
}

#' New matrix for distance
#'
#' @param matrix True-False Matrix for caclulations.
#' @param rownames All rownames from matrix. Names should be unique.
#' @param colnames All colnames from matrix. Names should be unique.
#' @return Returns matrix containing float value for each row-column.
#' @examples
#' distances <- getDistance(matrix_T_F, rownames, colnames)
#' @export
getDistance <- function(matrix, rownames,colnames){
  mat <- matrix(nrow = length(rownames), ncol = length(colnames),
                dimnames = list(rownames,colnames))
  for(i in rownames){ #row
    for(j in colnames){ #every col
      mat[i,j] <- sum(matrix[,i] & matrix[,j])/sum(matrix[,i] | matrix[,j])

    }
  }
  return(mat)
}

#' Estimated values of odd ands pval
#'
#' @param data Matrix that has numerical values of True/False ratio.
#' @param row An array which will be used as a base to test to.
#' @return Returns two matrices: first one is odds and the other - pvalues.
#' @examples
#' list_of_datas <- getEstimatedVals(matrix, annoNames, colnames)
#' @export
getEstimatedVals <- function(data, row){
  #odds  <- matrix(nrow = nrow(data), ncol = ncol(data),
  #      dimnames = list(rownames(data),  colnames(data)))
  odds <- c()
  pvals <- odds

  for (i in 1:ncol(data)){
    tab <- table(row, data[,i])
    fisher <- fisher.test(tab)
    odds[i] <- fisher$estimate
    pvals[i] <- fisher$p.value
  }

  return(list(odds,pvals))
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
getSimilarCols <- function(data, row = data[,13]){

  odds <- getEstimatedVals(data,row)
  odds <- odds[[1]]
  names(odds)<- colnames(data)
  #pvals
  odds <- odds[odds > 0]
  odds <- sort(odds, decreasing = TRUE)
  #similar <- data[,names(odds)]
  return(names(odds))
}

#kokia liga, nauja funkc

#' FIll repetitive data
#'
#' @param matrix Matrix that has NA and is mirror type.
#' @return Returns full matrix.4
#' @export

makeEqual <- function(pval){
  for(i in 1:ncol(pval)){
    for(j in i:ncol(pval)){
      pval[j,i] <- pval[i,j]
    }
  }
  return(pval)

}

