
#' True and False matrix
#'
#' @param rowNum A number which indicates the length of a row in matrix
#' @param row An array of rowanames from the matrix
#' @param col An array of colnames from the matrix
#' @return Returns a matrix of rows and columns. Value is either TRUE or
#' FALSE depending on column and row match.
#' @examples
#' matrix <- rowAndCol(2, c("cg_00000000", "cg_00000000"), c("Headache","Ache"))
#' matrix <- rowAndCol(length(dataFrame$Probe), dataFrame$Probe, dataFrame$Trait)
#' @export
rowAndCol <-function(row ,col){
  row <- as.character(row)
  col <- as.character(col)
  mat <- matrix(nrow = length(unique(row)), ncol = length(unique(col)),
                dimnames = list(unique(row),  unique(col)))

  for (i in 1:length(row)){
    row1 <- row[i]
    col1 <- col[i]
    mat[row1,col1] <- TRUE
  }
  mat
  mat <- !is.na(mat)
  return(mat)
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
  arr <- c()
  for(i in 1:length(data[1,])){
    sum <- 0
    # id will get position from row since it can be diffrent
    #from row's position
    id <- names(data[,i])
    for(j in 1:length(id)){
      rowid <- which(names(row) == id[j])
      rowObj <- as.character(row[rowid])
      if((rowObj == "TRUE") & (data[j,i] == TRUE)){
        sum <- sum +1
      }
    }
    # Store all sums for later sorting
    # For each column id
    arr[i] <- sum
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
#' @param anno Array of names from annotation that contains cg_00000000.
#' @param col Array of column names from the matrix.
#' @return Returns two matrices: first one is odds and the other - pvalues.
#' @examples
#' list_of_datas <- getEstimatedVals(matrix, annoNames, colnames)
#' @export
getEstimatedVals <- function(data, anno, col){
  odds  <- matrix(nrow = length(unique(col)), ncol = length(unique(col)),
        dimnames = list(unique(col),  unique(col)))
  pvals <- matrix(nrow = length(unique(col)), ncol = length(unique(col)),
        dimnames = list(unique(col),  unique(col)))
  for(i in 1:length(col)){
    col1 <- anno %in% rownames(data)[data[,i]]
    col1 <- factor(col1, levels = c("TRUE", "FALSE"))
    for(j in i:length(col)){
      col2 <- anno %in% rownames(data)[data[,j]]
      col2 <- factor(col2, levels = c("TRUE", "FALSE"))
      tab  <- table(col1, col2)
      fish <- fisher.test(tab)

      odds[i,j]  <- fish$estimate
      pvals[i,j] <- fish$p.value
    }
  }
  return(list(odds,pvals))
}

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
