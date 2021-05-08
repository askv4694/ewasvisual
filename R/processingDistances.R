makeMat <-function(rowNum, row ,col){
  mat <- matrix(nrow = length(unique(row)), ncol = length(unique(col)),
                dimnames = list(unique(row),  unique(col)))

  for (i in 1:rowNum){
    if(i %% 100 == 0){
      cat('\r', i)
      flush.console()
    }
    row1 <- row[i]
    col1 <- col[i]
    mat[row1,col1] <- TRUE
  }
  mat<- replace_na(mat, FALSE)
  return(mat)
}
