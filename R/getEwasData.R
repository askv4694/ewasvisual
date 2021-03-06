#' Get Raw Mock Data
#'
#' @return List of matrices: cohorts, associations, studies and annotations
#' @examples
#' data <- rawMockData()
#' @export
rawMockData <- function(){
  data1 <- readRDS("mockdata/matrix1.rds")
  data2 <- readRDS("mockdata/matrix2.rds")
  data3 <- readRDS("mockdata/matrix3.rds")
  data4 <- readRDS("mockdata/matrix4.rds")
  return(list(data1,data2,data3,data4))
}

# Removes multiple study.ID since ID can have several platforms
#' Remove duplicate Study.id
#'
#' @param data Data matrix that must have Study.id, Platform, Sample.size
#' Tissue and Ancestry columns
#' @return Returns new matrix that contains data with no duplicated Study.id
#' @examples
#' newData <- removeDuplicates(data)
#' @export
removeDuplicates <- function(data){
  # Get unique ID for new matrix, add info
  studId <- unique(data$Study.id)
  studId <- studId[!is.na(studId)]
  newM <- matrix(nrow = length(studId), ncol = 5)
  colnames(newM) <- c("Study.id", "Platform", "Sample.size",
                      "Tissue", "Ancestry")
  # For each Study.id get sum sample size
  for (id in 1:length(studId)){
    index <- data$Study.id == studId[id]
    temp <- data[which(index),]
    platform = NA
    sum = NA
    # Must be same tissue and same platform
    if (length(unique(temp$Tissue)) == 1  &
        length(unique(temp$Platform)) == 1){
      if (temp$Platform[1] == "450K"){
        sum = sum(temp$Sample.size)

        newM[id,] <- c(temp$Study.id[1], temp$Platform[1],
                sum, temp$Tissue[1], temp$Ancestry[1])
      }
    }
  }
  # Remove all NA value (Platform can be NA)
  newM <- newM[!is.na(newM[,1]),]
  return(newM)
}

#' Merge several data matrices
#'
#' @param matrices A list of matrices to merge
#' @param merge_by A column name that will be merged by.
#' Must have same column name
#' @return Returns merged dataframe of listed matrices
#' @examples
#' data<- mergeData(list(mat1, mat2, mat3), "Study.id")
#' @export
mergeData <- function(matrices, merge_by){
  data <- data.frame(matrices[[1]], stringsAsFactors = FALSE)
  # Merge each data matrix with main data matrix
  for(i in 2:length(matrices)){
    data <- merge(data, data.frame(matrices[[i]],stringsAsFactors = FALSE),
                  by=merge_by)
    #main <- remove.factors(merge(stud, cohorts, by= "Study.id"))
  }
  return(data)
}

#' Leave only certain columns
#'
#' @param data A dataframe of merged matrices
#' @return Dataframe with Study.id, Sample.size, Tissue, Trait, PMID, Ancestry
#' @examples
#' data <- deleteCols(dataFrame)
#' @export
deleteCols <- function(data){
  df <- data.frame("Study.id" = data$Study.id, "Probe.id" = data$Probe.id,
                   "Sample.size" = data$Sample.size,
                   "Tissue" = data$Tissue,"Trait" = data$Trait.x,
                   "PMID" = data$PMID.x, "Ancestry" = data$Ancestry,
                   stringsAsFactors = FALSE)
  return(df)
}

#' Clean data and merge other matrices
#'
#' @param data A list of data matrices, first matrix must contain Study.id
#' and tissue
#' @param merge_by Parameter which indicated which column matrices will
#' be merged by
#' @return Cleaned and merged dataframe file
#' @examples
#' data <- cleanAndMergeData()
#' data <- cleanAndMergeData(data = list(matrix1, matrix2, matrix3))
#' data <- cleanAndMergeData( merge_by = "Study.id")
#' @export
cleanAndMergeData<- function(data, merge_by = "Study.id"){
  # By default calls mock data
  if(missing(data)){
    print("Getting mock data...")
    data <- rawMockData()[1:3]
  }
  print("Removing duplicates...")
  # First one must be cohorts file - contains platforma and tissue
  data[[1]] <- removeDuplicates(data[[1]])
  print("Merging other files...")
  data <- mergeData(data, merge_by)
  data <- deleteCols(data)
  data <- data[which(grepl("cg\\d{8}",data$Probe.id)),]
  data$Sample.size <- as.numeric(data$Sample.size)
  return(data)
}
