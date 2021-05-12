

# To be implemented for data from URL if it will work
#getEwasData <- function(arrays){
# url <- 'd'
#}


# Read raw mockdata
#' Get Raw Data
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
#' @param data Data matrix that must have Study.id, platform, sample size
#' tissue and ancestry columns
#' @return Returns new matrix that contains data with no duplicated Study.id
#' @examples
#' newData <- removeDuplicates(data)
#' @export
removeDuplicates <- function(data){
  # Get unique ID for new matrix, add info
  studId <- as.character(unique(data$Study.id))
  studId <- studId[!is.na(studId)]
  newM <- matrix(nrow = length(studId), ncol = 5)
  colnames(newM) <- c("Study.id", "Platform", "Sample.size", "Tissue", "Ancestry")
  # For each Study.id get sum sample size
  for (id in 1:length(studId)){
    index <- as.character(data$Study.id) == studId[id]
    temp <- data[which(index),]
    platform = NA
    sum = NA
    # Remove factors if there are any
    tissues <- unique(as.character(temp$Tissue))
    platform <- unique(as.character(temp$Platform))

    # Must be same tissue and same platform
    if (length(tissues) == 1  & length(platform) == 1){
      if (platform[1] == "450K"){
        sum = sum(temp$Sample.size)
      }
      newM[id,] <- c(as.character(temp$Study.id[1]), platform[1],
                sum, tissues[1], as.character(temp$Ancestry[1]))
    }
  }
  # Remove all NA value (Platform can be NA)
  newM <- newM[!is.na(newM[,1]),]
  return(newM)
}

# Merge several data matrices, all of them must have same column name to merge by
#'
mergeData <- function(arrays, merge_by){
  data <- arrays[[1]]
  # Merge each data matrix with main data matrix
  for(i in 2:length(arrays)){
    data <- merge(as.data.frame(data), as.data.frame(arrays[[i]]),
                  by=merge_by)
    #main <- remove.factors(merge(stud, cohorts, by= "Study.id"))
  }
  return(data)
}

#' Clean data and merge other matrices
#'
#' @param data A list of data matrices. First matrix must contain Study.id
#' and tissue
#' @param merge_by Parameter which indicated which column matrices will
#' be merged by
#' @return Cleaned and merged dataframe file
#' @examples
#' data <- cleanAndMergeData()
#' data <- cleanAndMergeData(data = c(matrix1, matrix2, matrix3))
#' data <- cleanAndMergeData( merge_by = "Study.id")
#' @export
cleanAndMergeData<- function(data, merge_by){
  # By default calls mock data
  if(missing(data)){
    print("Getting mock data...")
    data <- rawMockData()[1:3]
  }
  if (missing(merge_by)){
    merge_by <- "Study.id"
  }
  print("Removing duplicates...")
  # First one must be cohorts file - contains platforma and tissue
  data[[1]] <- removeDuplicates(data[[1]])
  print("Merging other files...")
  data <- mergeData(data, merge_by)
  return(data)
}

#data <- cleanAndMergeData(merge_by = "Study.id")
#data <- cleanAndMergeData(list(matrix1,matrix2,matrix3),merge_by = "Study.id")
#head(data)
