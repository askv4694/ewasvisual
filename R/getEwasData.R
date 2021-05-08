

# To be implemented for data from URL if it will work
#getEwasData <- function(arrays){
# url <- 'd'
#}

# Read raw mockdata
rawMockData <- function(){
  data1 <- readRDS("mockdata/matrix1.rds")
  data2 <- readRDS("mockdata/matrix2.rds")
  data3 <- readRDS("mockdata/matrix3.rds")
  data4 <- readRDS("mockdata/matrix4.rds")
  return(list(data1,data2,data3,data4))
}

# Removes multiple study.ID since ID can have several platforms
removeDuplicates <- function(data){
  # Get unique ID for new matrix, add info
  studId <- unique(data$Study.id)
  newM <- matrix(nrow = length(studId), ncol = 5)
  colnames(newM) <- c("Study.id", "Platform", "Sample.size", "Tissue", "Ancestry")
  # For each Study.id get sum sample size
  for (id in 1:length(studId)){
    temp <- data[data$Study.id == studId[id],]
    platform = NA
    sum = NA
    # Must be same tissue and same platform
    if (length(unique(temp$Tissue)) == 1  & length(unique(temp$Platform)) == 1){
      if (temp$Platform[1] == "450K" || temp$Platform[1] == "850K"){
        platform = temp$Platform[1]
        sum = sum(temp$Sample.size)
      }
    }
    newM[id,] <- c(as.character(temp$Study.id[1]), platform, sum,
                   as.character(temp$Tissue[1]), as.character(temp$Ancestry[1]))
  }
  # Remove all NA value (Platform can be NA)
  newM <- newM[!is.na(newM[,"Platform"]),]
  return(newM)
}

# Merge several data matrices, all of them must have same column name to merge by
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

#
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

saveAsRDS<- function(data, filename){
  write.csv(data, filename, append = FALSE, sep = "\t")
}

clean<- function(){
  rm(list= ls())
}


#data <- cleanAndMergeData(merge_by = "Study.id")
#head(data)
