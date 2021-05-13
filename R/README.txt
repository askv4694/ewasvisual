How to use:
-------------
1) getEwasData.R

data <- cleanAndMergeData()
#only 10000 rows

--------------
2) processingDistances.R

data <- readRDS("mockdata/wholeMatrix.rds")
# already cleaned and merged

new_data <- rowAndCol(data$Probe.id, data$Study.id)
# or new_data <- readRDS("mockdata/TrueFalse.rds") 

row <- new_data[,12]
sorted_matrix <- getCompatibility(row, new_data)
#sorted by column 

