How to use:
-------------
1) getEwasData.R

data <- cleanAndMergeData()
#only 10000 rows by default

--------------
2) processingDistances.R

data <- readRDS("mockdata/wholeMatrix.rds")
# already cleaned and merged (updated)

new_data <- rowAndCol(data$Probe.id, data$Study.id)
# or new_data <- readRDS("mockdata/TrueFalse.rds") 

-------
Side note : 

only matrix2 has probe.id. After selecting only 450K
platforms the rest of unique probe.id is 203900 cg positions.

Suggestion: if external study has at least one different cg position
then append the dataframe row. New row will have all FALSE since it does
not exist in dataframe.
data <- appendMatrixSize <- function(col, data)
---

row <- new_data[,12]
sorted_matrix <- getCompatibility(row, new_data)
#sorted by column 

