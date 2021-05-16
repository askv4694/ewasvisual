

hideArrows <- function(df){
  for(i in 1:nrow(df)){
    row <- df[i,]
    check <- which(df$from == row$to & df$to == row$from)
    if (length(check) > 0 && df$arrows[i] != ""){
      df[check,"arrows"] <- ""
      df[check,"to"] <- 0
    }
  }
  return(df)
}


makeNodes <- function(df){
  df <- df[union(df$from,df$to),]
  nodes <- data.frame(id = df$id,
                      group = df$trait,
                      label = df$id,
                      value = df$value,
                      shape = df$shape,
                      title = apply(df, 1, function(x)
                        paste(x["title_n"], x["trait"], sep = "\n")),
                      color = df$color
                      #shadow = FALSE
                      )
  return(nodes)
}

makeEdges <- function(df){
  edges <- data.frame(from = df$from,
                      to = df$to,
                      value = df$title_e,
                      #length = df$title_e,
                      #arrows = df$arrows,
                      #dashes = FALSE,
                      title = df$title_e
                      #smooth = FALSE,
                      #shadow = FALSE
                      )
  return(edges)
}


getVisNetwork <- function(data, save = FALSE, path, name){
  library(visNetwork)
  data <- hideArrows(data)
  nodes <- makeNodes(data)
  edges <- makeEdges(data)
  v <-visNetwork(nodes, edges, width = "100%") %>%
    visClusteringOutliers(1)
  if(save){
    visSave(v, file.path(paste0(path,"/",name)),
            selfcontained = TRUE, background = "white")
  }
  return(v)
}

getSimilarTraits <- function(data, study = "", id = 0){
  if (id == 0){ id <- data2$id[data2$title_n == study][1]}
  dat <- data2[data2$from == id,]
  #dat[,-c("from", "to","arrows")]
  return(dat)
}


#######################
########################
#########################

library(dplyr)
library(tidyverse)
dataset <- structure(list(ID = c(6, 7, 8, 9, 10, 11, 12, 13, 14,
                       15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
          Category = c("5 - Expert", "2 - Novice", "3 - Intermediate", "5 - Expert",
                                       "2 - Novice", "3 - Intermediate", "3 - Intermediate",
                       "3 - Intermediate", "2 - Novice", "3 - Intermediate", "2 - Novice", "4 - Advanced",
                       "2 - Novice", "3 - Intermediate", "2 - Novice", "5 - Expert", "4 - Advanced",
                       "2 - Novice", "2 - Novice", "3 - Intermediate"),
          Task1 = structure(c(300, 360, 240, 180, 180, 240, 240, 360, 300,
                              300, 180, 360, 240, 240, 240, 300, 240, 240, 240, 240),
                            class = c("hms", "difftime"), units = "secs"),
          Task2 = structure(c(480, 360, 660, 420, 660, 240, 660, 540, 780, 360, 540, 720, 360,
                              480, 540, 300, 420, 600, 240, 660), class = c("hms", "difftime"),
                            units = "secs"), Task3 = structure(c(360, 480, 240, 300, 240, 240, 240, 240,
                                             240, 180, 240, 180, 120, 120, 240, 240,
                                           240, 240, 300, 240), class = c("hms", "difftime"),
                            units = "secs")), row.names = c(NA, -20L),
          class = c("tbl_df", "tbl", "data.frame"))

dataset_long <- dataset %>% gather(task, value, Task1:Task3)

ggplot(dataset_long) + geom_col(aes(x = Category, y = value, fill = task))

library(ggplot2)
#ggplot(data=iris, aes(x=Sepal.Width,fill = Species)) + geom_histogram()
ggplot(data=data2, aes(x=title_e ,fill = trait)) + geom_histogram()

stackedHistogram <- function(data, positions, studies){
  data <- data[data$Probe.id %in% positions & data$Study.id %in%studies,]
  names <- unique(data$Trait)
  #spec <-

  ggplot(data, aes_string(x=Probe.id)) + geom_histogram()+
    scale_x_discrete(labels <- unique(data$Probe.id))
}





library(networkD3)
library(dplyr)


# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=data$Trait[c(1,100:110, 200:250)],
  target=data$Probe.id[c(1,100:110, 200:250)],
  value=1
)

nodes <- data.frame(
  name=c(as.character(links$source),
         as.character(links$target)) %>% unique()
)
# With networkD3, connection must be provided using id,
#not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1

p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name",
                   sinksRight=FALSE)
p
#library(htmlwidgets)
#library("savePlotAsPng")
#install.packages("savePlotAsPng")
#savePlotAsPng(v, file = "mockdata/v.png", width = 600, height = 480)

#saveWidget(v, file = "mockdata/v.html")

