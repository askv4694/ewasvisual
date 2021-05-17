

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

  if (id == 0){
    id <- data$id[data$title_n == study][1]
  }

  dat <- data[data$from == id,]
  #dat[,-c("from", "to","arrows")]
  return(dat)
}

#
data <- readRDS("mockdata/wholeMatrix.rds")
col <- unique(data$Probe.id)[c(1:22,33:44,48:68,102:132)]
positions <- unique(data$Probe.id)[1:300]
studies <- unique(data$Study.id)[c(1:14,18:24,55, 76:99,105)]
data1 <- processData(data,col, positions, studies)
traits <- traitTable(data)
data2 <- makeConnections(data1, col, "STUDY", traits)
v<- getVisNetwork(data2)
v

sim <- getSimilarTraits(data2, id = 1)
#######################
########################
#########################

library(dplyr)
library(tidyverse)

dfForHistogram <- function(data, traits, positions){
  library(ggplot2)
  traits <- unique(traits)
  pos <- unique(positions)
  data <- data[data$Probe.id %in% positions & data$Trait %in% traits,]
  df <- data.frame("Probe.id" = character(), "Trait" = character(),
             "count" = numeric(), stringsAsFactors = FALSE)
  id <- 1
  for(i in 1:length(pos)){
    for(j in 1:length(traits)){
      count <- length(data$Probe.id[data$Probe.id == pos[i] &
                                    data$Trait == traits[j]])
      if(count > 0){
        df[id,1] <- pos[i]
        df[id,2] <- traits[j]
        df[id,3] <- count
        id <- id +1
      }
    }
  }
  return(df)
}

stackedHistogram <- function(df){
  g <- ggplot(df[df$count > 1,], aes(x = Probe.id, y = count,
                 label = count, fill = Trait)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(g)
}

#unique(df$trait)
#ggplot(df[df$count > 3,]) + geom_col(aes(x = position, y = count))+








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

