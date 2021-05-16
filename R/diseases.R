

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
                      #group = df$group,
                      label = df$id,
                      value = df$value,
                      shape = df$shape,
                      title = df$title_n,
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
                      arrows = df$arrows,
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

getDiseases <- function(data){

}


#library(htmlwidgets)
#library("savePlotAsPng")
#install.packages("savePlotAsPng")
#savePlotAsPng(v, file = "mockdata/v.png", width = 600, height = 480)

#saveWidget(v, file = "mockdata/v.html")

