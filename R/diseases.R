




makeNodes <- function(df){
  nodes <- data.frame(id = df$id,
                      group = df$group,
                      value = df$value,
                      shape = df$shape,
                      title = df$title_n,
                      color = df$color,
                      shadow = FALSE)
  return(nodes)
}

makeEdges <- function(df){
  edges <- data.frame(from = df$from,
                      to = df$to,
                      length = df$length,
                      arrows = "middle",
                      dashes = FALSE,
                      title = df$title_e,
                      smooth = FALSE,
                      shadow = FALSE)
  return(edges)
}


getVisNetwork <- function(data, save = FALSE, path, name){
  library(visNetwork)

  nodes <- makeNodes(data)
  edges <- makeEdges(data)
  v <-visNetwork(nodes, edges, width = "100%")
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

