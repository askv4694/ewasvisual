




makeNodes <- function(vals, df){
  nodes <- data.frame(id = df$id,
                      group = df$group,
                      value = df$value,
                      shape = df$shape,
                      title = df$title_n,
                      color = df$color,
                      shadow = FALSE)
  return(nodes)
}

makeEdges <- function(vals,df){
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



library(visNetwork)

v <-visNetwork(nodes, edges, width = "100%")
v
library(htmlwidgets)
#library("savePlotAsPng")
#install.packages("savePlotAsPng")
#savePlotAsPng(v, file = "mockdata/v.png", width = 600, height = 480)

#saveWidget(v, file = "mockdata/v.html")
visSave(v, file.path(paste0(getwd(),"/mockdata/network.html")),
        selfcontained = TRUE, background = "white")
