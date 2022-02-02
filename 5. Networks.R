rm(list = ls())
required_libs = c('tidyverse', 'visNetwork', 'igraph', "reshape2", "networkD3")
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)



# Elaboration --------------------------------------------------------------------------------------

generate_network = function(adjacency, nodes, network, prefix, suffix, 
                            n=25, ret=F, reduce=F, popularity=F){
   adj = read_csv(adjacency)
   adj[upper.tri(adj)] = NA
   nodes = read_csv(nodes)
   colnames(nodes) = c('id', 'value', 'prop', 'pop')
   
   edges = melt(adj) %>% as_tibble()
   colnames(edges) = c('from', 'to', 'width')
   edges = edges %>%
      filter(to != from) %>%
      mutate(width = log(width)/2)
   if(reduce) edges = edges %>% mutate(width = width/2)
   edges = edges[!is.na(edges$width) & !is.infinite(edges$width),] %>%
      filter(width > 0)
   
   if(popularity){
      vis.nodes = nodes %>% arrange(desc(pop)) %>% slice_head(n=n)
      vis.nodes$title  = paste0(vis.nodes$pop, " citations")
   }
   else{
      vis.nodes = nodes %>% arrange(desc(value)) %>% slice_head(n=n)
      vis.nodes$title  = paste0(vis.nodes$value, " occurences")
   }
   vis.links = edges[(edges$from %in% nodes$id) & (edges$to %in% nodes$id),] 
   if(reduce){
      vis.links$title = paste0(as.integer(exp(vis.links$width * 4)), " co-occurences")
   }
   else{
      vis.links$title = paste0(as.integer(exp(vis.links$width * 2)), " co-occurences")
   }
   
   vis.nodes$shape  = "dot"  
   vis.nodes$shadow = TRUE 
   vis.nodes$label  = vis.nodes$id 
   vis.nodes$font.color = "black"
   vis.nodes$size   = vis.nodes$prop 
   vis.nodes$borderWidth = 2 
   
   vis.nodes$color.background = "black"
   vis.nodes$color.border = "white"
   vis.nodes$color.highlight.background = "orange"
   vis.nodes$color.highlight.border = "darkred"
   vis.links$color.highlight = "darkred"
   
   net = visNetwork(vis.nodes, vis.links, height="800px", width="1200px",
                    main = paste0(prefix, " Digital Topics Network ", suffix),
                    submain = "Double-click on a node to make a cluster") %>% 
      visPhysics(stabilization = T,   barnesHut = list(
         gravitationalConstant = -1000,
         springConstant = 0.002,
         springLength = 100
      ))  %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
                 manipulation = TRUE, collapse = TRUE) 
   save(net, file=paste0("data/plots/networks/RData/original/", network, "_network.RData"))
   htmlwidgets::saveWidget(net, paste0("data/plots/networks/html/original/", network, "_network.html"),
                           selfcontained = FALSE, libdir = "lib")
   net = net %>%
      visIgraphLayout() %>%
      visEdges(smooth=T) %>%
      visPhysics(stabilization = T,   barnesHut = list(
         gravitationalConstant = -1000,
         springConstant = 0.002,
         springLength = 100
      ))
   save(net, file=paste0("data/plots/networks/RData/smooth/", network, "_network.RData"))
   htmlwidgets::saveWidget(net, paste0("data/plots/networks/html/smooth/", network, "_network.html"),
                           selfcontained = FALSE, libdir = "lib")
   # visSave(net, file = paste0("data/plots/networks/", network, "_network.html"))
   if(ret) return(net)
}

generate_network("data/counts/adj_mat/patents_co_occ.csv", "data/counts/patents_count.csv",
                 "patents", "Industry", "", n=25, ret=T)



# Industry ------------------------------------------------------------------------------------

# overall
generate_network("data/counts/adj_mat/patents_co_occ.csv", "data/counts/patents_count.csv",
                 "patents", "Industry", "", n=25, ret=F)

# per year
for(year in 2000:2019){
   generate_network(paste0("data/counts/adj_mat/patents_co_occ_", year, ".csv"),
                    paste0("data/counts/year/patents_count_year_", year, ".csv"),
                    paste0("patents_", year), "Industry", year, n=25, ret=F)
}


# Science ------------------------------------------------------------------------------------

# overall
generate_network("data/counts/adj_mat/papers_co_occ.csv", "data/counts/papers_count.csv",
                 "papers", "Science", "", n=25, ret=T, reduce=T)
generate_network("data/counts/adj_mat/papers_co_occ.csv", "data/counts/papers_count.csv",
                 "papers_pop_", "Science", "", n=25, ret=T, reduce=T, popularity=T)

# per year
for(year in 2000:2019){
   generate_network(paste0("data/counts/adj_mat/papers_co_occ_", year, ".csv"),
                    paste0("data/counts/year/papers_count_year_", year, ".csv"),
                    paste0("papers_", year), "Science", year, n=25, ret=F, reduce=T)
}
for(year in 2000:2019){
   generate_network(paste0("data/counts/adj_mat/papers_co_occ_", year, ".csv"),
                    paste0("data/counts/year/papers_count_year_", year, ".csv"),
                    paste0("papers_pop_", year), "Science", year, n=25, ret=F, reduce=T, popularity=T)
}




# Overall ---------------------------------------------------------------------------------------------

generate_network2 = function(adjacency, nodes, network, prefix, suffix, n=25, ret=F){
   adj1 = read_csv(adjacency[1])
   adj2 = read_csv(adjacency[2])
   adj1 = adj1[, colnames(adj1)[colnames(adj1) %in% colnames(adj2)]] %>%
      filter(Topic %in% colnames(adj2))
   adj2 = adj2[, colnames(adj2)[colnames(adj2) %in% colnames(adj1)]] %>%
      filter(Topic %in% colnames(adj1))
   adj1 = adj1[, colnames(adj2)]
   adj = adj1$Topic %>% cbind((adj1[, 2:ncol(adj1)] + adj2[, 2:ncol(adj1)])) %>% as_tibble()
   adj[upper.tri(adj)] = NA
   
   nodes1 = read_csv(nodes[1])
   nodes2 = read_csv(nodes[2])
   nodes = inner_join(nodes1, nodes2, by='topic') %>% mutate(v = (proportion.x + proportion.y)/2) %>%
      select(topic, v)
   colnames(nodes) = c('id', 'value')
   
   edges = melt(adj) %>% as_tibble()
   colnames(edges) = c('from', 'to', 'width')
   edges = edges %>%
      filter(to != from) %>%
      mutate(width = log(width)/4)
   edges = edges[!is.na(edges$width) & !is.infinite(edges$width),] %>%
      filter(width > 0)
   
   vis.nodes = nodes %>% arrange(desc(value)) %>% slice_head(n=n)
   vis.links = edges[(edges$from %in% nodes$id) & (edges$to %in% nodes$id),] 
   
   vis.nodes$shape  = "dot"  
   vis.nodes$shadow = TRUE 
   vis.nodes$title  = paste0(round(100*vis.nodes$value, 1), "% average frequency")
   vis.nodes$label  = vis.nodes$id 
   vis.nodes$font.color = "black"
   vis.nodes$size   = vis.nodes$value 
   vis.nodes$borderWidth = 2 
   
   vis.nodes$color.background = "black"
   vis.nodes$color.border = "white"
   vis.nodes$color.highlight.background = "orange"
   vis.nodes$color.highlight.border = "darkred"
   vis.links$color.highlight = "darkred"
   vis.links$title = paste0(as.integer(exp(vis.links$width * 4)), " co-occurences")
   
   net = visNetwork(vis.nodes, vis.links, height="800px", width="1200px",
                    main = paste0(prefix, " Digital Topics Network ", suffix),
                    submain = "Double-click on a node to make a cluster") %>% 
      visPhysics(stabilization = T,   barnesHut = list(
         gravitationalConstant = -1000,
         springConstant = 0.002,
         springLength = 100
      ))  %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
                 manipulation = F, collapse = TRUE) 
   save(net, file=paste0("data/plots/networks/RData/original/", network, "_network.RData"))
   htmlwidgets::saveWidget(net, paste0("data/plots/networks/html/original/", network, "_network.html"),
                           selfcontained = FALSE, libdir = "lib")
   net = net %>%
      visIgraphLayout() %>%
      visEdges(smooth=T) %>%
      visPhysics(stabilization = T,   barnesHut = list(
         gravitationalConstant = -1000,
         springConstant = 0.002,
         springLength = 100
      ))
   save(net, file=paste0("data/plots/networks/RData/smooth/", network, "_network.RData"))
   htmlwidgets::saveWidget(net, paste0("data/plots/networks/html/smooth/", network, "_network.html"),
                           selfcontained = FALSE, libdir = "lib")
   # visSave(net, file = paste0("data/plots/networks/", network, "_network.html"))
   if(ret) return(net)
}

generate_network2(c("data/counts/adj_mat/patents_co_occ.csv", "data/counts/adj_mat/papers_co_occ.csv"),
                  c("data/counts/patents_count.csv", "data/counts/papers_count.csv"),
                    "all", "Industry", "", n=25, ret=T)

for(year in 2000:2019){
   generate_network2(c(paste0("data/counts/adj_mat/patents_co_occ_", year, ".csv"),
                      paste0("data/counts/adj_mat/papers_co_occ_", year, ".csv")),
                    c(paste0("data/counts/year/patents_count_year_", year, ".csv"),
                      paste0("data/counts/year/papers_count_year_", year, ".csv")),
                    paste0("all_", year), "Industry", year, n=25, ret=F)
}







