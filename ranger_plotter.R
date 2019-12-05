library(dplyr)
library(plyr)
library(ggraph)
library(igraph)

getTreeranger=function(RF,ntree) {
  trees=lapply(1:ntree,getTreeranger_k,RF=RF)
  return(trees)
}

getTreeranger_k=function(RF,k=1){
  trees=RF$forest
  split.ids=trees$split.varIDs[[k]]
  split.ids = mapvalues(split.ids, 1:RF$num.independent.variables, trees$independent.variable.names)
  status=rep(1,length(split.ids))
  child.nodes.left=trees$child.nodeIDs[[k]][[1]]+1
  terminal=which(child.nodes.left == 1)
  child.nodes.left[terminal]=0
  child.nodes.right=trees$child.nodeIDs[[k]][[2]]+1
  child.nodes.right[terminal]=0
  status[terminal]=0
  split.values=as.matrix(trees$split.values[[k]])
  pred=as.matrix(trees$terminal.class.counts[[k]])
  pred[sapply(pred, function(x) length(x) == 0)] <- ""
  pred[sapply(pred, function(x) length(x) == 2)] <- sapply(pred[sapply(pred, function(x) length(x) == 2)], "[[", 1)
  id=c(1:length(split.ids))
  ktree=cbind(id,child.nodes.left,child.nodes.right,split.ids,split.values,status,pred)
  
  colnames(ktree)=c("nodeID","leftdaughter","rightdaughter","splitvariable","splitpoint","status","pred")
  return(as.data.frame(ktree))
}

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- getTreeranger_k(final_model, tree_num) %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(splitpoint = ifelse(status != 0, splitpoint, NA)) %>%
    mutate(splitvariable = ifelse(status != 0, splitvariable, ""))
  
  left_frame <- data.frame(tree[1],tree[2])
  colnames(left_frame) <- c("to", "from")
  right_frame <- data.frame(tree[1],tree[3])
  colnames(right_frame) <- c("to", "from")
  graph_frame <- rbind(left_frame, right_frame)
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$splitvariable))
  V(graph)$leaf_label <- as.character(tree$pred)
  V(graph)$split <- as.character(round(as.numeric(tree$splitpoint), digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}