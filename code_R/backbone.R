get.backbone = function(graph, alpha = 0.05, directed = FALSE)
{
  G = graph
  
  # get edgelist
  edgelist = get.data.frame(G)
  colnames(edgelist) = c("from","to","weight")
  
  # get nodes list
  nodes = unique(c(edgelist[,1], edgelist[,2]))
  N = length(nodes)
  
  # initialize backbone dataframe
  backbone = NULL
  
  cat("Disparity Filter\n")
  cat("alpha =", alpha, "\n")
  cat("\nOriginal graph\n")
  print(G)
  
  for (i in 1:N) # for each node
  {
    # get neighbors
    nei = edgelist[edgelist$from == nodes[i],]
    nei = rbind(nei, edgelist[edgelist$to == nodes[i],])
    
    # get degree for node i
    k_i = length(edgelist$to[edgelist$to == nodes[i]]) + length(edgelist$to[edgelist$from == nodes[i]])
    
    if (k_i>1)
    {
      for (j in 1:k_i) # for each neighbor
      {
        # compute weighted edge
        p_ij = as.numeric(nei$weight[j]) / sum(as.numeric(nei$weight))
        
        # VIA INTEGRATION
        #integrand_i = function(x){(1-x)^(k_i-2)}
        #integration = integrate(integrand_i, lower = 0, upper = p_ij)
        #alpha_ij = 1 - (k_i - 1) * integration$value
        
        alpha_ij = (1 - p_ij)^(k_i - 1)
        
        if (alpha_ij < alpha)
        {
          backbone = rbind(backbone, c(nei$from[j], nei$to[j], nei$weight[j]))
        }
      }
    }
  }
  
  colnames(backbone) = c("from","to","weight")
  backbone = unique(backbone[,c('from','to','weight')])
  G_backbone = graph.data.frame(backbone, directed = directed)
  
  cat("\nBackbone graph\n")
  print(G_backbone)
  return(G_backbone)
}