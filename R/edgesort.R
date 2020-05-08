#' Edge Sort
#'
#' This function uses the output from the 'estimateNetowrk' package of boonet to create sorted list (descending) of edge weights.
#' @param Name of a network created using bootnet
#' @return A desecending list of edgeweights
#' @examples
#' network <- estimateNetwork(data,default="EPICglasso")
#' edgesort(network)
#' @export


edgesort <- function(networkname){
  x <- (networkname)$graph
  x[upper.tri(x)] <- 0
  rownames(x) <- (networkname)$labels
  colnames(x) <- (networkname)$labels
  y1 <- as.data.frame(x)
  y1 <- cbind(From=rownames(y1),y1)
  ylong <- pivot_longer(y1,cols=2:18,names_to="To", values_to = "weight")
  ylong %>% arrange(desc(weight))
}



