#' Edge Sort
#'
#' This function uses the output from the 'estimateNetowrk' package of boonet to create sorted list (descending) of edge weights.
#' @param networkname name of a network created using bootnet
#' @return A desecending list of edgeweights
#' @examples
#' library(psychTools)
#' library(bootnet)
#' data(bfi)
#' network <- estimateNetwork(bfi, default="EBICglasso", threshold = TRUE)
#' edgesort(network)
#' 
#' @importFrom dplyr %>%
#' @export

edgesort <- function(networkname) {
  # All variables set to NULL to avoid the 'no visible binding' note 
  weight <- NULL
  # Check if the object is generated via bootnet::estimateNetwork
  if(methods::is(networkname) != "bootnetResult"){
    stop("networkname object is not of type bootnetResult. Function terminated.")
  }
  
  x <- (networkname)$graph
  x[upper.tri(x)] <- 0
  rownames(x) <- (networkname)$labels
  colnames(x) <- (networkname)$labels
  y1 <- as.data.frame(x)
  y1 <- cbind(From = rownames(y1), y1)
  ylong <-
    tidyr::pivot_longer(y1,
                        cols = 2:18,
                        names_to = "To",
                        values_to = "weight")
  res <- ylong %>% dplyr::arrange(dplyr::desc(weight))
  return(res)
}



