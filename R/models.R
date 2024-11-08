get_layer_names <- function(model) {

  n <- length(model$layers)
  layer_names <- vector("character", n)
  for (i in 1:n) {
    layer_names[i] <- model$layers[[i]]$name
  }
  layer_names
}
