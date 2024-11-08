#' aaaaaa
#'
#' aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.
#'
#' @examples
#' aaa()
#'
#' @returns A integer.
#' @export
aaa <- function() {
  a <- 2 + 2
  reticulate::is_py_object(3)
  return(a)
}


#' Mean AUC score
#'
#' Compute AUC score as additional metric. If model has several output neurons with binary crossentropy loss, will use the average score.
#'
#' @export
#' @param model_output_size Number of neurons in model output layer.
#' @param loss Loss function of model, for which metric will be applied to; must be `"binary_crossentropy"`
#' or `"categorical_crossentropy"`.
#' @examplesIf reticulate::py_module_available("tensorflow")
#'
#' input_shape <- 3
#' \donttest{
#' library(keras)
#' model <- keras::keras_model_sequential(input_shape = input_shape) %>%
#'   keras::layer_dense(1, activation = "sigmoid")
#'
#' model %>% keras::compile(
#'   optimizer = keras::optimizer_adam(0.0001),
#'   loss = "binary_crossentropy",
#'   metrics = "acc"
#' )
#'
#' y_true <- c(1,0,0,1,1,0,1,0,0) %>% matrix(ncol = 3)
#' y_pred <- c(0.9,0.05,0.05,0.9,0.05,0.05,0.9,0.05,0.05) %>% matrix(ncol = 3)
#'
#' auc_metric <- auc_wrapper(3L, "binary_crossentropy")
#'
#' auc_metric$update_state(y_true, y_pred)
#' auc_metric$result()
#' }
#'
#' @returns A keras metric.
auc_wrapper <- function(model_output_size = 3L,
                        loss = "binary_crossentropy") {

  model <- keras::keras_model_sequential(input_shape = c(3)) %>%
    keras::layer_dense(1, activation = "sigmoid")

  model %>% keras::compile(
    optimizer = keras::optimizer_adam(0.0001),
    loss = "binary_crossentropy",
    metrics = "acc"
  )

  auc_metric <- tensorflow::tf$keras$metrics$AUC(label_weights = NULL,
                                                 multi_label = FALSE)

  return(auc_metric)
}

#' Custom model
#'
#' Custom model Custom model Custom model Custom model.
#'
#' @examplesIf reticulate::py_module_available("tensorflow")
#'
#' 2+2
#' \donttest{
#' library(keras)
#' model <- custom_model()
#' }
#' @returns A keras model.
#' @export
custom_model <- function() {

  custom_layer <- layer <- zz()
  model <- keras::keras_model_sequential(input_shape = c(3)) %>%
    custom_layer %>%
    keras::layer_dense(1, activation = "sigmoid")
  return(model)
}

#' zzzz
#'
#' zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz.
#'
#' @examplesIf reticulate::py_module_available("tensorflow")
#'
#' 2+2
#' \donttest{
#' library(keras)
#' l <- zz()
#' }
#' @returns A keras layer.
#' @export
zz <- function() {

  super <- NULL
  self <- NULL

  layer_1 <- keras::new_layer_class(
    "layer_1",

    initialize = function(method, multi_in=FALSE, ...) {
      super$initialize(...)
      self$axis <- 0L
      self$multi_in <- multi_in
    },

    call = function(inputs, mask = NULL) {

      a <- tensorflow::tf$math$reduce_sum(c(1,2,3), axis = self$axis)
      out <- inputs + a
      out
    },

    get_config = function() {
      config <- super$get_config()
      config$multi_in <- self$multi_in
      config
    }
  )

  return(layer_1())


}


