#' bla 123
#'
#' Compute AUC score as additional metric. If model has several output neurons with binary crossentropy loss, will use the average score.
#'
#' @examples
#' aaa()
#'
#' @returns A keras metric.
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
#' \donttest{
#' library(keras)
#' model <- keras::keras_model_sequential(input_shape = c(3)) %>%
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

#' bla 333
#'
#' zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz.
#'
#' @examplesIf reticulate::py_module_available("tensorflow")
#'
#' \donttest{
#' library(keras)
#' l <- zz()
#' }
#' @returns A keras layer.
#' @export
zz <- function() {

  layer_1 <- keras::new_layer_class(
    "layer_1",

    initialize = function(method, multi_in=FALSE, ...) {
      super$initialize(...)
      self$axis <- ifelse(multi_in, 0L, 1L)
      self$multi_in <- multi_in
    },

    call = function(inputs, mask = NULL) {

      out <- tensorflow::tf$math$reduce_sum(inputs, axis = self$axis)
      out
    },

    get_config = function() {
      config <- super$get_config()
      config$multi_in <- self$multi_in
      config
    }
  )
  layer_1
}

