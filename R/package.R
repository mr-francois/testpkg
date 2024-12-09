#' pkg bla
#'
#' pkg is an open source software library for building deep neural
#' networks for genomic modeling.
#'
#'
"_PACKAGE"

# globals
.globals <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  #requireNamespace('keras')

  # Minimal HDF5 operation to prevent errors from hdf5r package
  temp_file <- tempfile(fileext = ".h5")
  h5_file <- hdf5r::H5File$new(temp_file, mode = "w")
  h5_file$close_all()
  file.remove(temp_file)

  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 3)

  return(NULL)

}

# # Define a package-specific environment
# deepG_env <- new.env(parent = emptyenv())
#
# check_tensorflow <- function() {
#   # Check if TensorFlow availability has already been stored in the package environment
#   if (!exists(".tensorflow_checked", envir = deepG_env)) {
#     deepG_env$.tensorflow_checked <- reticulate::py_module_available("tensorflow")
#   }
#   return(deepG_env$.tensorflow_checked)
# }

.onAttach <- function(libname, pkgname) {

  #requireNamespace('tensorflow')
  #tf_available <- reticulate::py_module_available("tensorflow")

}

