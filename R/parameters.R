#' Load parameters from a YAML file
#'
#' This function reads parameters from a specified YAML file. These parameters
#' can then be used to configure nodes and pipelines.
#'
#' @param path A character string specifying the path to the parameters YAML file.
#'   Defaults to "conf/base/parameters.yaml".
#' @return A list containing the parameters loaded from the YAML file. If the
#'   file is not found or cannot be parsed, the function will stop with an error.
#' @export
#' @examples
#' \dontrun{
#' # params <- load_parameters()
#' # custom_params <- load_parameters("conf/custom/my_params.yaml")
#'
#' # To use in a node, you might pass parameter names:
#' # create_node(
#' #   func = my_func,
#' #   inputs = "input_data",
#' #   outputs = "output_data",
#' #   parameters = list(value = "params_key_from_yaml")
#' # )
#' # And then the pipeline runner would resolve "params_key_from_yaml"
#' # against the loaded parameters.
#' }
load_parameters <- function(path = "conf/base/parameters.yaml") {
  if (!file.exists(path)) {
    stop(paste("Parameters file not found at path:", path))
  }

  # Ensure the 'yaml' package is available
  # Depending on how dependencies are managed, this might be assumed or explicitly checked.
  # For now, assume it's listed in DESCRIPTION and available.
  # if (!requireNamespace("yaml", quietly = TRUE)) {
  #   stop("The 'yaml' package is required to load parameters. Please install it.")
  # }

  tryCatch({
    params <- yaml::read_yaml(path)
    # Older versions used yaml.load_file, read_yaml is generally preferred now.
    # params <- yaml::yaml.load_file(path) 
    return(params)
  }, error = function(e) {
    stop(paste("Error loading or parsing YAML file at", path, ":", e$message))
  })
}
