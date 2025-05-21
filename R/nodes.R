#' Create a raptor_node object
#'
#' This function creates a structured object representing a node in a raptorML pipeline.
#' A node consists of a function to be executed, along with declarations of its
#' inputs, outputs, and optional metadata such as name, tags, and parameters.
#'
#' @param func The R function to be executed by this node.
#' @param inputs A character vector or a named list of character vectors specifying
#'   the names of input datasets required by the function. These names should
#'   correspond to entries in the data catalog or outputs of other nodes.
#' @param outputs A character vector or a named list of character vectors specifying
#'   the names of output datasets produced by the function.
#' @param name An optional character string providing a descriptive name for the node.
#'   If NULL, a name might be derived from the function name.
#' @param tags An optional character vector of tags for categorizing or filtering the node.
#' @param parameters An optional named list or character vector specifying parameters
#'   that the node's function might require. These could be actual values or references
#'   to parameter names in a global parameter configuration.
#'
#' @return An S3 object of class `raptor_node` containing the function, its
#'   input/output definitions, and any provided metadata.
#'
#' @export
#' @examples
#' \dontrun{
#' # Define a simple function
#' my_processing_func <- function(raw_data, param1) {
#'   # process data
#'   processed_data <- raw_data * param1
#'   return(processed_data)
#' }
#'
#' # Create a node
#' data_processing_node <- create_node(
#'   func = my_processing_func,
#'   inputs = "raw_data_catalog_key",
#'   outputs = "processed_data_catalog_key",
#'   name = "ProcessRawData",
#'   tags = c("data_processing", "stage1"),
#'   parameters = list(param1 = "my_parameter_key")
#' )
#' }
create_node <- function(func, inputs, outputs, name = NULL, tags = NULL, parameters = NULL) {

  # Validate inputs
  if (!is.function(func)) {
    stop("`func` must be an R function.")
  }
  if (!is.character(inputs) && !is.list(inputs)) {
    stop("`inputs` must be a character vector or a list.")
  }
  if (is.list(inputs) && !all(sapply(inputs, is.character))) {
    stop("If `inputs` is a list, all its elements must be character vectors.")
  }
  if (!is.character(outputs) && !is.list(outputs)) {
    stop("`outputs` must be a character vector or a list.")
  }
  if (is.list(outputs) && !all(sapply(outputs, is.character))) {
    stop("If `outputs` is a list, all its elements must be character vectors.")
  }
  if (!is.null(name) && !is.character(name)) {
    stop("`name` must be a character string or NULL.")
  }
  if (!is.null(tags) && !is.character(tags)) {
    stop("`tags` must be a character vector or NULL.")
  }
  # `parameters` can be flexible (list, vector), so validation might be context-dependent
  # or handled at runtime. For now, basic list check.
  if (!is.null(parameters) && !is.list(parameters) && !is.character(parameters)) {
    stop("`parameters` must be a list, a character vector, or NULL.")
  }

  # Default name if not provided
  if (is.null(name)) {
    name <- deparse(substitute(func))
  }

  node <- list(
    func = func,
    inputs = inputs,
    outputs = outputs,
    name = name,
    tags = if (is.null(tags)) character(0) else tags,
    parameters = if (is.null(parameters)) list() else parameters
  )

  # Assign the class `raptor_node` for S3 dispatch
  class(node) <- "raptor_node"

  return(node)
}
