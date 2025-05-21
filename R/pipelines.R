#' Create a raptor_pipeline object
#'
#' This function constructs a pipeline from a list of raptor_node objects.
#' It performs initial validation on the nodes provided.
#'
#' @param ... A list of \code{raptor_node} objects, or individual \code{raptor_node}
#'   objects provided as separate arguments. If pipelines are passed, they will be
#'   expanded into their constituent nodes.
#' @param name An optional character string for the name of the pipeline.
#'
#' @return An S3 object of class \code{raptor_pipeline}. This object is a list
#'   containing the nodes in the pipeline and potentially other metadata.
#' @export
#' @examples
#' \dontrun{
#' # Assuming node1, node2 are raptor_node objects
#' # pipe <- create_pipeline(node1, node2, name = "my_simple_pipeline")
#' # pipe_from_list <- create_pipeline(list(node1, node2), name = "my_list_pipeline")
#' }
create_pipeline <- function(..., name = NULL) {
  
  # Capture all arguments passed as ...
  elements <- list(...)

  # If the first element is a list, and it's the only element passed in ...,
  # it's likely the user passed a list of nodes/pipelines directly.
  if (length(elements) == 1 && is.list(elements[[1]]) && !inherits(elements[[1]], "raptor_node") && !inherits(elements[[1]], "raptor_pipeline")) {
    elements <- elements[[1]]
  }

  nodes <- list()
  # Flatten any pipelines passed in ... and collect all nodes
  for (el in elements) {
    if (inherits(el, "raptor_node")) {
      nodes <- c(nodes, list(el))
    } else if (inherits(el, "raptor_pipeline")) {
      # Append nodes from the nested pipeline
      # Assuming pipeline object stores its nodes in an element named 'nodes'
      nodes <- c(nodes, el$nodes) 
    } else {
      stop("All elements passed to create_pipeline must be raptor_node or raptor_pipeline objects.")
    }
  }

  # Basic validation: Check if all are raptor_node objects after expansion
  if (!all(sapply(nodes, inherits, "raptor_node"))) {
    stop("All components of a pipeline must ultimately be raptor_node objects.")
  }

  # Check for unique node names (if names are present and not NULL)
  node_names <- sapply(nodes, function(n) n$name)
  # Filter out NULL or empty names before checking for duplicates
  valid_node_names <- node_names[!sapply(node_names, is.null) & node_names != ""]
  if (any(duplicated(valid_node_names))) {
    warning(paste("Pipeline contains duplicate node names:", 
                  paste(unique(valid_node_names[duplicated(valid_node_names)]), collapse=", ")))
  }
  
  # Placeholder for more advanced validation (dependency graph, circular checks)
  # For example, one might build an adjacency list and check for cycles.
  # Also, check if inputs of nodes are met by outputs of preceding nodes or catalog.
  # This will be more involved and developed in a later stage.
  # For now, we just store the nodes.

  pipeline <- list(
    nodes = nodes,
    name = name
    # Add a 'validated' flag or similar if more complex checks are done
    # validated_topology = FALSE 
  )

  class(pipeline) <- "raptor_pipeline"
  return(pipeline)
}
