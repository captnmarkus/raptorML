#' Run a raptor_pipeline
#'
#' Executes the nodes in a raptor_pipeline object in sequence.
#' It handles loading inputs for each node using the catalog, passing outputs
#' from one node to the next in memory, and making parameters available to nodes.
#' It also supports filtering the nodes to be executed.
#'
#' @param pipeline A \code{raptor_pipeline} object.
#' @param parameters A list of parameters, typically loaded by \code{load_parameters()}.
#'   Defaults to an empty list if not provided.
#' @param catalog_path The path to the main catalog YAML file. Defaults to
#'   "conf/base/catalog.yaml".
#' @param credentials_path The path to the credentials YAML file. Defaults to
#'   "conf/credentials/credentials.yaml".
#' @param initial_catalog An optional named list representing an already loaded catalog.
#'   If provided, `catalog_path` is ignored for initial loading, but still passed to
#'   `catalog_load` if a node explicitly needs it.
#' @param from_nodes An optional character vector of node names. If provided,
#'   the pipeline execution will start from the first node listed in this vector.
#'   Subsequent nodes will be executed in their original order. Ignored if `node_names` is provided.
#' @param to_nodes An optional character vector of node names. If provided,
#'   the pipeline execution will stop after the last node listed in this vector has run.
#'   Ignored if `node_names` is provided.
#' @param node_names An optional character vector of specific node names to run.
#'   Only these nodes (and their dependencies, if dependency resolution is implemented)
#'   will be run. For now, assume only the listed nodes are run in their pipeline order.
#'   If `from_nodes` or `to_nodes` are also provided, `node_names` takes precedence for selecting the initial set, then `from/to` might further constrain. (For simplicity in this iteration, if `node_names` is provided, `from_nodes` and `to_nodes` could be ignored or issue a warning).
#' @param tags An optional character vector of tags. Only nodes that have AT LEAST ONE
#'   of these tags will be run.
#' @return A list containing the outputs of the executed nodes,
#'   or an environment containing all intermediate and terminal outputs.
#' @export
#' @examples
#' \dontrun{
#' # Assuming pipe is a raptor_pipeline object and params are loaded
#' # results <- run_pipeline(pipe, params)
#' # results_tagged <- run_pipeline(pipe, params, tags = c("data_prep"))
#' # results_subset <- run_pipeline(pipe, params, node_names = c("node_A", "node_C"))
#' # results_range <- run_pipeline(pipe, params, from_nodes = "node_B", to_nodes = "node_D")
#' }
run_pipeline <- function(pipeline,
                         parameters = list(),
                         catalog_path = "conf/base/catalog.yaml",
                         credentials_path = "conf/credentials/credentials.yaml",
                         initial_catalog = NULL,
                         from_nodes = NULL,
                         to_nodes = NULL,
                         node_names = NULL,
                         tags = NULL) {

  if (!inherits(pipeline, "raptor_pipeline")) {
    stop("`pipeline` argument must be a raptor_pipeline object.")
  }
  if (!is.list(parameters)) {
    stop("`parameters` argument must be a list.")
  }

  data_env <- new.env(parent = emptyenv())
  
  # Start with all nodes from the pipeline definition
  nodes_to_run <- pipeline$nodes 

  # --- Filtering Logic ---

  # 1. Tag filtering
  if (!is.null(tags) && length(tags) > 0) {
    nodes_to_run <- Filter(function(node) {
      !is.null(node$tags) && length(node$tags) > 0 && any(sapply(tags, function(t) t %in% node$tags))
    }, nodes_to_run)
  }

  # 2. Node name filtering (takes precedence over from/to as per simplified instruction)
  if (!is.null(node_names) && length(node_names) > 0) {
    if (!is.null(from_nodes) || !is.null(to_nodes)) {
      warning("`node_names` is provided; `from_nodes` and `to_nodes` will be ignored.")
    }
    # Filter the already (potentially) tag-filtered list
    nodes_to_run <- Filter(function(node) node$name %in% node_names, nodes_to_run)
    
    # Ensure the order of nodes_to_run is based on their original appearance in pipeline$nodes
    if (length(nodes_to_run) > 0) { 
        original_pipeline_node_names <- sapply(pipeline$nodes, `[[`, "name") # Use original pipeline for full order context
        current_node_names_in_filtered_list <- sapply(nodes_to_run, `[[`, "name")
        
        # Get names that are in both, in the order they appear in the *original* pipeline
        ordered_names_to_keep <- intersect(original_pipeline_node_names, current_node_names_in_filtered_list)
        
        # Re-index to get the actual node objects from the current 'nodes_to_run' list in the correct order
        nodes_to_run <- nodes_to_run[match(ordered_names_to_keep, current_node_names_in_filtered_list)]
        nodes_to_run <- nodes_to_run[!is.na(sapply(nodes_to_run, function(n) !is.null(n)))] # Clean NAs if any
    }

  } else {
    # 3. From/To node filtering (only if node_names was NOT provided)
    # Apply to the (potentially) tag-filtered list (nodes_to_run at this point)
    current_node_names_in_list <- sapply(nodes_to_run, `[[`, "name") # Names of nodes after tag filtering
    
    start_index <- 1
    if (!is.null(from_nodes) && length(from_nodes) > 0) {
      # Find first occurrence of any node name in from_nodes within the current list
      from_indices <- which(current_node_names_in_list %in% from_nodes)
      if (length(from_indices) > 0) {
        start_index <- min(from_indices)
      } else {
        # If no 'from_nodes' are found in the current list, effectively run nothing.
        start_index <- length(nodes_to_run) + 1 
      }
    }

    end_index <- length(nodes_to_run)
    if (!is.null(to_nodes) && length(to_nodes) > 0) {
      # Find last occurrence of any node name in to_nodes within the current list
      to_indices <- which(current_node_names_in_list %in% to_nodes)
      if (length(to_indices) > 0) {
        end_index <- max(to_indices)
      } else {
        # If no 'to_nodes' are found in the current list, effectively run nothing.
        end_index <- 0
      }
    }
    
    if (start_index <= end_index && start_index > 0 && end_index <= length(nodes_to_run) && length(nodes_to_run) > 0) {
      nodes_to_run <- nodes_to_run[start_index:end_index]
    } else {
      nodes_to_run <- list() 
      # Only warn if filtering was actually attempted and resulted in empty
      # or if the initial list of nodes_to_run (after tag filtering) was not empty.
      # And if from_nodes or to_nodes were actually specified.
      if (length(pipeline$nodes) > 0 && (!is.null(from_nodes) || !is.null(to_nodes)) && 
          (length(current_node_names_in_list) > 0 || (start_index > end_index && !(start_index == (length(current_node_names_in_list) + 1) && end_index == 0 && length(from_nodes)>0 && length(to_nodes)>0) ) ) ) { 
        # Avoid warning if both from/to are specified but neither found in an already empty list
        warning("Invalid from_nodes/to_nodes sequence or names not found in the (potentially tag-filtered) set. No nodes will be run based on this filter.")
      }
    }
  }
  
  if (length(nodes_to_run) == 0) {
    cat("No nodes selected to run after filtering.\n")
    return(as.list(data_env))
  }
  
  cat(sprintf("Effective list of nodes to run after filtering (%d nodes):\n", length(nodes_to_run)))
  sapply(nodes_to_run, function(node_obj) cat(sprintf("  - %s\n", node_obj$name)))

  # --- End of Filtering Logic --- (Rest of the function remains the same as previous correct version)
  
  for (node_obj in nodes_to_run) {
    cat(sprintf("Running node: %s\n", node_obj$name))

    node_inputs <- list()
    resolved_node_params <- list()

    if (length(node_obj$parameters) > 0) {
      for (p_name in names(node_obj$parameters)) {
        p_value <- node_obj$parameters[[p_name]]
        if (is.character(p_value) && p_value %in% names(parameters)) {
          resolved_node_params[[p_name]] <- parameters[[p_value]]
        } else {
          resolved_node_params[[p_name]] <- p_value
        }
      }
    }
    
    if (length(node_obj$inputs) > 0) {
      input_sources <- node_obj$inputs
      if (is.character(input_sources) && is.null(names(input_sources))) {
        names(input_sources) <- input_sources
      } else if (is.list(input_sources) && any(sapply(names(input_sources), function(x) x == ""))) {
        unnamed_idx <- which(names(input_sources) == "")
        if(all(sapply(input_sources[unnamed_idx], is.character))) {
           names(input_sources)[unnamed_idx] <- unlist(input_sources[unnamed_idx])
        }
      }

      for (input_arg_name in names(input_sources)) {
        input_data_key <- input_sources[[input_arg_name]]
        
        if (exists(input_data_key, envir = data_env, inherits = FALSE)) {
          cat(sprintf("  Loading input '%s' ('%s') from memory\n", input_arg_name, input_data_key))
          node_inputs[[input_arg_name]] <- get(input_data_key, envir = data_env, inherits = FALSE)
        } else {
          cat(sprintf("  Loading input '%s' ('%s') from catalog\n", input_arg_name, input_data_key))
          tryCatch({
            node_inputs[[input_arg_name]] <- catalog_load(
              dataset_name = input_data_key,
              catalog_path = catalog_path,
              credentials_path = credentials_path
            )
            assign(input_data_key, node_inputs[[input_arg_name]], envir = data_env)
          }, error = function(e) {
            stop(sprintf("Failed to load input '%s' for node '%s': %s", input_data_key, node_obj$name, e$message))
          })
        }
      }
    }
    
    call_args <- c(node_inputs, resolved_node_params)
    cat(sprintf("  Executing function for node '%s'\n", node_obj$name))
    
    func_params <- formals(node_obj$func)
    final_call_args <- list()
    func_arg_names <- names(func_params)
    
    for (arg_name in func_arg_names) {
      if (arg_name == "...") { 
        next
      }
      if (arg_name %in% names(call_args)) {
        final_call_args[[arg_name]] <- call_args[[arg_name]]
      } else {
        # Check if the argument has a default value
        if (identical(func_params[[arg_name]], quote(expr=)) || 
            (is.symbol(func_params[[arg_name]]) && deparse(func_params[[arg_name]]) == "")) {
           # This means the argument has no default value
           stop(sprintf("Node '%s': Missing required argument '%s' for function '%s'. Provided args: %s", 
                         node_obj$name, arg_name, deparse(substitute(node_obj$func)),
                         paste(names(call_args), collapse=", ")))
        }
        # If it has a default, R's do.call will handle it, so no need to add to final_call_args here
      }
    }
    
    # Handle '...' arguments: pass any remaining arguments from call_args
    if ("..." %in% func_arg_names) {
      additional_args <- call_args[!names(call_args) %in% func_arg_names] # Get args not already matched
      final_call_args <- c(final_call_args, additional_args)
    } else {
      # Warn about unused arguments if function does not have '...'
      unused_args <- call_args[!names(call_args) %in% func_arg_names]
      if (length(unused_args) > 0) {
          cat(sprintf("  Warning for node '%s': Unused arguments provided and function has no '...': %s\n", 
                      node_obj$name, paste(names(unused_args), collapse=", ")))
      }
    }
    
    node_results <- do.call(node_obj$func, final_call_args)

    if (!is.null(node_results)) {
        output_def <- node_obj$outputs
        if (length(output_def) == 1 && (is.null(names(output_def)) || names(output_def) == "" || names(output_def)[1] == "")) {
            output_name_key <- as.character(unlist(output_def)[1])
            assign(output_name_key, node_results, envir = data_env)
            cat(sprintf("  Stored output as '%s' in memory\n", output_name_key))
        } else if (is.list(node_results) && !is.data.frame(node_results) && length(output_def) > 0) {
            if (is.null(names(output_def))) {
                 warning(sprintf("Node '%s': 'outputs' is a list/vector but not named. Cannot map function results to data environment keys reliably for multiple outputs.", node_obj$name))
            } else {
                for (func_return_name in names(output_def)) {
                    output_data_key <- output_def[[func_return_name]]
                    if (func_return_name %in% names(node_results)) {
                        assign(output_data_key, node_results[[func_return_name]], envir = data_env)
                        cat(sprintf("  Stored output '%s' (from function's '%s') in memory\n", output_data_key, func_return_name))
                    } else {
                        warning(sprintf("Node '%s' was expected to return list element '%s' (for output key '%s'), but it was not found in the results.",
                                        node_obj$name, func_return_name, output_data_key))
                    }
                }
            }
        } else if (length(output_def) > 0) { 
            output_name_key <- if (!is.null(names(output_def)) && names(output_def)[1] != "") {
              output_def[[1]] 
            } else {
              as.character(unlist(output_def)[1]) 
            }
            assign(output_name_key, node_results, envir = data_env)
            cat(sprintf("  Stored single output as '%s' in memory\n", output_name_key))
        }
    } else {
        cat("  Node function returned NULL or no outputs declared.\n")
    }
    cat(sprintf("Finished node: %s\n", node_obj$name))
  }

  cat("Pipeline execution completed.\n")
  return(as.list(data_env))
}

# Helper for default values if config parameters are NULL
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
