#' Load dataset from configuration
#'
#' This function reads a catalog configuration from a YAML file and loads the specified
#' dataset based on its configuration. Currently supports SQL queries from Snowflake,
#' as well as CSV and Excel files.
#'
#' @param dataset_name The name of the dataset to load as specified in the catalog
#' @param catalog_path The path to the catalog YAML file. Defaults to "conf/base/catalog.yaml".
#' @param credentials_path The path to the credentials YAML file. Defaults to "conf/credentials/credentials.yaml".
#' @return The loaded dataset as a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- catalog_load("example_data")
#' # df_custom_paths <- catalog_load("example_data",
#' #                                 catalog_path = "conf/custom/catalog.yaml",
#' #                                 credentials_path = "conf/custom/credentials.yaml")
#' }
catalog_load <- function(dataset_name, catalog_path = "conf/base/catalog.yaml", credentials_path = "conf/credentials/credentials.yaml") {

  # Load the catalog configuration
  catalog <- yaml::yaml.load_file(catalog_path)

  # Check if the dataset is in the catalog
  if (!dataset_name %in% names(catalog)) {
    stop(paste("Dataset", dataset_name, "not found in catalog at", catalog_path))
  }

  # Get the dataset configuration
  config <- catalog[[dataset_name]]

  # Load the data based on its type
  if (config$type == "SQLDataSet") {
    # Load credentials
    credentials_full <- yaml::yaml.load_file(credentials_path)
    if (!config$creds_name %in% names(credentials_full)) {
      stop(paste("Credentials entry", config$creds_name, "not found in", credentials_path))
    }
    credentials <- credentials_full[[config$creds_name]]

    # Establish a connection based on the database type
    if (config$db_type == "Snowflake") {
      conn <- DBI::dbConnect(odbc::odbc(),
                             Driver="Snowflake",
                             Server=credentials$Server,
                             Database=credentials$Database,
                             SCHEMA=credentials$Schema,
                             UID=credentials$User,
                             PWD=credentials$Password,
                             WAREHOUSE=credentials$Warehouse)
    } else {
      stop(paste("Unsupported database type:", config$db_type))
    }

    # Load the SQL query
    # Ensure sql_path is treated as relative to the catalog file's directory
    # or make it absolute, or relative to project root.
    # For now, assuming sql_path is accessible as is (e.g. relative to project root)
    if (!file.exists(config$sql_path)) {
        stop(paste("SQL file not found at path:", config$sql_path))
    }
    sql_query_lines <- base::readLines(config$sql_path)
    sql_query <- paste(sql_query_lines, collapse = "\n")


    # Fetch the data
    data <- DBI::dbGetQuery(conn, sql_query)

    # Disconnect from the database
    DBI::dbDisconnect(conn)
  } else if (config$type == "CSVDataset") {
    # Load data from a CSV file
    data <- readr::read_delim(config$path,
                              delim = if(!is.null(config$sep)) config$sep else ",", # Default to comma if not specified
                              quote = if(!is.null(config$quote)) config$quote else "\"",
                              escape_backslash = if(!is.null(config$escape_backslash)) config$escape_backslash else FALSE,
                              escape_double = if(!is.null(config$escape_double)) config$escape_double else TRUE,
                              col_names = if(!is.null(config$col_names)) config$col_names else TRUE,
                              col_types = if(!is.null(config$col_types)) config$col_types else NULL,
                              # col_select = config$col_select, # Keep NULL if not specified
                              locale = readr::default_locale(), # Use default_locale() for safety
                              na = if(!is.null(config$na)) config$na else c("", "NA"),
                              quoted_na = if(!is.null(config$quoted_na)) config$quoted_na else TRUE,
                              comment = if(!is.null(config$comment)) config$comment else "",
                              trim_ws = if(!is.null(config$trim_ws)) config$trim_ws else FALSE, # Default to FALSE for readr
                              skip = if(!is.null(config$skip)) config$skip else 0,
                              n_max = if(!is.null(config$n_max)) config$n_max else Inf,
                              guess_max = if(!is.null(config$guess_max)) config$guess_max else min(1000, if(is.infinite(config$n_max %||% Inf)) 1000 else (config$n_max %||% 1000)),
                              name_repair = if(!is.null(config$name_repair)) config$name_repair else "unique",
                              num_threads = readr::readr_threads(),
                              progress = readr::show_progress(),
                              show_col_types = readr::should_show_types(),
                              skip_empty_rows = if(!is.null(config$skip_empty_rows)) config$skip_empty_rows else TRUE,
                              lazy = if(!is.null(config$lazy)) config$lazy else readr::should_read_lazy())
    if (!is.null(config$col_select)) { # Handle col_select separately due to its evaluation
      data <- data[, config$col_select, drop = FALSE]
    }
  } else if (config$type == "EXCELDataset") {
    # Load data from an Excel file
    data <- readxl::read_excel(config$path, sheet = config$sheet)
  } else {
    stop(paste("Unsupported data type:", config$type))
  }

  return(data)
}

# Helper for default values if config parameters are NULL
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
