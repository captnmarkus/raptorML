#' Load dataset from configuration
#'
#' This function reads a catalog configuration from a YAML file and loads the specified
#' dataset based on its configuration. Currently supports SQL queries from Snowflake,
#' as well as CSV and Excel files.
#'
#' @param dataset_name The name of the dataset to load as specified in the catalog
#' @return The loaded dataset as a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- catalog_load("example_data")
#' }

catalog_load <- function(dataset_name) {

  # Load the catalog configuration
  catalog <- yaml::yaml.load_file("conf/base/catalog.yaml")

  # Check if the dataset is in the catalog
  if (!dataset_name %in% names(catalog)) {
    stop(paste("Dataset", dataset_name, "not found in catalog"))
  }

  # Get the dataset configuration
  config <- catalog[[dataset_name]]

  # Load the data based on its type
  if (config$type == "SQLDataSet") {
    # Load credentials
    credentials <- yaml::yaml.load_file("conf/credentials/credentials.yaml")[[config$creds_name]]

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
    sql_query <- base::readLines(config$sql_path)

    # Fetch the data
    data <- DBI::dbGetQuery(conn, sql_query)

    # Disconnect from the database
    DBI::dbDisconnect(conn)
  } else if (config$type == "CSVDataset") {
    # Load data from a CSV file
    data <- readr::read_delim(config$path,
                              delim = config$sep,
                              quote = config$quote,
                              escape_backslash = config$escape_backslash,
                              escape_double = config$escape_double,
                              col_names = config$col_names,
                              col_types = config$col_types,
                              col_select = config$col_select,
                              id = NULL,
                              locale = default_locale(),
                              na = c("", "NA"),
                              quoted_na = TRUE,
                              comment = "",
                              trim_ws = FALSE,
                              skip = 0,
                              n_max = Inf,
                              guess_max = min(1000, n_max),
                              name_repair = "unique",
                              num_threads = readr_threads(),
                              progress = show_progress(),
                              show_col_types = should_show_types(),
                              skip_empty_rows = TRUE,
                              lazy = should_read_lazy())
  } else if (config$type == "EXCELDataset") {
    # Load data from an Excel file
    data <- readxl::read_excel(config$path, sheet = config$sheet)
  } else {
    stop(paste("Unsupported data type:", config$type))
  }

  return(data)
}

