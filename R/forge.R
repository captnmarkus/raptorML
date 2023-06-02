#' Create a new project directory structure
#'
#' This function asks the user for a project name and creates a new directory
#' structure for the project. The created directories include 'conf', 'data',
#' 'docs', 'logs', 'notebooks', and 'src'. Within 'conf', the function creates
#' 'base' and 'credentials' subdirectories. Within 'data', the function creates
#' '00_sql_files', '01_raw', '02_processed', and '03_model_ready' subdirectories.
#' If a directory with the specified project name already exists in the working
#' directory, the function stops and returns an error message. Additionally, it
#' creates three YAML files: 'catalog.yaml', 'parameters.yaml' in the 'conf/base'
#' directory, and 'credentials.yaml' in the 'conf/credentials' directory. It also
#' creates a 'Readme.Rmd' file, an Rproj file named after the project in the project's
#' root directory, and a .gitignore file with standard R ignore patterns.
#'
#' @return A message indicating that the project structure was created successfully.
#' If a directory with the specified project name already exists, the function
#' stops and returns an error message.
#' @export
#'
#' @examples
#' \dontrun{
#' forge()
#' }

forge <- function() {

  # Ask the user for a project name
  project_name <- readline(prompt = "What's your project name? ")

  # Check if directory already exists
  if (dir.exists(project_name)) {
    stop("A directory with this name already exists. Rerun the function and choose a different name")
  }

  # Create directories
  dir.create(project_name)
  dir.create(file.path(project_name, "conf"))
  dir.create(file.path(project_name, "conf", "base"))
  dir.create(file.path(project_name, "conf", "credentials"))
  dir.create(file.path(project_name, "data"))
  dir.create(file.path(project_name, "data", "00_sql_files"))
  dir.create(file.path(project_name, "data", "01_raw"))
  dir.create(file.path(project_name, "data", "02_processed"))
  dir.create(file.path(project_name, "data", "03_model_ready"))
  dir.create(file.path(project_name, "docs"))
  dir.create(file.path(project_name, "logs"))
  dir.create(file.path(project_name, "notebooks"))
  dir.create(file.path(project_name, "src"))
  dir.create(file.path(project_name, "src", "project_functions"))

  # Creates a catalog.yaml template in conf/base directory
  writeLines(c("# Here you can define all your data using YAML syntax",
               "# Please store sql files in the '00_sql_files' folder",
               "# ",
               "# Example for CSV:",
               "# ",
               "# test_data:",
               "#   type: CSVDataset",
               "#   path: 'data/01_raw/test_data.csv'",
               "#   sep: ';'",
               "# ",
               "# Example for EXCELDataSet",
               "#",
               "# test_data:",
               "#   type: EXCELDataSet",
               "#   path: 'data/01_raw/test_data.xlsx'",
               "#   sheet: 'Tabelle1'",
               "# ",
               "# Example for SQLDataSet",
               "#",
               "# test_data:",
               "#   type: SQLDataSet",
               "#   db_type: 'Snowflake'",
               "#   sql_path: 'data/00_sql_files/test_query.sql'",
               "#   creds_name: 'snowflake_creds'"),
             file.path(project_name, "conf", "base", "catalog.yaml"))


  # Creates a sql file template in data/00_sql_files directory
  writeLines(c("--'You can paste any SQL SELECT here as a sql statement, including subqueries",
               "SELECT TOP 1000 *",
               "FROM YOURDATABASE;"
            ), file.path(project_name, "data", "00_sql_files", "template.sql"))


  # Creates a parameters.yaml template in conf/base directory
  writeLines(c("# Here you can define all your parameters using YAML syntax",
               "# Example for base random forest model",
               "# ",
               "# n_estimators: 100",
               "# depth: 10",
               "# col_weight: 20",
               "# seed: 42"), file.path(project_name, "conf", "base", "parameters.yaml"))

  # Creates a credentials.yaml template in conf/credentials directory
  writeLines(c("# Here you can define all your credentials for SQL databases locally using YAML syntax",
               "# Example for snowflake database",
               "# ",
               "# snowflake_creds:",
               "#  Server: ...",
               "#  Database: ...",
               "#  Schema: ...",
               "#  User: ...",
               "#  Password: ...",
               "#  Warehouse ..."), file.path(project_name, "conf", "credentials", "credentials.yaml"))

  # Creates a Readme.Rmd template in root directory

  writeLines("", file.path(project_name, "README.Rmd"))

  # Creates an Rproj file template in root directory
  writeLines("", file.path(project_name, paste0(project_name, ".Rproj")))

  # Creates a .gitignore file in root directory with standard R ignore patterns
  writeLines(c(
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata"
  ), file.path(project_name, ".gitignore"))

  # Creates a main function template file in src directory
  writeLines(c(
    "Define your packages",
    "library(raptorML)"
  ), file.path(project_name,"src", "_main_.R"))

  cat("Project structure created successfully.\n")
}



