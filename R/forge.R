#' Create a new project directory structure
#'
#' This function asks the user for a project name and creates a new directory
#' structure for the project, inspired by Kedro. Key directories include 'conf', 'data',
#' 'src', 'logs', 'notebooks', and 'docs'. It pre-populates 'conf/base' with
#' 'catalog.yaml' and 'parameters.yaml', 'conf/credentials' with 'credentials.yaml',
#' and 'src' with '_main_.R', 'nodes.R', and 'pipelines.R' templates to demonstrate
#' a runnable example pipeline.
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
  # Removed: dir.create(file.path(project_name, "src", "project_functions"))

  # --- Create src/nodes.R ---
  nodes_r_content <- c(
    "# src/nodes.R",
    "",
    "#' Example Node Function: Generate Data",
    "#' This function simulates generating some initial data (e.g. the Iris dataset).",
    "#' @param generation_param A parameter (e.g., number of rows, not used here).",
    "#' @return A data frame (e.g. Iris dataset).",
    "generate_data_node_func <- function(generation_param) {",
    "  message(paste(\"Running: generate_data_node_func with param:\", generation_param))",
    "  # Using Iris dataset as an example",
    "  return(as.data.frame(iris))",
    "}",
    "",
    "#' Example Node Function: Process Data",
    "#'",
    "#' This function simulates a data processing step.",
    "#' @param input_df The input data frame.",
    "#' @param example_param A parameter to modify processing.",
    "#' @return A processed data frame.",
    "process_data_node_func <- function(input_df, example_param) {",
    "  message(paste(\"Running: process_data_node_func with param:\", example_param))",
    "  # Example processing: add a column based on the parameter",
    "  processed_df <- input_df",
    "  processed_df$new_column <- example_param",
    "  return(processed_df)",
    "}",
    "",
    "#' Example Node Function: Save Data (Placeholder)",
    "#'",
    "#' This function simulates saving the processed data.",
    "#' @param data_to_save The data frame to \"save\".",
    "#' @param output_location_param A parameter indicating where to save (unused in this example).",
    "#' @return The data frame that was passed in.",
    "save_data_node_func <- function(data_to_save, output_location_param) {",
    "  message(paste(\"Running: save_data_node_func. Output Location Param (unused):\", output_location_param))",
    "  # In a real scenario, this function would write to a file:",
    "  # write.csv(data_to_save, file = output_location_param, row.names = FALSE)",
    "  # For now, we just return it for the runner to place in the data environment.",
    "  return(data_to_save)",
    "}"
  )
  writeLines(nodes_r_content, file.path(project_name, "src", "nodes.R"))

  # --- Create src/pipelines.R ---
  pipelines_r_content <- c(
    "# src/pipelines.R",
    "# Defines the nodes and pipelines for the project.",
    "",
    "# Source the node functions",
    "source(\"nodes.R\") # Assumes nodes.R is in the same directory",
    "",
    "# Define nodes using functions from nodes.R",
    "node_generate_data <- create_node(",
    "  func = generate_data_node_func,",
    "  inputs = list(), # No dataset inputs from catalog for this example",
    "  outputs = \"raw_iris_data_in_memory\", # This key will be used by the next node",
    "  name = \"generate_initial_data\",",
    "  parameters = list(generation_param = \"params.generation.example_setting\"),",
    "  tags = c(\"data_generation\")",
    ")",
    "",
    "node_process_data <- create_node(",
    "  func = process_data_node_func,",
    "  inputs = list(input_df = \"raw_iris_data_in_memory\"), # Matches output of previous node",
    "  outputs = \"intermediate_data_2\",",
    "  name = \"process_data\",",
    "  parameters = list(example_param = \"params.processing.example_value\"),",
    "  tags = c(\"data_processing\")",
    ")",
    "",
    "node_save_data <- create_node(",
    "  func = save_data_node_func,",
    "  inputs = list(data_to_save = \"intermediate_data_2\"),",
    "  outputs = \"final_output_data\", # This will be stored in the runner's data_env",
    "  name = \"save_processed_data\",",
    "  parameters = list(output_location_param = \"params.saving.output_path\"),",
    "  tags = c(\"data_saving\")",
    ")",
    "",
    "# Define the main pipeline by assembling nodes",
    "primary_pipeline <- create_pipeline(",
    "  node_generate_data,",
    "  node_process_data,",
    "  node_save_data,",
    "  name = \"PrimaryDataPipeline\"",
    ")",
    "",
    "# You can define other pipelines as well",
    "# secondary_pipeline <- create_pipeline(...)"
  )
  writeLines(pipelines_r_content, file.path(project_name, "src", "pipelines.R"))

  # --- Update src/_main_.R ---
  main_r_content <- c(
    "# src/_main_.R: Main execution script for the project",
    "",
    "# Load required libraries (raptorML should be sufficient if functions are exported)",
    "library(raptorML)",
    "",
    "message(\"Starting project execution...\")",
    "",
    "# 1. Load parameters",
    "message(\"Loading parameters...\")",
    "params <- load_parameters() # Assumes conf/base/parameters.yaml",
    "# print(params) # For debugging",
    "",
    "# 2. Source pipelines and access the desired pipeline",
    "message(\"Loading pipeline definitions...\")",
    "source(\"pipelines.R\") # Loads node_ definitions and primary_pipeline",
    "",
    "# Select the pipeline to run (e.g., the one named 'primary_pipeline' in pipelines.R)",
    "pipeline_to_run <- primary_pipeline ",
    "# print(pipeline_to_run) # For debugging",
    "",
    "# 3. Run the pipeline",
    "message(paste(\"Running pipeline:\", pipeline_to_run$name, \"...\"))",
    "pipeline_results <- run_pipeline(",
    "  pipeline = pipeline_to_run,",
    "  parameters = params",
    "  # catalog_path and credentials_path will use defaults from run_pipeline",
    ")",
    "",
    "message(\"Pipeline execution finished.\")",
    "message(\"Final outputs from the pipeline run are now in 'pipeline_results'.\")",
    "# print(names(pipeline_results)) # To see what data is available",
    "",
    "# Example: Access a specific output",
    "# if (\"final_output_data\" %in% names(pipeline_results)) {",
    "#   message(\"Accessing 'final_output_data':\")",
    "#   if (is.data.frame(pipeline_results$final_output_data)) print(head(pipeline_results$final_output_data)) else print(pipeline_results$final_output_data)",
    "# }",
    "",
    "message(\"Project execution complete.\")"
  )
  writeLines(main_r_content, file.path(project_name,"src", "_main_.R"))

  # --- Update conf/base/catalog.yaml ---
  catalog_yaml_content <- c(
    "# conf/base/catalog.yaml",
    "# This catalog is initially empty as the example pipeline generates its own data.",
    "# Add entries here if your pipeline needs to load data from external sources via the catalog.",
    "",
    "# Example for a CSV you might have:",
    "# my_csv_data:",
    "#   type: CSVDataset",
    "#   path: 'data/01_raw/my_data.csv'",
    "#   sep: ','",
    "",
    "# Example for an EXCELDataSet",
    "# my_excel_data:",
    "#   type: EXCELDataSet",
    "#   path: 'data/01_raw/my_data.xlsx'",
    "#   sheet: 'Sheet1'",
    "",
    "# Example for a SQLDataSet (requires credentials.yaml setup)",
    "# sales_data_from_db:",
    "#   type: SQLDataSet",
    "#   db_type: 'Snowflake' # or other supported type",
    "#   sql_path: 'data/00_sql_files/get_sales.sql'",
    "#   creds_name: 'my_db_creds'"
  )
  writeLines(catalog_yaml_content, file.path(project_name, "conf", "base", "catalog.yaml"))

  # --- Update conf/base/parameters.yaml ---
  parameters_yaml_content <- c(
    "# conf/base/parameters.yaml",
    "# Example parameters for the template pipeline",
    "",
    "params.generation.example_setting: \"using_iris_dataset\"",
    "",
    "params.processing.example_value: \"added_by_raptorML_node\"",
    "",
    "params.saving.output_path: \"data/03_model_ready/final_iris_data.csv\" # Note: example save_data_node_func doesn't actually write to disk"
  )
  writeLines(parameters_yaml_content, file.path(project_name, "conf", "base", "parameters.yaml"))

  # --- Keep existing template files (SQL, Credentials, Rproj, Readme, .gitignore) ---
  writeLines(c("-- You can paste any SQL SELECT here as a sql statement, including subqueries",
               "SELECT TOP 1000 *",
               "FROM YOURDATABASE;"
            ), file.path(project_name, "data", "00_sql_files", "template.sql"))

  writeLines(c("# Here you can define all your credentials for SQL databases locally using YAML syntax",
               "# Example for snowflake database",
               "# ",
               "# my_db_creds:",
               "#  Server: your_server.snowflakecomputing.com",
               "#  Database: YOUR_DB",
               "#  Schema: YOUR_SCHEMA",
               "#  User: YOUR_USER",
               "#  Password: YOUR_PASSWORD", 
               "#  Warehouse: YOUR_WAREHOUSE"),
             file.path(project_name, "conf", "credentials", "credentials.yaml"))

  readme_content <- c(
    "---",
    paste0("title: \"", project_name, " - Project README\""),
    "output: html_document",
    "---",
    "",
    "## raptorML Project: ", project_name,
    "",
    "This project was initialized using the `raptorML::forge()` function, providing a Kedro-inspired structure.",
    "",
    "### Project Structure",
    "- `conf/`: Contains configuration files.",
    "  - `base/catalog.yaml`: Defines data sources and how to load them.",
    "  - `base/parameters.yaml`: Defines parameters for nodes and pipelines.",
    "  - `credentials/credentials.yaml`: For storing database credentials (ensure this is in .gitignore if sensitive).",
    "- `data/`: For storing data. Subdirectories `00_sql_files`, `01_raw`, `02_processed`, `03_model_ready` are provided.",
    "- `docs/`: Project documentation.",
    "- `logs/`: Log files.",
    "- `notebooks/`: Jupyter or RMarkdown notebooks for exploration.",
    "- `src/`: Source code for the project.",
    "  - `_main_.R`: Main script to run the project pipeline(s).",
    "  - `nodes.R`: Defines individual node functions.",
    "  - `pipelines.R`: Defines how nodes are assembled into pipelines.",
    "- `README.Rmd`: This file.",
    paste0("- `", project_name, ".Rproj`: RStudio project file."),
    "- `.gitignore`: Specifies intentionally untracked files that Git should ignore.",
    "",
    "### Getting Started",
    "1. **Open the Project:** Open the `.Rproj` file in RStudio.",
    "2. **Install Dependencies:** Ensure `raptorML` and any packages used by your nodes (e.g., `yaml`, `DBI`, `odbc`, `readr`, `readxl`) are installed.",
    "3. **Review Configuration:**",
    "   - Edit `conf/base/catalog.yaml` to define your actual data sources if needed.",
    "   - Edit `conf/base/parameters.yaml` to set parameters for your project.",
    "   - If using databases, configure `conf/credentials/credentials.yaml` (and ensure it's gitignored if containing real credentials).",
    "4. **Develop Nodes:**",
    "   - Implement your data processing logic in functions within `src/nodes.R`.",
    "5. **Define Pipelines:**",
    "   - Assemble your nodes into pipelines in `src/pipelines.R`.",
    "6. **Run Pipeline:**",
    "   - Execute your main pipeline by running the `src/_main_.R` script from the R console (e.g., `source(\"src/_main_.R\")`),",
    "     or by using `raptorML::orchestrate()` after setting your working directory to the project root.",
    "     ```R",
    "     # Example from R console (after opening the .Rproj file):",
    "     # source(\"src/_main_.R\") ",
    "     # OR",
    "     # raptorML::orchestrate() # This will look for src/_main_.R by default",
    "     ```",
    "",
    "The template pipeline in `src/pipelines.R` is designed to be runnable out-of-the-box and demonstrates:",
    "- A node that generates sample data (`generate_data_node_func` in `nodes.R`).",
    "- A node that processes this data (`process_data_node_func`).",
    "- A placeholder node for saving data (`save_data_node_func`).",
    "",
    "For more information on `raptorML`, see its documentation."
  )
  writeLines(readme_content, file.path(project_name, "README.Rmd"))

  writeLines(c("Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default", "AlwaysSaveHistory: Default", "",
               "EnableCodeIndexing: Yes", "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8", "",
               "RnwWeave: Sweave", "LaTeX: pdfLaTeX", "", "AutoAppendNewline: Yes", "StripTrailingWhitespace: Yes"),
             file.path(project_name, paste0(project_name, ".Rproj")))

  gitignore_content <- c(
    "# R specific", ".Rproj.user/", ".Rhistory", ".RData", ".RUserFiles/", ".Ruserdata/",
    "", "# raptorML / Kedro specific", "logs/*", "!logs/.gitkeep",
    "data/01_raw/*", "!data/01_raw/.gitkeep", "data/02_processed/*", "!data/02_processed/.gitkeep",
    "data/03_model_ready/*", "!data/03_model_ready/.gitkeep",
    "conf/credentials/*", "!conf/credentials/.gitkeep", "conf/local/*", "!conf/local/.gitkeep",
    "", "# Notebooks", "notebooks/.ipynb_checkpoints", "notebooks/*.html",
    "", "# Python specific (if used alongside R)", "__pycache__/", "*.py[cod]", "*$py.class",
    ".pytest_cache/", "build/", "develop-eggs/", "dist/", "downloads/", "eggs/", ".eggs/",
    "lib/", "lib64/", "parts/", "sdist/", "var/", "wheels/", "share/python-wheels/",
    "*.egg-info/", ".installed.cfg", "*.egg", "MANIFEST",
    "", "# Environment variables", ".env"
  )
  writeLines(gitignore_content, file.path(project_name, ".gitignore"))
  
  # Add .gitkeep files
  for(dir_to_keep in c(
    file.path(project_name, "logs"),
    file.path(project_name, "data", "01_raw"),
    file.path(project_name, "data", "02_processed"),
    file.path(project_name, "data", "03_model_ready"),
    file.path(project_name, "conf", "credentials"),
    file.path(project_name, "conf", "local")
  )) {
    dir.create(dir_to_keep, showWarnings = FALSE, recursive = TRUE)
    writeLines(character(0), file.path(dir_to_keep, ".gitkeep"))
  }

  cat(paste0("Project '", project_name, "' created successfully with a Kedro-like structure.\n",
             "Navigate to the project directory and open the .Rproj file to get started.\n",
             "The template includes a runnable example pipeline in 'src/'.\n",
             "Make sure to review and update conf/base/catalog.yaml and conf/base/parameters.yaml for your project.\n"))
}
