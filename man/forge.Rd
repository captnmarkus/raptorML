% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forge.R
\name{forge}
\alias{forge}
\title{Create a new project directory structure}
\usage{
forge()
}
\value{
A message indicating that the project structure was created successfully.
If a directory with the specified project name already exists, the function
stops and returns an error message.
}
\description{
This function asks the user for a project name and creates a new directory
structure for the project. The created directories include 'conf', 'data',
'docs', 'logs', 'notebooks', and 'src'. Within 'conf', the function creates
'base' and 'credentials' subdirectories. Within 'data', the function creates
'00_sql_files', '01_raw', '02_processed', and '03_model_ready' subdirectories.
If a directory with the specified project name already exists in the working
directory, the function stops and returns an error message. Additionally, it
creates three YAML files: 'catalog.yaml', 'parameters.yaml' in the 'conf/base'
directory, and 'credentials.yaml' in the 'conf/credentials' directory. It also
creates a 'Readme.Rmd' file, an Rproj file named after the project in the project's
root directory, and a .gitignore file with standard R ignore patterns.
}
\examples{
\dontrun{
forge()
}
}
