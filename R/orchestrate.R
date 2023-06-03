#' Run the '_main_.R' script
#'
#' This function runs the '_main_.R' file located in the 'src' directory. This is part of the project's
#' orchestration process, enabling the execution of a predefined workflow.
#' Note: This function assumes that the working directory is set correctly.
#'
#' @return None
#' @export
#' @examples
#' \dontrun{
#' orchestrate()
#' }

orchestrate <- function() {
  source("src/_main_.R")
}
