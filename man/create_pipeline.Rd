% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipelines.R
\name{create_pipeline}
\alias{create_pipeline}
\title{Create a raptor_pipeline object}
\usage{
create_pipeline(..., name = NULL)
}
\arguments{
\item{...}{A list of \code{raptor_node} objects, or individual \code{raptor_node}
objects provided as separate arguments. If pipelines are passed, they will be
expanded into their constituent nodes.}

\item{name}{An optional character string for the name of the pipeline.}
}
\value{
An S3 object of class \code{raptor_pipeline}. This object is a list
containing the nodes in the pipeline and potentially other metadata.
}
\description{
This function constructs a pipeline from a list of raptor_node objects.
It performs initial validation on the nodes provided.
}
\examples{
\dontrun{
# Assuming node1, node2 are raptor_node objects
# pipe <- create_pipeline(node1, node2, name = "my_simple_pipeline")
# pipe_from_list <- create_pipeline(list(node1, node2), name = "my_list_pipeline")
}
}
