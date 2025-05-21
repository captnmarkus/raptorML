# tests/testthat/test-core_framework.R
library(testthat)
# Assuming raptorML functions are available (e.g. if loaded via devtools::load_all() or library(raptorML))
# Or, explicitly source them if necessary for testing environment:
# source("../../R/nodes.R")
# source("../../R/pipelines.R")
# source("../../R/parameters.R") # Added for load_parameters

# --- Tests for create_node ---
context("create_node Functionality")

dummy_func <- function(input1, param1) { input1 + param1 }
dummy_func_no_param <- function(input1) { input1 }

test_that("create_node creates a valid raptor_node object", {
  node <- create_node(
    func = dummy_func,
    inputs = list(input1 = "data_in"),
    outputs = "data_out",
    name = "TestNode1",
    parameters = list(param1 = 10),
    tags = c("test", "core")
  )
  expect_s3_class(node, "raptor_node")
  expect_equal(node$name, "TestNode1")
  expect_equal(node$inputs, list(input1 = "data_in"))
  expect_equal(node$outputs, "data_out")
  expect_true(is.function(node$func))
  expect_equal(node$parameters, list(param1 = 10))
  expect_equal(node$tags, c("test", "core"))
})

test_that("create_node generates default name if NULL", {
  node <- create_node(func = dummy_func_no_param, inputs = "in", outputs = "out")
  expect_s3_class(node, "raptor_node")
  expect_equal(node$name, "dummy_func_no_param") # or deparse(substitute(dummy_func_no_param))
})

test_that("create_node handles various input/output formats", {
  node1 <- create_node(func = dummy_func, inputs = "data_in", outputs = "data_out", parameters = list(param1=1))
  expect_equal(node1$inputs, "data_in") # Keep as character if passed as character
  
  node2 <- create_node(func = dummy_func, inputs = c(input1="data_in"), outputs = "data_out", parameters = list(param1=1))
  expect_equal(node2$inputs, c(input1="data_in"))

  node3 <- create_node(func = dummy_func, inputs = list(input1="data_in1", input2="data_in2"), outputs = c("out1", "out2"), parameters = list(param1=1))
  expect_true(is.list(node3$inputs))
  expect_true(is.character(node3$outputs)) # Multiple outputs as char vector
})

test_that("create_node input validation works", {
  expect_error(create_node(func = "not_a_function", inputs = "in", outputs = "out"),
               "`func` must be an R function.")
  expect_error(create_node(func = dummy_func, inputs = 123, outputs = "out", parameters = list(param1=1)),
               "`inputs` must be a character vector or a list.")
  expect_error(create_node(func = dummy_func, inputs = "in", outputs = 123, parameters = list(param1=1)),
               "`outputs` must be a character vector or a list.")
  expect_error(create_node(func = dummy_func, inputs = "in", outputs = "out", name=123, parameters = list(param1=1)),
               "`name` must be a character string or NULL.")
  expect_error(create_node(func = dummy_func, inputs = "in", outputs = "out", tags=123, parameters = list(param1=1)),
               "`tags` must be a character vector or NULL.")
})

# --- Tests for create_pipeline ---
context("create_pipeline Functionality")

node1 <- create_node(func = dummy_func, inputs = "in1", outputs = "mid1", name = "N1", parameters = list(param1=1))
node2 <- create_node(func = dummy_func, inputs = "mid1", outputs = "out1", name = "N2", parameters = list(param1=1))
node3_dup_name <- create_node(func = dummy_func, inputs = "in2", outputs = "out2", name = "N1", parameters = list(param1=1))

test_that("create_pipeline creates a valid raptor_pipeline object", {
  pipe <- create_pipeline(node1, node2, name = "TestPipe1")
  expect_s3_class(pipe, "raptor_pipeline")
  expect_equal(pipe$name, "TestPipe1")
  expect_length(pipe$nodes, 2)
  expect_s3_class(pipe$nodes[[1]], "raptor_node")
})

test_that("create_pipeline handles list of nodes as input", {
  pipe <- create_pipeline(list(node1, node2), name = "TestPipe2")
  expect_s3_class(pipe, "raptor_pipeline")
  expect_length(pipe$nodes, 2)
})

test_that("create_pipeline flattens nested pipelines", {
  inner_pipe <- create_pipeline(node1, name = "InnerPipe")
  outer_pipe <- create_pipeline(inner_pipe, node2, name = "OuterPipe")
  expect_s3_class(outer_pipe, "raptor_pipeline")
  expect_length(outer_pipe$nodes, 2)
  expect_equal(outer_pipe$nodes[[1]]$name, "N1")
  expect_equal(outer_pipe$nodes[[2]]$name, "N2")
})

test_that("create_pipeline input validation works", {
  expect_error(create_pipeline(node1, "not_a_node"),
               "All elements passed to create_pipeline must be raptor_node or raptor_pipeline objects.")
})

test_that("create_pipeline warns on duplicate node names", {
  # N1 and node3_dup_name both have name "N1"
  expect_warning(create_pipeline(node1, node2, node3_dup_name),
                 "Pipeline contains duplicate node names: N1")
})


# --- Tests for load_parameters ---
context("load_parameters Functionality") # This will also generate a warning

test_that("load_parameters loads a valid YAML file correctly", {
  # Create a temporary valid YAML file
  valid_yaml_content <- c(
    "param1: value1",
    "param2:",
    "  nested_param: value2",
    "  another_list:",
    "    - item1",
    "    - item2",
    "param3: 123"
  )
  temp_yaml_file <- tempfile(fileext = ".yaml")
  writeLines(valid_yaml_content, temp_yaml_file)
  on.exit(unlink(temp_yaml_file)) # Ensure cleanup

  # Assuming load_parameters is available (e.g., package loaded or sourced)
  params <- load_parameters(path = temp_yaml_file)
  
  expect_equal(params$param1, "value1")
  expect_true(is.list(params$param2))
  expect_equal(params$param2$nested_param, "value2")
  expect_equal(params$param2$another_list, c("item1", "item2")) # Adjusted expectation
  expect_equal(params$param3, 123)
})

test_that("load_parameters handles non-existent file", {
  non_existent_file <- tempfile(fileext = ".yaml") 
  # Ensure it doesn't exist, tempfile() just gives a name
  if (file.exists(non_existent_file)) unlink(non_existent_file)

  expect_error(
    load_parameters(path = non_existent_file),
    paste("Parameters file not found at path:", non_existent_file)
  )
})

test_that("load_parameters handles malformed YAML file", {
  # Create a temporary malformed YAML file
  malformed_yaml_content <- c(
    "param1: value1",
    "param2: [missing_quote" # Malformed
  )
  temp_malformed_file <- tempfile(fileext = ".yaml")
  writeLines(malformed_yaml_content, temp_malformed_file)
  on.exit(unlink(temp_malformed_file))

  # The exact error message from yaml::read_yaml might vary, 
  # so we match for a part of our custom error message.
  expect_error(
    load_parameters(path = temp_malformed_file),
    "Error loading or parsing YAML file" 
  )
})
