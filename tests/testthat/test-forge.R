test_that("forge creates directories and files correctly", {

  # Run the function with a test project name
  test_project <- "example_project"

  # Check if directories are created
  expect_true(dir.exists(file.path(test_project, "conf")))
  expect_true(dir.exists(file.path(test_project, "conf", "base")))
  expect_true(dir.exists(file.path(test_project, "conf", "credentials")))
  expect_true(dir.exists(file.path(test_project, "data")))
  expect_true(dir.exists(file.path(test_project, "src")))

  # Check if files are created and contain the right content
  expect_true(file.exists(file.path(test_project, "conf", "base", "catalog.yaml")))
  expect_true(file.exists(file.path(test_project, "data", "00_sql_files", "template.sql")))
  expect_true(file.exists(file.path(test_project, "conf", "base", "parameters.yaml")))
  expect_true(file.exists(file.path(test_project, "conf", "credentials", "credentials.yaml")))
  expect_true(file.exists(file.path(test_project, "README.Rmd")))
  expect_true(file.exists(file.path(test_project, paste0(test_project, ".Rproj"))))
  expect_true(file.exists(file.path(test_project,"src", "_main_.R")))

})
