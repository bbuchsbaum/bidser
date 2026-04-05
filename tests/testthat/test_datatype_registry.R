# tests/testthat/test_datatype_registry.R

library(testthat)
library(bidser)

context("register_datatype() extension point")

# ---------------------------------------------------------------------------
# Built-in registration
# ---------------------------------------------------------------------------

test_that("five built-in datatypes are registered on load", {
  nms <- list_datatypes()
  expect_true("func"     %in% nms)
  expect_true("anat"     %in% nms)
  expect_true("fmap"     %in% nms)
  expect_true("funcprep" %in% nms)
  expect_true("anatprep" %in% nms)
})

test_that("list_datatypes(scope='raw') returns only raw built-ins", {
  nms <- list_datatypes(scope = "raw")
  expect_true("func" %in% nms)
  expect_true("anat" %in% nms)
  expect_true("fmap" %in% nms)
  expect_false("funcprep" %in% nms)
  expect_false("anatprep" %in% nms)
})

test_that("list_datatypes(scope='derivative') returns only derivative built-ins", {
  nms <- list_datatypes(scope = "derivative")
  expect_true("funcprep" %in% nms)
  expect_true("anatprep" %in% nms)
  expect_false("func" %in% nms)
  expect_false("anat" %in% nms)
})

test_that("get_datatype_spec returns correct fields for 'func'", {
  entry <- get_datatype_spec("func")
  expect_equal(entry$name, "func")
  expect_equal(entry$folder, "func")
  expect_equal(entry$scope, "raw")
  expect_true(isTRUE(entry$builtin))
  expect_true(is.list(entry$spec))
  expect_true(is.list(entry$parser_fn))
})

test_that("get_datatype_spec errors for unknown name", {
  expect_error(get_datatype_spec("nonexistent_xyz"), "No datatype registered as")
})

# ---------------------------------------------------------------------------
# Custom datatype registration
# ---------------------------------------------------------------------------

test_that("register_datatype adds a new custom datatype", {
  on.exit(try(unregister_datatype("dwi_test"), silent = TRUE))

  register_datatype(
    name      = "dwi_test",
    spec      = func_spec(),
    parser_fn = func_parser(),
    folder    = "dwi",
    scope     = "raw"
  )

  expect_true("dwi_test" %in% list_datatypes())
  entry <- get_datatype_spec("dwi_test")
  expect_equal(entry$folder, "dwi")
  expect_equal(entry$scope, "raw")
  expect_false(isTRUE(entry$builtin))
})

test_that("register_datatype errors on duplicate without overwrite", {
  on.exit(try(unregister_datatype("dup_test"), silent = TRUE))

  register_datatype(
    name      = "dup_test",
    spec      = func_spec(),
    parser_fn = func_parser()
  )
  expect_error(
    register_datatype(
      name      = "dup_test",
      spec      = func_spec(),
      parser_fn = func_parser()
    ),
    "already registered"
  )
})

test_that("register_datatype with overwrite=TRUE replaces existing", {
  on.exit(try(unregister_datatype("ow_test"), silent = TRUE))

  register_datatype("ow_test", spec = func_spec(), parser_fn = func_parser(), folder = "orig")
  register_datatype("ow_test", spec = func_spec(), parser_fn = func_parser(), folder = "new",
                    overwrite = TRUE)
  expect_equal(get_datatype_spec("ow_test")$folder, "new")
})

test_that("register_datatype errors on overwriting built-in without overwrite=TRUE", {
  expect_error(
    register_datatype("func", spec = func_spec(), parser_fn = func_parser()),
    "Cannot overwrite built-in"
  )
})

test_that("register_datatype returns name invisibly", {
  on.exit(try(unregister_datatype("invisible_test"), silent = TRUE))
  result <- register_datatype("invisible_test", spec = func_spec(), parser_fn = func_parser())
  expect_equal(result, "invisible_test")
})

test_that("register_datatype validates name is non-empty string", {
  expect_error(register_datatype("", spec = func_spec(), parser_fn = func_parser()),
               "non-empty character string")
  expect_error(register_datatype(123L, spec = func_spec(), parser_fn = func_parser()),
               "non-empty character string")
})

# ---------------------------------------------------------------------------
# Unregister
# ---------------------------------------------------------------------------

test_that("unregister_datatype removes a custom datatype", {
  register_datatype("to_remove", spec = func_spec(), parser_fn = func_parser())
  expect_true("to_remove" %in% list_datatypes())
  unregister_datatype("to_remove")
  expect_false("to_remove" %in% list_datatypes())
})

test_that("unregister_datatype errors on built-in", {
  expect_error(unregister_datatype("func"), "Cannot unregister built-in")
})

test_that("unregister_datatype errors when name not registered", {
  expect_error(unregister_datatype("not_there_xyz"), "No datatype registered as")
})

test_that("unregister_datatype returns name invisibly", {
  register_datatype("ret_test", spec = func_spec(), parser_fn = func_parser())
  result <- unregister_datatype("ret_test")
  expect_equal(result, "ret_test")
})

# ---------------------------------------------------------------------------
# Scope filtering in list_datatypes
# ---------------------------------------------------------------------------

test_that("list_datatypes(scope='both') returns only 'both'-scope entries", {
  on.exit(try(unregister_datatype("both_scope_test"), silent = TRUE))
  register_datatype("both_scope_test", spec = func_spec(), parser_fn = func_parser(),
                    scope = "both")
  nms <- list_datatypes(scope = "both")
  expect_true("both_scope_test" %in% nms)
  expect_false("func" %in% nms)      # func is "raw", not "both"
  expect_false("funcprep" %in% nms)  # funcprep is "derivative", not "both"
})

test_that("list_datatypes(scope='all') returns everything", {
  on.exit(try(unregister_datatype("all_scope_test"), silent = TRUE))
  register_datatype("all_scope_test", spec = func_spec(), parser_fn = func_parser(),
                    scope = "raw")
  nms <- list_datatypes(scope = "all")
  expect_true("func"          %in% nms)
  expect_true("funcprep"      %in% nms)
  expect_true("all_scope_test" %in% nms)
})
