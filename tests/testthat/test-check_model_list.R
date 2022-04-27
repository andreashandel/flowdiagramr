

# Test that error flag is TRUE when variables are missing -----------------

variables = c("S","R")
flows = list(S_flows = c("-b*S*I"),
             I_flows = c("b*S*I","-g*I"),
             R_flows = c("g*I"))
model_list = list(variables = variables, flows = flows)

ret <- suppressWarnings(flowdiagramr:::check_model_list(model_list))

test_that("error flag is returned if variables missing", {
  expect_false(is.null(ret))
})



# Test that error flag is FALSE when all inputs are present ---------------

variables = c("S", "I", "R")
flows = list(S_flows = c("-b*S*I"),
             I_flows = c("b*S*I","-g*I"),
             R_flows = c("g*I"))
model_list = list(variables = variables, flows = flows)

ret <- flowdiagramr:::check_model_list(model_list)

test_that("error flag is not returned if all variables present", {
  expect_null(ret)
})



# Test that error flag is TRUE when flows are not present -----------------

variables = c("S","I","R")
model_list = list(variables = variables)

ret <- flowdiagramr:::check_model_list(model_list)

test_that("error flag is returned when no flows supplied", {
  expect_false(is.null(ret))
})



# Test that error flag is FALSE when no variables supplied ----------------

flows = list(S_flows = c("-b*S*I"),
             I_flows = c("b*S*I","-g*I"),
             R_flows = c("g*I"))
model_list <- list(flows = flows)
ret <- flowdiagramr:::check_model_list(model_list)

test_that("error flag is returned when no flows supplied", {
  expect_false(is.null(ret))
})



