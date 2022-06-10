variables = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I"),
  I_flows = c("+b*S*I", "-g*I"),
  R_flows = c("+g*I")
)

varlocations = matrix(
  data = c("S", "", "R",
           "", "I", ""),
  nrow = 2,
  ncol = 3,
  byrow = TRUE
)
sirmodel = list(
  variables = variables,
  flows = flows)

sirsettings <- list(
  varlocations = varlocations
)

diagram_list <- prepare_diagram(sirmodel, sirsettings)

testthat::test_that("y values are not all equal when 2 rows specified", {
  testthat::expect_equal(2, length(unique(diagram_list$variables$ymin)))
})
