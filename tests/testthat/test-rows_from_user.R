varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I"),
  I_flows = c("+b*S*I", "-g*I"),
  R_flows = c("+g*I")
)
varnames = c("Susceptible", "Infected", "Recovered")
varlocations = matrix(
  data = c("S", "", "R",
           "", "I", ""),
  nrow = 2,
  ncol = 3,
  byrow = TRUE
)
sirmodel = list(
  varlabels = varlabels,
  flows = flows,
  varnames = varnames,
  varlocations = varlocations
)

diagram_list <- prepare_diagram(sirmodel)

test_that("y values are not all equal when 2 rows specified", {
  expect_equal(2, length(unique(diagram_list$nodes$ymin)))
})
