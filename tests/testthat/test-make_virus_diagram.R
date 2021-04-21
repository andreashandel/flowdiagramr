#This is the "Basic Virus" model from DSAIRM
#https://shiny.ovpr.uga.edu/DSAIRM/
# note that the original model has a scaling factor in front of the V infection term
# that is too hard to get right, we omit here


varlabels = c("U","I","V")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V", "-dI*I"),
             V_flows = c("p*I", "-dV*V", "-b*U*V")
)
mymodel = list(varlabels = varlabels, flows = flows)

model_list = mymodel
nodes_matrix = NULL

diagram_list = prepare_diagram(mymodel)
# make_diagram(diagram_list)


test_that("right number of vertical edges", {
  expect_equal(4, nrow(diagram_list$vertical_edges))
})

