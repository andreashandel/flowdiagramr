#This is the "Environmental Transmission" model from DSAIDE
#https://shiny.ovpr.uga.edu/DSAIDE/

varlabels = c("S","I","R","P")
flows = list(S_flows = c("n","-m*S","-bI*S*I", "-bP*S*P"),
             I_flows = c("bI*S*I", "bP*S*P", "-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"),
             P_flows = c("q*I", "-c*P")
            )
mymodel = list(varlabels = varlabels, flows = flows)
model_list <- mymodel
# nodes_matrix = matrix(c("", "P", "",
#                         "S", "I", "R"),
#                       ncol = 3, byrow = TRUE)


diagram_list = prepare_diagram(mymodel, nodes_matrix = nodes_matrix)
make_diagram(diagram_list)


test_that("right number of vertical edges", {
  expect_equal(5, nrow(diagram_list$vertical_edges))
})
