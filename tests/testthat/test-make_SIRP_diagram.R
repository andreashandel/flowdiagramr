#This is the "Environmental Transmission" model from DSAIDE
#https://shiny.ovpr.uga.edu/DSAIDE/

variables = c("S","I","R","P")
flows = list(S_flows = c("n","-m*S","-bI*S*I", "-bP*S*P"),
             I_flows = c("bI*S*I", "bP*S*P", "-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"),
             P_flows = c("q*I", "-c*P")
            )

mymodel = list(variables = variables, flows = flows)

diagram_list = prepare_diagram(mymodel)

test_that("right number of flows", {
  expect_equal(10, nrow(diagram_list$flows))
})
