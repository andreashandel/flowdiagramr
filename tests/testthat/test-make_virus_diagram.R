#This is the "Basic Virus" model from DSAIRM
#https://shiny.ovpr.uga.edu/DSAIRM/
# note that the original model has a scaling factor in front of the V infection term
# that is too hard to get right, we omit here


variables = c("U","I","V")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V", "-dI*I"),
             V_flows = c("p*I", "-dV*V", "-b*U*V")
)
mymodel = list(variables = variables, flows = flows)

diagram_list = prepare_diagram(mymodel)
diag <- make_diagram(diagram_list)

testthat::expect_is(diag, "ggplot" )




