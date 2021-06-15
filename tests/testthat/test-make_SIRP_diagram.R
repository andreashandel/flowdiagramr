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

diagram_list = prepare_diagram(mymodel)
# make_diagram(diagram_list)

externals <- subset(diagram_list$flows, interaction == FALSE & (is.na(to) | is.na(from)))

test_that("right number of external flows", {
  expect_equal(5, nrow(externals))
})
