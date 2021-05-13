# make model
varlabels = c("S","I","R")
flows = list(S_flows = c("-b*S*I"),
             I_flows = c("+b*S*I","-g*I"),
             R_flows = c("+g*I"))
varnames = c("Susceptible","Infected","Recovered")
varlocations = matrix(data = c("S", "", "R",
                               "", "I", "" ),nrow = 2, ncol = 3, byrow = TRUE)
sirmodel = list(varlabels = varlabels, flows = flows, varnames = varnames, varlocations = varlocations)

check_model_list(sirmodel)


# run prepare_diagram works
#diagram_list <- prepare_diagram(sirmodel)


#flowdiagramr:::check_model_list(sirmodel)

