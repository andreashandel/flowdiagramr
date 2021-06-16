library(flowdiagramr)
varlabels = c("S","I","R")
flows = list(S_flows = c("n", "-b*S*I", "-m*S"),
             I_flows = c("+b*S*I","-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"))
varnames = c("Susceptible","Infected","Recovered")
varlocations = matrix(data = c("S", "", "R",
                               "", "I", "" ),nrow = 2, ncol = 3, byrow = TRUE)
model_list = list(varlabels = varlabels, flows = flows)
model_settings = list(varnames = varnames)
diagram_list <- prepare_diagram(model_list, model_settings)

?make_diagram()

make_diagram(diagram_list, diagram_settings = list(main_flow_on = FALSE))
make_diagram(diagram_list, diagram_settings = list(external_flow_on = FALSE))
make_diagram(diagram_list, diagram_settings = list(interaction_flow_on = FALSE))
make_diagram(diagram_list, diagram_settings = list(interaction_flow_label_on = FALSE))
make_diagram(diagram_list, diagram_settings = list(main_flow_label_on = FALSE))
make_diagram(diagram_list, diagram_settings = list(external_flow_label_on = FALSE))
make_diagram(diagram_list, diagram_settings = list(external_flow_label_on = FALSE,
                                                   external_flow_on = FALSE))

make_diagram(diagram_list, diagram_settings = list(main_flow_color = "blue",
                                                   interaction_flow_color = "red",
                                                   external_flow_color = "orange",
                                                   external_flow_linetype = "dotted",
                                                   var_fill_color = c("salmon", "cyan"),
                                                   var_outline_color = "dodgerblue",
                                                   main_flow_size = 5))

