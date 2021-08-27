library(flowdiagramr)



varlabels = c("S","I","R")
flows = list(S_flows = c("n", "-b*S*I", "-m*S"),
             I_flows = c("+b*S*I","-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"))
varnames = c("Susceptible","Infected","Recovered")

varlocations = matrix(data = c("S", "", "R",
                               "", "I", "" ),nrow = 2, ncol = 3, byrow = TRUE)

model_list = list(varlabels = varlabels, flows = flows)
model_settings = list(
  varlocations = NULL,
  varbox_x_scaling = 1,
  varbox_y_scaling = 1,
  varspace_x_scaling = 1,
  varspace_y_scaling = 1)
diagram_list <- prepare_diagram(model_list, model_settings)
make_diagram(diagram_list)

model_settings = list(
  varlocations = varlocations,
  varbox_x_scaling = 2,
  varbox_y_scaling = 2,
  varspace_x_scaling = 2,
  varspace_y_scaling = 2)
diagram_list <- prepare_diagram(model_list, model_settings)
make_diagram(diagram_list, diagram_settings = list(with_grid = FALSE))


model_list = list(varlabels = varlabels, flows = flows)
model_settings = list(
  varlocations = NULL,
  varbox_x_scaling = 1,
  varbox_y_scaling = 1,
  varspace_x_scaling = 1,
  varspace_y_scaling = 1)
diagram_list <- prepare_diagram(model_list, model_settings)
make_diagram(diagram_list, diagram_setting=list(var_label_text = c("Susc", "Inf", "Rec")))



varlabels = c("Pat","Imm")
flows     = list(Pat_flows = c("g*Pat*(1-Pat/pmax)", "-dP*Pat", "-k*Pat*Imm"),
                 Imm_flows = c("r*Pat*Imm", "-dI*Imm"))
model_list = list(varlabels = varlabels, flows = flows)
model_settings = list(
  varlocations = NULL,
  varbox_x_scaling = 1,
  varbox_y_scaling = 1,
  varspace_x_scaling = 1,
  varspace_y_scaling = 1)
diagram_list <- prepare_diagram(model_list, model_settings)
make_diagram(diagram_list, diagram_settings = list(with_grid = TRUE))



model_settings = list(varnames = varnames, varlocations = varlocations, use_varnames = TRUE)


diagram_list <- prepare_diagram(model_list, model_settings)

write_diagram(model_list, model_settings)
write_diagram(diagram_list = diagram_list)

?make_diagram()

make_diagram(diagram_list)
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


make_diagram(diagram_list, diagram_settings = list(main_flow_color = "blue",
                                                   interaction_flow_color = "red",
                                                   external_flow_color = "orange",
                                                   external_flow_linetype = 6,
                                                   var_fill_color = c("salmon", "cyan"),
                                                   var_outline_color = "dodgerblue",
                                                   main_flow_size = 5))







varlabels = c("Sc","Ic","Rc","Sa","Ia","Ra","P")
varnames = c("Susceptible Children","Infected Children","Recovered Children",
             "Susceptible adults","Infected adults","Recovered adults",
             "Pathogen in Environment")
flows = list(Sc_flows = c("-Sc*bcc*Ic","-Sc*bca*Ia","-Sc*bcp*P"),
             Ic_flows = c("Sc*bcc*Ic","Sc*bca*Ia","Sc*bcp*P","-gc*Ic"),
             Rc_flows = c("gc*Ic"),
             Sa_flows = c("-Sa*bac*Ic","-Sa*baa*Ia","-Sa*bap*P"),
             Ia_flows = c("Sa*bac*Ic","Sa*baa*Ia","Sa*bap*P","-ga*Ia"),
             Ra_flows = c("ga*Ia"),
             P_flows = c("sc*Ic","sa*Ia","-d*P")
)
varlocations = matrix(data = c("Sc", "Ic", "Rc",
                               "",   "P",   "",
                               "Sa", "Ia", "Ra"),nrow = 3, byrow = TRUE)
model_list = list(varlabels = varlabels, flows = flows)
model_settings = list(varlocations = NULL, varbox_x_scaling = 1,
                      varbox_y_scaling = 1,
                      varspace_x_scaling = 1,
                      varspace_y_scaling = 1)
diagram_list <- prepare_diagram(model_list,model_settings)

diagram_settings <- list(
  var_fill_color = c("#6aa4c8", "#eb5600", "#1a9988", "#2987c2", "#e38b59", "#5c948c", "#e8e656"),
  interaction_flow_label_size = 4,
  interaction_flow_color = "blue",
  with_grid = TRUE
)
model_plot <- make_diagram(diagram_list, diagram_settings)
plot(model_plot)

