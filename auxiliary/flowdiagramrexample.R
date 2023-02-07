# trying to make one of the DAGs
# seems to not be able to do a pretty one with dagitty
# so using our flowdiagramr package
#devtools::install_github("andreashandel/flowdiagramr")
library(flowdiagramr)
variables <- c("Me","Hb","Ac","Di","So")
varnames <- c("Medication","HbA1c","Access","Diet","Socioeconomic\nstatus")
names(varnames) <- variables
flows <- list(Me_flows = c("-k1*Me","k2*Ac"),
              Hb_flows = c("k1*Me","k3*Di"),
              Ac_flows = c("-k2*Ac","k4*So"),
              Di_flows = c("-k3*Di","k5*So"),
              So_flows = c("-k4*So","-k5*So")
)
model <- list(variables = variables, flows = flows)
locations = matrix( c("Me","","Hb",
                      "Ac","","Di",
                      "","So",""),
                    nrow = 3, byrow = TRUE)
model_settings <- list(varlocations = locations,
                       varbox_x_size = c(2,2,2,2,3) #this works ok
                       #varbox_x_size = c(2,2,1,1,3) #this produces bad default placement
)
diag_list <- prepare_diagram(model, model_settings)

styling = list(var_fill_color = c(all = "white", Me = "yellow", Hb = "lightblue"),
               var_label_text = varnames,
               var_label_color = c(all = "black"),
               flow_show_label = c(all = FALSE),
               var_label_size = c(all = 8))

diag_list2 <- update_diagram(diag_list,styling)

p1 <- make_diagram(diag_list2)
plot(p1)