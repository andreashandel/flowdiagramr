#######################
# Collection of examples that currently don't work right
#######################
library(flowdiagramr)


#######################
# acute virus and IR model - DSAIRM
# original version fails
# several alternatives I tried also fail
# seems the division component is not processed right?
# getting this to work is important since models like that
# show up a lot in DSAIRM (and other places)
#######################

variables = c("U","I","V","F","T")
flows = list(U_flows = c("-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             # V_flows = c("p*I", "-dV*V","-g*b*U*V"), # works
             # V_flows = c("I/F", "-dV*V","-g*b*U*V"), # also fails
             # V_flows = c("p*I", "-dV*V","-g*b*U*V"), #also fails
             #V_flows = c("p*I/(kF*F)", "-dV*V","-g*b*U*V"), #also fails
             V_flows = c("p*I/(1+kF*I)","-dV*V","-g*b*U*V"), #original, fails
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*F","-dT*T")
)
model <- list(variables = variables, flows = flows)

dlist <- prepare_diagram(model)
diag <- make_diagram(dlist)
plot(diag)


#######################
# extended bacteria model - DSAIRM
# version with log() kinda works (surprising to me, since I didn't think we supported that yet
# but arrow placement is poor
# taking out log doesn't improve arrow placement
#######################

variables = c("B","I","A")
flows = list(B_flows = c("g*B*(1-B/bmax)","-dB*B","-kI*B*I", "-kA*B*A"),
             I_flows = c("rI*B*(1-I/imax)", "-dI*I"),
             A_flows = c("rA*A*I/(h+I)","-dA*A") #gives pretty much same result
             #A_flows = c("rA*A*log(I)/(h+log(I))","-dA*A") #original
)
model <- list(variables = variables, flows = flows)

layout = list(varlocations = matrix(c("","B","",
                                      "I","","A"),
                                    nrow = 2, byrow = TRUE)
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)










#######################
# A DAG
# this produces poor placement of some arrows
# also, would be nice af the Access and Diet boxes (and in general all boxes)
# are centered
# strangely enough, if one slightly changes the box sizes (see code)
# the arrows start/end at the right places. Not sure what's going on

#######################
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
                       varbox_x_size = c(2,2,1,1,3) #this produces bad default placement
                       # varbox_x_size = c(2,2,2,2,3) #this works ok

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
