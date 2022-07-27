library(flowdiagramr)

# For hand-drawn diagrams see the 'model' section/tab for each app
# https://shiny.ovpr.uga.edu/DSAIRM/

####################################
# basic virus model
# runs, original not looking good
# removing extra g*b*U*V flow looks mostly good
# curved arrow for p*I from I to V, not sure why curved
#####################################
variables = c("U","I","V")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I"),
             V_flows = c("p*I", "-dV*V")
#             V_flows = c("p*I", "-dV*V","-g*b*U*V") #original
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V"),
                                    nrow = 1, byrow = TRUE),
              varspace_x_size = 1
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)
#ggplot2::ggsave("basicvirus.png",diag)

####################################
# acute virus and IR model
# original fails to run
####################################
variables = c("U","I","V","F","T")
flows = list(U_flows = c("-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I/(1+kF*F)","-dV*V","-g*b*U*V"), #original, fails
             #V_flows = c("p*I/(1+kF*I)","-dV*V","-g*b*U*V"), #this version (which makes no biological sense) works
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*F","-dT*T")
             )
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","","I","","V",
                                      "","F","","T",""),
                            nrow = 2, byrow = TRUE),
              varspace_x_size = 0.3
              )
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)





####################################
# chronic virus and IR model
# original fails to run
# probably same problem as above
####################################
variables = c("U","I","V","F","T")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I/(1+kF*F)","-dV*V","-g*b*U*V"), #original, fails
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*V","-dT*T")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","","I","","V",
                                      "","F","","T",""),
                                    nrow = 2, byrow = TRUE),
              varspace_x_size = 0.3
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


####################################
# extended virus and IR model
# original fails to run
# simplified version also doesn't work
# not sure way, all terms seems fairly standard
####################################
variables = c("U","I","V","F","T","B","A")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I","-dV*V","-b*U*V","-kA*A*V"),
             F_flows = c("pF","-dF*F","-V*gF*F"),
             T_flows = c("gT*F*V","rT*T"),
             B_flows = c("gB*B*F"),
             A_flows = c("rA*B","-dA*A","-kA*A*V")
             #V_flows = c("p*I/(1+sF*F)","-dV*V","-b*U*V","-kA*A*V"), #original, not working
             #F_flows = c("pF","-dF*F","V*gF*(fmax-F)/(V+hV)"), #original, not working
             #B_flows = c("gB*B*F*V/(F*V+hF)"), #original doesn't work
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V",
                                      "F","T","A",
                                      "","B",""),
                                    nrow = 3, byrow = TRUE),
              varspace_x_size = 0.5
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


####################################
# basic bacteria model
# OK
####################################
variables = c("B","I")
flows = list(B_flows = c("g*B*(1-B/bmax)","-dB*B","-kI*B*I"),
             I_flows = c("rI*B*(1-I/imax)", "-dI*I")
          )
model <- list(variables = variables, flows = flows)
#layout = list(varlocations = matrix(c("B","I"), nrow = 1, byrow = TRUE))
#dlist <- prepare_diagram(model,layout)
dlist <- prepare_diagram(model)
diag <- make_diagram(dlist)
plot(diag)


####################################
# extended bacteria model
# mostly works. Unclear why all interaction flows are curved by default.
####################################
variables = c("B","I","A")
flows = list(B_flows = c("g*B*(1-B/bmax)","-dB*B","-kI*B*I", "-kA*B*A"),
             I_flows = c("rI*B*(1-I/imax)", "-dI*I"),
             A_flows = c("rA*A*log(I)/(h+log(I))","-dA*A")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("B","","I",
                                      "","A",""),
                                    nrow = 2, byrow = TRUE))
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)

####################################
# modal variant exploration model
# original fails to run
# simplified also doesn't run
####################################
variables = c("U","I","V","F","A")
flows = list(U_flows = c("n","-dU*U","-b*U*V","-k1*F*U"),
             I_flows = c("b*U*V","-dI*I","-k2*F*I","-k4*A*I","-k5*A*I/(A+sA)"),
             V_flows = c("p*I/(1+k3*F)","-dV*V","-b*U*V","-k6*A*V"),
             F_flows = c("pF","-dF*F","-f1*V*F","f2*V*F","f3*I*F"),
             A_flows = c("a1*F*A","a2*V*F","a3*F*A","-dA*A")
             #F_flows = c("pF","-dF*F","f1*V*(fmax-F)","f2*V*F/(V+sV)","f3*V*I*F/(V*I+sV)"), #original, not working
             #A_flows = c("a1*F*A","a2*V*F/(F+hV)","a3*F*V*A/(F*V+hV)","-dA*A") #original, not working
             )
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","","I","","V",
                                      "","F","","A",""),
                                    nrow = 2, byrow = TRUE),
              varspace_x_size = 0.5
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


####################################
# antiviral treatment app
# looks ok after removal of additional flow
####################################
variables = c("U","I","V")
flows = list(U_flows = c("n","-dU*U","-(1-f)*b*U*V"),
             I_flows = c("(1-f)*b*U*V","-dI*I"),
             V_flows = c("(1-e)*p*I", "-dV*V")
             #V_flows = c("(1-e)*p*I", "-dV*V","-(1-f)*g*b*U*V") #original, doesn't look good
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V"),
                                    nrow = 1, byrow = TRUE),
              varspace_x_size = 0.3
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


####################################
# model comparison app
# seems mostly ok,
# just cosmetic changes needed that can be done manually
####################################
variables = c("U","I","V","T","A")
flows = list(U_flows = c("-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I", "-dV*V","-kA*V*A"), #original
             #V_flows = c("p*I", "-dV*V","-g*b*U*V","-kA*V*A"), #original
             T_flows = c("aT*V", "rT*T"),
             A_flows = c("rA*V*A", "-dA*A")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("","","T","",
                                      "U","I","","V",
                                      "","","A",""),
                                    nrow = 3, byrow = TRUE),
              varspace_x_size = 0.5
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)



####################################
# PK-PD model app
# not working
####################################
variables = c("U","I","V","C")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I"),
             V_flows = c("(1-emax*C)*p*I", "-dV*V"), #also not working
             #V_flows = c("(1-emax*C^k/(C^k_c50))*p*I", "-dV*V", "-g*b*U*V") #original, not working
             C_flows = c("-dC*C")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V",
                                      "","C",""),
                                    nrow = 2, byrow = TRUE),
              varspace_x_size = 1
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)

####################################
# antivirals and drug resistance app
# looks reasonable, only needs cosmetic tweaks
####################################
variables = c("U","Is","Ir","Vs","Vr")
flows = list(U_flows = c("-b*U*Vs","-b*U*Vr"),
             Is_flows = c("b*U*Vs","-dI*Is"),
             Ir_flows = c("b*U*Vr","-dI*Ir"),
             Vs_flows = c("(1-e)*(1-m)*p*Is", "-dV*Vs"),
             Vr_flows = c("(1-e)*m*p*Is","(1-f)*p*Ir", "-dV*Vr")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("","Is","Vs",
                                      "U","","",
                                      "","Ir","Vr"),
                                    nrow = 3, byrow = TRUE),
              varspace_x_size = 1
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


