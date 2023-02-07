# A lot of additional models

library(flowdiagramr)




# These are DSAIRM models

## ---- basicvirus ----
variables = c("U","I","V")
# original
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I"),
             V_flows = c("p*I", "-dV*V","-g*b*U*V")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V"),
                                    nrow = 1, byrow = TRUE),
              varspace_x_size = 1
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)

# fix
flows2 = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I"),
             V_flows = c("p*I", "-dV*V")
)
model2 <- list(variables = variables, flows = flows2)
dlist2 <- prepare_diagram(model2,layout)
diag2 <- make_diagram(dlist2)
plot(diag2)




## ---- acutevirus ----
variables = c("U","I","V","F","T")
flows = list(U_flows = c("-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I/(1+kF*F)","-dV*V"),
             #V_flows = c("p*I/(1+kF*F)","-dV*V","-g*b*U*V"), #original
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*F","-dT*T")
             )
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","","I","","V",
                                      "","T","","F",""),
                            nrow = 2, byrow = TRUE),
              varspace_x_size = 0.3
              )
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)

# make it pretty
update_diagram(dlist)
diag_updates <- list(
  flow_curvature = c(i_pI1kFF = 0, i_rTTF = 0, g_rFI = -0.2),
  flow_ystart = c(m_rTTF = -0.25, i_kTTI = 0.5, g_rFI = -0.25),
  flow_yend = c(m_rTTF = -0.75, i_rTTF = -1, g_rFI = 0.5),
  flow_xstart = c(m_rTTF = 0.75, i_kTTI = -0.5),
  flow_xend = c(m_rTTF = 0.25, i_rTTF = 1, i_kTTI = -0.5, g_rFI = 0.15),
  flow_xlabel = c(i_rTTF = 0.5, g_rFI = 0.35, e_dII = 0.15, i_pI1kFF = -0.5),
  flow_ylabel = c(i_rTTF = -0.7, g_rFI = 0.5, i_pI1kFF = 0.9)
)
dlist2 <- update_diagram(diagram_list = dlist,
                         diagram_settings = diag_updates
                         )
diag2 <- make_diagram(dlist2)
plot(diag2)



## ---- chronicvirus ----
variables = c("U","I","V","F","T")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I/(1+kF*F)","-dV*V"),
             #V_flows = c("p*I/(1+kF*F)","-dV*V","-g*b*U*V"), #original
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*V","-dT*T")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","","I","","V",
                                      "","T","","F",""),
                                    nrow = 2, byrow = TRUE),
              varspace_x_size = 0.3
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)
update_diagram(dlist)
diag_updates <- list(
  flow_curvature = c(i_pI1kFF = 0, i_rTTV = 0, g_rFI = -0.2),
  flow_ystart = c(m_rTTV = -0.25, i_kTTI = 0.5, g_rFI = -0.25),
  flow_yend = c(m_rTTV = -0.75, i_rTTV = -1, g_rFI = 0.5),
  flow_xstart = c(m_rTTV = 0.75, i_kTTI = -0.5),
  flow_xend = c(m_rTTV = 0.25, i_rTTV = 1, i_kTTI = -0.5, g_rFI = 0.15),
  flow_xlabel = c(i_rTTV = -0.4, g_rFI = 0.25, e_dII = 0.15, i_pI1kFF = -0.5),
  flow_ylabel = c(i_rTTV = -0.7, g_rFI = 0.5, i_pI1kFF = 0.9)
)
dlist2 <-  update_diagram(diagram_list = dlist,
                          diagram_settings = diag_updates
)
diag2 <- make_diagram(dlist2)
plot(diag2)


## ---- extendedvirus ----
# ka*A*V flow is not showing at all
variables = c("U","I","V","F","T","B","A")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I/(1+sF*F)","-dV*V","-kA*A*V"), #for some strange reason not working
             #V_flows = c("p*I/(1+sF*F)","-dV*V","-b*U*V","-kA*A*V"), #original
             F_flows = c("pF","-dF*F","V*gF*(fmax-F)/(V+hV)"), #original
             #F_flows = c("pF","-dF*F","-V*gF*F"),
             T_flows = c("gT*F*V","rT*T"),
             B_flows = c("gB*B*F"),
             #B_flows = c("gB*B*F*V/(F*V+hF)"), #original
             A_flows = c("rA*B","-dA*A","-kA*A*V")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V",
                                      "F","T","A",
                                      "","B",""),
                                    nrow = 3, byrow = TRUE),
              varbox_x_size = 1.5,
              varspace_y_size = 2,
              varspace_x_size = 1.5
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)
update_diagram(dlist)
diag_updates <- list(
  flow_curvature = c(i_pI1kFF = 0, i_rTTV = 0, g_rFI = -0.2),
  flow_ystart = c(m_rTTV = -0.25, i_kTTI = 0.5, g_rFI = -0.25),
  flow_yend = c(m_rTTV = -0.75, i_rTTV = -1, g_rFI = 0.5),
  flow_xstart = c(m_rTTV = 0.75, i_kTTI = -0.5),
  flow_xend = c(m_rTTV = 0.25, i_rTTV = 1, i_kTTI = -0.5, g_rFI = 0.15),
  flow_xlabel = c(i_rTTV = -0.4, g_rFI = 0.25, e_dII = 0.15, i_pI1kFF = -0.5),
  flow_ylabel = c(i_rTTV = -0.7, g_rFI = 0.5, i_pI1kFF = 0.9)
)
dlist2 <-  update_diagram(diagram_list = dlist,
                          diagram_settings = diag_updates
)
diag2 <- make_diagram(dlist2)
plot(diag2)


## ---- basicbacteria ----
variables = c("B","I")
flows = list(B_flows = c("g*B*(1-B/bmax)","-dB*B","-kI*B*I"),
             I_flows = c("rI*B*(1-I/imax)", "-dI*I")
          )
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("B","I"), nrow = 1, byrow = TRUE))
dlist <- prepare_diagram(model,layout)
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


