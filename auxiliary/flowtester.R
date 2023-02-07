# This is a internal development testing script for the
# flowdiagramr R package. Mostly used by ATT and AH for
# finding edge cases where errors might occur.


# Load flowdiagramr ------------------------------------------------------
library(flowdiagramr)



varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I"),
  I_flows = c("b*S*I","-g*I"),
  R_flows = c("g*I","-k*R*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)


# this currently treats S^2 as a single variable and preserves it
# I think that's ok right now? We don't really support "to the power" yet though, do we?
# The diagram looks ok based on the model, just wondering if/how things can go wrong with the ^ symbol
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S^2*I"),
  I_flows = c("b*S^2*I","-g*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

# trying the power thing again
# seems actually ok, but the arrow goes through a box
# so placement of interaction arrow needs some tweaking
# otherwise seems ok
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S^2*I"),
  I_flows = c("b*S^2*I","-g*I"),
  R_flows = c("g*I","-k*R*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

# this is a trick one. The model is properly formulated
# the arrow placement is somewhat off
# i think flipping curvature for cSI would make it right
# though the double-directional arrow between S and I is hard to read
# such loops show up I think rarely enough that we could ask the user
# to manually offset one of the arrows between S and I so it's clear those are
# 2 separate loops/processes
# UPDATE: I think back and forth might actually happen, see my updated goldilocks example
# (now as blog post)
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I", "c*S*I"),
  I_flows = c("b*S*I","-g*I", "-c*S*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)


# covid model
variables <- c("S", "L", "Ia", "Isu", "Isd", "C", "H", "D", "R")
flows <- list(
  S_flows = c("-f(t)*S"),
  L_flows = c("f(t)*S", "-g*a*L", "-(1-g)*(1-a)*L", "-(1-g)*a*L"),
  Ia_flows = c("g*a*L", "-s*Ia"),
  Isu_flows = c("(1-g)*(1-a)*L", "-s*Isu"),
  Isd_flows = c("(1-g)*a*L", "-s*v(t)*Isu"),
  C_flows = c("s*v(t)*Isu", "-w*C*(1-h)/v(t)", "-w*C*h/v(t)"),
  H_flows = c("w*C*h/v(t)", "-z*H*(1-m(t))", "-z*H*m(t)"),
  D_flows = c("z*H*m(t)"),
  R_flows = c("s*Ia", "s*Isu", "w*C*(1-h)/v(t)", "z*H*(1-m(t))")
)

model_list <- list(variables = variables, flows = flows)
locs <- matrix(data = c("", "", "Ia", "", "", "",
                        "S", "L", "Isu", "", "", "R",
                        "", "", "Isd", "C", "H", "D"),
               nrow = 3, ncol = 6, byrow = TRUE)
setts <- list(varlocations = locs, varspace_x_size = 1, varbox_x_size = 1.5)
mod <- prepare_diagram(model_list, model_settings = setts)
newsettings <- list(var_outline_color = c(all = "white",
                                          Ia = "#ef6677",
                                          Isu = "#ef6677"),
                    var_fill_color = c(S = "#91cdf0",
                                       L = "#cdbb44",
                                       Ia = "white",
                                       Isu = "#fbd9dd",
                                       Isd = "#ef6677",
                                       C = "#a6a6a6",
                                       H = "#322f8a",
                                       D = "#842257",
                                       R = "#50aa98"),
                    var_label_color = c(Ia = "grey25",
                                        Isu = "grey25"),
                    var_label_text = c(S = "susceptible",
                                       L = "latent",
                                       Ia = "asymptomic\ninfectious",
                                       Isu = "undetected\ninfectious",
                                       Isd = "detected\ninfectious",
                                       C = "cases",
                                       H = "hospitalizations",
                                       D = "deaths",
                                       R = "recovered"),
                    var_label_size = c(all = 4,
                                       H = 3.5),
                    flow_line_color = c(all = "grey15"),
                    flow_label_text = c(all = ""),
                    flow_ystart = c(m_wC1hvt = 0.25, m_zH1mt = 0.25),
                    flow_yend = c(m_wC1hvt = -0.25, m_zH1mt = -0.5, m_sIa = 0.25))
mod2 <- update_diagram(mod, newsettings)
make_diagram(mod2)
# ggplot2::ggsave("../../Desktop/covid-model.png", height=4, width = 10, units = "in", dpi = 360)


# Simple SIR model for an easy test ---------------------------------------

# make model
variables = c("S","I","R")
flows = list(S_flows = c("n", "-b*S*I", "-m*S"),
             I_flows = c("+b*S*I","-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"))
model_list = list(variables = variables, flows = flows)

check_model_list(model_list)


dfs <- prepare_diagram(model_list)
make_diagram(dfs)  # prints the model diagram
make_diagram(dfs, with_grid = TRUE)  # show the grid


# quick test of update
newsettings <- list(var_label_color = c(S = "green", I = "blue", R = "red"),
                    var_xlabel = c(all = -0.25, R = 0.25),
                    var_ylabel = c(all = 0.25, R = -0.25),
                    flow_line_size = c(interaction = 1.5),
                    flow_line_color = c(all = "grey25",
                                        interaction = "orange",
                                        e_n = "red"),
                    flow_xstart = c(i_bSI = -0.5),
                    flow_xlabel = c(all = 0.1, i_bSI = -0.25),
                    flow_ylabel = c(i_bSI = -0.1),
                    flow_label_text = c(i_bSI = "transmission"))
diag_list_up <- update_diagram(dfs, diagram_settings = newsettings)
make_diagram(diag_list_up)

# and one with an error
newsettings <- list(var_label_color = c(S = "green", I = "blue", R = "red"),
                    flow_line_size = c(interaction = 1.5),
                    flow_line_color = c(all = "grey25",
                                        interaction = "orange",
                                        e_n = "red"),
                    flow_xstart = c(z_n = 0.5))  # ERROR HERE IN NAME
# diag_list_up <- update_diagram(dfs, diagram_settings = newsettings)



# Test locations for SIR --------------------------------------------------

varlocs1 = matrix(c("S","","R","","I",""),byrow=TRUE,nrow=2)
varlocs2 = matrix(c("S","I","R"),byrow=TRUE,nrow=3)
varlocs3 = matrix(data = c("S", "",
                           "", "I",
                           "R", "" ),
                        nrow = 3, ncol = 2, byrow = TRUE)


# two rows, with I in second row
model_settings = list(varlocs1)
make_diagram(prepare_diagram(model_list, model_settings))  #should error

model_settings = list(varlocations = varlocs1)
make_diagram(prepare_diagram(model_list, model_settings))  #should work

# vertical diagram -- not always the best looking
model_settings = list(varlocations = varlocs2, varspace_y_size = 1)
make_diagram(prepare_diagram(model_list, model_settings), with_grid = F)

# two columns, 3 rows

model_settings = list(varlocations = varlocs3)
diagram_list <- prepare_diagram(model_list , model_settings)
make_diagram(diagram_list)



# Test model_settings and errors/warnings ---------------------------------

# these settings are good model settings, should work
model_settings1g = list(
  varbox_x_size = 0.5,
  varbox_y_size = 2)

model_settings2g = list(
  varlocations = varlocs2,
  varbox_x_size = 0.5,
  varbox_y_size = 0.5,
  varspace_x_size = 1,
  varspace_y_size = 1)

model_settings3g = list(
  varlocations = varlocs1,
  varbox_x_size = c(1,2,1),
  varbox_y_size = c(0.5,0.5,2),
  varspace_x_size = 1,
  varspace_y_size = 1)

model_settings4g = list(
  varlocations = varlocs1,
  varbox_x_size = c(1,2,1),
  varbox_y_size = c(0.5,0.5,2)
  )

model_settings5g = list(
  varlocations = varlocs1,
  varbox_x_size = c(1,0.5,1),
  varbox_y_size = c(0.5,2,2)
)

model_settings6g = list(
  varlocations = varlocs2,
  varspace_y_size = c(1,2)
)

# now try them out...no errors/warnings should be produced
diagram_list_orig <- prepare_diagram(model_list)
diagram_list1g <- prepare_diagram(model_list, model_settings1g)
diagram_list2g <- prepare_diagram(model_list, model_settings2g)
diagram_list3g <- prepare_diagram(model_list, model_settings3g)
diagram_list4g <- prepare_diagram(model_list, model_settings4g)
diagram_list5g <- prepare_diagram(model_list, model_settings5g)
diagram_list6g <- prepare_diagram(model_list, model_settings6g)

# do they all print as expected?
make_diagram(diagram_list_orig)
make_diagram(diagram_list1g)
make_diagram(diagram_list2g)
make_diagram(diagram_list3g)
make_diagram(diagram_list4g)



# these are bad settings, should fail
model_settings1b = list(
  varlocations = varlocs1,
  varbox_x_size = 0.5,
  varbox_y_size = 0.5,
  varspace_x_size = c(1, 1),
  varspace_y_size = c(0.1, 0.1))
prepare_diagram(model_list, model_settings1b)  # isses Error


# test sizes for each box
variables = c("S","I","R")
flows = list(S_flows = c("n", "-b*S*I", "-m*S"),
             I_flows = c("+b*S*I","-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"))
model_list = list(variables = variables, flows = flows)
test_settings = list(varlocations = matrix(data = c("S", "",
                                                   "", "I",
                                                   "R", "" ),
                                          nrow = 3, ncol = 2, byrow = TRUE),
                    varbox_x_size = c(1, 1.5, 1),
                    varbox_y_size = c(2, 1, 1),
                    varspace_x_size = 1.5,
                    varspace_y_size = c(0.5,1)
)

dlist <- prepare_diagram(model_list, test_settings)
make_diagram(dlist, with_grid = TRUE)




# Test model updating -----------------------------------------------------

# this should work
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(
  diagram_list,
  diagram_settings = list(flow_line_color = c(main = "orange")))
make_diagram(diagram_list_new)

# this should work
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(
  diagram_list,
  diagram_settings = list(flow_line_color = c(main = "orange"),
                          flow_arrow_size = c(main = 2),
                          flow_show_arrow = c(interaction = FALSE)))
make_diagram(diagram_list_new)

# this should issue a warning about no new settings
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(diagram_list)  # warning issued

# this should work
diagram_settings <- list(var_outline_color = c(S = "black", I = "white", R = "red"))
diagram_list_ok <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_ok)  # good!

#this should work
diagram_settings <- list(var_outline_color = c(S = "black", I = "white", R = "red"),
                         var_fill_color = c(all ="red"))
diagram_list_ok <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_ok)  # good!

# this should error out - it does
diagram_settings <- list(var_outline_color = c(J = "black", K = "red"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this should also error out - wrong number of entries again
# it does error out
diagram_settings <- list(var_outline_color = c(all = "solid"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this should also error out - it does!
diagram_settings <- list(flow_line_type = c(main = "black"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)




# Test make_diagram -------------------------------------------------------

make_diagram(diagram_list_orig)

# this works
make_diagram(diagram_list_ok)



# test predator prey model
variables = c("Pat","Imm")
flows     = list(Pat_flows = c("g*Pat*(1-Pat/pmax)", "-dP*Pat", "-k*Pat*Imm"),
                 Imm_flows = c("r*Pat*Imm", "-dI*Imm"))
model_list = list(variables = variables, flows = flows)
diagram_list <- prepare_diagram(model_list)
make_diagram(diagram_list)


# test an inflow
variables <- c("A", "B", "C")
flows = list(A_flows = c(""),
             B_flows = c("r*A*C"),
             C_flows = c(""))
model_list <- list(variables = variables, flows = flows)
make_diagram(prepare_diagram(model_list))


# environmental transmission
variables = c("S","I","R","P")
flows = list(S_flows = c("n","-m*S","-bI*S*I", "-bP*S*P"),
             I_flows = c("bI*S*I", "bP*S*P", "-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"),
             P_flows = c("q*I", "-c*P")
)

mymodel = list(variables = variables, flows = flows)
diagram_list = prepare_diagram(mymodel)
make_diagram(diagram_list)

# better positioning
myvarlocs = matrix(c("","P","","S","I","R"),byrow=TRUE,nrow=2)
model_settings = list(varlocations = myvarlocs)
make_diagram(prepare_diagram(mymodel, model_settings = model_settings))



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
             # V_flows = c("p*I/(kF*F)", "-dV*V","-g*b*U*V"), #also fails
             V_flows = c("p*I/(1+kF*I)","-dV*V","-g*b*U*V"), #original, fails
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


## ATT testing
model_list <- model
model_settings = list(varlocations = matrix(c("U", "I", "V",
                                              "F", "T", ""),
                                            nrow = 2, byrow = TRUE))
dlist <- prepare_diagram(model_list, model_settings)
diag <- make_diagram(dlist, with_grid = TRUE)
plot(diag)

# try to make it prettier...
model_settings = list(varlocations = matrix(c("", "V", "",
                                              "U", "", "I",
                                              "F", "", "T"),
                                            nrow = 3, byrow = TRUE))
dlist <- prepare_diagram(model_list, model_settings)
diag <- make_diagram(dlist, with_grid = TRUE)
plot(diag)
update_diagram(diagram_list = dlist)
newd <- update_diagram(
  diagram_list = dlist,
  diagram_settings = list(
    flow_xstart = c(i_pI1kFI = 0.5, i_bUV = 0.5),
    flow_ystart = c(
      i_pI1kFI = 0.5,
      i_rTTF = -0.25,
      i_rFI = -0.25,
      i_bUV = -0.75
    ),
    flow_xend = c(
      i_rFI = -0.5,
      i_bUV = 0.5,
      i_gbUV = -0.5
    ),
    flow_yend = c(i_rFI = 0.5),
    flow_ylabel = c(
      i_rFI = -0.2,
      i_rTTF = -0.1,
      i_bUV = -0.75
    ),
    flow_xlabel = c(
      i_pI1kFI = 0.9,
      i_kTTI = 1.1,
      i_bUV = 1.6,
      i_gbUV = -0.25,
      e_dVV = 0.55
    ),
    flow_curvature = c(
      i_kTTI = 1,
      i_rFI = 0.1,
      i_bUV = -0.8
    )
  )
)
make_diagram(newd)

#######################
# extended bacteria model - DSAIRM
# version with log() kinda works (surprising to me, since I didn't think we supported that yet
# but arrow placement is poor
# taking out log doesn't improve arrow placement
#######################

variables = c("B","I","A")
flows = list(B_flows = c("g*B*(1-B/bmax)","-dB*B","-kI*B*I", "-kA*B*A"),
             I_flows = c("rI*B*(1-I/imax)", "-dI*I"),
             # A_flows = c("rA*A*I/(h+I)","-dA*A") #gives pretty much same result
             A_flows = c("rA*A*log(I)/(h+log(I))","-dA*A") #original
)
model_list <- list(variables = variables, flows = flows)

model_settings = list(varlocations = matrix(c("","B","",
                                              "I","","A"),
                                            nrow = 2, byrow = TRUE)
)
dlist <- prepare_diagram(model_list, model_settings)
diag <- make_diagram(dlist)
plot(diag)

# make it pretty
model_settings = list(varlocations = matrix(c("B","","I",
                                              "","A",""),
                                            nrow = 2, byrow = TRUE))
dlist <- prepare_diagram(model_list, model_settings)
make_diagram(dlist)
update_diagram(diagram_list = dlist)
newd <- update_diagram(
  diagram_list = dlist,
  diagram_settings = list(
    flow_xstart = c(e_dBB = -0.2, e_kIBI = 0.2, i_rAAlogIhlogI = 0.2),
    flow_xend = c(e_dBB = -0.2, e_kIBI = 0.2, i_kIBI = 0.2, i_rAAlogIhlogI = 0.3),
    flow_ystart = c(i_kIBI = 0.2, i_rAAlogIhlogI = -0.5),
    flow_yend = c(i_rAAlogIhlogI = -0.2),
    flow_xlabel = c(e_dBB = -0.6, i_kABA = -0.6),
    flow_ylabel = c(i_kABA = -0.5, i_kIBI = 0.4, i_rAAlogIhlogI = -0.3),
    flow_curvature = c(i_kABA = -0.5, i_kIBI = 0)))
make_diagram(newd)











