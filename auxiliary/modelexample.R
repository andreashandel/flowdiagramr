# Idea: The input structure to the main diagram prep/generation file is a simple list

# suggested list structure as below.
# the easiest way is for a user to build this simple structure themselves.

# alternatively, helper functions will take an mbmodel or potential other future inputs and convert to the structure that is fed into the main diagram generating function
# e.g. something like this
# diag_input_list <- make_diag_input(mbmodel)


#Environmental transmission example
varlabels = c("S","I","R","P")
varnames = c('Susceptible',"Infected","Recovered","Pathogen in environment")
S_flows = c("+n",      "-m*S",    "-bI*I*S", "-bP*P*S")
I_flows = c("-g*I",      "-m*I",    "bI*I*S", "bP*P*S")
R_flows = c("g*I", "-m*R")
P_flows = c("+q*I", "-c*P")

#flows need to be named following the "varlables_flows" convention.
#varnames is optional
#varlabels and varnames need to be specified/names in this way

#with full-text names for variables
diag_input_list = list(varlabels = varlabels, varnames = varnames, S_flows = S_flows, I_flows = I_flows, R_flows = R_flows, P_flows = P_flows)

#also ok
diag_input_list = list(varlabels = varlabels, S_flows = S_flows, I_flows = I_flows, R_flows = R_flows, P_flows = P_flows)

