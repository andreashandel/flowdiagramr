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
model = list(varlabels = varlabels, varnames = varnames, S_flows = S_flows, I_flows = I_flows, R_flows = R_flows, P_flows = P_flows)

#also ok
model = list(varlabels = varlabels, S_flows = S_flows, I_flows = I_flows, R_flows = R_flows, P_flows = P_flows)
