# R script to generate a modelbuilder model object with code.

mbmodel = list() #create empty list

#Model meta-information
mbmodel$title = 'Host Heterogeneity Model'
mbmodel$description = 'An SIR type model stratified for two different types of hosts.'
mbmodel$author = 'Andreas Handel, Alexis Vittengl'
mbmodel$date = Sys.Date()
mbmodel$details = 'This model tracks susceptibles, infected and recovered of 2 different types. Think of those types as e.g. males/females, children/adults, etc. The model includes infection, recovery and waning immunity processes for both hosts.'

#Information for all variables
var = vector('list',6)
id = 0
id = id + 1
var[[id]]$varname = 'S1'
var[[id]]$vartext = 'Susceptible type 1 hosts'
var[[id]]$varval = 1000
var[[id]]$flows = c('-b11*S1*I1', '-b12*S1*I2', '+w1*R1')
var[[id]]$flownames = c('Susceptible 1 hosts infected by infected 1 hosts', 'Susceptible 1 hosts infected by infected 2 hosts', 'Loss of immunity by recovered 1 hosts')

id = id + 1
var[[id]]$varname = 'I1'
var[[id]]$vartext = 'Infected type 1 hosts'
var[[id]]$varval = 1
var[[id]]$flows = c('+b11*S1*I1', '+b12*S1*I2', '-g1*I1')
var[[id]]$flownames = c('Susceptible 1 hosts infected by infected 1 hosts', 'Susceptible 1 hosts infected by infected 2 hosts', 'Infected 1 hosts recovery')

id = id + 1
var[[id]]$varname = 'R1'
var[[id]]$vartext = 'Recovered type 1 hosts'
var[[id]]$varval = 0
var[[id]]$flows = c('+g1*I1', '-w1*R1')
var[[id]]$flownames = c('Infected 1 hosts recovery', 'Loss of immunity by recovered 1 hosts')

id = id + 1
var[[id]]$varname = 'S2'
var[[id]]$vartext = 'Susceptible type 2 hosts'
var[[id]]$varval = 200
var[[id]]$flows = c('-b21*S2*I1', '-b22*S2*I2', '+w2*R2')
var[[id]]$flownames = c('Susceptible 2 hosts infected by infected 1 hosts', 'Susceptible 2 hosts infected by infected 2 hosts', 'Loss of immunity by recovered 2 hosts')

id = id + 1
var[[id]]$varname = 'I2'
var[[id]]$vartext = 'Infected type 2 hosts'
var[[id]]$varval = 1
var[[id]]$flows = c('+b21*S2*I1', '+b22*S2*I2', '-g2*I2')
var[[id]]$flownames = c('Susceptible 2 hosts infected by infected 1 hosts', 'Susceptible 2 hosts infected by infected 2 hosts', 'Infected 2 hosts recovery')

id = id + 1
var[[id]]$varname = 'R2'
var[[id]]$vartext = 'Recovered type 2 hosts'
var[[id]]$varval = 0
var[[id]]$flows = c('+g2*I2', '-w2*R2')
var[[id]]$flownames = c('Infected 2 hosts recovery', 'Loss of immunity by recovered 2 hosts')

mbmodel$var = var

#Information for all parameters
par = vector('list',8)
id = 0
id = id + 1
par[[id]]$parname = 'b11'
par[[id]]$partext = 'rate of transmission to susceptible type 1 host from infected type 1 host'
par[[id]]$parval = 0.002

id = id + 1
par[[id]]$parname = 'b12'
par[[id]]$partext = 'rate of transmission to susceptible type 1 host from infected type 2 host'
par[[id]]$parval = 0

id = id + 1
par[[id]]$parname = 'b21'
par[[id]]$partext = 'rate of transmission to susceptible type 2 host from infected type 1 host'
par[[id]]$parval = 0

id = id + 1
par[[id]]$parname = 'b22'
par[[id]]$partext = 'rate of transmission to susceptible type 2 host from infected type 2 host'
par[[id]]$parval = 0.01

id = id + 1
par[[id]]$parname = 'g1'
par[[id]]$partext = 'the rate at which infected type 1 hosts recover'
par[[id]]$parval = 1

id = id + 1
par[[id]]$parname = 'g2'
par[[id]]$partext = 'the rate at which infected type 2 hosts recover'
par[[id]]$parval = 1

id = id + 1
par[[id]]$parname = 'w1'
par[[id]]$partext = 'the rate at which type 1 host immunity wanes'
par[[id]]$parval = 0

id = id + 1
par[[id]]$parname = 'w2'
par[[id]]$partext = 'the rate at which type 2 host immunity wanes'
par[[id]]$parval = 0

mbmodel$par = par

#Information for time parameters
time = vector('list',3)
id = 0
id = id + 1
time[[id]]$timename = 'tstart'
time[[id]]$timetext = 'Start time of simulation'
time[[id]]$timeval = 0

id = id + 1
time[[id]]$timename = 'tfinal'
time[[id]]$timetext = 'Final time of simulation'
time[[id]]$timeval = 60

id = id + 1
time[[id]]$timename = 'dt'
time[[id]]$timetext = 'Time step'
time[[id]]$timeval = 0.1

mbmodel$time = time

mbsirtwohost <- mbmodel
usethis::use_data(mbsirtwohost, overwrite = TRUE)
