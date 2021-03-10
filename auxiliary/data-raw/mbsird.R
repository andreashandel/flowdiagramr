# R script to generate a modelbuilder model object with code.

mbmodel = list() #create empty list

#Model meta-information
mbmodel$title = 'SIRSd model'
mbmodel$description = 'A SIRSd model with 3 compartments. Processes are infection, recovery, births deaths and waning immunity.'
mbmodel$author = 'Andreas Handel'
mbmodel$date = Sys.Date()
mbmodel$details = 'The model includes susceptible, infected, and recovered compartments. The processes which are modeled are infection, recovery, natural births and deaths and waning immunity.'

#Information for all variables
var = vector('list',3)
id = 0
id = id + 1
var[[id]]$varname = 'S'
var[[id]]$vartext = 'Susceptible'
var[[id]]$varval = 1000
var[[id]]$flows = c('-b*S*I', '+w*R', '+n', '-m*S')
var[[id]]$flownames = c('infection', 'waning immunity', 'births', 'natural deaths')

id = id + 1
var[[id]]$varname = 'I'
var[[id]]$vartext = 'Infected'
var[[id]]$varval = 1
var[[id]]$flows = c('+b*S*I', '-g*I', '-m*I')
var[[id]]$flownames = c('infection', 'recovery', 'natural deaths')

id = id + 1
var[[id]]$varname = 'R'
var[[id]]$vartext = 'Recovered'
var[[id]]$varval = 0
var[[id]]$flows = c('+g*I', '-w*R', '-m*R')
var[[id]]$flownames = c('recovery', 'waning immunity', 'natural death')

mbmodel$var = var

#Information for all parameters
par = vector('list',5)
id = 0
id = id + 1
par[[id]]$parname = 'b'
par[[id]]$partext = 'infection rate'
par[[id]]$parval = 0.002

id = id + 1
par[[id]]$parname = 'g'
par[[id]]$partext = 'recovery rate'
par[[id]]$parval = 1

id = id + 1
par[[id]]$parname = 'w'
par[[id]]$partext = 'waning immunity rate'
par[[id]]$parval = 1

id = id + 1
par[[id]]$parname = 'n'
par[[id]]$partext = 'birth rate'
par[[id]]$parval = 0

id = id + 1
par[[id]]$parname = 'm'
par[[id]]$partext = 'death rate'
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
time[[id]]$timeval = 100

id = id + 1
time[[id]]$timename = 'dt'
time[[id]]$timetext = 'Time step'
time[[id]]$timeval = 0.1

mbmodel$time = time

mbsird <- mbmodel
usethis::use_data(mbsird, overwrite = TRUE)
