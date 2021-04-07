library(flowdiagramr)

varlabels = c("S","I","R")
flows = list(S_flows = c("-b*S*I"),
             I_flows = c("b*S*I","-g*I"),
             R_flows = c("g*I"))
mymodel = list(varlabels = varlabels, flows = flows)

input_structure <- prepare_diagram(mymodel)


write_diagram(input_list  = mymodel,
              directory = "../../Desktop/",
              filename = "test1.R")

write_diagram(input_structure = input_structure,
              directory = "../../Desktop/",
              filename = "test2.R")
