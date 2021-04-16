library(flowdiagramr)

varlabels = c("S","I","R")
flows = list(S_flows = c("-b*S*I"),
             I_flows = c("b*S*I","-g*I"),
             R_flows = c("g*I"))
mymodel = list(varlabels = varlabels, flows = flows)

diagram_list <- prepare_diagram(model_list = mymodel)


write_diagram(model_list = mymodel,
              make_diagram_settings = list(node_fill_color = "red"),
              directory = "../../Desktop/",
              filename = "test1.R")

write_diagram(diagram_list = diagram_list,
              directory = "../../Desktop/",
              filename = "test2.R")

write_diagram(model_list = mymodel,
              diagram_list = diagram_list,
              directory = "../../Desktop/",
              filename = "test3.R")
