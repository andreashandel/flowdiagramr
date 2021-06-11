
pkg = 'flowdiagramr' #so we can easily switch names
library(pkg, character.only = TRUE)


# simple SIR model
varlabels = c("S","I","R")
flows = list(S_flows = c("-b*S*I"),
             I_flows = c("b*S*I","-g*I"),
             R_flows = c("g*I"))
mymodel = list(varlabels = varlabels, flows = flows)

diagram_list <- prepare_diagram(mymodel)

my_diagram <- make_diagram(diagram_list)

plot(my_diagram)
ggplot2::ggsave("sirdiagram.png",my_diagram)

# predator-prey model
varlabels = c("Pat","Imm")
flows     = list(Pat_flows = c("g*Pat*(1-Pat/pmax)", "-dP*Pat", "-k*Pat*Imm"),
                 Imm_flows = c("r*Pat*Imm", "-dI*Imm"))
mymodel = list(varlabels = varlabels, flows = flows)

diagram_list <- prepare_diagram(mymodel)

my_diagram_2 <- make_diagram(
  diagram_list,
  diagram_settings = list(node_fill_color = c('blue','red'),
                          main_arrow_linetype = "dashed",
                          interaction_arrow_linetype = "solid"
                          )
)

plot(my_diagram_2)

# 7-compartment model
varlabels = c("Sc","Ic","Rc","Sa","Ia","Ra","P")
flows = list(Sc_flows = c("-Sc*bcc*Ic","-Sc*bca*Ia","-Sc*bcp*P"),
             Ic_flows = c("Sc*bcc*Ic","Sc*bca*Ia","Sc*bcp*P","-gc*Ic"),
             Rc_flows = c("gc*Ic"),
             Sa_flows = c("-Sa*bac*Ic","-Sa*baa*Ia","-Sa*bap*P"),
             Ia_flows = c("Sa*bac*Ic","Sa*baa*Ia","Sa*bap*P","-ga*Ia"),
             Ra_flows = c("ga*Ia"),
             P_flows = c("sc*Ic","sa*Ia","-d*P")
             )
mymodel = list(varlabels = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)

model_plot <- make_diagram(diagram_list)
plot(model_plot)
