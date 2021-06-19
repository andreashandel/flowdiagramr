## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

# avoid check where vignette filename must equal the vignette title
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, include = FALSE---------------------------------------------------
pkg = 'flowdiagramr' #so we can easily switch names
library(pkg, character.only = TRUE)

## -----------------------------------------------------------------------------
varlabels = c("S","I","R")
flows = list(S_flows = c("-b*S*I"), 
             I_flows = c("b*S*I","-g*I"), 
             R_flows = c("g*I"))
sirmodel1 = list(varlabels = varlabels, flows = flows)

## ---- eval= FALSE-------------------------------------------------------------
#  write_diagram(model_list = sirmodel1, filename = 'sirmodel1_diagram_code.R', always_overwrite = TRUE)

