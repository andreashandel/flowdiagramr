library(flowdiagramr)

varlabels = c("S","I","R")
flows = list(S_flows = c("-b*S*I"),
             I_flows = c("b*S*I","-g*I"),
             R_flows = c("g*I"))
mymodel = list(varlabels = varlabels, flows = flows)

input_structure <- prepare_diagram(mymodel)

write_diagram(input_list  = mymodel,
              filename = "test1.R")

write_diagram(input_structure = input_structure,
              filename = "test2.R")

# in test1.R and test2.R, colors need to be strings. Scripts currently don't run.
## ATT: Done. Anything not logical or numeric is a string.

# more documentation for each code block/line of code.
## ATT: Done. Tried to describe the overall structure at the top and then
##      each block.

# and if possible 'nice' layout/spacing of code (e.g. trailing spaces/alignment)
## ATT: in progress. The 'styler' package is an option, but requires an
##      external dependency.

#can we rename input_list to model_list or just model?
# And should be same name for prepare_diagram.
## ATT: Done. Renamed to model_list here and in prepare_model.

#maybe following naming: model_list as input into prepare_diagram.
# diagram_list as output from prepare_diagram/input to make_diagram?
# either model_list or diagram_list as input to write_diagram_code
## ATT: Done. We'll need to check vignettes...

# (if both are provided, they'll be written into the script but only diagram_list will be used for the gpplot code)
## ATT: I currently have the function throw an error if both are provided.
##      Would you prefer that both just be written without the error?

#I also saw in prepare_diagram help file that it takes a nodes_df. My thought was to make this part of the model specification,
# i.e. optional matrix of model_list structure (see vignette).
#Is there a reason you split it this way?
## ATT: No reason. I have not really started thinking about this yet. So
##      now is the perfect time to discuss this.


# minor: should one actually think of the ggplot components as layers (i.e. a later layer on top of an earlier)
# or are these just 'elements' of the plot? if elements are actually placed on top of each other as the layers indicate,
# We can stick with that wording, otherwise i would call each bit 'element' or 'component' or such of the diagram
## ATT: Technically, each ggplot "geom" is a layer that goes on top of the
##      one above it. They are also elements, but the ordering matters because
##      of the layering. I have no strong feelings about what we call them in
##      the comments, though.

#this should also work at some point
# the make_diagram_options input could be supplied as list or vector, whatever is better
## ATT: So, either a vector or a list? Or just move to the list option, only?
write_diagram(input_list  = mymodel,
              make_diagram_options = list(with_grid = FALSE, main_arrow_size = 1) )
