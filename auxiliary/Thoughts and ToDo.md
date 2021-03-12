Did this:
reorganized and rewrote vignettes
see here for 'odd' naming of vignettes:
https://github.com/r-lib/pkgdown/issues/995


renamed main functions:  prepare_diagram, make_diagram, write_diagram


### General todo/notes
Get predator-prey model to work

As possible, use fs package for file/path operations (e.g. saving R code), seems better then base R

At some point, write unit tests with testthat

### For prepare_diagram:
Maybe be flexible with input structure. E.g. if varlabels/varnames/flows are provided in a different order, still ok? And if they are not named, can we instead try to see if the list has 2 or 3 entries, and try to interpret the 1st as varlabels, 2nd as varnames, 3rd as flows.

Adjust code such that trailing + signs are not required (e.g. it's ok to write 'b*S*I' instead of '+b*S*I')

Update/fix code so that the predator-prey model diagram works.

help file function needs

### For make_diagram:
rename label_flows to flow_labels, rename interaction_label to interaction_flows (since that turns the flows on and off, not just the label), introduce a new interaction_labels, which turns on/off labels on interaction flows only. 

add an option to label the boxes/compartments with varnames instead of varlabels

Also Need to show a few examples in help file for make_diagram


### For write_diagram:
set it up such that it takes the user-created model and optional location and filename for result.
default path is current working directory. default filename is diagram_ggplot_code.R

write_diagram(filepath = filepath, filename = filename, model = mymodel)

The code should then include the model object definition, preparation step and code generation.
Basically in such a way that a user can call this script and reproduce the diagram without needing the original model object.

a sketch of the kind of code this function should produce is shown in the new_write_diagram_code.R script in auxiliary.

