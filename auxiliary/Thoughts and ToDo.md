******
2021-04-01

- why do nodes in input_structure object not have xstart/xend, ystart/yend to determine size of box, and only have x and y?

- add more details to each public function roxygen block, e.g. examples and more documentation/information.

- write_diagram should take as input either a model list or the input_structure object. if model list, then the function will include a call to prepare_diagram. for either scenario, the code that is being generated should be fully 'stand-alone'. that means the original model list or the input_structure object should be written at the top of the output code.

- add a simple 'vertical = TRUE' feature to make_diagram? Or skip and just let user specify input matrix?

COMPLETED----
- prepare_diagram function should explain in detail both the input and output objects and their structure -- DONE.
- modify diagram (vignette B) produces warning messages. Should probably fix and/or suppress. -- FIXED.
- change main_arrow_linetype input to a word instead of a number (e.g. solid/dashed/etc.) -- ALREADY POSSIBLE, WILL UPDATE DOC.
- labeling of main vs interaction flows doesn't seem to work for PP model? see 1st vignette diagram, only main arrows should be dashed, but interaction arrows are also dashed.  -- WORKS FOR ME. INTERACTION ARROW TYPE IS DASHED DEFAULT, MUST SPECIFY OTHERWISE.


******

### General todo/notes
Get predator-prey model to work -- DONE.

As possible, use fs package for file/path operations (e.g. saving R code), seems better then base R

At some point, write unit tests with testthat

Somewhere (e.g. in auxiliary or inst folders), there should be a text file giving all the details needed for developers to work on this. E.g. big-picture description of all functions in the /R folder and on other folders. See e.g. the 'doscfordevelopers' folder in the DSAIDE package, especially documentation.md


### For prepare_diagram:
At start of function, the input should be checked to make sure it looks as needed. If not, a meaningful error message should be given to user. This checking might be best done in a separate function?

Maybe be flexible with input structure. E.g. if varlabels/varnames/flows are provided in a different order, still ok? And if they are not named, can we instead try to see if the list has 2 or 3 entries, and try to interpret the 1st as varlabels, 2nd as varnames, 3rd as flows? Not that crucial for now, but we could decide what we allow as input structure. Just need to make sure we fully document this, both in the function help file and the vignette.

Adjust code such that trailing + signs are not required (e.g. it's ok to write 'b*S*I' instead of '+b*S*I')

Update/fix code so that the predator-prey model diagram works.  -- DONE.

help file needs more details

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

