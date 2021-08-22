2021-08-22

* At some point, check repo and merge or delete any "orphaned" branches.


******
# Urgent/Next
******

My current thinking (up for discussion):

* By default, the first (or, if present, lower left variable in varlocations matrix) is placed on a grid with the lower left corner of that box (xmin/ymin) at (0,0). By default, boxes are of size 1, with both horizontal and vertical spacing between boxes size 2.
* model_settings in prepare_diagram will take these inputs: varlocations, varbox_x_scaling, varbox_y_scaling, varspace_x_scaling, varspace_y_scaling. The first 2 scale the box size by a factor along that direction, e.g. varbox_x_scaling = 1.5 makes each box of size 1.5. Might be easiest to just push the max value out by that amount? varspace does the same for the empty space between boxes, e.g. it scales the default value of 2 to create more/less spacing between boxes. These settings determine the location of all boxes. Once all boxes are 'placed', arrows will then be drawn between boxes based on box start/end settings.
* varnames, use_varnames, var_label_size will be removed from model_settings
* Also, let's call the low/high values for boxes and arrows the same. right now it's xmin and xstart for boxes and arrows respectively. That leads to extra cognitive load by the user :) Just pick one labeling and use the same for both boxes and arrows. I prefer min/max since it doesn't indicate directionality, but I'm ok with either (or something else).

-> Rewrite prepare_diagram to implement those changes. Also, streamline/simplify code. Remove "legacy code", e.g. the sdf/vdf/cdf/etc. distinctions. Make code as streamlined/simple/documented as possible, so I can follow :) and thus maintain/update. That also means changing variable names to something consistent (instead of renaming at end, like we are doing for some currently.)


* make_diagram gets a new entry for diagram_settings called var_label_text. The user provides a vector of text to be printed into the boxes (e.g. the variable names, or anything else). If provided, this is used, otherwise the default is to use varlabels from the model_list. This basically reproduces the varnames/use_varnames functionality in a more flexible way. It also reduces confusion about specifying var_label_size twice.

* If a user specifies one of the entries in diagram_settings, it overwrites whatever is in diagram_list. If a user wants to do more detailed adjustments, they need to edit diagram_list and leave that entry of diagram_settings empty. Maybe (if easy to do) if code detects a non-default setting in diagram_list AND styling for that entry in diagram_settings, it could issue a warning message.

-> Rewrite make_diagram to implement those changes. Also, as for prepare diagram, streamline/simplify code.


******
# Important/Later
******

* In vignette C, last plot of example 1 the I label should be red, i.e. preserved from diagram_list modification. It is not, needs fixing. -- UPDATED TO WORK. BUT WE NEED TO DISCUSS DETAILS.

* Arrow placing for last plot in vignette F is poor. Might not be an easy fix.

* For CRAN submission, we want no errors/warning/notes. To prevent "Undefined global functions or variables", message, mostly caused by ggplot2 code, need to one of dplyr/rlang or utils::globalVariables. 
Right now I'm defining global variables in global.R. I still don't fully understand how 'clean' this approach is. In general, I think one should try to minimize global variables, though it's hard with ggplot and dplyr code. 
The rlang option described at link below can work. As you modify code, see if you can use that approach. And we should give all global variable distinct and unique names so that there isn't another variable with that name, which might lead to conflicts. Right now there are 'x' and 'y' as global variables (see globals.R) which is not ideal.
https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887


* Moved this from vignette A: **NEED TO CHECK THAT DESCRIPTION REGARDING MODEL_LIST SPECIFICATION IS 1) CORRECT, 2) AGREES WITH HELP CONTENT 3) CHECK_MODEL_LIST CHECKS VALIDITY OF ALL THAT.** As discussed, should be combined with modelbuilder to not duplicate checking code. 

* convert_from_modelbuilder function has been updated to provide the new output as list with elements model_list and model_settings. This will require adjustments of modelbuilder code when using flowdiagramr to show diagrams in modelbuilder.

* The quickstart vignette shows an error message "Error in prepare_diagram(model_list): flowdiagramr cannot currently process flows that include an interaction between more than two variables". Not doing more than 2 variables is a problem/limitation we might need to resolve. See e.g. the new 'more model examples' vignette where I tried to implement a model that is biologically reasonable, and ideally should work. Should discuss how difficult fixing this would be. (and first address the other points).

* Document/briefly describe all functions (both exported and internal) in documentation.md inside docsfordevelopers. Big picture, i.e. what function does and how it's called is enough. More detailed explanations should be in each function. Basically anything a new person working on this package needs to know to quickly pick up on things.

* Example 2 in vignette G does not look right, some arrows don't start/end at boxes.

******
# Less Important/Later
******

* In general, if I were to manually label diagrams, I would try to place the text for interaction flows close to the tip of the arrow, i.e., where the interaction happens. E.g. in the basic SIR diagram, I would move it down to where the bSI arrow meets the S->I arrow. Not sure if placement could be changed to be generally close to interaction, and if that would produce worse looking diagrams?

* Would it be useful to include a `box_scaling` argument in the optional prepare_diagram list, which would scale all boxes by a factor (i.e. default is 1, if a user says 2 then boxes would be increased in size by a factor of 2.) That could allow for flexible adjustment of boxes. Not sure if good idea, we can discuss.  

* More comments in the created ggplot code would be good. More or less every line/bit of code should have a brief explanation so user knows what it is/does.

* Write unit tests with testthat

* Implement more error checking inside functions

* Currently, combining flow terms doesn't work. I'm ok for now forcing the user to write them explicitly one by one. One could consider adding parsing logic that can take e.g. S1*(b11*I2 + b12*I2) and parses out the 2 terms. But low priority/not now.


******
# General
******

* Should we try to write a manuscript describing the package?

* There seems to be some overlap between functionality (e.g. processing flows, etc.) done in flowdiagramr and done in modelbuilder. Since I want to use the flowdiagramr functionality in modelbuilder, I think we'll make modelbuilder depend on flowdiagramr. That means we could think about structuring the 2 packages such that certain helper functions live inside flowdiagramr and are used there and also in modelbuilder. E.g. add_plus_signs is likely one. Or get_vars_pars. I'm not sure how to best go about starting that integration, but we should discuss.




