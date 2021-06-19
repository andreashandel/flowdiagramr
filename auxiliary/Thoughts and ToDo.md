******
# Urgent
******

* In vignette C, last plot of example 1 the I label should be red, i.e. preserved from diagram_list modification. It is not, needs fixing.

* get write_code to work. Then change all `source` code chunks in vignette D from eval=FALSE to eval=TRUE

******
# Important/Later
******

* Arrow placing for last plot in vignette F is poor. Might not be an easy fix.

* Rewrite prepare_diagram. Contains a lot of "legacy code", e.g. the sdf/vdf/cdf/etc. distinctions. And there is a lot of code overall, seems like it should be possible to streamline a good bit. As it is right now, really hard to follow (for me) and thus maintain/update.

* Fix size of boxes when variable names are used. Still not quite working, see e.g. the example in the slides. 

* Moved this from vignette A: **NEED TO CHECK THAT DESCRIPTION REGARDING MODEL_LIST SPECIFICATION IS 1) CORRECT, 2) AGREES WITH HELP CONTENT 3) CHECK_MODEL_LIST CHECKS VALIDITY OF ALL THAT.** As discussed, should be combined with modelbuilder to not duplicate checking code. 

* convert_from_modelbuilder function has been updated to provide the new output as list with elements model_list and model_settings. This will require adjustments of modelbuilder code when using flowdiagramr to show diagrams in modelbuilder.

* For CRAN submission, we want no errors/warning/notes. To prevent "Undefined global functions or variables", message, mostly caused by ggplot2 code, need to one of dplyr/rlang or utils::globalVariables. 
See e.g. here:
https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
We should also try to minimize global use, and give them distinct names so that there isn't another variable with that name.

* In `prepare_diagram`, unclear how this block throws an error message that leads to exit out of the main function:
if(!is.null(nodes_matrix)) {
    # returns fatal error if variables do not match
    check_nodes_matrix(model_list, nodes_matrix)
  }

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




