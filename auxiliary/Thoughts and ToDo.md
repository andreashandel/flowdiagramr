2022-01-10 Andreas Notes

***

* New idea: Have prepare_diagram return a data frame that has columns for all possible settings (e.g. also things like `var_fill_color`, etc.). Have a new helper function called `update_diagram` (or such) that takes the variable and flows dataframes returned by prepare_diagram and updates settings. Alternatively, user can manipulate specific elements "by hand" by directly replacing entries in data frame. As such the 'update' function is just a convenience function.

With that change, the make_diagram function does not do any modifications anymore, it just turns the provided input, diagram_list, into ggplot code.

Basically:
diagram_list <- prepare_diagram(mymodel, mysettings)
diagram_list_new <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_new)

Questions/Thoughts: 

Does it make sense to send the full diagram_list into the update function, or should one have update functions for vars and flows separately. And should the input be the whole diagram_list (4 data frames) or just the part to be updated? 

Is it still useful/necessary to have prepare_diagram return the original mymodel/mysettings input? Or just the 2 prepared data frames?

The data frames returned from prepare_diagram will be big if we change as outlined above. is that a problem?

***

* Moved all but first vignettes into 'oldvignettes' to allow package to compile easier (still not working, code is still broken)

* Changed inputs to model list. This will impact everything.

* Should add ability to have more then 2 variables in a flows - is it actually 3 distinct variables that we can't handle (e.g. S*I*R or also something like S*I^2)?

* Should think through what is provided as output by prepare_diagram and what is not. One option is to provide every possible setting, including all that can be changed through make_diagram. Another option is to not provide any by default that can be updated with make_diagram, but allow user to add them. Or simply keep what make_diagram does completely away from prepare_diagram.

* Rewrote check_model_list function, but might not work, haven't fully tested/debugged yet.

* Working through prepare_diagram step by step and updating, so far done checks, currently on add_locations


11/4/ Notes from Andreas

* Used dplyr's bind_cols/bind_rows at some places due to better handling of cases when one data frame has no entries. Added dplyr to dependency.

* Removed aesthetics from the prepare_diagram outputs. If a user can provide settings to make_diagram in a vectorized format, it's the same as manipulating entries in the diagram_list for aesthetics. So we can simply only allow it in one place. Then diagram_list will only contain entries that can't be supplied as settings to make_diagram.

* Need to figure out how to best vectorize inputs to make_diagram.

* Changed prepare_diagram inputs for scaling/spacing. Updated function documentation to explain updates. Not yet fully implemented in code.

* All optional settings for prepare_diagram are now by default NULL, and if not user-supplied, generated at start of prepare_diagram function.

* Need to update set_curvature code (and any other place) to not use row information but work with only values in xmin/xmax/ymin/ymax. Also, need to avoid hard-coding any offsets/shifts and instead use them based on x/y-coordinates of the to/from boxes (or arrow dimensions).

* If there are more than 10 out/in/interaction flows, does the current numbering system work? E.g. is there flow 55512 or how's the labeling? Will code work? Sounds from comments in code right now that max is 9. If true, need to recode to remove any hard limit.

* Examples in prepare_diagram need updating to show all possible inputs


* Minor: I prefer a from-to order/logic everywhere, both in entry/column order and code.

* Minor: Should do order of variables first, then flows in all functions, etc.

* Minor: Unclear what interaction/direct_interaction/out_interaction are, can you clarify? E.g. add comment in code first time those are created/show up.

* Minor: Can we restructure prepare_diagram code as much as possible to first do all variable-related processing, then all flow-related?


9/5 thoughts

For vectorizing those values, the varbox_ ones should be straightforward, the entries should correspond to the number and ordering in varlabels.  

For vectorizing the varspace_ entry, it's a bit trickier how it applies. Let's say we had this for varlocations: 
c("S", "", "R",
  "", "I", "" )

Then we need to figure out what it means to supply varspace_. There is always 1 row or 1 column less of spacing. For the example above, we  have a 2 x 3 matrix for varlocations. This will give us a 2 x 2 matrix for varspace_x and a 1 x 3 for varspace_y. 
As example, we could have a these:
varspace_x_size = matrix(c(1  ,2,
                          0.5 ,3)) 
That would say adding a spacing of 1 between S and "" and empty space of 2 between "" and R for the top row, and 0.5 between "" and I and 3 between I and "".

And similar
varspace_y_size = matrix(c(1,2,0.5)) means a spacing of 1 between S and "" a spacing of 2 between "" and I and a spacing of 0.5 between R and "".

It seems to me that if we allow users to provide varbox_ as a single number or vector for each box, and varspace_ as a single number or matrix that's 1 row/column less than varlocations, we would be very general. 
As a note, we can't allow varspace_ matrices unless user provides varlocations. In the case where the user wants the default single row placement for boxes but still wants to adjust spacing between each, e.g. 0.5 between S and I and 1.5 between I and R, we might just need to force them to supply a 1xN matrix as varlocations. In this case, varspace_x would be 1 x (N-1) and varspace_y not allowed. (Same for a fully vertical diagram).

Main question: How difficult will this flexible framework make any adjustments downstream? Is it currently set up that once the boxes/variables are placed, the flows are placed automatically, so things should be easy? Or is this going to cause major problems?

Now I'm also wondering again where to place (0,0). Bottom left makes sense to me. But then we need to decide which order the spacing matrices go, do they start in the bottom left of the matrix, or the top left, which is sort of the usual way to read it (and how I wrote it above.) Or we place (0,0) to the top left. It's less consistent with x-y diagrams, but it's more consistent how humans read those matrices, we start in the top left and go bottom right. Thoughts?




9/2 updates

* Rewrote vignettes A and B. One issue might be box sizing, see ALL CAPS text in vignette B. Might need some discussion.




******
# Urgent/Next
******

My current thinking (up for discussion):

* By default, the first (or, if present, lower left variable in varlocations matrix) is placed on a grid with the lower left corner of that box (xmin/ymin) at (0,0). By default, boxes are of size 1, with both horizontal and vertical spacing between boxes size 2.

THIS IS DONE.

* model_settings in prepare_diagram will take these inputs: varlocations, varbox_x_scaling, varbox_y_scaling, varspace_x_scaling, varspace_y_scaling. The first 2 scale the box size by a factor along that direction, e.g. varbox_x_scaling = 1.5 makes each box of size 1.5. Might be easiest to just push the max value out by that amount? varspace does the same for the empty space between boxes, e.g. it scales the default value of 2 to create more/less spacing between boxes. These settings determine the location of all boxes. Once all boxes are 'placed', arrows will then be drawn between boxes based on box start/end settings.

THIS IS DONE. BUT SCALINGS ARE NOT VECTORIZED -- THEY CAN ONLY TAKE ONE VALUE AT THIS TIME.

* varnames, use_varnames, var_label_size will be removed from model_settings

DONE. I REMOVED USE_VARNAMES AND SIMPLY CHECK WHETHER VARNAMES ARE SUPPLIED AND ASSUME THE USER WANTS TO USE VARNAMES IF THEY PROVIDE THEM. THESE ARGUMENTS ARE NOW IN diagram_settings.

* Also, let's call the low/high values for boxes and arrows the same. right now it's xmin and xstart for boxes and arrows respectively. That leads to extra cognitive load by the user :) Just pick one labeling and use the same for both boxes and arrows. I prefer min/max since it doesn't indicate directionality, but I'm ok with either (or something else).

DONE.

* Rewrite prepare_diagram to implement those changes. Also, streamline/simplify code. Remove "legacy code", e.g. the sdf/vdf/cdf/etc. distinctions. Make code as streamlined/simple/documented as possible, so I can follow :) and thus maintain/update. That also means changing variable names to something consistent (instead of renaming at end, like we are doing for some currently.)

DONE. THOUGH THERE IS PROBABLY MORE REFACTORING THAT CAN BE IDENTIFIED DURING CODE REVIEW.


* make_diagram gets a new entry for diagram_settings called var_label_text. The user provides a vector of text to be printed into the boxes (e.g. the variable names, or anything else). If provided, this is used, otherwise the default is to use varlabels from the model_list. This basically reproduces the varnames/use_varnames functionality in a more flexible way. It also reduces confusion about specifying var_label_size twice.

DONE.

* If a user specifies one of the entries in diagram_settings, it overwrites whatever is in diagram_list. If a user wants to do more detailed adjustments, they need to edit diagram_list and leave that entry of diagram_settings empty. Maybe (if easy to do) if code detects a non-default setting in diagram_list AND styling for that entry in diagram_settings, it could issue a warning message.

I THINK THIS IS DONE. BUT WILL REQUIRE TESTING THE SPECIFIC USE CASE YOU HAVE IN MIND.

* Rewrite make_diagram to implement those changes. Also, as for prepare diagram, streamline/simplify code.

DONE. MOST OF THE CODE IS RECYCLING VALUES FOR AESTHETICS. WE CAN MOVE ALL THOSE TO EXTERNAL FUNCTION IF YOU WANT TO CLEAN UP THE FUNCTION MORE.


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




