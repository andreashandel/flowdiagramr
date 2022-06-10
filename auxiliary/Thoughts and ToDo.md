****************************
Things to do at some later point (maybe)
****************************

* It seems that the default placing of flow labels is just above the horizontal arrow for single-row flows, but overlaps with arrow on multi-row diagrams (see e.g. example 6 in B and the same model, example 4 in C). Maybe shifting default placement of labels up a bit? Similarly, maybe default placement for external flows could be placed at tip of arrow to minimize overlap, see e.g., Ex 2b and 3 and 4 in B. 

* Maybe, since you need to do various checks for those inputs, put all input checks in a separate error check function (e.g. check_update_inputs() ) that is called at the beginning of update_diagram? **DONE. All checks still in the update_diagram function. They don't take up much space, but we can discuss best placement.**

* Not a focus now, but worth keeping in the back: Any tweaks the automatic/default placement of things that improve the default look are good. E.g. getting closer to the final figure in vignette C with reduced manual intervention.

* Arrows in example 2 of vignette F are poorly placed. Of course user can adjust. But wondering if there's still some tweaks/improvements one can make to the logic inside prepare_diagram to make automatic placement better? Might be worth scribbling down thoughts for future 
implementation.

* Example 4 in vignette C suggests that some of the default settings for flow placement could still be improved, e.g. trying to figure out which side of the box leads to the shortest distance to the target and placing the arrow there. Though this might not be easy, so maybe something "for later"? (Though if you end up fiddling with that part of the code anyway, see if there are quick fixes). Otherwise, for now we can ask users to manually move the arrows.


* Maybe worth considering moving some of the flowtester examples into vignette F, this way they get run on every package check and we can easily see if stuff fails in the future (and it gives users more examples). I can do that.

* Additional nice-to-have style options: var_outline_width setting to adjust thickness of box borders. var_shape that might allow a few different shapes, e.g. rectangle, circle, diamond. Though that might potentially be difficult with the connection points of the arrows? var_label_text_font, etc. to allow changing the font for the various text elements.



****************************
2022-06-09 Andreas notes

* I accidentally defined some model settings list with providing just varlocations and not naming it.
I wanted this
model_settings = list(varlocations=varlocations)
I did this:
model_settings = list(varlocations)
Calling prepare_diagram, it silently ignored varlocations. 
dag_list = prepare_diagram(dag_model, model_settings)
Should give an error message. **DONE.**

* I added another example to vignette E, one arrow placement is wrong, see vignette. **DONE**

* Would be good if each for loop in the ggplot code is labeled. Other further comments in that code wouldn't hurt either :)  **DONE**

* The goldilocks example in D looks good when run interactively but squished when knitted. I noticed that in general that figures look different when knitting versus printing. Same issue with the circle for P in that vignette. Do you know a way to get those closer aligned? (Might be some knitr setting, not sure).  **DONE**

* At this stage, I suggest we merge the develop over to main, clean up/delete any other branches, and just work off main. This way I can ask others to install from github and play with it.  **DONE!!!!**





****************************
2022-02-09 Andreas Notes

DONE. All added.
* Currently, data frames returned from prepare_diagram do not contain all columns/settings that can be changed. Add them or not?

DONE (I think). Still may be some mismatching to update.
* Need to make sure matching between column names in diagram_list data frames and inputs to update_diagram() is obvious. E.g. suggest to change color -> border_color (or outline_color) and fill -> fill_color in variable dataframe (and same names in update_diagram)

DONE.
* Not sure we want to have default values in update_diagram. Defaults should be spit out after prepare_diagram. Then in update_, only whatever the user provides is processed/updated, everything else remains untouched.

ADDED. DONE.
* Why are the logicals (show/hide) for arrows not part of update_?

DONE.
* Right now, update_diagram allows for nonsensical entries (e.g. linetype = "green"). Such nonsense can also arise if user manipulates data frames by hand. Thus, we might need a function at start of make_diagram that makes sure all columns in diag_list have the right names, and all entries are ok ones (e.g. anything that's numeric needs to be a number), color needs to be a color, etc. We could then also run that function during update_diagram to prevent nonsense (or ignore for update diagram and just do the check before making diagram).

DONE. But with a warniing.
* Maybe just throw an error if user doesn't provide proper diagram_list and diagram_settings entries for update_diagram()? So basically at least one entry to be updated in diagram_settings, and must match column/variable in diagram_list, otherwise fail.

DONE. GGPLOT CODE DIRECTLY IN FUNCTION
* It somehow seems "dangerous" to produce the plot in make_diagram by evaluating it with environmental variables. Are we sure that might not lead to some unforeseen bad consequences if user has an unexpected local environment? Not sure, maybe ok because environment is only what's inside the function?

DONE. GGPLOT CODE DIRECTLY IN FUNCTION
* Is there an advantage to having the get_code() function instead of just storing the ggplot skeleton code in a string variable, either inside make_diagram or somewhere else, and just loading it?

DONE. Should work now.
* Seems like extra model_settings in prepare diagram that determine size/spacing currently don't work?

DONE.
* make_diagram fails on a data frame that I think should work. see flowtester.R. Seems to be due to update_diagram() altering some column types from numeric to character.

DONE. But might still need some documentation/comments.
* Any further code simplification is good :) E.g. some of the helper functions one might be able to write simpler? And in general I think having the helper functions is good, but if it's only a 1-2 lines of code that isn't used too often, I think we can move into the calling function instead (e.g. without trying to think through it, it seems to me something like remove_na_rows could be done with some built-in function and maybe just 1 line of code? (but maybe not, i didn't try to work my way through the function)). Also, helper functions could use some more commenting/documentation. Sometimes hard to see what's going on without spending a good bit of time working through the code.

DONE. Except vignettes.
* Need to update documentation, examples, etc. for all functions to match with new setup.

DONE. (I think.)
* Once working, prepare_diagram function needs an example showing full use of model_settings, including vectorization.

***

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

* Working through prepare_diagram step by step and updating, so far done checks, currently on add_locations and starting looking at flow processing part.



11/4/ Notes from Andreas


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




******
# Important/Later
******

* Arrow placing for last plot in vignette F is poor. Might not be an easy fix.

* For CRAN submission, we want no errors/warning/notes. To prevent "Undefined global functions or variables", message, mostly caused by ggplot2 code, need to one of dplyr/rlang or utils::globalVariables. 
Right now I'm defining global variables in global.R. I still don't fully understand how 'clean' this approach is. In general, I think one should try to minimize global variables, though it's hard with ggplot and dplyr code. 
The rlang option described at link below can work. As you modify code, see if you can use that approach. And we should give all global variable distinct and unique names so that there isn't another variable with that name, which might lead to conflicts. Right now there are 'x' and 'y' as global variables (see globals.R) which is not ideal.
https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887


* Moved this from vignette A: **NEED TO CHECK THAT DESCRIPTION REGARDING MODEL_LIST SPECIFICATION IS 1) CORRECT, 2) AGREES WITH HELP CONTENT 3) CHECK_MODEL_LIST CHECKS VALIDITY OF ALL THAT.** As discussed, should be combined with modelbuilder to not duplicate checking code. 

* convert_from_modelbuilder function has been updated to provide the new output as list with elements model_list and model_settings. This will require adjustments of modelbuilder code when using flowdiagramr to show diagrams in modelbuilder.

* The quickstart vignette shows an error message "Error in prepare_diagram(model_list): flowdiagramr cannot currently process flows that include an interaction between more than two variables". Not doing more than 2 variables is a problem/limitation we might need to resolve. See e.g. the new 'more model examples' vignette where I tried to implement a model that is biologically reasonable, and ideally should work. Should discuss how difficult fixing this would be. (and first address the other points).

* Document/briefly describe all functions (both exported and internal) in documentation.md inside docsfordevelopers. Big picture, i.e. what function does and how it's called is enough. More detailed explanations should be in each function. Basically anything a new person working on this package needs to know to quickly pick up on things.





******
# Less Important/Later
******

* In general, if I were to manually label diagrams, I would try to place the text for interaction flows close to the tip of the arrow, i.e., where the interaction happens. E.g. in the basic SIR diagram, I would move it down to where the bSI arrow meets the S->I arrow. Not sure if placement could be changed to be generally close to interaction, and if that would produce worse looking diagrams?

* Would it be useful to include a `box_scaling` argument in the optional prepare_diagram list, which would scale all boxes by a factor (i.e. default is 1, if a user says 2 then boxes would be increased in size by a factor of 2.) That could allow for flexible adjustment of boxes. Not sure if good idea, we can discuss.  

* More comments in the created ggplot code would be good. More or less every line/bit of code should have a brief explanation so user knows what it is/does.

* Write unit tests with testthat

* Implement more error checking inside functions

* Currently, combining flow terms doesn't work. I'm ok for now forcing the user to write them explicitly one by one. One could consider adding parsing logic that can take e.g. S1*(b11*I2 + b12*I2) and parses out the 2 terms. But low priority/not now.

* Branched flows currently dont work (I think). E.g. -bSI leaving a compartment and fbSI arriving in one and (1-f)bSI in another are not allowed. Those flows need to be written explicitly currently as 2 independent flows both on inflow and outflow.

* #make sure each parameter name is only used in distinct flows, either once or twice in a inflow/outflow pair




******
# General
******

* Should we try to write a manuscript describing the package?

* There seems to be some overlap between functionality (e.g. processing flows, etc.) done in flowdiagramr and done in modelbuilder. Since I want to use the flowdiagramr functionality in modelbuilder, I think we'll make modelbuilder depend on flowdiagramr. That means we could think about structuring the 2 packages such that certain helper functions live inside flowdiagramr and are used there and also in modelbuilder. E.g. add_plus_signs is likely one. Or get_vars_pars. I'm not sure how to best go about starting that integration, but we should discuss.




