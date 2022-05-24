****************************
2022-05-24 Andreas notes

* At some point I accidentally set flow_line_size twice. Results weren't right, only one was evaluated. I suggest that update_diagram() checks that each setting matches one of the allowed ones, and none is accidentally supplied more than once. Otherwise produce error and stop.

* Some bits are not working, I made comments in vignette C, always in ALL CAPS with AH: in front.

* I updated update_diagram such that an empty setting returns names of elements. For some reason my line break with \n doesn't work in the output when returning names. Can you get that to work? To see what I mean, see vignette C.

* I realized that it's really hard for users to pick absolute values for changes to anything that has to do with locations or curvature, since they have no idea what an x value of say 3 actually means. They would have to look at the various xmin/xmax etc values inside the variables/flows data frames, which we want to avoid. My suggestion is to rewrite update_diagram such that any entries the user supplies are relative to the current value. So for instance var_xmin = (S = 0.1, I = -0.2) would shift the xmin of the S box by +0.1 and the I box by -0.2 This way the user can explore shifting things around on the plot without having to know the absolute numbers. Should be pretty quick to do in code, instead of replacing the old value with the new, it's just the old value plus the new. 

* Not urgent but maybe nice at some point: have a var_outline_width setting to adjust thickness of box borders? 

* Another option one could add would be var_shape that might allow a few different shapes, e.g. rectangle, circle, diamond. Though that might potentially be difficult with the connection points of the arrows? I say not now, just noting here as something one could consider doing.




****************************
2022-05-19 Andreas notes

* in vignette C, you write: "NOTE: THE ABOVE IS TRUE EXCEPT FOR THE LOCATION INFORMATION AND CURVATURE. THOSE MUST BE VECTORS EQUAL IN LENGTH TO THE NUMBER OF VARS OR FLOWS." does that mean if i wanted to change the xmin of one flow, i needed to enter the xmin for all of them, even the unchanged? seems a bit tedious and it would also require digging into the data frame to get the original locations. can we make it such that one can specify only the ones that should be changed? 

related to this, which would solve the above issue and make other bits easier too: i'm wondering if we should allow changing elements through naming them. specifically, i'm thinking we could create a unique and descriptive name for each variable and flow. for the variable, that's just the existing "name" column. for the flows, we could maybe redefine names so they become unique. E.g. a name that has a shorthand for the type of flow (m/i/e) and then the actual flow, without the special symbols. what i mean is e.g. m_gI, i_bSI, e_mR, etc. then a user could update as follows: 
update_diagram(diag_list, diagram_settings = list(var_label_color = c( S = "black", R = "green" ),
                                                  external_flow_linesize = c(e_mI = 1.5, e_mR = 0.7)
                                                  ) )  

If we did that, we could allow 2 options. Either a single unnamed value that is applied to every element in a group (e.g. var_label_color = "orange" or  external_flow_linesize = 2) or a vector of named values and those are matched and applied. if a user provides a name that doesn't match, there's a warning. then we could get rid of checking if the entries have the right length. This way a user can easily target any number of elements without needing to specify values for all, and also without guessing if the one they want to change might be entry 5 or 6.

We could additionally make it that a call to update_diagram with only diag_list an no diagram_list will print the names of the variables an flows to the screen. The user would often call that one first, then they know what to change and create the named vectors.

This change seems to not be too hard, but I'm not sure. Let me know what you think.



****************************
2022-05-10 Andreas notes

* **FIXED** Example 5 in vignette B isn't quite right. It seems the box sizes are not assigned according to the order of the elements in the variables vector, but some other order. Specifically, the I box has size 1/1 and the R box has size 1.5/1, but it should be the other way around.

* Example 6 in vignette B now looks I think like it should. It's not good yet, but arrangement is ok. It will get better with each vignette we discuss :)

* **DONE** I suggest we give each type of flow a different default line style. E.g. main/solid, interaction/dashed, external/dotted.

* Vignette A: For the pred/prey model with custom settings through update_diagram, is that external flow style actually shown as dotted? looks dashed to me, but hard to tell. *ATT*: I looked into this and it is in fact dotted. But you're right that it doesn't look very dotted. I googled R linetypes and the examples for dotted look like what we're seeing.

* **DONE** I think we should give users the option to update everything with update_diagram, including the xmin/xmax/curvature, etc. I think would be nice, this way they would rarely need to "hack" the data frames directly. Also see comments in vignette C for last 2 examples. *ATT*: I added a note vignette C about how these locations and curvature settings must be vectors. This is also in the documentation.

* **DONE** When arrow is turned off, label should also be turned off. Currently label is shown even if arrow is gone. See example 2 in vignette C. Alternatively, and maybe better, we should have an XX_flow_show_label setting, which allows one to turn on/off the label separate from the arrow. (I assume this could be achieved by supplying label text as empty string, but that might not be obvious to all users.)

* **DONE (but can still be updated as desired)** Can we order the columns of the diagram_list data frames logically, i.e. all the line_ settings together and all the label_ settings together, etc.

* **DONE.** I noticed some flows don't have a name. For instance the b*S*I process which leads S -> I has no name and no label. The no label makes sense since it is already labeled by the b*S*I interaction flow. But maybe we should still give the flow the same b*S*I name, so a user knows which part of the model that flow corresponds to? Would it cause a problem if flows had names that showed up more than once? As far as I understand, the "name" entry isn't really further used in make_diagram, is that right? 

* Example 4 in vignette C suggests that some of the default settings for flow placement could still be improved, e.g. trying to figure out which side of the box leads to the shortest distance to the target and placing the arrow there. Though this might not be easy, so maybe something "for later"? (Though if you end up fiddling with that part of the code anyway, see if there are quick fixes). Otherwise, for now we can ask users to manually move the arrows.



****************************

2022-05-06 Andreas notes

* Vignette A seems to work.

* Vignette B fails at line 123: diagram_list4 <- prepare_diagram(model_list = sirmodel2, model_settings = sirsettings2)
Several other problems like that with other exmamples in the vignette that fail, likely all the same error. I commented out all code chunks in that vignette that currently fail.
**Fixed. There was a missing logic chunk for right-to-left physical flows.**

* Vignette B last example layout does not look like it should. My guess is the empty slots are not treated quite right? But not sure what's going on, the diagram just looks overall strange :)
**Maybe fixed...? It looks different after I made a few changes to add_locations. But, it still looks super busy. We might need to go over this one together.**

* There's code in add_locations I don't understand, it doesn't seem quite right. Around line 61, see my comment.


* Flowtester.R around line 204, produces warning that should be checked. See also my comment starting with AH: That is likely related to the previous point. I also copied one of the examples from vignette B that failed into flowtester. Of course doesn't work there either ATM.
**Fixed.**

* I realized that this fails mymodel = list(variables, flows) since we require the entries to be named. Maybe a bit too strict? I recoded such that now we just expect mymodel to consist of a nested list with 2 elements, and we name it inside prepare_diagram, right after checking.



****************************
2022-03-24 Andreas notes

* DONE -- Is it possible/easy to reshuffle code in prepare_diagram.R such that first all variables are processed and the variables data frame completed, and then move on to flows? Seems just easier to follow along. Basically, move whatever code is needed to complete variable DF before (current) line 330. Could also consider to refactor things to have a make_variables_df() function that has all the parts for making the variables data frame. But only if easy and if it makes code more readable, so let's contemplate first if useful.


Quick recap of overall structure (so I don't forget). 

Main user-facing functions are:
diagram_list <- prepare_diagram(model_list, model_settings)
diagram_list_new <- update_diagram(diagram_list, diagram_settings)
diag <- make_diagram(diagram_list_new, with_grid)
write_diagram(diagram_list_new, "filename",...)

ATT: This function is not tested yet.
Helper-functions for users:
convert_from_modelbuilder()


YES.
All other .R files/functions are internal helpers.

****************************
2022-03-04 Andreas notes


* flow data frame right now has columns label, math and label_text. Are all 3 needed/used? How do they differ? **DONE. There is now label and label_text in both the variables and flows data frames for consitency.**

* Just to confirm: for flow data frame, the from/to columns are just so users can easier understand, but if they were to edit those, nothing would happen, only actual xmin/xmax, etc. values are used for making the diagram, correct? Should probably be mentioned in the help file which columns user could/should add and which ones they should leave alone. **DONE. Correct, and this information is in the details section of the help file for prepare_diagram.**

* flow data frame has columns color and size. not sure what they refer to. arrows? if yes, there is another arrow_size coming later. should maybe be first all columns related to arrows and naming as arrow_color, arrow_size, arrow_type, show_arrow, then all columns related to label? **DONE. This is the size of the line and color of line. These are update to be line_color and line_size.**

* general comment: sometimes hard for me to figure out what happens if code and help text are not in sync. would be good if after each round of modifications, you can check that roxygen header/help text agrees with the latest code. otherwise i'm at times lost and need to go fishing in the code to figure out what is actually the new correct way of using the functions. **DONE. All documentation update -- but there still may be errors to be caught.**

* Maybe a simpler way to add locations to variables? add_locations seems very complicated. Should be a way to multiply matrix with size/spacing vectors. Need to think through it. **DONE. Rewrote the add locations function.**

* Are these dummy compartments needed? It strikes me as rather complicated right now. **DONE. Dummy compartments gone and simpler, though more verbose, code used.**




2022-02-23 Andrew and Andreas notes


DONE.
* Add a final check function inside `make_diagram()`. Check for:
  - nonsense in update settings  -- DONE
  - column names are correct -- DONE
  - check values for basic conformity (chr, str, num, etc.)
    - DO WE WANT TO ALLOW CHR and NUM for some (like linetypes)? -- JUST CHR for now
  - add to end of `update_diagram()` -- DONE
  - add to beginning of `make_diagram()` -- DONE

DONE.
* Make interaction and external arrow lengths depend on the box size (e.g., go from center out at 45 degree angle until the "edge" of the box is found).



***
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




