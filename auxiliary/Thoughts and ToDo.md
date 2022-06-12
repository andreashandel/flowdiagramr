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
2022-06-11 Andreas notes

* Went through old stuff, processed/cleaned up.

* I added several examples to the top of flowtester.R that currently don't work as they should.

* The weird thing when having double ** happened in the goldilocks example:
P_flows = c("k1*GL","-k2*P","-k3*P","-k4**P"), #AH: THIS LEADS TO A WEIRD PLOT
Note that this example got removed from vignette, I turned it into a blog post, see here:
https://www.andreashandel.com/posts/flowdiagramr-r-package/




****************************
2022-06-09 Andreas notes


* I added another example to vignette E, one arrow placement is wrong, see vignette. **DONE**

* Would be good if each for loop in the ggplot code is labeled. Other further comments in that code wouldn't hurt either :)  **DONE**

* The goldilocks example in D looks good when run interactively but squished when knitted. I noticed that in general that figures look different when knitting versus printing. Same issue with the circle for P in that vignette. Do you know a way to get those closer aligned? (Might be some knitr setting, not sure).  **DONE**






****************************
2022-02-09 Andreas Notes



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
=======
2021-08-22

* At some point, check repo and merge or delete any "orphaned" branches.
