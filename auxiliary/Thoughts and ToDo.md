****************************
Things to do at some later point (maybe)
****************************

* Add more tests to get code coverage close to 100% 

* Move relevant flowtester.R code snippets to test functions, delete rest

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
(Feedback on blog post welcome)

* I added a quick description of some helper functions to vignette G. See if there are any others worth mentioning. 

* Related to that, can we rename check_dataframes to check_diagram_list?

* Can you check that all functions/files in the R folder are currently still used. If not, move to auxiliary/oldcode

* I saw you had a couple functions not exported because they came from stackoverflow. I'm leaning toward still making the exported/public and just in header acknowledging the origin of the code. Thoughts on that?
