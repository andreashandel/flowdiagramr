****************************
ToDo soon
****************************

* Add ggflowchart to 'other packages' vignette:
https://nrennie.github.io/ggflowchart/index.html

* Get the models in the 'models-to-test' folder to work. If suitable, add to vignette.

* Do all DSAIRM and DSAIDE models



****************************
General notes for things to do at some later point
****************************

* Add more tests to get code coverage close to 100% 

* Move relevant flowtester.R code snippets to test functions, delete rest

* It seems that the default placing of flow labels is just above the horizontal arrow for single-row flows, but overlaps with arrow on multi-row diagrams (see e.g. example 6 in B and the same model, example 4 in C). Maybe shifting default placement of labels up a bit? Similarly, maybe default placement for external flows could be placed at tip of arrow to minimize overlap, see e.g., Ex 2b and 3 and 4 in B. 

* Not a focus now, but worth keeping in the back: Any tweaks the automatic/default placement of things that improve the default look are good. E.g. getting closer to the final figure in vignette C with reduced manual intervention.

* Arrows in example 2 of vignette F are poorly placed. Of course user can adjust. But wondering if there's still some tweaks/improvements one can make to the logic inside prepare_diagram to make automatic placement better? Might be worth scribbling down thoughts for future 
implementation.

* Example 4 in vignette C suggests that some of the default settings for flow placement could still be improved, e.g. trying to figure out which side of the box leads to the shortest distance to the target and placing the arrow there. Though this might not be easy, so maybe something "for later"? (Though if you end up fiddling with that part of the code anyway, see if there are quick fixes). Otherwise, for now we can ask users to manually move the arrows.

* Maybe worth considering moving some of the flowtester examples into vignette F, this way they get run on every package check and we can easily see if stuff fails in the future (and it gives users more examples). I can do that.

* Additional nice-to-have style options: var_outline_width setting to adjust thickness of box borders. var_shape that might allow a few different shapes, e.g. rectangle, circle, diamond. Though that might potentially be difficult with the connection points of the arrows? var_label_text_font, etc. to allow changing the font for the various text elements. Different shapes would be quite nice since they are used in DAG to differentiate things. Should look into that soon(ish).



****************************
2022-08-24 Andreas notes
****************************
Copied this over from the other file.

Have an add_flow function, maybe like this:
diag_list <- add_flow(diaglist, from = "S", to = "I", type = "interaction", label = "bSI")
It would basically add a row to the flows dataframe of diag_list. User can then further manipulate/style with update_diagram()
Could be from = "I", to = "m_bUV", for example to connect to an arrow. Or one entry could be empty to indicate external flow.


****************************
2022-07-27 Andreas notes
****************************

* Implemented all DSAIRM models with flowdiagramr, see DSAIRM_diagram_generation.R script. Most currently do not work. Need to figure out how much we can enhance DSAIRM to get more working. Might not be able to get them all to work.

* Should we place the label for interaction flows close to the tip of that flow? Seems like that is where it might often look best.



****************************
2022-06-11 Andreas notes
****************************

* Went through old stuff, processed/cleaned up.

* I added a few examples to failing-examples.R that currently don't work as they should. Would be good to address those first since diagrams like that are frequent (and I'm trying to make them in flowdiagramr for a new release of DSAIRM which I need to get on CRAN in the next 1-2 weeks).

* I added several examples to the top of flowtester.R that currently don't work as they should.

* The weird thing when having double ** happened in the goldilocks example:
P_flows = c("k1*GL","-k2*P","-k3*P","-k4**P"), #AH: THIS LEADS TO A WEIRD PLOT
Note that this example got removed from vignette, I turned it into a blog post, see here:
https://www.andreashandel.com/posts/flowdiagramr-r-package/
(Feedback on blog post welcome)

* Can you check that all functions/files in the R folder are currently still used. If not, move to auxiliary/oldcode

* Vignette F the last example default layout doesn't look good. Any more simple tweaks you can think of one could apply to layout logical to make defaults look better? Only things that you can think of without spending an inordinate amount of time though.

* I added a quick description of some helper functions to vignette G. See if there are any others worth mentioning. 
* Related to that, can we rename check_dataframes to check_diagram_list?
* Also related to that, since we are telling users about the various check_ functions, maybe we should have a separate function for check_diagram_settings() (the checks done at the beginning of update_diagram) after all? 
This is from previous discussion: Maybe, since you need to do various checks for those inputs, put all input checks in a separate error check function (e.g. check_update_inputs() ) that is called at the beginning of update_diagram? **DONE. All checks still in the update_diagram function. They don't take up much space, but we can discuss best placement.**
This way, user can directly access functions that check each of the main inputs, model_list, model_settings, diagram_list, diagram_settings

* I saw you had a couple functions not exported because they came from stackoverflow. I'm leaning toward still making the exported/public and just in header acknowledging the origin of the code. Thoughts on that?

* Vignette G, set margins using ggplot plot.margin does for some reason not work. Not sure why. If you happen to know right away, fix/adjust. Otherwise I'll revisit and try to figure out what's going on.



