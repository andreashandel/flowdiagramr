******
# 2021-06-02 and 2021-06-07 and 2021-06-10

* Should we try to write a manuscript describing the package?

* I'm currently confused about the use_varnames implementation. I thought in the latest discussion, this needed to happen in prepare_diagram, but looks like it's now in the make_diagram phase? Just need clarification on that.

* It seems to me it might be better to supply all settings for make_diagram as one list, this makes it easier to keep things together. To that end, I moved use_varnames and show_grid into diagram_settings. I hope this didn't break too much. I think we should also rename/reorder/add listelements such that it structures by type (vars/main flows/interactions/etc.) and for each one there is on/off and styling (the latter ignored if it's turned off).  Each type of flow (main and interaction) should have their own settings for an on/off flag to show arrows and labels, as well as text size/color settings. Also, let's reorder diagram_settings inputs such that the node related one comes first, then all main flow related, then all interaction related. 

* The quickstart vignette shows an error message "Error in prepare_diagram(model_list): flowdiagramr cannot currently process flows that include an interaction between more than two variables". Not doing more than 2 variables is a problem/limitation we might need to resolve. See e.g. the new 'more model examples' vignette where I tried to implement a model that is biologically reasonable, and ideally should work. Should discuss how difficult fixing this would be. (and first address the other points).

* I think we should try to avoid flipping between 'variable' and 'node', it might confuse people. While node is in some sense more general, since our starting point is models with variables and flows, I think it's best if we use those two words throughout. That means renaming all input options to make_diagram should be var_XX instead of node_XX. It would also mean renaming the nodes entry in the object that prepare_diagram returns to vars. Could do that in a last step, and then at beginning of make_diagram code, rename again internally to nodes so that you won't have to change a ton of internal code, but the user still sees var/vars/variable/variables instead of node terminology. This also means nodes (or edges) should not show up inside the ggplot2 code given to the user, just the vars/flows terminology. Maybe a search and replace over all code, changing node -> var and nodes -> vars will do the trick? And edge -> flow, edges -> flows. (I use a (windows) progam called Find and Replace for such stuff).

* Last model in vignette B doesn't look quite right, it seems that the d*P arrow goes from P to Ia instead of from P to nowhere (and to nowhere flows usually go at a slanted angle). Not sure what's happening there. Also several interaction arrows start at the center of the variables instead of the edges.

* In the return object from make_diagram, the naming of horizontal_edges and vertical_edges is not ideal. Users will think those are how they look graphically. Can we rename those data frames to say "vars, main_flows, interaction_flows, external_flows, feedback_flows". And would it maybe be useful for future development to have all flows have all attributes? E.g. all have the interaction attribute, it's either TRUE or FALSE. And all have a curvature setting, it's sometimes 0 for straight flows. Doing so one could even go from 5 data frames to 2, one for variables, one for flows, and there is an extra column for flows that specifies the type of flow. Not sure if that would be better than as list structure, but an option.  

* Vignette C still has comment from me that says "Need to be able to change label location for all flows. Not currently working." Is that now implemented? Seems like it is, just checking since the comment is still there.

* Can you format the help file for prepare_diagram such that the model_list explanation is easier readable, e.g., each list element explained by itself.

* The 2nd example in vignette D is not working. see comment there.

* Shouldn't default settings in a function call be what's described? E.g. right now in write_diagram, the default for directory is NULL but that equates to the current working directory. Shouldn't the default then be set to '.' or something like that? Same for filename, if the default is 'diagram_code.R' then I think that's what should be shown in the function call (I did that one, wasn't sure how to do directory best).

* I still think producing ggplot2 code that uses loops would be easier for users to modify. Instead of efficiently/vectorized adding of components (vars/flows), I would do a loop over each, i.e. a loop over all vars, over all flows, etc. Then one can more easily intervene manually for a specific one and change it.

* Need a function that checks that all model_list conventions are adhered to (see vignette A). (might already exist?)

******
# 2021-05-11


* More comments in the created ggplot code would be good. More or less every line/bit of code should have a brief explanation so user knows what it is/does.

* Using the apply functions is good coding style, but in my experience most novice coders are entirely confused by them. Would it be possible to write the output code without using the lapply functions? If an inefficient (but easier to understand) loop replaces it, that would be ok with me. I think a loop would also make it easier for a user to modify a specific component, e.g. if there is a loop over horizontal edges 1-5, a user could stick in something like (pseudocode) `if n==3 edge_color = orange` into the loop. Right now, it's hard to do that inside the lapply statement.

* Check documentation/help for all exported functions, make sure it's fully up-to-date and complete. Also have examples for each user-facing/exported function.

* Document/briefly describe all functions (both exported and internal) in documentation.md inside docsfordevelopers. Big picture, i.e. what function does and how it's called is enough. More detailed explanations should be in each function. Basically anything a new person working on this package needs to know to quickly pick up on things.

* Reviewer for useR suggested to put package on CRAN, which I think we should do before the conference, and asked how our package was different to others, e.g. DiagrammeR. We should maybe add a vignette that addresses how our package integrates with and is different from others. I started a shell/draft. Drop any other package names or blurbs in there if you can think of any. Right now just a dumping place, we'll write this fully at some point.

******
# 2021-05-10

* Ok with idea to move use_varnames as a setting for prepare_diagram so box size can be adjusted. If user doesn't provide varnames but wants to use them, produce error or warning message (see next point). Update help files and vignettes accordingly.

* I set 'use_varnames' to TRUE on a model that didn't have them specified (by accident). What happens is that nothing shows. Behavior should be that if no varnames argument exists, the use_varnames should be ignored and the labels plotted, maybe with a notification message to the user.

* Further update of prepare_diagram help. Specifically streamline Value section so curved_edge entries are not repeat of horizontal_edge. Also make sure everything else is up-to-date and full explained in help file/header. Question: for labels on flows (i.e. labelx and labely) do those specify the start or mid-point of the text? I suggest we do start (e.g. lower left corner of text), seems easier for a user to visualize shifting around. In any case, should be specified. And why does curved_edges have labelx and labely and the other edges don't? Seems like something they should all have. One should be able to change label location for any label.

* I tried the with_grid option in make_diagram, but it doesn't seem to work. I think having the ability to show a grid/coordinate system would be very helpful during the manual adjustment stages, so let's have that option. Also, I noticed the y coordinates are at times negative. Wouldn't it be more conventional to have the coordinate 0/0 point in the bottom left corner of the diagram and all x/y values are then positive? But if that would be a major recoding, then I'm ok with having the 0/0 point wherever it is (I actually don't know right now without seeing a coord system where exactly that point is).

* Why do the nodes in diagram_list have x and y values, shouldn't xmin/xmax/ymin/ymax specify everything? Seems a bit confusing. Also, if there is a row entry for $nodes, should there be a column entry too? And is that a quantity the user could/should edit? I think we should in the help file specify which ones the user could touch, and which ones they should leave alone (for all entries in the diagram_list structure). 


~All of these are related to my new example I added to the quick-start vignette, see there:~

* Currently, combining flow terms doesn't work. I'm ok for now forcing the user to write them explicitly one by one. One could consider adding parsing logic that can take e.g. S1*(b11*I2 + b12*I2) and parses out the 2 terms. But low priority/not now.

* prepare_diagram produces confusing error messages. Can we add some more logic checks to make sure models that are supplied are proper? Some should already be implemented in check_model in modelbuilder (and are definitely needed there), so we might want to use only a single function for this for both packages. Since we decided to make this package a dependence of modelbuilder, we can move the full check_model logic/function into here (even checks that are not needed for the purpose of drawing, but might be needed for the purpose of running).
See the latest example in vignette A.

* The resulting diagram is one long line of 7 compartments. Maybe we should have some simple logic that puts at most 4-5 compartments on a row, and then starts a 2nd row? Not sure if worth it since users will have to arrange anyway, but a thought. 

* Another idea: If one could easily add some logic that tries to see if there is some stratification and then places each stratum on a separate row. Might only work if user uses e.g. S1,I1,S2,I2 or Sa1,Sa2, etc. Otherwise figuring out things seems too tricky. I'm not sure if either this or the previous point are worth it given that for those models the user will likely have to place things anyway.

* I also added the new example as example 3 in the 'basic modifications'. Plot is kind of a mess :) I guess the arrow logic needs to take into account the manual placement that a user supplies.

* Let's make sure we call the object that's returned from prepare_diagram and sent into make_diagram the diagram_list everywhere (with a note somewhere that it can have any name). This will make it easier to refer to it in the help files, the vignettes, etc. If you see any place where it's called something else, replace. I do call it sir_diagram_list at some point, I think that's ok so users can see it can be any name, but it's still clear that this is the diagram_list object.

* If you can think of any use-cases or alternate models that should be shown in the first 3 vignettes, please add.

## COMPLETED

* I tried the with_grid option in make_diagram, but it doesn't seem to work. I think having the ability to show a grid/coordinate system would be very helpful during the manual adjustment stages, so let's have that option. Also, I noticed the y coordinates are at times negative. Wouldn't it be more conventional to have the coordinate 0/0 point in the bottom left corner of the diagram and all x/y values are then positive? But if that would be a major recoding, then I'm ok with having the 0/0 point wherever it is (I actually don't know right now without seeing a coord system where exactly that point is).
    + with_grid option got accidentally shunted into the aesthetics list; is working now as a stand alone argument in make_diagram.
    + The first node is now centered on 0,0. So the ymin will be negative. It is easiest to have the first node be the origin. But we can probably move the origin to the bottom right by shifting the entire diagram before exporting the data frames. **Take a look at the diagram with_grid now and see what you think.**



# 2021-04-30

## High

- Add the use_varnames = TRUE setting described in the 'modify diagrams' vignette. This should replace the box labels with their full names. This might mean the boxes need to be sized in a way that ensures the text fits into them. 
    + This is implemented but it is tricky to get the boxes the correction size. This is mostly because of the current workflow: locations and dimensions of the node boxes are specified in the `prepare_diagram` function, which happens *before* the package knows whether the user is going to specify the `use_varnames` argument as TRUE. One workaround is to have the `use_varnames` argument in `prepare_diagram` rather than in `make_diagram`. Then I could implement something that expands the x and y limits of the rectangles based on character number and default size.
    + Current workaround in the B vignette (not ideal, but works) is to change the `node_text_size` argument in the `diagram_settings` list so it all fits.

## Low

- There can never be too much documentation/comments :)

- General thought: There seems to be some overlap between functionality (e.g. processing flows, etc.) done in flowdiagramr and done in modelbuilder. Since I want to use the flowdiagramr functionality in modelbuilder, I think we'll make modelbuilder depend on flowdiagramr. That means we could think about structuring the 2 packages such that certain helper functions live inside flowdiagramr and are used there and also in modelbuilder. E.g. add_plus_signs is likely one. Or get_vars_pars. I'm not sure how to best go about starting that integration, but we should discuss.

- Write unit tests with testthat

- Implement error checking inside functions

- Add code to check size of content for box and make box size properly. Also content to minimize label and arrow overlap?

- Add an option to give each flow their own name/label (e.g. replace/add to b*S*I by calling "infection process"). Similar to flowlabels and flownames for boxes.

- Explain limitations on naming of variables and parameters in vignette and when describing input structure for prepare_diagram()

- Maybe be flexible with input structure. E.g. if varlabels/varnames/flows are provided in a different order, still ok? And if they are not named, can we instead try to see if the list has 2 or 3 entries, and try to interpret the 1st as varlabels, 2nd as varnames, 3rd as flows? Not that crucial for now, but we could decide what we allow as input structure. Just need to make sure we fully document this, both in the function help file and the vignette.

******
## COMPLETED

- Adjust code such that the last example in the 'modify diagrams' vignette looks good (will also apply to example in 'other diagrams' vignette). That means some logic that tries to draw flow arrows between boxes from their closest sides, instead of always leaving on right, entering on left.

- getting the default values for make_diagram by pulling them from get_diagram_settings_default is I think not ideal. I can easily see myself changing the defaults in make_diagram and forgetting to do it in that other R script. Could you instead pull the defaults using something like args(make_diagram) or formals(make_diagram) and getting rid of get_diagraim_settings_default altogether?

- I think we should maybe change the default settings for make_diagram such that the default already looks rather nice. E.g. settings that are more similar to the nice_diagram example on the 'modify diagrams' vignette.

- For the above, when adding the variable text, if it is more than one word (.e.g. 'Asymptomatic Recovered'), could we implement a centered two-line placement of the text? Maybe this is more easily done as part of the manual intervention approach by either modifying the input list or directly the code? 

- When writing string/regex code, if possible explain for each regex what it means/does (since I'm terrible at regex). E.g. like so:  

```pattern = "(\\+|-).*"  #find first plus or minus sign in string```
```replacement = "\\1"  #the first occurrence of the pattern```
```si <- gsub(pattern = pattern, replacement = replacement, fl) #pull out first + or - sign from string```

- Add check_model() and check_input_structure() helper functions
    + This is `check_model_list.R`
    
- If possible/code doesn't get too confusing, use explicit package::function() syntax in code, makes it easier to see in which package a function lives.
    + I didn't see too many instances of this. We mostly call base R functions or interal functions. We can add `flowdiagramr::function()` to all internal functions if you want, though.
    
- At start of prepare_diagram function, the input should be checked to make sure it looks as needed. If not, a meaningful error message should be given to user.
    + This is `check_model_list.R`
