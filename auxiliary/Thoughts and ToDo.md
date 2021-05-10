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
