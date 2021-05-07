******
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
