******
2021-04-19

- I tried 2 new models, see 'SIRP_diagram' and 'virus_diagram' code in the testthat folder. Both don't quite work. The main issue is the logic for some of the flows. For the SIRP model, the logic for the q*I term going into P is the problem. For the basic virus model, the b*U*V term is the problem.

To fix the q*I flow, I think we need to check all inflows for any containing variables, and any variables other than themselves, we should draw arrows from those variables. So, in this case, an arrow from I to P. And since q*I is arriving in P but not leaving I, we can guess that it's not a physical flow, but instead I is 'producing' P without 'becoming' P and labeling it as interaction (non-physical) flow.

For the b*U*V terms, similar. We parse inflows, we see that b*U*V arrives in I and has U and V in it, so we make two arrows from U and V to I, those should somehow 'meet' before or when they arrive at I since they are the same. Right now they do that, but there are too many flows.

Not sure what needs to change, but I think the underlying logic should be to look at all flows, for each flow see how often they show up, and if it's only an inflow, only an outflow, or shows up both as in and out. And what variables are inside the flow. Based on that, draw lines. But each flow should only be processed once, so even though b*U*V shows up 3 times in the model, it should be taken once, determined that it's an outflow for U and V and an inflow for I, and lines drawn accordingly. 

I have some of that kind of logic in modelbuilder, e.g. in the generate_stochastic function. There it needs to parse flows to build a transition matrix, is in some sense fairly similar to what we need here. Could take a look, though I'm not sure if taking that code and adjusting saves any time, might be easier to write from scratch.

- When writing string/regex code, if possible explain for each regex what it means/does (since I'm terrible at regex).
E.g. like so:  

pattern = "(\\+|-).*"  #find first plus or minus sign in string
replacement = "\\1"  #the first occurrence of the pattern
si <- gsub(pattern = pattern, replacement = replacement, fl) #pull out first + or - sign from string

And in general, there can never be too much documentation/comments :)

- getting the default values for make_diagram by pulling them from get_diagram_settings_default is I think not ideal. I can easily see myself changing the defaults in make_diagram and forgetting to do it in that other R script. Could you instead pull the defaults using something like args(make_diagram) or formals(make_diagram) and getting rid of get_diagraim_settings_default altogether?


- General thought: There seems to be some overlap between functionality (e.g. processing flows, etc.) done in flowdiagramr and done in modelbuilder. Since I want to use the flowdiagramr functionality in modelbuilder, I think we'll make modelbuilder depend on flowdiagramr. That means we could think about structuring the 2 packages such that certain helper functions live inside flowdiagramr and are used there and also in modelbuilder. E.g. add_plus_signs is likely one. Or get_vars_pars. I'm not sure how to best go about starting that integration, but we should discuss.




******
2021-04-09

- If possible/code doesn't get too confusing, use explicit package::function() syntax in code, makes it easier to see in which package a function lives.

- I decided that if I started messing with the code now, we might just confuse each other (I would change code and you would need time to figure out what I'm doing). So I think I'll let you keep coding for now, and once you are 'done' (i.e. current contract is over or we think version 1.0 of package is done), we'll switch roles, namely I'll then start coding and will ask you for input if I get stuck.

- ~~write_diagram: If user provides both the model_list and diagram_list, my suggestion is to re-print model_list at top, also have the prepare_diagram() call in the code, but uncomment it and add a note to it saying "Since a user-supplied diagram_list is provided, the default one created by prepare_diagram() is not used" or something like that. If idea not clear, let me know and I'll sketch up something in an example R script.~~

- ~~write_diagram: supplying make_diagram settings to the write_diagram function can be as list or as a named vector, whichever you think is the better option. We don't need to allow both, just pick the one we think is better and tell the user to specify it that way. It might be good to have it in a way that can be used for both make_diagram and write_diagram. Say a user defines some custom settings while playing with make_diagram, like this:~~

~~mysettings = c(interaction_arrow_size = 1.5, label_flows = FALSE, flow_text_size = 2)
diag = make_diagram(diagram_list, mysettings)~~

~~and then use that same structure if they want to get the code, like this:~~

~~write_diagram(diagram_list, make_diagram_settings = mysettings)~~

~~This means mysettings needs to be in a form that is accepted as input by both make_diagram and write_diagram. A named vector seems a bit simpler, but if we need the flexibility of a list, I think that's ok too.~~

*ATT: I used a list because we are mixing logicals, numerics, and characeters.*

~~- layout for prepare_diagram(). Take a look at what I wrote for vignette B (basic modification). See if you think that's a good approach. If yes, implement. If not, let's discuss more. That would replace the nodes_df input into prepare_diagram.~~

~~ok about layers terminology for ggplot.~~


******
2021-04-05


High:

- AT: add content to 'documentation.md' 

- AT: make sure all code is documented well

~~- AT: add more details to each public function roxygen block, e.g. examples and more documentation/information.~~~

~~- AT: implement write_diagram_code function~~

~~- AT: give nodes in input_structure object xstart/xend, ystart/yend to determine size of box~~

~~- AT: implement grid layout and changes for make_diagram outlined below (see vignette B)~~



Low:

- Write unit tests with testthat

~~- Use fs package for file/path operations (e.g. saving R code), seems better then base R~~

- Implement error checking inside functions

- Add code to check size of content for box and make box size properly. Also content to minimize label and arrow overlap?

- Add check_model() and check_input_structure() helper functions

- Add an option to give each flow their own name/label (e.g. replace/add to b*S*I by calling "infection process"). Similar to flowlabels and flownames for boxes.

Questions:

- What are current limitations on naming of variables and parameters? Need to explain somewhere, both in vignette and when describing input structure for prepare_diagram()





### For write_diagram:
~~write_diagram_code should take as input either a model list OR the input_structure object. 
if model list, then the function will include a call to prepare_diagram. for either scenario, the code that is being generated should be fully 'stand-alone'. that means the original model list or the input_structure object should be written at the top of the output code.~~

~~user can also provide optional location and filename for result. default path is current working directory. default filename is diagram_code.R~~

~~write_diagram(filepath = filepath, 
			  filename = filename, 
			  model = mymodel, #either
			  input_structure = input_structure, #or - if provided, ignore model
			  make_diagram_settings = list(node_text_color = "white", node_text_size = 10, use_varnames=) 
			  )~~

~~#not used since you provided your own input_structure
#prepare_diagram(mymodel)~~



### For prepare_diagram:
At start of function, the input should be checked to make sure it looks as needed. If not, a meaningful error message should be given to user. This checking might be best done in a separate function?

Maybe be flexible with input structure. E.g. if varlabels/varnames/flows are provided in a different order, still ok? And if they are not named, can we instead try to see if the list has 2 or 3 entries, and try to interpret the 1st as varlabels, 2nd as varnames, 3rd as flows? Not that crucial for now, but we could decide what we allow as input structure. Just need to make sure we fully document this, both in the function help file and the vignette.

~~Adjust code such that trailing + signs are not required (e.g. it's ok to write 'b*S*I' instead of '+b*S*I')~~


### For make_diagram:

Change inputs a bit:

Plot or don't plot these flows
show_mainflows
show_interactionflows
show_externalflows

Label or don't label these flows (if arrows are not plotted, will be ignored)
label_mainflows
label_interactionflows
label_externalflows

Show labels or names of compartments.
If both are true, it will be "Susceptible, S"
show_varlabels
show_varnames 




