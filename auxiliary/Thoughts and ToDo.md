******
2021-04-05


High:

- AT: PP model not working (after removal of trailing + signs). SIR model in vignette B not working after removing trailing + from B*S*I. "Birth arrow" not showing in vignette B SIR.

- AT: warning msg for remove_na_rows.R on check needs fixing.

- AT: add content to 'documentation.md' 

- AT: make sure all code is documented well

- AT: add more details to each public function roxygen block, e.g. examples and more documentation/information.

- AT: implement write_diagram_code function

- AT: give nodes in input_structure object xstart/xend, ystart/yend to determine size of box

- AH: implement grid layout and changes for make_diagram outlined below (see vignette B)



Low:

- Write unit tests with testthat

- Use fs package for file/path operations (e.g. saving R code), seems better then base R

- Implement error checking inside functions

- Add code to check size of content for box and make box size properly. Also content to minimize label and arrow overlap?

- Add check_model() and check_input_structure() helper functions

- Add an option to give each flow their own name/label (e.g. replace/add to b*S*I by calling "infection process"). Similar to flowlabels and flownames for boxes.

Questions:

- What are current limitations on naming of variables and parameters? Need to explain somewhere, both in vignette and when describing input structure for prepare_diagram()





### For write_diagram:
write_diagram_code should take as input either a model list OR the input_structure object. 
if model list, then the function will include a call to prepare_diagram. for either scenario, the code that is being generated should be fully 'stand-alone'. that means the original model list or the input_structure object should be written at the top of the output code.

user can also provide optional location and filename for result. default path is current working directory. default filename is diagram_code.R

write_diagram(filepath = filepath, 
			  filename = filename, 
			  model = mymodel, #either
			  input_structure = input_structure, #or - if provided, ignore model
			  make_diagram_settings = list(node_text_color = "white", node_text_size = 10, use_varnames=) 
			  )

#not used since you provided your own input_structure
#prepare_diagram(mymodel)



### For prepare_diagram:
At start of function, the input should be checked to make sure it looks as needed. If not, a meaningful error message should be given to user. This checking might be best done in a separate function?

Maybe be flexible with input structure. E.g. if varlabels/varnames/flows are provided in a different order, still ok? And if they are not named, can we instead try to see if the list has 2 or 3 entries, and try to interpret the 1st as varlabels, 2nd as varnames, 3rd as flows? Not that crucial for now, but we could decide what we allow as input structure. Just need to make sure we fully document this, both in the function help file and the vignette.

Adjust code such that trailing + signs are not required (e.g. it's ok to write 'b*S*I' instead of '+b*S*I')


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
If both are true, it will be Susceptible, S
show_varlabels
show_varnames 




