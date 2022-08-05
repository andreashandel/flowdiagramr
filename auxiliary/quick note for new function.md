It's currently easy to remove a flow that is not right with 
update_diagram(flow_show_arrow = FALSE)  # make sure this removes label, too


It might be good to have a function that can manually add a flow that flowdiagramr wasn't able to do.

Maybe have this function:

diag_list <- add_flow(diaglist, from = "S", to = "I", type = "interaction", label = "bSI")

It would basically add a row to the flows dataframe of diag_list. User can then further manipulate/style with update_diagram()

Could be from = "I", to = "m_bUV", for example to connect to an arrow.
