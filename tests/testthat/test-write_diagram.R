context("test-write_diagram.R")

test_that("write_diagram function works",
          {

            variables <- c("S","I","R")
            varnames <- c("Susceptible","Infected","Recovered")  # optional
            flows <- list(S_flows = c("-b*S*I"),
                          I_flows = c("b*S*I","-g*I"),
                          R_flows = c("g*I"))
            varlocations <-  matrix(data = c("S", "", "R", "", "I", "" ),
                                    nrow = 2, ncol = 3, byrow = TRUE)
            model_list <- list(variables = variables,
                               flows = flows)
            diagram_list <- prepare_diagram(model_list = model_list)

            # generate R code from model_list
            write_diagram(model_list = model_list,
                          filename = "test_code1.R",
                          always_overwrite = TRUE)
            testthat::expect_true(fs::file_exists('./test_code1.R'))
            fs::file_delete('test_code1.R')

            # generate R code from diagram_list
            write_diagram(diagram_list = diagram_list,
                          filename = "test_code2.R",
                          always_overwrite = TRUE)
            testthat::expect_true(fs::file_exists('./test_code2.R'))
            fs::file_delete('test_code2.R')

            #' # generate R code from both
            write_diagram(model_list = model_list,
                          diagram_list = diagram_list,
                          filename = "test_code3.R",
                          always_overwrite = TRUE)
            testthat::expect_true(fs::file_exists('./test_code3.R'))
            fs::file_delete('test_code3.R')

          })


