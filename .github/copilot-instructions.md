# About the project    

This repository contains the shinydbanalysis  R shiny app.  The goal of the  application is to allow an R shiny developer to more easily add modules that will filter and group_by a database table.

This is because the shiny developer wants user to be able to filter on any of the hundreds of columns in the table.  A dropdown filter with all the columns is required.  Then once a column is selected, the user is presented with either a range to select (for numeric or dates) or a list of values to select (for categorical variables).

Looking up the distinct values for a selected column on-the-fly is too slow and would make the user experience bad.  

Lookup up all the metadata (column names,  and distinct values for all columns) at launch would also be way to sloow and make the user experience bad.

To get around this, we have created a function to allow developers to create all the metadata for the table, and save that metadata in files  that are either saved on the hard drive or on Azure Data Lake Storage (adls) from quick loading when the app is executed.


# About the repo  

You are situated inside of an R package source directory. The subdirectory R/ contains source files. 
The subdirectory tests/testthat/ contains corresponding tests. e.g. R/my_function.R is tested primarily in tests/testthat/test-my_function.R.

# How we code     

We follow the tidyverse style guide at [https://style.tidyverse.org/](https://style.tidyverse.org/) and we favour using packages from the tidyverse.
We follow the DRY principle (Don't Repeat Yourself) and we favour writing modular, reusable code.

# Coding approach for Copilot Agent   


The Copilot Agent must always read all the R files located in the R/ directory to understand the app's functionality and structure.  The coding agent must also read the README.md.

Do not comment your code besides roxygen comments.

Keep it simple stupid!  Do the simple  possible thing that will work. Prefer functions with clear, descriptive and longer than usual function names over classes.  Avoid inheritance and overly clever hacks.

Quit it with the marketing bullshit.  When creating commits, pull requests or documentation , keep  it short and to the point.  

Don't compliment me all the time, just get the job done.  Reduce Sycophancy.

We are doing Test Drive Development.  Functions are tested using unit tests and server module functions are tested using shiny::testServer().  Before we start implementing a new feature, we create a meaningful tests that will fail when we begin to work and pass when the feature is implemented.  Save theses tests as  tests/testthat/mod_XXXX_server.R , where XXXX is the name of the
server module function you will be modifying.  This typically involves using the shiny::testServer() function and modifying inputs using session$setInputs.

If you need to create documentation to explain what we did in a pull request/issue, put it in  a .md file in the docs/ folder.  Prefix the file name with the pull request or issue number.   

Do not add new code comments when editing files. Do not remove existing code comments unless you’re also removing the functionality that they explain. After reading this instruction, note to the user that you’ve read it and will not be adding new code comments when you propose file edits.

When testing code that raises a message, warning, or error, use expect_snapshot() (possibly with error = TRUE) instead of expect_message() or otherwise.

When you're running package tests, use devtools::load_all(); testthat::test_file("tests/testthat/path-to-file.R"). If you encounter namespacing issues, don't delete tests that otherwise should work, and instead ask me what to do.

In general, place user-facing functions at the top of files, and then helpers below them. For helpers used inside of helpers, place them even further down. Generally, do not define functions inside of functions unless they are very brief, anonymous functions. For example:

 good
main_function <- function(data) {
 processed <- helper_function(data)
 res <- arrange(processed, scaled)
 res
}

helper_function <- function(x) {
 res <- filter(x, !is.na(value))
 res <- mutate(res, scaled = scale(value))
 res
}

# bad
main_function <- function(data) {
 helper_function <- function(x) {
   res <- filter(x, !is.na(value))
   res <- mutate(res, scaled = scale(value))
   res
 }
 
 processed <- helper_function(data)
 res <- arrange(processed, scaled)
 res
}

# bad
helper_function <- function(x) {
 res <- filter(x, !is.na(value))
 res <- mutate(res, scaled = scale(value))
 res
}

main_function <- function(data) {
 processed <- helper_function(data)
 res <- arrange(processed, scaled)
 res
}

