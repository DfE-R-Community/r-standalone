# Standalone R Functionality for the DfE

This repo houses self-contained R scripts which implement functionality commonly
used in DfE code. To import a standalone script into your own project, you can
simply copy/paste the code into your own repo, or alternatively (and preferably)
do so with {usethis}:

``` r
# Will import R/sql-connections.R
usethis::use_standalone("DfE-R-Community/r-standalone", "sql-connections")

# Will allow you to choose from available standalone scripts
usethis::use_standalone("DfE-R-Community/r-standalone")
```

NB, while this repo uses a package structure, it should not typically be
installed by users - the package structure is used to facilitate development,
e.g. unit testing using {testthat}.

## Contents

*  [`academic_year`](R/standalone-academic_year.R): Implements a {vctrs} S3 
   class for working with academic years
   
*  [`financial_year`](R/standalone-financial_year.R): Implements a {vctrs} S3 
   class for working with financial years
   
*  [`sql-connections`](R/standalone-sql-connections.R): Easy-to-edit helpers for
   connecting to databases and reading from SQL
   
*  [`with_datatable`](R/standalone-with_datatable.R): Implements a single
   function for translating {dplyr} code to use {data.table}, which is often
   quicker

## Why Use Standalone Scripts?
Standalone scripts have several benefits over code contained in R packages:

*  Once imported, standalone scripts can be easily modified to suit individual 
   requirements

*  Standalone scripts can be updated more frequently, possibly with breaking
   changes, without affecting existing users
   
*  Each standalone script can have its own set of dependency packages, so
   you only need to take on dependencies for the functionality you actually
   use
   

## Dependencies
Standalone scripts may consider the following packages to be 'free' dependencies:

*  [cli](https://github.com/r-lib/cli)
*  [glue](https://github.com/tidyverse/glue)
*  [rlang](https://github.com/r-lib/rlang)
*  [vctrs](https://github.com/r-lib/vctrs)
*  [withr](https://github.com/r-lib/withr)