Sys.setenv("R_TESTS" = "")

library("testthat")
library("LACE")

test_check("LACE")
