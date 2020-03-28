data("data")

context("LACE")

data("inference")
test_that("LACE produces correct output", {
    expect_equal(names(inference),c("B","C","corrected_genotypes","clones_prevalence","relative_likelihoods","joint_likelihood","clones_summary","equivalent_solutions","error_rates"))
})
