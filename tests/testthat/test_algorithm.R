data("data_HN120Primary")

context("LACE")

test_that("LACE produces correct output", {
    expect_equal(names(LACE(D=data_HN120Primary,lik_w=c(0.338,0.329,0.333),alpha=list(c(0.01,0.01,0.02)),beta=list(c(0.01,0.01,0.02)),num_rs=5,num_iter=10,n_try_bs=5,num_processes=NA,seed=12345,verbose=FALSE)),c("B","C","clones_prevalence","relative_likelihoods","joint_likelihood","clones_summary","error_rates"))
})
