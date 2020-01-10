data("data")

context("LACE")

test_that("LACE produces correct output", {
    expect_equal(names(LACE(D=data,lik_w=c(0.2308772,0.2554386,0.2701754,0.2435088),alpha=list(c(0.10,0.05,0.05,0.05)),beta=list(c(0.10,0.05,0.05,0.05)),num_rs=5,num_iter=10,n_try_bs=5,num_processes=NA,seed=12345,verbose=FALSE)),c("B","C","clones_prevalence","relative_likelihoods","joint_likelihood","clones_summary","error_rates"))
})
