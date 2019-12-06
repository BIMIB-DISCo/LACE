# Longitudinal Analysis of Cancer Evolution (LACE)
================

| Branch              | Stato CI      |
|---------------------|---------------|
| master | [![Build Status](https://travis-ci.com/BIMIB-DISCo/LACE/builds/139504986)](https://travis-ci.com/BIMIB-DISCo/LACE/builds/139504986) |
| development | [![Build Status](https://travis-ci.com/BIMIB-DISCo/LACE/builds/139505042)](https://travis-ci.com/BIMIB-DISCo/LACE/builds/139505042) |

================

**OVERVIEW**

In this repository we provide an R implementation of *LACE*. 

*LACE* is an algorithmic framework that processes single-cell somatic mutation profiles from cancer samples collected at different 
time points and in distinct experimental settings, to produce longitudinal models of cancer evolution. The approach solves a Boolean Matrix 
Factorization problem with phylogenetic constraints, by maximizing a weighed likelihood function computed on multiple time points. 

**INSTALLING LACE R IMPLEMENTATION**

The R version of *LACE* can be installed from Github. To do so, we need to install the R packages *LACE* depends on and the devtools package. 

First we run an R session and we execute the following commands. 

```r
# run this commands only if the following R packages are not already installed
install.packages("devtools", dependencies = TRUE)
install.packages("Rfast", dependencies = TRUE)
```

Now we can install and run *LACE* as follows: 

```r
# install CIMLR from Github
library("devtools")
install_github("BIMIB-DISCo/LACE", ref = "master")

# load LACE library
library("LACE")
```

**RUNNING LACE R IMPLEMENTATION**

We now present an example of longitudinal analysis of cancer evolution with *LACE* using single-cell data obtained from Sharma, A. et al. (2018). 

As a first step, we load data for cell line HN120 obtained from a primary oral squamous cell carcinoma. The data comprises point mutations for three 
time points: (1) initial sequencing, (2) right after therapy, showing potentially resistant clones and (3) therapy holiday. 

```r
library("LACE")
data(data_HN120Primary)
names(data_HN120Primary)

## [1] "T1_Sensitive" "T2_Resistant" "T3_Holiday"
```

We setup the main parameter in oder to perform the inference. First of all, as the three data proint may potentially provide sequencing for an unbalanced 
number of cells, we weight each time point proportionally to the sample sizes as follow. We refer to the paper for details. 

```r
lik_weights = c(0.338, 0.329, 0.333)
```

The second main parameter to be defined as input is represented by the false positive and false negative error rates, i.e., alpha and beta. We can specify a 
different rate per time point as a list of rates. When multiple set of rates are provided, *LACE* performs a grid search in order to estimate the best set of error rates. 

```r
alpha = list()
alpha[[1]] = c(0.01,0.01,0.02)
alpha[[2]] = c(0.05,0.05,0.10)
beta = list()
beta[[1]] = c(0.01,0.01,0.02)
beta[[2]] = c(0.10,0.10,0.10)
head(alpha)

## [[1]]
## [1] 0.01 0.01 0.02
##
## [[2]]
## [1] 0.05 0.05 0.10

head(beta)

## [[1]]
## [1] 0.01 0.01 0.02
##
## [[2]]
## [1] 0.1 0.1 0.1
```

We can now perform the inference as follow. 

```r
inference = LACE(D = data_HN120Primary, 
	lik_w = lik_weights, 
	alpha = alpha, 
	beta = beta, 
	num_rs = 5, 
	num_iter = 10, 
	n_try_bs = 5, 
	num_processes = NA, 
	seed = 12345, 
	verbose = FALSE)
```

We notice that the inference resulting on the command above should be considered only as an example; the parameters num_rs, num_iter and n_try_bs representing the number of 
steps perfomed during the inference are downscaled to reduce execution time. We refer to the Manual for discussion on default values. We provide within the package results 
of inferences performed with correct parameters as RData. 

```r
data(inference_HN120Primary)
head(names(inference_HN120Primary))

## [1] "B" "C" "clones_prevalence" 
## [4] "relative_likelihoods" "joint_likelihood" "clones_summary"
```

*LACE* returns a list of seven elements as results. Namely, B and C provide respectively the maximum likelihood longitudinal tree and cells attachments; clones_prevalence, 
the estimated prevalence of any observed clone; relative_likelihoods and joint_likelihood the estimated likelihoods for each time point and the weighted likelihood; clones_summary provide a summary of association of mutations to clones. Finally, error rates provide the best error rates (alpha and beta) as estimated by the grid search. 

**DEBUG**

Please feel free to contact us if you have problems running our tool at daniele.ramazzotti1@gmail.com or d.maspero@campus.unimib.it. 
