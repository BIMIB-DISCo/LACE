# Longitudinal Analysis of Cancer Evolution (LACE)
================

| Branch              | Stato CI      |
|---------------------|---------------|
| master | [![Build Status](https://travis-ci.com/BIMIB-DISCo/LACE/builds/139504986)](https://travis-ci.com/BIMIB-DISCo/LACE/builds/139504986) |
| development | [![Build Status](https://travis-ci.com/BIMIB-DISCo/LACE/builds/139505042)](https://travis-ci.com/BIMIB-DISCo/LACE/builds/139505042) |

================

**OVERVIEW**

In this repository we provide an R implementation of *LACE* (link paper XXX). 

*LACE* is an algorithmic framework that processes single-cell somatic mutation profiles from cancer samples collected at different 
time points and in distinct experimental settings, to produce longitudinal models of cancer evolution. The approach solves a Boolean Matrix 
Factorization problem with phylogenetic constraints, by maximizing a weighed likelihood function computed on multiple time points. 

**RUNNING LACE R IMPLEMENTATION**

The R version of *LACE* can be installed from Github. To do so, we need to install the R packages *LACE* depends on and the devtools package. 

First we run an R session and we execute the following commands. 

```r
# run this commands only if the following R package is not already installed
install.packages("Rfast", dependencies = TRUE)
```

Now we can install and run *LACE* as follows: 

```r
# install CIMLR from Github
library("devtools")
install_github("BIMIB-DISCo/LACE", ref = 'master')

# load CIMLR library
library("LACE")
```

**DEBUG**

Please feel free to contact us if you have problems running our tool at daniele.ramazzotti1@gmail.com or d.maspero@campus.unimib.it. 
