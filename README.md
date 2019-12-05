# LACE
================

| Branch              | Stato CI      |
|---------------------|---------------|
| master | [![Build Status](https://travis-ci.org/danro9685/SparseSignatures.svg?branch=master)](https://travis-ci.org/danro9685/SparseSignatures) |
| development | [![Build Status](https://travis-ci.org/danro9685/SparseSignatures.svg?branch=development)](https://travis-ci.org/danro9685/SparseSignatures) |

Longitudinal Analysis of Cancer Evolution (LACE)

LACE is an algorithmic framework that processes single-cell somatic mutation profiles from cancer samples collected at different 
time points and in distinct experimental settings, to produce longitudinal models of cancer evolution. The approach solves a Boolean Matrix 
Factorization problem with phylogenetic constraints, by maximizing a weighed likelihood function computed on multiple time points. 
