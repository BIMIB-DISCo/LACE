LACE 2.0: an interactive R tool for the inference and visualization of longitudinal cancer evolution
============================================

[![Actions Status](https://github.com/BIMIB-DISCo/LACE/workflows/check-master/badge.svg)](https://github.com/BIMIB-DISCo/LACE/actions?query=workflow%3Acheck-master)
[![Actions Status](https://github.com/BIMIB-DISCo/LACE/workflows/check-development/badge.svg)](https://github.com/BIMIB-DISCo/LACE/actions?query=workflow%3Acheck-development)

LACE 2.0 is a new release of the LACE R Bioconductor package. LACE 2.0 is capable of performing clonal evolution analyses for single-cell sequencing data including longitudinal experiments. LACE 2.0 allows to annotate variants and retrieve the relevant mutations interactively based on user-defined filtering criteria; it infers the maximum likelihood clonal tree, cell matrix attachment and false positive/negative rates using boolean matrix factorization. Furthermore, it generates the longitudinal clonal tree. Finally, LACE 2.0 allows to investigate cancer clonal evolution under different experimental conditions and the occurrence of single mutations which can be queried via *ensembl* database. 

A detailed vignette on LACE 2.0 is provided here: https://bimib-disco.github.io/LACE/articles/3_LACE_interface.html 

## CITATION

When using our tool, please cite: Ramazzotti, Daniele, et al. "LACE: Inference of cancer evolution models from longitudinal single-cell sequencing data." Journal of Computational Science 58 (2022): 101523. 

## Installation of LACE 2.0 R package

The package is available on GitHub and Bioconductor.
LACE 2.0 requires R > 4.1.0 and Bioconductor.
To install Bioconductor run:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
```

To install LACE 2.0 run:
```
remotes::install_github("https://github.com/BIMIB-DISCo/LACE", dependencies = TRUE)
```

LACE 2.0 uses *Annovar* and *Samtools suite* as back-ends for variant calling annotation and depth computation, respectively. Please refer to the next section to install them. 

## Installation of other required softwares 

*Annovar* is a widely used variant calling software freely available upon registration to their website at https://annovar.openbioinformatics.org/en/latest/.
The package contains *Perl* scripts and variant calling annotation reference databases for the human species. For other databases, please refer to their website.
If the scripts are installed in binary search path, then LACE 2.0 will detect them automatically. 

*Perl*  (https://www.perl.org/) is required to run *Annovar*. 

*Samtools suite* is a standard set of tools and libraries to handle SAM/BAM/BED file format and perform a variety of common operations on sequencing data. It is freely available at http://www.htslib.org/ and https://github.com/samtools/htslib. To install *Samtools* follow the instructions in their website. 

## Run LACE 2.0

To start LACE 2.0 user interface run: 
```
library(LACE)
LACE2()
```
![Picture](https://github.com/BIMIB-DISCo/LACE/blob/master/vignettes/resources/Display_tab.png?raw=true)
