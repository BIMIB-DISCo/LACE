LACE 2.0: an interactive R tool for the inference and visualization of longitudinal cancer evolution
============================================

[![Actions Status](https://github.com/BIMIB-DISCo/LACE/workflows/check-master/badge.svg)](https://github.com/BIMIB-DISCo/LACE/actions?query=workflow%3Acheck-master)
[![Actions Status](https://github.com/BIMIB-DISCo/LACE/workflows/check-development/badge.svg)](https://github.com/BIMIB-DISCo/LACE/actions?query=workflow%3Acheck-development)
<a href="http://bioconductor.org/packages/release/bioc/html/LACE.html#since"><img border="0" src="http://bioconductor.org/shields/years-in-bioc/LACE.svg" title="How long since the package was first in a released Bioconductor version (or is it in devel only)."/></a>

LACE 2.0 is a new release of the LACE R Bioconductor package. LACE 2.0 is capable of performing clonal evolution analyses for single-cell sequencing data including longitudinal experiments. LACE 2.0 allows to annotate variants and retrieve the relevant mutations interactively based on user-defined filtering criteria; it infers the maximum likelihood clonal tree, cell matrix attachment and false positive/negative rates using boolean matrix factorization. Furthermore, it generates the longitudinal clonal tree. Finally, LACE 2.0 allows to investigate cancer clonal evolution under different experimental conditions and the occurrence of single mutations which can be queried via *ensembl* database. 

A detailed vignette on LACE 2.0 is provided here: https://bimib-disco.github.io/LACE/articles/3_LACE_interface.html 

## CITATION

When using our tool, please cite: Ramazzotti, Daniele, et al. "LACE: Inference of cancer evolution models from longitudinal single-cell sequencing data." Journal of Computational Science 58 (2022): 101523. 

## Installation of LACE 2.0 R package

The package is available on GitHub and Bioconductor.
LACE 2.0 requires R > 4.1.0.

To install from Bioconductor, please run:
```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("LACE")
```

To install LACE 2.0 from GitHub run:
```
if (!require("devtools")) install.packages("devtools")
library("devtools")
install_github("BIMIB-DISCo/LACE", ref = "development", dependencies = TRUE)
```

LACE 2.0 uses *Annovar* and *Samtools suite* as back-ends for variant calling annotation and depth computation, respectively. Please refer to the next section to install them. 

## Installation of other required softwares 

*Annovar* is a widely used variant calling software freely available upon registration to their website at https://annovar.openbioinformatics.org/en/latest/.
The package contains *Perl* scripts and variant calling annotation reference databases for the human species. For other databases, please refer to their website.
If the scripts are installed in binary search path, then LACE 2.0 will detect them automatically. 

*Perl*  (https://www.perl.org/) is required to run *Annovar*. 

*Samtools suite* is a standard set of tools and libraries to handle SAM/BAM/BED file format and perform a variety of common operations on sequencing data. It is freely available at http://www.htslib.org/ and https://github.com/samtools/htslib. To install *Samtools* follow the instructions in their website. 


#### For *Windows* users, we suggest the following guidelines:

* Download *MSYS2* or *WSL*
* Download the *Samtools* source files from http://www.htslib.org/
* The field `db_home`, in the *Samtools* source file `etc/nsswitch.conf`, should be changed to `windows` such that:
    ```
        db_home: windows
    ```
        
* Install *MSYS2*/*WSL* (it is preferably to have *MSYS2* in the "C:" path), and install the packages required by Samtools as stated in the INSTALL documentation file within the Samtool source folder
* Inside a *MSYS2*/*WSL* shell, add the following directories to the variable PATH using the command: 
	```
        export PATH="/mingw64/bin/:/mingw64/:$PATH"
    ```
* From the above *MSYS2*/*WSL* shell, follow the Samtools documentation to build and install the software 
* Change the Windows `PATH` variable in the System variables and add the following paths:
	```
        C:\msys64\usr\bin 
        C:\msys64\usr 
        C:\msys64\mingw64\bin 
        C:\msys64\mingw64
    ```
* We remind that *Annovar* is a *Perl* script, and `.pl` files need be associated to *Perl* executable.   

Eventually, from the *Windows* command prompt, users should be able to start *Samtools* using the command  `samtools` and directly execute *Perl* scripts by calling their filenames.

## Run LACE 2.0

To start LACE 2.0 user interface run: 
```
library(LACE)
LACEview()
```
![Picture](https://github.com/BIMIB-DISCo/LACE/blob/master/vignettes/resources/Display_tab.png?raw=true)
