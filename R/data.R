#' @name longitudinal_sc_variants
#' @title mutation data from Rambow, Florian, et al. "Toward minimal residual disease-directed therapy in melanoma." Cell 174.4 (2018): 843-855.
#' @description the dataset includes somatic single nucleotide variants at the single cell resolution. SNVs are called from SMARTseq2 fastq obtained from Gene Expression Omnibus database 
#' with the accession number: GSE116237. The dataset includes single cell data from a PDX melanoma model before and on treatment with BRAF and MEK inhibitors. The fastq files are processed 
#' to obtain the mutational profile following GATK best practice (https://gatkforums.broadinstitute.org/gatk/discussion/3891/calling-variants-in-rnaseq) usign the GRCh38 human genome as reference. 
#' Mutation data are stored in an N x M binary matrix with N single cells and M somatic single nucleotide variants. Row names report the ID of the fastq file related to a specific single cell; 
#' columns names report the SNV that are formatted as GeneName_chromosome_position_referenceAllele_alternateAllele. Each matrix entry can be 1 (mutation detected), 0 (mutation absent) or NA 
#' (too low coverage to determine the presence or absence of that mutation). For further details, please refer to the Methods Section and the section 3.1 of supplementary materials of 
#' Ramazzotti, Daniele, et al. "Longitudinal cancer evolution from single cells." bioRxiv (2020). 
#' @docType data
#' @usage data(longitudinal_sc_variants)
#' @format list of mutation data for four time points
#' @source Rambow, Florian, et al. "Toward minimal residual disease-directed therapy in melanoma." Cell 174.4 (2018): 843-855.
#' @return list of mutational data for a total of 475 single cells
NULL

#' @name inference
#' @title results obtained with the function LACE on the provided input data from Rambow, Florian, et al. "Toward minimal residual disease-directed therapy in melanoma." Cell 174.4 (2018): 843-855.
#' @description results obtained with the function LACE on the provided input data from Rambow, Florian, et al. "Toward minimal residual disease-directed therapy in melanoma." Cell 174.4 (2018): 843-855.
#' @docType data
#' @usage data(inference)
#' @format results obtained with the function LACE on the provided input data
#' @return results obtained with the function LACE on the provided input data
NULL
