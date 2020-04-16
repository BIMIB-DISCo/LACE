#' @name longitudinal_sc_variants
#' @title mutation data from Rambow, Florian, et al. "Toward minimal residual disease-directed therapy in melanoma." Cell 174.4 (2018): 843-855.
#' @description dataset is a binary matrix that reports presence (1) or absence (0) of somatic single nucleotide variants for each single cell (rows). SNVs are called from SMARTseq2 fastq files of melanoma cancer cells sequencing. 
#' Please refer to the Methods Section of Ramazzotti, Daniele, et al. "Longitudinal cancer evolution from single cells." bioRxiv (2020).
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
