### info_help.R --
###
### Help strings for various bits and pieces of the LACE GUI.
###
### See file LICENSE in the main folder for licensing and copyright
### information.

text = list()


text[["pr_tab_help"]] =
    list("Create a project where the inferential, the longitudinal tree and the
configuration for data filtering can be stored and reloaded. If there
is already a project with the same path, it is automatically
recognized and loaded upon request. Use the sidebar to open recent
projects, or save and reload tab specific configurations.",
      br(),
      br(),
      "In the sidebar you can also choose one of the 'demo projects' to load
some pre-computed data and see the results.")


text[["m_tab_help"]] =
    list("Information regarding the longitudinal single cell experiment. At a
minimum, information about the cells' identifiers and the sample they
belong to is required.  The identifiers are used to identify the file
names to include in the anlysis and the samplings represent the time
points. The cells used in the downstream analysis are the intersection
between the identifiers present in the metadata and the BAM files
stored in the drive.",
         br(),
         br(),
         "You may have to create such a file from scratch. See the (i) button
below.",
         br(),
         br(),
         "If you have loaded one of the 'demo projects', changes on this tab are
not considered.")

text[["av_tab_help"]] =
    list("Annotation of single nucleotide variants for each cell require
Annovar, a database of variant annotations provided by or downloadable
at", tags$a("https://annovar.openbioinformatics.org/en/latest/user-guide/download/"),", and the respective VCF files. If the database of
variant annotations is not suitable for the experiment under analysis,
follow the instructions from Annovar to generate your own
database.",
         br(),
         br(),
         "If you have loaded one of the 'demo projects', changes on this tab are
not considered.")

text[["thr_tab_help"]] =
    list("Single nucleotide variants are not all the same. Some of them are
characterized by low quality score, and mutations regarding exonic
regions may cause specific functional effect on the resulting
proteins. You can use the alternative frequency, minor allele
frequency and the minimum number of cells with the same mutation to
select the SNVs. Choose the functional exonic SNVs to considers e.g.,
if the case, unknown SNVs could be neglected, and reduce the number of
SNPs in the next steps.",
         br(),
         br(),
         "If you have loaded one of the 'demo projects', changes on this tab are
not considered.")

text[["dp_tab_help"]] =
    list("The number of reads per SNV site represents an optimal filter to
retrieve relevant mutations. Depth at specific sites is usually not
provided in standard alignment or variant calling pipelines and are
computationally expensive. Provide the folder with aligned data and,
if not found already, the samtools executable location to compute
depth only on sites passing the filters set in the previous tab.",
         br(),
         br(),
         "If you have loaded one of the 'demo projects', changes on this tab are
not considered.")

text[["va_tab_help"]] =
    list("Relevant variants allows to reproduce more significant longitudinal
clonal tree for the experiment. Apply some filters to retrieve
relevant variants as part of the processing step.",
         br(),
         br(),
         "Press 'Select variant' to do some pre-processing right now, and then,
interactively modify filters and see the results on selected variants
as they are changed. Otherwise, set all the filters and go next to
perform processing and inference all together.")


text[["inf_tab_help"]] =
    list("Set the preferences for the Monte Carlo Markov Chain (MCMC) used in
the inferential step. Add, at least, one false positive 'alpha' and
false negative 'beta' rate for each sampling time point. Multiple sets
of alpha and beta can be added by inserting values on the 'add row'
cells. If multiple rate sets are provided, then the best set of alpha
e beta are evaluated as part of the inference and returned.",
         br(),
         br(),
         "Press 'Run LACE' to save your current configuration, start processing
data and infer the longitudinal clonal tree. If the metadata, and
filters did not change, expensive variant and depth computations are
performed only on cells previously not included.")


text[["pr_name"]] =
    "Give a name to your project\'s experiment. All the results will be stored inside a subfolder with the same name."

text[["pr_folder"]] =
    "Select a folder to store the the project. Path containing project is recognized and saved state is reloaded."

text[["sc_metadata_file"]] =
    "Select file containing at least two columns, one with cell ids (such that id.vcf and id.bam identify the respective files) and the other with the sampling times (\'before drug\', etc.). Accepted file formats are csv and tsv, or rds containg a data.frame object. Headers are required. The list can contains more cells than what are stored in HD."

text[["m_idCol"]] =
    "Select the column containing cell ids. (such that id.vcf and id.bam identify the respective files)"

text[["m_timePointsCol"]] =
    "Select the column containing the sampling times. Sampling time tags can concide with the ids if the case."

text[["m_timePoints"]] =
    "Select and drug one or more sampling points to order them chronologically."

text[["m_doSave"]] =
    "Alpha release: Save state for the current tab."

text[["m_doLoad"]] =
    "Alpha release: Load state for the current tab. Inconguencies are reported (you messed with the config files)."

text[["av_exec"]] =
    "Alpha release: Run current part of the pipeline (in the last tab run the whole pipeline [alpha: did u test it before releasing it?])."

text[["av_anovar_exec_dir"]] =
    'Folder containing the Annovar executables. Annovar is available at this <a href="https://annovar.openbioinformatics.org/en/latest/user-guide/download/">link</a>.'

text[["av_anovar_db_dir"]] =
    "Annovar requires a database with annotations.
     Some are distributed with the software. Refer to the website guide to create your own database of annotated variants."

text[["av_vcf_in_dir"]] =
    "This is the folder where variant calling files are stored.
     File names need to have the format ID.vcf, where ID belongs to the metadata list, otherwise the file is neglected."

text[["thr_maf"]] =
    "Minor allele frequency (MAF) for each referenced SNP included in a default global population, ranging from <1 to <50%.
MAF is a ratio defined as
$MAF={Alleles positive for the variant}/{Total alleles screened}$ . When there are more than two alleles at a variant location, the second most frequent is used to calculate MAF."

text[["thr_alleles_ratio"]] =
    "Alternate allele frequency is the ratio of the obseved alterative allele and total obervations at that locus in a cell."

text[["thr_freq"]] =
    "How frequent is a variant found in a sample. It represents the ratio of cells where the mutation was observed over the total number of cells at a specific sampling time."

text[["thr_bucket_var_list"]] =
    tags$ul(
      tags$li("frameshift insertion: an insertion of one or more nucleotides that cause frameshift changes in protein coding sequence"), 
      tags$li("frameshift deletion: a deletion of one or more nucleotides that cause frameshift changes in protein coding sequence"), 
      tags$li("frameshift block substitution: a block substitution of one or more nucleotides that cause frameshift changes in protein coding sequence"),
      tags$li("stopgain:  a mutation that lead to the immediate creation of stop codon at the variant site"), 
      tags$li("stoploss: a mutation that lead to the immediate elimination of stop codon at the variant site"), 
      tags$li("nonframeshift insertion: an insertion of 3 or multiples of 3 nucleotides that do not cause frameshift changes in protein coding sequence"), 
      tags$li("nonframeshift deletion: a deletion of 3 or mutliples of 3 nucleotides that do not cause frameshift changes in protein coding sequence"),
      tags$li("nonframeshift block substitution: a block substitution of one or more nucleotides that do not cause frameshift changes in protein coding"),
      tags$li("nonsynonymous SNV: a single nucleotide change that cause an amino acid change"), 
      tags$li("synonymous SNV: a single nucleotide change that does not cause an amino acid change"), 
      tags$li("unknown:  unknown function (due to various errors in the gene structure definition in the database file)"))

text[["dp_samtools_exec_dir"]] =
    'Folder containing the Samtools suit executables. Samtools is available at this <a href="https://www.htslib.org/">link</a>.'

text[["dp_bam_dir"]] =
    "This is the folder where Binary Alignment Map files are stored.
File names need to have the format ID.bam, where ID is in the metadata list, otherwise the file is neglected."

text[["va_depth_minimum"]] =
    "Minimum depth to set values to NA."

text[["va_missing_values_max"]] =
    "Maximum number of considered missing data per gene."

text[["va_minumum_median_total"]] =
    "Minimum median depth for total reads."

text[["va_minumum_median_mutation"]] =
    "Minimum median depth for reads supporting mutations."

text[["va_verified_genes"]] =
    "Depending on the experiment, some genes may be more relevant than others.
The list of available genes changes based on the above filters.
Leave empty to consider all the vailable genes."

text[["inf_learning_rate"]] =
    "Parameter to tune the probability of accepting solutions at lower values during mcmc. Value of learning_rate = 1 (default), set a probability proportional to the difference in likelihood; values of learning_rate greater than 1 inclease the chance of accepting solutions at lower likelihood during mcmc while values lower than 1 decrease such probability."

text[["inf_alpha"]] =
    "False positive error rate provided as list of elements; if a vector of alpha (and beta) is provided, the inference is performed for multiple values and the solution at maximum-likelihood is returned."

text[["inf_beta"]] =
    "False negative error rate provided as list of elements; if a vector of beta (and alpha) is provided, the inference is performed for multiple values and the solution at maximum-likelihood is returned."

text[["inf_num_iter"]] =
    "Maximum number of mcmc steps to be performed during the inference."

text[["inf_num_rs"]] =
    "Number of restarts during mcmc inference."

text[["inf_n_try_bs"]] =
    "Number of steps without change in likelihood of best solution after which to stop the mcmc."

text[["inf_num_processes"]] =
    "Number of processes to be used during parallel execution. To execute in single process mode, this parameter needs to be set to either NA or NULL."

text[["inf_seed"]] =
    "Seed for reproducibility."

text[["inf_random_tree"]] =
    "Boolean. Shall I start MCMC search from a random tree? If FALSE (default) and initialization is NULL, search is started from a TRaIT tree (BMC Bioinformatics . 2019 Apr 25;20(1):210. doi: 10.1186/s12859-019-2795-4)."

text[["inf_marginalize"]] =
    "Boolean. If true, the attachment of cells to clones matrix is marginalized when computing likelihood."

text[["inf_keep_equivalent"]] =
    "Boolean. If true, results (the maximum likelihood longitudinal clonal tree, the attachment of cells to clones matrix) at equivalent likelihood with the best returned solution are returned."

text[["inf_check_indistinguishable"]] =
    "Boolean. If true, any indistinguishable event is removed from input data prior inference."

text[["inf_error_move"]] =
    "Boolean. If true, estimation of error rates in the MCMC moves is included."

text[["inf_show"]] =
    "Boolean. Show the interactive interface to explore the output."


### end of file -- info_help.R
