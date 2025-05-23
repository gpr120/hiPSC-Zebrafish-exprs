# title

This repository contains code used for analyzing NGS data (from zebrafish embryos) and microarray data (from human iPS cells), as well as for generating figures related to our study entitled "Effects of Bisphenol A and Retinoic Acid Exposure on Neuron and Brain Formation: a Study in Human Induced Pluripotent Stem Cells and Zebrafish Embryos"

(Note: the title is subject to change).

## Dependency

- This program checked in ubuntu or Window subsystem for Linux 2 (ubuntu18.04).
- Check Dockerfile.
- R: check `renv.lock` for detail

## Setup

- Install R
- Donload or git clone this repository
- R dependency were managed by [Renv](https://rstudio.github.io/renv/articles/renv.html).
- Run script.

## Papers

- Title
  Name, Name
  paper

## Usage

- Functions to detect differentially expressed genes (DEGs) using rowFtests (genefilter).
  - `231014_Rit_update_exprs`

- A Shell script to calculate gene expression levels from FASTQ files．
  - `zebra/NGS_analysis_shell/*.sh`,
- Functions to detect differentially expressed genes (DEGs) using TCC (NGS).
  - `NGS_analysis_R/221227_rsem2TCC.R`

- Functions make expressionSet at Zebra to compere with iPS.
  - `NGS_analysis_R/make_expressionset_NGS.R`
    - depend: `NGS_analysis_R/221227_rsem2TCC.R`

- Functions used for plotting.
  - Figure 3.a-c: `microarray_analysis_R/230107_Rit_analysis_microarray`
  - Figure 7.a: `NGS_analysis_R/221227_ngs_PCA_1-3.R`
  - Figure 7.b-d: `NGS_analysis_R/230106_z-score_zebra.R`

## Citation

A BibTeX-formatted citation will be added upon acceptance of the manuscript.
