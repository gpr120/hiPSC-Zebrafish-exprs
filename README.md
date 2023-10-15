# xxx

This code is.

## Dependency

- This program checked in ubuntu or WSL2.
- Check Dockerfile.
- R: check `renv.lock` for detail

## Setup

- Install R, Python
- Donload or git clone this repository
- R dependency were managed by [Renv](https://rstudio.github.io/renv/articles/renv.html).
- Run script.

## Papers

- Title
  Name, Name
  paper

## Usage

- A Shell script to calculate gene expression levels from FASTQ files．
  - `zebra/NGS_analysis_shell/*.sh`,
- A function to detect differentially expressed genes (DEGs) using TCC.
  - `NGS_analysis_R/221227_rsem2TCC.R`
- Functions used for plotting.
  - Figure X: `NGS_analysis_R/221227_ngs_PCA_1-3.R`
  - Figure X: `NGS_analysis_R/230106_z-score_zebra.R`
  - Figure X: `microarray_analysis_R/230107_Rit_analysis_microarray`

- Functions make expressionSet at Zebra to compere with iPS.
  - `NGS_analysis_R/make_expressionset_NGS.R`
    - depend: `NGS_analysis_R/221227_rsem2TCC.R`

## Citation

bibtexフォーマットにて記載予定．
