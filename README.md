# Studies on Reproductive Allocation for 14 plant species in Kuringgai National Park, Australia

This repository contains code needed to reproduce the figures in the following articles:

**Wenk EH, Abramowicz K, Westoby M, & Falster DS** (2018) Investment in reproduction for 14 iteroparous perennials is large and associated with other life-history and functional traits. Journal of Ecology 106: 1338-1348.

**Wenk EH, Abramowicz K, Falster DS & Westoby M** (2024) Is allocation among reproductive tissues coordinated with seed size? Oikos, in press.


[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1183415.svg)](https://doi.org/10.5281/zenodo.1183415)


## Instructions

All analyses were done in `R`. To compile the dataset, figures and supplementary material we use the [`remake`](https://github.com/richfitz/remake) package for `R`, by Rich FitzJohn. You can install `remake` using the `devtools` package:

```r
devtools::install_github("richfitz/remake", dependencies=TRUE)
```
(run `install.packages("devtools")` to install devtools if needed.)

The `remake` package also depends on `storr`, install it like this:
```r
devtools::install_github("richfitz/storr", dependencies=TRUE)
```

Next you need to download this repository and then open an R session with working directory set to the root of the project.

We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

If you wish to compile the pdfs for supplementary materials, this requires a reasonably complete LaTeX installation.

**Data set**:

Our analyses are based on a collection of raw data files, provided in the directory `data`. For each file, there is a corresponding meta-data file, listing the variable names, definitions, and units.

Assembly of the entire dataset requires about 1 hr of computation, because of the 

An export of the assembled data can be obtained by running:

```r
remake::make("export")
```
which will save a combined set of variables and metadata into the folder `export`.

Those wishing to reuse our data should start with these summary files, before delving into the raw-data.

**Results from the 2018 paper**:

To build the figures reported in the main text open a fresh R session and run:

```r
remake::make("ms-RA")
```

Figures will appear in the directory `ms/RA/figures/`.

To build the supplementary materials run:

```r
remake::make("ms/Accessory/Wenk-RA-SI.pdf")
```

**Results from the 2024 paper**:

Instructions for creating results for the 2024 paper are provided in the subfolder `ms/Accessory`. 

