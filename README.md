# Studies on Reproductive Allocation for 14 plant species in Kuringgai National Park, Australia

This repository contains code needed to reproduce the article:

**Wenk EH, Abramowicz K, Westoby M, & Falster DS** Coordinated shifts in allocation among reproductive tissues across 14 coexisting plant species. In review.

**Wenk EH, Abramowicz K, Westoby M, & Falster DS** How much do iteroparous perennials invest in reproduction and is this coordinated with other life-history and functional traits? In review.

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

Next you need to [download this repository](https://github.com/traitecoevo/reproductive_allocation_kuringgai/archive/master.zip), and then open an R session with working directory set to the root of the project.

We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

To build the dataset simply run:

```r
remake::make("export")
```

**First paper**:

To build the figures and rerun analyses reported in the main text open a fresh R session and run the commands in the file `Analysis-accessory.R`.

Figures will appear in the directory `ms/Accessory/figures/`.

To build the supplementary materials run:

```r
remake::make("ms/Accessory/Wenk-Accessory-SI.pdf")
```

**Second paper**:

To build the figures reported in the main text open a fresh R session and run:

```r
remake::make("ms-RA")
```

Figures will appear in the directory `ms/RA/figures/`.


If you wish to compile the pdfs for supplementary materials, this requires a reasonably complete LaTeX installation (e.g. [MacTeX](https://tug.org/mactex/) for OSX or [MikTex](http://miktex.org/) for windows). The LaTeX compilation will depend on a few packages from CTAN, make sure to allow automatic package installation by your LaTeX distribution.
