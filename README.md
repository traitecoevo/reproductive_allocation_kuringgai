# Studies on Reproductive Allocation for 14 plant species in Kuringgai National Park, Australia

This repository contains code needed to reproduce the figures in the following articles:

**Wenk, EH, Abramowicz, K, Westoby, M, Falster, DS**. Investment in reproduction for 14 iteroparous perennials is large and associated with other life‐history and functional traits. J Ecol. 2018; 106: 1338– 1348. [DOI](https://doi.org/10.1111/1365-2745.12974)

**Wenk EH, Abramowicz K, Falster DS & Westoby M** (2024) Is allocation among reproductive tissues coordinated with seed size? Oikos, in press.

The materials in this repository have also been archived on Zenodo:

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1183415.svg)](https://doi.org/10.5281/zenodo.1183415)


All analyses were done in `R`. All code needed to reproduce the submitted products is included in this repository. To ensure long-term [computational reproducibility](https://www.britishecologicalsociety.org/wp-content/uploads/2017/12/guide-to-reproducible-code.pdf) of this work, we have created a [Docker](http://dockerhub.com) image to enable others to reproduce these results on their local machines using the same software and versions we used to conduct the original analysis. Instructions for reproducing this work using the docker image are available at the bottom of the page. 

## Material included in the repository include:

- `data/`: directory containing raw data. For each file, there is a corresponding meta-data file, listing the variable names, definitions, and units.
- `ms/`: directory containing materials specific for manuscripts
- `R/`: directory containing functions to reproduce manuscript
- `export/`: intermediate outputs from analysis
- `output/`: compiled datasets
- `DESCRIPTION`: A machine-readable compendium file containing key metadata and dependencies 
- `LICENSE`: License for the materials
- `docker/Dockerfile` files used to generate docker container for long-term reproducibility

## Reproducing results


If you are reproducing these results on your own machine, first download the code and then install the required packages listed in the `DESCRIPTION` file. This can be achieved by opening the Rstudio project and running:

```{r}
#install.packages("remotes")
remotes::install_deps()
```

Next you need to download this repository and then open an R session with working directory set to the root of the project.

**Creating the dataset**

An export of the assembled data can be obtained by running the code in `remake raw data.R`, which will save a combined set of variables and metadata into the folder `export`.

Those wishing to reuse our data should start with these summary files, before delving into the raw-data.

**Results from the 2018 paper**:

To build the figures reported in the main text see materails in  `ms/RA`.

**Results from the 2024 paper**:

To build the figures reported in the main text see materails in  `ms/Accessory`. 


## Running via Docker

If you have Docker installed, you can recreate the computing environment as follows in the terminal. 

Fetch the container:

```
docker pull traitecoevo/reproductive_allocation_kuringgai
```

Navigate to the downloaded repo, then launch the container using the following code (it will map your current working directory inside the docker container): 

```
docker run --platform linux/amd64 --user root --volume="$(pwd):/home/rstudio/" -p 8787:8787 -e DISABLE_AUTH=true traitecoevo/reproductive_allocation_kuringgai
```

The code above initialises a docker container, which runs an RStudio session accessed by pointing your browser to [localhost:8787](http://localhost:8787). For more instructions on running docker, see the info from [rocker](https://hub.docker.com/r/rocker/rstudio).

**Building the docker image**

For posterity, the docker image was built off [`rocker/verse:4.4.2` container](https://hub.docker.com/r/rocker/verse) via the following command, in a terminal contained within the downloaded repo:

```
docker build --platform linux/amd64 -t traitecoevo/reproductive_allocation_kuringgai .
docker login --username XXXX
docker push traitecoevo/reproductive_allocation_kuringgai
```

and was then pushed to [dockerhub](https://hub.docker.com/repository/docker/traitecoevo/reproductive_allocation_kuringgai/general).
