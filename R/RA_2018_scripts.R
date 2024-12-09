# superset of required libraries
library("stats")
library("stringr")
library("smatr")
library("plyr")
library("tidyr")
library("dplyr")
library("xtable")
library("igraph")
library("rmarkdown")
library("broom")
library("ggtern")
library("ggmap")
library("yaml")
library("mgcv")
library("knitr")
source("R/figures.R")
source("R/FlowerParts.R")
source("R/GraphRepresentation.R")
source("R/Growth.R")
source("R/Investment.R")
source("R/ms-accessory.R")
source("R/ms-RA.R")
source("R/ms-SuppMatt.R")
source("R/ReproductionTissues.R")
source("R/Species.R")
source("R/Summaries.R")
source("R/Utils.R")
source("R/WeightCalculations.R")


# figures for 2018 RA paper
pdf("ms/RA/figures/leaf_weight.pdf", width = 16L, height = 9L)
figure_leaf_weight(Growth_all)
dev.off()
pdf("ms/RA/figures/height.pdf", width = 16L, height = 9L)
figure_height(Growth_all)
dev.off()
ReproductiveCosts_all <- combine_data_frames(BAER_ReproductiveCosts, BOLE_ReproductiveCosts, COER_ReproductiveCosts, EPMI_ReproductiveCosts, GRBU_ReproductiveCosts, GRSP_ReproductiveCosts, HATE_ReproductiveCosts, HEPU_ReproductiveCosts, LEES_ReproductiveCosts, PELA_ReproductiveCosts, PEPU_ReproductiveCosts, PHPH_ReproductiveCosts, PILI_ReproductiveCosts, PUTU_ReproductiveCosts, NULL)
SummaryInd_1 <- combine_by_individual(IndividualsList, Growth_all, ReproductiveCosts_all, LMA, leafLoss, wood_density_spp, seedsize)
SummaryInd <- sort_by_variable(SummaryInd_1, variable_list)
fits_figure_allocation_all_a <- fit_allocation_model(SummaryInd)
pdf("ms/RA/figures/RA_all_a.pdf", width = 16L, height = 9L)
figure_allocation_all_a(fits_figure_allocation_all_a)
dev.off()
pdf("ms/RA/figures/investment.pdf", width = 16L, height = 9L)
figure_investment_weight(SummaryInd)
dev.off()
SummarySppAge <- get_species_values(SummaryInd, groups = c("species", "age"))
pdf("ms/RA/figures/RA_demo.pdf", width = 16L, height = 4L)
figure_allocation_demo_single(SummarySppAge)
dev.off()
pdf("ms/RA/figures/RA_demo_all.pdf", width = 16L, height = 32L)
figure_allocation_demo_all(SummarySppAge)
dev.off()
pdf("ms/RA/figures/abstract.pdf", width = 8L, height = 4L)
figure_graphical_abstract(SummarySppAge)
dev.off()
SummarySpp <- get_species_values(SummaryInd, groups = "species")
pdf("ms/RA/figures/life_history.pdf", width = 8L, height = 4L)
figure_life_history(SummarySpp, SummarySppAge, fits_figure_allocation_all_a)
dev.off()
pdf("ms/RA/figures/leaf_loss.pdf", width = 4L, height = 4L)
figure_LMA_leafloss(SummarySpp)
dev.off()
knitr::knit("ms/RA/Wenk-RA-SI.Rnw", "ms/RA/Wenk-RA-SI.tex")
latex_build("ms/RA/Wenk-RA-SI.tex", clean = TRUE)
