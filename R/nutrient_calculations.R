## Libraries
  
library(broom)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(yaml)
library(purrr)

source("R/figures.R")

## Load data

SummaryInd <- readRDS("output/SummaryInd.rds")
SummarySpp <- readRDS("output/SummarySpp.rds")
SummarySppAge <- readRDS("output/SummarySppAge.rds")
HarvestData <- readRDS("output/HarvestData.rds")
InvestmentByPart <- readRDS("output/Investment_FD_all.rds")
PartsSummary <- readRDS("output/PartsSummary_all.rds")

nutrient <- read.csv("data/nutrient_data.csv")
soil <- read.csv("data/nutrient_soil.csv")
accessoryList <- read.csv("data/accessoryList2.csv")
species_full <- read.csv("data/species.csv")
InvestmentCategories <- yaml.load_file("data/accessoryParts.yml")

## Add nutrient data to SummaryInd and SummarySpp
### Calculate NP by species, tissue

nutrient_spp <- nutrient %>%
  # convert units from nutrient spreadsheet into mg/mg for calculating amounts of nutrients in tissues
  dplyr::mutate(
          N = N/100, # convert from % to mg/mg
          P = P/10^6, # convert from mg/kg to mg/mg
          K = K/10^6 # convert from mg/kg to mg/mg
    ) %>% 
  dplyr::mutate(without_bark_diam_mm =gsub(",",";",without_bark_diam_mm),
         temp = strsplit(as.character(without_bark_diam_mm),";"),
         diameter_without_bark = map_dbl(temp, function(x) mean(as.numeric(x))),
         with_bark_diam_mm =gsub(",",";",with_bark_diam_mm),
         temp = strsplit(as.character(with_bark_diam_mm),";"),
         diameter_with_bark = map_dbl(temp, function(x) mean(as.numeric(x))),
         bark_thickness = (diameter_with_bark - diameter_without_bark)/2) %>%
  dplyr::select(-temp,-without_bark_diam_mm,-with_bark_diam_mm)

mean_spp_values <- function(nutrient_spp) {
  dplyr::select(nutrient_spp,species,parts_to_match,live_dead,K,P,N) %>%
    dplyr::group_by(species,parts_to_match,live_dead) %>%
    dplyr::summarise(across(c(N,P,K)), mean(.x, na.rm= TRUE)) %>% ungroup()
}

nutrient_summary <- nutrient_spp %>%
  dplyr::select(species, parts_to_match, live_dead, K, P, N) %>%
  group_by(species, parts_to_match, live_dead) %>%
  dplyr::summarise(across(c(N, P, K), 
                          ~mean(.x, na.rm= TRUE))) %>%
  ungroup() 

nutrient_summary_N <- nutrient_summary %>%
  dplyr::select(species,parts_to_match,live_dead, N) %>%
  dplyr::filter(!is.nan(N)) %>%
  tidyr::pivot_wider(names_from = live_dead, values_from = N) %>% 
  dplyr::rename(N_dead = dead, N_live = live, N_mixed = mixed)

nutrient_summary_P <- nutrient_summary %>%
  dplyr::select(species,parts_to_match,live_dead, P) %>%
  dplyr::filter(!is.nan(P)) %>%
  pivot_wider(names_from = live_dead, values_from = P) %>% 
  dplyr::rename(P_dead = dead, P_live = live, P_mixed = mixed)

nutrient_summary_K <- nutrient_summary %>%
  dplyr::select(species,parts_to_match,live_dead, K) %>%
  dplyr::filter(!is.nan(K)) %>%
  pivot_wider(names_from = live_dead, values_from = K) %>% 
  dplyr::rename(K_dead = dead, K_live = live, K_mixed = mixed)

nutrient_summary_by_spp <- nutrient_summary_N %>%
  full_join(nutrient_summary_P, by = join_by(species, parts_to_match)) %>%
  full_join(nutrient_summary_K, by = join_by(species, parts_to_match)) %>%
  mutate(
    NUE = 1 - (N_dead/N_live),
    PUE = 1 - (P_dead/P_live),
    KUE = 1 - (K_dead/K_live)
  )

### Create separate data frames for each tissue

leaf_nutrients <-
  nutrient_summary_by_spp %>%
  dplyr::filter(parts_to_match =="leaves") %>%
  dplyr::select(species,N_dead,N_live,P_dead,P_live,NUE,PUE) %>% 
  dplyr::rename(dead_leaf_N = N_dead, live_leaf_N = N_live, 
                dead_leaf_P = P_dead, live_leaf_P = P_live)

sapwood_nutrients <- 
  nutrient_summary_by_spp %>%
  dplyr::filter(parts_to_match =="sapwood") %>%
  dplyr::select(species,N_dead,N_live,P_dead,P_live,NUE,PUE) %>% 
  dplyr::rename(shed_sapwood_N = N_dead, sapwood_N = N_live, 
                shed_sapwood_P = P_dead, sapwood_P = P_live, 
                NUE_sapwood = NUE, PUE_sapwood = PUE)

bark_nutrients <-
  nutrient_summary_by_spp %>%
  dplyr::filter(parts_to_match =="bark") %>%
  dplyr::select(species,N_dead,N_live,P_dead,P_live,NUE,PUE) %>% 
  dplyr::rename(shed_bark_N = N_dead, bark_N = N_live, 
                shed_bark_P = P_dead, bark_P = P_live, 
                NUE_bark = NUE, PUE_bark = PUE)

base_wood <- 
  nutrient_summary_by_spp %>%
  dplyr::filter(parts_to_match == "wood_from_base") %>%
  dplyr::select(N_mixed,  P_mixed, species)  %>%
  dplyr::rename(N_base_wood = N_mixed, 
                P_base_wood = P_mixed)

mid_wood <- 
  nutrient_summary_by_spp %>%
  dplyr::filter(parts_to_match == "wood_from_middle") %>%
  dplyr::select(N_mixed,  P_mixed, species) %>%
  dplyr::rename(N_mid_wood = N_mixed, 
                P_mid_wood = P_mixed)

all_wood_by_species <- 
  sapwood_nutrients %>%
  dplyr::full_join(base_wood, by = join_by(species)) %>%
  dplyr::full_join(mid_wood, by = join_by(species)) %>%
  dplyr::full_join(bark_nutrients, by = join_by(species)) %>%
  dplyr::mutate(
    NUE_base_over_sapwood = 1 - (N_base_wood / sapwood_N),
    PUE_base_over_sapwood = 1 - (P_base_wood / sapwood_P)
  )

### Calculate repro investment in N, P for individuals
## gives many-to-many warning, because:
## in accessoryList there are tissues that contribute to both pre-pollination and packaging & dispersal (e.g. cones)

InvestIndPart <- InvestmentByPart %>% 
  dplyr::left_join(accessoryList, by = c("species", "part")) %>%
  dplyr::mutate(dry_mass_inv_to_use = ifelse(is.na(number_of_seeds_linked_to_dry_mass),weight,weight/number_of_seeds_linked_to_dry_mass)) %>%
  replace(is.na(.),0) %>%
  dplyr::select(-sort)

accessory_tissue_nutrient_contents <- nutrient_summary_by_spp %>%
  dplyr::select(-N_dead,-P_dead, -K_dead, -NUE, -PUE, -KUE, -N_mixed, -P_mixed, -K_mixed) %>%
  dplyr::filter(parts_to_match %in% c("bud", "cone", "fruit_seed_immature", "petals", "seed", "seed_pod", 
                                      "finished_flower", "fruit_mature", "green_parts", "bract", 
                                      "flower_stigma","calyx"))

# for accessory tissues and propagules, calculate total N (mg) and P (mg) investment in tissues
repro_NP_investment <- 
  InvestIndPart %>%
  dplyr::left_join(accessory_tissue_nutrient_contents, by=c("species", "parts_to_match")) %>% 
  dplyr::mutate(dry_mass_mg_per_unit = dry_mass_inv_to_use/count) %>%  
  dplyr::mutate(N_mass_repro_tissues = N_live*dry_mass_inv_to_use,
                P_mass_repro_tissues = P_live*dry_mass_inv_to_use) %>%
  dplyr::mutate(accessory_simple1 = ifelse(accessory_part %in% c("prepollination_success", "prepollination_abort"), "prepollination_all", ""),
                accessory_simple2 = ifelse(accessory_part %in% c("packaging_dispersal", "postpollination_abort"), "postpollination_all", ""),
                accessory_simple3 = ifelse(accessory_part == "propagule", "propagule",""),
                accessory_simple = paste0(accessory_simple1, accessory_simple2, accessory_simple3)) %>%
  dplyr::select(-accessory_simple1, -accessory_simple2, -accessory_simple3)


### Calculate N/P success costs (investment in required tissues per seed)

nutrients_for_repro_success_parts_by_spp <- 
  repro_NP_investment %>%
  dplyr::filter(required_for_success == "required") %>%
  dplyr::select(species, accessory_part, parts_to_match, part, 
                N_live, P_live, dry_mass_mg_per_unit, proportion_nut_mix) %>% 
  dplyr::group_by(species,parts_to_match, accessory_part) %>%
  dplyr::distinct(part,.keep_all=TRUE) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(N_inv_per_successful_unit = N_live*dry_mass_mg_per_unit,
                P_inv_per_successful_unit = P_live*dry_mass_mg_per_unit)

nutrients_for_repro_success_single_unit <- 
  nutrients_for_repro_success_parts_by_spp %>%
  dplyr::filter(accessory_part %in% c("prepollination_success", "propagule", "packaging_dispersal")) %>%
  dplyr::mutate(
    N_mass_prop_to_use = N_inv_per_successful_unit*proportion_nut_mix,
    P_mass_prop_to_use = P_inv_per_successful_unit*proportion_nut_mix,
    weight_prop_to_use = dry_mass_mg_per_unit*proportion_nut_mix
  ) %>%
  dplyr::select(-N_live,-P_live, -parts_to_match, -part) %>% 
  dplyr::group_by(species,accessory_part) %>%
  dplyr::summarise(across(c(N_inv_per_successful_unit, P_inv_per_successful_unit, dry_mass_mg_per_unit, 
                            N_mass_prop_to_use, P_mass_prop_to_use, weight_prop_to_use, proportion_nut_mix), 
                          ~sum(.x, na.rm= TRUE))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    scaled_N_content_accessory_parts_for_success = N_mass_prop_to_use/weight_prop_to_use,
    scaled_P_content_accessory_parts_for_success = P_mass_prop_to_use/weight_prop_to_use)

#formulation without scaling parts based on continued development
#scaled_N_content_accessory_parts_for_success = N_mass_per_successful_unit/dry_mass_mg_per_unit,
#scaled_P_content_accessory_parts_for_success = P_mass_per_successful_unit/dry_mass_mg_per_unit) 

N_inv_successful_units <- 
  nutrients_for_repro_success_single_unit %>%
  dplyr::select(species,accessory_part, N_inv_per_successful_unit) %>%
  tidyr::pivot_wider(names_from = accessory_part, values_from = N_inv_per_successful_unit) %>%
  dplyr::rename(N_inv_per_packaging_dispersal_success = packaging_dispersal, 
                N_inv_per_prepollination_success = prepollination_success, 
                N_inv_per_propagule = propagule) %>%
  replace(is.na(.), 0)

scaled_N_content_successful_units <- 
  nutrients_for_repro_success_single_unit %>%
  dplyr::select(species, accessory_part, scaled_N_content_accessory_parts_for_success) %>%
  tidyr::pivot_wider(names_from = accessory_part, values_from = scaled_N_content_accessory_parts_for_success) %>%
  dplyr::rename(scaled_N_content_per_packaging_dispersal_success = packaging_dispersal, 
                scaled_N_content_per_prepollination_success = prepollination_success, 
                scaled_N_content_per_propagule = propagule) %>%
  replace(is.na(.),0) 

P_inv_successful_units <- 
  nutrients_for_repro_success_single_unit %>%
  dplyr::select(species, accessory_part, P_inv_per_successful_unit) %>%
  tidyr::pivot_wider(names_from = accessory_part, values_from = P_inv_per_successful_unit) %>%
  dplyr::rename(P_inv_per_packaging_dispersal_success = packaging_dispersal, 
                P_inv_per_prepollination_success = prepollination_success, 
                P_inv_per_propagule = propagule) %>%
  replace(is.na(.),0)

scaled_P_content_successful_units <- 
  nutrients_for_repro_success_single_unit %>%
  dplyr::select(species, accessory_part, scaled_P_content_accessory_parts_for_success) %>%
  tidyr::pivot_wider(names_from = accessory_part, values_from = scaled_P_content_accessory_parts_for_success) %>%
  dplyr::rename(scaled_P_content_per_packaging_dispersal_success = packaging_dispersal, 
                scaled_P_content_per_prepollination_success = prepollination_success, 
                scaled_P_content_per_propagule = propagule) %>%
  replace(is.na(.),0)


# nutrient content (%) for a single successful unit, weighted by contribution of different accessory tissues
accessory_nut_percent <- nutrients_for_repro_success_single_unit %>%
  dplyr::select(species, accessory_part, N_inv_per_successful_unit, P_inv_per_successful_unit, dry_mass_mg_per_unit) %>%
  dplyr::filter(accessory_part %in% c("packaging_dispersal", "prepollination_success")) %>%
  dplyr::select(-accessory_part) %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(
    across(c(N_inv_per_successful_unit, P_inv_per_successful_unit, dry_mass_mg_per_unit), 
           ~sum(.x, na.rm = T))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    N_accessory_percent = N_inv_per_successful_unit/dry_mass_mg_per_unit,
    P_accessory_percent = P_inv_per_successful_unit/dry_mass_mg_per_unit,
    tissue_simple = "accessory"
  ) 


### Nutrient investment and concentrations in accessory tissues, by individual

propagule_nutrient_investment <-
  repro_NP_investment %>%
  dplyr::filter(accessory_part == "propagule") %>%
  dplyr::group_by(species, individual) %>%
  dplyr::summarise(across(c(N_mass_repro_tissues, P_mass_repro_tissues, count), 
                          ~sum(.x, na.rm = TRUE))) %>%
  dplyr::ungroup() %>%
  dplyr::rename(N_inv_propagule = N_mass_repro_tissues, 
                P_inv_propagule = P_mass_repro_tissues, 
                seed_count2 = count)

accessory_temp <- 
  repro_NP_investment %>%
  dplyr::select(individual, species, weight, accessory_part, accessory_simple, proportion_nut_mix, 
                N_live, P_live, N_mass_repro_tissues, P_mass_repro_tissues) %>%
  dplyr::filter(accessory_part != "propagule") %>%
  dplyr::mutate(
    N_mass_prop_to_use = ifelse(proportion_nut_mix==1, N_mass_repro_tissues, N_mass_repro_tissues*proportion_nut_mix), 
    P_mass_prop_to_use = ifelse(proportion_nut_mix==1, P_mass_repro_tissues, P_mass_repro_tissues*proportion_nut_mix)
  ) %>%
  dplyr::group_by(individual, species, accessory_simple) %>%
  dplyr::summarise(across(c(weight, N_mass_repro_tissues, P_mass_repro_tissues), 
                          ~sum(.x, na.rm=TRUE))) %>%
  dplyr::ungroup()

dry_mass_inv_accessory <- 
  accessory_temp %>%
  dplyr::select(individual, species, accessory_simple, weight) %>%
  dplyr::group_by(individual,species) %>%
  tidyr::pivot_wider(names_from = accessory_simple, values_from = weight) %>% 
  dplyr::ungroup() %>%
  dplyr::rename(dry_mass_inv_prepollination_all = prepollination_all, 
                dry_mass_inv_postpollination_all = postpollination_all)

N_inv_accessory <- 
  accessory_temp %>%
  dplyr::select(individual, species, accessory_simple, N_mass_repro_tissues) %>%
  dplyr::group_by(individual, species) %>%
  tidyr::pivot_wider(names_from = accessory_simple, values_from = N_mass_repro_tissues) %>% 
  dplyr::ungroup() %>%
  dplyr::rename(N_inv_prepollination_all = prepollination_all, 
                N_inv_postpollination_all = postpollination_all)

P_inv_accessory <-
  accessory_temp %>%
  dplyr::select(individual,species, accessory_simple,P_mass_repro_tissues) %>%
  dplyr::group_by(individual,species) %>%
  tidyr::pivot_wider(names_from = accessory_simple, values_from = P_mass_repro_tissues) %>% 
  dplyr::ungroup() %>%
  dplyr::rename(P_inv_prepollination_all = prepollination_all,
                P_inv_postpollination_all = postpollination_all)

nutrient_investment_v2 <-
  dry_mass_inv_accessory %>%
  dplyr::full_join(N_inv_accessory, by = c("individual", "species")) %>%
  dplyr::full_join(P_inv_accessory, by = c("individual", "species")) %>%
  dplyr::full_join(propagule_nutrient_investment, by = c("individual", "species")) %>%
  dplyr::full_join(N_inv_successful_units, by = c("species")) %>%
  dplyr::full_join(scaled_N_content_successful_units, by = c("species")) %>%
  dplyr::full_join(P_inv_successful_units, by = c("species")) %>%
  dplyr::full_join(scaled_P_content_successful_units, by = c("species")) %>%
  dplyr::mutate(
    N_content_prepollination_all = N_inv_prepollination_all / dry_mass_inv_prepollination_all,
    N_content_postpollination_all = N_inv_postpollination_all / dry_mass_inv_prepollination_all,
    P_content_prepollination_all = P_inv_prepollination_all / dry_mass_inv_prepollination_all,
    P_content_postpollination_all = P_inv_postpollination_all / dry_mass_inv_prepollination_all) %>% 
  replace(is.na(.), 0)


### Add nutrient data to SummaryInd

temp <- 
  SummaryInd %>% 
  dplyr::full_join(nutrient_investment_v2, by = c("individual", "species")) %>%
  dplyr::mutate(
    postpollen_aborted_inv = repro_inv - prepollen_success_inv - prepollen_discarded_inv - pack_disp_success_inv - propagule_inv,
    N_inv_prepollen_success = prepollen_success_inv * scaled_N_content_per_prepollination_success,
    N_inv_prepollen_abort = N_inv_prepollination_all - N_inv_prepollen_success,  
    #N_inv_prepollen_abort = prepollen_discarded_inv * N_content_prepollination_all,  
    N_inv_postpollen_success = pack_disp_success_inv * scaled_N_content_per_packaging_dispersal_success,
    N_inv_postpollen_abort = N_inv_postpollination_all - N_inv_postpollen_success,
    #N_inv_postpollen_abort = postpollen_aborted_inv * N_content_postpollination_all,
    N_inv_propagules = propagule_inv * scaled_N_content_per_propagule,
    N_inv_all_repro = N_inv_prepollen_success+ N_inv_prepollen_abort + N_inv_postpollen_success + N_inv_postpollen_abort + N_inv_propagules,
    P_inv_prepollen_success = prepollen_success_inv * scaled_P_content_per_prepollination_success,
    #P_inv_prepollen_abort = prepollen_discarded_inv * P_content_prepollination_all,  
    P_inv_prepollen_abort = P_inv_prepollination_all - P_inv_prepollen_success,  
    P_inv_postpollen_success = pack_disp_success_inv * scaled_P_content_per_packaging_dispersal_success,
    #P_inv_postpollen_abort = postpollen_aborted_inv * P_content_postpollination_all,
    P_inv_postpollen_abort = P_inv_postpollination_all - P_inv_postpollen_success,
    P_inv_propagules = propagule_inv * scaled_P_content_per_propagule, 
    P_inv_all_repro = P_inv_prepollen_success+ P_inv_prepollen_abort + P_inv_postpollen_success + P_inv_postpollen_abort + P_inv_propagules
  )

nutrient_accessory_proportions <-
  temp %>%
  dplyr::filter(repro_inv > 0) %>%
  replace(is.na(.),0) %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(
    prop_N_inv_propagules = N_inv_propagule / N_inv_all_repro,
    prop_N_inv_prepollen_success = N_inv_prepollen_success / N_inv_all_repro,
    prop_N_inv_prepollen_abort = N_inv_prepollen_abort / N_inv_all_repro,
    prop_N_inv_postpollen_success = N_inv_postpollen_success / N_inv_all_repro,
    prop_N_inv_postpollen_abort = N_inv_postpollen_abort / N_inv_all_repro,
    prop_N_prepollen_vs_required_success = N_inv_prepollen_success/ (N_inv_prepollen_success+N_inv_postpollen_success+N_inv_propagule),
    prop_N_postpollen_vs_required_success = N_inv_postpollen_success/ (N_inv_prepollen_success+N_inv_postpollen_success+N_inv_propagule),
    prop_N_propagule_vs_required_success = N_inv_propagule/ (N_inv_prepollen_success+N_inv_postpollen_success+N_inv_propagule),
    prop_N_prepollen_success_vs_all_prepollen = N_inv_prepollen_success/ (N_inv_prepollen_success+N_inv_prepollen_abort),
    prop_N_postpollen_success_vs_all_postpollen = N_inv_postpollen_success/ (N_inv_postpollen_success+N_inv_postpollen_abort),
    prop_N_prepollen_failure_vs_all_prepollen = N_inv_prepollen_abort/ (N_inv_prepollen_success+N_inv_prepollen_abort),
    N_mass_per_successful = N_inv_per_packaging_dispersal_success + N_inv_per_prepollination_success + N_inv_per_propagule,
    prop_N_prepollen_vs_required_success = N_inv_per_prepollination_success / N_mass_per_successful,
    prop_N_postpollen_vs_required_success = N_inv_per_packaging_dispersal_success / N_mass_per_successful, 
    prop_N_propagule_vs_required_success = N_inv_per_propagule / N_mass_per_successful
  ) %>% 
  dplyr::mutate(
    prop_P_inv_propagules = P_inv_propagule / P_inv_all_repro,
    prop_P_inv_prepollen_success = P_inv_prepollen_success / P_inv_all_repro,
    prop_P_inv_prepollen_abort = P_inv_prepollen_abort / P_inv_all_repro,
    prop_P_inv_postpollen_success = P_inv_postpollen_success / P_inv_all_repro,
    prop_P_inv_postpollen_abort = P_inv_postpollen_abort / P_inv_all_repro,
    prop_P_prepollen_vs_required_success = P_inv_prepollen_success/ (P_inv_prepollen_success+P_inv_postpollen_success+P_inv_propagule),
    prop_P_postpollen_vs_required_success = P_inv_postpollen_success/ (P_inv_prepollen_success+P_inv_postpollen_success+P_inv_propagule),
    prop_P_propagule_vs_required_success = P_inv_propagule/ (P_inv_prepollen_success+P_inv_postpollen_success+P_inv_propagule),
    prop_P_prepollen_success_vs_all_prepollen = P_inv_prepollen_success/ (P_inv_prepollen_success+P_inv_prepollen_abort),
    prop_P_postpollen_success_vs_all_postpollen = P_inv_postpollen_success/ (P_inv_postpollen_success+P_inv_postpollen_abort),
    prop_P_prepollen_failure_vs_all_prepollen = P_inv_prepollen_abort/ (P_inv_prepollen_success+P_inv_prepollen_abort),
    P_mass_per_successful = P_inv_per_packaging_dispersal_success + P_inv_per_prepollination_success + P_inv_per_propagule,
    prop_P_prepollen_vs_required_success = P_inv_per_prepollination_success / P_mass_per_successful,
    prop_P_postpollen_vs_required_success = P_inv_per_packaging_dispersal_success / P_mass_per_successful, 
    prop_P_propagule_vs_required_success = P_inv_per_propagule / P_mass_per_successful
  ) %>%
  dplyr::select(species,individual, N_inv_propagules, prop_N_inv_propagules, prop_N_inv_prepollen_success, prop_N_inv_prepollen_abort,
                prop_N_inv_postpollen_success, prop_N_inv_postpollen_abort,
                prop_N_prepollen_vs_required_success, prop_N_postpollen_vs_required_success,
                prop_N_propagule_vs_required_success, prop_N_prepollen_success_vs_all_prepollen,
                prop_N_prepollen_failure_vs_all_prepollen, N_mass_per_successful,
                prop_N_prepollen_vs_required_success, prop_N_postpollen_vs_required_success,
                prop_N_propagule_vs_required_success, P_inv_propagules,
                prop_P_inv_propagules, prop_P_inv_prepollen_success, prop_P_inv_prepollen_abort, 
                prop_P_inv_postpollen_success, prop_P_inv_postpollen_abort,
                prop_P_prepollen_vs_required_success, prop_P_postpollen_vs_required_success, 
                prop_P_propagule_vs_required_success, prop_P_prepollen_success_vs_all_prepollen,
                prop_P_prepollen_failure_vs_all_prepollen, P_mass_per_successful,
                prop_N_postpollen_success_vs_all_postpollen, prop_P_postpollen_success_vs_all_postpollen)



SummaryInd_nutrient <-
  temp %>%
  dplyr::left_join(nutrient_accessory_proportions, by = c("individual", "species")) %>%
  dplyr::full_join(accessory_temp, by= c("individual", "species")) %>%
  #full_join(accessory_total_by_individual, by= c("individual")) %>% #this data frame doesn't exist (2024), so added line above
  dplyr::full_join(leaf_nutrients, by = c("species")) %>%
  dplyr::full_join(all_wood_by_species, by = c("species")) %>%
  dplyr::mutate(
    growth_leaf = ifelse(growth_leaf<0,0,growth_leaf),
    leaf_N_lost = leaf_shed*dead_leaf_N,
    leaf_P_lost = leaf_shed*dead_leaf_P,
    leaf_N_resorbed = leaf_shed*(live_leaf_N-dead_leaf_N),
    leaf_N_resorbed = ifelse(leaf_N_resorbed <0,0,leaf_N_resorbed),
    leaf_P_resorbed = leaf_shed*(live_leaf_P-dead_leaf_P),
    leaf_N_new = leaf_inv_gross*live_leaf_N,
    leaf_P_new = leaf_inv_gross*live_leaf_P,
    leaf_N_net = leaf_N_new - leaf_N_resorbed,
    leaf_P_net = leaf_P_new - leaf_P_resorbed,
    leaf_N_for_RA = growth_leaf*live_leaf_N,
    leaf_P_for_RA = growth_leaf*live_leaf_P,
    stem_N_inv = growth_stem*sapwood_N,
    stem_P_inv = growth_stem*sapwood_P,
    NP_live_leaf = live_leaf_N/live_leaf_P,
    NP_dead_leaf = dead_leaf_N/dead_leaf_P,
    RA_N = N_inv_all_repro /(N_inv_all_repro + leaf_N_for_RA),
    RA_P = P_inv_all_repro / (P_inv_all_repro + leaf_P_for_RA),
    RE_C = repro_inv / gross_inv,
    RE_N = N_inv_all_repro / (N_inv_all_repro + stem_N_inv + leaf_N_new),
    RE_P = P_inv_all_repro / (P_inv_all_repro + stem_P_inv + leaf_P_new),
    RE_C_propagule = propagule_inv / gross_inv,
    RE_N_propagule = N_inv_propagule / (N_inv_all_repro + stem_N_inv + leaf_N_new),
    RE_N_propagule = ifelse(is.nan(RE_N_propagule), 0, RE_N_propagule),
    RE_P_propagule = P_inv_propagule / (P_inv_all_repro + stem_P_inv + leaf_P_new),
    RE_P_propagule = ifelse(is.nan(RE_P_propagule), 0, RE_P_propagule),
    total_N_per_year = leaf_N_new + N_inv_all_repro,
    total_P_per_year = leaf_P_new + P_inv_all_repro,
    prop_recycled_N = leaf_N_resorbed / leaf_N_new,
    prop_recycled_P = leaf_P_resorbed / leaf_P_new,
    prop_required_N = leaf_N_resorbed / total_N_per_year,
    prop_required_P = leaf_P_resorbed / total_P_per_year,
    LL_death = shoot_leaf_count_start/(shoot_leaf_count-shoot_leaf_count_new),
    seed_prop_N = N_inv_propagule / (N_inv_all_repro + leaf_N_for_RA),
    seed_prop_P = P_inv_propagule / (P_inv_all_repro + leaf_P_for_RA),
    age_for_cex = paste0("y", age, "y")
  )

SummaryInd_nutrient <- SummaryInd_nutrient[order(-SummaryInd_nutrient$age),]

# SummaryInd_nutrient %>% write_csv("data/SummaryInd_nutrient.csv")

### Calculate repro investment in N, P for species

### As part of this step change all units to what will be used in analyses
### mg/g for N, mg/kg for P

accessory_nut_percent <- accessory_nut_percent |>
  # convert units
  dplyr::mutate(
    N_accessory_percent = N_accessory_percent*1000, # convert from mg/mg to mg/g
    P_accessory_percent = P_accessory_percent*1000000 # convert from mg/mg to mg/kg
  )
  
propagule_nutrient_content_by_spp <- SummaryInd_nutrient %>%
  dplyr::select(species, scaled_N_content_per_propagule, scaled_P_content_per_propagule) %>%
  dplyr::rename(N = scaled_N_content_per_propagule, P = scaled_P_content_per_propagule) %>%
  dplyr::filter(N > 0) %>%
  # convert units
  dplyr::mutate(
    N = N*1000, # convert from mg/mg to mg/g
    P = P*1000000 # convert from mg/mg to mg/kg
  ) |>
  dplyr::group_by(species) %>%
  dplyr::summarise(across(c(N,P), ~mean(.x, na.rm= TRUE))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(tissue_simple = "propagule")

nutrient_temp <- nutrient %>%
  dplyr::select(species, tissue_simple, tissue_complete, N, P) %>%
  dplyr::filter(tissue_simple %in% c("bark", "wood_from_tip", "leaves")) %>%
  dplyr::group_by(species, tissue_complete, tissue_simple) %>%
  dplyr::summarise(across(c(N,P), ~mean(.x, na.rm= TRUE))) %>%
  dplyr::ungroup() %>%
  # convert units
  dplyr::mutate(
    N = N*10, # convert from % to mg/g
    P = P # retain as mg/kg
  ) |>
  dplyr::bind_rows(propagule_nutrient_content_by_spp) %>%
  dplyr::bind_rows(accessory_nut_percent)


nutrient_temp2 <- nutrient %>%
  dplyr::select(species, tissue_simple, tissue_complete, P, N) %>%
  dplyr::filter(tissue_simple %in% c("bark","wood_from_tip","leaves","senescent_leaves","shed_sapwood","wood_from_base")) %>%
  # convert units
  dplyr::mutate(
    N = N*10, # convert from % to mg/g
    P = P # retain as mg/kg
  ) |>
  dplyr::group_by(species,tissue_complete,tissue_simple) %>%
  dplyr::summarise(
    N_SE = sd(N)/sqrt(length((N))),
    P_SE = sd(P)/sqrt(length((P))),
    N = mean(N, na.rm = TRUE),
    P = mean(P, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(propagule_nutrient_content_by_spp) %>%
  dplyr::bind_rows(accessory_nut_percent %>%
                     dplyr::select(species, tissue_simple,
                                   N = N_accessory_percent,
                                   P = P_accessory_percent)
                   ) %>%
  dplyr::mutate(tissue_group = case_when(
    tissue_simple %in% c("accessory", "propagule") ~ "reproductive",
    tissue_simple %in% c("leaves", "bark", "wood_from_base", "wood_from_tip") ~ "live",
    tissue_simple %in% c("senescent_leaves", "shed_sapwood") ~ "senesced"
  ))

### Species-by-age summaries

#### summarise nutrient contents by species-by-age, plant part

nutrient_summary_N_by_age <-
  nutrient_spp %>%
  dplyr::select(species, age, parts_to_match,live_dead, N) %>%
  dplyr::group_by(species, age, parts_to_match, live_dead) %>%
  dplyr::summarise(N = mean(N, na.rm= TRUE)) %>% 
  dplyr::ungroup() %>%
  dplyr::select(species, age, parts_to_match, live_dead, N) %>%
  dplyr::filter(!is.nan(N)) %>%
  tidyr::pivot_wider(names_from = live_dead, values_from = N) %>% 
  dplyr::rename(N_dead = dead, N_live = live)

nutrient_summary_P_by_age <-
  nutrient_spp %>%
  dplyr::select(species, age, parts_to_match,live_dead, P) %>%
  dplyr::group_by(species, age, parts_to_match, live_dead) %>%
  dplyr::summarise(P = mean(P, na.rm= TRUE)) %>% 
  dplyr::ungroup() %>%
  dplyr::select(species, age, parts_to_match, live_dead, P) %>%
  dplyr::filter(!is.nan(P)) %>%
  tidyr::pivot_wider(names_from = live_dead, values_from = P) %>% 
  dplyr::rename(P_dead = dead, P_live = live)

nutrient_summary_K_by_age <-
  nutrient_spp %>%
  dplyr::select(species, age, parts_to_match,live_dead, K) %>%
  group_by(species, age, parts_to_match, live_dead) %>%
  dplyr::summarise(K = mean(K, na.rm= TRUE)) %>% 
  dplyr::ungroup() %>%
  dplyr::select(species, age, parts_to_match, live_dead, K) %>%
  dplyr::filter(!is.nan(K)) %>%
  tidyr::pivot_wider(names_from = live_dead, values_from = K) %>% 
  dplyr::rename(K_dead = dead, K_live = live)

nutrient_summary_by_spp_age <- nutrient_summary_N_by_age %>%
  dplyr::full_join(nutrient_summary_P_by_age, by = join_by(species, age, parts_to_match, mixed)) %>%
  dplyr::full_join(nutrient_summary_K_by_age, by = join_by(species, age, parts_to_match, mixed)) %>%
  dplyr::mutate(
    NUE = 1 - (N_dead/N_live),
    PUE = 1 - (P_dead/P_live),
    KUE = 1 - (K_dead/K_live),
    age_for_cex = paste0("y",age,"y")
  )

nutrient_summary_by_spp_age <- nutrient_summary_by_spp_age[order(-nutrient_summary_by_spp_age$age),] 
