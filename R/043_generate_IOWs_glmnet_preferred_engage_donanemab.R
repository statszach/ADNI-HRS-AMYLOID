# This script specify IOWs for different subsets: preferred,
rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))
source(here::here('R', '005_libraries.R'))
library(magrittr)

#---- Load the Data ----
harmonized <- readRDS(here::here("R_objects", "040_imp_data.RDS")) %>%
  dplyr::mutate(dataset = dplyr::if_else(DATA == "HRS", 1, 0)) %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         SEXF1M0 = dplyr::if_else(GENDER == "0", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0),
         MCI1Y0N = dplyr::if_else(DX == "MCI", 1, 0),
         DEM1Y0N = dplyr::if_else(DX == "Dementia", 1, 0),
         APOE41Y0N = as.numeric(APOE41Y0N),
         APOE41Y0N = dplyr::if_else(APOE41Y0N == 2, 1, 0)) %>% # recode for LR
  arrange(ID)

#---- Subset according to different scenarios ----
pet_data <- readRDS(here::here("R_objects", "020_amy_pet_04.RDS"))
survey <- readRDS(here::here("R_objects", "042_survey_data.RDS"))

adni_data <- survey %>%
  dplyr::filter(DATA == "ADNI") %>%
  rename(RID = ID) %>%
  mutate(RID = as.numeric(RID))
merge1 <- left_join(adni_data, pet_data, by = "RID")

# ENGAGE CRITERIA
engage_id <- merge1 %>%
  filter(bl_wholecereb > 25) %>%
  pull(RID)

# donanemab CRITERIA
donanemab_id <- merge1 %>%
  filter(bl_wholecereb > 36) %>%
  pull(RID)

# shouldbe CRITERIA
shouldbe_id <- merge1 %>%
  filter(between(bl_wholecereb, 15, 50)) %>%
  pull(RID)

#---- load functions ----
get_std_mean_diff <- function(data, mean_var, sd_var){

  mean_hrs <- data %>%
    filter(DATA == "HRS") %>%
    select(all_of(mean_var)) %>%
    pull()

  mean_adni <- data %>%
    filter(DATA == "ADNI") %>%
    select(all_of(mean_var)) %>%
    pull()

  sd_hrs <- data %>%
    filter(DATA == "HRS") %>%
    select(all_of(sd_var)) %>%
    pull()

  return((mean_adni - mean_hrs) / sd_hrs)

}

get_cohen_h <- function(p1, p2){

  h <- 2*asin(sqrt(p1))-2*asin(sqrt(p2))
}

#---- Function for weight development ----
weight_dev <- function(subdataset){
  #---- Use elastic net to look at all second order interactions ----
  response <- as.matrix(subdataset$dataset)
  predictors <- subdataset %>%
    dplyr::transmute(age_rcs = rms::rcs(AGE),
                     MCI1Y0N = MCI1Y0N,
                     DEM1Y0N = DEM1Y0N,
                     MMSE_rcs = rms::rcs(MMSE),
                     SEXF1M0 = SEXF1M0,
                     EDYRS_rcs = rms::rcs(EDYRS),
                     HISP1Y0N = HISP1Y0N,
                     BLACK1Y0N = BLACK1Y0N,
                     APOE41Y0N = APOE41Y0N)
  ## see https://stackoverflow.com/questions/27580267/how-to-make-all-interactions-before-using-glmnet
  ## for making interaction terms

  create_intx <- as.formula(response ~ .^2)
  predictors_intx <- model.matrix(create_intx, predictors)[, -1]

  elasticnet_second <- glmnet::glmnet(x = predictors_intx,
                                      y = response,
                                      family = "binomial"(link = "logit"),
                                      weights = subdataset$WEIGHT,
                                      nlambda = 50)
  # 20-fold cross validation
  cv_second <- glmnet::cv.glmnet(x = predictors_intx,
                                 y = response,
                                 family = "binomial"(link = "logit"),
                                 weights = subdataset$WEIGHT,
                                 type.measure = "mse",
                                 nfolds = 20)

  # cv_second$lambda.min
  # cv_second$lambda.1se

  # coef(elasticnet_second, s = cv_second$lambda.min)

  #---- Generate IOWs ----
  subdataset$predprob_glmnet <-
    predict(elasticnet_second, newx = predictors_intx[1:nrow(subdataset), ],
            type = "response", s = cv_second$lambda.min)

  pS <- sum(1 - subdataset$dataset)/
    sum(subdataset$WEIGHT)
  # dataset = 1 corresponds to S = 0 here, this corresponds to P(S = 1)/P(S = 0)

  subdataset_elastic <- subdataset %>%
    dplyr::mutate(
      weights = predprob_glmnet / (1 - predprob_glmnet),
      # Since dataset = 1 corresponds to S = 0 here, the weights was put as this
      # it should be the same as (1 - P(S=0|Z))/P(S=1|Z)
      stblz_weights = case_when(
        dataset == 0 ~ weights*pS/(1 - pS), # S = 1, ADNI
        dataset == 1 ~ WEIGHT # S = 0, ADAMS
      )
    )

  # sum_IOWs_ADNI <- subdataset_elastic %>%
  #   dplyr::filter(DATA == "ADNI") %>%
  #   dplyr::summarize(sum = sum(stblz_weights)) %>%
  #   pull() ## Pull sum of the IPWs in just ADNI For scaling
  #
  # adni_sample_size <- subdataset_elastic %>%
  #   dplyr::filter(DATA == "ADNI") %>%
  #   nrow()

  subdataset_elastic <- subdataset_elastic %>%
    dplyr::mutate(scaled_weights = case_when(
      dataset == 0 ~ stblz_weights,
      # dataset == 0 ~ (adni_sample_size/sum_IOWs_ADNI) * stblz_weights, # scale weights
      dataset == 1 ~ WEIGHT))

  subdataset_IOW_svydesign <- subdataset_elastic %>%
    srvyr::as_survey_design(ids = ID, weight = scaled_weights) #survey design for st mean diff

  subdataset_noIOW_svydesign <- subdataset_elastic %>%
    srvyr::as_survey_design(ids = ID, weight = WEIGHT)
  # Sanity check
  # with(subdataset_elastic %>% filter(DATA == "ADNI"),table(WEIGHT, useNA = "ifany"))

  for (scenario in c("IOW", "noIOW")){
    data_svydesign <- get(paste0("subdataset_", scenario,
                                 "_svydesign"))
    # smd for continuous variables
    cont_vars <- c("AGE", "MMSE", "EDYRS")
    temp_stats <- data_svydesign %>% # calculated std mean diff
      group_by(DATA) %>%
      summarize(across(all_of(cont_vars),
                       list(mean = function(x) survey_mean(x),
                            std = function(x) survey_sd(x)))) %>%
      select(-contains("_se"))

    temp_eff_size <- tibble()
    for (var in cont_vars){
      temp_eff_size[1, paste0("eff_size_", var)] <-
        get_std_mean_diff(temp_stats, paste0(var, "_mean"),
                          paste0(var, "_std"))
    }
    # cohen's h for categorical variables
    cat_vars <- c("APOE41Y0N", "SEXF1M0", "HISP1Y0N", "BLACK1Y0N",
                  "MCI1Y0N", "DEM1Y0N")
    temp_stats <- temp_stats %>% left_join(
      data_svydesign %>%
        group_by(DATA) %>%
        summarize(across(all_of(cat_vars),
                         list(Y = function(x) survey_mean(x),
                              N = function(x) 1 - survey_mean(x)))) %>%
        select(-contains("_se")), by = "DATA")

    for (var in cat_vars){
      temp_eff_size[1, paste0("eff_size_", var)] <-
        get_cohen_h(
          temp_stats[temp_stats$DATA == "HRS", paste0(var, "_Y")],
          temp_stats[temp_stats$DATA == "ADNI", paste0(var, "_Y")])
    }

    temp_eff_size <- t(temp_eff_size) %>% set_colnames("eff_size") %>%
      as_tibble() %>% mutate(
        var = gsub("eff_size_", "", rownames(t(temp_eff_size))),
        IOW = case_when(scenario == "IOW" ~ "Yes",
                        scenario == "noIOW" ~ "No"))
    assign(paste0(scenario, "_effect_size"), temp_eff_size)
  }

  plot_data <- rbind(IOW_effect_size, noIOW_effect_size) %>%
    dplyr::mutate(names = dplyr::case_when(var == "AGE" ~ "Age (in years)",
                                           var == "MMSE" ~ "MMSE",
                                           var == "HISP1Y0N" ~ "Hispanic ethnicity",
                                           var == "EDYRS" ~ "Years of education",
                                           var == "BLACK1Y0N" ~ "Black/African American",
                                           var == "APOE41Y0N" ~ "APOEe4 Allele present",
                                           var == "SEXF1M0" ~ "Female sex",
                                           var == "MCI1Y0N" ~ "MCI",
                                           var == "DEM1Y0N" ~ "Dementia"))

  # Return a list for the plot data and the dataset with IOW
  return(list(plot_data = plot_data, survey_data = subdataset_elastic))
}

#---- Weight development ----
# Total dataset
total_results <- weight_dev(harmonized)
# ENGAGE CRITERIA
engage_harmonized <- harmonized %>% filter(DATA == "HRS" |
                                             ID %in% engage_id)
engage_results <- weight_dev(engage_harmonized)
# donanemab CRITERIA
donanemab_harmonized <- harmonized %>% filter(DATA == "HRS" |
                                                ID %in% donanemab_id)
donanemab_results <- weight_dev(donanemab_harmonized)
# shouldbe CRITERIA
shouldbe_harmonized <- harmonized %>% filter(DATA == "HRS" |
                                               ID %in% shouldbe_id)
shouldbe_results <- weight_dev(shouldbe_harmonized)

#---- Balance Plots ----
for (scenario in c("total", "engage", "donanemab", "shouldbe")){

  p <- get(paste0(scenario, "_results"))$plot_data %>%
    ggplot(aes(y = fct_inorder(names), x = eff_size)) +
    geom_vline(xintercept = 0, color = "black") +
    geom_vline(xintercept = -.25, color = "grey", linetype = "longdash") +
    geom_vline(xintercept = .25, color = "grey", linetype = "longdash") +
    geom_line(aes(group = var), color = "black") +
    geom_point(aes(shape = IOW), fill = "white") +
    scale_shape_manual(values = c(21,16)) +
    # scale_color_manual(values = c("snow4", "black")) +
    theme_classic() +
    labs(title = paste0("Balance plot: ", scenario),
         x = "Effect size") +
    theme(axis.title.y = element_blank(),
          legend.position = "none")

  assign(paste0(scenario, "_balance_plot"), p)
}

total_balance_plot
engage_balance_plot
donanemab_balance_plot
shouldbe_balance_plot

#---- Assess overlap of participation ----
for (scenario in c("total", "engage", "donanemab", "shouldbe")){
  p <- get(paste0(scenario, "_results"))$survey_data %>%
    ggplot() +
    geom_density(aes(x = predprob_glmnet, group = DATA, linetype = DATA)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_classic() +
    theme(legend.position = "none") +
    ylab("Density") +
    scale_x_continuous(name = "Probability", breaks = c(0, 1))

  assign(paste0(scenario, "_density_plot"), p)
}

total_density_plot
engage_density_plot
donanemab_density_plot
shouldbe_density_plot

#---- Save the dataset ----
for (scenario in c("total", "engage", "donanemab", "shouldbe")){
  saveRDS(get(paste0(scenario, "_results"))$survey_data,
          here::here("R_objects", paste0("042_survey_data_", scenario, ".RDS")))
}
