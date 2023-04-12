rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

#---- Package loading ----
if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("tidyverse", "labelled", "gtsummary", "gt")

#---- Load data ----
harmonized_imp <- readRDS(here::here("R_objects", "040_imp_data.RDS")) %>%
  mutate(ID = as.double(ID))
survey <- readRDS(here::here("R_objects", "042_survey_data.RDS")) %>%
  mutate(ID = as.double(ID))
pet_data <- readRDS(here::here("R_objects", "020_amy_pet_04.RDS"))

table(harmonized_imp$GENDER, useNA = "ifany")
table(as.numeric(harmonized_imp$GENDER), useNA = "ifany")
table(harmonized_imp$ETHNICITY, useNA = "ifany")
table(harmonized_imp$DX, useNA = "ifany")

harmonized_imp_amy <- harmonized_imp %>%
  left_join(pet_data, by = c("ID" = "RID"))
# Sanity check
# colSums(is.na(harmonized_imp_amy %>% filter(DATA == "ADNI")))
# colSums(is.na(pet_data %>% filter(RID %in% harmonized_imp$ID)))

survey_amy <- survey %>%
  left_join(pet_data, by = c("ID" = "RID"))

#---- Weighted and unweighted Table 1 ----
survey_amy_svy <- survey_amy %>%
  select(DATA, AGE, GENDER, EDYRS, RACE_3CAT, ETHNICITY, MMSE, APOE41Y0N,
         bl_composite, DX, scaled_weights) %>%
  mutate(DATA = case_when(DATA == "ADNI" ~ "weighted ADNI",
                          TRUE ~ DATA)) %>%
  rbind(., harmonized_imp_amy %>%
          filter(DATA == "ADNI") %>%
          select(DATA, AGE, GENDER, EDYRS, RACE_3CAT, ETHNICITY, MMSE, APOE41Y0N,
                 bl_composite, DX) %>%
          mutate(scaled_weights = 1,
                 DATA = "unweighted ADNI")) %>%
  mutate(GENDER = as.numeric(GENDER) - 1,# Female = 1, male = 0
         ETHNICITY = as.numeric(ETHNICITY) - 1) %>% # ETHNICITY = 1 hispanic, 0 non-hispanic
  mutate(across(c("GENDER", "ETHNICITY", "APOE41Y0N"),
                function(x) factor(x, levels = c("1", "0"),
                                   labels = c("Yes", "No"))),
         DATA = ifelse(DATA == "HRS", "ADAMS", DATA),
         DX = ifelse(DX == "CN", "Normal", as.character(DX)),
         DX = factor(DX, levels = c("Normal", "MCI", "Dementia"))) %>%
  drop_unused_value_labels() %>%
  set_variable_labels(
    AGE = "Age",
    DX = "Dementia diagnosis",
    MMSE = "MMSE score",
    GENDER = "Female",
    EDYRS = "Education years",
    ETHNICITY = "Ethnicity: Hispanic/Latino",
    RACE_3CAT = "Race",
    APOE41Y0N = "APOE 4 allele: at least 1",
    bl_composite = "Amyloid SUVR (centiloid)",
    DATA = "Dataset",
    scaled_weights = "Scaled IOW") %>%
  survey::svydesign(
    ~ 1, data = ., weights = ~scaled_weights)

survey_amy_weighted <- tbl_svysummary(
  data = survey_amy_svy,
  type = list(c(scaled_weights) ~ "continuous"),
  statistic = list(all_categorical() ~ "{n} ({p}%)",
                   all_continuous() ~ "{mean} ({sd})"),

  by =  DATA,
  digits = list(all_continuous() ~ 1,
                all_categorical() ~ c(0, 1)),
  missing = "no") %>%
  # add_overall %>%
  modify_header(label = "") %>%
  modify_spanning_header(starts_with("stat_") ~ "**Dataset**") %>%
  bold_labels()

# NAed baseline SUVR for HRS ADAMS
survey_amy_weighted$table_body <- survey_amy_weighted$table_body %>%
  mutate(stat_1 = case_when(variable == "bl_composite" ~ NA_character_,
                            variable == "scaled_weights" ~ NA_character_,
                            TRUE ~ stat_1),
         stat_2 = case_when(variable == "scaled_weights" ~ NA_character_,
                            TRUE ~ stat_2))

survey_amy_weighted %>% as_flex_table()

survey_amy_weighted %>% as_flex_table() %>%
  flextable::save_as_docx(path = here::here("tables", "weighted_table1.docx"))

# #---- OLD ----
# #---- Table 1 ----
# # This was deprecated from the gtsummary package
# # options(
# #   gtsummary.tbl_summary.percent_fun = function(x) sprintf(x * 100, fmt='%#.2f'))
#
# table_1_df <- harmonized_imp_amy %>%
#   select(DATA, AGE, GENDER, EDYRS, RACE_3CAT, ETHNICITY, MMSE, APOE41Y0N,
#          bl_composite, DX) %>%
#   mutate(DATA = ifelse(DATA == "HRS", "ADAMS", DATA),
#          DX = ifelse(DX == "CN", "Normal", as.character(DX)),
#          DX = factor(DX, levels = c("Normal", "MCI", "Dementia")),
#          GENDER = as.numeric(GENDER) - 1,
#          ETHNICITY = as.numeric(ETHNICITY) - 1,
#          APOE41Y0N = as.numeric(APOE41Y0N) - 1
#   ) %>%
#   drop_unused_value_labels() %>%
#   set_variable_labels(
#     AGE = "Age",
#     DX = "Dementia diagnosis",
#     MMSE = "MMSE score",
#     GENDER = "Female",
#     EDYRS = "Education years",
#     ETHNICITY = "Ethnicity: Hispanic/Latino",
#     RACE_3CAT = "Race",
#     APOE41Y0N = "APOE 4 allele: at least 1",
#     bl_composite = "Amyloid SUVR (centiloid)",
#     DATA = "Dataset"
#   ) %>%
#   # set_value_labels(
#   #   APOE41Y0N = c("Yes" = "1", "No" = "0")) %>%
#   modify_if(is.labelled, to_factor) %>%
#   tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)",
#                                all_continuous() ~ "{mean} ({sd})"),
#               by =  DATA,
#               digits = list(all_continuous() ~ 1,
#                             all_categorical() ~ c(0, 1)),
#               missing = "no") %>%
#   # add_p %>%
#   add_overall %>%
#   modify_header(label = "") %>%
#   modify_spanning_header(starts_with("stat_") ~ "**Dataset**") %>%
#   bold_labels()
#
# # NAed baseline SUVR for overall and HRS ADAMS
# table_1_df$table_body <- table_1_df$table_body %>%
#   mutate(stat_0 = case_when(variable == "bl_composite" ~ NA_character_,
#                             TRUE ~ stat_0),
#          stat_1 = case_when(variable == "bl_composite" ~ NA_character_,
#                             TRUE ~ stat_1))
#
# table_1_df %>% as_flex_table()
#
# table_1_df %>% as_flex_table() %>%
#   flextable::save_as_docx(path = here::here("tables", "table1.docx"))
#
# # writexl::write_xlsx(as_tibble(table_1_df), here::here("tables", "table1.xlsx"))
