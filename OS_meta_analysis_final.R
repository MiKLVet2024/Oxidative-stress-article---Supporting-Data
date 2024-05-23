

##################################################
##################################################
##                                              ##
##                                              ##
##      Meta-analysis for the paper             ##
##    ~ Oxidative status:                       ##
##      A general but overlooked indicator      ##
##      of welfare across animal species? ~     ##
##                                              ##
##                                              ##
##################################################
##################################################

# Clear previous data to avoid mixing up names of variables
rm(list = ls())

# Set working directory
setwd("/Users/simon/Dropbox/R_scripts_for_Michael")

# Load packages
require(openxlsx)
require(metafor)
require(dplyr)


##################################################
##################################################
# Social isolation
##################################################
##################################################

# Load data file
social <- openxlsx::read.xlsx("New_data_file_from_Michael_2024_-_modified_by_Simon.xlsx",
                              sheet = 1,
                              check.names = TRUE)

# Study number as character vector
social$study <- as.character(social$study)

# Effect size ID (one per row)
social$ES_ID <- seq.int(nrow(social))

# Treating factors as factors
social$Tissues <- as.factor(social$test)


##################################################
# Calculating effect sizes
##################################################

# Calculating Hedges' g's and effect size variance
social_MDA <- metafor::escalc(n1i = n_exp,
                              n2i = n_controls,
                              m1i = mean_exp_MDA,
                              m2i = mean_controls_MDA,
                              sd1i = sd_exp_MDA,
                              sd2i = sd_controls_MDA,
                              data = social,
                              measure = "SMD",
                              append = TRUE)

# Subsetting
social_MDA_subset <- social_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% 
  base::droplevels(social_MDA$Tissues)

# Calculating Hedges' g's and effect size variance
social_GSH <- metafor::escalc(n1i = n_exp,
                              n2i = n_controls,
                              m1i = mean_exp_GSH,
                              m2i = mean_controls_GSH,
                              sd1i = sd_exp_GSH,
                              sd2i = sd_controls_GSH,
                              data = social,
                              measure = "SMD",
                              append = TRUE)

# Subsetting
social_GSH_subset <- social_GSH %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% 
  base::droplevels(social_GSH$Tissues)

# Calculating Hedges' g's and effect size variance
social_CAT <- metafor::escalc(n1i = n_exp,
                              n2i = n_controls,
                              m1i = mean_exp_CAT,
                              m2i = mean_controls_CAT,
                              sd1i = sd_exp_CAT,
                              sd2i = sd_controls_CAT,
                              data = social,
                              measure = "SMD",
                              append = TRUE)

# Subsetting
social_CAT_subset <- social_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% 
  base::droplevels(social_CAT$Tissues)

# Calculating Hedges' g's and effect size variance
social_SOD <- metafor::escalc(n1i = n_exp,
                              n2i = n_controls,
                              m1i = mean_exp_SOD,
                              m2i = mean_controls_SOD,
                              sd1i = sd_exp_SOD,
                              sd2i = sd_controls_SOD,
                              data = social,
                              measure = "SMD",
                              append = TRUE)

# Subsetting
social_SOD_subset <- social_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% 
  base::droplevels(social_SOD$Tissues)


##################################################
# POOLED TISSUES
##################################################


##################################################
# Social ~ MDA
##################################################

mod_social_MDA <- metafor::rma.mv(yi, # Effect size
                                  vi, # Variance
                                  random = list(~ 1 | study,
                                                ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                  # model, rather than an Equal Effects (EE) model
                                  tdist = TRUE, # t-distribution more accurate
                                  # than default Z-distribution
                                  data = social_MDA_subset,
                                  method = "REML")


##################################################
# Social ~ GSH
##################################################

mod_social_GSH <- metafor::rma.mv(yi, # Effect size
                                  vi, # Variance
                                  random = list(~ 1 | study,
                                                ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                  # model, rather than an Equal Effects (EE) model
                                  tdist = TRUE, # t-distribution more accurate
                                  # than default Z-distribution
                                  data = social_GSH_subset,
                                  method = "REML")


##################################################
# Social ~ CAT
##################################################

mod_social_CAT <- metafor::rma.mv(yi, # Effect size
                                  vi, # Variance
                                  random = list(~ 1 | study,
                                                ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                  # model, rather than an Equal Effects (EE) model
                                  tdist = TRUE, # t-distribution more accurate
                                  # than default Z-distribution
                                  data = social_CAT_subset,
                                  method = "REML")


##################################################
# Social ~ SOD
##################################################

mod_social_SOD <- metafor::rma.mv(yi, # Effect size
                                  vi, # Variance
                                  random = list(~ 1 | study,
                                                ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                  # model, rather than an Equal Effects (EE) model
                                  tdist = TRUE, # t-distribution more accurate
                                  # than default Z-distribution
                                  data = social_SOD_subset,
                                  method = "REML")


##################################################
# Testing model assumptions
##################################################

# Social ~ MDA
# stats::profile(mod_social_MDA)
# 
# # Social ~ GSH
# stats::profile(mod_social_GSH)
# 
# # Social ~ CAT
# stats::profile(mod_social_CAT)
# 
# # Social ~ SOD
# stats::profile(mod_social_SOD)


##################################################
# Results
##################################################

# Social ~ MDA
summary(mod_social_MDA)

# Social ~ GSH
summary(mod_social_GSH)

# Social ~ CAT
summary(mod_social_CAT)

# Social ~ SOD
summary(mod_social_SOD)


##################################################
# METAREGRESSION
##################################################

# Checking sample sizes. Returns "FALSE" if
# any subcategory sample size is lower than 5
sum(table(social_MDA_subset$Tissues) < 5) == 0
sum(table(social_GSH_subset$Tissues) < 5) == 0
sum(table(social_CAT_subset$Tissues) < 5) == 0
sum(table(social_SOD_subset$Tissues) < 5) == 0


##################################################
# Social ~ MDA ~ Tissues
##################################################

mod_social_MDA_metareg <- metafor::rma.mv(yi, # Effect size
                                          vi, # Variance
                                          mods = ~ Tissues,
                                          random = list(~ 1 | study,
                                                        ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                          # model, rather than an Equal Effects (EE) model
                                          tdist = TRUE, # t-distribution more accurate
                                          # than default Z-distribution
                                          data = social_MDA_subset,
                                          method = "REML")


##################################################
# Social ~ GSH ~ Tissues
##################################################

mod_social_GSH_metareg <- metafor::rma.mv(yi, # Effect size
                                          vi, # Variance
                                          mods = ~ Tissues,
                                          random = list(~ 1 | study,
                                                        ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                          # model, rather than an Equal Effects (EE) model
                                          tdist = TRUE, # t-distribution more accurate
                                          # than default Z-distribution
                                          data = social_GSH_subset,
                                          method = "REML")


##################################################
# Social ~ CAT ~ Tissues
##################################################

# mod_social_CAT_metareg <- metafor::rma.mv(yi, # Effect size
#                                           vi, # Variance
#                                           mods = ~ Tissues,
#                                           random = list(~ 1 | study,
#                                                         ~ 1 | ES_ID), # ES_ID to fit a Random Effects
#                                           # model, rather than an Equal Effects (EE) model
#                                           tdist = TRUE, # t-distribution more accurate
#                                           # than default Z-distribution
#                                           data = social_CAT_subset,
#                                           method = "REML")


##################################################
# Social ~ SOD ~ Tissues
##################################################

mod_social_SOD_metareg <- metafor::rma.mv(yi, # Effect size
                                          vi, # Variance
                                          mods = ~ Tissues,
                                          random = list(~ 1 | study,
                                                        ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                          # model, rather than an Equal Effects (EE) model
                                          tdist = TRUE, # t-distribution more accurate
                                          # than default Z-distribution
                                          data = social_SOD_subset,
                                          method = "REML")


##################################################
# Testing model assumptions
##################################################

# # Social ~ MDA ~ Tissues
# stats::profile(mod_social_MDA_metareg)
# 
# # Social ~ GSH ~ Tissues
# stats::profile(mod_social_GSH_metareg)
# 
# # Social ~ CAT ~ Tissues
# stats::profile(mod_social_CAT_metareg)
# 
# # Social ~ SOD ~ Tissues
# stats::profile(mod_social_SOD_metareg)


##################################################
# Results
##################################################

# Social ~ MDA ~ Tissues
summary(mod_social_MDA_metareg)

# Social ~ GSH ~ Tissues
summary(mod_social_GSH_metareg)

# # Social ~ CAT ~ Tissues
# summary(mod_social_CAT_metareg)

# Social ~ SOD ~ Tissues
summary(mod_social_SOD_metareg)


##################################################
# SELECTION MODELS
##################################################


##################################################
# Social ~ MDA
##################################################

mod_social_MDA_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                    vi, # Variance
                                                    data = social_MDA_subset,
                                                    test = "t",
                                                    method = "REML")

mod_social_MDA_selmodel <- metafor::selmodel(mod_social_MDA_without_study_ID,
                                             type = "stepfun",
                                             alternative = "greater",
                                             steps = c(0.001))


##################################################
# Social ~ GSH
##################################################

mod_social_GSH_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                    vi, # Variance
                                                    data = social_GSH_subset,
                                                    test = "t",
                                                    method = "REML")

mod_social_GSH_selmodel <- metafor::selmodel(mod_social_GSH_without_study_ID,
                                             type = "stepfun",
                                             alternative = "greater",
                                             steps = c(0.975))


##################################################
# Social ~ CAT
##################################################

mod_social_CAT_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                    vi, # Variance
                                                    data = social_CAT_subset,
                                                    test = "t",
                                                    method = "REML")

mod_social_CAT_selmodel <- metafor::selmodel(mod_social_CAT_without_study_ID,
                                             type = "stepfun",
                                             alternative = "greater",
                                             steps = c(0.400))


##################################################
# Social ~ SOD
##################################################

mod_social_SOD_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                    vi, # Variance
                                                    data = social_SOD_subset,
                                                    test = "t",
                                                    method = "REML")

mod_social_SOD_selmodel <- metafor::selmodel(mod_social_SOD_without_study_ID,
                                             type = "stepfun",
                                             alternative = "greater",
                                             steps = c(0.900))


##################################################
# Testing model assumptions
##################################################

# # Social ~ MDA
# stats::profile(mod_social_MDA_selmodel)
# 
# # Social ~ GSH
# stats::profile(mod_social_GSH_selmodel)
# 
# # Social ~ CAT
# stats::profile(mod_social_CAT_selmodel)
# 
# # Social ~ SOD
# stats::profile(mod_social_SOD_selmodel)


##################################################
# Results
##################################################

# Social ~ MDA
summary(mod_social_MDA_selmodel)
# metafor::plot.rma.uni.selmodel(mod_social_MDA_selmodel)

# Social ~ GSH
summary(mod_social_GSH_selmodel)
# metafor::plot.rma.uni.selmodel(mod_social_GSH_selmodel)

# Social ~ CAT
summary(mod_social_CAT_selmodel)
# metafor::plot.rma.uni.selmodel(mod_social_CAT_selmodel)

# Social ~ SOD
summary(mod_social_SOD_selmodel)
# metafor::plot.rma.uni.selmodel(mod_social_SOD_selmodel)


##################################################
# SUB-ANALYSES - ONE PER TAXON / TISSUE
##################################################


##################################################
# Social ~ MDA
##################################################

# Hippocampus
social_MDA_hippocampus <- social_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Hippocampus") %>%
  as.data.frame()

mod_social_MDA_hippocampus <- metafor::rma.mv(yi, # Effect size
                                              vi, # Variance
                                              random = list(~ 1 | study,
                                                            ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                              # model, rather than an Equal Effects (EE) model
                                              tdist = TRUE, # t-distribution more accurate
                                              # than default Z-distribution
                                              data = social_MDA_hippocampus,
                                              method = "REML")

# PFC
social_MDA_PFC <- social_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "PFC") %>%
  as.data.frame()

mod_social_MDA_PFC <- metafor::rma.mv(yi, # Effect size
                                      vi, # Variance
                                      random = list(~ 1 | study,
                                                    ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                      # model, rather than an Equal Effects (EE) model
                                      tdist = TRUE, # t-distribution more accurate
                                      # than default Z-distribution
                                      data = social_MDA_PFC,
                                      method = "REML")


##################################################
# Social ~ GSH
##################################################

# Hippocampus
social_GSH_hippocampus <- social_GSH %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Hippocampus") %>%
  as.data.frame()

mod_social_GSH_hippocampus <- metafor::rma.mv(yi, # Effect size
                                              vi, # Variance
                                              random = list(~ 1 | study,
                                                            ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                              # model, rather than an Equal Effects (EE) model
                                              tdist = TRUE, # t-distribution more accurate
                                              # than default Z-distribution
                                              data = social_GSH_hippocampus,
                                              method = "REML")

# PFC
social_GSH_PFC <- social_GSH %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "PFC") %>%
  as.data.frame()

mod_social_GSH_PFC <- metafor::rma.mv(yi, # Effect size
                                      vi, # Variance
                                      random = list(~ 1 | study,
                                                    ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                      # model, rather than an Equal Effects (EE) model
                                      tdist = TRUE, # t-distribution more accurate
                                      # than default Z-distribution
                                      data = social_GSH_PFC,
                                      method = "REML")


##################################################
# Social ~ CAT
##################################################

# Hippocampus
social_CAT_hippocampus <- social_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Hippocampus") %>%
  as.data.frame()

mod_social_CAT_hippocampus <- metafor::rma.mv(yi, # Effect size
                                              vi, # Variance
                                              random = list(~ 1 | study,
                                                            ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                              # model, rather than an Equal Effects (EE) model
                                              tdist = TRUE, # t-distribution more accurate
                                              # than default Z-distribution
                                              data = social_CAT_hippocampus,
                                              method = "REML")

# PFC
social_CAT_PFC <- social_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "PFC") %>%
  as.data.frame()

mod_social_CAT_PFC <- metafor::rma.mv(yi, # Effect size
                                      vi, # Variance
                                      random = list(~ 1 | study,
                                                    ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                      # model, rather than an Equal Effects (EE) model
                                      tdist = TRUE, # t-distribution more accurate
                                      # than default Z-distribution
                                      data = social_CAT_PFC,
                                      method = "REML")


##################################################
# Social ~ SOD
##################################################

# Hippocampus
social_SOD_hippocampus <- social_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Hippocampus") %>%
  as.data.frame()

mod_social_SOD_hippocampus <- metafor::rma.mv(yi, # Effect size
                                              vi, # Variance
                                              random = list(~ 1 | study,
                                                            ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                              # model, rather than an Equal Effects (EE) model
                                              tdist = TRUE, # t-distribution more accurate
                                              # than default Z-distribution
                                              data = social_SOD_hippocampus,
                                              method = "REML")

# PFC
social_SOD_PFC <- social_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "PFC") %>%
  as.data.frame()

mod_social_SOD_PFC <- metafor::rma.mv(yi, # Effect size
                                      vi, # Variance
                                      random = list(~ 1 | study,
                                                    ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                      # model, rather than an Equal Effects (EE) model
                                      tdist = TRUE, # t-distribution more accurate
                                      # than default Z-distribution
                                      data = social_SOD_PFC,
                                      method = "REML")


##################################################
# Testing model assumptions
##################################################

# # Social ~ MDA
# stats::profile(mod_social_MDA_hippocampus)
# stats::profile(mod_social_MDA_PFC)
# 
# # Social ~ GSH
# stats::profile(mod_social_GSH_hippocampus)
# 
# # Social ~ CAT
# stats::profile(mod_social_CAT_hippocampus)
# stats::profile(mod_social_CAT_PFC)
# 
# # Social ~ SOD
# stats::profile(mod_social_SOD_hippocampus)
# stats::profile(mod_social_SOD_PFC)


##################################################
# Results
##################################################

# Social ~ MDA
summary(mod_social_MDA_hippocampus)
summary(mod_social_MDA_PFC)

# Social ~ GSH
summary(mod_social_GSH_hippocampus)

# Social ~ CAT
summary(mod_social_CAT_hippocampus)
summary(mod_social_CAT_PFC)

# Social ~ SOD
summary(mod_social_SOD_hippocampus)
summary(mod_social_SOD_PFC)








##################################################
##################################################
# Noise exposure
##################################################
##################################################

# Load data file
noise <- openxlsx::read.xlsx("New_data_file_from_Michael_2024_-_modified_by_Simon.xlsx",
                             sheet = 2,
                             check.names = TRUE)

# Study number as character vector
noise$study <- as.character(noise$study)

# Effect size ID (one per row)
noise$ES_ID <- seq.int(nrow(noise))

# Treating factors as factors
noise$Tissues <- as.factor(noise$test)


##################################################
# Calculating effect sizes
##################################################

# Calculating Hedges' g's and effect size variance
noise_MDA <- metafor::escalc(n1i = n_exp,
                             n2i = n_controls,
                             m1i = mean_exp_MDA,
                             m2i = mean_controls_MDA,
                             sd1i = sd_exp_MDA,
                             sd2i = sd_controls_MDA,
                             data = noise,
                             measure = "SMD",
                             append = TRUE)

# Subsetting
noise_MDA_subset <- noise_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>%
  base::droplevels(noise_MDA$Tissues)

# Calculating Hedges' g's and effect size variance
noise_GSH <- metafor::escalc(n1i = n_exp,
                             n2i = n_controls,
                             m1i = mean_exp_GSH,
                             m2i = mean_controls_GSH,
                             sd1i = sd_exp_GSH,
                             sd2i = sd_controls_GSH,
                             data = noise,
                             measure = "SMD",
                             append = TRUE)

# Subsetting
noise_GSH_subset <- noise_GSH %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>%
  base::droplevels(noise_GSH$Tissues)

# Calculating Hedges' g's and effect size variance
noise_CAT <- metafor::escalc(n1i = n_exp,
                             n2i = n_controls,
                             m1i = mean_exp_CAT,
                             m2i = mean_controls_CAT,
                             sd1i = sd_exp_CAT,
                             sd2i = sd_controls_CAT,
                             data = noise,
                             measure = "SMD",
                             append = TRUE)

# Subsetting
noise_CAT_subset <- noise_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>%
  base::droplevels(noise_CAT$Tissues)

# Calculating Hedges' g's and effect size variance
noise_SOD <- metafor::escalc(n1i = n_exp,
                             n2i = n_controls,
                             m1i = mean_exp_SOD,
                             m2i = mean_controls_SOD,
                             sd1i = sd_exp_SOD,
                             sd2i = sd_controls_SOD,
                             data = noise,
                             measure = "SMD",
                             append = TRUE)

# Subsetting
noise_SOD_subset <- noise_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% 
  base::droplevels(noise_SOD$Tissues)


##################################################
# POOLED TISSUES
##################################################


##################################################
# Noise ~ MDA
##################################################

mod_noise_MDA <- metafor::rma.mv(yi, # Effect size
                                 vi, # Variance
                                 random = list(~ 1 | study,
                                               ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                 # model, rather than an Equal Effects (EE) model
                                 tdist = TRUE, # t-distribution more accurate
                                 # than default Z-distribution
                                 data = noise_MDA_subset,
                                 method = "REML")


##################################################
# Noise ~ GSH
##################################################

mod_noise_GSH <- metafor::rma.mv(yi, # Effect size
                                 vi, # Variance
                                 random = list(~ 1 | study,
                                               ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                 # model, rather than an Equal Effects (EE) model
                                 tdist = TRUE, # t-distribution more accurate
                                 # than default Z-distribution
                                 data = noise_GSH_subset,
                                 method = "REML")


##################################################
# Noise ~ CAT
##################################################

mod_noise_CAT <- metafor::rma.mv(yi, # Effect size
                                 vi, # Variance
                                 random = list(~ 1 | study,
                                               ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                 # model, rather than an Equal Effects (EE) model
                                 tdist = TRUE, # t-distribution more accurate
                                 # than default Z-distribution
                                 data = noise_CAT_subset,
                                 method = "REML")


##################################################
# Noise ~ SOD
##################################################

mod_noise_SOD <- metafor::rma.mv(yi, # Effect size
                                 vi, # Variance
                                 random = list(~ 1 | study,
                                               ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                 # model, rather than an Equal Effects (EE) model
                                 tdist = TRUE, # t-distribution more accurate
                                 # than default Z-distribution
                                 data = noise_SOD_subset,
                                 method = "REML")


##################################################
# Testing model assumptions
##################################################

# # Noise ~ MDA
# stats::profile(mod_noise_MDA)
# 
# # Noise ~ GSH
# stats::profile(mod_noise_GSH)
# 
# # Noise ~ CAT
# stats::profile(mod_noise_CAT)
# 
# # Noise ~ SOD
# stats::profile(mod_noise_SOD)


##################################################
# Results
##################################################

# Noise ~ MDA
summary(mod_noise_MDA)

# Noise ~ GSH
summary(mod_noise_GSH)

# Noise ~ CAT
summary(mod_noise_CAT)

# Noise ~ SOD
summary(mod_noise_SOD)


##################################################
# METAREGRESSION
##################################################

# Checking sample sizes. Returns "FALSE" if
# any subcategory sample size is lower than 5
sum(table(noise_MDA_subset$Tissues) < 5) == 0
sum(table(noise_GSH_subset$Tissues) < 5) == 0
sum(table(noise_CAT_subset$Tissues) < 5) == 0
sum(table(noise_SOD_subset$Tissues) < 5) == 0


##################################################
# Noise ~ MDA ~ Tissues
##################################################

mod_noise_MDA_metareg <- metafor::rma.mv(yi, # Effect size
                                         vi, # Variance
                                         mods = ~ Tissues,
                                         random = list(~ 1 | study,
                                                       ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                         # model, rather than an Equal Effects (EE) model
                                         tdist = TRUE, # t-distribution more accurate
                                         # than default Z-distribution
                                         data = noise_MDA_subset,
                                         method = "REML")


##################################################
# Noise ~ GSH ~ Tissues
##################################################

# mod_noise_GSH_metareg <- metafor::rma.mv(yi, # Effect size
#                                          vi, # Variance
#                                          mods = ~ Tissues,
#                                          random = list(~ 1 | study,
#                                                        ~ 1 | ES_ID), # ES_ID to fit a Random Effects
#                                          # model, rather than an Equal Effects (EE) model
#                                          tdist = TRUE, # t-distribution more accurate
#                                          # than default Z-distribution
#                                          data = noise_GSH_subset,
#                                          method = "REML")


##################################################
# Noise ~ CAT ~ Tissues
##################################################

noise_CAT_subset_for_metareg <- noise_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(n() >= 5) %>% # low sample size filter
  base::droplevels(noise_CAT$Tissues) %>%
  as.data.frame()

mod_noise_CAT_metareg <- metafor::rma.mv(yi, # Effect size
                                         vi, # Variance
                                         mods = ~ Tissues,
                                         random = list(~ 1 | study,
                                                       ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                         # model, rather than an Equal Effects (EE) model
                                         tdist = TRUE, # t-distribution more accurate
                                         # than default Z-distribution
                                         data = noise_CAT_subset_for_metareg,
                                         method = "REML")


##################################################
# Noise ~ SOD ~ Tissues
##################################################

mod_noise_SOD_metareg <- metafor::rma.mv(yi, # Effect size
                                         vi, # Variance
                                         mods = ~ Tissues,
                                         random = list(~ 1 | study,
                                                       ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                         # model, rather than an Equal Effects (EE) model
                                         tdist = TRUE, # t-distribution more accurate
                                         # than default Z-distribution
                                         data = noise_SOD_subset,
                                         method = "REML")


##################################################
# Testing model assumptions
##################################################

# # Noise ~ MDA ~ Tissues
# stats::profile(mod_noise_MDA_metareg)
# 
# # Noise ~ GSH ~ Tissues
# stats::profile(mod_noise_GSH_metareg)
# 
# # Noise ~ CAT ~ Tissues
# stats::profile(mod_noise_CAT_metareg)
# 
# # Noise ~ SOD ~ Tissues
# stats::profile(mod_noise_SOD_metareg)


##################################################
# Results
##################################################

# Noise ~ MDA ~ Tissues
summary(mod_noise_MDA_metareg)

# # Noise ~ GSH ~ Tissues
# summary(mod_noise_GSH_metareg)

# Noise ~ CAT ~ Tissues
summary(mod_noise_CAT_metareg)

# Noise ~ SOD ~ Tissues
summary(mod_noise_SOD_metareg)


##################################################
# SELECTION MODELS
##################################################


##################################################
# Noise ~ MDA
##################################################

mod_noise_MDA_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                   vi, # Variance
                                                   data = noise_MDA_subset,
                                                   test = "t",
                                                   method = "REML")

mod_noise_MDA_selmodel <- metafor::selmodel(mod_noise_MDA_without_study_ID,
                                            type = "stepfun",
                                            alternative = "greater",
                                            steps = c(0.001))


##################################################
# Noise ~ GSH
##################################################

mod_noise_GSH_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                   vi, # Variance
                                                   data = noise_GSH_subset,
                                                   test = "t",
                                                   method = "REML")

mod_noise_GSH_selmodel <- metafor::selmodel(mod_noise_GSH_without_study_ID,
                                            type = "stepfun",
                                            alternative = "greater",
                                            steps = c(0.9999))


##################################################
# Noise ~ CAT
##################################################

mod_noise_CAT_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                   vi, # Variance
                                                   data = noise_CAT_subset,
                                                   test = "t",
                                                   method = "REML")

mod_noise_CAT_selmodel <- metafor::selmodel(mod_noise_CAT_without_study_ID,
                                            type = "stepfun",
                                            alternative = "greater",
                                            steps = c(0.900))


##################################################
# Noise ~ SOD
##################################################

mod_noise_SOD_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                   vi, # Variance
                                                   data = noise_SOD_subset,
                                                   test = "t",
                                                   method = "REML")

mod_noise_SOD_selmodel <- metafor::selmodel(mod_noise_SOD_without_study_ID,
                                            type = "stepfun",
                                            alternative = "greater",
                                            steps = c(0.990))


##################################################
# Testing model assumptions
##################################################

# # Noise ~ MDA
# stats::profile(mod_noise_MDA_selmodel)
# 
# # Noise ~ GSH
# stats::profile(mod_noise_GSH_selmodel)
# 
# # Noise ~ CAT
# stats::profile(mod_noise_CAT_selmodel)
# 
# # Noise ~ SOD
# stats::profile(mod_noise_SOD_selmodel)


##################################################
# Results
##################################################

# Noise ~ MDA
summary(mod_noise_MDA_selmodel)
# metafor::plot.rma.uni.selmodel(mod_noise_MDA_selmodel)

# Noise ~ GSH
summary(mod_noise_GSH_selmodel)
# metafor::plot.rma.uni.selmodel(mod_noise_GSH_selmodel)

# Noise ~ CAT
summary(mod_noise_CAT_selmodel)
# metafor::plot.rma.uni.selmodel(mod_noise_CAT_selmodel)

# Noise ~ SOD
summary(mod_noise_SOD_selmodel)
# metafor::plot.rma.uni.selmodel(mod_noise_SOD_selmodel)


##################################################
# SUB-ANALYSES - ONE PER TAXON / TISSUE
##################################################


##################################################
# Noise ~ MDA
##################################################

# Hippocampus
noise_MDA_hippocampus <- noise_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Hippocampus") %>%
  as.data.frame()

mod_noise_MDA_hippocampus <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = noise_MDA_hippocampus,
                                             method = "REML")

# Brain
noise_MDA_brain <- noise_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Brain") %>%
  as.data.frame()

mod_noise_MDA_brain <- metafor::rma.mv(yi, # Effect size
                                       vi, # Variance
                                       random = list(~ 1 | study,
                                                     ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                       # model, rather than an Equal Effects (EE) model
                                       tdist = TRUE, # t-distribution more accurate
                                       # than default Z-distribution
                                       data = noise_MDA_brain,
                                       method = "REML")

# Plasma
noise_MDA_plasma <- noise_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Plasma") %>%
  as.data.frame()

mod_noise_MDA_plasma <- metafor::rma.mv(yi, # Effect size
                                        vi, # Variance
                                        random = list(~ 1 | study,
                                                      ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                        # model, rather than an Equal Effects (EE) model
                                        tdist = TRUE, # t-distribution more accurate
                                        # than default Z-distribution
                                        data = noise_MDA_plasma,
                                        method = "REML")


##################################################
# Noise ~ GSH
##################################################

# Hippocampus
noise_GSH_hippocampus <- noise_GSH %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Hippocampus") %>% 
  as.data.frame()

mod_noise_GSH_hippocampus <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = noise_GSH_hippocampus,
                                             method = "REML")


##################################################
# Noise ~ CAT
##################################################

# Hippocampus
noise_CAT_hippocampus <- noise_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Hippocampus") %>%
  as.data.frame()

mod_noise_CAT_hippocampus <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = noise_CAT_hippocampus,
                                             method = "REML")

# Brain
noise_CAT_brain <- noise_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Brain") %>%
  as.data.frame()

mod_noise_CAT_brain <- metafor::rma.mv(yi, # Effect size
                                       vi, # Variance
                                       random = list(~ 1 | study,
                                                     ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                       # model, rather than an Equal Effects (EE) model
                                       tdist = TRUE, # t-distribution more accurate
                                       # than default Z-distribution
                                       data = noise_CAT_brain,
                                       method = "REML")


##################################################
# Noise ~ SOD
##################################################

# Hippocampus
noise_SOD_hippocampus <- noise_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Hippocampus") %>%
  as.data.frame()

mod_noise_SOD_hippocampus <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = noise_SOD_hippocampus,
                                             method = "REML")

# Brain
noise_SOD_brain <- noise_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Brain") %>%
  as.data.frame()

mod_noise_SOD_brain <- metafor::rma.mv(yi, # Effect size
                                       vi, # Variance
                                       random = list(~ 1 | study,
                                                     ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                       # model, rather than an Equal Effects (EE) model
                                       tdist = TRUE, # t-distribution more accurate
                                       # than default Z-distribution
                                       data = noise_SOD_brain,
                                       method = "REML")

# Plasma
noise_SOD_plasma <- noise_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Tissues) %>%
  stats::na.omit() %>% dplyr::group_by(Tissues) %>%
  dplyr::filter(Tissues == "Plasma") %>%
  as.data.frame()

mod_noise_SOD_plasma <- metafor::rma.mv(yi, # Effect size
                                        vi, # Variance
                                        random = list(~ 1 | study,
                                                      ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                        # model, rather than an Equal Effects (EE) model
                                        tdist = TRUE, # t-distribution more accurate
                                        # than default Z-distribution
                                        data = noise_SOD_plasma,
                                        method = "REML")


##################################################
# Testing model assumptions
##################################################

# # Noise ~ MDA
# stats::profile(mod_noise_MDA_hippocampus)
# stats::profile(mod_noise_MDA_brain)
# stats::profile(mod_noise_MDA_plasma)
# 
# # Noise ~ GSH
# stats::profile(mod_noise_GSH_hippocampus)
# 
# # Noise ~ CAT
# stats::profile(mod_noise_CAT_hippocampus)
# stats::profile(mod_noise_CAT_brain)
# 
# # Noise ~ SOD
# stats::profile(mod_noise_SOD_hippocampus)
# stats::profile(mod_noise_SOD_brain)
# stats::profile(mod_noise_SOD_plasma)


##################################################
# Results
##################################################

# Noise ~ MDA
summary(mod_noise_MDA_hippocampus)
summary(mod_noise_MDA_brain)
summary(mod_noise_MDA_plasma)

# Noise ~ GSH
summary(mod_noise_GSH_hippocampus)

# Noise ~ CAT
summary(mod_noise_CAT_hippocampus)
summary(mod_noise_CAT_brain)

# Noise ~ SOD
summary(mod_noise_SOD_hippocampus)
summary(mod_noise_SOD_brain)
summary(mod_noise_SOD_plasma)








##################################################
##################################################
# Predation exposure
##################################################
##################################################

# Load data file
predation <- openxlsx::read.xlsx("New_data_file_from_Michael_2024_-_modified_by_Simon.xlsx",
                                 sheet = 3,
                                 check.names = TRUE)

# Study number as character vector
predation$study <- as.character(predation$study)

# Effect size ID (one per row)
predation$ES_ID <- seq.int(nrow(predation))

# Treating factors as factors
predation$Taxon <- as.factor(predation$test)

# Treating sample size as a numeric variable
predation$n_controls <- as.numeric(predation$n_controls)
predation$n_exp <- as.numeric(predation$n_exp)


##################################################
# Calculating effect sizes
##################################################

# Calculating Hedges' g's and effect size variance
predation_MDA <- metafor::escalc(n1i = n_exp,
                                 n2i = n_controls,
                                 m1i = mean_exp_MDA,
                                 m2i = mean_controls_MDA,
                                 sd1i = sd_exp_MDA,
                                 sd2i = sd_controls_MDA,
                                 data = predation,
                                 measure = "SMD",
                                 append = TRUE)

# Subsetting
predation_MDA_subset <- predation_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% 
  base::droplevels(predation_MDA$Taxon)

# Calculating Hedges' g's and effect size variance
predation_GSH <- metafor::escalc(n1i = n_exp,
                                 n2i = n_controls,
                                 m1i = mean_exp_GSH,
                                 m2i = mean_controls_GSH,
                                 sd1i = sd_exp_GSH,
                                 sd2i = sd_controls_GSH,
                                 data = predation,
                                 measure = "SMD",
                                 append = TRUE)

# Subsetting
predation_GSH_subset <- predation_GSH %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% 
  base::droplevels(predation_GSH$Taxon)

# Calculating Hedges' g's and effect size variance
predation_CAT <- metafor::escalc(n1i = n_exp,
                                 n2i = n_controls,
                                 m1i = mean_exp_CAT,
                                 m2i = mean_controls_CAT,
                                 sd1i = sd_exp_CAT,
                                 sd2i = sd_controls_CAT,
                                 data = predation,
                                 measure = "SMD",
                                 append = TRUE)

# Subsetting
predation_CAT_subset <- predation_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% 
  base::droplevels(predation_CAT$Taxon)

# Calculating Hedges' g's and effect size variance
predation_SOD <- metafor::escalc(n1i = n_exp,
                                 n2i = n_controls,
                                 m1i = mean_exp_SOD,
                                 m2i = mean_controls_SOD,
                                 sd1i = sd_exp_SOD,
                                 sd2i = sd_controls_SOD,
                                 data = predation,
                                 measure = "SMD",
                                 append = TRUE)

# Subsetting
predation_SOD_subset <- predation_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% 
  base::droplevels(predation_SOD$Taxon)


##################################################
# POOLED TAXA
##################################################


##################################################
# Predation ~ MDA
##################################################

mod_predation_MDA <- metafor::rma.mv(yi, # Effect size
                                     vi, # Variance
                                     random = list(~ 1 | study,
                                                   ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                     # model, rather than an Equal Effects (EE) model
                                     tdist = TRUE, # t-distribution more accurate
                                     # than default Z-distribution
                                     data = predation_MDA_subset,
                                     method = "REML")


##################################################
# Predation ~ GSH
##################################################

# mod_predation_GSH <- metafor::rma.mv(yi, # Effect size
#                                      vi, # Variance
#                                      random = list(~ 1 | study,
#                                                    ~ 1 | ES_ID), # ES_ID to fit a Random Effects
#                                      # model, rather than an Equal Effects (EE) model
#                                      tdist = TRUE, # t-distribution more accurate
#                                      # than default Z-distribution
#                                      data = predation_GSH_subset,
#                                      method = "REML")


##################################################
# Predation ~ CAT
##################################################

mod_predation_CAT <- metafor::rma.mv(yi, # Effect size
                                     vi, # Variance
                                     random = list(~ 1 | study,
                                                   ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                     # model, rather than an Equal Effects (EE) model
                                     tdist = TRUE, # t-distribution more accurate
                                     # than default Z-distribution
                                     data = predation_CAT_subset,
                                     method = "REML")


##################################################
# Predation ~ SOD
##################################################

mod_predation_SOD <- metafor::rma.mv(yi, # Effect size
                                     vi, # Variance
                                     random = list(~ 1 | study,
                                                   ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                     # model, rather than an Equal Effects (EE) model
                                     tdist = TRUE, # t-distribution more accurate
                                     # than default Z-distribution
                                     data = predation_SOD_subset,
                                     method = "REML")


##################################################
# Testing model assumptions
##################################################

# # Predation ~ MDA
# stats::profile(mod_predation_MDA)
# 
# # # Predation ~ GSH
# # stats::profile(mod_predation_GSH)
# 
# # Predation ~ CAT
# stats::profile(mod_predation_CAT)
# 
# # Predation ~ SOD
# stats::profile(mod_predation_SOD)


##################################################
# Results
##################################################

# Predation ~ MDA
summary(mod_predation_MDA)

# # Predation ~ GSH
# summary(mod_predation_GSH)

# Predation ~ CAT
summary(mod_predation_CAT)

# Predation ~ SOD
summary(mod_predation_SOD)


##################################################
# METAREGRESSION
##################################################

# Checking sample sizes. Returns "FALSE" if
# any subcategory sample size is lower than 5
sum(table(predation_MDA_subset$Taxon) < 5) == 0
sum(table(predation_GSH_subset$Taxon) < 5) == 0
sum(table(predation_CAT_subset$Taxon) < 5) == 0
sum(table(predation_SOD_subset$Taxon) < 5) == 0


##################################################
# Predation ~ MDA ~ Taxon
##################################################

mod_predation_MDA_metareg <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             mods = ~ Taxon,
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = predation_MDA_subset,
                                             method = "REML")


##################################################
# Predation ~ GSH ~ Taxon
##################################################

# mod_predation_GSH_metareg <- metafor::rma.mv(yi, # Effect size
#                                              vi, # Variance
#                                              mods = ~ Taxon,
#                                              random = list(~ 1 | study,
#                                                            ~ 1 | ES_ID), # ES_ID to fit a Random Effects
#                                              # model, rather than an Equal Effects (EE) model
#                                              tdist = TRUE, # t-distribution more accurate
#                                              # than default Z-distribution
#                                              data = predation_GSH_subset,
#                                              method = "REML")


##################################################
# Predation ~ CAT ~ Taxon
##################################################

mod_predation_CAT_metareg <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             mods = ~ Taxon,
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = predation_CAT_subset,
                                             method = "REML")


##################################################
# Predation ~ SOD ~ Taxon
##################################################

predation_SOD_subset_for_metareg <- predation_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% group_by(Taxon) %>%
  filter(n() >= 5) %>% # low sample size filter
  base::droplevels(predation_SOD$Taxon) %>%
  as.data.frame()

mod_predation_SOD_metareg <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             mods = ~ Taxon,
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = predation_SOD_subset_for_metareg,
                                             method = "REML")


##################################################
# Testing model assumptions
##################################################

# # Predation ~ MDA ~ Taxon
# stats::profile(mod_predation_MDA_metareg)
# 
# # # Predation ~ GSH ~ Taxon
# # stats::profile(mod_predation_GSH_metareg)
# 
# # Predation ~ CAT ~ Taxon
# stats::profile(mod_predation_CAT_metareg)
# 
# # Predation ~ SOD ~ Taxon
# stats::profile(mod_predation_SOD_metareg)


##################################################
# Results
##################################################

# Predation ~ MDA ~ Taxon
summary(mod_predation_MDA_metareg)

# # Predation ~ GSH ~ Taxon
# summary(mod_predation_GSH_metareg)

# Predation ~ CAT ~ Taxon
summary(mod_predation_CAT_metareg)

# Predation ~ SOD ~ Taxon
summary(mod_predation_SOD_metareg)


##################################################
# SELECTION MODELS
##################################################


##################################################
# Predation ~ MDA
##################################################

mod_predation_MDA_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                       vi, # Variance
                                                       data = predation_MDA_subset,
                                                       test = "t",
                                                       method = "REML")

mod_predation_MDA_selmodel <- metafor::selmodel(mod_predation_MDA_without_study_ID,
                                                type = "stepfun",
                                                alternative = "greater",
                                                steps = c(0.100))


##################################################
# Predation ~ GSH
##################################################

# mod_predation_GSH_without_study_ID <- metafor::rma.uni(yi, # Effect size
#                                                        vi, # Variance
#                                                        data = predation_GSH_subset,
#                                                        test = "t",
#                                                        method = "REML")
# 
# mod_predation_GSH_selmodel <- metafor::selmodel(mod_predation_GSH_without_study_ID,
#                                                 type = "stepfun",
#                                                 alternative = "greater",
#                                                 steps = c(0.025))


##################################################
# Predation ~ CAT
##################################################

mod_predation_CAT_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                       vi, # Variance
                                                       data = predation_CAT_subset,
                                                       test = "t",
                                                       method = "REML")

mod_predation_CAT_selmodel <- metafor::selmodel(mod_predation_CAT_without_study_ID,
                                                type = "stepfun",
                                                alternative = "greater",
                                                steps = c(0.250))


##################################################
# Predation ~ SOD
##################################################

mod_predation_SOD_without_study_ID <- metafor::rma.uni(yi, # Effect size
                                                       vi, # Variance
                                                       data = predation_SOD_subset,
                                                       test = "t",
                                                       method = "REML")

mod_predation_SOD_selmodel <- metafor::selmodel(mod_predation_SOD_without_study_ID,
                                                type = "stepfun",
                                                alternative = "greater",
                                                steps = c(0.400))


##################################################
# Testing model assumptions
##################################################

# # Predation ~ MDA
# stats::profile(mod_predation_MDA_selmodel)
# 
# # # Predation ~ GSH
# # stats::profile(mod_predation_GSH_selmodel)
# 
# # Predation ~ CAT
# stats::profile(mod_predation_CAT_selmodel)
# 
# # Predation ~ SOD
# stats::profile(mod_predation_SOD_selmodel)


##################################################
# Results
##################################################

# Predation ~ MDA
summary(mod_predation_MDA_selmodel)
# metafor::plot.rma.uni.selmodel(mod_predation_MDA_selmodel)

# # Predation ~ GSH
# summary(mod_predation_GSH_selmodel)
# metafor::plot.rma.uni.selmodel(mod_predation_GSH_selmodel)

# Predation ~ CAT
summary(mod_predation_CAT_selmodel)
# metafor::plot.rma.uni.selmodel(mod_predation_CAT_selmodel)

# Predation ~ SOD
summary(mod_predation_SOD_selmodel)
# metafor::plot.rma.uni.selmodel(mod_predation_SOD_selmodel)


##################################################
# SUB-ANALYSES - ONE PER TAXON / TISSUE
##################################################


##################################################
# Predation ~ MDA
##################################################

# Insects
predation_MDA_insects <- predation_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% dplyr::group_by(Taxon) %>%
  dplyr::filter(Taxon == "Insects") %>%
  as.data.frame()

mod_predation_MDA_insects <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = predation_MDA_insects,
                                             method = "REML")

# Amphibians
predation_MDA_amphibians <- predation_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% dplyr::group_by(Taxon) %>%
  dplyr::filter(Taxon == "Amphibians") %>%
  as.data.frame()

mod_predation_MDA_amphibians <- metafor::rma.mv(yi, # Effect size
                                                vi, # Variance
                                                random = list(~ 1 | study,
                                                              ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                                # model, rather than an Equal Effects (EE) model
                                                tdist = TRUE, # t-distribution more accurate
                                                # than default Z-distribution
                                                data = predation_MDA_amphibians,
                                                method = "REML")

# Crustaceans
predation_MDA_crustaceans <- predation_MDA %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% dplyr::group_by(Taxon) %>%
  dplyr::filter(Taxon == "Crustaceans") %>%
  as.data.frame()

mod_predation_MDA_crustaceans <- metafor::rma.mv(yi, # Effect size
                                                 vi, # Variance
                                                 random = list(~ 1 | study,
                                                               ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                                 # model, rather than an Equal Effects (EE) model
                                                 tdist = TRUE, # t-distribution more accurate
                                                 # than default Z-distribution
                                                 data = predation_MDA_crustaceans,
                                                 method = "REML")


##################################################
# Predation ~ CAT
##################################################

# Insects
predation_CAT_insects <- predation_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% dplyr::group_by(Taxon) %>%
  dplyr::filter(Taxon == "Insects") %>%
  as.data.frame()

mod_predation_CAT_insects <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = predation_CAT_insects,
                                             method = "REML")

# Amphibians
predation_CAT_amphibians <- predation_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% dplyr::group_by(Taxon) %>%
  dplyr::filter(Taxon == "Amphibians") %>%
  as.data.frame()

mod_predation_CAT_amphibians <- metafor::rma.mv(yi, # Effect size
                                                vi, # Variance
                                                random = list(~ 1 | study,
                                                              ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                                # model, rather than an Equal Effects (EE) model
                                                tdist = TRUE, # t-distribution more accurate
                                                # than default Z-distribution
                                                data = predation_CAT_amphibians,
                                                method = "REML")

# Crustaceans
predation_CAT_crustaceans <- predation_CAT %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% dplyr::group_by(Taxon) %>%
  dplyr::filter(Taxon == "Crustaceans") %>%
  as.data.frame()

mod_predation_CAT_crustaceans <- metafor::rma.mv(yi, # Effect size
                                                 vi, # Variance
                                                 random = list(~ 1 | study,
                                                               ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                                 # model, rather than an Equal Effects (EE) model
                                                 tdist = TRUE, # t-distribution more accurate
                                                 # than default Z-distribution
                                                 data = predation_CAT_crustaceans,
                                                 method = "REML")


##################################################
# Predation ~ SOD
##################################################

# Insects
predation_SOD_insects <- predation_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% dplyr::group_by(Taxon) %>%
  dplyr::filter(Taxon == "Insects") %>%
  as.data.frame()

mod_predation_SOD_insects <- metafor::rma.mv(yi, # Effect size
                                             vi, # Variance
                                             random = list(~ 1 | study,
                                                           ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                             # model, rather than an Equal Effects (EE) model
                                             tdist = TRUE, # t-distribution more accurate
                                             # than default Z-distribution
                                             data = predation_SOD_insects,
                                             method = "REML")

# Amphibians
predation_SOD_amphibians <- predation_SOD %>% 
  dplyr::select(yi, vi, ES_ID, study, Taxon) %>%
  stats::na.omit() %>% dplyr::group_by(Taxon) %>%
  dplyr::filter(Taxon == "Amphibians") %>%
  as.data.frame()

mod_predation_SOD_amphibians <- metafor::rma.mv(yi, # Effect size
                                                vi, # Variance
                                                random = list(~ 1 | study,
                                                              ~ 1 | ES_ID), # ES_ID to fit a Random Effects
                                                # model, rather than an Equal Effects (EE) model
                                                tdist = TRUE, # t-distribution more accurate
                                                # than default Z-distribution
                                                data = predation_SOD_amphibians,
                                                method = "REML")


##################################################
# Testing model assumptions
##################################################

# # Predation ~ MDA
# stats::profile(mod_predation_MDA_insects)
# stats::profile(mod_predation_MDA_amphibians)
# stats::profile(mod_predation_MDA_crustaceans)
# 
# # Predation ~ CAT
# stats::profile(mod_predation_CAT_insects)
# stats::profile(mod_predation_CAT_amphibians)
# stats::profile(mod_predation_CAT_crustaceans)
# 
# # Predation ~ SOD
# stats::profile(mod_predation_SOD_insects)
# stats::profile(mod_predation_SOD_amphibians)


##################################################
# Results
##################################################

# Predation ~ MDA
summary(mod_predation_MDA_insects)
summary(mod_predation_MDA_amphibians)
summary(mod_predation_MDA_crustaceans)

# Predation ~ CAT
summary(mod_predation_CAT_insects)
summary(mod_predation_CAT_amphibians)
summary(mod_predation_CAT_crustaceans)

# Predation ~ SOD
summary(mod_predation_SOD_insects)
summary(mod_predation_SOD_amphibians)

