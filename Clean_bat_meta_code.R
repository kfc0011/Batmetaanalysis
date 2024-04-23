###Meta Results

#install.packages("readxl")
library(readxl)
setwd("C:\\Users\\kfc0011\\OneDrive - Auburn University\\Bat ID Meta-analysis")
datum <- read_excel("C:/Users/kfc0011/OneDrive - Auburn University/Bat ID Meta-analysis/Bat_meta_data_full.xlsx", 
                    sheet = "Included_studies")
View(datum)

#install.packages("metafor")
library(metafor)

#install.packages("mice")
library(mice)

# install.packages('pacman')
# rm(list = ls())
# devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)
# pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans,
#                ape, phytools, flextable)
library(orchaRd)

#install.packages("meta")
library(meta)

#install.packages("writexl")
library(writexl)

#print information on the package versions used for these calculations
sessionInfo()

# # R version 4.3.2 (2023-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22631)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    
# 
# time zone: America/Chicago
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] writexl_1.5.0       meta_7.0-0          orchaRd_2.0         mice_3.16.0         metafor_4.6-0       numDeriv_2016.8-1.1
# [7] metadat_1.2-0       Matrix_1.6-1.1      readxl_1.4.3       
# 
# loaded via a namespace (and not attached):
#   [1] shape_1.4.6.1      htmlwidgets_1.6.4  devtools_2.4.5     remotes_2.4.2.1    CompQuadForm_1.4.3 lattice_0.21-9    
# [7] tzdb_0.4.0         mathjaxr_1.6-0     vctrs_0.6.5        tools_4.3.2        generics_0.1.3     tibble_3.2.1      
# [13] fansi_1.0.6        pan_1.9            pacman_0.5.1       pkgconfig_2.0.3    jomo_2.7-6         lifecycle_1.0.4   
# [19] compiler_4.3.2     stringr_1.5.1      codetools_0.2-19   httpuv_1.6.14      htmltools_0.5.7    usethis_2.2.2     
# [25] glmnet_4.1-8       nloptr_2.0.3       later_1.3.2        pillar_1.9.0       urlchecker_1.0.1   tidyr_1.3.1       
# [31] MASS_7.3-60        ellipsis_0.3.2     rsconnect_1.2.2    cachem_1.0.8       sessioninfo_1.2.2  iterators_1.0.14  
# [37] rpart_4.1.21       boot_1.3-28.1      foreach_1.5.2      mitml_0.4-5        nlme_3.1-163       mime_0.12         
# [43] tidyselect_1.2.1   digest_0.6.34      stringi_1.8.3      dplyr_1.1.4        purrr_1.0.2        splines_4.3.2     
# [49] fastmap_1.1.1      grid_4.3.2         cli_3.6.2          magrittr_2.0.3     survival_3.5-7     pkgbuild_1.4.3    
# [55] utf8_1.2.4         broom_1.0.5        readr_2.1.5        promises_1.2.1     backports_1.4.1    nnet_7.3-19       
# [61] lme4_1.1-35.2      cellranger_1.1.0   hms_1.1.3          memoise_2.0.1      shiny_1.8.0        miniUI_0.1.1.1    
# [67] profvis_0.3.8      rlang_1.1.3        Rcpp_1.0.12        xtable_1.8-4       glue_1.7.0         xml2_1.3.6        
# [73] pkgload_1.3.4      minqa_1.2.6        rstudioapi_0.16.0  R6_2.5.1           fs_1.6.3          

# set up prediction matrix
predMatrix <- make.predictorMatrix(datum)
predMatrix

# remove unwanted variables from prediction matrix
predMatrix[,"perc_acc"] <- 0
predMatrix["perc_acc",] <- 0
predMatrix["Samp_var",] <- 0
predMatrix


# impute data
imp <- mice(datum, print=FALSE, method = "mean", m = 1, maxit = 1)
complete(imp)

#selected imputed sample size calls for es #36-39 and 42, 2374 calls

# calculate individual effect sizes using logit transform
ies <- escalc(xi=xi, ni = Sample_size_calls, data = datum, measure = "PLO")
View(ies)

# weighted meta-analysis (aka pooled effect size, pes)
pes.logit=rma(yi,vi,data=ies,method="REML") 
pes.logit

# Confidence intervals
confint(pes.logit, digits = 4)
# estimate    ci.lb    ci.ub 
# tau^2    0.8539   0.5692   1.3726 
# tau      0.9241   0.7545   1.1716 
# I^2(%)  99.6683  99.5032  99.7934 
# H^2    301.4709 201.2958 483.9811 

#Inverse of logit transformation 
pes <- predict(pes.logit, transf = transf.ilogit) 
pes

# pred  ci.lb  ci.ub  pi.lb  pi.ub 
# 0.7460 0.6897 0.7951 0.3197 0.9483 

#make orchard plot
model_results <- orchaRd::mod_results(pes.logit, mod = "1", at = NULL, group = "Study_Number")
model_results
# name estimate   lowerCL  upperCL    lowerPR  upperPR
# 1 Intrcpt 1.077279 0.7988692 1.355689 -0.7551449 2.909703

orchaRd::orchard_plot(pes.logit, mod = "1", group = "Study_Number", xlab = "Log Odds", 
                      transfm_ll = "none", twig.size = 0.5, trunk.size = 1)

forest(pes.logit)
funnel(pes.logit)

#metaregression
metareg.moderators <- rma(yi, vi, data = data, method="REML", mods =~Year + Sample_size_calls + n_species + n_myotis)
metareg.moderators

# Mixed-Effects Model (k = 44; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of residual heterogeneity):     0.0508 (SE = 0.3239)
# tau (square root of estimated tau^2 value):             0.2254
# I^2 (residual heterogeneity / unaccounted variability): 3.48%
# H^2 (unaccounted variability / sampling variability):   1.04
# R^2 (amount of heterogeneity accounted for):            97.31%
# 
# Test for Residual Heterogeneity:
#   QE(df = 39) = 39.9926, p-val = 0.4259
# 
# Test of Moderators (coefficients 2:5):
#   QM(df = 4) = 58.8225, p-val < .0001
# 
# Model Results:
#   
#   estimate       se     zval    pval    ci.lb     ci.ub      
# intrcpt            133.1943  63.6149   2.0938  0.0363   8.5115  257.8772    * 
#   Year                -0.0689   0.0316  -2.1799  0.0293  -0.1309   -0.0070    * 
#   Sample_size_calls   -0.0002   0.0000  -5.2602  <.0001  -0.0003   -0.0001  *** 
#   n_species            0.0194   0.0634   0.3056  0.7599  -0.1048    0.1435      
# n_myotis            -0.2166   0.1603  -1.3519  0.1764  -0.5307    0.0974      
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#moderator analysis- analysis type
results.atype<-rma.mv(yi,vi,mods=~Analysis_type,data=ies,method="REML",random=~1|Study_Number)
results.atype

# Multivariate Meta-Analysis Model (k = 44; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.8255  0.9085     44     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 41) = 8348.4282, p-val < .0001
# 
# Test of Moderators (coefficients 2:3):
#   QM(df = 2) = 3.4054, p-val = 0.1822
# 
# Model Results:
#   
#   estimate      se     zval    pval    ci.lb   ci.ub      
# intrcpt                      1.4131  0.2514   5.6207  <.0001   0.9203  1.9058  *** 
#   Analysis_typeAlgorithm      -0.6705  0.3645  -1.8394  0.0659  -1.3850  0.0439    . 
# Analysis_typeMathematical   -0.3611  0.3328  -1.0850  0.2779  -1.0133  0.2912      
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#moderator analysis- file type
results.ftype<-rma.mv(yi,vi,mods=~File_type,data=ies,method="REML",random=~1|Study_Number)
results.ftype

# Multivariate Meta-Analysis Model (k = 44; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.7855  0.8863     44     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 41) = 10598.3810, p-val < .0001
# 
# Test of Moderators (coefficients 2:3):
#   QM(df = 2) = 5.6335, p-val = 0.0598
# 
# Model Results:
#   
#   estimate      se    zval    pval    ci.lb   ci.ub     
# intrcpt                    0.6181  0.2378  2.5993  0.0093   0.1520  1.0842  ** 
#   File_typeTime expansion    0.6198  0.3510  1.7657  0.0775  -0.0682  1.3078   . 
# File_typeZero crossing     0.7278  0.3226  2.2559  0.0241   0.0955  1.3602   * 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#moderator analysis- continent
results.cont<-rma.mv(yi,vi,mods=~Continent,data=ies,method="REML",random=~1|Study_Number)
results.cont

# Multivariate Meta-Analysis Model (k = 44; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.8257  0.9087     44     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 41) = 10735.4116, p-val < .0001
# 
# Test of Moderators (coefficients 2:3):
#   QM(df = 2) = 3.3398, p-val = 0.1883
# 
# Model Results:
#   
#   estimate      se    zval    pval    ci.lb   ci.ub      
# intrcpt                   0.8666  0.1865  4.6463  <.0001   0.5010  1.2321  *** 
#   ContinentNorth America    0.4468  0.2860  1.5626  0.1182  -0.1136  1.0073      
# ContinentSouth America    1.0793  0.9462  1.1407  0.2540  -0.7751  2.9338      
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#moderator analysis- country
results.count<-rma.mv(yi,vi,mods=~Country,data=ies,method="REML",random=~1|Study_Number)
results.count

# Multivariate Meta-Analysis Model (k = 35; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.6401  0.8000     35     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 28) = 5919.7835, p-val < .0001
# 
# Test of Moderators (coefficients 2:7):
#   QM(df = 6) = 13.3639, p-val = 0.0376
# 
# Model Results:
#   
#   estimate      se     zval    pval    ci.lb    ci.ub      
# intrcpt                  3.3777  0.8014   4.2149  <.0001   1.8070   4.9483  *** 
#   CountryChile            -1.4318  1.1476  -1.2476  0.2122  -3.6811   0.8175      
# CountryItaly            -2.8234  0.8973  -3.1465  0.0017  -4.5821  -1.0647   ** 
#   CountrySweden           -2.5470  0.9254  -2.7523  0.0059  -4.3608  -0.7332   ** 
#   CountrySwitzerland      -2.2791  1.1343  -2.0092  0.0445  -4.5023  -0.0558    * 
#   CountryUnited Kingdom   -1.7315  0.8580  -2.0181  0.0436  -3.4131  -0.0499    * 
#   CountryUnited States    -2.1918  0.8256  -2.6548  0.0079  -3.8099  -0.5737   ** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#moderator analysis- model used for identification
results.model<-rma.mv(yi,vi,mods=~,data=ies,method="REML",random=~1|Study_Number)
results.model

# Multivariate Meta-Analysis Model (k = 43; method: REML)
# 
# Variance Components: none
# 
# Test for Residual Heterogeneity:
#   QE(df = 27) = 3984.5017, p-val < .0001
# 
# Test of Moderators (coefficients 2:16):
#   QM(df = 15) = 3884.1603, p-val < .0001
# 
# Model Results:
#   
#   estimate      se      zval    pval    ci.lb    ci.ub      
# intrcpt               0.7012  0.0296   23.6871  <.0001   0.6432   0.7592  *** 
#   ModelBatClassify      1.4524  0.0723   20.0832  <.0001   1.3106   1.5941  *** 
#   ModelBatExplorer     -1.5013  0.0533  -28.1443  <.0001  -1.6059  -1.3968  *** 
#   ModelBatIdent         0.5071  0.0570    8.8892  <.0001   0.3953   0.6189  *** 
#   ModelBCID            -0.2539  0.0334   -7.5924  <.0001  -0.3194  -0.1883  *** 
#   ModelCA              -0.7032  0.1043   -6.7426  <.0001  -0.9076  -0.4988  *** 
#   ModelCART            -0.7072  0.1043   -6.7809  <.0001  -0.9116  -0.5028  *** 
#   ModelDFA              0.7197  0.0367   19.6097  <.0001   0.6477   0.7916  *** 
#   ModelGARD            -0.7252  0.0827   -8.7637  <.0001  -0.8874  -0.5630  *** 
#   ModelGSE             -0.5167  0.0831   -6.2211  <.0001  -0.6794  -0.3539  *** 
#   ModelKaleidoscope    -0.0850  0.0328   -2.5931  0.0095  -0.1493  -0.0208   ** 
#   ModelNN              -0.1475  0.0855   -1.7240  0.0847  -0.3151   0.0202    . 
# ModelRF               1.1501  0.2766    4.1586  <.0001   0.6080   1.6921  *** 
#   ModelSonoChiro       -0.4248  0.0417  -10.1808  <.0001  -0.5066  -0.3430  *** 
#   ModelStudy created    0.9570  0.0752   12.7233  <.0001   0.8096   1.1045  *** 
#   ModelSVM              1.1204  0.1058   10.5944  <.0001   0.9131   1.3276  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1