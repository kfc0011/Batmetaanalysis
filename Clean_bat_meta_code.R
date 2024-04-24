###Meta Results

#install.packages("readxl")
library(readxl)
setwd("C:\\Users\\kfc0011\\OneDrive - Auburn University\\Bat ID Meta-analysis")
datum <- read_excel("C:\\Users\\kfc0011\\OneDrive - Auburn University\\Bat ID Meta-analysis\\Texts for class paper\\Bat_meta_data_final.xlsx", 
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

impMethod <- make.method(datum)
impMethod
imp <- mice(datum, print=FALSE, m=20, predictorMatrix=predMatrix, method="rf", seed=1)
complete(imp)
#selected imputed sample size calls for es #36-39 and 42 = 2374 calls

# calculate individual effect sizes using logit transform
ies <- escalc(xi=xi, ni = Sample_size_calls, data = datum, measure = "PLO")
View(ies)

# weighted meta-analysis (aka pooled effect size, pes)
pes.logit=rma(yi,vi,data=ies,method="REML") 
pes.logit

# Random-Effects Model (k = 47; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 0.8276 (SE = 0.1789)
# tau (square root of estimated tau^2 value):      0.9097
# I^2 (total heterogeneity / total variability):   99.66%
# H^2 (total variability / sampling variability):  296.65
# 
# Test for Heterogeneity:
#   Q(df = 46) = 12393.9215, p-val < .0001
# 
# Model Results:
#   
#   estimate      se    zval    pval   ci.lb   ci.ub      
# 1.1228  0.1352  8.3026  <.0001  0.8578  1.3879  *** 
#   
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Confidence intervals
confint(pes.logit, digits = 4)

# estimate    ci.lb    ci.ub 
# tau^2    0.8276   0.5585   1.3057 
# tau      0.9097   0.7473   1.1427 
# I^2(%)  99.6629  99.5013  99.7861 
# H^2    296.6455 200.5130 467.4474 

#Inverse of logit transformation 
pes <- predict(pes.logit, transf = transf.ilogit) 
pes

# pred  ci.lb  ci.ub  pi.lb  pi.ub 
# 0.7545 0.7022 0.8003 0.3363 0.9491

#make orchard plot
model_results <- orchaRd::mod_results(pes.logit, mod = "1", at = NULL, group = "Study_Number")
model_results

# name estimate   lowerCL  upperCL    lowerPR  upperPR
# 1 Intrcpt 1.122807 0.8577516 1.387863 -0.6797779 2.925393

orchaRd::orchard_plot(pes.logit, mod = "1", group = "Study_Number", xlab = "Log Odds", 
                      transfm = "none", twig.size = 0.5, trunk.size = 1)

forest(pes.logit)
funnel(pes.logit)

#classical Egger's test regression analysis
regtest(pes.logit)
# Regression Test for Funnel Plot Asymmetry
# 
# Model:     weighted regression with multiplicative dispersion
# Predictor: standard error
# 
# Test for Funnel Plot Asymmetry: t = 1.0108, df = 45, p = 0.3175
# Limit Estimate (as sei -> 0):   b = 0.7239 (CI: 0.3578, 1.0900)

ranktest(pes.logit)

# Rank Correlation Test for Funnel Plot Asymmetry
# 
# Kendall's tau = 0.0860, p = 0.4009

#metaregression
metareg.moderators <- rma(yi, vi, data = ies, method="REML", mods =~Year + Sample_size_calls + n_species + n_myotis)
metareg.moderators

# Mixed-Effects Model (k = 47; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of residual heterogeneity):     0.7861 (SE = 0.1785)
# tau (square root of estimated tau^2 value):             0.8866
# I^2 (residual heterogeneity / unaccounted variability): 99.60%
# H^2 (unaccounted variability / sampling variability):   251.88
# R^2 (amount of heterogeneity accounted for):            5.01%
# 
# Test for Residual Heterogeneity:
#   QE(df = 42) = 11163.1031, p-val < .0001
# 
# Test of Moderators (coefficients 2:5):
#   QM(df = 4) = 6.3092, p-val = 0.1772
# 
# Model Results:
#   
#   estimate       se     zvalAn    pval     ci.lb     ci.ub    
# intrcpt             74.6107  56.3318   1.3245  0.1853  -35.7976  185.0190    
# Year                -0.0364   0.0280  -1.2976  0.1944   -0.0913    0.0186    
# Sample_size_calls    0.0000   0.0000   1.0981  0.2722   -0.0000    0.0001    
# n_species           -0.0362   0.0396  -0.9147  0.3604   -0.1138    0.0414    
# n_myotis             0.0130   0.1026   0.1266  0.8993   -0.1882    0.2142    
# 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# Define the inverse logit function
inv_logit <- function(beta) {exp(beta) / (1 + exp(beta))}

# Apply the inverse logit function to effect sizes
metareg_inv_logit <- inv_logit(metareg.moderators[["beta"]])
metareg_inv_logit

# [,1]
# intrcpt           1.0000000
# Year              0.4909126
# Sample_size_calls 0.5000098
# n_species         0.4909449
# n_myotis          0.5032476

#moderator analysis- analysis type
results.atype<-rma.mv(yi,vi,mods=~Analysis_type -1,data=ies,method="REML",random=~1|Study_Number)
results.atype

# Multivariate Meta-Analysis Model (k = 47; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.7523  0.8674     47     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 45) = 10128.7498, p-val < .0001
# 
# Test of Moderators (coefficients 1:2):
#   QM(df = 2) = 80.7630, p-val < .0001
# 
# Model Results:
#   
#   estimate      se    zval    pval   ci.lb   ci.ub      
# Analysis_typeAI           1.5910  0.2408  6.6069  <.0001  1.1190  2.0629  *** 
#   Analysis_typeAlgorithm    0.9322  0.1530  6.0920  <.0001  0.6323  1.2322  *** 
#   
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Apply the inverse logit function to effect sizes
atype_inv_logit <- inv_logit(results.atype[["beta"]])
atype_inv_logit

# [,1]
# Analysis_typeAI        0.8307528
# Analysis_typeAlgorithm 0.7175287

#moderator analysis- file type
results.ftype<-rma.mv(yi,vi,mods=~File_type -1,data=ies,method="REML",random=~1|Study_Number)
results.ftype

# Multivariate Meta-Analysis Model (k = 47; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.7665  0.8755     47     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 44) = 11550.8703, p-val < .0001
# 
# Test of Moderators (coefficients 1:3):
#   QM(df = 3) = 79.6204, p-val < .0001
# 
# Model Results:
#   
#   estimate      se    zval    pval   ci.lb   ci.ub      
# File_typeFull spectrum     0.6946  0.2270  3.0594  0.0022  0.2496  1.1396   ** 
#   File_typeTime expansion    1.2375  0.2551  4.8505  <.0001  0.7375  1.7376  *** 
#   File_typeZero crossing     1.3922  0.2037  6.8361  <.0001  0.9930  1.7913  *** 
#   
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Apply the inverse logit function to effect sizes
file_inv_logit <- inv_logit(results.ftype[["beta"]])
file_inv_logit

# [,1]
# File_typeFull spectrum  0.6669862
# File_typeTime expansion 0.7751356
# File_typeZero crossing  0.8009397

#moderator analysis- continent
results.cont<-rma.mv(yi,vi,mods=~Continent -1,data=ies,method="REML",random=~1|Study_Number)
results.cont

# Multivariate Meta-Analysis Model (k = 47; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.8064  0.8980     47     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 43) = 10877.1280, p-val < .0001
# 
# Test of Moderators (coefficients 1:4):
#   QM(df = 4) = 74.6962, p-val < .0001
# 
# Model Results:
#   
#   estimate      se    zval    pval   ci.lb   ci.ub      
# ContinentAustralia        1.7370  0.6362  2.7300  0.0063  0.4900  2.9840   ** 
#   ContinentEurope           0.9025  0.1806  4.9967  <.0001  0.5485  1.2566  *** 
#   ContinentNorth America    1.3177  0.2144  6.1461  <.0001  0.8975  1.7379  *** 
#   ContinentSouth America    1.9459  0.9171  2.1217  0.0339  0.1483  3.7435    * 
#   
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Apply the inverse logit function to effect sizes
cont_inv_logit <- inv_logit(results.cont[["beta"]])
cont_inv_logit

# [,1]
# ContinentAustralia     0.8503018
# ContinentEurope        0.7114722
# ContinentNorth America 0.7887924
# ContinentSouth America 0.8750000

#moderator analysis- country
results.count<-rma.mv(yi,vi,mods=~Country -1,data=ies,method="REML",random=~1|Study_Number)
# Warning message:
#   10 rows with NAs omitted from model fitting. 
results.count

# Multivariate Meta-Analysis Model (k = 37; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.6192  0.7869     37     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 29) = 5942.7749, p-val < .0001
# 
# Test of Moderators (coefficients 1:8):
#   QM(df = 8) = 107.8458, p-val < .0001
# 
# Model Results:
#   
#   estimate      se    zval    pval    ci.lb   ci.ub      
# CountryAustralia         1.7370  0.5579  3.1136  0.0018   0.6436  2.8305   ** 
#   CountryCanada            3.3777  0.7883  4.2849  <.0001   1.8327  4.9227  *** 
#   CountryChile             1.9459  0.8087  2.4062  0.0161   0.3609  3.5310    * 
#   CountryItaly             0.5542  0.3972  1.3952  0.1630  -0.2243  1.3328      
# CountrySweden            0.8306  0.4553  1.8245  0.0681  -0.0617  1.7229    . 
# CountrySwitzerland       1.0986  0.7897  1.3911  0.1642  -0.4493  2.6465      
# CountryUnited Kingdom    1.6452  0.3016  5.4551  <.0001   1.0541  2.2364  *** 
#   CountryUnited States     1.1901  0.1956  6.0854  <.0001   0.8068  1.5734  *** 
#   
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Apply the inverse logit function to effect sizes
count_inv_logit <- inv_logit(results.count[["beta"]])
count_inv_logit

# [,1]
# CountryAustralia      0.8503099
# CountryCanada         0.9670000
# CountryChile          0.8750000
# CountryItaly          0.6351105
# CountrySweden         0.6964887
# CountrySwitzerland    0.7500000
# CountryUnited Kingdom 0.8382471
# CountryUnited States  0.7667538

#moderator analysis- model used for identification
results.model<-rma.mv(yi,vi,mods=~Model -1,data=ies,method="REML",random=~1|Study_Number)
results.model


# Multivariate Meta-Analysis Model (k = 47; method: REML)
# 
# Variance Components:
#   
#   estim    sqrt  nlvls  fixed        factor 
# sigma^2    0.5896  0.7679     47     no  Study_Number 
# 
# Test for Residual Heterogeneity:
#   QE(df = 29) = 3720.0362, p-val < .0001
# 
# Test of Moderators (coefficients 1:18):
#   QM(df = 18) = 129.3608, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval    ci.lb   ci.ub      
# ModelANN              1.5079  0.2693   5.6004  <.0001   0.9802  2.0357  *** 
#   ModelBatClassify      2.1535  0.7707   2.7943  0.0052   0.6430  3.6641   ** 
#   ModelBatExplorer     -0.8001  0.7691  -1.0403  0.2982  -2.3076  0.7074      
# ModelBatIdent         1.2083  0.7694   1.5704  0.1163  -0.2997  2.7163      
# ModelBCID             0.4473  0.7680   0.5824  0.5603  -1.0580  1.9526      
# ModelCA              -0.0020  0.7743  -0.0026  0.9979  -1.5197  1.5157      
# ModelCART            -0.0060  0.7743  -0.0077  0.9938  -1.5237  1.5117      
# ModelDFA              1.3189  0.2090   6.3101  <.0001   0.9093  1.7286  *** 
#   ModeleANN             1.7744  0.7757   2.2876  0.0222   0.2541  3.2946    * 
#   ModelGANN            -0.5624  0.7720  -0.7284  0.4664  -2.0756  0.9508      
# ModelGARD            -0.0240  0.7717  -0.0311  0.9752  -1.5366  1.4886      
# ModelGSE              0.1845  0.7718   0.2391  0.8110  -1.3281  1.6972      
# ModelKaleidoscope     0.6274  0.4438   1.4137  0.1574  -0.2424  1.4973      
# ModelNN               0.5537  0.7720   0.7172  0.4732  -0.9594  2.0669      
# ModelRF               1.7758  0.4084   4.3477  <.0001   0.9752  2.5763  *** 
#   ModelSonoChiro        0.2975  0.5438   0.5470  0.5844  -0.7683  1.3632      
# ModelStudy created    2.1972  0.7683   2.8599  0.0042   0.6914  3.7030   ** 
#   ModelSVM              1.5548  0.4738   3.2814  0.0010   0.6261  2.4835   ** 
  
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Apply the inverse logit function to effect sizes
model_inv_logit <- inv_logit(results.model[["beta"]])
model_inv_logit

# [,1]
# ModelANN           0.8187572
# ModelBatClassify   0.8960000
# ModelBatExplorer   0.3100000
# ModelBatIdent      0.7700000
# ModelBCID          0.6100000
# ModelCA            0.4995000
# ModelCART          0.4985000
# ModelDFA           0.7890022
# ModeleANN          0.8550000
# ModelGANN          0.3630000
# ModelGARD          0.4940000
# ModelGSE           0.5460000
# ModelKaleidoscope  0.6519101
# ModelNN            0.6350000
# ModelRF            0.8551738
# ModelSonoChiro     0.5738207
# ModelStudy created 0.9000000
# ModelSVM           0.8256090