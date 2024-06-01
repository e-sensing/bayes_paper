### SETUP AND PACKAGE INSTALLATION

# if devtools is not installed, install it
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools", repos = "https://cloud.r-project.org/")
library(devtools)

# if terra is not installed, install it
if (!requireNamespace("terra", quietly = TRUE))
  install.packages("terra", repos = "https://cloud.r-project.org/")
library(terra)

# if bayesEO is not installed, install it
if (!requireNamespace("bayesEO", quietly = TRUE))
  devtools::install_github("e-sensing/bayesEO", dependencies = TRUE)
library(bayesEO)

## Code for Section - Reading a probability data cube

# select the file containing the class probability
data_dir <- system.file("/extdata/probs", package = "bayesEO")
probs_file <- paste0(data_dir, "/", 
                     "SENTINEL-2_MSI_20LLQ_2020-06-04_2021-08-26_probs_v0.tif")
# set the labels
labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
            "ClearCut_Veg", "Forest", "Wetland")
# read the probability image 
probs_image <- bayes_read_probs(probs_file, labels)
# plot the probabilities for classes Forest, ClearCut_Soil, 
# ClearCut_Veg, ClearCut_Burn
bayes_plot_probs(probs_image, labels = c("Forest", "ClearCut_Soil", 
                                         "ClearCut_Veg", "ClearCut_Burn"))
# produce a labelled map
map_no_smooth <- bayes_label(probs_image)
# show the map
bayes_plot_map(map_no_smooth)

## Code for Section - Estimating the local logit variances
# estimate the local variance
var_image <- bayes_variance(
  x = probs_image,
  window_size = 7,
  neigh_fraction = 0.50)
# produce the summary
bayes_summary(var_image)

# Logit variance map showing values above the 3rd quartile 
# for classes ClearCutVeg and Forest
bayes_plot_var(var_image, 
               quantile = 0.75, 
               palette = "Greens",
               labels = c("ClearCut_Veg", "Forest"))

## Code for Section - Applying Bayesian smoothing to remove outliers

# calculate the smoothed probability values
smooth_probs <- bayes_smooth(
  probs_image,
  window_size = 7,
  smoothness = c(
    "Water" = 15, "ClearCut_Burn" = 5, "ClearCut_Soil" = 6, 
    "ClearCut_Veg" =  8, "Forest" = 10, "Wetland" = 5)
)
# show the smoothed probability maps
map_smooth_bayes <- bayes_label(smooth_probs)
bayes_plot_map(map_smooth_bayes)

# get the summary of the non-smoothed map
sum1 <- bayes_summary(map_no_smooth)
colnames(sum1) <- c("class", "area_k2_no_smooth")
# get the summary of the smoothed map
sum2 <- bayes_summary(map_smooth_bayes)
colnames(sum2) <- c("class", "area_k2_smooth")
# compare class areas of non-smoothed and smoothed maps
dplyr::inner_join(sum1, sum2, by = "class")

## Code for Section - Comparison with other methods 

# compute Gaussian smoothing, label and plot map
gaussian_smooth(probs_image, window_size = 7, sigma = 5 ) |> 
  bayes_label() |> 
  bayes_plot_map()

# compute Bilateral smoothing
bilat_smooth <- bilateral_smooth(
  probs_image,
  window_size = 7,
  sigma = 5,
  tau = 2.0
)
# produce the labelled map
bilat_map <- bayes_label(bilat_smooth)
# plot the map produced by Bilateral smoothing
bayes_plot_map(bilat_map)



