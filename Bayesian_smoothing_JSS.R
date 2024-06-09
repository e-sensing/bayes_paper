### SETUP AND PACKAGE INSTALLATION
# if bayesEO is not installed, install it
if (!requireNamespace("bayesEO", quietly = TRUE))
  install.packages("bayesEO", dependencies = TRUE)
library(bayesEO)

## Code for Section - Reading a probability data cube

# select the file containing the class probability
data_dir <- system.file("/extdata/probs/", package = "bayesEO")
probs_file <- paste0(data_dir, 
                     "SENTINEL-2_MSI_20LLQ_2020-06-04_2021-08-26_probs_v0.tif")
# set the labels
labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
            "ClearCut_Veg", "Forest", "Wetland")
# read the probability image 
probs_image <- bayes_read_probs(probs_file, labels)
# plot the probabilities for classes Forest and ClearCut_Soil
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
# for classes ClearCut_Soil and Forest
bayes_plot_var(var_image, 
               quantile = 0.75, 
               palette = "Greens",
               labels = c("ClearCut_Soil", "Forest"))

# Logit variance map showing values above the 3rd quartile 
# for classes ClearCut_Veg and ClearCut_Burn
bayes_plot_var(var_image, 
               quantile = 0.75, 
               palette = "Greens",
               labels = c("ClearCut_Veg", "ClearCut_Burn"))

# function to return quantiles for the logit variances
var_quantiles <- function(var_image, intervals, quantiles){
  values <- terra::spatSample(var_image, size = 15000, na.rm = TRUE)
  mat <- apply(values, 2, function(x){ 
    quantile(x, probs = seq(0, 1, intervals))
  })
  return(mat[quantiles, ])
}
# get the 75%-95% quantiles of the variance map
var_quantiles(var_image, 0.05, c("75%", "80%", "85%", "90%", "95%", "100%"))

## Code for Section - Applying Bayesian smoothing to remove outliers

# calculate the smoothed probability values
map_smooth_bayes <- bayes_smooth(
  probs_image,
  window_size = 7,
  smoothness = c("Water" = 5.0, 
                 "ClearCut_Burn" = 20, 
                 "ClearCut_Soil" = 1, 
                 "ClearCut_Veg" =  15, 
                 "Forest" = 3.5, 
                 "Wetland" = 0.40
  )
) |> bayes_label()
# plot the smoothed map
bayes_plot_map(map_smooth_bayes)

## Code for Section - Comparison with other methods 

# compute Gaussian smoothing, label and plot map
map_gaussian <- gaussian_smooth(
  probs_image, 
  window_size = 7, 
  sigma = 5 
) |> 
  bayes_label()

# plot the gaussian map
bayes_plot_map(map_gaussian)

# compute Bilateral smoothing and produce the labelled map
map_bilat <- bilateral_smooth(
  probs_image,
  window_size = 7,
  sigma = 5,
  tau = 2.0
) |> 
  bayes_label()

# plot the map produced by Bilateral smoothing
bayes_plot_map(map_bilat)

# get the summary of the non-smoothed map
sum1 <- bayes_summary(map_no_smooth)
colnames(sum1) <- c("class", "no_smooth")
# get the summary of the smoothed map
sum2 <- bayes_summary(map_smooth_bayes)
colnames(sum2) <- c("class", "smooth")
# get the summary of the gaussian map
sum3 <- bayes_summary(map_gaussian)
colnames(sum3) <- c("class", "gauss")
# get the summary of the bilateral map
sum4 <- bayes_summary(map_bilat)
colnames(sum4) <- c("class", "bilat")
# compare class areas of non-smoothed and smoothed maps
dplyr::inner_join(sum1, sum2, by = "class") |> 
  dplyr::inner_join(sum3, by = "class") |> 
  dplyr::inner_join(sum4, by = "class") 

