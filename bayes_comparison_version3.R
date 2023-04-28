
# ------
data_dir <- system.file("/extdata/Rondonia-20LLQ/", package = "sitsdata")

labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
            "ClearCut_Veg", "Forest", "Wetland")
probs_cube <- sits_cube(
    source = "MPC",
    collection = "SENTINEL-2-L2A",
    bands = "probs",
    labels = labels,
    version = "v1",
    parse_info = c("X1", "X2", "tile", "start_date", "end_date", "band", "version"),
    data_dir = data_dir
)
plot(probs_cube, labels = "Forest")

label_cube_no_smooth <- sits_label_classification(
    cube = probs_cube,
    memsize = 24,
    multicores = 4,
    output_dir = tempdir(),
    version = "no_smooth"
)
plot(label_cube_no_smooth)

# test different combinations of window_size and smoothness
# "Water", "ClearCut_Burn", "ClearCut_Soil", "ClearCut_Veg", "Forest", "Wetland"
smooth_equal <- c(10, 10, 10, 10, 10, 10)
smooth_variance <- c(10, 5, 10, 5, 10, 5)
smooth_defor <- c(20, 20, 20, 20, 5, 20)
smooth_water <- c(0, 20, 20, 20, 20, 20)
smooth_water_forest <- c(0, 10, 10, 10, 20, 10)

smooth_cube_v1 <- sits_smooth(
    cube = probs_cube,
    window_size = 7,
    neigh_fraction = 0.5,
    smoothness = c(10, 10, 10, 10, 10, 10),
    multicores = 4,
    memsize = 24,
    output_dir = tempdir(),
    version = "smooth_v1"
)
label_cube_smooth_v1 <- sits_label_classification(
    cube = smooth_cube_v1,
    memsize = 24,
    multicores = 4,
    output_dir = tempdir(),
    version = "smooth_v1"
)

sum1 <- summary(label_cube_no_smooth, only_stats = TRUE)

var_cube <- sits_variance(
  cube = probs_cube,
  window_size = 7,
  neigh_fraction = 0.5,
  output_dir = tempdir())

smooth_cube_v3 <- sits_smooth(
    cube = probs_cube,
    window_size = 7,
    neigh_fraction = 0.5,
    smoothness = c(2, 2, 2, 2, 2, 2),
    multicores = 4,
    memsize = 24,
    output_dir = tempdir(),
    version = "smooth_v3"
)
label_cube_smooth_v3 <- sits_label_classification(
    cube = smooth_cube_v3 ,
    memsize = 24,
    multicores = 4,
    output_dir = tempdir(),
    version = "smooth_v3"
)
plot(label_cube_smooth_v3)
summary(label_cube_v2)

sum1 <- summary(label_cube_no_smooth, only_stats = TRUE)
names(sum1) <- c("class", "area_k2_no_smooth")
sum2 <- summary(label_cube_smooth_v1, only_stats = TRUE)
names(sum2) <- c("class", "area_k2_smooth")
sum3 <- summary(label_cube_smooth_v3, only_stats = TRUE)
names(sum3) <- c("class", "area_k2_smooth_v3")
dplyr::inner_join(sum1, sum2, by = "class") %>% 
  dplyr::inner_join(sum3, by = "class")
# -----

plot(var_cube, labels = "Forest", palette = "YlOrRd")



system.time({new_cube_w7_n10_swf <- sits_smooth(
    cube = new_cube,
    window_size = 7,
    neigh_fraction = 1.0,
    smoothness = smooth_water_forest,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w7_n10_swf"
)})
label_cube_w7_n10_swf <- sits_label_classification(
    cube = new_cube_w7_n10_swf,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w7_n10_swf"
)
system.time({new_cube_w7_s50_swf <- sits_smooth(
    cube = new_cube,
    window_size = 7,
    neigh_fraction = 0.5,
    smoothness = smooth_water_forest,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w7_s50_swf"
)})

label_cube_w7_s50_swf <- sits_label_classification(
    cube = new_cube_w7_s50_swf,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w7_s50_swf"
)

# test different combinations of window_size and smoothness
# "Water", "ClearCut_Burn", "ClearCut_Soil", "ClearCut_Veg", "Forest", "Wetland"
smooth_equal <- c(20, 20, 20, 20, 20, 20)
smooth_forest <- c(5, 5, 5, 5, 20, 5)
smooth_defor <- c(20, 20, 20, 20, 5, 20)
smooth_water <- c(40, 20, 20, 20, 5, 20)

new_cube_w9_n05_s20 <- sits_smooth(
    cube = new_cube,
    window_size = 9,
    neigh_fraction = 0.5,
    smoothness = smooth_equal,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w9-n05-s20"

)
new_cube_w9_n05_sw <- sits_smooth(
    cube = new_cube,
    window_size = 9,
    neigh_fraction = 0.5,
    smoothness = smooth_water,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w9-n05-sw"

)



label_cube_w9_n05_s20 <- sits_label_classification(
    cube = new_cube_w9_n05_s20,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w9-n05-s20"
)

new_cube_w9_n05_sf <- sits_smooth(
    cube = new_cube,
    window_size = 9,
    neigh_fraction = 0.5,
    smoothness = smooth_forest,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w9-n05-sf"
)

label_cube_w9_n05_sf <- sits_label_classification(
    cube = new_cube_w9_n05_sf,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w9-n05-sf"
)

new_cube_w9_n05_sd <- sits_smooth(
    cube = new_cube,
    window_size = 9,
    neigh_fraction = 0.5,
    smoothness = smooth_defor,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w9-n05-sd"
)

label_cube_w9_n05_sd <- sits_label_classification(
    cube = new_cube_w9_n05_sd,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w9-n05-sd"
)

label_cube_no_smooth <- sits_label_classification(
    cube = new_cube,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "no_smooth"
)

new_cube_w7_n05_sw <- sits_smooth(
    cube = new_cube,
    window_size = 7,
    neigh_fraction = 0.5,
    smoothness = smooth_water,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w7-n05-sw"

)
label_cube_w7_n05_sw <- sits_label_classification(
    cube = new_cube_w7_n05_sw,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w7-n05-sw"
)
# --- window 5 x 5 -- neigh - 1.0 -
smooth_water2 <- c(80, 10, 10, 10, 5, 10)

new_cube_w5_n1_sw2 <- sits_smooth(
    cube = new_cube,
    window_size = 5,
    neigh_fraction = 1.0,
    smoothness = smooth_water2,
    min_samples = 20,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w5-n1-sw2"

)
label_cube_w7_n05_sw <- sits_label_classification(
    cube = new_cube_w5_n1_sw2,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w5-n1-sw2"
)

# --- window 7 x 7 -- neigh - 1.0 - sw3
smooth_water3 <- c(100, 10, 10, 10, 5, 10)

new_cube_w7_n1_sw3 <- sits_smooth(
    cube = new_cube,
    window_size = 7,
    neigh_fraction = 1.0,
    smoothness = smooth_water3,
    min_samples = 20,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w7-n1-sw3"

)
label_cube_w7_n1_sw3 <- sits_label_classification(
    cube = new_cube_w7_n1_sw3,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w7-n1-sw3"
)
new_cube_w7_n05_sw3 <- sits_smooth(
    cube = new_cube,
    window_size = 7,
    neigh_fraction = 0.5,
    smoothness = smooth_water3,
    min_samples = 20,
    multicores = 4,
    memsize = 24,
    output_dir = data_dir_2,
    version = "w7-n05-sw3"

)
label_cube_w7_n05_sw3 <- sits_label_classification(
    cube = new_cube_w7_n05_sw3,
    memsize = 24,
    multicores = 4,
    output_dir = data_dir_2,
    version = "w7-n05-sw3"
)
