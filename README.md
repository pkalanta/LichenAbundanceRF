# LichenAbundanceRF
SpaDES-ifying an existing model Lichen Abundance that made using Random Forest algorithm
# Here’s a simplified example of modeling lichen abundance using Random Forest in R, 
# structured to align with the SpaDES framework principles. The example assumes you have spatial data 
# (e.g., raster layers of environmental predictors) and point observations of lichen abundance.
# Parvin Kalantari - January 2nd, 2025 -PFC
repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project"))
  install.packages("SpaDES.project", repos = repos)

setwd("~")
# newModule("LichenAbundanceWithEliot", path = "~/SpaDES_CEFWS/m")
# newModule("CaribouModel", path = "~/SpaDES_CEFWS/m")
out <- SpaDES.project::setupProject(
  paths = list(modulePath = "m",
               projectPath = "SpaDES_CEFWS"),
  modules = c("LichenAbundanceWithEliot"),
  require = c("randomForest", "raster", "sp", "sf", "rgdal", "terra",
              "reproducible", "SpaDES.core (>= 2.1.5)"),
  params = list(),
  env1 = raster(nrows = 100, ncols = 100, xmn = 0, xmx = 100, ymn = 0, ymx = 100),
  env2 = env1,
  env3 = env1,
  predictors_dummy = {
    values(env1) <- runif(ncell(env1), min = 0, max = 100) # Predictor 1
    values(env2) <- runif(ncell(env2), min = 0, max = 50)  # Predictor 2
    values(env3) <- runif(ncell(env3), min = 10, max = 200)
    raster::stack(env1, env2, env3)
  },
  lichen_data = prepInputs(url = "https://drive.google.com/file/d/1kdlKO3H8K52mL1-d37FsIZcaJMKUpCn2/view?usp=drive_link",
                            fun = sf::st_read) |> Cache(),
  lichen_abundance_dummy = {
    set.seed(123)
    lichen_abundance <- 0.5 * values(env1) + 0.3 * values(env2) + 0.2 * values(env3) + rnorm(ncell(env1), mean = 0, sd = 10)
    # Normalize abundance between 0 and 1 for consistency
    (lichen_abundance - min(lichen_abundance)) / 
      (max(lichen_abundance) - min(lichen_abundance))
  },
  lichen_data_dummy = {
    # Assign to random points
    n_points <- 50
    coords <- cbind(runif(n_points, 0, 100), runif(n_points, 0, 100))
    SpatialPointsDataFrame(coords, data = data.frame(abundance = runif(n_points, 0, 1)))
  },
  # Extract raster values for the points
  extracted_values = raster::extract(predictors_dummy, lichen_data_dummy),
  # Convert to a data frame and bind with abundance
  lichen_data_dummy_df = cbind(lichen_data_dummy@data, extracted_values),
  studyArea = sf::st_convex_hull(sf::st_union(lichen_data)),
  rasterToMatch = lichen_data
)

sim <- SpaDES.core::simInitAndSpades2(out)

