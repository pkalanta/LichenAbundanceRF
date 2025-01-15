# Here’s a simplified example of modeling lichen abundance using Random Forest in R, 
# structured to align with the SpaDES framework principles. The example assumes you have spatial data 
# (e.g., raster layers of environmental predictors) and point observations of lichen abundance.
# Parvin Kalantari - January 2nd, 2025 -PFC, Victoria, BC
repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project"))
  install.packages("SpaDES.project", repos = repos)
Require::Require(c("sf", "reproducible", "withr", "googledrive", "terra", "caTools"))

setwd("~")
# newModule("LichenAbundanceWithEliot", path = "~/SpaDES_CEFWS/m")
# newModule("LichenAbundanceWith", path = "~/SpaDES_CEFWS/m")
out <- SpaDES.project::setupProject(
  paths = list(modulePath = "m", projectPath = "SpaDES_CEFWS"),
  modules = c("LichenAbundanceWithEliot"),
  require = c("randomForest",  "dplyr", "caTools", "sp", "sf", "rgdal", "terra", "drive.google",
              "reproducible", "SpaDES.core (>= 2.1.5)"),
  params = list(),
  #lichen_data= prepInputs(url = "https://drive.google.com/file/d/1kdlKO3H8K52mL1-d37FsIZcaJMKUpCn2/view?usp=drive_link",
  #fun = sf::st_read) |> Cache(),
  lichen_data_csv <- prepInputs(url = "https://drive.google.com/file/d/1xBRTq0GYgD8CAJgsoDCV77JMUOXHZP5H/view?usp=drive_link",
                                fun = data.table::fread) |> Cache(),
  Lichens = lichen_data_csv, 
  response <- "REC_LICHEN",  # We are predicting REC_LICHEN
  features <- c("LATITUDE", "LONGITUDE", "ALTITUDE", "PC_PENT","drainage", "ageori", "essence"),
  predictors <- features , # Covariantes
  Lichens$drainage <- as.factor(Lichens$drainage),
  Lichens$ageori <- as.factor(Lichens$ageori),
  Lichens$essence <- as.factor(Lichens$essence), #essence =type of forest
  Lichens$REC_LICHEN <- as.numeric(Lichens$REC_LICHEN),
  Lichens$LATITUDE <- as.numeric(Lichens$LATITUDE),
  Lichens$LONGITUDE <- as.numeric(Lichens$LONGITUDE),
  Lichens$ALTITUDE <- as.numeric(Lichens$ALTITUDE),
  Lichens$PC_PENT <- as.numeric(Lichens$PC_PENT),
  # Split the data into train and test sets
  set.seed(123),
  #Create a training and testing split 75/25
  split <- caTools::sample.split(Lichens$ID_POE, SplitRatio = 0.75),
  train_data <- Lichens[split, ] ,    # 75% for training
  test_data  <- Lichens[!split, ] ,   # 25% for testing
  # Convert to an sf object using LATITUDE and LONGITUDE columns
  lichen_data_sf <- sf::st_as_sf(Lichens, 
                                 coords = c("LONGITUDE", "LATITUDE"), 
                                 crs = 4326), # CRS set to WGS84
  studyArea <- sf::st_convex_hull(st_union(lichen_data_sf))
)

sim <- SpaDES.core::simInitAndSpades2(out)





