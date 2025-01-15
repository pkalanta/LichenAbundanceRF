#Define the Module
defineModule(sim, list(
  name = "LichenAbundanceWithEliot",
  description = "Predict lichen abundance using Random Forest",
  authors = person("Parvin Kalantari", email = "parvin.kalantari.1@ulaval.ca"),
  version = list(LichenAbundanceWithEliot = "1.0.0"),
  keywords = character(0),
  childModules = character(0),
  spatialExtent = raster::extent(-180, 180, -90, 90),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  loadOrder = list(1),
  documentation = list("NEWS.md", "README.md", "LichenAbundanceWithEliot.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.0)", "randomForest","dplyr", "caTools", "sp", "sf", "rgdal", "terra", "ggplot2"),
  parameters = bindrows(
    defineParameter("ntree", "numeric", 500, 100, 1000, "Number of trees for the Random Forest model"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  
  inputObjects = bindrows(
    expectsInput("predictors", "data.frame", "Environmental predictors"),  
    expectsInput("Lichens", "data.frame", "Observed lichen abundance data")
  ),
  outputObjects = bindrows(
    createsOutput("predicted_abundance", "data.frame", "Predicted lichen abundance", "rf_model", "Trained Random Forest Model") # Changed to df
  )
  
))

#Event Function
doEvent.LichenAbundanceWithEliot <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    # Initialization event
    init = {
      sim <- lichenAbundanceInit(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "LichenAbundanceWithEliot", "predict")
    },
    
    # Prediction event
    predict = {
      sim <- lichenAbundancePredict(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "LichenAbundanceWithEliot", "plot")
      
      # Ensure prediction was successful before scheduling next event
      if (!is.null(sim$predicted_abundance)) {
        sim <- scheduleEvent(sim, time(sim) + 1, "LichenAbundanceWithEliot", "plot")
      } else {
        warning("Prediction failed, skipping plot event.")
      }
    },
    
    # Plot event
    plot = {
      if (!is.null(sim$predicted_abundance)) {
        plot(sim$predicted_abundance, main = "Predicted Lichen Abundance")
      } else {
        warning("No data available for plotting.")
      }
    },
    
    # Error handling for unknown event types
    {
      stop(paste("Undefined event type:", eventType))
    }
  )
  return(invisible(sim))
}

# Initialization and Prediction Functions
lichenAbundanceInit <- function(sim) {
  # Fit Random Forest model
  sim$rf_model <- randomForest(
    abundance ~ ., 
    data = sim$train_data[, c(abundance, predictors)],
    ntree = P(sim)$ntree, 
  )
  
  sim$importance_values <- importance(sim$rf_model)
  plot(sim$rf_model)
  return(sim)
}

lichenAbundancePredict <- function(sim) {
  # Predict lichen abundance for the entire raster stack
  sim$test_data$prediction <-  predict(sim$rf_model, newdata = sim$test_data[, predictors])
  #sim$predicted_abundance <- predict(sim$predictors, sim$rf_model)
  return(sim)
}
# Performance Metrics Calculation
sim$mse <- mean((sim$test_data$REC_LICHEN - sim$test_data$prediction)^2)
sim$r_squared <- 1 - (sum((sim$test_data$REC_LICHEN - sim$test_data$prediction)^2) /
                        sum((sim$test_data$REC_LICHEN - mean(sim$test_data$REC_LICHEN))^2))

# Print metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", r_squared, "\n")
