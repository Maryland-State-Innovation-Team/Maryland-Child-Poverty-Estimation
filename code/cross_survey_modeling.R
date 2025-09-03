#-----------------------------------------------------------------------------#
# Maryland Child Poverty Estimation Script (2014-2023 Data)
#
# This script builds a model to estimate child poverty rates at the census
# tract level in Maryland. It uses ACS 5-year data for 2014-2023, trains
# an XGBoost model, and applies spatial smoothing to the results, following
# the methods described in the SEHSD Working Paper on cross-survey modeling.
# https://www2.census.gov/library/working-papers/2025/demo/sehsd-wp2025-05.pdf
#-----------------------------------------------------------------------------#

## 1. SETUP: LOAD PACKAGES AND API KEY

# Boilerplate for package installation and loading
list.of.packages <- c(
  "data.table", "tidycensus", "sf", "dplyr", "ggplot2", "scales",
  "dotenv", "stringr", "httr", "xgboost", "caret", "spdep", "spatialreg",
  "patchwork"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only = TRUE))

# Set working directory (update this path to your project location)
setwd("C:/git/Maryland-Child-Poverty-Estimation/")

# Load Census API key from .env file
load_dot_env()
api_key <- Sys.getenv("CENSUS_API_KEY")
census_api_key(api_key, install = TRUE, overwrite = TRUE)


#-----------------------------------------------------------------------------#
# 2. DATA ACQUISITION: DOWNLOAD AND PROCESS ACS DATA (2014-2023)
#-----------------------------------------------------------------------------#


# Function for data profiles
get_precomp = function(year, profile, variables, table_type="profile"){
  dp_url = paste0(
    "https://api.census.gov/data/",
    year,
    "/acs/acs5/",
    table_type,
    "?get=group(",
    profile,
    ")&ucgid=pseudo(0400000US24$1400000)"
  )
  dp_req = GET(dp_url)
  dp_content = content(dp_req)
  list_no_nulls <- lapply(dp_content, function(row) {
    lapply(row, function(item) {
      if (is.null(item)) NA else item
    })
  })
  dp = as.data.frame(do.call(rbind, list_no_nulls))
  dp = as.data.frame(lapply(dp, unlist))
  names(dp) = dp[1,]
  dp = dp[2:nrow(dp),]
  dp$GEOID = gsub("1400000US", "", dp$GEO_ID)
  for(variable in variables){
    dp[,variable] = as.numeric(dp[,variable])
    dp[which(dp[,variable] < 0),variable] = NA
  }
  dp = dp[,c("GEOID", variables)]
  names(dp) = c("GEOID", names(variables))
  return(dp)
}

# Function to get and process data for a single year
get_yearly_data <- function(year) {
  message(paste("Fetching data for ACS 5-Year", year, "..."))
  
  dp02 = get_precomp(
    year, 
    "DP02",
    c(
      edu_less_than_hs_pct = "DP02_0060PE",
      disability_pct = "DP02_0072PE",
      linguistic_isolation_pct = "DP02_0115PE",
      married_couple_family_pct="DP02_0002PE",
      cohabitating_couple_family_pct="DP02_0004PE",
      single_male_headed_family_pct="DP02_0006PE",
      single_female_headed_family_pct = "DP02_0010PE"
    )
  )
  
  dp03 = get_precomp(
    year, 
    "DP03",
    c(
      child_poverty_pct = "DP03_0129PE",
      child_poverty_moe = "DP03_0129PM",
      unemployment_pct = "DP03_0009PE",
      lf_participation_pct = "DP03_0002PE",
      health_insurance_pct = "DP03_0096PE",
      private_health_insurance_pct = "DP03_0097PE",
      public_health_insurance_pct = "DP03_0098PE"
    )
  )
  
  dp04 = get_precomp(
    year, 
    "DP04",
    c(
      no_vehicle_available_pct = "DP04_0058PE",
      one_vehicle_available_pct = "DP04_0059PE",
      two_vehicles_available_pct = "DP04_0060PE",
      three_or_more_vehicles_available_pct = "DP04_0061PE"
    )
  )
  
  acs5 <- get_acs(
    geography = "tract",
    state = "MD",
    variables = c(
      paste0("B19001_0", str_pad(1:12, width=2, pad="0")), # Income thresholds
      "B19058_001", # Public assistance or SNAP denominator
      "B19058_002", # With public assistance or SNAP
      "B19013_001", # Median household income
      "B25140_002", # Owned units with mortgage
      "B25140_003", # Owned units with mortgage, housing costs gt 30% housing costs
      "B25140_006", # Owned units without a mortgage
      "B25140_007", # Owned units without a mortgage, housing costs gt 30% housing costs
      "B25140_010", # Rented units
      "B25140_011", # Rented units, housing costs gt 30% housing costs
      "B25107_001", # Median home value
      "B25003_001", # Tenure denominator
      "B25003_002", # Owner occupied
      "B25003_003", # Renter occupied
      "B11003_001", # Households with children denominator
      "B11003_002", # Married households
      "B11003_003", # Married households with own children under 18
      "B11003_008", # Other families
      "B11003_009", # Other families, male householder
      "B11003_010", # Other families, male householder with own children under 18
      "B11003_015", # Other families, female householder
      "B11003_016", # Other families, female householder with own children under 18
    ),
    year = year,
    survey = "acs5"
  )
  acs5_wide = dcast(data.table(acs5), GEOID~variable, value.var="estimate")
  for(var in names(acs5_wide)){
    if(var %in% c("GEOID", "B19001_001", "B19058_001", "B19058_002")){next}
    (acs5_wide[,var] = acs5_wide[,var,with=F] / acs5_wide$B19001_001)
  }
  acs5_wide$assistance_snap_pct = acs5_wide$B19058_002 / acs5_wide$B19058_001
  acs5_wide[,c("B19001_001", "B19058_001", "B19058_002")] = NULL
  setnames(
    acs5_wide,
    paste0("B19001_0", str_pad(2:12, width=2, pad="0")),
    c(
      "lt10k_pct",
      "10kto15k_pct",
      "15kto20k_pct",
      "20kto25k_pct",
      "25kto30k_pct",
      "30kto35k_pct",
      "35kto40k_pct",
      "40kto45k_pct",
      "45kto50k_pct",
      "50kto60k_pct",
      "60kto75k_pct"
    )
  )
  
  s0101 = get_precomp(
    year,
    "S0101",
    c(
      total_population = "S0101_C01_001E",
      under18_pop = "S0101_C01_022E"
    ),
    table_type="subject"
  )
  s0101$under18_pct = (s0101$under18_pop / s0101$total_population)
  
  data = merge(
    dp02, dp03, by="GEOID", all=T
  )
  data = merge(
    data, dp04, by="GEOID", all=T
  )
  
  # Convert DP percentages to decimals
  data <- data %>%
    mutate(across(ends_with("_pct"), ~ . / 100))
  
  data = merge(
    data, acs5_wide, by="GEOID", all=T
  )
  data = merge(
    data, s0101, by="GEOID", all=T
  )
  
  data$year = year
  
  return(data)
}

# Loop through the years 2014-2023 and bind the data together.
if(!file.exists("input/cross_data.RData")){
  all_years_data <- do.call(rbind, lapply(2014:2023, get_yearly_data))
  save(all_years_data, file="input/cross_data.RData")
}else{
  load("input/cross_data.RData")
}

message("Data acquisition complete.")
glimpse(all_years_data)


#-----------------------------------------------------------------------------#
# 3. DATA PREPARATION
#-----------------------------------------------------------------------------#

# Create the final modeling dataset by removing unreliable estimates and NAs
model_data <- all_years_data %>%
  select(-GEOID, -child_poverty_moe, -year) %>% # Remove non-predictor columns
  na.omit()

message(paste("Final dataset for modeling contains", nrow(model_data), "observations."))


#-----------------------------------------------------------------------------#
# 4. XGBOOST MODEL TRAINING AND EVALUATION
#-----------------------------------------------------------------------------#

# Set up for modeling
set.seed(123) # for reproducibility

# Split data into training (80%) and testing (20%) sets
train_index <- createDataPartition(model_data$child_poverty_pct, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Define the cross-validation method (5-fold CV)
# This retrains the model on different "folds" of the data to get a
# robust performance estimate.
cv_control <- trainControl(
  method = "cv",
  number = 5
)

# Train the XGBoost model
message("Training XGBoost model with 5-fold cross-validation...")

xgb_model <- train(
  child_poverty_pct ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = cv_control,
  verbose = FALSE
)

# Print the cross-validation results (RMSE is the key metric)
message("Cross-validation results:")
print(xgb_model)

# Evaluate the final model on the held-out test data
predictions <- predict(xgb_model, test_data)
rmse_test <- RMSE(predictions, test_data$child_poverty_pct)
r2_test <- R2(predictions, test_data$child_poverty_pct)

message(paste("Final Model RMSE on reserved test data:", round(rmse_test, 4)))
message(paste("Final Model R2 on reserved test data:", round(r2_test, 4)))

importance <- varImp(xgb_model, scale = TRUE)

# Print the ranked list of variables
print(importance)

# Plot the top 20 most important variables
plot(importance, top = 20)

#-----------------------------------------------------------------------------#
# 5. SPATIAL SMOOTHING OF PREDICTIONS (EXAMPLE ON 2023 DATA)
#-----------------------------------------------------------------------------#
message("Performing spatial smoothing on 2023 predictions as an example...")

# Step 5.1: Get 2023 data with spatial geometry
if(!file.exists("input/pop_tracts_2023.RData")){
  md_tracts_2023_sf <- get_acs(
    geography = "tract",
    state = "MD",
    variables = "B01003_001", # Just need population for the model
    year = 2023,
    survey = "acs5",
    geometry = TRUE
  )
  save(md_tracts_2023_sf, file="input/pop_tracts_2023.RData")
}else{
  load("input/pop_tracts_2023.RData")
}


# Step 5.2: Generate initial XGBoost predictions for all 2023 tracts
# First, get the full predictor dataset for 2023
load("input/cross_data.RData")
full_2023_data <- all_years_data %>% filter(year==2023) %>% na.omit()

# Make predictions
full_2023_data$xgb_pred <- predict(xgb_model, full_2023_data)

# Join predictions to the spatial data
md_tracts_2023_sf <- md_tracts_2023_sf %>%
  left_join(full_2023_data, by = "GEOID") %>%
  rename(population = estimate) %>%
  filter(!is.na(xgb_pred)) # Ensure we only model tracts with predictions

# Step 5.3: Create spatial weights matrix
# Using 5 nearest neighbors as described in the paper
neighbors <- knearneigh(st_centroid(md_tracts_2023_sf), k = 5)
nb_weights <- knn2nb(neighbors)
list_weights <- nb2listw(nb_weights, style = "W")

# Step 5.4: Fit the Spatial Autoregressive (SAR) Model
# Following the paper's formula: y = rho*W*y + B*x_popsize + e
# where 'y' is our initial XGBoost prediction.
sar_model <- spautolm(
  xgb_pred ~ sqrt(population), # Weight by sqrt of pop size
  data = md_tracts_2023_sf,
  listw = list_weights,
  family = "SAR"
)

# Step 5.5: Get the final, smoothed predictions
md_tracts_2023_sf$smoothed_pred <- fitted(sar_model)

message("Spatial smoothing complete.")

# View the results - compare original XGBoost vs. smoothed predictions
print(
  head(
    md_tracts_2023_sf %>%
      as.data.frame() %>%
      select(NAME, child_poverty_pct, xgb_pred, smoothed_pred)
  )
)

out_file = data.table(md_tracts_2023_sf)
out_file$geometry = NULL
fwrite(out_file, "output/cross_survey_model_output.csv")

plot(child_poverty_pct~xgb_pred, data=md_tracts_2023_sf)
plot(xgb_pred~smoothed_pred, data=md_tracts_2023_sf)
plot(child_poverty_pct~smoothed_pred, data=md_tracts_2023_sf)

# Limit plot to Baltimore City
baltimore_tracts <- subset(md_tracts_2023_sf, startsWith(GEOID, "24510"))

# Find the min/max across all three columns for consistent scaling
fill_min <- min(
  baltimore_tracts$child_poverty_pct,
  baltimore_tracts$xgb_pred,
  baltimore_tracts$smoothed_pred,
  na.rm = TRUE
)
fill_max <- max(
  baltimore_tracts$child_poverty_pct,
  baltimore_tracts$xgb_pred,
  baltimore_tracts$smoothed_pred,
  na.rm = TRUE
)

p1 <- ggplot(baltimore_tracts) +
  geom_sf(aes(fill=child_poverty_pct), color="transparent") +
  scale_fill_gradient(labels=percent, limits = c(fill_min, fill_max)) +
  labs(title="Original DP03 Estimates", fill="Under 18 poverty %") +
  theme_void()

p2 <- ggplot(baltimore_tracts) +
  geom_sf(aes(fill=xgb_pred), color="transparent") +
  scale_fill_gradient(labels=percent, limits = c(fill_min, fill_max)) +
  labs(title="XGB Predictions", fill="Under 18 poverty %") +
  theme_void()

p3 <- ggplot(baltimore_tracts) +
  geom_sf(aes(fill=smoothed_pred), color="transparent") +
  scale_fill_gradient(labels=percent, limits = c(fill_min, fill_max)) +
  labs(title="Spatially-smoothed XGB Predictions", fill="Under 18 poverty %") +
  theme_void()

(p1 | p2 | p3)
