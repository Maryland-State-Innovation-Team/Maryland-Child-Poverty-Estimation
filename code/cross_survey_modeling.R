#-----------------------------------------------------------------------------#
# Maryland Child Poverty Estimation Script (2012-2023 Data)
#
# This script builds a model to estimate child poverty rates at the census
# tract level in Maryland. It uses ACS 5-year data for 2012-2023, trains
# an XGBoost model, and applies spatial smoothing to the results, inspired by
# the methods described in the SEHSD Working Paper on cross-survey modeling.
# https://www2.census.gov/library/working-papers/2025/demo/sehsd-wp2025-05.pdf
#-----------------------------------------------------------------------------#

## 1. SETUP: LOAD PACKAGES AND API KEY
options(tigris_use_cache = TRUE)
nad83_maryland_epsg <- 26985

# Boilerplate for package installation and loading
list.of.packages <- c(
  "data.table", "tidycensus", "sf", "dplyr", "ggplot2", "scales",
  "dotenv", "stringr", "httr", "xgboost", "caret", "spdep", "spatialreg",
  "patchwork", "units", "ranger"
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
# 2. DATA ACQUISITION: DOWNLOAD AND PROCESS ACS DATA (2012-2023)
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

dp02_variables = list(
  "2012" = c(
    edu_less_than_hs_pct = "DP02_0059PE",
    disability_pct = "DP02_0071PE",
    linguistic_isolation_pct = "DP02_0113PE",
    married_couple_family_pct="DP02_0004PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0008PE"
  ),
  "2013" = c(
    edu_less_than_hs_pct = "DP02_0059PE",
    disability_pct = "DP02_0071PE",
    linguistic_isolation_pct = "DP02_0113PE",
    married_couple_family_pct="DP02_0004PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0008PE"
  ),
  "2014" = c(
    edu_less_than_hs_pct = "DP02_0059PE",
    disability_pct = "DP02_0071PE",
    linguistic_isolation_pct = "DP02_0113PE",
    married_couple_family_pct="DP02_0004PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0008PE"
  ),
  "2015" = c(
    edu_less_than_hs_pct = "DP02_0059PE",
    disability_pct = "DP02_0071PE",
    linguistic_isolation_pct = "DP02_0113PE",
    married_couple_family_pct="DP02_0004PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0008PE"
  ),
  "2016" = c(
    edu_less_than_hs_pct = "DP02_0059PE",
    disability_pct = "DP02_0071PE",
    linguistic_isolation_pct = "DP02_0113PE",
    married_couple_family_pct="DP02_0004PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0008PE"
  ),
  "2017" = c(
    edu_less_than_hs_pct = "DP02_0059PE",
    disability_pct = "DP02_0071PE",
    linguistic_isolation_pct = "DP02_0113PE",
    married_couple_family_pct="DP02_0004PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0008PE"
  ),
  "2018" = c(
    edu_less_than_hs_pct = "DP02_0059PE",
    disability_pct = "DP02_0071PE",
    linguistic_isolation_pct = "DP02_0113PE",
    married_couple_family_pct="DP02_0004PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0008PE"
  ),
  "2019" = c(
    edu_less_than_hs_pct = "DP02_0060PE",
    disability_pct = "DP02_0072PE",
    linguistic_isolation_pct = "DP02_0114PE",
    married_couple_family_pct="DP02_0002PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0010PE"
  ),
  "2020" = c(
    edu_less_than_hs_pct = "DP02_0060PE",
    disability_pct = "DP02_0072PE",
    linguistic_isolation_pct = "DP02_0115PE",
    married_couple_family_pct="DP02_0002PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0010PE"
  ),
  "2021" = c(
    edu_less_than_hs_pct = "DP02_0060PE",
    disability_pct = "DP02_0072PE",
    linguistic_isolation_pct = "DP02_0115PE",
    married_couple_family_pct="DP02_0002PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0010PE"
  ),
  "2022" = c(
    edu_less_than_hs_pct = "DP02_0060PE",
    disability_pct = "DP02_0072PE",
    linguistic_isolation_pct = "DP02_0115PE",
    married_couple_family_pct="DP02_0002PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0010PE"
  ),
  "2023" = c(
    edu_less_than_hs_pct = "DP02_0060PE",
    disability_pct = "DP02_0072PE",
    linguistic_isolation_pct = "DP02_0115PE",
    married_couple_family_pct="DP02_0002PE",
    single_male_headed_family_pct="DP02_0006PE",
    single_female_headed_family_pct = "DP02_0010PE"
  )
)

dp03_variables = list(
  "2012" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2013" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2014" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2015" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2016" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2017" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2018" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2019" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2020" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2021" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2022" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  ),
  "2023" = c(
    total_poverty_pct = "DP03_0128PE",
    child_poverty_pct = "DP03_0129PE",
    child_poverty_moe = "DP03_0129PM",
    unemployment_pct = "DP03_0009PE",
    lf_participation_pct = "DP03_0002PE",
    health_insurance_pct = "DP03_0096PE",
    private_health_insurance_pct = "DP03_0097PE",
    public_health_insurance_pct = "DP03_0098PE"
  )
)

dp04_variables = list(
  "2012" = c(
    no_vehicle_available_pct = "DP04_0057PE",
    one_vehicle_available_pct = "DP04_0058PE",
    two_vehicles_available_pct = "DP04_0059PE",
    three_or_more_vehicles_available_pct = "DP04_0060PE"
  ),
  "2013" = c(
    no_vehicle_available_pct = "DP04_0057PE",
    one_vehicle_available_pct = "DP04_0058PE",
    two_vehicles_available_pct = "DP04_0059PE",
    three_or_more_vehicles_available_pct = "DP04_0060PE"
  ),
  "2014" = c(
    no_vehicle_available_pct = "DP04_0057PE",
    one_vehicle_available_pct = "DP04_0058PE",
    two_vehicles_available_pct = "DP04_0059PE",
    three_or_more_vehicles_available_pct = "DP04_0060PE"
  ),
  "2015" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  ),
  "2016" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  ),
  "2017" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  ),
  "2018" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  ),
  "2019" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  ),
  "2020" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  ),
  "2021" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  ),
  "2022" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  ),
  "2023" = c(
    no_vehicle_available_pct = "DP04_0058PE",
    one_vehicle_available_pct = "DP04_0059PE",
    two_vehicles_available_pct = "DP04_0060PE",
    three_or_more_vehicles_available_pct = "DP04_0061PE"
  )
)

# Function to get and process data for a single year
get_yearly_data <- function(year) {
  message(paste("Fetching data for ACS 5-Year", year, "..."))
  dp02 = get_precomp(
    year, 
    "DP02",
    dp02_variables[[as.character(year)]]
  )
  
  dp03 = get_precomp(
    year, 
    "DP03",
    dp03_variables[[as.character(year)]]
  )
  
  dp04 = get_precomp(
    year, 
    "DP04",
    dp04_variables[[as.character(year)]]
  )
  
  acs5 <- get_acs(
    geography = "tract",
    state = "MD",
    variables = c(
      paste0("B19001_0", str_pad(1:12, width=2, pad="0")), # Income thresholds
      "B19058_001", # Public assistance or SNAP denominator
      "B19058_002", # With public assistance or SNAP
      "B19013_001", # Median household income
      "B25106_024", # Renter-occupied housing units
      "B25106_025", # Renter-occupied housing units less than $20k
      "B25106_028", # Renter-occupied housing units less than $20k, burden 30% or more
      "B25106_029", # Renter-occupied housing units $20-$35k
      "B25106_032", # Renter-occupied housing units $20-$35k, burden 30% or more
      "B25106_033", # Renter-occupied housing units $35-$50k
      "B25106_036", # Renter-occupied housing units $35-$50k, burden 30% or more
      "B25106_045", # Renter-occupied housing units, zero or negative income
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
      "B25002_001", # Occupancy denominator
      "B25002_003" # Occupancy, vacant
    ),
    year = year,
    survey = "acs5",
    geometry = T
  )
  acs5_geometry = unique(acs5[,c("GEOID", "geometry")])
  acs5 = data.table(acs5)
  acs5$geometry = NULL
  acs5_geometry = st_transform(acs5_geometry, nad83_maryland_epsg) # Maryland projection
  acs5_geometry$area = st_area(acs5_geometry)
  acs5_geometry = data.table(acs5_geometry)
  acs5_geometry$geometry = NULL
  acs5_wide = dcast(data.table(acs5), GEOID~variable, value.var="estimate")
  acs5_wide = merge(acs5_wide, acs5_geometry, by="GEOID")
  
  # Calculate the vacancy rate
  acs5_wide[, vacant_pct := B25002_003 / B25002_001]
  
  # Calculate the percentage of households receiving SNAP benefits.
  acs5_wide[, assistance_snap_pct := B19058_002 / B19058_001]
  
  # Calculate housing tenure percentages (owner vs. renter occupied).
  acs5_wide[, owner_occupied_pct := B25003_002 / B25003_001]
  acs5_wide[, renter_occupied_pct := B25003_003 / B25003_001]
  
  # Calculate the percentage of renter households that are cost-burdened, by income bracket.
  # For each income bracket, this is the number of cost-burdened households divided by the total
  # number of renter households in that bracket.
  acs5_wide[, renter_cost_burden_lt20k_pct := B25106_028 / B25106_025]
  acs5_wide[, renter_cost_burden_20kto35k_pct := B25106_032 / B25106_029]
  acs5_wide[, renter_cost_burden_35kto50k_pct := B25106_036 / B25106_033]
  # Note: Zero/negative income households are calculated as a percentage of all renters.
  acs5_wide[, renter_zero_neg_income_pct := B25106_045 / B25106_024]
  
  # Calculate the percentage of each family type that has children under 18.
  # The denominator for each is the total count of that specific family type.
  acs5_wide[, married_with_children_pct := B11003_003 / B11003_002]
  acs5_wide[, male_hh_with_children_pct := B11003_010 / B11003_009]
  acs5_wide[, female_hh_with_children_pct := B11003_016 / B11003_015]
  acs5_wide[, single_mother_household_pct := B11003_016 / B11003_001]
  
  # --- Re-calculate Income Thresholds as Percentages ---
  
  # Define the income variables to be converted to percentages
  income_vars <- paste0("B19001_0", str_pad(2:12, width = 2, pad = "0"))
  
  # This loop calculates the percentage for each income bracket.
  # It divides each bracket's count by the total number of households (B19001_001).
  for (var in income_vars) {
    acs5_wide[, (var) := .SD[[var]] / B19001_001, .SDcols = var]
  }
  
  # --- Clean Up and Rename Columns ---
  
  # 1. Replace NaN values with 0.
  # This can happen during division if the denominator is zero.
  # This loop iterates through each column and replaces NaN with 0.
  for (j in names(acs5_wide)) {
    set(acs5_wide, which(is.nan(acs5_wide[[j]])), j, 0)
  }
  
  # 2. List all original raw count and denominator columns that are now redundant.
  cols_to_remove <- c(
    "B19001_001", "B19058_001", "B19058_002", "B25106_024", "B25106_025",
    "B25106_028", "B25106_029", "B25106_032", "B25106_033", "B25106_036",
    "B25106_045", "B25003_001", "B25003_002", "B25003_003", "B11003_001",
    "B11003_002", "B11003_003", "B11003_008", "B11003_009", "B11003_010",
    "B11003_015", "B11003_016", "B25002_001", "B25002_003"
  )
  
  # Remove these columns from the data.table
  acs5_wide[, (cols_to_remove) := NULL]
  
  # 3. Rename the income threshold columns to be more descriptive.
  setnames(
    acs5_wide,
    old = paste0("B19001_0", str_pad(2:12, width=2, pad="0")),
    new = c(
      "income_lt10k_pct",
      "income_10kto15k_pct",
      "income_15kto20k_pct",
      "income_20kto25k_pct",
      "income_25kto30k_pct",
      "income_30kto35k_pct",
      "income_35kto40k_pct",
      "income_40kto45k_pct",
      "income_45kto50k_pct",
      "income_50kto60k_pct",
      "income_60kto75k_pct"
    )
  )
  
  # 4. Rename median income and home value columns.
  setnames(
    acs5_wide,
    old = c("B19013_001", "B25107_001"),
    new = c("median_household_income", "median_home_value")
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
  s0101[is.na(s0101)] = 0 # Replace NA under_18_pct (which occurs at 0 pop) with 0
  s0101$under18_pct = (s0101$under18_pop / s0101$total_population)
  s0101$under18_pct[which(is.nan(s0101$under18_pct))] = 0
  
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
  zerom2 = set_units(0, m^2)
  # Remove zero area and zero population
  data = subset(data, area > zerom2)
  data = subset(data, total_population > 0)
  data$population_density = data$total_population / data$area
  data[,c("area", "total_population", "under18_pop")] = NULL
  
  # Replace IV NAs with median values
  for (j in names(data)) {
    if(!j %in% c("child_poverty_pct", "GEOID")){
      set(data, which(is.na(data[[j]])), j, median(data[[j]], na.rm=T))
    }
  }
  
  # Engineered features
  data = data.table(data)
  
  ## Combine lowest two income brackets
  data[, deep_poverty_income_pct := income_lt10k_pct + income_10kto15k_pct]
  
  ## Add polynomial features for a key variables
  data[, total_poverty_pct_sq := total_poverty_pct^2]
  data[, assistance_snap_pct_sq := assistance_snap_pct^2]
  data[, deep_poverty_income_pct_sq := deep_poverty_income_pct^2]
  data[, under18_pct_sq := under18_pct^2]
  data[, median_hh_income_sq := median_household_income^2]
  data[, private_health_insurance_pct := private_health_insurance_pct^2]
  
  ## Interaction: Extreme poverty times assistance
  data[, economic_distress_idx := income_lt10k_pct * assistance_snap_pct]
  
  ## Interaction:  Tenancy times vehicle ownership
  data[, housing_transit_insecurity := renter_occupied_pct * no_vehicle_available_pct]

  ## Interaction:  Naive child poverty
  data[, naive_child_poverty_pct := total_poverty_pct * under18_pct]
  
  ## Interaction: Renters * lowest income bracket cost burdens
  data[, concentrated_housing_burden := renter_occupied_pct * (renter_cost_burden_lt20k_pct + renter_cost_burden_20kto35k_pct)]
  
  # Public to private health insurance ratio, add a small constant to avoid division by zero
  data[, health_ins_dependency_ratio := public_health_insurance_pct / (private_health_insurance_pct + 0.01)]
  
  # Create a "gap" feature to measure disproportionate impact
  data[, child_poverty_gap := total_poverty_pct - naive_child_poverty_pct]
  
  # Create a ratio of single-parent to married-couple families
  data[, single_to_married_family_ratio := 
         (single_male_headed_family_pct + single_female_headed_family_pct) / (married_couple_family_pct + 0.01)]
  
  # Create an index for compounding workforce challenges
  data[, workforce_challenge_idx := unemployment_pct * edu_less_than_hs_pct]
  
  return(data)
}

# Loop through the years 2012-2023 and bind the data together.
if(!file.exists("input/cross_data.RData")){
  all_years_data <- do.call(rbind, lapply(2012:2023, get_yearly_data))
  save(all_years_data, file="input/cross_data.RData")
}else{
  load("input/cross_data.RData")
}
out_data = all_years_data %>% select(-child_poverty_moe)
fwrite(out_data, "output/model_data.csv")

message("Data acquisition complete.")
glimpse(all_years_data)


#-----------------------------------------------------------------------------#
# 3. DATA PREPARATION
#-----------------------------------------------------------------------------#

# Create the final modeling dataset by removing NAs and adding 'year' as a predictor
model_data <- all_years_data %>%
  select(-GEOID, -child_poverty_moe) %>% # Keep 'year' as a predictor
  na.omit()


message(paste("Final dataset for modeling contains", nrow(model_data), "observations."))


#-----------------------------------------------------------------------------#
# 4. MODEL TRAINING AND EVALUATION (REVISED)
#-----------------------------------------------------------------------------#

# --- 4.1 Model Setup ---

set.seed(123) # for reproducibility

# Split data into training (80%) and testing (20%) sets
train_index <- createDataPartition(model_data$child_poverty_pct, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Define the cross-validation method (5-fold CV)
cv_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE # See progress
)


# --- 4.2 Tuned XGBoost Model ---
message("Training a tuned XGBoost model...")

xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(4, 6, 8),
  eta = c(0.05, 0.1),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

if(!file.exists("output/xgb_model.RData")){
  xgb_model_tuned <- train(
    child_poverty_pct ~ .,
    data = train_data,
    method = "xgbTree",
    trControl = cv_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  save(xgb_model_tuned, file="output/xgb_model.RData")
}else{
  load("output/xgb_model.RData")
}

message("Tuned XGBoost training complete.")
print(xgb_model_tuned)


# --- 4.3 Evaluate  Model on Test Data ---

predictions <- predict(xgb_model_tuned, test_data)

# Clip predictions to be between 0 and 1
predictions[predictions < 0] <- 0
predictions[predictions > 1] <- 1

rmse_test <- RMSE(predictions, test_data$child_poverty_pct)
r2_test <- R2(predictions, test_data$child_poverty_pct)

message(paste("RMSE on test data:", round(rmse_test, 4)))
message(paste("R-squared on test data:", round(r2_test, 4)))

# Variable importance plot for the model
importance <- varImp(xgb_model_tuned, scale = TRUE)
print(importance)
plot(importance, top = 20)

# --- 5. Build a Parsimonious Model with RFE ---

# Random forest functions since they are fast at ranking features.
rfe_control <- rfeControl(
  functions = rfFuncs, # Functions to use for ranking
  method = "cv",       # Cross-validation
  number = 5,          # 5 folds
  verbose = TRUE
)

# Define the feature set (X) and the outcome (Y)
x_vars <- train_data %>% select(-child_poverty_pct)
y_var <- train_data$child_poverty_pct

# Run the RFE algorithm
# test models with 5, 10, 15, 20, 25 and 30 variables.
set.seed(123)
if(!file.exists("output/feature_selection.RData")){
  feature_selection <- rfe(
    x = x_vars,
    y = y_var,
    sizes = c(5, 10, 15, 20, 25, 30),
    rfeControl = rfe_control
  )
  save(feature_selection, file="output/feature_selection.RData")
}else{
  load("output/feature_selection.RData")
}


# Print the results
print(feature_selection)

# List the optimal predictors
optimal_predictors = predictors(feature_selection)

# Plot the results
plot(feature_selection, type = c("g", "o"))

# Test parsimonious model
max_predictors = 20
train_data = train_data %>% select(c("child_poverty_pct",optimal_predictors[1:max_predictors]))
p_xgb_model_tuned <- train(
  child_poverty_pct ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = cv_control,
  tuneGrid = xgb_grid,
  verbose = FALSE
)
p_predictions <- predict(p_xgb_model_tuned, test_data)

p_predictions[p_predictions < 0] <- 0
p_predictions[p_predictions > 1] <- 1

rmse_test <- RMSE(p_predictions, test_data$child_poverty_pct)
r2_test <- R2(p_predictions, test_data$child_poverty_pct)

message(paste("RMSE on test data:", round(rmse_test, 4)))
message(paste("R-squared on test data:", round(r2_test, 4)))


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
full_2023_data$xgb_pred <- predict(p_xgb_model_tuned, full_2023_data)
full_2023_data$xgb_pred[which(full_2023_data$xgb_pred < 0)] <- 0
full_2023_data$xgb_pred[which(full_2023_data$xgb_pred > 1)] <- 1

# Join predictions to the spatial data
md_tracts_2023_sf <- md_tracts_2023_sf %>%
  left_join(full_2023_data, by = "GEOID") %>%
  rename(population = estimate) %>%
  filter(!is.na(xgb_pred)) # Ensure we only model tracts with predictions

# Step 5.3: Create spatial weights matrix
# Using Queen contiguity for a more natural definition of neighbors
nb_weights <- poly2nb(md_tracts_2023_sf, queen = TRUE)
list_weights <- nb2listw(nb_weights, style = "W")

# Step 5.4: Calculate the spatial lag of the XGBoost predictions
# This is the average prediction value of a tract's neighbors
xgb_lag <- lag.listw(list_weights, md_tracts_2023_sf$xgb_pred)

# Step 5.5: Create the final smoothed prediction by blending
# Set alpha to control smoothing strength. Let's try 0.5 (50% original, 50% smoothed)
alpha <- 0.5
md_tracts_2023_sf$smoothed_pred <- (alpha * md_tracts_2023_sf$xgb_pred) + ((1 - alpha) * xgb_lag)

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

sum(baltimore_tracts$child_poverty_pct == 0)
sum(baltimore_tracts$xgb_pred == 0)

sum(baltimore_tracts$child_poverty_pct >= 0.3)
sum(baltimore_tracts$xgb_pred >= 0.3)
