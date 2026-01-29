list.of.packages <- c(
  "data.table", "tidycensus", "sf", "dplyr", "ggplot2", "scales",
  "dotenv", "stringr", "httr", "survey", "ipumsr", "tidyverse", "srvyr"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only = TRUE))

setwd("C:/git/Maryland-Child-Poverty-Estimation/")

rss = function(x, na.rm=T){
  return(sqrt(sum(x^2, na.rm)))
}
rowRSS = function(dt, na.rm=T){
  dt_squared = apply(dt, MARGIN=c(1, 2), FUN=function(x){return(x^2)})
  dt_rowSums = rowSums(dt_squared, na.rm)
  dt_rowRSS = sqrt(dt_rowSums)
  return(dt_rowRSS)
}

# Precompiled estimates
get_precomp = function(year, profile, variables, table_type="profile", survey="acs5", geography="tract"){
  if(geography=="tract"){
    ucgid="pseudo(0400000US24$1400000)"
  }else if(geography=="county"){
    ucgid="pseudo(0400000US24$0500000)" # County
  }else if(geography=="state"){
    ucgid="0400000US24"
  }
  dp_url = paste0(
    "https://api.census.gov/data/",
    year,
    "/acs/",
    survey,
    "/",
    table_type,
    "?get=group(",
    profile,
    ")&ucgid=",
    ucgid
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
  if(geography=="tract"){
    dp$GEOID = gsub("1400000US", "", dp$GEO_ID)
  }else if(geography=="county"){
    dp$GEOID = gsub("0500000US", "", dp$GEO_ID)
  }else if(geography=="state"){
    dp$GEOID = "24"
  }
  
  for(variable in variables){
    dp[,variable] = as.numeric(dp[,variable])
    dp[which(dp[,variable] < 0),variable] = NA
  }
  dp = dp[,c("GEOID", variables)]
  names(dp) = c("GEOID", names(variables))
  return(dp)
}

state_child_poverty = 
  get_precomp(
    2024,
    "DP03",
    variables = c(
      child_poverty_pct = "DP03_0129PE",
      child_poverty_moe = "DP03_0129PM"
    ),
    survey="acs1",
    geography = "state"
  )

state_s0101 =
  get_precomp(
    2024,
    "S0101",
    variables=c(under18_pop = "S0101_C01_022E"),
    table_type="subject",
    survey="acs1",
    geography="state"
  )

state_child_poverty = merge(
  state_child_poverty,
  state_s0101,
  by="GEOID"
)
state_child_poverty$child_poverty_pop =
  (state_child_poverty$child_poverty_pct / 100) *
  state_child_poverty$under18_pop

state_child_poverty$child_poverty_pop_moe =
  (state_child_poverty$child_poverty_moe / 100) *
  state_child_poverty$under18_pop


# ACS API estimates
# B17020_003
# Estimate!!Total:!!Income in the past 12 months below poverty level:!!Under 6 years
# Poverty Status in the Past 12 Months by Age
# 
# B17020_004
# Estimate!!Total:!!Income in the past 12 months below poverty level:!!6 to 11 years
# Poverty Status in the Past 12 Months by Age
# 
# B17020_005
# Estimate!!Total:!!Income in the past 12 months below poverty level:!!12 to 17 years
# Poverty Status in the Past 12 Months by Age
acs1_long = get_acs(
  geography="state",
  variables=c(
    "pov_u6"="B17020_003",
    "pov_6to11"="B17020_004",
    "pov_12to17"="B17020_005",
    "above_u6"="B17020_011",
    "above_6to11"="B17020_012",
    "above_12to17"="B17020_013"
  ),
  year=2024,
  survey="acs1",
  state="MD"
)

acs1_agg_total =
  data.table(acs1_long)[,.(
    child_poverty_pop=sum(estimate),
    child_poverty_pop_moe=rss(moe)
  ), by="GEOID"]

acs1_long_pov = subset(acs1_long, variable %in% c("pov_u6", "pov_6to11", "pov_12to17"))
acs1_agg =
  data.table(acs1_long_pov)[,.(
    child_poverty_pop=sum(estimate),
    child_poverty_pop_moe=rss(moe)
  ), by="GEOID"]


state_child_poverty$child_poverty_pop
state_child_poverty$child_poverty_pop_moe

acs1_agg$child_poverty_pop
acs1_agg$child_poverty_pop_moe

acs1_agg$child_poverty_pop / acs1_agg_total$child_poverty_pop

# Families with children in poverty
# B17010_004
# Estimate!!Total:!!Income in the past 12 months below poverty level:!!Married-couple family:!!With related children of the householder under 18 years:
# Poverty Status in the Past 12 Months of Families by Family Type by Presence of Related Children Under 18 Years by Age of Related Children

# B17010_011
# Estimate!!Total:!!Income in the past 12 months below poverty level:!!Other family:!!Male householder, no spouse present:!!With related children of the householder under 18 years:
# Poverty Status in the Past 12 Months of Families by Family Type by Presence of Related Children Under 18 Years by Age of Related Children

# B17010_017
# Estimate!!Total:!!Income in the past 12 months below poverty level:!!Other family:!!Female householder, no spouse present:!!With related children of the householder under 18 years:
# Poverty Status in the Past 12 Months of Families by Family Type by Presence of Related Children Under 18 Years by Age of Related Children

acs1_fam_long = get_acs(
  geography="state",
  variables=c(
    "pov_married_children"="B17010_004",
    "pov_male_children"="B17010_011",
    "pov_female_children"="B17010_017"
  ),
  year=2024,
  survey="acs1",
  state="MD"
)

acs1_fam_agg_total =
  data.table(acs1_fam_long)[,.(
    household_children_poverty_pop=sum(estimate),
    household_children_pop_moe=rss(moe)
  ), by="GEOID"]

acs1_fam_agg_total$household_children_poverty_pop
acs1_fam_agg_total$household_children_pop_moe

# Household income of families in poverty
ddi <- read_ipums_ddi("large_input/usa_00016.xml")
data <- read_ipums_micro(ddi)

# -------------------------------------------------------------------------
# 1. Data Cleaning & Preparation
# -------------------------------------------------------------------------

# assumptions: 
# - 'data' is your dataframe
# - HHINCOME code 9999999 = Missing/N/A
# - POVERTY is the IPUMS standard (% of threshold): < 100 is poverty
# - GQ: Codes 1 and 2 are usually households; 3+ are Group Quarters

data_clean <- data %>%
  # Filter to Keep only Householders (PERNUM = 1) 
  # This ensures we count each household exactly once.
  filter(PERNUM == 1) %>%
  
  # Filter out Group Quarters (Institutions, etc.) if strictly analyzing households
  filter(GQ %in% c(1, 2, 5)) %>%
  
  # Filter out missing income data
  filter(HHINCOME != 9999999) %>%
  
  # Create boolean flags for your subpopulations
  mutate(
    has_children = NCHILD > 0,
    # Poverty < 100 means income is below 100% of the poverty threshold
    # Poverty > 0 excludes N/A cases
    in_poverty = POVERTY < 100 & POVERTY > 0
  )

# -------------------------------------------------------------------------
# 2. Create Survey Design Object
# -------------------------------------------------------------------------

# We define the survey design to handle the weights and clustering correctly.
# If 'STRATA' were in your object, you would include strata = STRATA.
svy_design <- data_clean %>%
  as_survey_design(
    ids = CLUSTER, # Cluster variable for variance estimation
    weights = HHWT # Household weights
  )

# -------------------------------------------------------------------------
# 3. Calculate Estimates
# -------------------------------------------------------------------------

# Estimate A: Median Income for ALL Households with Children
results_all_children <- svy_design %>%
  filter(has_children == TRUE) %>%
  summarize(
    group = "Households with Children",
    mean_inc = survey_mean(HHINCOME, vartype = "ci", level = 0.95)
  )

# Estimate B: Median Income for Households with Children IN POVERTY
results_poverty_children <- svy_design %>%
  filter(has_children == TRUE & in_poverty == TRUE) %>%
  summarize(
    group = "Households with Children (In Poverty)",
    mean_inc = survey_mean(HHINCOME, vartype = "ci", level = 0.95)
  )

# -------------------------------------------------------------------------
# 4. View Results
# -------------------------------------------------------------------------

final_results <- bind_rows(results_all_children, results_poverty_children)

print(final_results)

# SPM
ddi <- read_ipums_ddi("large_input/usa_00017.xml")
data <- read_ipums_micro(ddi)

# 1. Handle "Lonely PSUs"
#    This is a standard requirement for IPUMS data in R. 
#    It prevents errors when a stratum has only one sampling unit.
options(survey.lonely.psu = "adjust")

# 2. Prepare the Data & Create Survey Design
#    We perform the `PERNUM == 1` filter BEFORE creating the design object.
#    Since we are analyzing *households*, we must convert the person-level 
#    dataset into a household-level dataset first.

hh_design <- data %>%
  # Keep only the householder to treat this as a dataset of households
  filter(PERNUM == 1) %>%
  
  # Create the survey object
  # ids = CLUSTER (The Primary Sampling Unit)
  # strata = STRATA (The sampling strata)
  # weights = HHWT (Household weights)
  as_survey_design(
    ids = CLUSTER, 
    strata = STRATA, 
    weights = HHWT
  )

# 3. Calculate the Estimate
#    Now we filter the *design object* and calculate the weighted total.
#    Using `survey_total()` automatically gives you the Standard Error (SE).

results <- hh_design %>%
  # Filter for households with children (NCHILD > 0)
  filter(NCHILD > 0) %>%
  
  # Filter for SPM Poverty (Assuming 1 = In Poverty)
  filter(SPMPOV == 1) %>%
  
  # Calculate the total count
  summarise(
    total_households = survey_total(vartype = c("se", "ci"))
  )

# View Results
print(results)
