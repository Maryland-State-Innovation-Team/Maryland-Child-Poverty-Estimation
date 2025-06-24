list.of.packages = c(
  "data.table", "tidycensus", "sf", "dplyr", "ggplot2", "scales", "dotenv", "stringr"
)
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))

setwd("C:/git/Maryland-Child-Poverty-Estimation/")

load_dot_env()
api_key = Sys.getenv("CENSUS_API_KEY")
census_api_key(api_key)

# Utility functions
rss = function(x, na.rm=T){
  return(sqrt(sum(x^2, na.rm)))
}
rowRSS = function(dt, na.rm=T){
  dt_squared = apply(dt, MARGIN=c(1, 2), FUN=function(x){return(x^2)})
  dt_rowSums = rowSums(dt_squared, na.rm)
  dt_rowRSS = sqrt(dt_rowSums)
  return(dt_rowRSS)
}
moePct = function(moe_numerator, moe_denominator, numerator, denominator) {
  if (any(denominator == 0)) {
    warning("Denominator contains zero values. Resulting MOE will be NaN or Inf for those rows.")
  }
  
  # Calculate the percentage (proportion)
  pct = numerator / denominator
  
  # Calculate the term inside the square root
  term_inside_sqrt = moe_numerator^2 - (pct^2 * moe_denominator^2)
  
  # If the term is negative, use 0. Otherwise, calculate the square root.
  moe_pct_numerator = ifelse(term_inside_sqrt < 0, 0, sqrt(term_inside_sqrt))
  
  # Perform the final division
  moe_final = moe_pct_numerator / denominator
  
  return(moe_final)
}

sluggify = function(x){
  return(
    gsub(
      " ",
      "",
      tolower(x)
    )
  )
}

# Variable definitions
demographic_codes = c(
  "_" = "Total",
  "A" = "White Alone",
  "B" = "Black or African American Alone",
  "C" = "American Indian and Alaska Native Alone",
  "D" = "Asian Alone",
  "E" = "Native Hawaiian and Other Pacific Islander Alone",
  "F" = "Some Other Race Alone",
  "G" = "Two or More Races",
  "H" = "White Alone, Not Hispanic or Latino",
  "I" = "Hispanic or Latino"
)

variable_suffixes = list(
  "total" = "001",
  "below" = "002",
  "below_male" = "003",
  "below_male_under5" = "004",
  "below_male_5" = "005",
  "below_male_6to11" = "006",
  "below_male_12to14" = "007",
  "below_male_15" = "008",
  "below_male_16to17" = "009",
  "below_male_18to24" = "010",
  "below_male_25to34" = "011",
  "below_male_35to44" = "012",
  "below_male_45to54" = "013",
  "below_male_55to64" = "014",
  "below_male_65to74" = "015",
  "below_male_over75" = "016",
  "below_female" = "017",
  "below_female_under5" = "018",
  "below_female_5" = "019",
  "below_female_6to11" = "020",
  "below_female_12to14" = "021",
  "below_female_15" = "022",
  "below_female_16to17" = "023",
  "below_female_18to24" = "024",
  "below_female_25to34" = "025",
  "below_female_35to44" = "026",
  "below_female_45to54" = "027",
  "below_female_55to64" = "028",
  "below_female_65to74" = "029",
  "below_female_over75" = "030",
  "above" = "031",
  "above_male" = "032",
  "above_male_under5" = "033",
  "above_male_5" = "034",
  "above_male_6to11" = "035",
  "above_male_12to14" = "036",
  "above_male_15" = "037",
  "above_male_16to17" = "038",
  "above_male_18to24" = "039",
  "above_male_25to34" = "040",
  "above_male_35to44" = "041",
  "above_male_45to54" = "042",
  "above_male_55to64" = "043",
  "above_male_65to74" = "044",
  "above_male_over75" = "045",
  "above_female" = "046",
  "above_female_under5" = "047",
  "above_female_5" = "048",
  "above_female_6to11" = "049",
  "above_female_12to14" = "050",
  "above_female_15" = "051",
  "above_female_16to17" = "052",
  "above_female_18to24" = "053",
  "above_female_25to34" = "054",
  "above_female_35to44" = "055",
  "above_female_45to54" = "056",
  "above_female_55to64" = "057",
  "above_female_65to74" = "058",
  "above_female_over75" = "059"
)

variable_names = c()
variables = c()
for(i in 1:length(variable_suffixes)){
  variable_name_stem = names(variable_suffixes)[i]
  variable_suffix = variable_suffixes[[i]]
  for(j in 1:length(demographic_codes)){
    demographic_code = names(demographic_codes)[j]
    if(demographic_code == "_"){
      demographic_code = ""
    }
    demographic_name = sluggify(demographic_codes[j])
    variable = paste0("B17001", demographic_code, "_", variable_suffix)
    variable_name = paste0(variable_name_stem, "_", demographic_name)
    variable_names = c(variable_names, variable_name)
    variables = c(variables, variable)
  }
}

names(variables) = variable_names

# Tract, PUMA, county, and state poverty level ACS 5-year 2023 B17001
if(!file.exists("input/acs_child_pov_B17001_2023.RData")){
  acs_long_tract = get_acs(
    survey="acs5",
    geography="tract",
    variables = variables,
    state = "MD",
    year = 2023
  )
  acs_long_puma = get_acs(
    survey="acs5",
    geography="puma",
    variables = variables,
    state = "MD",
    year = 2023
  )
  acs_long_county = get_acs(
    survey="acs5",
    geography="county",
    variables = variables,
    state = "MD",
    year = 2023
  )
  acs_long_state = get_acs(
    survey="acs5",
    geography="state",
    variables = variables,
    state = "MD",
    year = 2023
  )
  save(
    acs_long_tract,
    acs_long_puma,
    acs_long_county,
    acs_long_state,
    file="input/acs_child_pov_B17001_2023.RData"
  )
}else{
  load("input/acs_child_pov_B17001_2023.RData")
}
acs_long_tract$geography = "tract"
acs_long_puma$geography = "puma"
acs_long_county$geography = "county"
acs_long_state$geography = "state"
acs_long = rbindlist(list(acs_long_tract, acs_long_puma, acs_long_county, acs_long_state))
acs_long = subset(acs_long, str_count(acs_long$variable, pattern="_")==3)
acs_long$poverty = str_split_i(acs_long$variable, pattern="_", i=1)
acs_long$sex = str_split_i(acs_long$variable, pattern="_", i=2)
acs_long$age = str_split_i(acs_long$variable, pattern="_", i=3)
acs_long$race_ethnicity = str_split_i(acs_long$variable, pattern="_", i=4)
acs_long$child = "Adult"
acs_long$child[
  which(
    acs_long$age %in% c("under5", "5", "6to11", "12to14", "15", "16to17")
  )
] = "Child"

# Aggregate race/ethnicity
acs_long$race_ethnicity[
  which(
    acs_long$race_ethnicity %in% c(
      "americanindianandalaskanativealone",
      "nativehawaiianandotherpacificislanderalone",
      "someotherracealone"
    )
  )
] = "combinedother"

# Aggregate ages
acs_long = acs_long[,.(
  estimate=sum(estimate),
  moe=rss(moe)
), by=.(geography, GEOID, NAME, race_ethnicity, child, sex, poverty)]

# Aggregate sexes
acs_long_total_sex = acs_long[,.(
  estimate=sum(estimate),
  moe=rss(moe)
), by=.(geography, GEOID, NAME, race_ethnicity, child, poverty)]
acs_long_total_sex$sex = "total"
acs_long = rbind(acs_long, acs_long_total_sex)

# Cast wide
acs = dcast(data.table(acs_long), geography+GEOID+NAME+race_ethnicity+sex~child+poverty, value.var="estimate")
acs_moe = dcast(data.table(acs_long), geography+GEOID+NAME+race_ethnicity+sex~child+poverty, value.var="moe")

# Sum estimates and MOEs for children and adults for total population
acs$total_child_pop = rowSums(
  acs[,c("Child_above", "Child_below")], na.rm=T
)
acs_moe$total_child_pop = rowRSS(
  acs_moe[,c("Child_above", "Child_below")], na.rm=T
)
acs$total_pop = rowSums(
  acs[,c("Child_above", "Child_below", "Adult_above", "Adult_below")], na.rm=T
)
acs_moe$total_pop = rowRSS(
  acs_moe[,c("Child_above", "Child_below", "Adult_above", "Adult_below")], na.rm=T
)
acs$total_pov = rowSums(
  acs[,c("Child_below", "Adult_below")], na.rm=T
)
acs_moe$total_pov = rowRSS(
  acs_moe[,c("Child_below", "Adult_below")], na.rm=T
)

# Calculate total/child poverty ratio and MOE ratio
acs$child_pov_pct = acs$Child_below / acs$total_child_pop 
acs_moe$child_pov_pct_moe = moePct(
  moe_numerator=acs_moe$Child_below,
  moe_denominator=acs_moe$total_child_pop,
  numerator=acs$Child_below,
  denominator=acs$total_child_pop
)
acs$pov_pct = acs$total_pov / acs$total_pop 
acs_moe$pov_pct_moe = moePct(
  moe_numerator=acs_moe$total_pov,
  moe_denominator=acs_moe$total_pop,
  numerator=acs$total_pov,
  denominator=acs$total_pop
)

# Merge
setnames(
  acs,
  c("Adult_above", "Adult_below", "Child_above", "Child_below"),
  c("adult_above_poverty_line_pop", "adult_below_poverty_line_pop", "child_above_poverty_line_pop", "child_below_poverty_line_pop")
)
setnames(
  acs_moe,
  c("Adult_above", "Adult_below", "Child_above", "Child_below", "total_child_pop", "total_pop", "total_pov"),
  c("adult_above_poverty_line_pop_moe", "adult_below_poverty_line_pop_moe", "child_above_poverty_line_pop_moe", "child_below_poverty_line_pop_moe", "total_child_pop_moe", "total_pop_moe", "total_pov_moe")
)
acs = merge(acs, acs_moe, by=c("geography","GEOID","NAME","race_ethnicity","sex"))
fwrite(acs, "output/all_geography_calculations.csv")

# Test DP03 Benchmark
acs_tract = subset(acs, geography=="tract" & race_ethnicity=="total" & sex=="total")
setnames(acs_tract, "GEOID", "GEO_ID")

# Load DP03 as groundtruth
dp03 = fread("input/dp03/ACSDP5Y2023.DP03-Data.csv", header=T, select=c("GEO_ID","DP03_0129PE", "DP03_0129PM"))
dp03 = dp03[2:nrow(dp03),] # Remove labels

dp03_tracts = dp03[2:nrow(dp03)] # Census tracts

acs_tract$GEO_ID = paste0("1400000US", acs_tract$GEO_ID)
dp03_tracts = merge(dp03_tracts, acs_tract, by="GEO_ID")
dp03_tracts$child_pov_pct = dp03_tracts$child_pov_pct * 100
dp03_tracts$child_pov_pct_moe = dp03_tracts$child_pov_pct_moe * 100
dp03_tracts$DP03_0129PE = as.numeric(dp03_tracts$DP03_0129PE)
dp03_tracts$DP03_0129PM = as.numeric(dp03_tracts$DP03_0129PM)

ggplot(dp03_tracts, aes(x=DP03_0129PE, y=child_pov_pct)) + geom_point() + geom_abline(slope = 1, intercept = 0)
ggplot(dp03_tracts, aes(x=DP03_0129PM, y=child_pov_pct_moe)) + geom_point() + geom_abline(slope = 1, intercept = 0)
sqrt(mean((dp03_tracts$DP03_0129PE - dp03_tracts$child_pov_pct)^2, na.rm=T))
sqrt(mean((dp03_tracts$DP03_0129PM - dp03_tracts$child_pov_pct_moe)^2, na.rm=T))

# Higher level aggregate substitutions

# Create crosswalks
puma_crosswalk = fread("input/2020_Census_Tract_to_2020_PUMA.txt")
puma_crosswalk$tract_geoid = paste0(
  str_pad(puma_crosswalk$STATEFP, 2, pad="0"),
  str_pad(puma_crosswalk$COUNTYFP, 3, pad="0"),
  str_pad(puma_crosswalk$TRACTCE, 6, pad="0")
)
puma_crosswalk$puma_geoid = paste0(
  str_pad(puma_crosswalk$STATEFP, 2, pad="0"),
  str_pad(puma_crosswalk$PUMA5CE, 5, pad="0")
)
puma_crosswalk$county_geoid = paste0(
  str_pad(puma_crosswalk$STATEFP, 2, pad="0"),
  str_pad(puma_crosswalk$COUNTYFP, 3, pad="0")
)
county_crosswalk = unique(puma_crosswalk[,c("county_geoid", "tract_geoid")])
county_crosswalk_map = county_crosswalk$county_geoid
names(county_crosswalk_map) = county_crosswalk$tract_geoid
puma_crosswalk_map = puma_crosswalk$puma_geoid
names(puma_crosswalk_map) = puma_crosswalk$tract_geoid
puma_crosswalk = puma_crosswalk[,c("puma_geoid", "tract_geoid")]

# Load geometries
puma_geometry = st_read("input/pumas/Maryland_pumas_4326.shp")
puma_geometry = st_transform(puma_geometry, 3857)
puma_geometry$area = st_area(puma_geometry)
county_geometry = st_read("input/counties/Maryland_Physical_Boundaries_-_County_Boundaries_(Generalized).shp")
county_geometry$area = st_area(county_geometry)
county_geometry$GEOID = paste0(
  "24",
  str_pad(county_geometry$county_fip, 3, pad="0")
)

acs$high_child_pov_pct_moe = acs$child_pov_pct < acs$child_pov_pct_moe * 0.5
acs$high_child_pov_pct_moe[which(is.na(acs$high_child_pov_pct_moe))] = F
acs$high_pov_pct_moe = acs$pov_pct < acs$pov_pct_moe * 0.5 
acs$high_pov_pct_moe[which(is.na(acs$high_pov_pct_moe))] = F

acs = data.frame(acs)

acs_tract = subset(acs, geography=="tract")
acs_puma = subset(acs, geography=="puma")
acs_county = subset(acs, geography=="county")
acs_state = subset(acs, geography=="state")

acs_tract$total_poverty_geography = "tract"
acs_tract$child_poverty_geography = "tract"
acs_tract$geography = NULL

acs_puma$total_poverty_geography = "puma"
acs_puma$child_poverty_geography = "puma"
acs_puma$geography = NULL

acs_county$total_poverty_geography = "county"
acs_county$child_poverty_geography = "county"
acs_county$geography = NULL

acs_state$total_poverty_geography = "state"
acs_state$child_poverty_geography = "state"
acs_state$geography = NULL

# For every census tract
child_substitution_columns = c(
  "child_pov_pct",
  "child_pov_pct_moe",
  "child_poverty_geography"
)
total_substitution_columns = c(
  "pov_pct",
  "pov_pct_moe",
  "total_poverty_geography"
)

find_substitute_data = function(tract_row, moe_column_name, geo_hierarchy, geo_datasets, geo_ids) {
  # Iterate through the geographic levels in the specified order
  for (geo_level in geo_hierarchy) {
    
    # Get the dataset and GEOID for the current level
    dataset = geo_datasets[[geo_level]]
    geoid = geo_ids[[geo_level]]
    
    # Find a candidate row in the current geographic dataset that matches
    # the demographic profile but does NOT have a high margin of error.
    candidate_row = subset(
      dataset,
      GEOID == geoid &
        race_ethnicity == tract_row$race_ethnicity &
        sex == tract_row$sex &
        !get(moe_column_name) # Safely get the column value by its string name
    )
    
    # If a valid substitute is found (with low MOE), return it immediately.
    if (nrow(candidate_row) > 0) {
      # Implicitly take the first matching row if there are multiple.
      return(candidate_row[1,])
    }
  }
  
  # If the loop completes without finding any valid substitute, return NULL.
  return(NULL)
}


# A named list of all geographic datasets for easy access in the helper function
geo_datasets = list(
  puma = acs_puma,
  county = acs_county,
  state = acs_state
)

pb = txtProgressBar(min = 0, max = nrow(acs_tract), style = 3)

for (i in 1:nrow(acs_tract)) {
  
  tract = acs_tract[i,]
  
  # Get GEOIDs for the corresponding PUMA and county
  puma_geoid = puma_crosswalk_map[tract$GEOID]
  county_geoid = county_crosswalk_map[tract$GEOID]
  
  # Create a named list of all relevant GEOIDs for this tract
  geo_ids = list(
    puma = puma_geoid,
    county = county_geoid,
    state = "24" # Hardcoded
  )
  
  # Determine the search order: try the smaller geographic area first
  puma_area = subset(puma_geometry, GEOID == puma_geoid)$area
  county_area = subset(county_geometry, GEOID == county_geoid)$area
  
  geo_hierarchy = if (puma_area > county_area) {
    # If PUMA is larger, try County first, then PUMA, then State
    c("county", "puma", "state")
  } else {
    # Otherwise, try PUMA first, then County, then State
    c("puma", "county", "state")
  }
  
  if (tract$high_child_pov_pct_moe) {
    substitute_row = find_substitute_data(
      tract_row       = tract,
      moe_column_name = "high_child_pov_pct_moe",
      geo_hierarchy   = geo_hierarchy,
      geo_datasets    = geo_datasets,
      geo_ids         = geo_ids
    )
    
    # If the helper function found a valid substitute, update the columns
    if (!is.null(substitute_row)) {
      acs_tract[i, child_substitution_columns] = substitute_row[, child_substitution_columns]
    }
  }

  if (tract$high_pov_pct_moe) {
    substitute_row = find_substitute_data(
      tract_row       = tract,
      moe_column_name = "high_pov_pct_moe",
      geo_hierarchy   = geo_hierarchy,
      geo_datasets    = geo_datasets,
      geo_ids         = geo_ids
    )
    
    # If the helper function found a valid substitute, update the columns
    if (!is.null(substitute_row)) {
      acs_tract[i, total_substitution_columns] = substitute_row[, total_substitution_columns]
    }
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

fwrite(acs_tract, "output/acs_5year_2023_geosubstitution.csv")
View(table(acs_tract$child_poverty_geography))
View(table(subset(acs_tract, race_ethnicity=="total" & sex=="total")$child_poverty_geography))
