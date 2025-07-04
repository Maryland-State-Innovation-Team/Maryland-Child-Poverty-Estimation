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

# Download tract-level ACS data for 2018-2023 and add year column
if(!file.exists("input/acs_child_pov_B17001_2018_2023.RData")){
  acs_long_tract_list = list()
  years = 2018:2023
  for (yr in years) {
    acs_long_tract = get_acs(
      survey = "acs5",
      geography = "tract",
      variables = variables,
      state = "MD",
      year = yr
    )
    acs_long_tract$year = yr
    acs_long_tract_list[[as.character(yr)]] = acs_long_tract
  }
  acs_long_tract_all = rbindlist(acs_long_tract_list)
  save(acs_long_tract_all, file="input/acs_child_pov_B17001_2018_2023.RData")
}else{
  load("input/acs_child_pov_B17001_2018_2023.RData")
}

acs_long_tract_all = subset(acs_long_tract_all, str_count(acs_long_tract_all$variable, pattern="_")==3)
acs_long_tract_all$poverty = str_split_i(acs_long_tract_all$variable, pattern="_", i=1)
acs_long_tract_all$sex = str_split_i(acs_long_tract_all$variable, pattern="_", i=2)
acs_long_tract_all$age = str_split_i(acs_long_tract_all$variable, pattern="_", i=3)
acs_long_tract_all$race_ethnicity = str_split_i(acs_long_tract_all$variable, pattern="_", i=4)
acs_long_tract_all$child = "Adult"
acs_long_tract_all$child[
  which(
    acs_long_tract_all$age %in% c("under5", "5", "6to11", "12to14", "15", "16to17")
  )
] = "Child"

# Aggregate race/ethnicity
acs_long_tract_all$race_ethnicity[
  which(
    acs_long_tract_all$race_ethnicity %in% c(
      "americanindianandalaskanativealone",
      "nativehawaiianandotherpacificislanderalone",
      "someotherracealone"
    )
  )
] = "combinedother"

# Aggregate ages
acs_long_tract_all = acs_long_tract_all[,.(
  estimate = sum(estimate),
  moe = rss(moe)
), by = .(year, GEOID, NAME, race_ethnicity, child, sex, poverty)]

# Aggregate sexes
acs_long_total_sex = acs_long_tract_all[,.(
  estimate = sum(estimate),
  moe = rss(moe)
), by = .(year, GEOID, NAME, race_ethnicity, child, poverty)]
acs_long_total_sex$sex = "total"
acs_long_tract_all = rbind(acs_long_tract_all, acs_long_total_sex)

# Cast wide
acs = dcast(data.table(acs_long_tract_all), year+GEOID+NAME+race_ethnicity+sex~child+poverty, value.var="estimate")
acs_moe = dcast(data.table(acs_long_tract_all), year+GEOID+NAME+race_ethnicity+sex~child+poverty, value.var="moe")

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
acs = merge(acs, acs_moe, by=c("year","GEOID","NAME","race_ethnicity","sex"))
fwrite(acs, "output/all_year_calculations.csv")

# Only keep tracts for further analysis
acs_tract = acs
acs_tract$high_child_pov_pct_moe = acs_tract$child_pov_pct < acs_tract$child_pov_pct_moe * 0.5
acs_tract$high_child_pov_pct_moe[which(is.na(acs_tract$high_child_pov_pct_moe))] = F
acs_tract$high_pov_pct_moe = acs_tract$pov_pct < acs_tract$pov_pct_moe * 0.5 
acs_tract$high_pov_pct_moe[which(is.na(acs_tract$high_pov_pct_moe))] = F
acs_tract = data.frame(acs_tract)

# Prepare wide table for 2023 tracts
acs_tract_2023 = subset(acs_tract, year == 2023)

child_substitution_columns = c(
  "child_pov_pct",
  "child_pov_pct_moe"
)
total_substitution_columns = c(
  "pov_pct",
  "pov_pct_moe"
)

acs_tract_2023$child_poverty_year = 2023
acs_tract_2023$total_poverty_year = 2023

# Helper to find substitute in previous years
find_substitute_year = function(tract_row, moe_column_name, years_desc, acs_tract_all) {
  for (yr in years_desc) {
    candidate_row = subset(
      acs_tract_all,
      year == yr &
        GEOID == tract_row$GEOID &
        race_ethnicity == tract_row$race_ethnicity &
        sex == tract_row$sex &
        !get(moe_column_name)
    )
    if (nrow(candidate_row) > 0) {
      return(candidate_row[1,])
    }
  }
  return(NULL)
}

# For every census tract in 2023, check MOE and substitute from previous years if needed
pb = txtProgressBar(min = 0, max = nrow(acs_tract_2023), style = 3)
years_desc = 2022:2018
for (i in 1:nrow(acs_tract_2023)) {
  tract = acs_tract_2023[i,]
  if (tract$high_child_pov_pct_moe) {
    substitute_row = find_substitute_year(
      tract_row = tract,
      moe_column_name = "high_child_pov_pct_moe",
      years_desc = years_desc,
      acs_tract_all = acs_tract
    )
    if (!is.null(substitute_row)) {
      acs_tract_2023[i, child_substitution_columns] = substitute_row[, child_substitution_columns]
      acs_tract_2023$child_poverty_year[i] = substitute_row$year
    } else {
      acs_tract_2023[i, child_substitution_columns] = NA
      acs_tract_2023$child_poverty_year[i] = NA
    }
  }
  if (tract$high_pov_pct_moe) {
    substitute_row = find_substitute_year(
      tract_row = tract,
      moe_column_name = "high_pov_pct_moe",
      years_desc = years_desc,
      acs_tract_all = acs_tract
    )
    if (!is.null(substitute_row)) {
      acs_tract_2023[i, total_substitution_columns] = substitute_row[, total_substitution_columns]
      acs_tract_2023$total_poverty_year[i] = substitute_row$year
    } else {
      acs_tract_2023[i, total_substitution_columns] = NA
      acs_tract_2023$total_poverty_year[i] = NA
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

fwrite(acs_tract_2023, "output/acs_5year_2023_yearsubstitution.csv")
View(table(acs_tract_2023$child_poverty_year, useNA="always"))
View(table(subset(acs_tract_2023, race_ethnicity=="total" & sex=="total")$child_poverty_year, useNA="always"))
