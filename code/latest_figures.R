list.of.packages <- c(
  "data.table", "tidycensus", "sf", "dplyr", "ggplot2", "scales",
  "dotenv", "stringr", "httr"
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

# Census tract
tract_child_poverty = 
  get_precomp(
    2024,
    "DP03",
    variables = c(
      child_poverty_pct = "DP03_0129PE",
      child_poverty_moe = "DP03_0129PM"
    ),
    survey="acs5",
    geography = "tract"
  )

tract_s0101 =
  get_precomp(
    2024,
    "S0101",
    variables=c(under18_pop = "S0101_C01_022E"),
    table_type="subject",
    survey="acs5",
    geography="tract"
  )

tract_child_poverty = merge(
  tract_child_poverty,
  tract_s0101,
  by="GEOID"
)
tract_child_poverty$child_poverty_pop =
  (tract_child_poverty$child_poverty_pct / 100) *
  tract_child_poverty$under18_pop

tract_child_poverty$child_poverty_pop_moe =
  (tract_child_poverty$child_poverty_moe / 100) *
  tract_child_poverty$under18_pop

fwrite(tract_child_poverty, "output/dp03_acs5_2024_under18_pov.csv")


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
acs5_long = get_acs(
  geography="tract",
  variables=c(
    "pov_u6"="B17020_003",
    "pov_6to11"="B17020_004",
    "pov_12to17"="B17020_005",
    "above_u6"="B17020_011",
    "above_6to11"="B17020_012",
    "above_12to17"="B17020_013"
  ),
  year=2024,
  survey="acs5",
  state="MD"
)

acs5_agg_total =
  data.table(acs5_long)[,.(
    under18_pop=sum(estimate),
    under18_pop_moe=rss(moe)
  ), by="GEOID"]

acs5_long_pov = subset(acs5_long, variable %in% c("pov_u6", "pov_6to11", "pov_12to17"))
acs5_agg =
  data.table(acs5_long_pov)[,.(
    child_poverty_pop=sum(estimate),
    child_poverty_pop_moe=rss(moe)
  ), by="GEOID"]

acs5_agg = merge(acs5_agg, acs5_agg_total, by="GEOID")
acs5_agg$child_poverty_pct = acs5_agg$child_poverty_pop / acs5_agg$under18_pop
