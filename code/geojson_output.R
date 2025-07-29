list.of.packages = c(
  "data.table", "tidycensus", "sf", "dplyr", "ggplot2", "scales", "dotenv", "stringr",
  "httr"
)
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))

setwd("C:/git/Maryland-Child-Poverty-Estimation/")

# Load iMap Census boundaries and transform to lat/lon
tracts = st_read("input/imap_tracts/Maryland_Census_Boundaries_-_Census_Tracts_2020.shp")
tracts = st_transform(tracts, 4326)
tracts = tracts[,c("GEOID20")]
tract_names = fread("input/md_geoid_names.csv")
tracts = merge(tracts, tract_names, by.x="GEOID20", by.y="tract_geoid")

# Load geo method and subset to total disaggregation
acs_tract = fread("output/acs_5year_2023_geosubstitution.csv")
acs_tract = subset(acs_tract, sex=="total" & race_ethnicity=="total")
acs_tract$child_pov_pct_cv = (acs_tract$child_pov_pct_moe / 1.645) / acs_tract$child_pov_pct
acs_tract$child_pov_pct_cv[which(is.infinite(acs_tract$child_pov_pct_cv))] = 0
acs_tract = acs_tract[,c(
  "GEOID",
  "child_pov_pct",
  "child_pov_pct_cv",
  "child_poverty_geography"
)]
names(acs_tract) = c("GEOID20", "GEO-child_pov_pct", "GEO-child_pov_pct_cv", "GEO-substitution")

# Load time method and subset to total disaggregation
acs_tract_year = fread("output/acs_5year_2023_yearsubstitution.csv")
acs_tract_year = subset(acs_tract_year, sex=="total" & race_ethnicity=="total")
acs_tract_year$child_pov_pct_cv = (acs_tract_year$child_pov_pct_moe / 1.645) / acs_tract_year$child_pov_pct
acs_tract_year$child_pov_pct_cv[which(is.infinite(acs_tract_year$child_pov_pct_cv))] = 0
acs_tract_year = acs_tract_year[,c(
  "GEOID",
  "child_pov_pct",
  "child_pov_pct_cv",
  "child_poverty_year"
)]
names(acs_tract_year) = c("GEOID20", "TIME-child_pov_pct", "TIME-child_pov_pct_cv", "TIME-substitution")

# Load DP03 baseline and calculate thresholding method
dp03 = fread("input/dp03/ACSDP5Y2023.DP03-Data.csv", header=T, select=c("GEO_ID","DP03_0129PE", "DP03_0129PM"))
dp03 = dp03[2:nrow(dp03),] # Remove labels

dp03_tracts = dp03[2:nrow(dp03)] # Census tracts
dp03_tracts$GEO_ID = gsub("1400000US", "", dp03_tracts$GEO_ID)
dp03_tracts$DP03_0129PE = as.numeric(dp03_tracts$DP03_0129PE) / 100
dp03_tracts$DP03_0129PM = as.numeric(dp03_tracts$DP03_0129PM) / 100
dp03_tracts$child_pov_pct_cv = (dp03_tracts$DP03_0129PM / 1.645) / dp03_tracts$DP03_0129PE
dp03_tracts$child_pov_pct_cv[which(is.infinite(dp03_tracts$child_pov_pct_cv))] = 0

# Load S0101 for under 18 precalculated
s0101_url = "https://api.census.gov/data/2023/acs/acs5/subject?get=group(S0101)&ucgid=pseudo(0400000US24$1400000)"
s0101_req = GET(s0101_url)
s0101_content = content(s0101_req)
## Replace NULLs with NAs
list_no_nulls <- lapply(s0101_content, function(row) {
  lapply(row, function(item) {
    if (is.null(item)) NA else item
  })
})

## Combine lists into a data frame and unlist the columns
s0101 = as.data.frame(do.call(rbind, list_no_nulls))
s0101 = as.data.frame(lapply(df_base, unlist))
names(s0101) = s0101[1,]
s0101 = s0101[2:nrow(s0101),c("GEO_ID", "S0101_C01_022E")]
s0101$GEO_ID = gsub("1400000US", "", s0101$GEO_ID)
s0101$S0101_C01_022E = as.numeric(s0101$S0101_C01_022E)
setnames(s0101, c("S0101_C01_022E"), c("under18_pop"))
dp03_tracts = merge(dp03_tracts, s0101, by="GEO_ID")

dp03_tracts$suppressed =
  dp03_tracts$DP03_0129PE == 0 &
  dp03_tracts$under18_pop > 10 &
  dp03_tracts$DP03_0129PM > 0.1
dp03_tracts$under18_pop = NULL
dp03_tracts$`THRESH-child_pov_pct` = dp03_tracts$DP03_0129PE
dp03_tracts$`THRESH-child_pov_pct`[which(dp03_tracts$suppressed)] = 
  (0.5 * dp03_tracts$DP03_0129PM)[which(dp03_tracts$suppressed)]

dp03_tracts$`THRESH-child_pov_pct_cv` = dp03_tracts$child_pov_pct_cv
dp03_tracts$`THRESH-substitution` = dp03_tracts$`THRESH-child_pov_pct` != dp03_tracts$DP03_0129PE
setnames(
  dp03_tracts,
  c("GEO_ID", "DP03_0129PE", "child_pov_pct_cv"),
  c("GEOID20", "BASE-child_pov_pct", "BASE-child_pov_pct_cv")
)
dp03_tracts$DP03_0129PM = NULL

# Merge
tracts = merge(
  tracts,
  dp03_tracts,
  by="GEOID20"
)
tracts = merge(
  tracts,
  acs_tract,
  by="GEOID20"
)
tracts = merge(
  tracts,
  acs_tract_year,
  by="GEOID20"
)
st_write(tracts, "output/comparison.geojson", delete_dsn = T)
