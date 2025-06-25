list.of.packages = c(
  "data.table", "tidycensus", "sf", "dplyr", "ggplot2", "scales", "dotenv", "stringr"
)
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))

setwd("C:/git/Maryland-Child-Poverty-Estimation/")

acs_tract = fread("output/acs_5year_2023_geosubstitution.csv")
acs_tract = subset(acs_tract, sex=="total" & race_ethnicity=="total")

setnames(acs_tract, "GEOID", "GEO_ID")

dp03 = fread("input/dp03/ACSDP5Y2023.DP03-Data.csv", header=T, select=c("GEO_ID","DP03_0129PE", "DP03_0129PM"))
dp03 = dp03[2:nrow(dp03),] # Remove labels

dp03_tracts = dp03[2:nrow(dp03)] # Census tracts

acs_tract$GEO_ID = paste0("1400000US", acs_tract$GEO_ID)
dp03_tracts = merge(dp03_tracts, acs_tract, by="GEO_ID")
dp03_tracts$child_pov_pct = dp03_tracts$child_pov_pct * 100
dp03_tracts$child_pov_pct_moe = dp03_tracts$child_pov_pct_moe * 100
dp03_tracts$DP03_0129PE = as.numeric(dp03_tracts$DP03_0129PE)
dp03_tracts$DP03_0129PM = as.numeric(dp03_tracts$DP03_0129PM)

dp03_tracts$half_moe_threshold = dp03_tracts$DP03_0129PE
dp03_tracts$half_moe_threshold[which(
  dp03_tracts$half_moe_threshold < 0.5 * dp03_tracts$DP03_0129PM
)] = (0.5 * dp03_tracts$DP03_0129PM)[which(
  dp03_tracts$half_moe_threshold < 0.5 * dp03_tracts$DP03_0129PM
)]

ggplot(dp03_tracts, aes(x=DP03_0129PE, y=child_pov_pct)) +
  geom_point() +
  geom_hline(yintercept=30) +
  geom_abline(slope = 1, intercept = 0)
ggplot(dp03_tracts, aes(x=DP03_0129PE, y=half_moe_threshold)) +
  geom_point() +
  geom_hline(yintercept=30) +
  geom_abline(slope = 1, intercept = 0)

sum(dp03_tracts$DP03_0129PE >= 30, na.rm=T)
# 169 tracts above 30% child poverty by original DP03 table
sum(dp03_tracts$child_pov_pct >= 30, na.rm=T)
# 191 tracts above 30% child poverty by regional adjusted
sum(dp03_tracts$half_moe_threshold >= 30, na.rm=T)
# 169 tracts above 30% child poverty by 1/2 MOE adjusted

sum(round(dp03_tracts$child_pov_pct) > round(dp03_tracts$DP03_0129PE), na.rm=T)
# 524 tracts with higher child poverty rates by regional adjustment
sum(round(dp03_tracts$child_pov_pct) < round(dp03_tracts$DP03_0129PE), na.rm=T)
# 26 tracts with lower child poverty rates by regional adjustment
mean(dp03_tracts$child_pov_pct - dp03_tracts$DP03_0129PE, na.rm=T)
# Average of 3.2% higher than DP03

sum(round(dp03_tracts$half_moe_threshold) > round(dp03_tracts$DP03_0129PE), na.rm=T)
# 289 tracts with higher child poverty rates by 1/2 MOE adjusted
sum(round(dp03_tracts$half_moe_threshold) < round(dp03_tracts$DP03_0129PE), na.rm=T)
# 0 tracts with lower child poverty rates by 1/2 MOE adjusted
mean(dp03_tracts$half_moe_threshold - dp03_tracts$DP03_0129PE, na.rm=T)
# Average of 0.77% higher than DP03

