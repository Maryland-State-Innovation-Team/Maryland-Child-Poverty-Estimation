list.of.packages <- c(
  "data.table", "httr", "jsonlite"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only = TRUE))

get_saipe = function(
  year,
  key,
  geography="county",
  state="24",
  variables=c(
    "SAEPOVRT0_17_PT",
    "SAEPOVRT0_17_MOE",
    "SAEPOVRTALL_PT",
    "SAEPOVRTALL_MOE"
  )
){
  if(geography=="state"){
    for_in = paste0("&for=state:", state)
  }else{
    for_in = paste0("&for=county:*&in=state:", state)
  }
  saipe_url = paste0(
    "https://api.census.gov/data/timeseries/poverty/saipe?get=",
    paste(variables, collapse=","),
    for_in,
    "&time=",
    year,
    "&key=",
    key
  )
  saipe = data.frame(fromJSON(saipe_url))
  names(saipe) = saipe[1,]
  saipe = saipe[-1,]
}
