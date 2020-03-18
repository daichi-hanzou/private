library(tidyverse)
library(jsonlite)
library(data.table)

api_key = 
ads_vec = 

get_location = function(ads_vec, api_key) {
  
  list_tmp = list()
  
  for (ads in ads_vec) {
    
    txt = paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", ads,
                 "components=country:JP&key=", api_key)
    res = fromJSON(txt)
    
    df_tmp = res$results$geometry$location
    df_tmp = df_tmp %>% 
      mutate(ads = ads)
    
    list_tmp = c(list_tmp, list(df_tmp))
  }
  
  df_tmp_all = rbindlist(list_tmp, use.names=T, fill=T)
  return(df_tmp_all)
}

df_test = get_location(ads_vec, api_key)
