library(tidyverse)
library(readxl)
library(data.table)

df = read_xlsx("/Users/hattoridaichi/R_studio/Practice/DCM/test_data.xlsx")

year_list = c(2014, 2015, 2016, 2017, 2018)

col_list_prev = c("営業利益率", "原価率")

for (year_itr in year_list) {
  
  year_flg = sym(paste0(year_itr, "_flg"))

  df = df %>% 
    mutate(!!year_flg := if_else(year<year_itr, 0, 1))
} 

func = list(
  `25percentile` = ~ quantile(., 0.25, na.rm=T),
  `50percentile` = ~ quantile(., 0.5, na.rm=T),
  `75percentile` = ~ quantile(., 0.75, na.rm=T))


get_percentile = function(df, year_list, col_list_prev) {
  
  list_df_tmp = list()
  for (year_itr in year_list) {
    
    year_flg = sym(paste0(year_itr, "_flg"))
    
    df_tmp = df %>% 
      filter(!!year_flg == 0) %>% 
      group_by(segment) %>% 
      mutate_at(vars(one_of(col_list_prev)), func) %>%
      ungroup() %>% 
      select(segment, matches("[0-9]{2}percentile$")) %>%
      mutate(year_fixed = year_itr) %>% 
      distinct()
    
    list_df_tmp = c(list_df_tmp, list(df_tmp))
  }
  df_tmp_all = rbindlist(list_df_tmp)
  return(df_tmp_all)
}

df_percentile = get_percentile(df, year_list, col_list_prev)
