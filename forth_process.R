library(tidyverse)
library(readxl)
library(data.table)

rm(list=ls())

df_master = fread(file="/Users/hattoridaichi/Desktop/df_master_all_sec.tsv", encoding="UTF-8")

# ファイル取り込み
df_raw = read_xlsx("/Users/hattoridaichi/Desktop/train_data.xlsx")

col_list = c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER", "ACCOUNT_GROUP")

# PLデータの抽出
df_PL = df_raw %>% 
  filter(BP_DIV == "PL") %>% 
  select(one_of(col_list))

# ラベルが貼られたデータにコードを取り出したカラムを作成する
df_label = df_PL %>% 
  left_join(df_master, by=c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER")) %>% 
  mutate(first_code = substr(ACCOUNT_NUMBER, 1, 1)) %>%
  # 勘定科目番号内の数字以外の文字を取り除いたカラムを作成する
  mutate(account_number = str_replace_all(ACCOUNT_NUMBER, pattern="[[:alpha:]]", replacement="")) %>%
  mutate(account_number = str_replace_all(account_number, pattern="\\*", replacement="")) %>%
  mutate(account_number = if_else(is.na(account_number), 0, as.numeric(account_number))) %>%
  mutate(account_number_ave = account_number) %>% 
  # 企業とラベルでグループ化して、勘定科目番号で平均を算出する
  group_by(COMPANY, col_label) %>%
  mutate_at(vars(account_number_ave), mean) %>%
  ungroup() %>%
  # 算出した平均値をラウンドする
  mutate(account_number_ave = round(account_number_ave, 0)) 

df_label %>% 
  mutate(flg = if_else(ACCOUNT_GROUP==col_label, 1, 0)) %>% 
  group_by(flg) %>% 
  summarise(cnt=n()) %>% 
  ungroup()

#################################################################################

# NAのみを取り出す
df_label_na = df_label %>% 
  filter(is.na(col_label)) %>% 
  select(-c(col_label, account_number_ave))

# NA以外を取り出して集約科目の候補データにする
df_label_master = df_label %>% 
  filter(!(is.na(col_label))) %>% 
  select(COMPANY, col_label, first_code, account_number_ave) %>% 
  distinct() 

# 候補データを貼り付ける
df_label_first_result = df_label_na %>% 
  left_join(df_label_master, by=c("COMPANY", "first_code")) %>% 
  select(-first_code) %>% 
  distinct()

# NAのみを抽出しておく
df_label_first_na = df_label_first_result %>% 
  filter(is.na(col_label))

# ラベルが複数となる勘定科目を抽出する
df_label_first_mul = df_label_first_result %>% 
  filter(!(is.na(col_label)))

# 各会社、ラベルごとの勘定科目番号平均のマスタを作成する
df_master_mul = df_label_first_mul %>% 
  select(account_number_ave, col_label, COMPANY) %>% 
  rename(label = col_label, 
         num = account_number_ave) %>% 
  distinct()

# ベクトル内の最も近い値を取り出す関数
get_nearest_value = function(account_number, company){
  
  df_tmp = df_master_mul %>% 
    filter(COMPANY == company)
  
  vec = unique(df_tmp$num)
  
  tmp_value = which(abs(vec-account_number) == min(abs(vec-account_number)))
  
  return(vec[tmp_value][1])
}

# 各会社でループさせて、重複する勘定科目番号が最も近いラベルを採用する関数を定義する
get_acc_func = function(df, df_master, company_list) {
  
  list_tmp = list()
  
  for (company in company_list) {
    
    df_tmp = df %>% 
      filter(COMPANY == company)
    
    df_master_tmp = df_master %>% 
      filter(COMPANY == company)
    
    df_all_tmp = df_tmp %>% 
      mutate(num = mapply(get_nearest_value, account_number, COMPANY))
    
    df_result = df_all_tmp %>% 
      left_join(df_master_tmp, by=c("num", "COMPANY")) %>% 
      select(-c(col_label, account_number_ave)) %>% 
      distinct()
    
    list_tmp = c(list_tmp, list(df_result))
  }
  
  df_all = rbindlist(list_tmp, fill=T)
  
  return(df_all)
}

# 会社のリストを作成する
company_list = unique(df_label_first_mul$COMPANY)

# 上の関数を適用する
df_result_fir = get_acc_func(df_label_first_mul, df_master_mul, company_list) %>% 
  rename(col_label = label)%>% 
  mutate(flg = if_else(ACCOUNT_GROUP==col_label, 1, 0)) %>% 
  group_by(flg) %>% 
  summarise(cnt=n()) %>% 
  ungroup() 
  