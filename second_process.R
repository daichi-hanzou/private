library(tidyverse)
library(readxl)
library(data.table)

rm(list=ls())

df_all = fread(file="/Users/hattoridaichi/Desktop/df_labeled.tsv", encoding="UTF-8")
df_master = fread(file="/Users/hattoridaichi/Desktop/df_labeled_all.tsv", encoding="UTF-8")
###################################################################################

# ループで集約科目のkeyと一致する勘定科目をラベルする関数
label_function = function(df, key_list, label, code) {
  
  list_tmp = list()
  # keyリストからのループ
  for (key in key_list) {
    
    df_tmp = df %>% 
      # keyに部分一致する勘定科目であれば、フラグを立てる
      mutate(flg_tmp = case_when( 
        str_detect(ACCOUNT_NAME, pattern=c(key)) ~ 1)) %>%
      # NAは0に置換する
      mutate(flg_tmp = if_else(is.na(flg_tmp), 0, flg_tmp)) %>% 
      group_by(COMPANY, !!!sym(code)) %>% 
      # 同じ会社で、codeが一緒であれば、同様にフラグを立てる
      mutate_at(vars(flg_tmp), max) %>% 
      ungroup() %>% 
      # フラグがあるならば、集約科目をラベルする
      mutate(col_label = if_else(flg_tmp==1, label, "NA")) %>% 
      select(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER, ACCOUNT_GROUP, col_label) %>%
      # ラベルしたデータフレームのみを抽出する
      filter(col_label != "NA")
    
    # keyごとに作成したリストに格納する
    list_tmp = c(list_tmp, list(df_tmp))
  }
  
  # リスト内のデータフレームを結合する
  df_all = rbindlist(list_tmp, fill=T)
  
  return(df_all)
}

# ラベルされなかったデータを取り出す
df_na = df_all %>%
  filter(is.na(col_label)) %>% 
  select(-col_label)

# 正解率の低くい集約科目
# 売上原価
sales_cost_list = c("売上原価",  "仕入高", "仕掛品", "商品仕入", "間接費")
df_salescost_labeled = label_function(df_na, sales_cost_list, "売上原価", "forth_code")

# 販売管理費及び一般管理費
gene_cost_list = c("販管", "役員報酬", "基準内賃金", "広告宣伝費", "職員給与",
                   "役員賞与", "販売費及び一般管理費", "役員 賞与", "一般管理費")
df_genecost_labeled = label_function(df_na, gene_cost_list, "販売費及び一般管理費", "third_code")

# ラベルされたデータをユニオンにする
df_master_sec = rbindlist(list(df_salescost_labeled, df_genecost_labeled)) %>% 
  select(-ACCOUNT_GROUP) %>% 
  distinct()

# マスターを結合する
df_all_sec = df_na %>% 
  left_join(df_master_sec, by=c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER")) 


df_sec_acc = df_all_sec %>%
  # ラベルの部分のみ抽出する
  filter(!(is.na(col_label))) %>%
  # 会社とラベルでグループ化して、その勘定科目番号の平均を取り出す
  mutate(account_number_ave = account_number) %>% 
  group_by(COMPANY, col_label) %>% 
  mutate_at(vars(account_number_ave), mean) %>%
  ungroup() %>%
  # ラウンドする
  mutate(account_number_ave = round(account_number_ave, 0))

# 複数ラベルをもつ勘定科目を取り出す
df_sec_mul =df_sec_acc %>%
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  mutate(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt>1) 

# 1つだけのラベルをもつデータを取り出す
df_sec_fixed = df_sec_acc %>% 
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  mutate(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt==1)

# 各会社、ラベルごとの勘定科目番号平均のマスタを作成する
df_master_sec_mul = df_sec_mul %>% 
  select(account_number_ave, col_label, COMPANY) %>% 
  rename(label = col_label, 
         num = account_number_ave) %>% 
  distinct()

get_nearest_value = function(account_number, COMPANY){
  
  company = COMPANY
  
  df_tmp = df_master_sec_mul %>% 
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

# 会社名のリストを作成する
company_list = unique(df_master_sec_mul$COMPANY)

# 関数を適用する
df_result_sec = get_acc_func(df_sec_mul, df_master_sec_mul, company_list) %>% 
  rename(col_label = label)

# マスタを結合する
df_master_all = rbindlist(list(df_master,df_result_sec, df_sec_fixed),
                               use.names=T, fill=T) %>% 
  select(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER, ACCOUNT_GROUP, col_label) %>% 
  distinct()

write.table(df_master_all, file="/Users/hattoridaichi/Desktop/df_master_all.tsv",
            sep="\t", col.names=T, row.names=F, quote=F, fileEncoding="UTF-8")

