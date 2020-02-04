# データクレンジング
# 以下正解率の高くなる集約科目ラベルのマッピング処理

# 1.keyによるラベル処理

# 集約科目を表すkeyをリスト化
# リスト内のkeyと一致する勘定科目をラベルする
# ラベルされた勘定科目と同じ会社と勘定科目番号の頭4つが同じであれば、同様にラベルする

# 2.重複するラベルの平均値による選択

# ラベル処理後、全ての集約科目について、関数より出力されたデータを結合させて、マスタ化する
# マスタを元データに結合させて、ラベルを貼り付ける
# ラベルのついてる部分のみ抽出して、会社と集約ラベルでグループ化して、勘定科目番号の平均を計算する
# 重複するラベルについては、その勘定科目番号がラベルの平均値に近い方にラベルされるようにする

library(tidyverse)
library(readxl)
library(data.table)

rm(list=ls())

#######################################################################

# ファイル取り込み
df_raw = read_xlsx("/Users/hattoridaichi/Desktop/train_data.xlsx")

# データクレンジング
df_train = df_raw %>%
  # 勘定科目番号内の数字以外の文字を取り除いたカラムを作成する
  mutate(account_number = str_replace_all(ACCOUNT_NUMBER, pattern="[[:alpha:]]", replacement="")) %>%
  mutate(account_number = str_replace_all(account_number, pattern="\\*", replacement="")) %>%
  mutate(account_number = if_else(is.na(account_number), 0, as.numeric(account_number))) %>% 
  mutate(first_code = substr(ACCOUNT_NUMBER, 1, 1)) %>% 
  mutate(second_code = substr(ACCOUNT_NUMBER, 1, 2)) %>% 
  mutate(third_code = substr(ACCOUNT_NUMBER, 1, 3)) %>% 
  mutate(forth_code =  substr(ACCOUNT_NUMBER, 1, 4)) %>% 
  mutate(fifth_code = substr(ACCOUNT_NUMBER, 1, 5)) 


df_PL = df_train %>% 
  filter(BP_DIV == "PL") %>% 
  select(-c(BP_DIV, ACCOUNTING_SYSTEM))
#######################################################################
# 自動マッピング加工処理

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
      # 同じ会社で、勘定科目番号の頭4つが一緒であれば、同様にフラグを立てる
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

# 正解率の高くなる集約科目

# 特別損失
ex_loss_list = c("特別損失", "特損", "減損損失")
df_exloss_labeled = label_function(df_PL, ex_loss_list, "特別損失", "forth_code")

# 特別利益
ex_pro_list = c("特別利益", "特別収益", "特益", "特利")
df_expro_labeled = label_function(df_PL, ex_pro_list, "特別利益", "forth_code")

# 税金費用
tax_list = c("法人税") 
df_tax_labeled = label_function(df_PL, tax_list, "税金費用", "forth_code")

# 営業外収益
non_ope_income_list = c("営業外収益", "受取利息", "受取配当金")
df_nonopein_labeled = label_function(df_PL, non_ope_income_list, "営業外収益", "forth_code")

# 営業外費用
non_ope_cost_list = c("営業外費用", "雑損", "為替差損", "支払利息", "為替差額")
df_nonopecos_labeled = label_function(df_PL, non_ope_cost_list, "営業外費用", "forth_code")

# 売上高
sales_list = c("売上高", "商品売上", "取扱収入", "商品供給高")
df_sales_labeled = label_function(df_PL, sales_list, "売上高", "forth_code")

# その他
other_list = c("仕入勘定（商品）", "入金決済勘定"
               , "組織変更用仮勘定（在庫）", "製造予定配賦額",
               "保証債務 海外", "仕掛品振替高＿一般", "売上原価振替仮勘定",
               "就業時間", "主原料＿製造勘定", "製造(材料費)主原料費", "移行用仮勘定")
df_other_labeled = label_function(df_PL, other_list, "その他", "forth_code")

# マスタの作成
# 関数によってラベルしたデータを結合してマスタにする
df_master_acc = rbindlist(list(df_exloss_labeled, df_expro_labeled,
                           df_tax_labeled, df_nonopein_labeled,
                           df_nonopecos_labeled, df_sales_labeled, 
                           df_other_labeled)) %>% 
  select(-c(ACCOUNT_GROUP, ACCOUNTING_SYSTEM)) %>% 
  distinct()

# 元データへの結合
df_labeled = df_PL %>% 
  left_join(df_master_acc, by=c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER")) 

write.table(df_labeled, file="/Users/hattoridaichi/Desktop/df_labeled.tsv",
            sep="\t", col.names=T, row.names=F, quote=F, fileEncoding="UTF-8")

# マスタを結合して、ラベル貼り付ける
df_labeled_acc = df_labeled %>%
  # ラベルの部分のみ抽出する
  filter(!(is.na(col_label))) %>%
  mutate(account_number_ave = account_number) %>%
  # 企業とラベルでグループ化して、勘定科目番号で平均を算出する
  group_by(COMPANY, col_label) %>%
  mutate_at(vars(account_number_ave), mean) %>%
  ungroup() %>%
  # 算出した平均値をラウンドする
  mutate(account_number_ave = round(account_number_ave, 0))

# ラベルが複数ある勘定科目は抽出する
df_labeled_mul = df_labeled_acc %>% 
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  mutate(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt>1) %>% 
  select(-c(cnt, BP_DIV, first_code))

# ラベルが1つだけ勘定科目は別で変数を作成しておく
df_labeled_fixed = df_labeled_acc %>% 
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  mutate(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt==1) %>% 
  select(-c(cnt, BP_DIV, first_code))

# 各会社、ラベルごとの勘定科目番号平均のマスタを作成する
df_master_mul = df_labeled_mul %>% 
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
company_list = unique(df_labeled_mul$COMPANY)

# 上の関数を適用する
df_result_fir = get_acc_func(df_labeled_mul, df_master_mul, company_list) %>% 
  rename(col_label = label)

# 一つだけラベルされたカラムをユニオンにする 
df_labeled_all = rbindlist(list(df_labeled_fixed, df_result_fir), use.names=T, fill=T) %>% 
  select(-c(account_number_ave, num))

write.table(df_labeled_all, file="/Users/hattoridaichi/Desktop/df_labeled_all.tsv",
            sep="\t", col.names=T, row.names=F, quote=F, fileEncoding="UTF-8")

