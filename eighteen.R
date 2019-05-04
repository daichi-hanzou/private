library(tidyverse)
library(readxl)
library(data.table)
library(config)
    
# configよりデータファイルパスの読み込み
config = config::get(file = "config.yml")

# 取り込みファイル全てのパスリストを作成する
file_list = list.files(config$var, pattern="xlsx", recursive=T, full.names=T)

# 連結仕訳帳とキャッシャフローでファイルリストを作成する
file_list_BSPL = file_list[grep("連結仕訳帳", file_list)]
file_list_CF = file_list[grep("CF", file_list)]

# jtbd_read_xlsxのファイルを取込む


# データのカラム形式が過年度で異なる
# 相違点：カラム名のコードが半角と全角になっいる
# 2016年より前のデータが半角であり、それ以降は全角
# 以下関数(論理式)によって、年度でファイルを分岐させて、取込みとカラム名の修正を行う
read_data = function (file) {
  
  # ファイル名の年度を取り出す
  year_int = as.integer(str_sub(basename(file), start=3, end=4))
  
  # 年度が2016年度より前のデータ
  if (year_int < 15) {
    
    # データを取込み
    df_tmp = read_xlsx(file) %>% 
      # 必要カラムを名前を変更して、作成する
      mutate(プロジェクトコード = `プロジェクト(ｺｰﾄﾞ)`) %>% 
      mutate(会社コード = `会社(ｺｰﾄﾞ)`) %>% 
      mutate(勘定科目コード = `勘定科目(ｺｰﾄﾞ)`) 
  
  } else{
      
    df_tmp = read_xlsx(file) %>% 
      # 必要カラムを名前を変更して、作成する
      mutate(プロジェクトコード = `プロジェクト(コード)`) %>% 
      mutate(会社コード = `会社(コード)`)%>% 
      mutate(勘定科目コード = `勘定科目(コード)`) 
    }
  
  return(df_tmp)
}

# 各財務諸表のファイルパスリストを受け取って、データを取り込み・結合する関数を作成する
bind_data = function (file_list) {
  
  # file_listをループさせて、ファイルを取り込む
  for (file in file_list) {
    
    if (!exists("df_tmp_all", inherits=F)) {
      
      # read_data関数の適用する
      df_tmp_all = read_data(file)
      
    } else {
      
      df_tmp = read_data(file)
      
      # 取り込んだデータを結合させる
      df_tmp_all = rbindlist(list(df_tmp_all, df_tmp), use.names=T, fill=T)  
    }
  }
  return(df_tmp_all)
}

# file_list_BSPLにbind_dataを適用する
df_BSPL_all = bind_data(file_list_BSPL)

# file_list_CFにbind_dataを適用する
df_CF_all = bind_data(file_list_CF)

# 連結仕訳帳とキャッシフローのデータと結合する
df_all = rbindlist(list(df_BSPL_all, df_CF_all), use.name=T, fill=T) %>% 
  # カラム名を変更して、新規のカラムを作成する
  mutate(会社名 = `会社(名称)`) %>% 
  mutate(勘定科目名 = `勘定科目(名称)`) %>% 
  # プロジェクトコードの要素を取り出して、年度カラムを作成する
  mutate(年度 = as.integer(substr(.$プロジェクトコード, 4, 7))-1) %>%
  # 必要カラムのみ取り出す
  select(年度, 会社コード, 会社名, 勘定科目コード, 勘定科目コード, 貸借金額)

# 必要マスタデータの取り込み

# df_seg

# df_coumpany_code

# df_account_code

# 上記、取込みと加工したデータに対して以下の処理を行う
# 1.過年度内の子会社が統一されているものがあるため、df_company_codeを使って、集約する
# 2.更新した会社情報を使って、セグメント情報を持たせる
# 3.指標計算の為に、df_account_codeを使って、勘定科目を集約する
# 4.会社ごとに、集約した勘定科目で貸借金額を合算する

df_all_fixed = df_all %>%
  # 元データに対して、以下のkeyを使って会社コードを結合させる
  left_join(df_company_code, by=c("会社コード"="過去コード")) %>%
  # 新しいカラムを作成して、現在のコードと会社名に統一する
  mutate(会社コード_fix = if_else(!is.na(現在コード), 現在コード, 会社コード)) %>% 
  mutate(会社名_fix = if_else(is.na(現在会社名), 現在会社名, 会社名)) %>% 
  # データにセグメント情報を持たせる
  left_join(df_seg, by=c("会社コード_fix"="会社コード")) %>% 
  # 勘定科目データを集約する
  left_join(df_account_code, by="勘定科目コード") %>% 
  # 新しいカラム名を作成して、集約科目とそれを持たない科目を取り出す
  mutate(勘定科目_fix = if_else(!is.na(勘定科目), 勘定科目, 勘定科目名)) %>% 
  # 必要カラムを取り出す
  select(勘定科目コード, 年度, 勘定科目_fix, 会社コード_fix, 会社名_fix, セグメント, 貸借金額) %>% 
  # カラム名の変更
  mutate(会社コード = 会社コード_fix) %>% 
  mutate(勘定科目 = 勘定科目_fix) %>% 
  mutate(会社名 = 会社名_fix) %>% 
  select(-c(会社コード_fix, 勘定科目_fix, 会社名_fix)) %>% 
  # 以下の項目でデータをグループ化する
  group_by(年度, 会社コード, 勘定科目, セグメント) %>% 
  summarise(金額 = sum(金額)) %>% 
  ungroup()
  
#データの出力







