library(tidyverse)
library(readxl)
library(data.table)

rm(list=ls())

df_master = fread(file="/Users/hattoridaichi/Desktop/df_master_all.tsv", encoding="UTF-8")

# ファイル取り込み
df_raw = read_xlsx("/Users/hattoridaichi/Desktop/train_data.xlsx")

col_list = c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER", "ACCOUNT_GROUP")

# PLデータの抽出
df_PL = df_raw %>% 
  filter(BP_DIV == "PL") %>% 
  select(one_of(col_list))

# ラベルが貼られたデータにコードを取り出したカラムを作成する
df_label = df_PL %>% 
  left_join(df_master, by=c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER", "ACCOUNT_GROUP"))


###############################################################################

# ラベルがNAデータのみを抽出する
df_label_na = df_label %>% 
  filter(is.na(col_label)) %>% 
  select(one_of(col_list))

# ラベルされた集約科目の候補データを取り出す
df_label_master = df_label %>% 
  filter(!(is.na(col_label))) %>% 
  mutate(first_code = substr(ACCOUNT_NUMBER, 1, 1)) %>% 
  mutate(second_code = substr(ACCOUNT_NUMBER, 1, 2)) %>% 
  mutate(third_code = substr(ACCOUNT_NUMBER, 1, 3)) %>% 
  mutate(forth_code =  substr(ACCOUNT_NUMBER, 1, 4)) %>% 
  mutate(fifth_code = substr(ACCOUNT_NUMBER, 1, 5)) %>% 
  select(COMPANY, col_label, first_code, second_code,third_code, forth_code, fifth_code) %>%
  distinct() 

# マスタを結合する

# first_code
df_label_first_result = df_label_na %>% 
  mutate(first_code = substr(ACCOUNT_NUMBER, 1, 1)) %>% 
  left_join(df_label_master %>% select(COMPANY, col_label, first_code),
            by=c("COMPANY", "first_code")) %>% 
  select(-first_code) %>% 
  distinct()

# ラベルが一つのみの勘定科目を抽出する
df_label_first_fixed = df_label_first_result %>% 
  filter(!(is.na(col_label))) %>% 
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt == 1) %>%
  select(-cnt) %>% 
  inner_join(df_label_first_result,
             by=c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER")) %>% 
  select(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER, col_label)

# ラベルが複数となる勘定科目を抽出する
df_label_first_mul = df_label_first_result %>% 
  filter(!(is.na(col_label))) %>%
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt > 1) %>% 
  select(-cnt) %>%
  inner_join(df_label_first_result,
             by=c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER")) %>% 
  select(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER)

# second_code
# 複数ラベルがついた勘定科目をsecond_codeで結合する
df_label_second_result = df_label_first_mul %>% 
  mutate(second_code = substr(ACCOUNT_NUMBER, 1, 2)) %>% 
  left_join(df_label_master %>% select(COMPANY, col_label, second_code),
            by=c("COMPANY", "second_code")) %>% 
  select(-second_code) %>% 
  distinct()

# NAのみを抽出しておく
df_label_second_na = df_label_second_result %>% 
  filter(is.na(col_label))

# ラベルが一つのみの勘定科目を抽出する
df_label_second_fixed = df_label_second_result %>% 
  filter(!(is.na(col_label))) %>% 
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt == 1) %>%
  select(-cnt) %>% 
  distinct() %>%
  inner_join(df_label_second_result,
             by=c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER"))

# ラベルが複数となる勘定科目を抽出する
df_label_second_mul = df_label_second_result %>% 
  filter(!(is.na(col_label))) %>%
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt > 1) %>% 
  select(-cnt)

# third_code
# 複数ラベルがついた勘定科目をthird_codeで結合する
df_label_third_result = df_label_second_mul %>% 
  mutate(third_code = substr(ACCOUNT_NUMBER, 1, 3)) %>% 
  left_join(df_label_master %>% select(COMPANY, col_label, third_code),
            by=c("COMPANY", "third_code")) %>% 
  select(-third_code) %>% 
  distinct()

# NAのみを抽出しておく
df_label_third_na = df_label_third_result %>% 
  filter(is.na(col_label))

# ラベルが一つのみの勘定科目を抽出する
df_label_third_fixed = df_label_third_result %>% 
  filter(!(is.na(col_label))) %>% 
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt == 1) %>%
  select(-cnt) %>% 
  distinct() %>%
  inner_join(df_label_third_result,
             by=c("COMPANY", "ACCOUNT_NAME", "ACCOUNT_NUMBER"))

# ラベルが複数となる勘定科目を抽出する
df_label_third_mul = df_label_third_result %>% 
  filter(!(is.na(col_label))) %>%
  group_by(COMPANY, ACCOUNT_NAME, ACCOUNT_NUMBER) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  filter(cnt > 1) %>% 
  select(-cnt)

# forth_code
# 複数ラベルがついた勘定科目をforth_codeで結合する
df_label_forth_result = df_label_third_mul %>% 
  mutate(forth_code = substr(ACCOUNT_NUMBER, 1, 4)) %>% 
  left_join(df_label_master %>% select(COMPANY, col_label, forth_code),
            by=c("COMPANY", "forth_code")) %>% 
  select(-forth_code)

# forth_codeは全てNA
df_label_fixed_all = rbindlist(list(df_label_first_fixed, df_label_second_fixed, df_label_third_fixed))

##########################################################################

df_master_all = rbindlist(list(df_master%>% select(-ACCOUNT_GROUP), df_label_fixed_all))

write.table(df_master_all, file="/Users/hattoridaichi/Desktop/df_master_all_sec.tsv",
            sep="\t", col.names=T, row.names=F, quote=F, fileEncoding="UTF-8")
