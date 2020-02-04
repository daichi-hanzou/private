train = df_BS %>% 
  select(ACCOUNT_GROUP, ACCOUNT_NAME, forth_code)

all_cnt = train %>% 
  summarise(total_cnt = n())

train_cnt = train %>% 
  group_by(ACCOUNT_GROUP) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  mutate(total_cnt = all_cnt$total_cnt) %>% 
  mutate(p_1 = sqrt(cnt/total_cnt))

vec_master_tmp = aggregate(df_BS$ACCOUNT_NAME, list(df_BS$ACCOUNT_GROUP), paste, collapse="") %>%
  rename(ACCOUNT_GROUP = Group.1, 
         vec = x) %>% 
  left_join(train_cnt, by="ACCOUNT_GROUP")

vec_master = aggregate(df_BS$ACCOUNT_NAME, list(df_BS$ACCOUNT_GROUP), paste, collapse="_") %>%
  rename(ACCOUNT_GROUP = Group.1, 
         vec_2 = x) %>%
  left_join(vec_master_tmp, by="ACCOUNT_GROUP")
  
test_train = train %>% 
  mutate(label = sapply(ACCOUNT_NAME, test_func))

key_list = unique(train$ACCOUNT_NAME)
test_1 = test_func(vec_master, name_list)

list_tmp=list()

df_vec = vec_master %>% 
  mutate(flg_tmp = if_else(str_detect(vec, pattern="現金"), 1, 0.1))



test = test_func(vec_master, name_list)

test_func = function(vec_master, key_list){
  
  list_tmp = list()
  
  for (key in key_list){
    
    df_vec = vec_master %>% 
      # keyに部分一致する勘定科目であれば、フラグを立てる
      mutate(flg_tmp = case_when( 
        str_detect(vec, pattern=c(key)) ~ 1)) #%>%
    # NAは0に置換する
    #mutate(flg_tmp = if_else(is.na(flg_tmp), 0.1, flg_tmp)) #%>% 
    #mutate(c = mapply(count_words, key, vec)) %>% 
    #mutate(d = p_1*c) %>% 
    #filter(d == max(d)) %>% 
    #mutate(acc_name = name)
    
    list_tmp = c(list_tmp, list(df_vec))
  }
  
  df_master = rbindlist(list_tmp)
  
  return(df_master)
}


count_words = function(word1, word2){
  vec_word1 = unlist(strsplit(word1, NULL))
  vec_word_tmp2 = unlist((strsplit(word2, NULL)))
  vec_word2 = unique(vec_word_tmp2)
  num = 0
  for (i in vec_word1){
    vec_tmp = vec_word2[vec_word2 == i]
    df_cnt = data.frame(cnt = vec_tmp[0:length(vec_tmp)])
    cnt_tmp = as.integer(count(df_cnt))
    num = cnt_tmp + num
  } 
  return(num)
}

