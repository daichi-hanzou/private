
# coding: utf-8

# In[2]:

import pandas as pd


# In[58]:




# In[4]:

def get_year(yyyy_mm):
    
    """"""
    yyyymmから年度を返す関数
    
    """"""
    month = int(yyyymm[-2:])
    
    if month >4:
        
        year = int(yyyymm[:-2])
        
    else:
        
        year = int(yyyymm[:-2])-1
        
    return year    


# In[29]:

def get_rawdata(filepath, df_first_master):
    
    """"""
    # ファイルを読み込んで必要なカラムを追加
    
    """"""
    
    # ファイルの読み込み
    df_raw = pd.read_csv(filepath, encoding="cp932", engine="python")
    
    # データ型の変換と空白の除去
    df_raw["貸方"] = df_raw["貸方"].fillna("0").apply(lambda x: x.strip().replace(",", "")).astype(np.int64)
    df_raw["借方"] = df_raw["借方"].fillna("0").apply(lambda x: x.strip().replace(",", "")).astype(np.int64)
    df_raw["勘定科目コード"] = df_raw["勘定科目コード"].astype(str).apply(lambda x: x.strip())
    df_raw["事業領域コード"] = df_raw["事業領域コード"].astype(str).apply(lambda x: x.strip())
    df_raw["勘定科目名"] = df_raw["勘定科目名"].apply(lambda x: x.strip())
    df_raw["事業領域名"] = df_raw["事業領域名"].apply(lambda x: x.strip())
    
    # 款コードの切り出し
    df_raw["first_code"] = df_raw["勘定科目コード"].str[:3]
    
    #款コードの名称マスタの結合
    df_raw = df_raw.merge(df_first_master, how="left", on="first_code")
    
    # 貸借金額を統一したカラムを作成する    
    df_raw["DC_amount"] = np.where(df_raw["借方"] == 0, df_raw["貸方"],  df_raw["借方"])
     
    #  年度カラムの作成　  年月の切り出し方は違うかも？
    df_raw["yyyymm"] = re.serch(r"¥{6}", filepath.stem).group()
    df_raw["year"] = df_raw["yyyymm"].apply(lambda x: get_year(x))
    
    # 貸借区分を作成
    df_raw["DC"] = np.nan
    conf_d = (df_raw["DC"].isnull()) & (df_raw["借方"]!=0) & ((df_raw["貸方"].isnull()) | (df_raw["貸方"] == 0)) 
    conf_c = (df_raw["DC"].isnull()) & (df_raw["貸方"]!=0) & ((df_raw["借方"].isnull()) | (df_raw["借方"] == 0)) 
    df_raw["DC"] = df["DC"].mask(conf_d, "D")
    df_raw["DC"] = df["DC"].mask(conf_c, "C")
    
    # 伝票番号ごとの貸借金額の合計
    df_debit = df_raw[["伝票番号", "借方"]].groupby("伝票番号", as_index=False).sum().rename(columns={"借方":"debit_sum"})
    df_credit = df_raw[["伝票番号", "貸方"]].groupby("伝票番号", as_index=False).sum().rename(columns={"貸方":"credit_sum"})
    df_raw = df_raw.merge(df_debit, on="伝票番号", how="left").merge(df_credit, on="伝票番号", how="left")
    
    return df_raw    


# In[43]:

def create_vec(df_raw):
    
    """"""
    # 伝票番号ごとにある事業領域、勘定科目、款の文字列結合を作成する
    # 伝票番号に紐づく事業領域をカウント
    
    """"""
    # 事業領域の文字列結合
    df_company_uniq = df_raw[["伝票番号", "事業領域名"]].drop_duplicates()
    df_company_vec = pd.DataFrame(df_company_uniq.groupby("伝票番号"))["事業領域名"].apply(list).apply(lambda x: sorted(x)).apply("_".join).reset_index()
    
    # 事業領域のカウント
    df_id_compay_count = df_company_uniq.groupby("伝票番号", as_index=False)["事業領域名"].count().rename(columns={"事業領域":"company_cnt"})
    df_company_vec = df_campany_vec.merge(df_id_company_count, how="left", on="伝票番号")
    
    # 勘定科目
    df_acc_uniq = df_raw[["伝票番号", "DC", "勘定科目名"]].drop_duplicates()
    df_acc_uniq["DC_acc_name"] = df_acc_uniq["DC"] + "_" + df_acc_uniq["勘定科目名"] 
    df_acc_vec = pd.DataFrame(df_acc_uniq.groupby("伝票番号"))["DC_acc_name"].apply(list).apply(lambda x: sorted(x)).apply("_".join).reset_index()
    
    # 款
    df_first_uniq = df_raw[["伝票番号", "DC","first_name"]].drop_duplicates()
    df_first_uniq["DC_first_name"] = df_first_uniq["DC"] + "_" + df_first_uniq["first_name"]
    df_first_vec = pd.DataFrame(df_first_uniq.groupby("伝票番号"))["DC_first_name"].apply(list).apply(lambda x: sorted(x)).apply("_".join).reset_index()
    
    # 作成した文字列の結合
    df_vec = pd.merge(df_company_vec, df_acc_vec, how="left", on="伝票番号").merge(df_first_vec, how="left", on="伝票番号")
    
    return df_vec
    


# In[46]:

# 事業領域に関するカラムを捨てているのは伝票番号ごとを跨いで紐づいているものがあるから
grouping_list = ["会社コード", "会社名", "会計年度", "会計期間", "転記日付", "伝票日付","yyyymm", "year",
                     "first_code", "first_name","勘定科目コード", "勘定科目名", "DC", "DC_amount", "debit_sum", "credit_sum"]


# In[47]:

def grouping_sum(df_raw, df_vec, groupling_list):
    
    """"""
    # 伝票番号と貸借区分、勘定科目単位で金額集計する
    # 集計データにdf_vecを結合する
    
    """"""
    
    # 金額の集計
    df_grouped = df_raw(grouping_list, as_index=False).agg({"DC_amount":"sum"})
    
    # df_vecの結合
    df_frame = df_grouped.merge(df_vec, how="left", on="伝票番号")
    
    return df_frame


# In[49]:

# 款
first_extract_list = ["伝票番号", "first_code", "DC_amount",  "debit_sum", "credit_sum", "DC"]
first_colname_list = ["伝票番号", "debit_sum", "credit_sum", "debit_first_code", "debit_DC_amount", "credit_first_code", "credit_DC_amount"]

# 勘定科目
acc_extract_list = ["伝票番号", "勘定科目コード", "DC_amount",  "debit_sum", "credit_sum", "DC"]
acc_colname_list = ["伝票番号", "debit_sum", "credit_sum", "debit_acc_code", "debit_DC_amount", "credit_acc_code", "credit_DC_amount"]


# In[50]:

def create_pattern(df_frame, extract_list, colname_list):
    
    """"""
    # リフト値を算出するように、勘定科目、款の借方と貸方の１対１のパターンを作る
    
    """"""
    
    # 必要データのみを抽出する
    df_tmp = df_frame[extract_list]
    
    # 借方と貸方でデータを分割する
    df_debit = df_tmp[df_tmp["DC"] == "D"].drop("DC", axis=1)
    df_credit = df_tmp[df_tmp["DC"] == "C"].drop("DC", axis=1)
    
    # 伝票番号で結合させて、１対１の勘定科目の組み合わせを作成する
    df_pattern = pd.merge(df_debit, df_credit, how="outer", on=["伝票番号", "debit_sum", "credit_sum"])
    df_pattern.colmns = colname_list  
    
    return df_pattern


# In[52]:

def calc_lift(df_pattern,  category):
    
    """"""
    # JAA方式のリフト値を算出する
    
    """"""
    
    # カウントするカラムの文字列を作成
    count_x = "debit_" + category  + "code" 
    count_y = "credit_" + category  + "code"
    
    # データ内の要素のカウント
    df_count_x = df_tmp.groupby(count_x, as_index=False)["伝票番号"].count().rename(columns={"伝票番号":"x_count"})
    df_count_y = df_tmp.groupby(count_y, as_index=False)["伝票番号"].count().rename(columns={"伝票番号":"y_count"})
    df_count_x_y = df_tmp.groupby([count_x, count_y], as_index=False)["伝票番号"].count().rename(columns={"伝票番号":"x_y_count"})
                                                                      
    # トータルカウント数のカラムを追加
    df_count_x_y["total_count"] = df_patten["伝票番号"].count()
    
    # 計算したカウントのデータフレームを結合する
    df_count_all = df_count_x_y.merge(df_count_x, how="lelft", on="count_x").merge(df_count_y, how="lelft", on="count_y")
    loglift = category + "_" + "loglift"
    df_count_all[loglift] = np.log((df_count_all["count_x_y"]/df_count_all["count_x"])/(dfunt_all["count_y"]/df_count_all["total_count"]))
    
    # 必要なカラムのみにする
    df_lift = df_count_all[count_x, count_y, loglift]
    
    return df_lift


# In[56]:

def amount_adjustment(df_lift, df_pattern, category):
    
    """"""
    # JAA方式のリフト値の金額加重平均の調整
    
    """"""
    
    # 文字列を作成する
    debit_code = "debit_" + category  + "code" 
    credit_code = "credit_" + category  + "code"
    loglift = category + "_" + "loglift"
    
    # 各貸借のパターンに対数リフト値を返す
    df_lift_pattern = pd.merge(df_pattern, df_lift, how="left", on=[debit_code,  credit_code])
    
    # 金額調整様の比率の計算
    df_lift_pattern["debit_adjustment"] =  df_lift_patten["debit_amount"]/df_lift_patten["debit_sum"]
    df_lift_pattern["credit_adjustment"] =  df_lift_patten["credit_amount"]/df_lift_patten["credit_sum"]   
   
    # カラム名の作成
    df_lift_pattern[loglift] = df_lift_pattern[loglift]*df_lift_pattern["debit_adjustment"]*df_lift_pattern["cresit_adjustment"]
    
    df_result = df_lift_pattern.groupby("伝票番号", as_index=False)[loglift].sum()
    
    return df_result


# In[57]:

def JAA_lift_processing(df_frame):
    
    """"""
    # JAA方式のリフト値計算処理を行う
    
    """"""
    
    # カテゴリパターンの計算
    df_first_pattern = create_pattern(df_frame, first_extract_list, first_colname_list)
    df_acc_pattern = create_pattern(df_frame, acc_extract_list, acc_colname_list)
    
    # 対数リフト値の計算
    df_first_lift = calc_lift(df_first_pattern, "first")
    df_acc_lift = calc_lift(df_acc_pattern, "acc")
    
    #　伝票番号ごとの対数リフト値の金額調整
    df_first_result = amount_adjustment(df_first_lift, df_first_pattern, "first")
    df_acc_result = amount_adjustment(df_acc_lift, df_acc_pattern, "acc")
    
    # 計算結果をdf_frameに結合する
    df_output = df_frame.merge(df_first_result, how="left", on="伝票番号").merge(df_acc_result, how="left", on="伝票番号")
    
    return df_output


# In[ ]:



