# coding: shift_jis
import numpy as np
import pandas as pd
import os
from scipy.interpolate import interp1d

os.getcwd()
os.chdir("/Users/hattoridaichi/python/practice/raw_data/sample_data")

#データの読み込み
df_MITC_raw = pd.read_table("MITC_modified_progress_data.tsv")
#スプライン補間出来ない工事Noを除外する
df_MITC = df_MITC_raw[(df_MITC_raw["除外フラグ"]==1) & ~(df_MITC_raw["工事No"].isin(["A5665K0100", "A4631L3650"]))]
#データ加工後のデータフレームに各金額情報を載せるため、下記カラムを変数(重複しないデータ)として、取り出す
df_amount_info = df_MITC[["契約金額_fix", "見積原価_fix", "工事No"]].drop_duplicates().rename(columns = {"契約金額_fix":"契約金額", "見積原価_fix":"見積原価"})
#データ加工後のデータフレームにrawデータを載せるため、下記データを取り出す
df_rowdata = df_MITC[["工事No", "期間進捗率_fix", "原価進捗率_fix"]].rename(columns = {"期間進捗率_fix":"期間進捗率", "原価進捗率_fix":"原価進捗率"})
#rawデータにカラムを作り、フラグとする
df_rowdata["raw_flg"] = 1

cons_project = df_MITC["工事No"].unique()

term_list = np.array([0.2, 0.4, 0.6, 0.8])

def spline_calculation(df, cons_no):

    xdata = df.loc[df["工事No"].isin([cons_no]), ["期間進捗率_fix"]].values.flatten()
    ydata = df.loc[df["工事No"].isin([cons_no]), ["原価進捗率_fix"]].values.flatten()
    spline_func = interp1d(xdata, ydata, kind="cubic", fill_value="extrapolate")
    term_range = term_list[(max(xdata) >= term_list) & (min(xdata) <= term_list)]
    df_result = pd.DataFrame({"原価進捗率":spline_func(term_list), "期間進捗率":term_range})
    df_result["工事No"] = cons_no
    return df_result

list_spline = []
for cons_no in cons_project:
    df_spline = spline_calculation(df_MITC, cons_no)
    list_spline.append(df_spline)
df_spline = pd.concat(list_spline)


def threshold_percentile(df_spline, term):

    df_term = df_spline.loc[df_spline["期間進捗率"].isin([term])]
    upper_threshold = np.percentile(df_term["原価進捗率"], 95, interpolation='lower')
    lower_threshold = np.percentile(df_term["原価進捗率"], 5, interpolation='higher')
    df_term["flg_thr_percentile"] = df_term["原価進捗率"].apply(lambda x: get_flg_percentile(x, upper_threshold, lower_threshold))
    return df_term

def get_flg_percentile(x, upper_threshold, lower_threshold):

    if x > upper_threshold or x < lower_threshold:
        return 1
    else:
        return 0

flg_list = []
for term in term_list:
    df_flg_percentile = threshold_percentile(df_spline, term)
    flg_list.append(df_flg_percentile)
df_flg_percentile = pd.concat(flg_list)


def IQR_threshold(df_spline, term):
    df_term = df_spline.loc[df_spline["期間進捗率"].isin([term])]
    Q1 = df_term["原価進捗率"].quantile(.25)
    Q3 = df_term["原価進捗率"].quantile(.75)
    IQR = Q3 - Q1
    thresholed_Q1 = Q1 - 1.5*IQR
    thresholed_Q3 = Q3 + 1.5*IQR
    df_term["flg_thr_IQR"] = df_term["原価進捗率"].apply(lambda x: get_flg_IQR(x, thresholed_Q1, thresholed_Q3))
    return df_term

def get_flg_IQR(x, thresholed_Q1, thresholed_Q3):

    if x > thresholed_Q3 or x < thresholed_Q1:
        return 1
    else:
        return 0

flg2_list = []
for term in term_list:
    df_flg_all = IQR_threshold(df_flg_percentile, term)
    flg2_list.append(df_flg_all)
df_flg_all = pd.concat(flg2_list)

#フラグ付きのデータを横持ちのデータに変換する
df_flgall_spread = df_flg_all.pivot_table(values=["原価進捗率"], columns = ["期間進捗率"], index=["工事No"]).reset_index()
new_colnames = ["工事No", "0.2", "0.4", "0.6", "0.8"]
df_flgall_spread.columns = new_colnames

#縦持ちデータについては、rawデータと金額データを結合する
df_flg_all["raw_flg"] = 0
df_all_tydy = pd.concat([df_flg_all, df_rowdata]).fillna({"flg_thr_IQR":0, "flg_thr_percentile":0})
df_all_tydy = pd.merge(df_all_tydy, df_amount_info, on =["工事No"], how = "left")
