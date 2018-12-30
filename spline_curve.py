import numpy as np
import pandas as pd
import os
from scipy.interpolate import interp1d

os.getcwd()
os.chdir("/Users/hattoridaichi/python/practice/raw_data/sample_data")

df_MITC = pd.read_csv("curve_ft_sample.csv")
df_MITC_fixed = df_MITC[["PJ", "x-1", "y-1"]]
df_MITC_fixed["raw_flg"] = 1
df_MITC
test = df_MITC[["amount1", "amount2", "PJ"]].drop_duplicates(subset = ["PJ"])

cons_project = df_MITC["PJ"].unique()
term_list = np.array([0.2, 0.4, 0.6, 0.8])
def spline_calculation(df, cons_no):

    xdata = df.loc[df["PJ"].isin([cons_no]), ["x-1"]].values.flatten()
    ydata = df.loc[df["PJ"].isin([cons_no]), ["y-1"]].values.flatten()
    spline_func = interp1d(xdata, ydata, kind="cubic", fill_value="extrapolate")
    term_range = term_list[(max(xdata) >= term_list) & (min(xdata) <= term_list)]
    df_result = pd.DataFrame({"y-1":spline_func(term_list), "x-1":term_range})
    df_result["PJ"] = cons_no
    return df_result

list_spline = []
for cons_no in cons_project:
    df_spline = spline_calculation(df_MITC, cons_no)
    list_spline.append(df_spline)
df_spline = pd.concat(list_spline)


def threshold_uptodawn_percentage(df_spline, term):

    df_term = df_spline.loc[df_spline["x-1"].isin([term])]
    up_threshold = np.percentile(df_term["y-1"], 95, interpolation='lower')
    down_threshold = np.percentile(df_term["y-1"], 5, interpolation='higher')
    df_term["flg_uptodown"] = df_term["y-1"].apply(lambda x: gen_flg(x, up_threshold, down_threshold))
    return df_term

def gen_flg(x, up_threshold, down_threshold):

    if x > up_threshold or x < down_threshold:
        return 1
    else:
        return 0

flg_list = []
for term in term_list:
    df_flg = threshold_uptodawn_percentage(df_spline, term)
    flg_list.append(df_flg)
df_flg = pd.concat(flg_list)


def IQR_threshold(df_spline, term):
    df_term = df_spline.loc[df_spline["x-1"].isin([term])]
    Q1 = df_term["y-1"].quantile(.25)
    Q3 = df_term["y-1"].quantile(.75)
    IQR = Q3 - Q1
    thresholed_Q1 = Q1 - 1.5*IQR
    thresholed_Q3 = Q3 + 1.5*IQR
    df_term["flg_IQR"] = df_term["y-1"].apply(lambda x: gen_flg2(x, thresholed_Q1, thresholed_Q3))
    return df_term

def gen_flg2(x, thresholed_Q1, thresholed_Q3):

    if x > thresholed_Q3 or x < thresholed_Q1:
        return 1
    else:
        return 0

flg2_list = []
for term in term_list:
    df_flg_add = IQR_threshold(df_flg, term)
    flg2_list.append(df_flg_add)
df_flg_fixed = pd.concat(flg2_list)

df_flgfixed_spread = df_flg_add.pivot_table(values=["y-1"], columns = ["x-1"], index=["PJ"]).reset_index()
new_colnames = ["PJ", "0.2", "0.4", "0.6", "0.8"]
df_flgfixed_spread.columns = new_colnames

df_flg_add["raw_flg"] = 0
df_flg_all = pd.concat([df_flg_add, df_MITC_fixed]).fillna({"flg_IQR":0, "flg_uptodown":0})
test = pd.merge(df_flg_all, test ,on =["PJ"], how = "left")
test
