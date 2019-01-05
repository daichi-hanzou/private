# coding: shift_jis
import numpy as np
import pandas as pd
import os
from scipy.interpolate import interp1d

os.getcwd()
os.chdir("/Users/hattoridaichi/python/practice/raw_data/sample_data")

#�f�[�^�̓ǂݍ���
df_MITC_raw = pd.read_table("MITC_modified_progress_data.tsv")
#�X�v���C����ԏo���Ȃ��H��No�����O����
df_MITC = df_MITC_raw[(df_MITC_raw["���O�t���O"]==1) & ~(df_MITC_raw["�H��No"].isin(["A5665K0100", "A4631L3650"]))]
#�f�[�^���H��̃f�[�^�t���[���Ɋe���z�����ڂ��邽�߁A���L�J������ϐ�(�d�����Ȃ��f�[�^)�Ƃ��āA���o��
df_amount_info = df_MITC[["�_����z_fix", "���ό���_fix", "�H��No"]].drop_duplicates().rename(columns = {"�_����z_fix":"�_����z", "���ό���_fix":"���ό���"})
#�f�[�^���H��̃f�[�^�t���[����raw�f�[�^���ڂ��邽�߁A���L�f�[�^�����o��
df_rowdata = df_MITC[["�H��No", "���Ԑi����_fix", "�����i����_fix"]].rename(columns = {"���Ԑi����_fix":"���Ԑi����", "�����i����_fix":"�����i����"})
#raw�f�[�^�ɃJ���������A�t���O�Ƃ���
df_rowdata["raw_flg"] = 1

cons_project = df_MITC["�H��No"].unique()

term_list = np.array([0.2, 0.4, 0.6, 0.8])

def spline_calculation(df, cons_no):

    xdata = df.loc[df["�H��No"].isin([cons_no]), ["���Ԑi����_fix"]].values.flatten()
    ydata = df.loc[df["�H��No"].isin([cons_no]), ["�����i����_fix"]].values.flatten()
    spline_func = interp1d(xdata, ydata, kind="cubic", fill_value="extrapolate")
    term_range = term_list[(max(xdata) >= term_list) & (min(xdata) <= term_list)]
    df_result = pd.DataFrame({"�����i����":spline_func(term_list), "���Ԑi����":term_range})
    df_result["�H��No"] = cons_no
    return df_result

list_spline = []
for cons_no in cons_project:
    df_spline = spline_calculation(df_MITC, cons_no)
    list_spline.append(df_spline)
df_spline = pd.concat(list_spline)


def threshold_percentile(df_spline, term):

    df_term = df_spline.loc[df_spline["���Ԑi����"].isin([term])]
    upper_threshold = np.percentile(df_term["�����i����"], 95, interpolation='lower')
    lower_threshold = np.percentile(df_term["�����i����"], 5, interpolation='higher')
    df_term["flg_thr_percentile"] = df_term["�����i����"].apply(lambda x: get_flg_percentile(x, upper_threshold, lower_threshold))
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
    df_term = df_spline.loc[df_spline["���Ԑi����"].isin([term])]
    Q1 = df_term["�����i����"].quantile(.25)
    Q3 = df_term["�����i����"].quantile(.75)
    IQR = Q3 - Q1
    thresholed_Q1 = Q1 - 1.5*IQR
    thresholed_Q3 = Q3 + 1.5*IQR
    df_term["flg_thr_IQR"] = df_term["�����i����"].apply(lambda x: get_flg_IQR(x, thresholed_Q1, thresholed_Q3))
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

#�t���O�t���̃f�[�^���������̃f�[�^�ɕϊ�����
df_flgall_spread = df_flg_all.pivot_table(values=["�����i����"], columns = ["���Ԑi����"], index=["�H��No"]).reset_index()
new_colnames = ["�H��No", "0.2", "0.4", "0.6", "0.8"]
df_flgall_spread.columns = new_colnames

#�c�����f�[�^�ɂ��ẮAraw�f�[�^�Ƌ��z�f�[�^����������
df_flg_all["raw_flg"] = 0
df_all_tydy = pd.concat([df_flg_all, df_rowdata]).fillna({"flg_thr_IQR":0, "flg_thr_percentile":0})
df_all_tydy = pd.merge(df_all_tydy, df_amount_info, on =["�H��No"], how = "left")
