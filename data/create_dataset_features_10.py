# -*- coding: utf-8 -*-
"""
Created on Sun Jun  7 12:50:55 2020
@author: UKJO
"""
import numpy as np
import pandas as pd
import os
from sklearn.preprocessing import StandardScaler

os.getcwd()
os.chdir("D:/data/onedrive/OneDrive - korea.ac.kr/2020-1/확률그래프모델및네트워크데이터/프로젝트/data")

df = pd.read_csv('data_10_features.csv', delimiter=',', encoding='EUC-KR')
df.columns

df_dropped_sgg = df.drop(['sgg'], axis=1)
df_dropped_sgg = df_dropped_sgg.drop(['yyyymm'], axis=1)


df_dropped_sgg = df_dropped_sgg.fillna(0)
df_dropped_sgg.unsold = df_dropped_sgg.unsold.replace('-',0)


df_dropped_sgg.describe()
df_dropped_sgg.info()  # move_population, apart_brand, unsold


for y in df_dropped_sgg.columns:
    if(df_dropped_sgg[y].dtype == np.float64 or df_dropped_sgg[y].dtype == np.int64):
        pass
    else:
        df_dropped_sgg[y].replace(df_dropped_sgg[y].replace(',', ''))
        df_dropped_sgg[y] = df_dropped_sgg[y].astype(int)


scaler = StandardScaler()
dropped_sgg_standardized = scaler.fit_transform(df_dropped_sgg) # y도 하는건가?
df_dropped_sgg_standardized = pd.DataFrame(dropped_sgg_standardized, index=df_dropped_sgg.index, columns=df_dropped_sgg.columns)

df_dropped_sgg_standardized.head()

df_dropped_sgg_standardized.to_csv('df_10_features_dropped_sgg_standardized.csv', encoding='EUC-KR')







