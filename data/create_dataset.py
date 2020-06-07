# -*- coding: utf-8 -*-
"""
Created on Sun Jun  7 12:50:55 2020
@author: UKJO
"""
import numpy as np
import pandas as pd
import os

os.getcwd()
os.chdir("D:/data/onedrive/OneDrive - korea.ac.kr/2020-1/확률그래프모델및네트워크데이터/프로젝트/")

df = pd.read_csv('data_only_price_sgg.csv', delimiter=',', encoding='EUC-KR')

#month_window = 3, 6, 12

len(df['sgg'].unique()) # 26
len(df['yyyymm'].unique())  # 152 

# 26 * 152 = 3952
list_sgg = df['sgg'].unique()
list_yyyymm = df['yyyymm'].unique()

list_yyyymm[0]


for sgg in list_sgg:
    for yyyymm in list_yyyymm:
        print( 
                df[ 
                        (df.sgg == sgg) & (df.yyyymm == yyyymm )                       ] 
        )
        


df.yyyymm = pd.to_datetime(df.yyyymm, format='%Y%m')

df.yyyymm = df.yyyymm - pd.to_datetime('000003', format='%Y%m')

