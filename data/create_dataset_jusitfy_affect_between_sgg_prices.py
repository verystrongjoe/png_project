# -*- coding: utf-8 -*-
"""
Created on Sun Jun  7 16:27:53 2020
@author: UKJO
"""


"""
https://drive.google.com/file/d/1a3TjxB4lyak1J1hebaqR-UhU0EU0fFIV/view
https://drive.google.com/file/d/1SLfvppKcGu-xEQ14vJ_yBlqaFM_p-Rf5/view
https://drive.google.com/file/d/1uDaB7EnJ_Ia-akZwXbYlfCCTMV1VHSEp/view

<데이터>
- 데이터셋은 지난회의에서 추가하지않고 EDA때 사용한 마지막 데이터셋 그대로 사용
- 26개의 시군구와 152개월치 데이터 즉, 3952개 시군구 월별 데이터를 사용하였슴
- 설명 가능한 피쳐 10개로 정리하여 PIR_소득 소득 처분가능소득 , dfg,  각종 면적등은 시계열로 봐도 계단으로 나와 제거
  사용한 피쳐 : 년월, 매매가격지수, 전세가격지수, PIR, 구별 인구이동, 지하철, 아파트브랜드, 혼인건수, 미분양

<최종 자료 발표 대비 3개 시나리오 대비 코드 구현>

1. 시점 구분 없애고 인과관계 (Undirected Graph) 추출
- 시계열 특성이 잘 들어나지 않고 계단식 같이 변경되는 discrete한 변수들은 제거하여 10개의 변수들을 뽑았습니다.  그다음 시군구와 월별 구분 또한 제거하여 표준 스케일 처리를 하였고 이 데이터를 가지고 glasso, mb, adaptive lasso를 돌려서 결과생성

ㅇ 위의 10가지 모든 설명변수들 고려하여 피쳐셋 구성
ㅇ 가격으로만 고려하여 피쳐셋 구성
ㅇ 매매가격지수와 edge가 맺어진 피쳐들로만 구성하여 피쳐셋 구성
    - 예를 들면 2018년 12월 송파구 기준으로 3개월 6개월 12개월 이전의 모든 다른 시군구의 가격변수와 가격 설명변수를 같은 row에 생성하여 데이터셋 구성하게 변경


2. 시군구별로 주변 시군구의 가격 영향 그래프
- 시군구별 가격의 영향들을 볼 수 있도록 하기 위하여 교수님 발표때도 언급한 것처럼 3,6,12개월 데이터를 시군구간 비교하게 만들 필요가 있었음
- 강남 3구와 마용성 정도일텐데 우선 모든 시군구별로 데이터 생성

 
3. 시점 구분 없애고 인과관계 (Directed Graph) 추출
"""

import numpy as np
import pandas as pd
import os
from datetime import datetime, timedelta
# reference http://egloos.zum.com/mcchae/v/11203068
from dateutil.relativedelta import relativedelta

os.getcwd()
os.chdir("D:/data/onedrive/OneDrive - korea.ac.kr/2020-1/확률그래프모델및네트워크데이터/프로젝트/")

N_ROWS = 3952
N_MONTH = 152
N_SGG = 26

# df = pd.read_csv('data_only_price_sgg.csv', delimiter=',', encoding='EUC-KR', index_col=['sgg','yyyymm'])
df = pd.read_csv('data_only_price_sgg.csv', delimiter=',', encoding='EUC-KR')
# df_indexed= df.set_index(['sgg','yyyymm'])
df= df.set_index(['sgg','yyyymm'], drop=False)


#df.loc['강남구', 201902]['price_index']
#df.loc['강남구', 201902]['중랑구_3']

data = np.zeros(N_ROWS)

len(df['sgg'].unique()) # 26
len(df['yyyymm'].unique())  # 152 

# 26 * 152 = 3952
list_sgg = df['sgg'].unique()
list_yyyymm = df['yyyymm'].unique()
month_windows = [3, 6, 12]

for sgg in list_sgg:
    for m in month_windows:
        df[sgg+"_"+str(m)] = data

for sgg in list_sgg:
    for yyyymm in list_yyyymm:
        for target_sgg in list_sgg:      
            for m in month_windows:
                target_yyyymm = int((datetime.strptime(str(yyyymm), '%Y%m')  - relativedelta(months=m)).strftime('%Y%m'))
                target_column = target_sgg + "_" + str(m)
                target_price = 0 
                try:
                    target_price  = df.loc[target_sgg, target_yyyymm]['price_index']
                except TypeError:
                    print('{} key is impossible'.format(target_yyyymm))
                
                
                df.loc[sgg, yyyymm][target_column] = target_price
            
        

            

