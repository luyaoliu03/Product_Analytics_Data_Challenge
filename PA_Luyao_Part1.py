"""
==================================================
=======For: Sr.Product Analyst @Conde Nast========
=======Author: Luyao Liu==========================
==================================================
"""

##This code only covers Part I and full code is in R.

import pandas as pd
import matplotlib.pyplot as plt
import os

os.chdir("/Users/dorisliu/Documents/Simon Strong/CMC/Job Searching/Assessment/Conde Nast")#Please change this path to your own dataset folder


df= pd.read_csv("Data_Challenge_Data.csv")

##################Part I########################

##(1) How many Pageviews did Glamour have in August?

G_subset = df[(df['Brand']=='Glamour') & (df['Date'] >= '2019-08-01') & (df['Date'] <= '2019-08-31')]
len(G_subset[G_subset['Event_Name'] == 'Pageview'])
## 4087 Pageviews happened in August for Glamour.

##(2) How many people saw a video on CNT?


CNT_subset = df[(df['Brand'] == 'Conde Nast Traveler') & (df['Event_Name'] == 'Impression') & (df['Component_Name'] == 'Video')]
len(CNT_subset.drop_duplicates(subset = 'User_Id'))
## 7749 people saw a video on CNT.


##(3) What percentage of SELF’s sessions are bounces. Which device type has the highest bounce rate?

S_subset = df[(df['Brand'] == 'Self') & (df['Event_Name'] == 'Pageview')]


S_sub_groupby = S_subset.groupby('Session_Id').count()
pct = len(S_sub_groupby[S_sub_groupby['Event_Name'] == 1])/len(S_sub_groupby)*100
round(pct,2)
## 67.09% of SELF's sessions are bounces.

device =  list(df[df['Brand'] == 'Self']['Device'].drop_duplicates())

i =0
device_percents = []

while i <3:
    S_d = S_subset[S_subset['Device'] == device[i]]
    S_d_groupby = S_d.groupby('Session_Id').count()
    pct_device = len(S_d_groupby[S_d_groupby['Event_Name'] == 1])/len(S_d_groupby)*100
    S_d_output = (round(pct_device,2),device[i])
    device_percents.append(S_d_output)
    i +=1
device_percents
x = []
y = []
for device in device_percents:
    x.append(device[1])
    y.append(device[0])
plt.ylim(60,70)
plt.xlabel('Device')
plt.ylabel('Bounce Rate')
plt.title('Bounce Rate by Device')
plt.bar(x,y)
plt.show()
