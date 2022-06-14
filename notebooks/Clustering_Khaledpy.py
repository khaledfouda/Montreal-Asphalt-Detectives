# -*- coding: utf-8 -*-
"""
Created on Tue Jun 14 11:40:48 2022

@author: Khaled
"""

import pandas as pd
import numpy as np
from sklearn import tree
import matplotlib.pyplot as plt
plt.style.use('ggplot')
import folium
import re
#import matplotlib.image as pltimg
#from sklearn.tree import DecisionTreeClassifier
#from sklearn.model_selection import train_test_split, GridSearchCV 
from sklearn import metrics
import seaborn as sns
from sklearn.cluster import DBSCAN, KMeans
sns.set(rc = {'figure.figsize':(15,15)})

#-------------------------------
# reading and plotting the data
data = pd.read_csv('data/created/Location_mapper.csv')

m = folium.Map(location=[data.Latitude.mean(),
                         data.Longitude.mean()], 
               zoom_start=9, tiles='OpenStreet Map')
for _, row in data.iterrows():
    folium.CircleMarker(
        location=[row.Latitude, row.Longitude],
        radius=5,
        #popup=re.sub(r'[^a-zA-Z ]+', '', row.NAME),
        color='#1787FE',
        fill=True,
        fill_colour='#1787FE'
    ).add_to(m)

m.save("mymap.html")
#---------------------------------------------
# 1. KMEANS clustering
K_clusters = range(1,10)
kmeans = [KMeans(n_clusters=i) for i in K_clusters]
Y_axis = data[['Latitude']]
X_axis = data[['Longitude']]
score = [kmeans[i].fit(Y_axis).score(Y_axis) for i in range(len(kmeans))] # Visualize
plt.plot(K_clusters, score)
plt.xlabel('Number of Clusters')
plt.ylabel('Score')
plt.title('Elbow Curve')
plt.show()

 

kmeans = KMeans(n_clusters = 8, init ='k-means++')
kmeans.fit(data.loc[:,['Latitude','Longitude']]) # Compute k-means clustering.X['cluster_label'] = kmeans.fit_predict(X[X.columns[1:3]])centers = kmeans.cluster_centers_ # Coordinates of cluster centers.labels = kmeans.predict(X[X.columns[1:3]]) # Labels of each pointX.head(10)


data['cluster_label'] = kmeans.fit_predict(data.loc[:,['Latitude','Longitude']])
centers = kmeans.cluster_centers_ # Coordinates of cluster centers.
labels = kmeans.predict(data.loc[:,['Latitude','Longitude']]) # Labels of each pointX.head(10)

data.head(5)
centers = kmeans.cluster_centers_
print(centers)


X=data.loc[:,['ID','Latitude','Longitude']]
data.plot.scatter(x = 'Latitude', y = 'Longitude',
               c=labels, s=50, cmap='viridis')
plt.scatter(centers[:, 0], centers[:, 1], c='black', s=200, alpha=0.5)
#----------------------------------------------------------
# 2. DBSCAN
data['Cluster_DBSCAN']= DBSCAN().fit_predict(data.loc[:,['Latitude','Longitude']])
