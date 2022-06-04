# -*- coding: utf-8 -*-
"""
Created on Fri Jun  3 15:31:22 2022

@author: Khaled
"""
import pandas as pd
import numpy as np
from sklearn import tree
import pydotplus
import matplotlib.pyplot as plt
import matplotlib.image as pltimg
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split, GridSearchCV 
from sklearn import metrics
import seaborn as sns
sns.set(rc = {'figure.figsize':(15,15)})

#-------------------------------
# reading and splitting the data
accidents = pd.read_csv('data/created/timeseries/ts_accidents_by_date.csv')
features = accidents.columns[2:109]
features = [x for x in features if not x.lower().startswith('nb_')]
X = accidents[features]
Y = accidents.NB_Accidents.astype(float)
x_train, x_test, y_train, y_test = train_test_split(X, Y, test_size=.3,random_state=2)
#----------------
# MODEL
dtree = DecisionTreeClassifier(criterion="entropy", ccp_alpha=0.0009381704296958534,
                               max_depth=9,min_samples_split=5,
                               splitter='best', random_state=2)
dtree = dtree.fit(x_train,y_train)
#-------------------------
# test and pred
y_pred = dtree.predict(x_test)
print("Accuracy:", metrics.accuracy_score(y_test, y_pred))
#      '\n F score: ', metrics.f1_score(y_test, y_pred))
#----------------------------------
# tree visualization
data = tree.export_graphviz(dtree, filled=True, rounded=True,
                            out_file=None, feature_names=features)
graph = pydotplus.graph_from_dot_data(data)
graph.write_png('Visualizations/test_tree.png')
#-----------------------------------------
# feature importance
imp_feat = pd.DataFrame({'Feature': features,
                         'importance': dtree.feature_importances_})
imp_feat = imp_feat[imp_feat.importance > 0]
imp_feat = imp_feat.sort_values('importance')
#importances = dtree.feature_importances_
#imp_feat = features[importances !=0]
#importances = importances[importances !=0]
#imp_feat = [x for _,x in sorted(zip(importances,imp_feat))]
#importances = sorted(importances)
sns.barplot(y='Feature', x='importance',data=imp_feat, orient='h')
plt.title('Features with importances > 0 \nfor predicting the number of accidents per day\n using decision trees')
plt.xlabel('Importance fraction (sums to 1)')
plt.ylabel('Features')
plt.show()
#-------------------------------------
# cross validation
parameters = {
    'max_depth':range(1,10),
    'criterion': ['gini','entropy'],
    'splitter': ['best'],
    'min_samples_split': range(2,6),
    'random_state':[2],
    'ccp_alpha': [0.0009381704296958534, .001, .1, .02, .01]
    }
clf_cv = GridSearchCV(DecisionTreeClassifier(), parameters, n_jobs=4,
                   scoring='accuracy')
clf_cv.fit(X, Y)
#---------------------------------------------------
# Tree pruning
dtree = DecisionTreeClassifier( random_state=2)
path = dtree.cost_complexity_pruning_path(x_train, y_train)
ccp_alphas, impurities = path.ccp_alphas, path.impurities
fig, ax = plt.subplots()
ax.plot(ccp_alphas[:-1], impurities[:-1], marker="o", drawstyle="steps-post")
ax.set_xlabel("effective alpha")
ax.set_ylabel("total impurity of leaves")
ax.set_title("Total Impurity vs effective alpha for training set")

clfs = []
for ccp_alpha in ccp_alphas:
    clf = DecisionTreeClassifier( random_state=2,
                                   ccp_alpha=ccp_alpha)
    clf.fit(x_train, y_train)
    clfs.append(clf)
print(
    "Number of nodes in the last tree is: {} with ccp_alpha: {}".format(
        clfs[-1].tree_.node_count, ccp_alphas[-1]
    )
)

clfs = clfs[:-1]
ccp_alphas = ccp_alphas[:-1]

node_counts = [clf.tree_.node_count for clf in clfs]
depth = [clf.tree_.max_depth for clf in clfs]
fig, ax = plt.subplots(2, 1)
ax[0].plot(ccp_alphas, node_counts, marker="o", drawstyle="steps-post")
ax[0].set_xlabel("alpha")
ax[0].set_ylabel("number of nodes")
ax[0].set_title("Number of nodes vs alpha")
ax[1].plot(ccp_alphas, depth, marker="o", drawstyle="steps-post")
ax[1].set_xlabel("alpha")
ax[1].set_ylabel("depth of tree")
ax[1].set_title("Depth vs alpha")
fig.tight_layout()

train_scores = [clf.score(x_train, y_train) for clf in clfs]
test_scores = [clf.score(x_test, y_test) for clf in clfs]

fig, ax = plt.subplots()
ax.set_xlabel("alpha")
ax.set_ylabel("accuracy")
ax.set_title("Accuracy vs alpha for training and testing sets")
ax.plot(ccp_alphas, train_scores, marker="o", label="train", drawstyle="steps-post")
ax.plot(ccp_alphas, test_scores, marker="o", label="test", drawstyle="steps-post")
ax.legend()
plt.show()


alpha_scores = pd.DataFrame({'alpha':ccp_alphas,
                             'test': test_scores,
                             'train': train_scores})