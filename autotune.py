from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsClassifier
from sklearn import metrics

import pandas as pd

import numpy as np

df = pd.read_csv("/Users/patrickkampmeyer/Dropbox/Hosani_Batrick/Wax/Wax_Sim/Fable.txt")


from matplotlib import pyplot


inputdata = []
with open("/Users/patrickkampmeyer/Dropbox/Hosani_Batrick/Wax/Wax_Sim/Setup.txt", "r") as reader:
    
    for line in reader.readlines():
        inputdata.append(line)
        
inputdata


outputdata = []
with open("/Users/patrickkampmeyer/Dropbox/Hosani_Batrick/Wax/Wax_Sim/Fable.txt", "r") as reader:
    
    for line in reader.readlines():
        outputdata.append(line)
        
variableNames = outputdata[4]

varnamesvec= variableNames.split(",")

varnamesvec= np.array(varnamesvec)

type(varnamesvec[1])

values = outputdata[6:100]


#df = pd.DataFrame(values, columns = varnamesvec)
#values = values.split(",")

values = [s.replace("\n", "") for s in values]

valuesarray = []

#values = [float(s) for s in values]

for i in range(1,90):

    newvals = np.array(values[i].split(",")).astype(float)


    valuesarray.append(newvals)

df = pd.DataFrame(valuesarray,columns = varnamesvec)


df.iloc[:,9]


                              
thicksum = sum(df.iloc[(((1-1)*6)+1):((1*6)+1),9])
                             
                             
                        
df.iloc[5,0]                     



#df['Name'] = ['Ankit', 'Ankita', 'Yashvardhan']
#df['Articles'] = [97, 600, 200]
#df['Improved'] = [2200, 75, 100]




n_sec = 6

n_time = 14


i = 5


newdf = pd.DataFrame(index = range(0,n_time), columns=varnamesvec)



for i in range(1,n_time+1):


    thicksum = sum(df.iloc[(((i-1)*n_sec)+1):((i*n_sec)+1),9])

    thickavg = float(thicksum/n_sec)

    newdf.iloc[i-1,9] = thickavg
    
    newdf.iloc[i-1,0] = df.iloc[(n_sec*i),0]
    newdf.iloc[i-1,7] = df.iloc[(1*i),7]
    newdf.iloc[i-1,8] = df.iloc[(n_sec*i),7]
    
    newdf.iloc[i-1,2] = df.iloc[(1*i),6]
    newdf.iloc[i-1,3] = df.iloc[(n_sec*i),6]
    
    
  
df2 = newdf.drop(df.columns[[1,4,5,6,11,12,13,14,15,16,17,18,19,20,21,22]], axis = 1)

maxtime = df2.iloc[:,0].max()

df2['time_bins'] = pd.cut(x = df2.iloc[:,0], bins=np.linspace(0, maxtime, num = 20))

df2

arrays = [outputdata[i].split(",") for i in range(10)]
np.stack(arrays, axis=0).shape

import numpy
data = numpy.random.random(100)
bins = numpy.linspace(0, 1, 10)
digitized = numpy.digitize(data, bins)
bin_means = [data[digitized == i].mean() for i in range(1, len(bins))]


iris = load_iris()

x = iris.data
y = iris.target


# use train/test split with different random state values

x_train, x_test, y_train, y_test = train_test_split(x,y,random_state=2)


# check classification accuracy of KNN with k=5

knn = KNeighborsClassifier(n_neighbors = 5)
knn.fit(x_train, y_train)
y_pred = knn.predict(x_test)

print (metrics.accuracy_score(y_test, y_pred))

# split the dataset into folds here 

from sklearn.model_selection import KFold


kf = KFold(5, shuffle = False)


kf




