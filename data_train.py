# coding:utf-8    ctrl+/  注释
import sys
import pymongo
import numpy as np
import test_gaode_api

reload(sys)
sys.setdefaultencoding("utf-8")

connection = pymongo.MongoClient()
tdb = connection.program
post_info = tdb.testhouse

lists=[]
count=0

for item in post_info.find():
    count+=1

    single_item=[item["room_number"],item["parlour_number"],item["house_size"],item["year"],
                 item["building_height"],item["house_height_inlist"],item["house_location_longtitude"],
                 item["house_location_latitude"],item["average_price_inlist"]]
    lists.extend([single_item])
    #print count


from sklearn import metrics
from sklearn import preprocessing
from sklearn.linear_model import LinearRegression
from sklearn.cross_validation import train_test_split

_array = np.array(lists)
x = _array[:, 0:8]
#normalized_X=preprocessing.normalize(x)
y = _array[:, 8]
#normalized_Y=preprocessing.normalize(y)
X_train, X_test, y_train, y_test = train_test_split(x, y,test_size=0.2)

print "训练集大小%d"%(X_train.shape[0])
print "测试集大小%d"%(X_test.shape[0])

#线性回归
clf = LinearRegression()
clf.fit(X_train, y_train)
print "线性回归模型参数:"
print clf.coef_
y_pred = clf.predict(X_test)

print ("线性回归误差:%d元"%(np.sqrt(metrics.mean_squared_error(y_test, y_pred))))


#神经网络
from sklearn.neural_network import MLPRegressor
kmodel = MLPRegressor(learning_rate='adaptive',max_iter=2000).fit(X_train, y_train)
y_kmodel_pred = kmodel.predict(X_test)
print ("神经网络误差:%d元"%np.sqrt(metrics.mean_squared_error(y_test, y_kmodel_pred)))

#svm模型
from sklearn import svm
smodel=svm.LinearSVR()
smodel.fit(X_train, y_train)
y_smodel_pred=smodel.predict(X_test)

print "svm误差:%d元"%np.sqrt(metrics.mean_squared_error(y_test, y_smodel_pred))


#决策树模型
from sklearn.tree import DecisionTreeRegressor
dmodel=DecisionTreeRegressor()
dmodel.fit(X_train, y_train)

y_dmodel_pred=dmodel.predict(X_test)
print "非线性决策树误差:%d元"%np.sqrt(metrics.mean_squared_error(y_test, y_dmodel_pred))
print "非线性决策树误差评分:%f"%dmodel.score(X_test,y_test)

print "各参数权重:"
print dmodel.feature_importances_
# coding:utf-8    ctrl+/  注释


import wx

app=wx.App()
win =wx.Frame(None,title="房价查询",size=(420,335))
win.Show()

btn=wx.Button(win,label='查询',pos=(315,5),size=(80,25))


house_size_input=wx.TextCtrl(win,pos=(80,5),size=(210,25))
house_size_input_label=wx.StaticText(win,label='房屋大小',pos=(10,10),size=(60,30))

room_number_input=wx.TextCtrl(win,pos=(80,45),size=(210,25))
room_number_input_label=wx.StaticText(win,label='房间数',pos=(10,50),size=(60,30))

parlour_number_input=wx.TextCtrl(win,pos=(80,85),size=(210,25))
parlour_number_input_label=wx.StaticText(win,label='厅数',pos=(10,90),size=(60,30))

house_height_input=wx.TextCtrl(win,pos=(80,125),size=(210,25))
house_height_input_label=wx.StaticText(win,label='层高',pos=(10,130),size=(60,30))

building_height_input=wx.TextCtrl(win,pos=(80,165),size=(210,25))
building_height_input_label=wx.StaticText(win,label='楼高',pos=(10,170),size=(60,30))

year_input=wx.TextCtrl(win,pos=(80,205),size=(210,25))
year_input_label=wx.StaticText(win,label='建造年份',pos=(10,210),size=(60,30))

address_input=wx.TextCtrl(win,pos=(80,245),size=(210,25))
address_input_label=wx.StaticText(win,label='地址',pos=(10,250),size=(60,30))


def search(event):
    hz = house_size_input.Value
    rn=room_number_input.Value
    pn=parlour_number_input.Value
    hh=house_height_input.Value
    bh=building_height_input.Value
    yi=year_input.Value
    ai=address_input.Value
    location=test_gaode_api.geocode(ai)["geocodes"][0]['location'].encode('utf-8')
    house_location_1=location.index(',')
    house_location_longtitude=float(location[0:house_location_1])
    house_location_latitude=float(location[house_location_1+1:])
    list=[]
    list.extend([[hz,rn,pn,hh,bh,yi,house_location_longtitude,house_location_latitude]])
    out_price=dmodel.predict(list)
    print out_price,float(out_price[0])*float(hz)
    #print hz,rn,pn,hh,bh,yi,house_location_longtitude,house_location_latitude

btn.Bind(wx.EVT_BUTTON,search)

app.MainLoop()
