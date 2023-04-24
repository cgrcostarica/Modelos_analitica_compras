import pymongo

myclient = pymongo.MongoClient("mongodb+srv://Mario:mqdBbDobQoUO9QiQ@mseg11.hvbahht.mongodb.net/test")

mydb = myclient["SDA"]
mycol = mydb["USERS"]

for x in  mycol.find({},{ "Grado_Academico": "Doctor" }):
  fila = x
  print(x)
  
