#test import


#test query
off <- resource(cnx, "species")

query(off)
query(off, conditions = buildCondition("genus", "==", "Bombus ")) #fix example query species for genus.
##FUCK genus has an space at the end FIX IN DATABASE!!!!
query(off, limit=2, skip=0)
query(off, limit=2, skip=2)


off <- resource(cnx, "Datasets") #qual es el nombre de la tabla??

query(off)
#query(off, conditions = buildCondition("genus", "==", "Bombus")) #fix example query species for genus.
query(off, limit=2, skip=0)
query(off, limit=2, skip=2)
