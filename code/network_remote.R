
######################################################
### Network data simulation
### Similacion de calculo de indice de complejidad
######################################################


library(igraph) # loads the igraph package
instituciones<-c("Depto_Amazonas", "PNN_Amacayacu","Resguardo1","Resguardo2", "Mun_TIMBIQUÍ","CorpoAmazonia", "ONG1")
Encoding(instituciones) <- "latin1" # acepta tildes
# genera datos simulados de interacciones entre 5 instituciones
dat<-as.data.frame(replicate(length(instituciones), rbinom(length(instituciones),size=1,prob=.19))) 
colnames(dat)<-instituciones
rownames(dat)<-instituciones
#### ver la matriz sumulada
m=as.matrix(dat) # coerces the data set as a matrix
g=graph.adjacency(m,mode="undirected",weighted=NULL) # this will create an 'igraph object'
V(g)$size=degree(g)*3.5 # cambia el tamaño del nodo según el grado y lo multiplica por 1.2 o lo que se quiera
#plot(g)
#Atributos a los nodos
#V(g)$name # Muestra los nombres de las columnas
#nombres del tipo de atributo , Colores
a<-as.data.frame(c("A","B","C","C","D","E","F"))
colnames(a)<-"Ambito"
a$row.names<-instituciones
V(g)$Ambito=as.character(a$Ambito[match(V(g)$name,a$row.names)]) # This code says to create a vertex attribute called "Sector" by extracting the value of the column "Sector" in the attributes file when the row.name matches the vertex name.
#V(g)$Ambito # This will print the new vertex attribute, "Ambito"
#Colocar color a los atributos
V(g)$color=V(g)$Ambito #assign the "Ambito" attribute as the vertex color
V(g)$color=gsub("A","red",V(g)$color)#Depto
V(g)$color=gsub("B","blue",V(g)$color)#PNN
V(g)$color=gsub("C","green",V(g)$color)#Resguardo
V(g)$color=gsub("D","yellow",V(g)$color)#Municip
V(g)$color=gsub("E","gray",V(g)$color)#Corporacion
V(g)$color=gsub("F","black",V(g)$color)#ONG
#### ver la red
plot(g)

# Atributos
f1<-degree(g)#calcula el grado de cada nodo
f2<-betweenness(g)
f3<-closeness(g)
mat<-cbind(f1,f2,f3)
colnames(mat)<-c("degree", "betweenness", "closeness")
mat<-as.data.frame(mat)
# write.csv(mat,"mat.csv")
# path.length.hist(g) 


library(vegan) # to calculate Shanon
Shanon<-diversity(mat[1], index = "shannon", MARGIN = 2, base = 2) # Shanon al degree
E<- sum(mat[1])/nrow(mat[1])
S<- nrow(mat[1])-sum(mat[1])
Complex<-E*S
#### ver indice de complejidad
Complex
Shanon


