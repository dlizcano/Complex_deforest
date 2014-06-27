


###############################
########## GIS PART  ##########
###############################

library(maptools)
library(maps)
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables

# Download shapefiles... slow!!!!! be patient
shpurl1 <- "https://github.com/dlizcano/Complex_deforest/blob/master/data/Deptos.zip"
shpurl2 <- "https://github.com/dlizcano/Complex_deforest/blob/master/data/Municip2014.zip"
shpurl3 <- "https://github.com/dlizcano/Complex_deforest/blob/master/data/PNN.zip"
shpurl4 <- "https://github.com/dlizcano/Complex_deforest/blob/master/data/ResgIndig.zip"
shpurl5 <- "https://github.com/dlizcano/Complex_deforest/blob/master/data/Squares100km.zip"
tmp    <- tempfile(fileext=".zip")
download.file(shpurl1, destfile=tmp)
download.file(shpurl2, destfile=tmp)
download.file(shpurl3, destfile=tmp)
download.file(shpurl4, destfile=tmp)
download.file(shpurl5, destfile=tmp)
files <- unzip(tmp, exdir=getwd())


#get polygons of deptos Colombia
col_dep <- readShapePoly(fn="Limite_Departamental.shp")
Encoding(levels(col_dep$NOMBRE_DPT)) <- "latin1"
names(col_dep)
length(col_dep$NOMBRE_DPT) # number of deptos
(labs_dep<-col_dep$NOMBRE_DPT) # see labs for deptos

#get polygons of deptos Colombia
col_mun <- readShapePoly(fn="Municipios.shp")
Encoding(levels(col_mun$NOM_MUNICI)) <- "latin1"
Encoding(levels(col_mun$NOM_DEPART)) <- "latin1"
names(col_mun)
length(col_mun$NOM_MUNICI) # number of municip
(labs_muni<-col_mun$NOM_MUNICI) # see labs for municip

#get polygons of PNN. File Edited, some parks in Andes missing
col_PNN <- readShapePoly(fn="Parques Nacionales Naturales segun Categoria (2012).shp",delete_null_obj=TRUE)
Encoding(levels(col_PNN$NOM_PARQUE)) <- "latin1"
names(col_PNN)
length(col_PNN$NOM_PARQUE) # number of parques
(labs_PNN<-col_PNN$NOM_PARQUE) # see labs for PNN

#get polygons of Resguardo Indigena Just Amazon. Andean were deleted
col_res <- readShapePoly(fn="Resguardos Indigenas (2012).shp",delete_null_obj=TRUE)
Encoding(levels(col_res$RINOMBRE)) <- "latin1"
names(col_res)
length(col_res$RINOMBRE) # number of resguardos
(labs_res<-col_res$RINOMBRE) # see labs for resguardos

#get polygons for squares
amaz_cuad <- readShapePoly(fn="Squares_100km.shp",delete_null_obj=TRUE)
names(amaz_cuad)
length(amaz_cuad$Unique_ID) # number of squares
(labs_cua<-amaz_cuad$Unique_ID) # see labs for resguardos


library(rgdal)
summary(col_dep) # notice no projection, but see Limite Departamental.prj
# the IGAC projection see http://spatialreference.org/ref/epsg/?search=colombia&srtext=Search
GAUSS_BTA_MAGNA<-CRS("+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
proj4string(col_dep)<- GAUSS_BTA_MAGNA
proj4string(col_mun)<- GAUSS_BTA_MAGNA
proj4string(col_PNN)<- GAUSS_BTA_MAGNA
proj4string(col_res)<- GAUSS_BTA_MAGNA
proj4string(amaz_cuad)<- GAUSS_BTA_MAGNA

### change to lat long wgs84
### take a look to http://spatialreference.org
latlon<- CRS("+proj=longlat +datum=WGS84")
col_dep_geo<-spTransform(col_dep, latlon)
col_mun_geo<-spTransform(col_mun, latlon)
col_PNN_geo<-spTransform(col_PNN, latlon)
col_res_geo<-spTransform(col_res, latlon)
col_cua_geo<-spTransform(amaz_cuad, latlon)


####### select amazon for municipios
col_mun_geo$amazonic<-0
for (i in 1:length(col_mun_geo$NOM_DEPART)){
	if (col_mun_geo$NOM_DEPART[i]==c("AMAZONAS")){col_mun_geo$amazonic[i]<-1}
	if (col_mun_geo$NOM_DEPART[i]==c("PUTUMAYO")){col_mun_geo$amazonic[i]<-1}
	if (col_mun_geo$NOM_DEPART[i]==c("CAQUETÁ")){col_mun_geo$amazonic[i]<-1}
	if (col_mun_geo$NOM_DEPART[i]==c("GUAINIA")){col_mun_geo$amazonic[i]<-1}
	if (col_mun_geo$NOM_DEPART[i]==c("GUAVIARE")){col_mun_geo$amazonic[i]<-1}
	if (col_mun_geo$NOM_DEPART[i]==c("VAUPES")){col_mun_geo$amazonic[i]<-1}
	if (col_mun_geo$NOM_DEPART[i]==c("META")){col_mun_geo$amazonic[i]<-1}
	if (col_mun_geo$NOM_DEPART[i]==c("VICHADA")){col_mun_geo$amazonic[i]<-1}
}
amaz_mun<-col_mun_geo[col_mun_geo$amazonic == 1,]


####### select amazon for deptos
col_dep_geo$amazonic<-0
for (i in 1:length(col_dep_geo$NOMBRE_DPT)){
	if (col_dep_geo$NOMBRE_DPT[i]==c("AMAZONAS")){col_dep_geo$amazonic[i]<-1}
	if (col_dep_geo$NOMBRE_DPT[i]==c("PUTUMAYO")){col_dep_geo$amazonic[i]<-1}
	if (col_dep_geo$NOMBRE_DPT[i]==c("CAQUETÁ")){col_dep_geo$amazonic[i]<-1}
	if (col_dep_geo$NOMBRE_DPT[i]==c("GUAINIA")){col_dep_geo$amazonic[i]<-1}
	if (col_dep_geo$NOMBRE_DPT[i]==c("GUAVIARE")){col_dep_geo$amazonic[i]<-1}
	if (col_dep_geo$NOMBRE_DPT[i]==c("VAUPES")){col_dep_geo$amazonic[i]<-1}
	if (col_dep_geo$NOMBRE_DPT[i]==c("META")){col_dep_geo$amazonic[i]<-1}
	if (col_dep_geo$NOMBRE_DPT[i]==c("VICHADA")){col_dep_geo$amazonic[i]<-1}
}
amaz_dept<-col_dep_geo[col_dep_geo$amazonic == 1,]

####### select amazon for PNN
amaz_PNN<-col_PNN_geo[col_PNN_geo$REGIONAL == "AMAZONIA ORINOQUIA",]

#####################
## Corporaciones ###
####################
# Corpoamazonia<-amaz_dept[amaz_dept$Corporacio == "Corpoamazonia",]
# CDA<-amaz_dept[amaz_dept$Corporacio == "CDA",]
# Cormacarena<-amaz_dept[amaz_dept$Corporacio == "Cormacarena",]
# Corporinoquia<-amaz_dept[amaz_dept$Corporacio == "Corporinoquia",]

#######################
## rasterize
#######################

library(raster)  
r <- raster(ncol=100, nrow=100) #resolucion pixeles
extent(r) <- extent(col_cua_geo)

rcorpo <- as.factor(rasterize(amaz_dept, r, 'Corporacio'))
rcorpo<- ratify(rcorpo) #make factor map
corpo <- levels(rcorpo)[[1]] #etract number of factors to table
corpo$corporacion <- levels(amaz_dept$Corporacio) #put factor names
corpo$code <- c(1:4) #put code
levels(rcorpo) <- corpo #embeed table
#plot(rcorpo)

rdepto <- rasterize(amaz_dept, r, 'NOMBRE_DPT')
rdepto<- ratify(rdepto) #make factor map
dep <- levels(rdepto)[[1]] #etract number of factors to table
dep$depto <- levels(amaz_dept$NOMBRE_DPT) #put factor names
dep$code <- c(1:length(levels(amaz_dept$NOMBRE_DPT))) #put code
levels(rdepto) <- dep #embeed table
#plot(rdepto)

rmunicip <- rasterize(amaz_mun, r, 'NOM_MUNICI')
rmunicip<- ratify(rmunicip) #make factor map
mun <- levels(rmunicip)[[1]] #etract number of factors to table
lablon<-levels(amaz_mun$NOM_MUNICI) # fix discrepancy
lablon<-lablon[c(mun[,1])]# fix discrepancy 
mun$municip <- lablon #put factor names
mun$code <- c(1:length(lablon)) #put code
levels(rmunicip) <- mun #embeed table
#plot(rmunicip)

rPNN <- rasterize(amaz_PNN, r, 'NOM_PARQUE')
rPNN<- ratify(rPNN) #make factor map
pnn <- levels(rPNN)[[1]] #etract number of factors to table
lablon<-levels(amaz_PNN$NOM_PARQUE) # fix discrepancy
lablon<-lablon[c(pnn[,1])]# fix discrepancy 
pnn$pnn <- lablon #put factor names
pnn$code <- c(1:length(lablon)) #put code
levels(rPNN) <- pnn #embeed table
#plot(rPNN)

rcol_res_geo<-rasterize(col_res_geo, r, 'RINOMBRE')
rcol_res_geo<- ratify(rcol_res_geo) #make factor map
resg <- levels(rcol_res_geo)[[1]] #etract number of factors to table
lablon<-levels(col_res_geo$RINOMBRE) # fix discrepancy
lablon<-lablon[c(resg[,1])]# fix discrepancy 
resg$resgu <- lablon #put factor names
resg$code <- c(1:length(lablon)) #put code
levels(rcol_res_geo) <- resg #embeed table
#plot(rcol_res_geo)

#stack layers
rlayers<-stack(rcorpo,rdepto,rmunicip,rPNN,rcol_res_geo)
names(rlayers)<- c('corpo', 'depto', "municip", "PNN", "resg") 
plot(rlayers) #plot layers


####################################
####  		TO DO			########
#### function to get info per square
#### loop over squares
#####################################

## Now lets “cookie-cut” the grid for cuadricula 1
cr <- crop(rlayers, extent(col_cua_geo[1,])) 
corpo[which(corpo$ID==unique(cr[[1]])),2] # corporacion etract
dep[which(dep$ID==unique(cr[[1]])),2] # depto etract
#### insert if lenght = 1 or more..... 
unique(cr[[3]])# municipio etract
unique(cr[[5]])# resguardo etract



#################################
### Network Part
##############################





