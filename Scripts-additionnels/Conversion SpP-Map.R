require(ggplot2)
require(reshape)
require(plyr)

ECNmetro <- spTransform(ECNmetro, CRS("+init=epsg:4326"))
reunion <- spTransform(reunion, CRS("+init=epsg:4326"))
#mayotte <- spTransform(mayotte, CRS("+init=epsg:4326"))
#martinique <- spTransform(martinique, CRS("+init=epsg:4326"))
#guyane <- spTransform(guyane, CRS("+init=epsg:4326"))
guadeloupe <- spTransform(guadeloupe, CRS("+init=epsg:4326"))
xx <- ECNmetro

xx@data$id <- rownames(xx@data)

# Convert to dataframe
xx.df <- as.data.frame(xx)
# xx.df$myid = xx$ID1*100 + xx$piece

#Fortfy automagic

xx.fort <- fortify(xx, region="id")

# Join operation - one row per coordinate vector

xx <- join(xx.fort, xx.df,by="id")
unique(xx$piece)

# generate unique id for each polygon piece
xx$myid <- paste0(xx$id, xx$piece)

# Split by myid because we need to add NA at end of each set of polygon coordinates to 'break' the line
xxSp <- split(xx, xx$myid)

# Need to insert NA at end of each polygon shape to cut off that shape
xxL <- do.call(rbind, (lapply(xxSp, function(x)
  { 
    j <- x[nrow(x) , ]
    j[1:2] <- c(NA,NA)
    rbind( x , j )
  }
)))


# Create list object with same structure as map object
xxMap <- list( x = xxL$long , y = xxL$lat , range = c(range(xxL$long[!(is.na(xxL$long))]), range(xxL$lat[!(is.na(xxL$lat))])), names = as.character(unique(xxL$group)))

# Define as a map class object
attr(xxMap , "class") <- "map"

map(xxMap , fill=T, col=2)

#leaMetro <- xxMap
leaReunion <- xxMap
#leaMayotte <- xxMap
#leaMartinique <- xxMap
#leaGuyane <- xxMap
leaReunion <- xxMap
leaReunion$regions <- c()
for(i in 1:length(leaReunion$names)){
  leaReunion$regions[i] <- unlist(strsplit(leaReunion$names[i],'[.]'))[1]
}

leaMayotte$names <- c("May.1","May.2","May.3")
leaMayotte$regions <- c("OceI","OceI","OceI")
leaReunion$names <- c("Reu.1")
leaReunion$regions <- c("OceI")
leaGuyane$names <- c("Guy.1","Guy.2")
leaGuyane$regions <- c("AntG","AntG")
leaMartinique$names <- c("Mar.1")
leaMartinique$regions <- c("AntG")
leaGuadeloupe$names <- c("Gua.1","Gua.2","Gua.3","Gua.4","Gua.5","Gua.6","Gua.7","Gua.8")
leaGuadeloupe$regions <- c("AntG","AntG","AntG","AntG","AntG","AntG","AntG","AntG")


# Important pour l'ensemble du code ci-dessus ECNmetro doit avoir un slot @data
Offre_vec_ville <- c(015,020,018,016,017,019,022,024,023,021,038,025,026,027,029,032,028,030,033,031,035,034,036,037,042,040,039,041,000)
Offre_vec_spe <- c(11,4,3,5,9,6,2,10,7,12,13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 32, 29, 30, 31, 0)
xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
indic_ville <- c(034,021,028,041,016,037,029,022,038,017,025,015,023,039,026,035,018,030,036,042,031,019,032,024,027,020,040,033)
xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
#xlab_spe.2 <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
xlab_spe.2 <- c("AR","Bi","GyM","GyO","MT","MG","Ped","Psy","SP","ChG","ChOr","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Nep","Neu","Onc","Pne","Rad","Rhu")
indic_spe <- c(004,003,005,009,006,011,010,002,007,032,028,029,030,031,012,013,014,015,016,017,018,019,020,021,023,022,024,025,026,027)

vec.spe <- c()
vec.sub <- c()

con <- file("./data/ECN2013.csv")
ECN_data <- read.csv(con, sep=",")
for(i in 1:length(ECN_data[,1])){
  vec.sub[i] <- indic_ville[which(xlab_ville==ECN_data[i,9])]
  vec.spe[i] <- indic_spe[which(xlab_spe==ECN_data[i,10])]
}
vec.sub <- as.character(vec.sub)
vec.spe <- as.character(vec.spe)
for(i in 1:length(vec.sub)){
  if(nchar(vec.sub[i])==2){
    vec.sub[i] <- paste0("0",vec.sub[i])
  }
  if(nchar(vec.sub[i])==1){
    vec.sub[i] <- paste0("00",vec.sub[i])
  }
}
for(i in 1:length(vec.spe)){
  if(nchar(vec.spe[i])==2){
    vec.spe[i] <- paste0("0",vec.spe[i])
  }
  if(nchar(vec.spe[i])==1){
    vec.spe[i] <- paste0("00",vec.spe[i])
  }
}        
ECN_data <- data.frame("sexe"=ECN_data[,4],"ddm"=ECN_data[,6],"dda"=ECN_data[,7],"Etudiant"=ECN_data[,8],"v5"=1,"v6"=1,"Subdivision"=ECN_data[,9],"Discipline"=ECN_data[,10],"v9"=1,"v10"=1,"SubDis"=paste0("[",vec.sub,vec.spe,"]"))

colnames(ECN_data)[9] <-"Désir (non officiel)"

ECN_data.dum <- ECN_data

ECN_data.dum[,7] <- as.character(ECN_data.dum[,7])
ECN_data.dum[,7] <- as.factor(ECN_data.dum[,7])
ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
df_corr_ville <- data.frame(levels(ECN_data.dum[,7]),xlab_ville)
ECN_data.dum[,7] <- mapvalues(ECN_data.dum[,7],from=levels(ECN_data.dum[,7]),to=xlab_ville)
df_order <- ddply(ECN_data.dum, .(Subdivision), summarize, 
                  median=median(Etudiant,na.rm=T),
                  mean=mean(Etudiant,na.rm=T),
                  max=max(Etudiant,na.rm=T),
                  min=min(Etudiant,na.rm=T),
                  TQuart=quantile(Etudiant, 0.75,na.rm=T),
                  PQuart=quantile(Etudiant, 0.25,na.rm=T)
)

#adaptation format carte
#ECNmetro <- unionSpatialPolygons(canton, canton$ECN[- which(canton$ECN=='O')])
#ECNmetro <- spTransform(ECNmetro, CRS("+init=epsg:4326"))

#création du df données carte
vecid <- c()
for(i in 1:length(ECNmetro@polygons)){
  vecid[i] <- ECNmetro@polygons[[i]]@ID  
}

dfmap <- data.frame()
for(j in 2:7){
  for(i in 1:length(vecid)){
    dfmap[i,j-1] <- df_order[which(df_order$Subdivision==vecid[i]),j]
  }
}

row.names(dfmap) <- vecid
colnames(dfmap) <- c("Mediane","Moyenne","Maximum","Minimum","PQuart","TQuart")

#fusion données-carte
ECNmetro <- SpatialPolygonsDataFrame(ECNmetro, data=dfmap)
ECNmetro$region <- vecid
