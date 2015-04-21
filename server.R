library("shiny")
library("RCurl")
library("XML")
library("plyr")
library("maps")
library("RColorBrewer")
#library("reshape2")
library("leaflet")

### Etat d'avancement :
# Couleur : données agrégées OK, PPP OK, Sexe OK, Attractivité OK, Age OK
# Popup : agrégation des 3 PPP + age
# Données agrégées en fonction de spé : OK
# Rang limites : pb NA + calul des rangs limites mal fait

#vecteurs d'appariemment caractères <-> numériques spé/subdivisions
Offre_vec_ville <- c(015,020,018,016,017,019,022,024,023,021,038,025,026,027,029,032,028,030,033,031,035,034,036,037,042,040,039,041,000)
Offre_vec_spe <- c(11,4,3,5,9,6,2,10,7,12,13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 32, 29, 30, 31, 0)
xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
indic_ville <- c(034,021,028,041,016,037,029,022,038,017,025,015,023,039,026,035,018,030,036,042,031,019,032,024,027,020,040,033)
xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
xlab_spe.2 <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
#xlab_spe.2 <- c("AR","Bi","GyM","GyO","MT","MG","Ped","Psy","SP","ChG","ChOr","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Nep","Neu","Onc","Pne","Rad","Rhu")
indic_spe <- c(004,003,005,009,006,011,010,002,007,032,028,029,030,031,012,013,014,015,016,017,018,019,020,021,023,022,024,025,026,027)
nomRegionMetro <- c("Aix-Marseilles","Amiens","Angers","Besançon","Bordeaux","Brest","Caen","Clermont-Ferrand","Dijon","Grenoble","l'IDF","Lille","Limoges","Lyon","Montpellier","Nancy","Nantes","Nice","Poitiers","Reims","Rennes","Rouen","St-Etienne","Strasbourg","Toulouse","Tours")
#xlab_spe[order(factor(indic_spe, levels=Offre_vec_spe[1:30]))]
#load("./data/cartef.RData")
load("./data/leaflet.RData")

colors <- brewer.pal(9, "YlOrRd")
pal <- colorRampPalette(colors)
col <- pal(28)

#à implémenter dans l'attribution des couleurs
CouCon <- function (data, couleur, n, reverse=FALSE){
  colors <- brewer.pal(9, couleur)
  pal <- colorRampPalette(colors)
  col <- pal(n)
  if(reverse==TRUE){col <- col[length(col):1]}
  diff(range(data))/n
  df <- data.frame(value=range(data)[1]+diff(range(data))/n*1:n-diff(range(data))/n, col=as.character(col))
  if(diff(range(data))==0){
    colData <- lapply(data, function(x){
      return(df[n/2,2])
    })
  } else {
    colData <- lapply(data, function(x){
      return(df[which.min(abs(x - df[,1])),2])
    })
  }
  return(colData <- as.character(unlist(colData)))
}

### Construction dfCoordPop

#listCoordPop <- lapply(ECNmetro@polygons, function(y){
#  lapply(y@Polygons, function(x) {
#    c(mean(x@coords[,1]),mean(x@coords[,2]))
#  })
#})

#listCoordPop <- unlist(listCoordPop)

#dfCoordPop <- data.frame(lat=listCoordPop[seq(2,128,2)],lng=listCoordPop[seq(1,127,2)])

#df <- data.frame(lng=leaMetro$x, lat=leaMetro$y)
#df$y <- rep(1, length(leaMetro$x))
#for(i in 2:length(df$lng)){
#  if (is.na(df$lng[i])){df$y[i] <- df$y[i-1]+1} else {df$y[i] <- df$y[i-1]}
#}

#list <- split(df$lng,df$y)

#lng <- unlist(lapply(list, function(x) {
#  mean(x, na.rm=T)
#}))

#list <- split(df$lat,df$y)

#lat <- unlist(lapply(list, function(x) {
#  mean(x, na.rm=T)
#}))

#dfCoordPop <- data.frame(lng=lng[-65], lat=lat[-65])


shinyServer(function(input, output, session){
  
  vecdum <- c('median', 'median')
  dum1 <- reactive({return(input$meth.order1)})
  dum2 <- reactive({return(input$meth.order2)})

  methorder <- reactive({
    methOrder <- 'median'
    if(dum1()  != vecdum[1]){methOrder <- dum1()}
    if(dum2()  != vecdum[2]){methOrder <- dum2()}
    vecdum <<- c(dum1(), dum2())
    if(input$ChoixBDD %in% c("simulations2014","affectations2014") && methOrder %in% c("Age","Sexe")) {methOrder <- 'median'}
    return(methOrder)
  })
    
  Offre <- reactive({
    if(input$ChoixBDD == "affectations2010"){
      Offre_data <- read.csv("./data/Offre_2010_CESP.csv", fileEncoding = "latin1")
    }
    if(input$ChoixBDD == "affectations2011"){
      Offre_data <- read.csv("./data/Offre_2011_CESP.csv", fileEncoding = "UTF-8")
    }
    if(input$ChoixBDD == "affectations2012"){
      Offre_data <- read.csv("./data/Offre_2012_CESP.csv", fileEncoding = "UTF-8")
    }
    if(input$ChoixBDD == "affectations2013"){
      Offre_data <- read.csv("./data/Offre_2013_CESP.csv", fileEncoding = "UTF-8")
    }
    if(input$ChoixBDD %in% c("simulations2014","affectations2014")){
      Offre_data <- read.csv("./data/Offre_2014.csv",fileEncoding = "UTF-8")
    }
    row.names(Offre_data) <- Offre_data[,1]
    Offre_data <- Offre_data[,-1]
    names(Offre_data) <- xlab_spe[order(factor(indic_spe, levels=Offre_vec_spe[1:30]))]
    return(Offre_data)
  })
  
  dataset <- reactive({
    if(input$ChoixBDD == "simulations2014"){
      con <- file("./data/ECN2014_simulations.prn",encoding="UTF-8")
    }
    if(input$ChoixBDD == "affectations2014"){
      con <- file("./data/ECN2014_affectations.prn",encoding="UTF-8")
    }
    if(input$ChoixBDD == "affectations2010"){
      con <- file("./data/ECN2010.csv")
    }
    if(input$ChoixBDD == "affectations2011"){
      con <- file("./data/ECN2011.csv")
    }
    if(input$ChoixBDD == "affectations2012"){
      con <- file("./data/ECN2012.csv")
    }
    if(input$ChoixBDD == "affectations2013"){
      con <- file("./data/ECN2013.csv")
    }
    ECN_data <- read.csv(con, sep=",")
    
    vec.spe <- rep(NA, length(ECN_data[,1]))
    vec.sub <- rep(NA, length(ECN_data[,1]))
    
    if(input$ChoixBDD %in% c("affectations2014","simulations2014")){
      ECN_data <- ECN_data[,-1]
      ECN_data$Discipline <- as.numeric(substr(ECN_data$SubDis, 5, 7))
      ECN_data$Discipline <- mapvalues(ECN_data[,'Discipline'],from=indic_spe, to=xlab_spe)      
      ECN_data$Subdivision <- as.numeric(substr(ECN_data$SubDis, 3, 4))
      ECN_data$Subdivision <- mapvalues(ECN_data[,'Subdivision'],from=indic_ville, to=xlab_ville)
    } else {
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
    }
    colnames(ECN_data)[9] <-"Désir (non officiel)"
    return(ECN_data)
  })
  
  donnees_agreg <- reactive({
    ECN_data <- dataset()
    if(input$ChoixBDD %in% c("affectations2014","simulations2014")){
      ECN_data.dum <- ECN_data[-which(ECN_data[,11]==""), ]
    } else {
      ECN_data.dum <- ECN_data
    }
    if(input$Spe == 0){} else {
      spe <- xlab_spe[which(indic_spe == input$Spe)]
      ECN_data.dum <- ECN_data.dum[which(ECN_data.dum$Discipline ==  spe),]
    }
    ECN_data.dum[,7] <- as.character(ECN_data.dum[,7])
    ECN_data.dum[,7] <- as.factor(ECN_data.dum[,7])
    ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
    df_order <- ddply(ECN_data.dum, .(Subdivision), summarize, 
      median=median(Etudiant,na.rm=T),
      mean=mean(Etudiant,na.rm=T),
      max=max(Etudiant,na.rm=T),
      min=min(Etudiant,na.rm=T),
      TQuart=quantile(Etudiant, 0.75,na.rm=T),
      PQuart=quantile(Etudiant, 0.25,na.rm=T),
      l = length(Etudiant)
    )
    if(length(levels(ECN_data.dum[,7])) == 28){} else {
      vec <- xlab_ville[which(!(xlab_ville %in% levels(ECN_data.dum[,7])))]
      dfdum <- data.frame(Subdivision=vec, median=NA, mean=NA, max=NA,min=NA,TQuart=NA, PQuart=NA, l=0)
      df_order <- rbind(df_order, dfdum)
      df_order <- df_order[order(as.character(df_order$Subdivision)),]
      df_order$Subdivision <- factor(df_order$Subdivision, levels=df_order$Subdivision)
    }
    ### rangs limites à developper
    if(input$ChoixBDD == "affectations2010"){
      Offre_data_T <- read.csv("./data/Offre_2010_CESP.csv", fileEncoding = "latin1")
    }
    if(input$ChoixBDD == "affectations2011"){
      Offre_data_T <- read.csv("./data/Offre_2011.csv", fileEncoding = "UTF-8")
    }
    if(input$ChoixBDD == "affectations2012"){
      Offre_data_T <- read.csv("./data/Offre_2012.csv", fileEncoding = "UTF-8")
    }
    if(input$ChoixBDD == "affectations2013"){
      Offre_data_T <- read.csv("./data/Offre_2013.csv", fileEncoding = "UTF-8", sep=";")
    }
    if(input$ChoixBDD %in% c("simulations2014","affectations2014")){
      Offre_data_T <- read.csv("./data/Offre_2014.csv",fileEncoding = "UTF-8")
    }
    row.names(Offre_data_T) <- Offre_data_T[,1]
    Offre_data_T <- Offre_data_T[,-1]
    names(Offre_data_T) <- xlab_spe[order(factor(indic_spe, levels=Offre_vec_spe[1:30]))]
    if(input$Spe == 0){
      df_order$o <- Offre_data_T[order(as.character(row.names(Offre_data_T)[1:28])), 31]
      df_order$d <- df_order$o - df_order$l
      rgL <- rep(NA, 28)
      df_order$rgL <- unlist(lapply(split(df_order, df_order$Subdivision), function(x){
        if(x$d == 0 & x$o != 0){rgL <- x$max}
        if(x$d > 0){rgL <- max(ECN_data.dum$Etudiant)+1}
        if(x$d < 0 & !(is.na(x$d))){
          vec <- ECN_data.dum$Etudiant[which(ECN_data.dum$Subdivision == x$Subdivision)]
          rgL <- vec[length(vec)+x$d]
        }
        return(rgL)
      }))
    } else {
      df_order$o <- Offre_data_T[order(as.character(row.names(Offre_data_T)[1:28])), spe]
      df_order$o[which(is.na(df_order$o))] <- 0
      df_order$d <- df_order$o - df_order$l
      rgL <- rep(NA, 28)
      df_order$rgL <- unlist(lapply(split(df_order, df_order$Subdivision), function(x){
        if(x$d == 0 & x$o != 0){rgL <- x$max}
        if(x$d > 0 & x$o != 0){rgL <- max(ECN_data$Etudiant)+1}
        if(x$d < 0 & x$o != 0){
          vec <- ECN_data.dum$Etudiant[which(ECN_data$Discipline == spe)]
          rgL <- vec[length(vec)+x$d]
        }
        if(x$o == 0){rgL <- NA}
        return(rgL)
      }))
    }
    return(df_order)
  })
  
  donnees_sel <- reactive({
    ECN_data <- dataset()
    Offre_data <- Offre()
    if(input$Restcand=="oui"){
      min <- as.numeric(input$Rang.min)
      max <- as.numeric(input$Rang.max)
      ECN_data.dum <- ECN_data[min:max,]
    } else {
      min <- 1
      max <- length(ECN_data[,1])
      ECN_data.dum <- ECN_data
    }
    spe.vec <- as.numeric(substr(ECN_data[,11], start=5, stop=7))
    ville.vec <- as.numeric(substr(ECN_data[,11], start=2, stop=4))
    df_offre.comp <- data.frame(ECN_data.dum[min:max,4], spe.vec[min:max], ville.vec[min:max])

    
    if(input$Choix.indic=="nbr"){
      ECN_data.dum[,7] <- as.factor(ECN_data.dum[,7])
      ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
      if(input$Spe == 0) {
        if(input$ChoixBDD %in% c("affectations2014","simulations2014")){
          ECN_data.dum <- ECN_data.dum[-which(ECN_data.dum[,11]==""), ]
        }
      } else {
        ECN_data.dum <- ECN_data.dum[which(spe.vec[min:max] == input$Spe),]
      }
      if(input$ChoixBDD %in% c("affectations2014","simulations2014")){
        vec.2 <- levels(as.factor(as.character(ECN_data[-which(ECN_data[,11]==""), ][,7])))
      } else {
        vec.2 <- levels(as.factor(as.character(ECN_data[,7])))
      }
      ECN_data.dum[,7] <- mapvalues(ECN_data.dum[,7],from=vec.2,to=xlab_ville)
      df_order.dum <- ddply(ECN_data.dum, .(Subdivision), summarize, median=length(Etudiant))
      vec0 <- xlab_ville[-which(xlab_ville %in% df_order.dum$Subdivision)]
      df_order <- data.frame(Subdivision = c(as.character(df_order.dum$Subdivision),vec0), Etudiant=c(df_order.dum[,2],rep(0,length(vec0))))
      df_order <- df_order[order(df_order[,2],decreasing=TRUE),]
      df_order[,1] <- factor(df_order[,1], levels = df_order[,1])
      return(df_order)
    }
    
    if(input$Choix.indic=="pourcent"){
      ECN_data.dum <- ECN_data.dum[-which(ECN_data.dum[,11]==""), ]
      ECN_data.dum[,7] <- as.factor(ECN_data.dum[,7])
      ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
      Pourvu <- c()
      Pourvu <- as.data.frame(Pourvu)
      for(i in Offre_vec_spe){
        for(j in Offre_vec_ville){
          poste.pourvu <- length(which(df_offre.comp[which(df_offre.comp[,2]==i),][,3]==j))
          if (length(poste.pourvu)==0){ 
            Pourvu[which(Offre_vec_ville==j),which(Offre_vec_spe==i)] <- 0
          } else { 
            Pourvu[which(Offre_vec_ville==j),which(Offre_vec_spe==i)] <- poste.pourvu
          }
        }
      }
      Pourvu[,dim(Pourvu)[2]] <- 0
      for(i in 1:(dim(Pourvu)[1]-1)){
        Pourvu[i,dim(Pourvu)[2]] <- sum(Pourvu[i,1:(dim(Pourvu)[2]-1)])
      }
      for(i in 1:(dim(Pourvu)[2])){
        Pourvu[dim(Pourvu)[1],i] <- sum(Pourvu[1:(dim(Pourvu)[1]-1),i])
      }
      
      Pourcent_Pourvu <- c()
      Pourcent_Pourvu <- as.data.frame(Pourcent_Pourvu)
      for( i in 1:length(Pourvu[,1])){
        for( j in 1:length(Pourvu[1,])){
          Pourcent_Pourvu[i,j] <- Pourvu[i,j]/as.numeric(Offre_data[i,j])
        }
      }
      
      for(i in 1:length(indic_spe)){
        colnames(Pourcent_Pourvu)[i] <- xlab_spe[which(indic_spe==Offre_vec_spe[i])]
      }
      colnames(Pourcent_Pourvu)[length(Pourcent_Pourvu[1,])] <-"Tot"
      
      for(i in 1:length(indic_ville)){
        rownames(Pourcent_Pourvu)[i] <- xlab_ville[which(indic_ville==Offre_vec_ville[i])]
      }
      rownames(Pourcent_Pourvu)[length(Pourcent_Pourvu[,1])] <-"Tot"
      df_order<-data.frame(Subdivision=rownames(Pourcent_Pourvu), Pourcentage=unlist(Pourcent_Pourvu[,which(Offre_vec_spe == input$Spe),drop=TRUE])*100)
      df_order <- df_order[order(df_order[,2],decreasing=TRUE),]
      df_order[,1] <- factor(df_order[,1], levels = df_order[,1])
      #if(length(which(is.na(df_order[,2])))==0) {} else {
      #  df_order <- df_order[-which(is.na(df_order[,2])),]
      #}
      return(df_order)
    }
    
    if(input$Choix.indic=="Offre"){
      datad <- unlist(Offre_data[-length(Offre_data[,1]),which(Offre_vec_spe==input$Spe),drop=TRUE])
      datad[which(is.na(datad))] <- 0
      df <- data.frame(Subdivision=Offre_vec_ville[-length(Offre_vec_ville)], Offre=datad)
      df <- df[order(df[,2],decreasing=TRUE),]
      df$Offre.p <- df$Offre + max(df$Offre)*0.03
      df$xlab_ville <- NA
      for(i in 1:28){
        df$xlab_ville[i] <- xlab_ville[which(indic_ville==df$Subdivision[i])]
      }
      df$Subdivision <- as.factor(df$xlab_ville)
      df[,4] <- factor(df[,4], levels = df[,4])
      df <- df[,c(1,2)]
      xh <- df[1,2]
      tot <- Offre_data[length(Offre_data[,1]),which(Offre_vec_spe==input$Spe)]
      tot <- paste("Total : ", as.character(tot))
      dftot <- data.frame(tot=tot, xh=xh)
      return(df)
    }
  })
  
  #données de sexe
  sexeD <- reactive({
    #data <- ECN_data
    if(input$ChoixBDD %in% c("simulations2014","affectations2014")){
      data <- dataset()
      return(data.frame(Subdivision = levels(data$Subdivision), valeur=NA))
    } else {
      data <- dataset()
      #variation selon les bornes
      if(input$Restcand == "oui"){
        min <- as.numeric(input$Rang.min)
        max <- as.numeric(input$Rang.max)
        data <- data[min:max,]
      }
      if(input$Restcand == "non"){
        min <- 1
        max <- length(data[,1])
        data <- data[min:max,]
      }
      #variation selon la spé
      #input <- list(Spe=4)
      if(input$Spe != 0){
        data$Discipline <- mapvalues(data$Discipline, from=xlab_spe, to=indic_spe)
        data <- data[(which(data$Discipline==input$Spe)),]
      }
      if(input$Spe == 0){
        data <- data
      }
      vecS <- split(data$sexe, data$Subdivision)
      listeS <- lapply(vecS, function(x){round(table(x)[1]/length(x),3)*100})
      df <- data.frame(Subdivision = names(listeS), valeur = unlist(listeS))
      return(df)
    }
  })
  
  #données d'age
  ageD <- reactive({
    #data <- ECN_data
    if(input$ChoixBDD %in% c("simulations2014","affectations2014")){
      data <- dataset()
      return(data.frame(Subdivision = levels(data$Subdivision), valeur=NA))
    } else {
      data <- dataset()
      if(input$ChoixBDD=="affectations2010"){ddp <- 2010}
      if(input$ChoixBDD=="affectations2011"){ddp <- 2011}
      if(input$ChoixBDD=="affectations2012"){ddp <- 2012}
      if(input$ChoixBDD=="affectations2013"){ddp <- 2013}
      data$age <- ddp + 5/12 - (data$dda + (data$ddm-0.5)/12)
      #variation selon les bornes
      if(input$Restcand == "oui"){
        min <- as.numeric(input$Rang.min)
        max <- as.numeric(input$Rang.max)
        data <- data[min:max,]
      }
      if(input$Restcand == "non"){
        min <- 1
        max <- length(data[,1])
        data <- data[min:max,]
      }
      #variation selon la spé
      if(input$Spe != 0){
        data$Discipline <- mapvalues(data$Discipline, from=xlab_spe, to=indic_spe)
        data <- data[(which(data$Discipline==input$Spe)),]
      }
      if(input$Spe == 0){
        data <- data
      }
      vecS <- split(data$age, data$Subdivision)
      if(input$choixAge == 'med'){listeS <- lapply(vecS, function(x){median(x)})}
      if(input$choixAge == 'moy'){listeS <- lapply(vecS, function(x){mean(x)})}
      if(input$choixAge == 'PQuart'){listeS <- lapply(vecS, function(x){quantile(x,0.25)})}
      if(input$choixAge == 'TQuart'){listeS <- lapply(vecS, function(x){quantile(x,0.75)})}
      if(input$choixAge == 'min'){listeS <- lapply(vecS, function(x){
        if(length(x)== 0){return(NA)} else {min(x)}})}
      if(input$choixAge == 'max'){listeS <- lapply(vecS, function(x){
        if(length(x)== 0){return(NA)} else {max(x)}})}
      df <- data.frame(Subdivision = names(listeS), valeur = unlist(listeS))
      return(df)
    }
  })
  
  attrD <- reactive({
    ECN_data <- dataset()
    Offre_data <- Offre()
    spe.vec <- as.numeric(substr(ECN_data[,11],start=5,stop=7))
    ville.vec <- as.numeric(substr(ECN_data[,11],start=2,stop=4))
    ECN_data.dum <- ECN_data
    df_offre.comp <- data.frame(as.numeric(as.character(ECN_data.dum[,4])),spe.vec,ville.vec)
    Attracti<-c()
    if(input$Spe==000){
      for(i in indic_ville){
        nbr.poste <- as.numeric(Offre_data[which(Offre_vec_ville==i),31])
        nbr.poste.t <- as.numeric(Offre_data[29,31])
        if(is.na(nbr.poste)){
          Attracti[which(indic_ville==i)] <- NA
        } else {
          if(input$ChoixBDD %in% c("affectations2014","simulations2014")){
            offre.attr <- df_offre.comp[-which(ECN_data[,11]==""),1]
          } else {
            offre.attr <- df_offre.comp[,1]
          }          
          if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
            SXmax <- (max(ECN_data.dum$Etudiant,na.rm=T)+1)*nbr.poste
          } else {
            times <- (nbr.poste.t - length(offre.attr))
            SXmax <- sum(c(offre.attr[(length(offre.attr) - (nbr.poste - (times))):length(offre.attr)], rep((max(ECN_data.dum$Etudiant,na.rm=T)+1), times)))
          }
          offre.attr <- offre.attr[1:nbr.poste]
          SXmin <- sum(offre.attr)
          x <- df_offre.comp[which(df_offre.comp[,3]==i),1]
          if(length(x) < length(offre.attr)){
            x <- c(x,rep((max(ECN_data.dum$Etudiant,na.rm=T)+1),length(offre.attr) - length(x)))
          }
          Sx <- sum(x)
          Attracti[which(indic_ville==i)] <- (Sx - SXmin)/(SXmax-SXmin)
        }
      }
    } else {
      if(input$Choix.calc=="glob"){
        for(i in indic_ville){
          nbr.poste <- as.numeric(Offre_data[which(Offre_vec_ville==i),which(Offre_vec_spe==input$Spe)])
          nbr.poste.t <- as.numeric(Offre_data[29,which(Offre_vec_spe==input$Spe)])
          if(is.na(nbr.poste)){
            Attracti[which(indic_ville==i)] <- NA
          } else {
            offre.attr <- df_offre.comp[which(df_offre.comp[,2]==input$Spe),1]
            if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
              SXmax <- (max(ECN_data.dum$Etudiant,na.rm=T)+1)*nbr.poste
            } else {
              borne.1 <- nbr.poste.t - nbr.poste + 1
              borne.2 <- nbr.poste.t
              borne.3 <- length(offre.attr)
              vec.1 <- offre.attr[borne.1:borne.3]
              vec.2 <- rep(max(ECN_data.dum$Etudiant,na.rm=T) + 1, borne.2 - borne.3)
              SXmax <- sum(c(vec.1,vec.2))
            }
            SXmin <- sum(offre.attr[1:nbr.poste])
            df_dum <- df_offre.comp[which(df_offre.comp[,3]==i),]
            x <- df_dum[which(df_dum[,2]==input$Spe),1]
            if(length(x) < nbr.poste){
              if(length(x) == 0){
                Sx <- (max(ECN_data.dum$Etudiant,na.rm=T)+1)*nbr.poste
              } else {
                x <- c(x,rep(max(ECN_data.dum$Etudiant,na.rm=T)+1,nbr.poste - length(x)))
                Sx <- sum(x)
              }
            } else {
              Sx <- sum(x)
            }
            Attracti[which(indic_ville == i)] <- (Sx - SXmin)/(SXmax - SXmin)
          }
        }
      }
      if(input$Choix.calc=="sel"){
        for(i in indic_ville){
          nbr.poste <- as.numeric(Offre_data[which(Offre_vec_ville==i),which(Offre_vec_spe==input$Spe)])
          nbr.poste.t <- as.numeric(Offre_data[29,which(Offre_vec_spe==input$Spe)])
          if(is.na(nbr.poste)){
            Attracti[which(indic_ville==i)] <- NA
          } else {
            offre.attr <- df_offre.comp[which(df_offre.comp[,2]==input$Spe),1]
            offre.attr <- c(1:length(offre.attr))
            if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
              SXmax <- (length(offre.attr) + 1) * nbr.poste
            } else {
              borne.1 <- nbr.poste.t - nbr.poste + 1
              borne.2 <- nbr.poste.t
              borne.3 <- length(offre.attr)
              vec.1 <- offre.attr[borne.1:borne.3]
              vec.2 <- rep(length(offre.attr) + 1, borne.2 - borne.3)
              SXmax <- sum(c(vec.1,vec.2))
            }
            SXmin  <- sum(offre.attr[1:nbr.poste])
            df_dum <- df_offre.comp[which(df_offre.comp[,2]==input$Spe),]
            df_dum$x <- 1:length(df_dum[,1])
            x <- df_dum$x[which(df_dum[,3]==i)]
            if(length(x) < nbr.poste){ 
              if(length(x) == 0){
                Sx <- (length(offre.attr) + 1)*nbr.poste
              } else {
                x <- c(x, rep(length(offre.attr) + 1, nbr.poste - length(x)))
                Sx <- sum(x)
              }
            } else {
              Sx <- sum(x)
            }
            Attracti[which(indic_ville==i)] <- (Sx - SXmin)/(SXmax - SXmin)
          }
        }
      }    
    }
    df_order <- data.frame(indic_ville,xlab_ville,Attr=100-Attracti*100)
    df_order <- df_order[order(df_order[,3],decreasing=TRUE),]
    df_order[,2] <- factor(df_order[,2], levels = df_order[,2])
    df <- data.frame(Subdivision=df_order[,2],valeur=df_order[,3])
    df <- df[order(as.character(df$Subdivision)),]
    return(df)
  })
  
  ### Partie carte 
  mapmetro <- createLeafletMap(session, "mapmetro")
  colMetro <- c()
  
  # Fonction de création des polygones (col : attribution dynamique des couleurs) 
  drawMap <- function(lea, map, col){
    lea <- map(lea, plot=FALSE, fill=TRUE)
    map$addPolygon(lea$y, lea$x, lea$names, 
      lapply(col, function(x) { 
        list(fillColor = x)
      }),
      defaultOptions=I(list(fill=TRUE, fillOpacity=0.8, stroke=TRUE, opacity=1, color="white", weight=1))
    )    
  }
  
  # Création des polygones sur la valeur médiane au démarrage de la session
  session$onFlushed(once=TRUE, function(){
    drawMap(lea=leaMetro, map=mapmetro, col=colMetro)
  })
  
  PPPCol <- reactive({
    df_order_sel <- donnees_sel()
    if(input$Choix.indic=="pourcent"){ df_order_sel <- df_order_sel[-which(df_order_sel$Subdivision =='Tot'),] }
    df_order_sel <- df_order_sel[order(as.character(df_order_sel$Subdivision)),]
    if(input$Choix.indic %in% c("nbr","Offre")){
      df_order_sel[which(df_order_sel[,2]==0),2] <- NA
    }
    vec <- which(is.na(df_order_sel[,2]))
    if(length(vec)==0){
      df <- data.frame(regions = as.character(df_order_sel$Subdivision), col=CouCon(df_order_sel[,2],'YlOrRd',1000, reverse=TRUE), valeur=df_order_sel[,2])
    } else {      
      df_order_sel[vec,2] <- mean(df_order_sel[,2], na.rm=TRUE)
      df <- data.frame(regions = as.character(df_order_sel$Subdivision), col=CouCon(df_order_sel[,2],'YlOrRd',1000, reverse=TRUE), valeur=df_order_sel[,2])
      df$col <- as.character(df$col)
      df$col[vec] <- "#FFFFFF"
    }
    return(df)
  })
  
  # Modification de ColMetro en fonction de methorder
  observe ({
    methorder <- methorder()
    if(methorder %in% c("median","mean","max","min","TQuart","PQuart","rgL")){
      dfS <- donnees_agreg()
      vec <- which(is.na(dfS[,methorder]))
      if(length(vec)==0){
        df <- data.frame(regions = dfS$Subdivision, col=CouCon(dfS[, methorder],'YlOrRd', 1000))
      } else {
        dfS[vec,methorder] <- mean(dfS[,methorder], na.rm=TRUE)
        df <- data.frame(regions = as.character(dfS$Subdivision), col=CouCon(dfS[,methorder],'YlOrRd', 1000), valeur=dfS[,methorder])
        df$col <- as.character(df$col)
        df$col[vec] <- "#FFFFFF"
      }
    }
    if(methorder %in% c("PPP")){
      df <- PPPCol()
    }
    if(methorder %in% c("Sexe", "Age")){
      if(methorder == "Sexe"){dfS <- sexeD()}
      if(methorder == "Age"){dfS <- ageD()}    
      vec <- which(is.na(dfS[,2]))
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){
        df <- data.frame(regions = dfS$Subdivision, col="#FFFFFF")
      } else {
        if(length(vec)==0){
          df <- data.frame(regions = dfS$Subdivision, col=CouCon(dfS[, "valeur"],'YlOrRd', 1000))
        } else {      
          dfS[vec,2] <- mean(dfS[,2], na.rm=TRUE)
          df <- data.frame(regions = as.character(dfS$Subdivision), col=CouCon(dfS[,2],'YlOrRd', 1000), valeur=dfS[,2])
          df$col <- as.character(df$col)
          df$col[vec] <- "#FFFFFF"
        }
      }
    }
    if(methorder %in% c("Attr")){
      dfS <- attrD()
      vec <- which(is.na(dfS[,2]))
      if(length(vec)==0){
        df <- data.frame(regions = dfS$Subdivision, col=CouCon(dfS[, "valeur"],'YlOrRd',1000, reverse=TRUE))
      } else {      
        dfS[vec,2] <- mean(dfS[,2], na.rm=TRUE)
        df <- data.frame(regions = as.character(dfS$Subdivision), col=CouCon(dfS[,2],'YlOrRd',1000, reverse=TRUE), valeur=dfS[,2])
        df$col <- as.character(df$col)
        df$col[vec] <- "#FFFFFF"
      }
    }
    df <- df[-which(df$regions %in% c('AntG','OceI')),]
    colMetro <<- rep(df$col, times=table(as.factor(leaMetro$regions)))
    drawMap(lea=leaMetro, map=mapmetro, col=colMetro)
  })
  
  dfOrderMetro <- reactive ({
    df <- dfCoordPop
    df_order <- donnees_agreg() 
    dfOrderMetro <- df_order[-which(df_order$Subdivision %in% c('AntG','OceI')),]
    dfOrderMetro <- df_order[rep(dfOrderMetro$Subdivision[order(dfOrderMetro$Subdivision)],times=table(as.factor(leaMetro$regions))),]
    df <- cbind(df, dfOrderMetro)
    df$noms <- rep(nomRegionMetro, times=table(as.factor(leaMetro$regions)))
    df$names <- leaMetro$names
    return(df)
  })
  
  dfOrderMetroSel <- reactive ({
    df <- dfCoordPop
    df_order <- donnees_sel()
    dfOrderM <- df_order[-which(df_order$Subdivision %in% c('AntG','OceI','Tot')),]
    if(input$Choix.indic=="Offre") {
      dfOrderM <- df_order[order(as.character(dfOrderM$Subdivision)),]
      dfOrderM <- dfOrderM[rep(1:26,times=table(as.factor(leaMetro$regions))),]
    } else {
      dfOrderM <- df_order[rep(dfOrderM$Subdivision[order(as.character(dfOrderM$Subdivision))],times=table(as.factor(leaMetro$regions))),]
    }
    df <- cbind(df, dfOrderM)
    df$noms <- rep(nomRegionMetro, times=table(as.factor(leaMetro$regions)))
    df$names <- leaMetro$names
    return(df)
  })
  
  dfMetroSexe <- reactive({
    df <- sexeD()   
    df <- df[rep(df[-which(df$Subdivision %in% c('AntG','OceI')),"Subdivision"],times=table(as.factor(leaMetro$regions))),]
    df$noms <- rep(nomRegionMetro, times=table(as.factor(leaMetro$regions)))
    df$names <- leaMetro$names
    return(df)
  })
  
  dfMetroAge <- reactive({
    df <- ageD()
    df <- df[rep(df[-which(df$Subdivision %in% c('AntG','OceI')),"Subdivision"],times=table(as.factor(leaMetro$regions))),]
    df$noms <- rep(nomRegionMetro, times=table(as.factor(leaMetro$regions)))
    df$names <- leaMetro$names
    return(df)
  })
  
  dfMetroAttr <- reactive({
    df <- attrD()
    df <- df[-which(df$Subdivision %in% c('AntG','OceI')),]
    vec <- order(as.character(df[,"Subdivision"]))
    df <- df[rep(vec,times=table(as.factor(leaMetro$regions))),]
    df$noms <- rep(nomRegionMetro, times=table(as.factor(leaMetro$regions)))
    df$names <- leaMetro$names
    return(df)
  })
    
  observe({
    mapmetro$clearPopups()
    
    event <- input$mapmetro_shape_click
    if (is.null(event)){ return()} else {return()}
    if (is.na(event)){
    #dfOrderMetro <- dfOrderMetro()
    df <- dfOrderMetro[which(leaMetro$names == event$id),]
    #df2 <- dfOrderMetroSel()
    df2 <- df2[which(leaMetro$names == event$id),]
    if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
      #df3 <- dfMetroSexe()
      df3 <- df3[which(leaMetro$names == event$id),]
      #df4 <- dfMetroAge()
      df4 <- df4[which(leaMetro$names == event$id),]
    }
    #df5 <- dfMetroAttr()
    df5 <- df5[which(leaMetro$names == event$id),]
    
    content <- as.character(tagList(
      tags$h4("Subdivision de ", df$noms),
      tags$h4("Rangs:"),
      tags$h5("médian :", df$median),
      tags$h5("moyen :", round(df$mean,1)),
      tags$h5("minimum :", df$min),
      tags$h5("maximum :", df$max),
      tags$h5("limite :", df$rgL),
      tags$h5("premier quartile :", df$PQuart),
      tags$h5("troisième quartile :", df$TQuart),
      if(input$Choix.indic=="pourcent"){
        if(is.na(df2[,4])){
          tags$h5("% postes pourvus :", "pas de postes proposés")
        } else {
          tags$h5("% postes pourvus :", round(df2[,4],1), "%")
        } 
      },
      if(input$Choix.indic=="nbr"){
        if(is.na(df2[,4])){
          tags$h5("Nbr postes pourvus :", "pas de postes proposés")
        } else {
          tags$h5("Nbr postes pourvus :", round(df2[,4],1))
        }
      },
      if(input$Choix.indic=="Offre"){
        if(is.na(df2[,4])){
          tags$h5("Offre :", "pas de postes proposés")
        } else {
          tags$h5("Offre :", round(df2[,4],1))
        }
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        tags$h5("% de femmes :", df3$valeur, "%")
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'med'){tags$h5("Age médian :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'moy'){tags$h5("Age moyen :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'PQuart'){tags$h5("Age - 1e quart :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'TQuart'){tags$h5("Age - 3e quart :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'min'){tags$h5("Age minimum :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'max'){tags$h5("Age maximum :", round(df4$valeur,1), "ans")}
      },
      tags$h5("Attractivité :", round(df5[,2],1))
    ))
    }
    #mapmetro$showPopup(df$lat,df$lng,content)
  })
  
  #Création des polygones Dom-Tom
  #Reunion
  mapReunion <- createLeafletMap(session, "mapReunion")
  colReunion <- c()
  
  session$onFlushed(once=TRUE, function(){
    drawMap(lea=leaReunion, map=mapReunion, col=colReunion)
  })
  
  #fonction couleur carte DOM TOM
  colDomTom <- function(region){
    methorder <- methorder()
    if(methorder %in% c("median","mean","max","min","TQuart","PQuart","rgL")){
      dfS <- donnees_agreg()
      vec <- which(is.na(dfS[,methorder]))
      if(length(vec)==0){
        df <- data.frame(regions = dfS$Subdivision, col=CouCon(dfS[, methorder],'YlOrRd', 1000))
      } else {
        dfS[vec,methorder] <- mean(dfS[,methorder], na.rm=TRUE)
        df <- data.frame(regions = as.character(dfS$Subdivision), col=CouCon(dfS[,methorder],'YlOrRd', 1000), valeur=dfS[,methorder])
        df$col <- as.character(df$col)
        df$col[vec] <- "#FFFFFF"
      }
    }
    if(methorder %in% c("PPP")){
      df <- PPPCol()
    }
    if(methorder %in% c("Sexe","Age")){
      if(methorder == "Sexe"){dfS <- sexeD()}
      if(methorder == "Age"){dfS <- ageD()}
      vec <- which(is.na(dfS[,2]))
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){
        df <- data.frame(regions = dfS$Subdivision, col="#FFFFFF")
      } else {
        if(length(vec)==0){
          df <- data.frame(regions = dfS$Subdivision, col=CouCon(dfS[, "valeur"],'YlOrRd',1000))
        } else {      
          dfS[vec,2] <- mean(dfS[,2], na.rm=TRUE)
          df <- data.frame(regions = as.character(dfS$Subdivision), col=CouCon(dfS[,2],'YlOrRd',1000), valeur=dfS[,2])
          df$col <- as.character(df$col)
          df$col[vec] <- "#FFFFFF"
        }
      }
    }
    if(methorder %in% c("Attr")){
      dfS <- attrD()
      vec <- which(is.na(dfS[,2]))
      if(length(vec)==0){
        df <- data.frame(regions = dfS$Subdivision, col=CouCon(dfS[, "valeur"],'YlOrRd',1000, reverse=TRUE))
      } else {      
        dfS[vec,2] <- mean(dfS[,2], na.rm=TRUE)
        df <- data.frame(regions = as.character(dfS$Subdivision), col=CouCon(dfS[,2],'YlOrRd',1000, reverse=TRUE), valeur=dfS[,2])
        df$col <- as.character(df$col)
        df$col[vec] <- "#FFFFFF"
      }
    }
    df <- df[which(df$regions %in% c(region)),]
    return(df)
  }
  
  observe ({
    df <- colDomTom(region='OceI')
    colReunion <<- rep(df$col, times=table(as.factor(leaReunion$regions)))
    drawMap(lea=leaReunion, map=mapReunion, col=colReunion)
  })
  
  #fonction Popup carte DOM TOM
  showPopupDomTom <- function (click, region, lea){
    mapmetro$clearPopups()
    
    event <- click
    if (is.null(event))
      return()
    
    df_order <- donnees_agreg()
    df <- df_order[which(df_order$Subdivision %in% c(region)),]
    if(region=='OceI') df$noms <- "Océan Indien"
    if(region=='AntG') df$noms <- "Antilles-Guyane"
    
    df_order <- donnees_sel()
    df2 <- df_order[which(df_order$Subdivision %in% c(region)),]
    df2 <- df2[1,]
    if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
      df3 <- sexeD()
      df3 <- df3[which(df3$Subdivision %in% c(region)),]
      df3 <- df3[1,]
      df4 <- ageD()
      df4 <- df4[which(df4$Subdivision %in% c(region)),]
      df4 <- df4[1,]
    }
    df5 <- attrD()
    df5 <- df5[which(df5$Subdivision %in% c(region)),]
    df5 <- df5[1,]

    indic <- 2
    
    content <- as.character(tagList(
      tags$h4("Subdivision de ", df$noms),
      tags$h4("Rangs:"),
      tags$h5("médian :", df$median),
      tags$h5("moyen :", round(df$mean,1)),
      tags$h5("minimum :", df$min),
      tags$h5("maximum :", df$max),
      tags$h5("limite :", df$rgL),
      tags$h5("premier quartile :", df$PQuart),
      tags$h5("troisième quartile :", df$TQuart),
      #tags$h4("Modulable selon la spé"),
      if(input$Choix.indic=="pourcent"){
        if(is.na(df2[,indic])){
          tags$h5("% postes pourvus :", "pas de postes proposés")
        } else {
          tags$h5("% postes pourvus :", round(df2[,indic],1), "%")
        }
      },
      if(input$Choix.indic=="nbr"){
        if(is.na(df2[,indic])){
          tags$h5("Nbr postes pourvus :", "pas de postes proposés")
        } else {
          tags$h5("Nbr postes pourvus :", round(df2[,indic],1))
        }
      },
      if(input$Choix.indic=="Offre"){
        if(is.na(df2[,indic])){
          tags$h5("Offre :", "pas de postes proposés")
        } else {
          tags$h5("Offre :", round(df2[,indic],1))
        }
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        tags$h5("% de femmes :", df3$valeur, "%")
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'med'){tags$h5("Age médian :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'moy'){tags$h5("Age moyen :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'PQuart'){tags$h5("Age - 1e quart :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'TQuart'){tags$h5("Age - 3e quart :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'min'){tags$h5("Age minimum :", round(df4$valeur,1), "ans")}
      },
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
        if(input$choixAge == 'max'){tags$h5("Age maximum :", round(df4$valeur,1), "ans")}
      },
      tags$h5("Attractivité :", round(df5[,2],1))
    ))
    mapmetro$showPopup(42,-2, content)
  }
  
  observe({
    #showPopupDomTom(click=input$mapReunion_shape_click, region='OceI', lea=leaReunion)
  })
  
  #Mayotte
  mapMayotte <- createLeafletMap(session, "mapMayotte")
  colMayotte <- c()  
  
  session$onFlushed(once=TRUE, function() {
    drawMap(lea=leaMayotte, map=mapMayotte, col=colMayotte)
  })
  
  observe ({
    df <- colDomTom(region='OceI')
    colMayotte <<- rep(df$col, times=table(as.factor(leaMayotte$regions)))
    drawMap(lea=leaMayotte, map=mapMayotte, col=colMayotte)
  })
  
  observe({
    #showPopupDomTom(click=input$mapMayotte_shape_click, region='OceI', lea=leaMayotte)
  })
  
  #Guyane
  mapGuyane <- createLeafletMap(session, "mapGuyane")
  colGuyane <- c()  
  
  session$onFlushed(once=TRUE, function() {
    drawMap(lea=leaGuyane, map=mapGuyane, col=colGuyane)
  })
  
  observe ({
    df <- colDomTom(region='AntG')
    colGuyane <<- rep(df$col,times=table(as.factor(leaGuyane$regions)))
    drawMap(lea=leaGuyane, map=mapGuyane, col=colGuyane)
  })
  
  observe({
    #showPopupDomTom(click=input$mapGuyane_shape_click, region='AntG', lea=leaGuyane)
  })
  
  #Guadeloupe
  mapGuadeloupe <- createLeafletMap(session, "mapGuadeloupe")
  colGuadeloupe <- c()  

  session$onFlushed(once=TRUE, function() {
    drawMap(lea=leaGuadeloupe, map=mapGuadeloupe, col=colGuadeloupe)
  })
  
  observe ({
    df <- colDomTom(region='AntG')
    colGuadeloupe <<- rep(df$col, times=table(as.factor(leaGuadeloupe$regions)))
    drawMap(lea=leaGuadeloupe, map=mapGuadeloupe, col=colGuadeloupe)
  })
  
  observe({
    #showPopupDomTom(click=input$mapGuadeloupe_shape_click, region='AntG', lea=leaGuadeloupe)
  })
  
  #Martinique
  mapMartinique <- createLeafletMap(session, "mapMartinique")
  colMartinique <- c()
  
  session$onFlushed(once=TRUE, function() {
    drawMap(lea=leaMartinique, map=mapMartinique, col=colMartinique)
  })
  
  observe ({
    df <- colDomTom(region='AntG')
    colMartinique <<- rep(df$col, times=table(as.factor(leaMartinique$regions)))
    drawMap(lea=leaMartinique, map=mapMartinique, col=colMartinique)
  })
  
  observe({
    #showPopupDomTom(click=input$mapMartinique_shape_click, region='AntG', lea=leaMartinique)
  })
  
  #Couleurs de la légende
  colLegend <- function (x){
    methorder <- methorder()
    if(methorder %in% c("median","mean","max","min","TQuart","PQuart","rgL")){
      df <- donnees_agreg()
      vec <- df[,methorder]
    }
    if(methorder %in% c("PPP")){
      df <- PPPCol()
      vec <- df[,3]
    }
    if(methorder %in% c("Sexe","Age")){
      if(input$ChoixBDD %in% c("simulations2014","affectations2014")){
        vec <- c(0,100, rep(50,26))
      } else {
        if(methorder %in% c("Sexe")){df <- sexeD()}
        if(methorder %in% c("Age")){df <- ageD()}
        vec <- df[,2]
      }
    }
    if(methorder %in% c("Attr")){
      df <- attrD()
      vec <- df[,2]
    }
    return(unlist(lapply(x, function(x){
      if(x != 0){
        if(methorder %in% c("median","mean","max","min","TQuart","PQuart","rgL")){
          df <- data.frame(valeur = seq(min(vec,na.rm=T),max(vec,na.rm=T),diff(range(vec,na.rm=T))/27), col=col)
          return(as.character(round(df[x,1],0)))
        }
        if(methorder %in% c("PPP")){
          df <- data.frame(valeur = seq(max(vec,na.rm=T),min(vec,na.rm=T),-diff(range(vec,na.rm=T))/27), col=col)
          if(input$Choix.indic=="pourcent"){
            return(paste(as.character(round(df[x,1],0)),"%"))
          } else {
            return(as.character(round(df[x,1],0)))
          }
        }
        if(methorder %in% c("Sexe")){
          df <- data.frame(valeur = seq(min(vec,na.rm=T),max(vec,na.rm=T),diff(range(vec,na.rm=T))/27), col=col)
          return(paste(as.character(round(df[x,1],0)), "%"))
        }
        if(methorder %in% c("Age")){
          df <- data.frame(valeur = seq(min(vec,na.rm=T),max(vec,na.rm=T),diff(range(vec,na.rm=T))/27), col=col)
          return(paste(as.character(round(df[x,1],1)),"ans"))
        }
        if(methorder %in% c("Attr")){
          df <- data.frame(valeur = seq(max(vec,na.rm=T),min(vec,na.rm=T),-diff(range(vec,na.rm=T))/27), col=col)
          return(as.character(round(df[x,1],1)))
        }
      } else {
        return("")
      }
    })))
  }

  #rendu de la Légende
  output$Legende <- renderUI({
    tags$table(
      mapply(function(text, color){
        tags$tr(
          tags$td(tags$div(
            style = sprintf("width: 16px; height: 20px; background-color: %s;", color)
          )),
          tags$td(tags$div(text))
        )
      }, colLegend(c(1,0,0,10,0,0,19,0,0,28)),
      col[c(1,4,7,10,13,16,19,22,25,28)], SIMPLIFY=FALSE)
    )
  })
  
  list2 <- list(NULL,NULL,NULL,NULL,NULL,NULL)
  
  #Contenu du bloc gauche
  output$Info <- renderUI({
    listEvent <- reactive({
      event1 <- reactive({
        event <- input$mapmetro_shape_click
      })
      event2 <- reactive({
        event <- input$mapReunion_shape_click
      })
      event3 <- reactive({
        event <- input$mapMayotte_shape_click
      })
      event4 <- reactive({
        event <- input$mapGuadeloupe_shape_click
      })
      event5 <- reactive({
        event <- input$mapMartinique_shape_click
      })
      event6 <- reactive({
        event <- input$mapGuyane_shape_click
      })
      return(list(event1(),event2(),event3(),event4(),event5(),event6()))
    })
    list1 <- listEvent()
    vec <- c()
    for(i in 1:6){
      ll2 <- length(list2[[i]])
      ll1 <- length(list1[[i]])
      if(is.null(list2[[i]][[ll2]])) {dum1 <- 2} else {dum1 <- list2[[i]][[ll2]]}
      if(is.null(list1[[i]][[ll1]])) {dum2 <- 2} else {dum2 <- list1[[i]][[ll1]]}
      vec[i] <- (dum1 != dum2)
    }
    if(length(which(vec==TRUE))==0){event <- NULL} else {
      event <- list1[[which(vec==TRUE)]]
    }
    list2 <<- list1
    if(is.null(event)){
      return(tags$div("Cliquez sur une région"))
    } else {
      if(event$id %in% leaMetro$names){
        dfOrderMetro <- dfOrderMetro()
        df <- dfOrderMetro[which(leaMetro$names == event$id),]
        df2 <- dfOrderMetroSel()
        df2 <- df2[which(leaMetro$names == event$id),]
        if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
          df3 <- dfMetroSexe()
          df3 <- df3[which(leaMetro$names == event$id),]
          df4 <- dfMetroAge()
          df4 <- df4[which(leaMetro$names == event$id),]
        }
        df5 <- dfMetroAttr()
        df5 <- df5[which(leaMetro$names == event$id),]
  
        return(tags$div(
          tags$h4(tags$strong(df$noms)),
          tags$h4("Rangs:"),
          tags$h5("médian :", df$median),
          tags$h5("moyen :", round(df$mean,1)),
          tags$h5("minimum :", df$min),
          tags$h5("maximum :", df$max),
          tags$h5("limite :", df$rgL),
          tags$h5("premier quartile :", df$PQuart),
          tags$h5("troisième quartile :", df$TQuart),
          #tags$h4("Modulable selon la spé"),
          if(input$Choix.indic=="pourcent"){
            if(is.na(df2[,4])){
              tags$h5("% postes pourvus :", "pas de postes proposés")
            } else {
              tags$h5("% postes pourvus :", round(df2[,4],1), "%")
            }
          },
          if(input$Choix.indic=="nbr"){
            if(is.na(df2[,4])){
              tags$h5("Nbr postes pourvus :", "pas de postes proposés")
            } else {
              tags$h5("Nbr postes pourvus :", round(df2[,4],1))
            }
          },
          if(input$Choix.indic=="Offre"){
            if(is.na(df2[,4])){
              tags$h5("Offre :", "pas de postes proposés")
            } else {
              tags$h5("Offre :", round(df2[,4],1))
            }
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            tags$h5("% de femmes :", df3$valeur, "%")
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'med'){tags$h5("Age médian :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'moy'){tags$h5("Age moyen :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'PQuart'){tags$h5("Age - 1e quart :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'TQuart'){tags$h5("Age - 3e quart :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'min'){tags$h5("Age minimum :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'max'){tags$h5("Age maximum :", round(df4$valeur,1), "ans")}
          },
          tags$h5("Attractivité :", round(df5[,2],1))
        ))
      } else {
        if(event$id %in% c(leaGuadeloupe$names, leaGuyane$names, 'M')) region <- 'AntG'
        if(event$id %in% c(leaMayotte$names, 'R')) region <- 'OceI'
        df_order <- donnees_agreg()
        df <- df_order[which(df_order$Subdivision %in% c(region)),]
        if(region=='OceI') df$noms <- "Océan Indien"
        if(region=='AntG') df$noms <- "Antilles-Guyane"
        
        df_order <- donnees_sel()    
        df2 <- df_order[which(df_order$Subdivision %in% c(region)),]
        df2 <- df2[1,]
        indic <- 2
        if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
          df3 <- sexeD()
          df3 <- df3[which(df3$Subdivision %in% c(region)),]
          df3 <- df3[1,]
          df4 <- ageD()
          df4 <- df4[which(df4$Subdivision %in% c(region)),]
          df4 <- df4[1,]
        }
        df5 <- attrD()
        df5 <- df5[which(df5$Subdivision %in% c(region)),]
        df5 <- df5[1,]
        
        return(tags$div(
          tags$h4(tags$strong(df$noms)),
          tags$h4("Rangs:"),
          tags$h5("médian :", df$median),
          tags$h5("moyen :", round(df$mean,1)),
          tags$h5("minimum :", df$min),
          tags$h5("maximum :", df$max),
          tags$h5("limite :", df$rgL),
          tags$h5("premier quartile :", df$PQuart),
          tags$h5("troisième quartile :", df$TQuart),
          #tags$h4("Modulable selon la spé"),
          if(input$Choix.indic=="pourcent"){
            if(is.na(df2[,indic])){
              tags$h5("% postes pourvus :", "pas de postes proposés")
            } else {
              tags$h5("% postes pourvus :", round(df2[,indic],1), "%")
            }
          },
          if(input$Choix.indic=="nbr"){
            if(is.na(df2[,indic])){
              tags$h5("Nbr postes pourvus :", "pas de postes proposés")
            } else {
              tags$h5("Nbr postes pourvus :", round(df2[,indic],1))
            }
          },
          if(input$Choix.indic=="Offre"){
            if(is.na(df2[,indic])){
              tags$h5("Offre :", "pas de postes proposés")
            } else {
              tags$h5("Offre :", round(df2[,indic],1))
            }
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            tags$h5("% de femmes :", df3$valeur, "%")
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'med'){tags$h5("Age médian :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'moy'){tags$h5("Age moyen :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'PQuart'){tags$h5("Age - 1e quart :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'TQuart'){tags$h5("Age - 3e quart :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'min'){tags$h5("Age minimum :", round(df4$valeur,1), "ans")}
          },
          if(input$ChoixBDD %in% c("simulations2014","affectations2014")){} else {
            if(input$choixAge == 'max'){tags$h5("Age maximum :", round(df4$valeur,1), "ans")}
          },
          tags$h5("Attractivité :", round(df5[,2],1))
        ))
      }
    }
  })
})