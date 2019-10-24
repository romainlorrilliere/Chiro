
require(data.table)
require(dplyr)
require(rnoaa)
require(sf)




descri_annee_data <- function(d="data/data_2019-06-06_clean_loc_sunset.csv") {
   ## d="data/data_2019-06-06_clean_loc_sunset.csv"
    library(data.table)
    library(sf)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(ggrepel)

    if(class(d)[1]=="character") d <- fread(d,dec=",")

  ##  d <- subset(d,diff_sunset_heure_valide)

    d_site_year <- unique(d[,c("X_CENTROID","Y_CENTROID","ANNEE")])

    d_site_year <- na.omit(d_site_year)

    geom_site <- st_as_sf(  d_site_year,coords=c("X_CENTROID","Y_CENTROID"), crs=4326) #lambert2 etendu 27572, lambert 93 2154



    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))

    gg <- ggplot() + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)+ geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + geom_sf(data=geom_site,aes(colour=ANNEE),size=.7,alpha=.7)
    gg <- gg + coord_sf(xlim=c(-5,9),ylim=c(41,52))
    gg <- gg + scale_colour_distiller(palette = "Spectral")
    gg <- gg + labs(title="Les sites de captures", colour="Années")

    ggfile <- "output/lesSites.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg,width=9,height=7)

    geom_site$groupe_annee <- cut(as.numeric(geom_site$ANNEE),breaks=c(1949,1990,seq(2000,2020,10)),labels=paste(c(1949,1990,seq(2000,2010,10)),"-",c(1990,seq(2000,2020,10))-1,sep=""),include.lowest=TRUE)

    gg <- ggplot() + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)+ geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.5)+facet_wrap(~groupe_annee,nrow=2)
    gg <- gg + geom_sf(data=geom_site,size=.7,alpha=.7,colour="#e6550d")
    gg <- gg + coord_sf(xlim=c(-5,9),ylim=c(41,52))
    gg <- gg + scale_colour_distiller(palette = "Spectral")
    gg <- gg + labs(title="Les sites de captures", colour="Années")

    ggfile <- "output/lesSites_annee.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg,width=10,height=10)


    nb_annee <- as.data.frame(table( d_site_year$ANNEE))
    colnames(nb_annee) <- c("annee","nombre")
    nb_annee$annee <- as.numeric(as.character(nb_annee$annee))
   nb_annee <- nb_annee[order(nb_annee$nombre,decreasing=TRUE),]
    nb_annee <- data.table(nb_annee)
    nb_annee <- nb_annee[,ordre := 1:.N]
    nb_annee$lab <- nb_annee$ordre<5 | (nb_annee$ordre %% 5 == 0)
    nb_annee$panel <- "Nombre de sites"
    nb_annee$group <- "Tout les sites"


    gg <- ggplot(data=nb_annee,aes(x=annee,y=nombre,label=nombre)) + geom_point(colour="#e6550d",size=2) + geom_line(colour="#e6550d",alpha=1,size=1.2)
    gg <- gg + geom_label_repel(colour="#e6550d",size=2.5,alpha=0.7)
    gg <- gg + labs(title="Historique du nombre de sites de captures", x="Années",y="Nombre de sites")

    ggfile <- "output/historic_nbSites.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg,width=13,height=7)


    d_site_nuit_year <- unique(d[,c("INSEE","DATE_NIGHT_POSIX","ANNEE","periode")])
    nb_site_nuit_annee <- aggregate(INSEE~ANNEE + periode, data=d_site_nuit_year, length)
    colnames(nb_site_nuit_annee) <- c("annee","group","nombre")
  ##  max_nb_nuit <- aggregate(nombre~ periode,nb_site_nuit_annee, max)

    nb_site_nuit_annee <- nb_site_nuit_annee[order(nb_site_nuit_annee$nombre,decreasing=TRUE),]
    nb_site_nuit_annee <- data.table(nb_site_nuit_annee)
    nb_site_nuit_annee <- nb_site_nuit_annee[,ordre := 1:.N , by=group]
    nb_site_nuit_annee$lab <- nb_site_nuit_annee$ordre<4 | (nb_site_nuit_annee$ordre %% 10 == 0)

    nb_site_nuit_annee$panel <- "Nombre d'échantillonnages"


    vecCouleur <- c("0_precoce"="#cccccc", "1_juin_juillet"="#2c7fb8","2_aout_octobre"="#253494","3_tardif"="#525252")
    vecCouleurLab <-  c("0_precoce"="Précoce", "1_juin_juillet"="juin -> juil","2_aout_octobre"="août -> oct","3_tardif"="tardif")

    gg <- ggplot(data=nb_site_nuit_annee,aes(x=annee,y=nombre,label=nombre,group=group,colour=group)) + geom_point(size=2) + geom_line(alpha=1,size=1.2)
    gg <- gg + geom_label_repel(data=subset(nb_site_nuit_annee,lab),size=2.5,alpha=0.9)
    gg <- gg + scale_colour_manual(values=vecCouleur,labels=vecCouleurLab)
    gg <- gg + labs(title="Historique du nombre de nuits (sites) de captures", x="Années",y="Nombre de nuits",colour="")
    ggfile <- "output/historic_nbNuits.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg,width=13,height=7)

    nb_site_nuit_annee <- data.frame(nb_site_nuit_annee)
    nb_site_nuit_annee <- nb_site_nuit_annee[,colnames(nb_annee)]

    nb_annee <- rbind(nb_annee,nb_site_nuit_annee)

    vecCouleur2 <- c("Tout les sites"="#810f7c",vecCouleur)
    vecCouleurLab2 <-  c("Tout les sites"="Tous les sites",vecCouleurLab)

    gg <- ggplot(data=nb_annee,aes(x=annee,y=nombre,label=nombre,group=group,colour=group)) + facet_grid(panel~.,scales="free_y")
    gg <- gg + geom_point(size=2) + geom_line(alpha=1,size=1.2)
    gg <- gg + geom_label_repel(data=subset(nb_annee,lab),size=2.5,alpha=0.9)
    gg <- gg + scale_colour_manual(values=vecCouleur2,labels=vecCouleurLab2)
    gg <- gg + labs(title="Historique du nombre de nuits (sites) de captures", x="Années",y="Nombre de nuits",colour="")
    ggfile <- "output/historic_nbSites_nbNuits.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg,width=11,height=12)


   gg <- ggplot(data=nb_annee,aes(x=annee,y=nombre,label=nombre,group=group,colour=group)) + facet_grid(panel~.,scales="free_y")
    gg <- gg + geom_point(size=2) + geom_line(alpha=1,size=1.2)
    gg <- gg + geom_label_repel(data=subset(nb_annee,lab),size=2.5,alpha=0.9)
    gg <- gg + scale_colour_manual(values=vecCouleur2,labels=vecCouleurLab)
    gg <- gg + labs(title="Historique du nombre de nuits (sites) de captures", x="Années",y="Nombre de nuits",colour="")
    gg <- gg + xlim(1990,max(nb_annee$annee))
    ggfile <- "output/historic_nbSites_nbNuits_1990.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg,width=8,height=10)


}




pheno_nuit <- function(d="data/data_2019-06-06_clean_loc_sunset.csv",output=FALSE) {

    library(data.table)
    library(ggplot2)
    library(dplyr)
    library(mgcv)

   d="data/data_2019-06-06_clean_loc_sunset.csv"
    if(class(d)[1]=="character") d <- fread(d)


    vecEspece <- unique(d$TAXON)
    i <- grep("_",vecEspece)
    vecEspece <- vecEspece[setdiff(1:length(vecEspece),i)]
    i <- grep("sp",vecEspece)
    vecEspece <- vecEspece[setdiff(1:length(vecEspece),i)]
    i <- grep("bulgaricus",vecEspece)
    vecEspece <- vecEspece[setdiff(1:length(vecEspece),i)]
       i <- grep("latipennis",vecEspece)
    vecEspece <- vecEspece[setdiff(1:length(vecEspece),i)]

    vecEspece <- setdiff(vecEspece,c("","Chiroptera","BREDOUILLE","Plecotus","Rhinolophus","Pipistrellus"))
    vecEspece <- as.vector(na.omit(vecEspece))

    d <- subset(d,TAXON %in% vecEspece)

    d$AGE2 <- ifelse(is.na(d$AGE),"U",ifelse(d$AGE %in% c("JUVENILE","IMMATURE"),"JUV","AD"))
    d$SEXE[is.na(d$SEXE)] <- "U"


    despece <- as.data.frame(table(d$TAXON))
    colnames(despece) <- c("espece","nb_capture")
    despece <- despece[order(despece$nb_capture,decreasing=TRUE),]
   ## despece <- subset(despece,nb_capture>20)

    despece$espece <- factor(despece$espece,levels= rev(as.character(despece$espece)))
    gg <- ggplot(data=despece,aes(x=espece,y=nb_capture,label=nb_capture)) + geom_col() + coord_flip() + geom_text(size=2.5,hjust=-0.05)
    gg <- gg + labs(title="Nombre de captures par espèce", x="Nombre de capture",y="")+ylim(0,max(despece$nb_capture)+1500)
    ggfile <- "output/gamm_periode/nbCapture_espece_dataBrut.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg)

    despeceSEX <- aggregate(pk_data~periode+TAXON + SEXE,data=d,length)
    colnames(despeceSEX)[(ncol(despeceSEX)-1):ncol(despeceSEX)] <- c("group","nb_capture")
    despeceSEX$variable <- "sexe"

    despeceAGE <- aggregate(pk_data~periode+TAXON + AGE2,data=d,length)
    colnames(despeceAGE)[(ncol(despeceAGE)-1):ncol(despeceAGE)] <- c("group","nb_capture")
    despeceAGE$variable <- "age"

    despece2 <- rbind(despeceSEX,despeceAGE)
    despece2 <- subset(despece2,TAXON %in% despece$espece)
    despece2$TAXON <- factor(despece2$TAXON, levels=rev(despece$espece))
    despece2$group <- factor(despece2$group,levels=rev(c("AD","JUV","FEMELLE","MALE","U")))

    vecFill <- c("AD"="#4d004b","JUV"="#8c6bb1","FEMELLE"="#ca0020","MALE"="#0571b0","U"="#737373")
    vecFillLab <- c("AD"="Adulte","JUV"="Juvénile","FEMELLE"="Femelle","MALE"="Mâle","U"="Inconnu")

    vecPeriode <- c("1_juin_juillet","2_aout_octobre")
    despece2 <- subset(despece2,periode %in% vecPeriode)



    gg <- ggplot(data=despece2,aes(x=TAXON,y=nb_capture,fill=group))  + coord_flip()+facet_grid(variable~periode)
    gg <- gg + geom_col()
    gg <- gg + scale_fill_manual(values=vecFill,labels=vecFillLab)
    gg <- gg + labs(title="Nombre de captures par espèce", x="Nombre de capture",y="",fill="")
    ggfile <- "output/gamm_periode/nbCapture_espece_sex_periode_brut.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg,width=13,height=10,)




    despece3 <- aggregate(pk_data~periode+TAXON + AGE2 + SEXE ,data=d,length)
    colnames(despece3)[(ncol(despece3)-2):ncol(despece3)] <- c("variable","group","nb_capture")
    despece3$variable <- ifelse(despece3$variable == "AD","Sexe Adulte","Sexe Juvénile")

    despeceAGE <- aggregate(pk_data~periode+TAXON + AGE2,data=d,length)
    colnames(despeceAGE)[(ncol(despeceAGE)-1):ncol(despeceAGE)] <- c("group","nb_capture")
    despeceAGE$variable <- "age"
    despeceAGE <- despeceAGE[,colnames(despece3)]
    despece3 <- rbind(despece3,despeceAGE)

    despece3 <- subset(despece3,TAXON %in% despece$espece)

    despece3$TAXON <- factor(despece3$TAXON, levels=rev(despece$espece))
    despece3$group <- factor(despece3$group,levels=rev(c("AD","JUV","FEMELLE","MALE","U")))

    vecFill <- c("AD"="#4d004b","JUV"="#8c6bb1","FEMELLE"="#ca0020","MALE"="#0571b0","U"="#737373")
    vecFillLab <- c("AD"="Adulte","JUV"="Juvénile","FEMELLE"="Femelle","MALE"="Mâle","U"="Inconnu")

    vecPeriode <- c("1_juin_juillet","2_aout_octobre")
    despece3 <- subset(despece3,periode %in% vecPeriode)


    for(v in unique(despece3$variable)) {
        gg <- ggplot(data=subset(despece3,variable==v),aes(x=TAXON,y=nb_capture,fill=group))  + coord_flip()+facet_grid(variable~periode)
        gg <- gg + geom_col()
        gg <- gg + scale_fill_manual(values=vecFill,labels=vecFillLab)
        gg <- gg + labs(title="Nombre de captures par espèce", x="Nombre de capture",y="",fill="")
        ggfile <- paste("output/gamm_periode/nbCapture_espece_sex_periode_brut_",v,".png",sep="")
        cat("plot -> ",ggfile,"\n")
        ggsave(ggfile,gg,width=8,height=5,)

    }



    d <- subset(d,!is.na(X_CENTROID))
     despece <- as.data.frame(table(d$TAXON))
    colnames(despece) <- c("espece","nb_capture")
    despece <- despece[order(despece$nb_capture,decreasing=TRUE),]
    despece <- subset(despece,nb_capture>200)

    despece$espece <- factor(despece$espece,levels= rev(as.character(despece$espece)))
    gg <- ggplot(data=despece,aes(x=espece,y=nb_capture,label=nb_capture)) + geom_col() + coord_flip() + geom_text(size=2.5,hjust=-0.05)+ylim(0,max(despece$nb_capture)+1500)
    gg <- gg + labs(title="Nombre de captures localisées par espèce", x="Nombre de capture",y="")
    gg

    ggfile <- "output/gamm_periode/nbCapture_espece_data_loc.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg)

    d <- subset(d,diff_sunset_heure_valide)

   sample <- unique(d[,c("DEPARTEMENT","CODE_DEPT","REGION","CODE_REG","REGION_ANCIEN","CODE_REG_ANCIEN","X_CENTROID","Y_CENTROID","DATE_POSIX","DATE_NIGHT_POSIX","DATE_MORNING_POSIX","sunrise","sunset","night_duration_heure")])
    sample$id_sample <- 1:nrow(sample)

    d <- data.table(inner_join(d,sample))


    d$diff_sunset_heure_round <- round(d$diff_sunset_heure,1)

    despece <- as.data.frame(table(d$TAXON))
    colnames(despece) <- c("espece","nb_capture")
    despece <- despece[order(despece$nb_capture,decreasing=TRUE),]
   ## despece <- subset(despece,nb_capture>200)

    despece$espece <- factor(despece$espece,levels= rev(as.character(despece$espece)))
    gg <- ggplot(data=despece,aes(x=espece,y=nb_capture,label=nb_capture)) + geom_col() + coord_flip()+ geom_text(size=2.5,hjust=-0.05)
    gg <- gg + labs(title="Nombre de captures localisées et horodatées par espèce", x="Nombre de capture",y="")+ylim(0,max(despece$nb_capture)+1500)
    ggfile <- "output/gamm_periode/nbCapture_espece.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg)




    despeceSEX <- aggregate(pk_data~periode+TAXON + SEXE,data=d,length)
    colnames(despeceSEX)[(ncol(despeceSEX)-1):ncol(despeceSEX)] <- c("group","nb_capture")
    despeceSEX$variable <- "sexe"

    despeceAGE <- aggregate(pk_data~periode+TAXON + AGE2,data=d,length)
    colnames(despeceAGE)[(ncol(despeceAGE)-1):ncol(despeceAGE)] <- c("group","nb_capture")
    despeceAGE$variable <- "age"

    despece2 <- rbind(despeceSEX,despeceAGE)
    despece2 <- subset(despece2,TAXON %in% despece$espece)
    despece2$TAXON <- factor(despece2$TAXON, levels=rev(despece$espece))
    despece2$group <- factor(despece2$group,levels=rev(c("AD","JUV","FEMELLE","MALE","U")))

    vecFill <- c("AD"="#4d004b","JUV"="#8c6bb1","FEMELLE"="#ca0020","MALE"="#0571b0","U"="#737373")
    vecFillLab <- c("AD"="Adulte","JUV"="Juvénile","FEMELLE"="Femelle","MALE"="Mâle","U"="Inconnu")

    despece2 <- subset(despece2,periode %in% vecPeriode)

    gg <- ggplot(data=despece2,aes(x=TAXON,y=nb_capture,fill=group))  + coord_flip()+facet_grid(variable~periode)
    gg <- gg + geom_col()
    gg <- gg + scale_fill_manual(values=vecFill,labels=vecFillLab)
    gg <- gg + labs(title="Nombre de captures localisées et horodatées par espèce", x="Nombre de capture",y="",fill="")
    ggfile <- "output/gamm_periode/nbCapture_espece_sex_periode.png"
    cat("plot -> ",ggfile,"\n")
    ggsave(ggfile,gg,width=13,height=9,)



    dpheno <- aggregate(pk_data ~ id_sample + diff_sunset_heure_round + TAXON + SEXE + AGE2, length,data=subset(d,TAXON %in% despece$espece))
   colnames(dpheno)[ncol(dpheno)] <- "nb_capture"

    dphenoTot <- aggregate(pk_data ~ id_sample, length,data=subset(d,!(TAXON %in% c("","BREDOUILLE"))))
    colnames(dphenoTot)[ncol(dphenoTot)] <- "nb_capture_tot"

    sps <- despece$espece
    ages <- unique(d$AGE2)
    sexes <- unique(d$SEXE)

    ## les intervalle de temps de capture de chaque sample
    dsample <- unique(d[,c("id_sample","diff_sunset_heure_round")])
    dphenoTot <- inner_join(dphenoTot,dsample)

    ## ajout des especes
    dphenoTot <- data.frame(id_sample=rep(dphenoTot$id_sample,length(sps)),diff_sunset_heure_round = rep(dphenoTot$diff_sunset_heure_round,length(sps)),nb_capture_tot=rep(dphenoTot$nb_capture_tot,length(sps)),TAXON = rep(sps,each=nrow(dphenoTot)))

    ## pour chaque espece on ne garde que les samples qui ont capturé au moins un indiv
    dsample_sp <- aggregate(pk_data ~ id_sample + TAXON , length,data=subset(d,TAXON %in% despece$espece))
    colnames(dsample_sp )[ncol(dsample_sp )] <- "nb_capture_tot_sp_sample"
    dphenoTot <- inner_join( dphenoTot,dsample_sp)

    ## ajout des ages
    dphenoTot <- data.frame(id_sample=rep(dphenoTot$id_sample,length(ages)),diff_sunset_heure_round = rep(dphenoTot$diff_sunset_heure_round,length(ages)),nb_capture_tot=rep(dphenoTot$nb_capture_tot,length(ages)),TAXON = rep(dphenoTot$TAXON,length(ages)),AGE2=rep(ages,each=nrow(dphenoTot)))
    ## ajout des sexes
    dphenoTot <- data.frame(id_sample=rep(dphenoTot$id_sample,length(sexes)),diff_sunset_heure_round = rep(dphenoTot$diff_sunset_heure_round,length(sexes)),nb_capture_tot=rep(dphenoTot$nb_capture_tot,length(sexes)),TAXON = rep(dphenoTot$TAXON,length(sexes)),AGE2=rep(dphenoTot$AGE2,length(sexes)),SEXE=rep(sexes,each=nrow(dphenoTot)))

    dpheno <- full_join(dpheno,dphenoTot)

    dpheno$nb_capture[is.na(dpheno$nb_capture)] <- 0

    dpheno$prop <- dpheno$nb_capture/dpheno$nb_capture_tot

      dperiode <- unique(d[,c("id_sample","periode")])
     dpheno <- inner_join(dpheno,dperiode)

    dpheno$groupe <- ifelse(dpheno$AGE2 == "JUV","JUV",paste0(dpheno$SEXE,"_",dpheno$AGE2))
    dpheno$groupe[dpheno$AGE2== "U" | dpheno$SEXE =="U"] <- "U"
dpheno$tout <- "TOUT"

    dpheno1 <- aggregate(prop~diff_sunset_heure_round + TAXON + groupe + periode + id_sample, subset(dpheno,groupe != "U"),sum)
    dphenoQ1 <- aggregate(prop~diff_sunset_heure_round + TAXON + groupe + periode, dpheno1, FUN=function(X) c("ICinf25"=quantile(X,.025),"med"=median(X),"ICsup75"=quantile(X,.975)))
    dphenoQ1 <- cbind(dphenoQ1[,1:4],dphenoQ1[,5])
    colnames(dphenoQ1) <- c("heure","espece","groupe","periode","ic_inf95","med","ic_up95")

      dpheno2 <- aggregate(prop~diff_sunset_heure_round + TAXON + tout + periode + id_sample, dpheno,sum)
   dphenoQ2 <- aggregate(prop~diff_sunset_heure_round + TAXON + tout + periode, dpheno2, FUN=function(X) c("ICinf25"=quantile(X,.025),"med"=median(X),"ICsup75"=quantile(X,.975)))
    dphenoQ2 <- cbind(dphenoQ2[,1:4],dphenoQ2[,5])
    colnames(dphenoQ2) <- c("heure","espece","groupe","periode","ic_inf95","med","ic_up95")
 dphenoQ <- rbind(dphenoQ1,dphenoQ2)
vecPeriode <- c("1_juin_juillet","2_aout_octobre")
   couleur=c(TOUT="#4d004b",FEMELLE_AD="#ca0020",MALE_AD="#0571b0",JUV="#8c6bb1")

    colnames(dpheno2) <- colnames(dpheno1)
    dpheno12 <- rbind(dpheno1,dpheno2)

    colnames(dpheno12) <- c("heure","espece","groupe","periode","id_sample","prop")

    for(sp in despece$espece){
        dphenoQ_sp <- subset(dpheno12,espece==sp & periode %in% vecPeriode)

       xmin <- max(-1.5,min(dphenoQ_sp$heure))
        xmax <- min(5,max(dphenoQ_sp$heure))
        lesCouleurs <- couleur[names(couleur) %in% dphenoQ_sp$groupe]

        gg <- ggplot(data=dphenoQ_sp,aes(x=heure,y=prop,colour=groupe,fill=groupe,group=groupe))+facet_grid(periode~.,scales="free_y")
       # gg <- gg + geom_ribbon(aes(ymin=ic_inf95, ymax=ic_up95),alpha=.2,colour=NA)+ geom_line(size=0.9)
        gg <- gg+geom_smooth()
        gg <- gg + scale_colour_manual(values=lesCouleurs)+scale_fill_manual(values=lesCouleurs)
        gg <- gg + scale_x_continuous(limits=c(xmin,xmax))
        gg <- gg + labs(y="",x="Heures après le couché du soleil",title=sp)

        ggfile <- paste("output/phenoQuantile/pheno_Quantile",sp,".png",sep="")
        cat("plot -> ",ggfile,"\n")
        ggsave(ggfile,gg)

   }


    gamData <- NULL
    combi <- data.frame(SEXE=c(NA,"MALE","FEMELLE",NA),AGE2=c(NA,"AD","AD","JUV"),groupe=c("TOUT","MALE_AD","FEMELLE_AD","JUV"),stringsAsFactors=FALSE)
 ##   couleur=c(TOUT="#4d004b",FEMELLE_AD="#ca0020",MALE_AD="#0571b0",JUV="#8c6bb1")


    file_output_gamm <- "output/gamm_periode/gamm_phenologie.txt"
    lesPeriodes <- c("1_juin_juillet","2_aout_octobre")

    for(sp in despece$espece){
     ##sp="Pipistrellus pipistrellus"
        cat(sp,"\n================================\n")
        ggGamData_sp <- NULL
        for(p in lesPeriodes) {
             cat("\n",p,"\n-----------------------------\n")
            for(i in 1:nrow(combi)) {
                sexe <- combi$SEXE[i]
                age <- combi$AGE2[i]
                groupe <- combi$groupe[i]
                cat("\n",p,groupe,"\n")
                dpheno_sp <- subset(dpheno,TAXON == sp & periode==p )
                if(groupe != "TOUT") {
                    if(groupe == "JUV") {
                        dpheno_sp <- subset(dpheno_sp,AGE2 == age )
                        dpheno_sp <- aggregate(nb_capture~id_sample + diff_sunset_heure_round,dpheno_sp,sum)
                        colnames(dpheno_sp)[ncol(dpheno_sp)] <- "nb_capture"
                    } else {
                        dpheno_sp <- subset(dpheno_sp,SEXE == sexe & AGE2 == age )
                    }
                } else {
                    dpheno_sp <- aggregate(nb_capture~id_sample + diff_sunset_heure_round,dpheno_sp,sum)
                    colnames(dpheno_sp)[ncol(dpheno_sp)] <- "nb_capture"
                }

                gg <- ggplot(data=dpheno_sp,aes(x=diff_sunset_heure_round,y=nb_capture))+geom_point(alpha=.7)
                gg <- gg + labs(y="",x="Heures après le couché du soleil",title=paste("Phenologie data brutes\n",sp,p,groupe))
                ggfile <- paste("output/gamm_periode/pheno_",sp,"_",p,"_brute_",groupe,".png",sep="")
                cat("plot -> ",ggfile,"\n")
                ggsave(ggfile,gg)

                cat("gamm ")
                gammgg <- gamm(nb_capture~s(diff_sunset_heure_round), data=dpheno_sp,random=reStruct(object = ~ 1| id_sample, pdClass="pdDiag"),familly="quasi-poisson")#,correlation=corAR1(form=~diff_sunset_heure_round))
                cat("DONE!\n")

                sink(file_output_gamm,append=TRUE)
                print(sp)
                print(groupe)
                print(summary(gammgg$gam))
                print(summary(gammgg$lm))
                print("==========================================")
                sink()

                filegamm <- paste("output/gamm_periode/gamm_phenologie",sp,"_",groupe,".Rdata",sep="")
                save(gammgg,file=filegamm)

                realHeure <- sort(unique(dpheno_sp$diff_sunset_heure_round))
                maxHeure <- max(dpheno_sp$diff_sunset_heure)
                minHeure <- min(dpheno_sp$diff_sunset_heure)
                heure.seq<-sort(unique(c(realHeure,(seq(minHeure, maxHeure,length=1000)))))
                heure.seq<-data.frame(diff_sunset_heure_round=heure.seq)

                preds<-predict(gammgg$gam, newdata=heure.seq, type="terms", se.fit=TRUE)
                heure <- heure.seq$diff_sunset_heure_round

                fit <- as.vector(preds$fit)
                init <- fit[1]
                fit.up95<-fit-1.96*as.vector(preds$se.fit)
                fit.low95<-fit+1.96*as.vector(preds$se.fit)


                fit <- fit - init
                fit.up95 <- fit.up95 - init
                fit.low95 <- fit.low95 - init

                ggGamData <- data.frame(espece=sp,periode=p,sexe=sexe,age=age,groupe=groupe,heure=heure, nb=fit,ic_low95 = fit.low95, ic_up95 = fit.up95,realHeure=heure %in% realHeure)
                ggGamData_sp <- rbind(ggGamData_sp,ggGamData)
            } #END  for(i in 1:nrow(combi))
        } #END for(p in lesPeriodes)

        xmin <- max(-2,min(subset(ggGamData_sp,realHeure)$heure))
        xmax <- min(12,max(subset(ggGamData_sp,realHeure)$heure))
        lesCouleurs <- couleur[names(couleur) %in% ggGamData_sp$groupe]
        ## The ggplot:
        gg <- ggplot(data=ggGamData_sp,aes(x=heure,y=nb,colour=groupe,fill=groupe,group=groupe))+facet_grid(periode~.,scales="free_y")
        gg <- gg + geom_ribbon(aes(ymin=ic_low95, ymax=ic_up95),alpha=.2,colour=NA)+ geom_line(size=0.9)
        gg <- gg + scale_colour_manual(values=lesCouleurs)+scale_fill_manual(values=lesCouleurs)
        gg <- gg + scale_x_continuous(limits=c(xmin,xmax))
        gg <- gg + labs(y="",x="Heures après le couché du soleil",title=sp)

        ggfile <- paste("output/gamm_periode/pheno_",sp,".png",sep="")
        cat("plot -> ",ggfile,"\n")
        ggsave(ggfile,gg)

        gamData <- rbind(gamData,ggGamData_sp)

    }

write.csv(gamData,"output/gamm_pheno.csv",row.names=FALSE)
   if(output) return(gamData)
}


download_weather_data <- function(year) {



    }

get_weather_nearest_station <- function() {
    library(rnoaa)
    file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
    repeat {
        try(download.file(file,"isd-history.csv", quiet = TRUE))
        if (file.info("isd-history.csv")$size > 0) {
            break
        }
    }
    st_raw <- read.csv("isd-history.csv")
    dim(st_raw)
    names(st_raw)


    st <- st_raw[st_raw$CTRY == "FR", ]


    st$LAT <- st$LAT/1000
    st$LON <- st$LON/1000
    st$ELEV <- st$ELEV/10
    st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
    st$END <- as.numeric(substr(st$END, 1, 4))


    mi.list <- st[(st$BEGIN <= 2014 & st$END >= 2014 & !is.na(st$BEGIN)), ]

    outputs <- as.data.frame(matrix(NA, dim(mi.list)[1],2))
    names(outputs) <- c("FILE", "STATUS")
    for (y in 2014:2014) {
        y.mi.list <- mi.list[mi.list$BEGIN <= y & mi.list$END >= y, ]
        for (s in 1:dim(y.mi.list)[1]) {
            outputs[s, 1] <- paste(sprintf("%06d", y.mi.list[s, 1]), "-", sprintf("%05d", y.mi.list[s, 2]), "-", y, ".gz", sep = "")
            wget <- paste("wget -P data/raw ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",
                          y, "/", outputs[s, 1], sep = "")
            print(wget)
            outputs[s, 2] <- try(shell(wget, intern = FALSE,
                                       ignore.stderr = TRUE))
        }
    }
    head(outputs)


}


