#############################################
##     les fonctions pour le rapportage    ##
#############################################

##if (!require("pacman")) install.packages("pacman")
##pacman::p_load(knitr, captioner, bundesligR, stringr,data.table ,ggmap,maps,ggplot2,dyplr,sf,lubridate,ggpubr)




# run.rmd()

run.rmd <- function(file.rmd="bilan_main.rmd",rep.out="outputRapport",file.out=NULL,year = 2016, id_group = "GCPC",format_output="html"){
    if(is.null(year)) year <- as.numeric(substr(Sys.time(),1,4))
    if(is.null(file.out)) file.out <- paste0("bilan_vigie_chiro_",id_group,"_",year,".",format_output)
    if(!is.null(rep.out)) file.out <- paste0(rep.out,"/",file.out)
    format <- paste0(format_output,"_document")
    title <- paste0("Rapport annuelle des capture de Chiroptères:  ",year,"  ",id_group)
    cat("rmd :",file.rmd,"\n")
    cat("title :",title,"\n")
    cat("format :",format,"\n")
    cat("output :",file.out,"\n")
    rmarkdown::render(file.rmd,output_file=file.out,output_format = format,
                      params = list(year=year,origine = id_group,set_output=format_output))#,set_title=title))
    cat("DONE !!!\n")
}



trend_sample_chiro <- function(dsample,group,last_year=NULL,column_group="ORIGINE",column_idsite = "INSEE",column_year="ANNEE",first_year_zoom=2000) {
    library(ggplot2)
    library(ggpubr)
    library(data.table)
#browser()
 ##group="GCPC";column_year="ANNEE";first_year_zoom=2000

    dsample <- data.table(dsample)
    if(!is.null(column_year)) dsample$year <- dsample[,column_year,with=FALSE]
    if(!is.null(column_group)) dsample$group <- dsample[,column_group,with=FALSE]
    if(!is.null(column_idsite))  dsample$id_site <- dsample[,column_idsite,with=FALSE]

    if(is.null(last_year)) last_year <- max(dsample$year)

    group <- as.character(group)

    dsample$group2 <- ifelse(dsample$group == group,group,"autre")
    dsample <- subset(dsample,year <= last_year)

    dunique <- unique(dsample[,c("id_site","year","group2")])

    dagg <- aggregate(id_site ~ year + group2, data = dunique, length)
    colnames(dagg)[ncol(dagg)] <- "nb"

    vecColour <- c("#b2182b","#2166ac")
    names(vecColour) <- c(group,"autre")


    gg1 <- ggplot(data=subset(dagg,year <= first_year_zoom),mapping=aes(x=year,y=nb,group=group2,colour=group2))
    ##   gg1 <- gg1 + geom_col(colour=NA,alpha=0.75,position="dodge2")
    gg1 <- gg1 + geom_line(alpha=0.6,size=1.1)+ geom_point(size=1.5)
    gg1 <- gg1 + scale_y_sqrt(breaks=c(1,5,10,25,50,100,200,400,600),limits=c(1,600))
    gg1 <- gg1 + scale_colour_manual(values=vecColour)
    gg1 <- gg1 + labs(y="Nombre de communes",x="",fill="")

    gg2 <- ggplot(data=subset(dagg,year >= first_year_zoom), mapping=aes(x=year,y=nb,group=group2,colour=group2))
    ## gg2 <- gg2 + geom_col(colour=NA,alpha=0.75,position="dodge2")
    gg2 <- gg2 + geom_line(alpha=0.6,size=1.1)+ geom_point(size=1.5)
    gg2 <- gg2 + scale_y_sqrt(breaks=c(1,5,10,25,50,100,200,400,600),limits=c(1,600))
    gg2 <- gg2 + scale_colour_manual(values=vecColour)
    gg2 <- gg2 + labs(y="",x="",fill="")

    gg <- ggarrange(gg1,gg2 + rremove("y.text"), ncol = 2, common.legend = T, legend = "bottom")
    print(gg)

}






map_local_commune_chiro <- function(dsample,last_year=NULL,group,
                                    column_longitude="X_CENTROID", column_latitude= "Y_CENTROID",
                                    column_year="ANNEE",column_group="ORIGINE",column_idsite = "INSEE",
                                    dsn_commune=NULL,returnPlot=TRUE) {
    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)
    library(dplyr)

## column_longitude="X_CENTROID"; column_latitude= "Y_CENTROID";  group = "GCPC"; column_year="ANNEE";last_year = 2018;column_year="ANNEE";column_group="ORIGINE";column_idsite = "INSEE";dsn_commune="C:\\git\\data_GIS\\communes-20190101\\communes-20190101.shp"
#browser()

    if(!is.null(column_longitude)) dsample$longitude <- dsample[,column_longitude,with=FALSE]
    if(!is.null(column_latitude)) dsample$latitude <- dsample[,column_latitude,with=FALSE]
    if(!is.null(column_year)) dsample$year <- dsample[,column_year,with=FALSE]
    if(!is.null(column_group)) dsample$group <- dsample[,column_group,with=FALSE]
    if(!is.null(column_idsite))  dsample$id_site <- dsample[,column_idsite,with=FALSE]

    if(is.null(last_year)) last_year <- max(dsample$year)

    if(is.null(dsn_commune)) dsn_commune <- file.chosse()

    group <- as.character(group)
    commune <- st_read(dsn_commune,quiet=TRUE)
    commune$insee <- as.character(commune$insee)

    dsample <- subset(dsample,!is.na(latitude))
    dsample$group2 <- ifelse(dsample$group == group,group,"autre")
    dsample <- subset(dsample,year <= last_year)

    dsample_y <- unique(subset(dsample,year == last_year,select=c("id_site","longitude","latitude","group","group2")))
    i_exclu_y <- which(dsample_y$id_site %in% dsample_y$id_site[dsample_y$group2 == group] & dsample_y$group2 == "autre")
    i_keep_y <- setdiff(1:nrow(dsample_y),i_exclu_y)
    dsample_y <- dsample_y[i_keep_y,]


    dsample_before <- unique(subset(dsample,year < last_year,select=c("id_site","longitude","latitude","group","group2")))
    dsample_before <- subset(dsample_before, !(id_site %in% dsample_y$id_site))
    i_exclu_before<- which(dsample_before$id_site %in% dsample_before$id_site[dsample_before$group2 == group] & dsample_before$group2 == "autre")
    i_keep_before <- setdiff(1:nrow(dsample_before),i_exclu_before)
    dsample_before <- dsample_before[i_keep_before,]

    dsample_y$last_year <- as.character(last_year)
    dsample_before$last_year <- paste("avant",last_year)

    dloc <- rbind(dsample_before,dsample_y)

    dloc$group3 <- paste(dloc$group2,"-",dloc$last_year)

    dloc2 <- st_as_sf(left_join(dloc,commune,by=c("id_site" = "insee")))


    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))

    vecCol <- c("#b2182b","#f4a582","#2166ac","#92c5de")
    names(vecCol) <- paste(rep(c(group,"autre"),each=2),"-",rep(c(last_year,paste("avant",last_year))))
    vecCol <- vecCol[names(vecCol) %in% unique(dloc2$group3)]

    xmin <- min(dsample$longitude[dsample$group2 == group]) - 0.5
    xmax <- max(dsample$longitude[dsample$group2 == group]) + 0.5
    ymin <- min(dsample$latitude[dsample$group2 == group]) - 0.5
    ymax <- max(dsample$latitude[dsample$group2 == group]) + 0.5
##browser()

    gg <- ggplot()
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)
    gg <- gg + geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + geom_sf(data=dloc2,aes(fill=group3),colour="#d9d9d9",size=0.1)
  ##  gg <- gg + geom_point(data=dloc,aes(x=longitude, y=latitude,colour=group3),size=1)
    gg <- gg + coord_sf(xlim=c(xmin,xmax),ylim=c(ymin,ymax))
    gg <- gg + scale_fill_manual(values=vecCol)
    gg <- gg + scale_color_manual(values=vecCol)
  #  gg <- gg + scale_size_manual(values=vecSize) + scale_shape_manual(values=vecShape)
    gg <- gg + labs(fill="",alpha="",x="",y="")


    if(returnPlot) return(gg) else  print(gg)


}




map_fr_chiro <- function(dsample,last_year=NULL,group,
                         column_longitude="X_CENTROID", column_latitude= "Y_CENTROID",
                         column_year="ANNEE",column_group="ORIGINE",column_idsite = "INSEE",
                         returnPlot=TRUE) {
    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)

#column_longitude="X_CENTROID"; column_latitude= "Y_CENTROID";  group = "GCPC"; column_year="ANNEE";last_year = 2016;column_year="ANNEE";column_group="ORIGINE";column_idsite = "INSEE"
#browser()

    if(!is.null(column_longitude)) dsample$longitude <- dsample[,column_longitude,with=FALSE]
    if(!is.null(column_latitude)) dsample$latitude <- dsample[,column_latitude,with=FALSE]
    if(!is.null(column_year)) dsample$year <- dsample[,column_year,with=FALSE]
    if(!is.null(column_group)) dsample$group <- dsample[,column_group,with=FALSE]
    if(!is.null(column_idsite))  dsample$id_site <- dsample[,column_idsite,with=FALSE]

    if(is.null(last_year)) last_year <- max(dsample$year)

    group <- as.character(group)

    dsample$group2 <- ifelse(dsample$group == group,group,"autre")
    dsample <- subset(dsample,year <= last_year)


    dsample_y <- unique(subset(dsample,year == last_year,select=c("id_site","longitude","latitude","group","group2")))
    i_exclu_y <- which(dsample_y$id_site %in% dsample_y$id_site[dsample_y$group2 == group] & dsample_y$group2 == "autre")
    i_keep_y <- setdiff(1:nrow(dsample_y),i_exclu_y)
    dsample_y <- dsample_y[i_keep_y,]


    dsample_before <- unique(subset(dsample,year < last_year,select=c("id_site","longitude","latitude","group","group2")))
    dsample_before <- subset(dsample_before, !(id_site %in% dsample_y$id_site))
    i_exclu_before<- which(dsample_before$id_site %in% dsample_before$id_site[dsample_before$group2 == group] & dsample_before$group2 == "autre")
    i_keep_before <- setdiff(1:nrow(dsample_before),i_exclu_before)
    dsample_before <- dsample_before[i_keep_before,]

    dsample_y$last_year <- as.character(last_year)
    dsample_before$last_year <- paste("avant",last_year)

    dloc <- rbind(dsample_before,dsample_y)
    dloc$group3 <- paste(dloc$group2,"-",dloc$last_year)

    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))



    vecSize <- c(1.8,1.8,1.3,1.3)
    vecShape <- c(19,19,1,1)
    names(vecSize) <-  paste(rep(c(group,"autre"),each=2),"-",rep(c(last_year,paste("avant",last_year))))
    names(vecShape) <-  paste(rep(c(group,"autre"),each=2),"-",rep(c(last_year,paste("avant",last_year))))

    vecCol <- c("#b2182b","#f4a582","#2166ac","#92c5de")
    names(vecCol) <- paste(rep(c(group,"autre"),each=2),"-",rep(c(last_year,paste("avant",last_year))))


    gg <- ggplot()
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)
    gg <- gg + geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + geom_point(data=dloc,aes(x=longitude, y=latitude,colour=group3),size=0.8)
    gg <- gg + coord_sf(xlim=c(-5,9),ylim=c(41.5,52))
    gg <- gg + scale_color_manual(values=vecCol)
    gg <- gg + scale_size_manual(values=vecSize) + scale_shape_manual(values=vecShape)
    gg <- gg + labs(colour="",alpha="",x="",y="",size="",shape="")


    if(returnPlot) return(gg) else  print(gg)
}



date_sample_chiro <- function(dsample,group,last_year=NULL,column_group="ORIGINE",column_idsite = "INSEE",column_year="ANNEE",column_date="DATE_POSIX") {
    library(ggplot2)
    library(ggpubr)
    library(data.table)

#browser()
 group="GCPC";last_year=2016;column_group="ORIGINE";column_idsite = "INSEE";column_year="ANNEE";column_date="DATE_POSIX"

    dsample <- data.table(dsample)
    if(!is.null(column_year)) dsample$year <- dsample[,column_year,with=FALSE]
    if(!is.null(column_group)) dsample$group <- dsample[,column_group,with=FALSE]
    if(!is.null(column_idsite))  dsample$id_site <- dsample[,column_idsite,with=FALSE]
    if(!is.null(column_date))  dsample$date <- dsample[,column_date,with=FALSE]

    if(is.null(last_year)) last_year <- max(dsample$year) else dsample <- subset(dsample,year <= last_year)

    group <- as.character(group)

    dsample$group2 <- ifelse(dsample$group == group,group,"autre")
    dsample$julian <- yday(dsample$date)
    dsample$date_D <- as.numeric(format(as.Date(paste0(dsample$julian,"-2020"),format="%j-%Y"),"%d"))
    dsample$date_DD <- sprintf("%02d", ifelse((( dsample$date_D - 1)%/% 15) >1,1,(( dsample$date_D - 1)%/% 15))*15+1)
    dsample$date_M <- format(as.Date(paste0(dsample$julian,"-2020"),format="%j-%Y"),"%m")
    dsample$date_MM_DD <- paste0(dsample$date_M,"-",dsample$date_DD)

    dsample$julian2 <- yday(as.Date(paste0("2020-",dsample$date_MM_DD)))
    dsample$groupYear <- ifelse(dsample$year == last_year,last_year,paste0("avant ",last_year))

    dsample$group3 <- paste0(dsample$group2," - ",ifelse(dsample$year < last_year,"avant ",""),last_year)

    vecCol <- c("#b2182b","#f4a582","#2166ac","#92c5de")
    names(vecCol) <- paste(rep(c(group,"autre"),each=2),"-",rep(c(last_year,paste("avant",last_year))))


    vecDate <- paste0(2020,"-",1:12,"-",1)
    vecX <- yday(as.Date(vecDate))
    names(vecX) <- format(as.Date(vecDate),"%b")

    gg <- ggplot(data=dsample,mapping=aes(x=julian2,fill=group3)) + geom_bar(width=13) + facet_wrap(group2~groupYear,scales="free_y")
    gg <- gg + scale_fill_manual(values=vecCol)
    gg <- gg + scale_x_continuous(breaks = vecX,labels=names(vecX))
    gg <- gg + theme(axis.text.x = element_text(angle = 90, vjust = 0.25,hjust=1))
    gg <- gg + labs(y="Nombre de session de capture par quinzaine",x="",fill="")
    gg <- gg + coord_cartesian(xlim = c(15,345))
    print(gg)

}




nb_sp <- function(d,group,last_year=NULL,colunm_sp="TAXON",column_group="ORIGINE",column_idsite = "INSEE",column_year="ANNEE",first_year_zoom=2000) {

## group="GCPC";last_year=2016;column_sp="TAXON";column_group="ORIGINE";column_idsite = "INSEE";column_year="ANNEE"

    library(ggplot2)
    library(ggpubr)
    library(data.table)


    dd <- data.table(d)
    if(!is.null(column_year)) dd$year <- dd[,column_year,with=FALSE]
    if(!is.null(column_group)) dd$group <- dd[,column_group,with=FALSE]
    if(!is.null(column_idsite))  dd$id_site <- dd[,column_idsite,with=FALSE]
    if(!is.null(column_sp))  dd$sp <- dd[,column_sp,with=FALSE]
    if(is.null(last_year)) last_year <- max(dd$year) else    dd <- subset(dd,year <= last_year)

    group <- as.character(group)

    dd$group2 <- ifelse(dd$group == group,group,"autre")
    dd$groupYear <- ifelse(dd$year == last_year,last_year,paste0("avant ",last_year))


    duniqueSp <- unique(dd[,c("group2","sp")])

    daggSp <- aggregate(sp ~ group2, data = duniqueSp, length)
    colnames(daggSp)[ncol(daggSp)] <- "nbSp"

    vecColour <- c("#b2182b","#2166ac")
    names(vecColour) <- c(group,"autre")




    ggSp <- ggplot(daggSp,aes(x = group2,y = nbSp,fill=group2)) + geom_col(alpha=.8,colour=NA)
    ggSp <-  ggSp + scale_fill_manual(values=vecColour,)
    ggSp <- ggSp + labs(x="",y="Nombre d'espece",fill="")


    duniqueVar <- unique(dd[,c("year","group2","sp")])

    daggVar <- aggregate(sp ~ year + group2, data = duniqueVar, length)
    colnames(daggVar)[ncol(daggVar)] <- "nbSp"

    ymax <- max(daggVar$nbSp)
    ggVar1 <- ggplot(subset(daggVar,year<=first_year_zoom),aes(x=year,y=nbSp,colour=group2))
    ggVar1 <- ggVar1 + geom_line(alpha=0.6,size=1.1)+ geom_point(size=1.5)
    ggVar1 <- ggVar1 + scale_colour_manual(values=vecColour)
    ggVar1 <- ggVar1 + labs(x="",y="",colour="") + ylim(0,ymax)
    ggVar1


  ggVar2 <- ggplot(subset(daggVar,year>=first_year_zoom),aes(x=year,y=nbSp,colour=group2))
    ggVar2 <- ggVar2 + geom_line(alpha=0.6,size=1.1)+ geom_point(size=1.5)
    ggVar2 <- ggVar2 + scale_colour_manual(values=vecColour)
    ggVar2 <- ggVar2 + labs(x="",y="",colour="") + ylim(0,ymax)
    ggVar2


     gg <- ggarrange(ggSp,ggVar1,ggVar2 + rremove("y.text"), ncol = 3,widths=c(1,2,2), common.legend = T, legend = "bottom")
    print(gg)



}


nb_sp_commune <- function(d,last_year=NULL,group,
                                    column_longitude="X_CENTROID", column_latitude= "Y_CENTROID",colunm_sp="TAXON",
                                    column_year="ANNEE",column_group="ORIGINE",column_idsite = "INSEE",
                                    dsn_commune=NULL,returnPlot=TRUE) {
    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)
    library(dplyr)
    library(ggrepel)

# column_longitude="X_CENTROID"; column_latitude= "Y_CENTROID";  olunm_sp="TAXON";group = "GCPC"; column_year="ANNEE";last_year = 2016;column_year="ANNEE";column_group="ORIGINE";column_idsite = "INSEE";dsn_commune="C:\\git\\GIS\\communes-20190101\\communes-20190101.shp"
##browser()
    dd <- data.table(d)
    if(!is.null(column_longitude)) dd$longitude <- dd[,column_longitude,with=FALSE]
    if(!is.null(column_latitude)) dd$latitude <- dd[,column_latitude,with=FALSE]
    if(!is.null(column_year)) dd$year <- dd[,column_year,with=FALSE]
    if(!is.null(column_group)) dd$group <- dd[,column_group,with=FALSE]
    if(!is.null(column_idsite))  dd$id_site <- dd[,column_idsite,with=FALSE]
    if(!is.null(column_sp))  dd$sp <- dd[,column_sp,with=FALSE]

    if(is.null(last_year)) last_year <- max(dd$year) else  dd <- subset(dd,year <= last_year)

    if(is.null(dsn_commune)) dsn_commune <- file.chosse()

    group <- as.character(group)
    commune <- st_read(dsn_commune,quiet=TRUE)
    commune$insee <- as.character(commune$insee)

    dd$group2 <- ifelse(dd$group == group,group,"autre")

    dd <- unique(dd[,c("id_site","longitude","latitude","group2","year","sp"),with=TRUE])

    dd_map <- unique(subset(dd,group2==group & !is.na(latitude),select=c("id_site","longitude","latitude","sp","group2")))
    agg_map <- aggregate(sp~id_site+longitude+latitude,data=dd_map,length)
    colnames( agg_map)[ncol( agg_map)] <- "nbSp"


    ddd <- unique(subset(dd,select=c("id_site","sp")))
    agg_sp_all<- aggregate(sp~id_site,data=ddd,length)
    colnames( agg_sp_all)[ncol( agg_sp_all)] <- "nbSp"
    medianSp <- median(agg_sp_all$nbSp)

    dloc2 <- st_as_sf(left_join(agg_map,commune,by=c("id_site" = "insee")))


    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))


    xmin <- min(agg_map$longitude) - 0.1
    xmax <- max(agg_map$longitude) + 0.1
    ymin <- min(agg_map$latitude) - 0.1
    ymax <- max(agg_map$latitude) + 0.1

    ggmap <- ggplot()
    ggmap <- ggmap + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)
    ggmap <- ggmap + geom_sf(data = france,fill="#d9d9d9", colour="#7f7f7f", size=0.5)
    ggmap <- ggmap + geom_sf(data=dloc2,aes(fill=nbSp),colour=NA)
    ggmap <- ggmap + scale_fill_gradient2(low = "#d73027",mid="#ffffbf", high = "#313695",midpoint=medianSp)
    ggmap <- ggmap + coord_sf(xlim=c(xmin,xmax),ylim=c(ymin,ymax))
    ggmap <- ggmap + labs(fill="Nombre d'espece",alpha="",x="",y="")
    ggmap <- ggmap + theme(legend.position="top")



    agg_site <- aggregate(sp~id_site+year + group2,data=dd,length)
    colnames( agg_site)[ncol( agg_site)] <- "nbSp"

    agg_var <- aggregate(nbSp ~ group2 + year,data=agg_site, quantile,c(.025,.5,.975))
    agg_var <- data.frame(agg_var[,1:(ncol(agg_var)-1)],agg_var[,ncol(agg_var)])
    colnames( agg_var)[(ncol( agg_var)-2):ncol( agg_var)] <- c("ICinf","median","ICsup")

    vecColour <- c("#b2182b","#2166ac")
    names(vecColour) <- c(group,"autre")

    xmin <- max(min(agg_var$year[agg_var$group2 != "autre"]) - 2,min(agg_var$year))
    xmax <- min(max(agg_var$year[agg_var$group2 != "autre"]) + 2,max(agg_var$year))

    ggvar <- ggplot(data=agg_var,aes(x=year,y=median,group=group2,colour=group2,fill=group2))
    ggvar <- ggvar + geom_ribbon(aes(ymin=ICinf,ymax=ICsup),colour=NA,alpha=0.2)
    ggvar <- ggvar + geom_line(size=1.1,alpha=.5) + geom_point(size=1.5,alpha=.8)
    ggvar <- ggvar + scale_colour_manual(values=vecColour)
    ggvar <- ggvar + scale_fill_manual(values=vecColour)
    ggvar <- ggvar + coord_cartesian(xlim = c(xmin,xmax))
    ggvar <- ggvar + labs(y="Nombre d'espece par commune",colour="",fill="",x="")
    ggvar <- ggvar + theme(legend.position="top")

    gg <- ggarrange(ggmap,ggvar)

    print(gg)


}



fig_sex_ratio <- function(d,last_year=NULL,group, column_sex="SEXE",
                                    column_longitude="X_CENTROID", column_latitude= "Y_CENTROID",colunm_sp="TAXON",
                                    column_year="ANNEE",column_group="ORIGINE",column_idsite = "INSEE",
                                    dsn_commune=NULL,returnPlot=TRUE) {
    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)
    library(dplyr)
    library(ggrepel)

column_sex = "SEXE";column_age = "AGE";column_longitude="X_CENTROID"; column_latitude= "Y_CENTROID";  olunm_sp="TAXON";group = "GCPC"; column_year="ANNEE";last_year = 2016;column_year="ANNEE";column_group="ORIGINE";column_idsite = "INSEE";dsn_commune="C:\\git\\GIS\\communes-20190101\\communes-20190101.shp"
##browser()
    dd <- data.table(d)
    if(!is.null(column_longitude)) dd$longitude <- dd[,column_longitude,with=FALSE]
    if(!is.null(column_latitude)) dd$latitude <- dd[,column_latitude,with=FALSE]
    if(!is.null(column_year)) dd$year <- dd[,column_year,with=FALSE]
    if(!is.null(column_group)) dd$group <- dd[,column_group,with=FALSE]
    if(!is.null(column_idsite))  dd$id_site <- dd[,column_idsite,with=FALSE]
    if(!is.null(column_sp))  dd$sp <- dd[,column_sp,with=FALSE]
    if(!is.null(column_sex))  dd$sex <- dd[,column_sex,with=FALSE]
    if(!is.null(column_age))  dd$age <- dd[,column_age,with=FALSE]

    if(is.null(last_year)) last_year <- max(dd$year) else  dd <- subset(dd,year <= last_year)

    if(is.null(dsn_commune)) dsn_commune <- file.chosse()

    group <- as.character(group)
    commune <- st_read(dsn_commune,quiet=TRUE)
    commune$insee <- as.character(commune$insee)

     dd$group2 <- ifelse(dd$group == group,group,"autre")

    dd <- unique(dd[,c("id_site","longitude","latitude","group2","year","sp"),with=TRUE])

    dd_map <- unique(subset(dd,group2==group & !is.na(latitude) ,select=c("id_site","longitude","latitude","sp","group2")))
    agg_map <- aggregate(sp~id_site+longitude+latitude,data=dd_map,length)
    colnames( agg_map)[ncol( agg_map)] <- "nbSp"


    ddd <- unique(subset(dd,select=c("id_site","sp")))
    agg_sp_all<- aggregate(sp~id_site,data=ddd,length)
    colnames( agg_sp_all)[ncol( agg_sp_all)] <- "nbSp"
    medianSp <- median(agg_sp_all$nbSp)

    dloc2 <- st_as_sf(left_join(agg_map,commune,by=c("id_site" = "insee")))


    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))


    xmin <- min(agg_map$longitude) - 0.1
    xmax <- max(agg_map$longitude) + 0.1
    ymin <- min(agg_map$latitude) - 0.1
    ymax <- max(agg_map$latitude) + 0.1

    ggmap <- ggplot()
    ggmap <- ggmap + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)
    ggmap <- ggmap + geom_sf(data = france,fill="#d9d9d9", colour="#7f7f7f", size=0.5)
    ggmap <- ggmap + geom_sf(data=dloc2,aes(fill=nbSp),colour=NA)
    ggmap <- ggmap + scale_fill_gradient2(low = "#d73027",mid="#ffffbf", high = "#313695",midpoint=medianSp)
    ggmap <- ggmap + coord_sf(xlim=c(xmin,xmax),ylim=c(ymin,ymax))
    ggmap <- ggmap + labs(fill="Nombre d'espece",alpha="",x="",y="")
    ggmap <- ggmap + theme(legend.position="top")



    agg_site <- aggregate(sp~id_site+year + group2,data=dd,length)
    colnames( agg_site)[ncol( agg_site)] <- "nbSp"

    agg_var <- aggregate(nbSp ~ group2 + year,data=agg_site, quantile,c(.025,.5,.975))
    agg_var <- data.frame(agg_var[,1:(ncol(agg_var)-1)],agg_var[,ncol(agg_var)])
    colnames( agg_var)[(ncol( agg_var)-2):ncol( agg_var)] <- c("ICinf","median","ICsup")

    vecColour <- c("#b2182b","#2166ac")
    names(vecColour) <- c(group,"autre")

    xmin <- max(min(agg_var$year[agg_var$group2 != "autre"]) - 2,min(agg_var$year))
    xmax <- min(max(agg_var$year[agg_var$group2 != "autre"]) + 2,max(agg_var$year))

    ggvar <- ggplot(data=agg_var,aes(x=year,y=median,group=group2,colour=group2,fill=group2))
    ggvar <- ggvar + geom_ribbon(aes(ymin=ICinf,ymax=ICsup),colour=NA,alpha=0.2)
    ggvar <- ggvar + geom_line(size=1.1,alpha=.5) + geom_point(size=1.5,alpha=.8)
    ggvar <- ggvar + scale_colour_manual(values=vecColour)
    ggvar <- ggvar + scale_fill_manual(values=vecColour)
    ggvar <- ggvar + coord_cartesian(xlim = c(xmin,xmax))
    ggvar <- ggvar + labs(y="Nombre d'espece par commune",colour="",fill="",x="")
    ggvar <- ggvar + theme(legend.position="top")

    gg <- ggarrange(ggmap,ggvar)

    print(gg)


}
