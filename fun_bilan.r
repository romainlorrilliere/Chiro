#############################################
##     les fonctions pour le rapportage    ##
#############################################



require(data.table)
require(ggmap)
require(ggplot2)


# run.rmd()

run.rmd <- function(file.rmd="bilan_main.rmd",rep.out="outputRapport",file.out=NULL,year = 2018, id_group = "GCPC",format_output="html"){
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



map_local_commune_chiro <- function(dsample,last_year=NULL,group,column_longitude="X_CENTROID", column_latitude= "Y_CENTROID",column_year="ANNEE",column_group="ORIGINE",column_idsite = "INSEE",dsn_commune=NULL) {
    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)
    library(dplyr)

#column_longitude="X_CENTROID"; column_latitude= "Y_CENTROID";  group = "GCPC"; column_year="ANNEE";last_year = 2016;column_year="ANNEE";column_group="ORIGINE";column_idsite = "INSEE";dsn_commune="C:/git/GIS/communes.shp"
#browser()

    if(!is.null(column_longitude)) dsample$longitude <- dsample[,column_longitude,with=FALSE]
    if(!is.null(column_latitude)) dsample$latitude <- dsample[,column_latitude,with=FALSE]
    if(!is.null(column_year)) dsample$year <- dsample[,column_year,with=FALSE]
    if(!is.null(column_group)) dsample$group <- dsample[,column_group,with=FALSE]
    if(!is.null(column_idsite))  dsample$id_site <- dsample[,column_idsite,with=FALSE]

    if(is.null(last_year)) last_year <- max(dsample$year)

    if(is.null(dsn_commune)) dsn_commune <- file.chosse()

    commune <- st_read(dsn_commune,quiet=TRUE)

    dsample$group2 <- ifelse(dsample$group == group,group,"autre")
    dsample <- subset(dsample,year <= last_year)

    dsample_y <- unique(subset(dsample,year == last_year,select=c("id_site","longitude","latitude","group","group2")))
    dsample_before <- unique(subset(dsample,year < last_year,select=c("id_site","longitude","latitude","group","group2")))
    dsample_y$last_year <- as.character(last_year)
    dsample_before$last_year <- paste("avant",last_year)

    dsample_before <- subset(dsample_before, !(id_site %in% dsample_y$id_site))

    dloc <- rbind(dsample_before,dsample_y)

    dloc$group3 <- paste(dloc$group2,dloc$last_year)

    dloc2 <- st_as_sf(left_join(dloc,commune,by=c("id_site" = "insee")))


    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))

    vecCol <- c("#b2182b","#f4a582","#2166ac","#92c5de")
    names(vecCol) <- paste(rep(c(group,"autre"),each=2),rep(c(last_year,paste("avant",last_year))))

    xmin <- min(dsample[dsample$group2==group,longitude]) - 0.5
    xmax <- max(dsample[dsample$group2==group,longitude]) + 0.5
    ymin <- min(dsample[dsample$group2==group,latitude]) - 0.5
    ymax <- max(dsample[dsample$group2==group,latitude]) + 0.5


    gg <- ggplot()
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)+ geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + geom_sf(data=dloc2,aes(fill=group3),colour="#d9d9d9")
    gg <- gg + coord_sf(xlim=c(xmin,xmax),ylim=c(ymin,ymax))
    gg <- gg + scale_fill_manual(values=vecCol)
  #  gg <- gg + scale_size_manual(values=vecSize) + scale_shape_manual(values=vecShape)
    gg <- gg + labs(fill="",alpha="",x="",y="")
    print(gg)


}




map_fr_chiro <- function(dsample,last_year=NULL,group,column_longitude="X_CENTROID", column_latitude= "Y_CENTROID",column_year="ANNEE",column_group="ORIGINE",column_idsite = "INSEE") {
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

    dsample$group2 <- ifelse(dsample$group == group,group,"autre")
    dsample <- subset(dsample,year <= last_year)

    dsample_y <- unique(subset(dsample,year == last_year,select=c("id_site","longitude","latitude","group","group2")))
    dsample_before <- unique(subset(dsample,year < last_year,select=c("id_site","longitude","latitude","group","group2")))
    dsample_y$last_year <- as.character(last_year)
    dsample_before$last_year <- paste("avant",last_year)

    dsample_before <- subset(dsample_before, !(id_site %in% dsample_y$id_site))

    dloc <- rbind(dsample_before,dsample_y)
    dloc$group3 <- paste(dloc$group2,dloc$last_year)

    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))



    vecSize <- c(1.8,1.8,1.3,1.3)
    vecShape <- c(19,19,1,1)
    names(vecSize) <-  paste(rep(c(group,"autre"),each=2),rep(c(last_year,paste("avant",last_year))))
    names(vecShape) <-  paste(rep(c(group,"autre"),each=2),rep(c(last_year,paste("avant",last_year))))

    vecCol <- c("#b2182b","#f4a582","#2166ac","#92c5de")
    names(vecCol) <- paste(rep(c(group,"autre"),each=2),rep(c(last_year,paste("avant",last_year))))




    gg <- ggplot()
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)+ geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + geom_point(data=dloc,aes(x=longitude, y=latitude,colour=group3,shape=group3,size=group3))
    gg <- gg + coord_sf(xlim=c(-5,9),ylim=c(41.5,52))
    gg <- gg + scale_color_manual(values=vecCol)
    gg <- gg + scale_size_manual(values=vecSize) + scale_shape_manual(values=vecShape)
    gg <- gg + labs(colour="",alpha="",x="",y="",size="",shape="")
    print(gg)
}




trend_fr_sample_chiro <- function(dsample,last_year=NULL,origine,column_year="ANNEE",first_year_zoom=2000) {



    if(!is.null(column_longitude)) dsample$longitude <- dsample[,column_longitude,with=FALSE]
    if(!is.null(column_latitude)) dsample$latitude <- dsample[,column_latitude,with=FALSE]
    if(!is.null(column_year)) dsample$year <- dsample[,column_year,with=FALSE]
    if(!is.null(column_groupe)) dsample$groupe <- dsample[,column_groupe,with=FALSE]

    if(is.null(last_year)) last_year <- max(dsample$year)


    dsample$group2 <- ifelse(dsample$group == group,group,"autre")
    dsample <- subset(dsample,year <= last_year)

    dunique <- unique(dsample[,c("id_site","year","group2")])



}
