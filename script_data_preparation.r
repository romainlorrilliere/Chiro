
require(ncdf4)
require(dplyr)
require(data.table)
require(sf)
require(tools)

mainCleaning <- function(d=NULL,dcolumn="library/capture_column.csv",dcommune="library/correctifs_communes.csv",shape_commune="library/CommunesCentr_L93.shp",weatherRdata_file="data/data_meteo_temp_prec_ens_mean_0.25deg_reg_v20.Rdata", do_data_cleaning=TRUE,do_commune_corrections=TRUE,do_localite_adding=TRUE,do_time_cleaning=TRUE,do_period_adding=TRUE,do_sunset_adding=FALSE,do_weather_adding=FALSE,saveStep=TRUE,repStep="data/",save=TRUE,fileoutput=NULL,repOut="data/",output=FALSE) {

    ## d=NULL;dcolumn="library/capture_column.csv";dcommune="library/correctifs_communes.csv";shape_commune="library/CommunesCentr_L93.shp"; do_data_cleaning=TRUE;do_commune_corrections=TRUE;do_localite_adding=TRUE;do_time_cleaning=TRUE;do_period_adding=TRUE;do_sunset_adding=FALSE;do_weather_adding=FALSE;saveStep=TRUE;repStep="data/";save=TRUE;fileoutput=NULL;repOut="data/";output=FALSE

    library(data.table)
    library(tools)

    start <-Sys.time()

    cat("\n----------------------------------------\n  Preparation des données capture chiro\n----------------------------------------\n\n")

    cat(as.character(start),"\n")

    file <- NULL
    if(class(d)[1]=="character") file <- d else file <- format(as.Date(Sys.time()),"%Y%m%d")

    if(is.null(fileoutput))
        fileoutput <- paste0("data_capt_",file,".csv")
    prefFile <- file_path_sans_ext(fileoutput)
    fileoutput <- paste0(repOut,prefFile,".csv")

    d <- my_import_fread(d,"ex: fichier_capt_mai2018_analysesbis.csv")


    cat(" -1- dataCleaning\n")
    if(do_data_cleaning){
        d <- data_cleaning(d,dfield=dcolumn ,output="data",save=saveStep,repOut=repStep,id_output=prefFile)
        cat("    DONE!\n")
    } else {
        cat("    Step canceled...\n")
    }

    cat(" -2- commune_correction\n")
    if(do_commune_corrections){

        d <- commune_corrections(data=d,com_cor=dcommune,output=TRUE,save=saveStep,fileoutput=paste0(prefFile,"_clean_commune.csv"))
        cat("    DONE!\n")
     } else {
        cat("    Step canceled...\n")
    }

    cat(" -3- add_localite\n")
    if(do_localite_adding){
        d <- add_localite(data=d,file_shape_commune=shape_commune,output=TRUE,save=saveStep,repOut=repStep,file_out=paste0(prefFile,"_clean_loc.csv"))
        cat("    DONE!\n")
    } else {
        cat("    Step canceled...\n")
    }

    cat(" -4- date_time_cleaning\n")
    if(do_time_cleaning){
        d <- date_cleaning(d,output=TRUE,save=saveStep,fileoutput=paste0(prefFile,"_clean_loc_time.csv"))
        cat("    DONE!\n")
    } else {
        cat("    Step canceled...\n")
    }

    cat(" -5- ajout_periode\n")
    if(do_period_adding){
        d <- ajout_period(d,output=TRUE,save=saveStep,fileoutput=paste0(prefFile,"_clean_loc_time_periode.csv"))
        cat("    DONE!\n")
    } else {
        cat("    Step canceled...\n")
    }

    cat(" -6- add_sunset_sunrise\n")
    if(do_sunset_adding){
        d <- add_sunset_sunrise(d,output=TRUE,save=saveStep,repout=repStep,fileoutput=paste0(prefFile,"_clean_loc_time_periode_sunset.csv"))
        cat("    DONE!\n")
    } else {
        cat("    Step canceled...\n")
    }

    cat(" -7- add_weather\n")
    if(do_weather_adding){
        d <- add_weather(d,output=TRUE,save=saveStep,repout=repStep,fileoutput=paste0(repStep,prefFile,"_clean_loc_time_periode_sunset_weather.csv"),fileoutput_weather=paste0(repStep,prefFile,"_sample_weather.csv"))
        cat("    DONE!\n")
    } else {
        cat("    Step canceled...\n")
    }


    if(save) write.csv(d,fileoutput,row.names=FALSE)
    if(output) return(d)
}



my_import_fread <- function(d,descri=NULL) {
    library(data.table)
      if (is.null(d)) {
          cat("select your data file\n")
          if(!is.null(descri)) cat(descri,"\n")
        d <- fread(file.choose())
    } else { # ELSE if (is.null(d))
        if(class(d)[1]=="character") {
            if(file.exists(d)) {
                cat("Importation:",d)
                d <- fread(d)
                cat("  DONE !\n")
            } else {
                print("File",d," does not exist, select the correct file:\n")
                d <- fread(file.choose())
            }
        }
        else {
            d <- data.table(d)
        }
    } # END ELSE if (is.null(d))
    return(d)
}



data_cleaning <- function(d=NULL,dfield="library/capture_column.csv",save=TRUE,repOut="data",id_output=NULL,output="data") {
    library(dplyr)
#browser()
    ## Pour deboguage ------
    ##  d <- NULL ;    dfield <- NULL;  save=TRUE;repOut="data";id_output=NULL;output="data"
    ## ------------------------

    d <- my_import_fread(d,"ex: fichier_capt_mai2018_analysesbis.csv")
    d <- data.frame(pk_data=1:nrow(d),d,error_data="",stringsAsFactors=FALSE)

    dfield <- my_import_fread(dfield,"that describe the column of data capture table")

    if(is.null(id_output)) id_output<- format(as.Date(Sys.time()),"%Y%m%d")

    dfield <- subset(dfield,keeped)
    dclass <- as.data.frame(sapply(d,class),stringsAsFactors=FALSE)
    dclass$name <- rownames(dclass)
    colnames(dclass)[1] <- "class"


    ddfield <- inner_join(dfield,dclass)
    ddfield <- ddfield[,c("name","expected_class","class")]
    ## only the class not expected
    ddfield <- ddfield[ddfield$expected_class != ddfield$class,]

    d <- d[,c("pk_data",dfield$name,"error_data")]
    d_error <-  data.frame(column_error=NA,type_error=NA,value_error=NA,new_value=NA,d[1,],stringsAsFactors=FALSE)
    d_error <- d_error[-1,]
    if(nrow(ddfield) > 0) {
        cat(nrow(ddfield)," column with error in the class automatically affected !!!\n")
        cat("  ->  ",paste(ddfield$name, collapse=", "),"\n\n")

        cat("  correcting :\n")
        for(n in ddfield$name) {
            ddfield_n <- subset(ddfield,name==n)
            print(ddfield_n,row.names=FALSE)
            exp_class <- ddfield_n$expected_class
            fun_2_class <- paste("as.",exp_class,sep="")
            col_n <- d[,n]
            col_n <- toupper(gsub(" ","",col_n))
            if(exp_class == "logical") {
                nb_replac <- length(which(col_n %in% c("OUI","NON")))
                if(nb_replac>0) {
                    cat("replacement of",nb_replac,"'OUI' and 'NON' by 'TRUE' and 'FALSE' \n")
                    col_n <- ifelse(col_n == "OUI","TRUE",ifelse(col_n=="NON","FALSE",NA))
                }
            }

            if(exp_class == "integer") {
                nb_replac <- length(which(col_n == "X"))
                if(nb_replac >0) {
                    cat("replacement of",nb_replac,"'X' by '0' \n")
                    col_n <- ifelse(col_n == "X","0",col_n)
                }
            }
            col_n <- do.call(fun_2_class,list(col_n))
            row_pb <- which(is.na(col_n) & !(is.na(d[,n]) | d[,n]=="NA" | d[,n] ==""))
            if(length(row_pb>0)) {
                cat(length(row_pb),"value(s) can't be tranformed\n")
                d$error_data[row_pb] <- paste(d$error_data[row_pb],n,sep="|")

                d_error_n <-  data.frame(column_error=n,type_error=paste(exp_class,"expected"),value_error=d[row_pb,n],new_value=NA,d[row_pb,],stringsAsFactors=FALSE)
                d_error <- rbind(d_error,d_error_n)

            } else {
                cat("All value(s) have been tranformed\n")
            }
            d[,n] <- col_n
            cat("\n")
        }


    }

    field_list <- subset(dfield,keeped&expected_class=="character"& allowed_val!= "")$name

    if(length(field_list) > 0) {
        cat(length(field_list)," colomn with list of allowed values\n")
        cat("       ",paste(field_list,collapse=", "),"\n\nChecking:\n")

        for(f in field_list) {
            ddf <- subset(dfield,name==f,select=c("name","allowed_val","NA_allowed"))
            print(ddf,row.names=FALSE)
            space_forbiden <- length(grep(" ",ddf$allowed_val)) == 0
            if(space_forbiden & length(grep(" ",d[,f])>0)) {
                cat("Deleting",length(grep(" ",d[,f])),"not allowed space\n")
                d[,f] <- gsub(" ","",d[,f])
            }

            lower_forbiden <- ddf$allowed_val == toupper(ddf$allowed_val)
            if(lower_forbiden) {
                cat("Lower case not allowed --> Forcing to upper case\n")
                d[,f] <- toupper(d[,f])
            }

            vecval <- as.vector(strsplit(ddf$allowed_val,"|",fixed=TRUE)[[1]])

            if(!("" %in% vecval) & length(which(d[,f]==""))> 0) {
                cat("Empty string is not allowed --> ",length(which(d[,f]=="")),"Replacing by NA\n")
                d[,f] <- ifelse(d[,f]=="",NA,d[,f])
            }


            if(ddf$NA_allowed) vecval <- c(vecval,NA)
            i_error <- which(!(d[,f] %in% vecval))

            if(length(i_error)>0) {
                cat(length(i_error),"value of the",f,"field do not allowed \n   replaced by NA\n")
                d$error_data[i_error] <- paste(d$error_data[i_error],f,sep=";")

                d_error_f <-  data.frame(column_error=f,type_error="value not allowed",value_error=d[i_error,f],new_value=NA,d[i_error,],stringsAsFactors=FALSE)
                d_error <- rbind(d_error,d_error_f)
            }
            cat("\n")
        }
    }

    d_error <- d_error[order(d_error$pk_data),]

    if(save) {
        prefix_file <- paste0(repOut,id_output)
        file_data <- paste0(prefix_file,"_clean.csv")
        file_error <- paste0(prefix_file,"_error.csv")
        cat("\n\n")
        cat("  -->", file_data)
        write.csv(d,file_data,row.names=FALSE)
        cat("   DONE !\n")
        cat("  -->", file_error)
        write.csv(d_error,file_error,row.names=FALSE)
        cat("   DONE !\n")
    }

    cat("\n\n")
    if(output=="all")return(list(d,d_error))
    if(output=="error") return(d_error)
    if(output=="data") return(d)
}




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title correction des noms de communes
##' @param data data table or path of the data table
##' @param com_cor correcting table of the path of the table
##' @return data.table of updating data
##' @author Romain Lorrilliere
commune_corrections <- function(data=NULL,com_cor="library/correctifs_communes.csv",output=FALSE,save=saveStep,fileoutput=NULL) {
    ## Pour deboguage ------
    ##data=NULL;com_cor=NULL
    ## ----------------------
    library(data.table)


    data <- my_import_fread(data,"ex: fichier_capt_mai2018_analysesbis_clean.csv")
    com_cor <- my_import_fread(com_cor," of commune file ex: library/correctifs_communes.csv")

    data <- data.table(data)
    data$COMMUNUE <- toupper(data$COMMUNE)

    cat("\nDeplacement des articles en début de nom\n\n",sep="")
    vecArticle <- c("LA","LE","LES","L'")
    for( a in vecArticle) {
        from <- paste("[-,\\s]*\\(",a,"\\)[-,\\s]*\\(?([0-9]{1,2}|2[A,B])?\\)?[-,\\s]*$",sep="")
        ifrom <- grep(from,data$COMMUNE,perl=TRUE)
        cat("(",a,"): ",gsub("\\","\\\\",from,fixed=TRUE),"\n","    -> ",length(ifrom),"\n",sep="")
        data$COMMUNE[ifrom] <- paste(a,ifelse(a=="L'",""," "),data$COMMUNE[ifrom],sep="")
        data$COMMUNE[ifrom] <- gsub(from,"",data$COMMUNE[ifrom],perl=TRUE)

    }


    cat("\nCorrectifs des noms de communes (",nrow(com_cor)," correctifs)\n\n",sep="")

    for(i in 1:nrow(com_cor)) {
        ##  i=1

        from <- gsub("\\\\","\\",com_cor[i,search],fixed=TRUE)
        ##  print(from)
        to <- com_cor[i,replaceBy]
        sub <- com_cor[i,restrict]!=""
        strict <- com_cor[i,strictly]
        cat(i,": ",gsub("\\","\\\\",as.vector(from),fixed=TRUE)," -> ", to," | subset:",sub,"  nom complet: ",strict,"\n")
        if(sub) { # on remplace un sous ensemble des noms
            col <- com_cor[i,fieldRestrict]
            val <- com_cor[i,restrict]
            cat("     ",col," %in% ",val," \n")
            if(strict) { # on remplace tout le nom
                nbcor <- nrow(data[data[[col]] %in% val & data$COMMUNE == from])
                if(nbcor>0)
                    data <- data[data[[col]] %in% val & data$COMMUNE == from,COMMUNE:= to]
            } else { # ELSE strict   remplace qu'un pattern
                nbcor <- length(grep(from,data[data[[col]] %in% val,COMMUNE]))
                if(nbcor>0)
                    data$COMMUNE <- ifelse(data[[col]] %in% val, gsub(from,to,data$COMMUNE),data$COMMUNE)
            } # END ELSE strict
        } else { # ELSE sub on remplace tous les noms
            if(strict) { # on remplace tout le nom
                nbcor <- nrow(data[data[["COMMUNE"]]==from])
                if(nbcor>0)
                    data <- data[data[["COMMUNE"]]==from,"COMMUNE":= to]
            } else { # ELSE strict   remplace qu'un pattern
                nbcor <- length(grep(from,data[,COMMUNE],perl=TRUE))
                if(nbcor>0)
                    data$COMMUNE <- gsub(from,to,data[,COMMUNE],perl=TRUE)
            } # END ELSE strict
        } #END ELSE sub
        cat("    -> ",nbcor," remplacement(s)\n")
    } #END for(i in 1:nrow(com_cor))
    if(save) {
        if(is.null(fileoutput))fileoutput<- paste0(format(as.Date(Sys.time()),"%Y%m%d"),"_clean_commune.csv")
        file_data <- paste0(repOut,fileoutput)
        cat("\n\n")
        cat("  -->", file_data)
        write.csv(data,file_data,row.names=FALSE)
    }

    if(output) return(data)
} # END commune_corrections



update_correctif_file <- function(data,dataloc_insee_erreur,file_cor="library/correctifs_communes.csv") {

    tab_cor <- NULL
    col <- c("COMMUNE","NOM_COMM")
    erreur_nom_com <- unique(dataloc_insee_erreur[,col])
    nb_erreur <- nrow(erreur_nom_com)
    print(data.table(erreur_nom_com))

    ii <- NA
    answer <- readline(prompt = "Noter les numéros de ligne (en les séparants d'une virgule x,x,x) qui ne doivent pas être corriger sans filtre. Si tous les corrections peuvent être intégées au fichiers de correctif tapez <ENTRER>:")
    if(answer != "") {
        ii <- as.vector(strsplit(answer,",")[[1]])
        ii <- gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",ii,perl=TRUE)
        ii <- ii[ii!=""]
        cat("Vous avez saisie les lignes suivante: ", ii,"\n")
        ii <- suppressWarnings(as.numeric(ii))
    }

    while(answer != "" & (0 %in% ii | any(is.na(ii)) | max(ii)> nb_erreur)) {
        cat(" ERREUR: Les numéros de lignes ne sont pas valide, les resaisirs ! \n")
        answer <- readline(prompt = "Noter les numéros de ligne (en les séparants d'une virgule x,x,x) qui ne doivent pas être corriger sans filtre. Si tous les corrections peuvent être intégées au fichiers de correctif tapez <ENTRER>:")
        if(answer != "") {
            ii <- as.vector(strsplit(answer,",")[[1]])
            ii <- gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",ii,perl=TRUE)
            ii <- ii[ii!=""]
            cat("   Vous avez saisie les lignes suivante: ", ii,"\n")}
    }

    if(answer != "") {
        cat("\n Pour les ",length(ii)," commune(s), veuillez saisir la colonnes de selection puis la ou les valeurs retenues:\n",sep="")
        cat("Les colonnes disponibles:\n ", colnames(data),"\n")
        for(i in ii) {
                                        # i=6

            print(data.table(erreur_nom_com[i,]))
            col <- readline(prompt = "la colonne:")
            col <- gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",col,perl=TRUE)
            col <- col[col!=""]
            cat("   Vous avez saisie la colonne: ", col,"\n")

            while(!(col %in% colnames(data))) {
                col <- readline(prompt = "ERREUR! la colonne n'est pas valide, ressaisissez la colonne:")
                col <- gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",col,perl=TRUE)
                col <- col[col!=""]
                cat("   Vous avez saisie la colonne: ",col,"\n")
            } # END while(!(col %in% colnames(data)))

            val <- readline(prompt = "La ou les valeurs (séparées par une virgule:")
            val <- as.vector(strsplit(val,",")[[1]])
            val <- gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",val,perl=TRUE)
            val <- val[val!=""]
            cat("   Vous avez saisie la ou les valeur(s) suivante(s): ", val,"\n")
            nbval <- nrow(data[data[[col]] %in% val & data[["COMMUNE"]] == erreur_nom_com$COMMUNE[i]])

            while(nbval == 0) {
                cat("ERREUR aucune ligne concernée par cette valeur, veuillez saisir une valeur valide ! \n")
                val <- readline(prompt = "La ou les valeurs (séparées par une virgule:")
                val <- as.vector(strsplit(val,",")[[1]])
                val <- gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",val,perl=TRUE)
                val <- val[val!=""]
                cat("   Vous avez saisie la ou les valeur(s) suivante(s): ", val,"\n")
                nbval <- nrow(data[data[[col]] %in% val & data[["COMMUNE"]] == erreur_nom_com$COMMUNE[i]])
            } #END while(nbval == 0)

            cat("  -> ",nbval," ligne(s) affectér par la correction\n",sep="")
            tab_cor <- rbind(tab_cor,data.table(search=erreur_nom_com$COMMUNE[i],replaceBy=erreur_nom_com$NOM_COMM[i],restrict=val,fieldRestrict=col,stricly=TRUE,comment=""))
        } # END for(i in ii)
        erreur_nom_com <- erreur_nom_com[-ii,]
        if(nrow(erreur_nom_com)>0)
            tab_cor <- rbind(tab_cor,data.table(search=erreur_nom_com$COMMUNE,replaceBy=erreur_nom_com$NOM_COMM,restrict="",fieldRestrict="",stricly=TRUE,comment=""))
    } else { # ELSE answer
        tab_cor <- rbind(tab_cor,data.table(search=erreur_nom_com$COMMUNE,replaceBy=erreur_nom_com$NOM_COMM,restrict="",fieldRestrict="",stricly=TRUE,comment=""))
    } # END IF ELSE answer

    cat("\nLes corrections ajoutées au fichier de correctif des communes:\n")
    print(tab_cor)
    cat(" -->",file_cor)
    write.table(tab_cor,file_cor,sep=";",row.names=FALSE,col.names=FALSE,append=TRUE)
    cat(" DONE !\n")
}



prepa_file_region <- function() {

    reg <- data.table(read.csv2("library/anciennes-nouvelles-regions.csv",encoding="UTF-8"))
    reg <- reg[,REGION := toupper(Nouveau.nom.normalisé)]
    reg$REGION <- gsub(" ","-",reg$REGION)

    reg <- reg[,REGION_ANCIEN:= toupper(Ancien.nom.normalisé)]
    reg$REGION_ANCIEN<- gsub(" ","-",reg$REGION_ANCIEN)

    colnames(reg)[c(1,3)] <- c("CODE_REG","CODE_REG_ANCIEN")
    reg <- unique(reg[,c(1,8,3,9)])
    fwrite(reg,"library/region.csv")
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @return
##' @author
##' @param data
##' @param file_cor
##' @param file_shape_commune
##' @param file_out
##' @param file_out_error
##' @param output
add_localite <- function(data="data/data_2019-06-06_clean.csv",
                         file_cor="library/correctifs_communes.csv",
                         file_shape_commune = "library/CommunesCentr_L93.shp",
                         save=TRUE,repOut="data/",
                         file_out=NULL,
                         suffix_orphelin="orphelin",
                         suffix_orphelin_unique="orphelin_uniquev",output=FALSE ){

                                        #  file_cor="library/correctifs_communes.csv"; file_shape_commune = "library/CommunesCentr_L93.shp"; file_out="output/data_2019-06-06_clean_loc.csv";  file_out_orphelin="data/data_2019-06-06_orphelin.csv";   file_out_orphelin_unique="data/data_2019-06-06_orphelin_unique.csv"

    library(sf)
    library(data.table)
    library(dplyr)

    data <- my_import_fread(data,"ex: fichier_capt_mai2018_analysesbis_clean.csv")

    com_shp <- st_read(file_shape_commune)

    print(head(com_shp))

    com_shp_centroid <- st_centroid(com_shp)
    com_shp <- st_transform(com_shp_centroid,crs=4326)
    print(head(com_shp))

    com_table <- data.table(cbind(st_drop_geometry(com_shp),st_coordinates(com_shp)))
    print(head(com_table))
    com_table$X_CENTROID <-  com_table$X
    com_table$Y_CENTROID <-  com_table$Y
    col_com_table <- c("NOM_COMM","INSEE_COM","NOM_DEPT","CODE_DEPT","NOM_REGION","CODE_REG","X_CENTROID","Y_CENTROID","Z_MOYEN")

    com_table <- com_table[,..col_com_table]
    colnames(com_table)[c(1:3,5,6)] <- c("COMMUNE","INSEE","DEPARTEMENT","REGION_ANCIEN","CODE_REG_ANCIEN")

    com_table$COMMUNE <- as.character(com_table$COMMUNE)
    com_table$DEPARTEMENT <- as.character(com_table$DEPARTEMENT)
    com_table$CODE_DEPT <- as.character(com_table$CODE_DEPT)
    com_table$REGION_ANCIEN<- as.character(com_table$REGION_ANCIEN)
    com_table$REGION_ANCIEN<- gsub(" ","-",com_table$REGION_ANCIEN)
    reg <- fread("library/region.csv")
    reg <- reg[,c(1,2,4)]
    com_table <- data.table(left_join(com_table,reg))
    col_com_table <- c("COMMUNE","INSEE","DEPARTEMENT","CODE_DEPT","REGION","CODE_REG","REGION_ANCIEN","CODE_REG_ANCIEN","X_CENTROID","Y_CENTROID","Z_MOYEN")
    com_table <- com_table[,..col_com_table]


    colloc <- c("pk_data","COMMUNE","INSEE","DEPARTEMENT","DEPARTEMENT2","REGION")
    dataloc <- data[,..colloc]
    colnames(dataloc)[5] <- "CODE_DEPT"
    dataloc_orphelin <- NULL

    newcol <- c("pk_data",colnames(com_table))

    dataloc$INSEE <- ifelse(is.na(dataloc$INSEE),NA,sprintf("%05d", dataloc$INSEE))
    dataloc$CODE_DEPT<- ifelse(is.na(dataloc$CODE_DEPT),NA,sprintf("%02d", dataloc$CODE_DEPT))
    dataloc$REGION <- toupper(dataloc$REGION)
    dataloc$REGION <- gsub(" ","-",dataloc$REGION)

    dataloc$INSEE[dataloc$INSEE==""] <- NA
    dataloc$REGION[dataloc$REGION==""] <- NA
    dataloc$COMMUNE[dataloc$COMMUNE==""] <- NA
    dataloc$DEPARTEMENT[dataloc$DEPARTEMENT==""] <- NA
    dataloc$DEPARTEMENT2[dataloc$DEPARTEMENT2==""] <- NA

    cat(nrow(dataloc),"données à georéférencer\n")

    ## INSEE
    cat("  1) Recherche des localisation par le code INSEE\n")
    dataloc_insee <- subset(dataloc,!is.na(INSEE),select=c("pk_data","COMMUNE","INSEE"))

    cat(nrow(dataloc_insee),"données avec un code INSEE\n")

    com_table2 <- com_table
    com_table2$INSEE <- as.character(com_table2$INSEE)
    colnames(com_table2)[1] <- "NOM_COMM"

    dataloc_insee <- left_join(dataloc_insee,com_table2,by="INSEE")
    dataloc_insee_inconnu <- subset(dataloc_insee,is.na(NOM_COMM))
    if(nrow(dataloc_insee_inconnu)>0) {
        cat("ATTENTION",nrow(dataloc_insee_inconnu),"code(s) INSEE inconnu(s):\n")
        cat(dataloc_insee_inconnu[,"INSEE"])
        cat("\n")
        dataloc_orphelin <- rbind(dataloc_orphelin,dataloc_insee_inconnu[,..newcol])
    }

    dataloc_insee <- subset(dataloc_insee,!is.na(NOM_COMM))
    dataloc_insee_erreur <- subset(dataloc_insee,COMMUNE != NOM_COMM)
    if(nrow(dataloc_insee_erreur)) {
        update_correctif_file()
        cat("\n\n!!Fonction add_localite() relancée !!\n\n")
        add_localite()
    }# END if(nrow(dataloc_insee_erreur))


    cat("  ==>",nrow(dataloc_insee),"données geo-référencée par le code INSEE\n")
    dataloc_new <- dataloc_insee[,newcol]

    dataloc <- subset(dataloc,!(pk_data %in% dataloc_new$pk_data))
    cat("Reste",nrow(dataloc),"localisation(s) à traiter\n")

    ## DEPARTEMENT + COMMUNE
    cat("  2) Recherche des localisation par le DEPARTEMENT\n")
    dataloc_dep <- subset(dataloc,!is.na(DEPARTEMENT) & !is.na(COMMUNE),select=c("pk_data","COMMUNE","DEPARTEMENT"))
    cat(nrow(dataloc_dep),"données avec un DEPARTEMENT\n")

    dataloc_dep <- left_join(dataloc_dep,com_table,by=c("DEPARTEMENT","COMMUNE"))

    dataloc_dep_inconnu <- subset(dataloc_dep,is.na(INSEE))
    if(nrow(dataloc_dep_inconnu)>0) {
        uniquedata <-data.table(unique(dataloc_dep_inconnu[,c("COMMUNE","DEPARTEMENT")]))
        cat("ATTENTION",nrow(uniquedata)," association(s) COMMUNE DEPARTEMENT inconnue(s) affectant",nrow(dataloc_dep_inconnu),"données:\n")
        print(uniquedata)

        dataloc_orphelin <- rbind(dataloc_orphelin,dataloc_dep_inconnu[,newcol])
    }

    cat("  ==>",nrow(dataloc_dep),"données geo-référencée par l'association COMMUNE DEPARTEMENT\n")
    dataloc_new <- rbind(dataloc_new,dataloc_dep[,newcol])

    dataloc <- subset(dataloc,!(pk_data %in% dataloc_new$pk_data))
    cat("Reste",nrow(dataloc),"localisation(s) à traiter\n")


    ## DEPARTEMENT2 + COMMUNE

    cat("  3) Recherche des localisation par le code CODE_DEPT\n")
    dataloc_codedep <- subset(dataloc,!is.na(CODE_DEPT) & !is.na(COMMUNE),select=c("pk_data","COMMUNE","CODE_DEPT"))
    cat(nrow(dataloc_codedep),"données avec un CODE_DEPT\n")

    dataloc_codedep <- left_join(dataloc_codedep,com_table,by=c("CODE_DEPT","COMMUNE"))

    dataloc_codedep_inconnu <- subset(dataloc_codedep,is.na(INSEE))
    if(nrow(dataloc_codedep_inconnu)>0) {
        uniquedata <-data.table(unique(dataloc_codedep_inconnu[,c("COMMUNE","CODE_DEPT")]))
        cat("ATTENTION",nrow(uniquedata)," association(s) COMMUNE CODE_DEPT inconnue(s) affectant",nrow(dataloc_codedep_inconnu),"données:\n")
        print(uniquedata)

        dataloc_orphelin <- rbind(dataloc_orphelin,dataloc_codedep_inconnu[,newcol])
    }

    cat("  ==>",nrow(dataloc_codedep),"données geo-référencée par l'association COMMUNE CODE_DEPT\n")
    dataloc_new <- rbind(dataloc_new,dataloc_codedep[,newcol])

    dataloc <- subset(dataloc,!(pk_data %in% dataloc_new$pk_data))
    cat("Reste",nrow(dataloc),"localisation(s) à traiter\n")



    ## REGION + COMMUNE
    cat("  4) Recherche des localisation par le code REGION\n")
    dataloc_region <- subset(dataloc,!is.na(REGION) & !is.na(COMMUNE),select=c("pk_data","COMMUNE","REGION"))
    cat(nrow(dataloc_region),"données avec une REGION\n")

    dataloc_region <- left_join(dataloc_region,com_table,by=c("REGION","COMMUNE"))

    dataloc_region_inconnu <- subset(dataloc_region,is.na(INSEE))
    if(nrow(dataloc_region_inconnu)>0) {
        uniquedata <-data.table(unique(dataloc_region_inconnu[,c("COMMUNE","REGION")]))
        cat("ATTENTION",nrow(uniquedata)," association(s) COMMUNE REGION inconnue(s) affectant",nrow(dataloc_region_inconnu),"données:\n")
        print(uniquedata)

        dataloc_region <- subset(dataloc_region,!is.na(INSEE))
    }


    doublon_region <- subset(as.data.frame(table(dataloc_region$pk_data)),Freq > 1)$Var1
    if(length(doublon_region)>0) {
        dataDoublon_region <- subset(dataloc_region,pk_data %in% doublon_region)

        communeDoublon_region <- data.table(unique(dataDoublon_region[,-1]))[,c(1:4,7)]
        communeDoublon <- unique(dataDoublon_region$COMMUNE)
        dataDoublon <-  unique(dataDoublon_region$pk_data)

        cat("ATTENTION",length(dataDoublon), "doublon(s)\n les pk_data:\n",dataDoublon,"\n")
        cat(length(communeDoublon),"commune(s) concernée(s):\n",communeDoublon,"\n")

        print(communeDoublon_region)

        cat("Pour ces data aucunes communes n'est affectées\n")

        dataloc_region <- subset(dataloc_region,!(pk_data %in% dataDoublon))
        dataloc_orphelin <- rbind(dataloc_orphelin,subset(dataloc_region,(pk_data %in% dataDoublon))[,newcol])
    }



    cat("  ==>",nrow(dataloc_region),"données geo-référencée par l'association COMMUNE REGION\n")
    dataloc_new <- rbind(dataloc_new,dataloc_region[,newcol])

    dataloc <- subset(dataloc,!(pk_data %in% dataloc_new$pk_data))
    cat("Reste",nrow(dataloc),"localisation(s) à traiter\n")


    ## REGION_ANCIEN + COMMUNE
    cat("  5) Recherche des localisation par la REGION ancienne\n")
    dataloc_region_ancien <- subset(dataloc,!is.na(REGION) & !is.na(COMMUNE),select=c("pk_data","COMMUNE","REGION"))
    cat(nrow(dataloc_region_ancien),"données encore avec une REGION \nnous les concidérons maintenant comme les noms des anciennes regions\n")
    colnames(dataloc_region_ancien)[3] <- "REGION_ANCIEN"
    dataloc_region_ancien <- left_join(dataloc_region_ancien,com_table,by=c("REGION_ANCIEN","COMMUNE"))

    dataloc_region_ancien_inconnu <- subset(dataloc_region_ancien,is.na(INSEE))
    if(nrow(dataloc_region_ancien_inconnu)>0) {
        uniquedata <-data.table(unique(dataloc_region_ancien_inconnu[,c("COMMUNE","REGION_ANCIEN")]))
        cat("ATTENTION",nrow(uniquedata)," association(s) COMMUNE REGION_ANCIEN inconnue(s) affectant",nrow(dataloc_region_ancien_inconnu),"données:\n")
        print(uniquedata)

        dataloc_orphelin <- rbind(dataloc_orphelin,dataloc_region_ancien_inconnu[,newcol])
    }


    doublon_region_ancien <- subset(as.data.frame(table(dataloc_region_ancien$pk_data)),Freq > 1)$Var1
    if(length(doublon_region_ancien)>0) {
        dataDoublon_region_ancien <- subset(dataloc_region_ancien,pk_data %in% doublon_region_ancien)

        communeDoublon_region_ancien <- data.table(unique(dataDoublon_region_ancien[,-1]))[,c(1:4,7)]
        communeDoublon <- unique(dataDoublon_region_ancien$COMMUNE)
        dataDoublon <-  unique(dataDoublon_region_ancien$pk_data)

        cat("ATTENTION",length(dataDoublon), "doublon(s)\n les pk_data:\n",dataDoublon,"\n")
        cat(length(communeDoublon),"commune(s) concernée(s):\n",communeDoublon,"\n")

        print(communeDoublon_region_ancien)

        cat("Pour ces data aucunes communes n'est affectées\n")

        dataloc_region_ancien <- subset(dataloc_region_ancien,!(pk_data %in% dataDoublon))
        dataloc_orphelin <- rbind(dataloc_orphelin,subset(dataloc_region_ancien,(pk_data %in% dataDoublon))[,newcol])
    }



    cat("  ==>",nrow(dataloc_region_ancien),"données geo-référencée par l'association COMMUNE REGION_ANCIEN\n")
    dataloc_new <- rbind(dataloc_new,dataloc_region_ancien[,newcol])

    dataloc <- subset(dataloc,!(pk_data %in% dataloc_new$pk_data))
    cat("Reste",nrow(dataloc),"localisation(s) à traiter\n")


    ##  COMMUNE seule
    cat("  5) Recherche des localisation par uniquement le nom ce COMMUNE\n")
    dataloc_commune <- subset(dataloc, !is.na(COMMUNE),select=c("pk_data","COMMUNE"))
    cat(nrow(dataloc_commune),"données avec une COMMUNE\n")

    dataloc_commune <- left_join(dataloc_commune,com_table,by=c("COMMUNE"))

    dataloc_commune_inconnu <- subset(dataloc_commune,is.na(INSEE))
    if(nrow(dataloc_commune_inconnu)>0) {
        uniquedata <-unique(dataloc_commune_inconnu[,c("COMMUNE")])
        cat("ATTENTION",length(uniquedata)," COMMUNE(s) inconnue(s) affectant",nrow(dataloc_commune_inconnu),"données:\n")
        cat(uniquedata,"\n")

        dataloc_orphelin <- rbind(dataloc_orphelin,dataloc_commune_inconnu[,newcol])
    }


    doublon_commune <- subset(as.data.frame(table(dataloc_commune$pk_data)),Freq > 1)$Var1
    if(length(doublon_commune)>0) {
        dataDoublon_commune <- subset(dataloc_commune,pk_data %in% doublon_commune)

        communeDoublon_commune <- data.table(unique(dataDoublon_commune[,-1]))[,c(1:4,7)]
        communeDoublon <- unique(dataDoublon_commune$COMMUNE)
        dataDoublon <-  unique(dataDoublon_commune$pk_data)

        cat("ATTENTION",length(dataDoublon), "doublon(s)\n les pk_data:\n")

        if(length(dataDoublon) > 201) cat(dataDoublon[1:100],"...",tail(dataDoublon,100),"\n") else cat(dataDoublon,"\n")

        cat(length(communeDoublon),"commune(s) concernée(s):\n",communeDoublon,"\n")

        print(communeDoublon_commune)

        cat("Pour ces data aucunes communes n'est affectées\n")

        dataloc_commune <- subset(dataloc_commune,!(pk_data %in% dataDoublon))
        dataloc_orphelin <- rbind(dataloc_orphelin,subset(dataloc_commune,(pk_data %in% dataDoublon))[,newcol])
    }

    cat("  ==>",nrow(dataloc_commune),"données geo-référencée par la COMMUNE \n")
    dataloc_new <- rbind(dataloc_new,dataloc_commune[,newcol])

    dataloc <- subset(dataloc,!(pk_data %in% dataloc_new$pk_data))
    cat("Reste",nrow(dataloc),"localisation(s) à traiter\n")
    newcol <- c("pk_data",setdiff(colnames(data),colnames(dataloc_new)))
    data_new <- data[,..newcol]
    data_new <- left_join(data_new ,dataloc_new)


    dOrigine <- data[,c("pk_data","ORIGINE","OBSERVATEUR")]
    dataloc_orphelin <- inner_join(dataloc_orphelin,dOrigine)

    if(save) {
        library(tools)
        if(is.null(file_out)) file_out<- format(as.Date(Sys.time()),"%Y%m%d") else file_out <- file_path_sans_ext(file_out)


        file_out_orphelin <- paste0(repOut,file_out,"_",suffix_orphelin,".csv")
        file_out_orphelin_unique<- paste0(repOut,file_out,"_",suffix_orphelin_unique,".csv")
        file_out <- paste0(repOut,file_out,".csv")

        cat("\nEnregistrement des données avec erreur dans la localisation:\n")
        cat(" ==> ",file_out_orphelin)
        write.csv(dataloc_orphelin,file_out_orphelin,row.names=FALSE)
        cat("   DONE!\n")

        dataloc_orphelin_unique <- unique(dataloc_orphelin[,-1])
        dataloc_orphelin_unique <- dataloc_orphelin_unique[,c(12:13,1:11)]
        dataloc_orphelin_unique <- data.table( dataloc_orphelin_unique)
        dataloc_orphelin_unique <-  setorder(dataloc_orphelin_unique)



        cat("\nEnregistrement des ",nrow(dataloc_orphelin_unique)," localisations orphelines :\n",sep="")
        cat(" ==> ",file_out_orphelin_unique)
        write.csv(dataloc_orphelin_unique,file_out_orphelin_unique,row.names=FALSE)
        cat("   DONE!\n")




        cat("\nEnregistrement des données:\n")
        cat(" ==> ",file_out)
        write.csv(data_new,file_out,row.names=FALSE)
        cat("   DONE!\n")
    }
    if(output) return(data_new)
} #END add_localite



date_cleaning <- function(d="data/data_2019-06-06_clean_loc.csv",output=FALSE,save=TRUE,fileoutput=NULL) {
    ## d="data/data_2019-06-06_clean_loc.csv"
    library(data.table)
    d <- my_import_fread(d,"ex: fichier_capt_mai2018_analysesbis_clean.csv")

    d$DATE_POSIX <- as.POSIXct(paste(d$ANNEE,sprintf("%02d",d$MOIS),d$JOUR,sep="-"))
    d$HEURE[d$HEURE==""] <- NA
    d$heure2 <- ifelse(is.na(d$HEURE)|d$HEURE=="","12:12:12",d$HEURE)
    time <- paste(d$DATE_POSIX,d$heure2)
    time <-  as.POSIXct(time)
    d$TIME_POSIX <- time
    d$TIME_POSIX[is.na(d$HEURE)] <- NA
    if(save) {
        if(is.null(fileoutput))fileoutput<- paste0(format(as.Date(Sys.time()),"%Y%m%d"),"_clean_time.csv")
        cat("\n\n")
        cat("  -->", fileoutput)
        write.csv(d,fileoutput,row.names=FALSE)
        cat("   DONE!\n")
    }
    if(output)(return(d))
}


ajout_period <- function(d="data/data_2019-06-06_clean_loc.csv",output=FALSE,save=TRUE,fileoutput=NULL) {
    library(data.table)
    if(class(d)[1]=="character") d <- fread(d)

    j0 <- format(as.Date("2019-01-01"),"%j")
    j1 <- format(as.Date("2019-06-01"),"%j")
    j2 <- format(as.Date("2019-07-31"),"%j")
    j3 <- format(as.Date("2019-10-30"),"%j")
    j4 <- format(as.Date("2019-12-31"),"%j")


    vecBreak <- c(j0,j1,j2,j3,j4)
    vecLab <- c("0_precoce","1_juin_juillet","2_aout_octobre","3_tardif")

    d$periode <- cut(d$JOUR_ANNEE,vecBreak,vecLab)

    d$période_num <- as.numeric(substring(d$periode,1,1))

  if(save) {
        if(is.null(fileoutput))fileoutput<- paste0(format(as.Date(Sys.time()),"%Y%m%d"),"_clean_periode.csv")
        cat("\n\n")
        cat("  -->", fileoutput)
        write.csv(d,fileoutput,row.names=FALSE)
        cat("   DONE!\n")
    }
    if(output) return(d)

}


add_sunset_sunrise <- function(d="data/data_2019-06-06_clean_loc.csv",output=FALSE,save=saveStep,fileoutput=NULL) {
    library(maptools)
    library(lubridate)
    library(dplyr)
    library(data.table)
    d <- my_import_fread(d,"ex: data/data_2019-06-06_clean_loc.csv")
#browser()

    dd <- subset(d,!is.na(d$DATE)&!is.na(d$HEURE))
    cat(nrow(dd),"samples with valid date and time\n")

    dd$DATE_NIGHT_POSIX <- as.Date(dd$DATE_POSIX)-ifelse(as.numeric(substr(dd$HEURE,1,2))>12,0,1)
    dd$DATE_MORNING_POSIX <- as.Date(dd$DATE_POSIX)+ifelse(as.numeric(substr(dd$HEURE,1,2))>12,1,0)

    ddd <- subset(dd,is.na(X_CENTROID))
    dd <- subset(dd,!is.na(X_CENTROID))

    cat(nrow(dd),"samples with valid location\n")

    coordinates(dd) <- c("X_CENTROID", "Y_CENTROID")
    lonlat <- SpatialPoints(coordinates(dd),proj4string=CRS("+proj=longlat +datum=WGS84"))
    dd$sunrise <- sunriset(lonlat, as.POSIXct(dd$DATE_MORNING_POSIX), direction="sunrise", POSIXct=TRUE)$time
    dd$sunset <- sunriset(lonlat, as.POSIXct(dd$DATE_NIGHT_POSIX), direction="sunset", POSIXct=TRUE)$time
    dd$diff_sunset_heure <- as.numeric(difftime(as.POSIXlt(dd$TIME_POSIX),as.POSIXlt(dd$sunset),units="hours"))
    dd$night_duration_heure<- as.numeric(difftime(as.POSIXlt(dd$sunrise),as.POSIXlt(dd$sunset),units="hours"))


    keepedCol <- c("pk_data",setdiff(names(dd),names(d)))
    dd <- as.data.table(dd)
    dd <- dd[, ..keepedCol]
    d <- full_join(d,dd)

    d$diff_sunset_heure_valide <- d$diff_sunset_heure >= -1 & d$diff_sunset_heure <= 5
    d$diff_sunset_heure_valide[is.na(d$diff_sunset_heure_valide)] <- FALSE
    if(save) {
        if(is.null(fileoutput))fileoutput<- paste0(format(as.Date(Sys.time()),"%Y%m%d"),"_clean_loc_sunset.csv")
        cat("\n\n")
        cat("  -->", fileoutput)
        write.csv(d,fileoutput,row.names=FALSE)
        cat("   DONE!\n")
    }

    if(output) return(d)
}



add_weather <- function(d="data/data_2019-06-06_clean_loc_sunset.csv",output=FALSE,save=saveStep,repout=NULL,fileoutput=NULL,fileoutput_weather=NULL) {

                                        #d="data/data_2019-06-06_clean_loc.csv";output=FALSE
    library(data.table)
    if(class(d)[1]=="character") d <- fread(d)

##    FTempMean="library/tg_0.25deg_reg_2011-2017_v17.0_2.nc"
##
##   ## nc <- create.nc(FTempMean)
##   load("library/temp.Rdata")
##   TempMean <- temp
##
##                                       #  TempMean <- temporal_mean(temp,time_avg="winow",win_length=1)

    dsample <- unique(subset(d,ANNEE>2011&ANNEE<2018 & !(is.na(X_CENTROID)),select=c("INSEE","X_CENTROID","Y_CENTROID","DATE_NIGHT_POSIX")))



    weather <- get_sample_weather(dsample,nc_data=weatherRdata_file,dsample_colnames=c("site_id"="INSEE","date"="DATE_NIGHT_POSIX","longitude"="X_CENTROID","latitude"="Y_CENTROID"))


##    dsample$id_sample <- 1:nrow(dsample)
##    dsample$longitude <- (floor(dsample$X_CENTROID*4)/4)+0.125
##    dsample$latitude <- (floor(dsample$Y_CENTROID*4)/4)+0.125
##
##    dsite <- unique(dsample[,c("INSEE","longitude","latitude")])
##    dsite$site_id <- 1:nrow(dsite)
##
##    dsiteREF <- dsite[,c("site_id","INSEE")]
##    dsite <- dsite[,c("site_id","longitude","latitude")]
##
##    point.TM <- point_grid_extract(temp,dsite)
##
##    Jour=yday(point.TM$date_extract)
##
##    weather <- data.frame(matrix(NA,nrow(dsample),5))
##    colnames(weather) <- c("AT1","AT3","AT9","AT27","AT81")
##    weather <- cbind(dsample,weather)
##
##    for (i in 1:nrow(dsample)) {
##                                        # cat(i,"")
##        if (i%%100==1){print(paste(i,Sys.time()))}
##        MatchLongLat=match(paste(dsample$longitude[i],dsample$latitude[i]),paste(dsite$longitude,dsite$latitude))
##        MatchDate=match(dsample$DATE_POSIX[i],as.character(point.TM$date_extract))
##        T1=point.TM[MatchDate,(MatchLongLat+1)]
##        D1=point.TM$date_extract[MatchDate]
##        J1=yday(D1)
##        J1_30=match(Jour,J1)
##        N1=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J1_30)))
##        weather$AT1[i]=T1-N1
##
##        T3=mean(point.TM[(MatchDate-2):MatchDate,(MatchLongLat+1)])
##        J3=c((J1-2):J1) #last 3 days
##        J3=J3-floor((J3-1)/365)*365 #to keep in 1:365 domain
##        J3_30=match(Jour,J3)
##        N3=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J3_30)))
##        weather$AT3[i]=T3-N3
##
##        T9=mean(point.TM[(MatchDate-8):MatchDate,(MatchLongLat+1)])
##        J9=c((J1-8):J1) #last 9 days
##        J9=J9-floor((J9-1)/365)*365 #to keep in 1:365 domain
##        J9_30=match(Jour,J9)
##        N9=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J9_30)))
##        weather$AT9[i]=T9-N9
##
##        T27=mean(point.TM[(MatchDate-26):MatchDate,(MatchLongLat+1)])
##        J27=c((J1-26):J1) #last 9 days
##        J27=J27-floor((J27-1)/365)*365 #to keep in 1:365 domain
##        J27_30=match(Jour,J27)
##        N27=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J27_30)))
##        weather$AT27[i]=T27-N27
##
##        T81=mean(point.TM[(MatchDate-80):MatchDate,(MatchLongLat+1)])
##        J81=c((J1-80):J1) #last 9 days
##        J81=J81-floor((J81-1)/365)*365 #to keep in 1:365 domain
##        J81_30=match(Jour,J81)
##        N81=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J81_30)))
##        weather$AT81[i]=T81-N81
##    }
##

      if(save) {
          if(is.null(fileoutput))fileoutput<- paste0(format(as.Date(Sys.time()),"%Y%m%d"),"_clean_loc_weather.csv")
          if(is.null(fileoutput_weather))fileoutput_weather<- paste0(format(as.Date(Sys.time()),"%Y%m%d"),"_sample_weather.csv")
          fileoutput <- paste0(repout,fileoutput)
          fileoutput_weather <- paste0(repout,fileoutput_weather)

          cat("\n\n")
          cat("  -->", fileoutput_weather)
          write.csv(weather,fileoutput_weather,row.name=FALSE)
          cat("   DONE!\n")


          cat("\n\n")
          cat("  -->", fileoutput)
          write.csv(d,fileoutput,row.names=FALSE)
          cat("   DONE!\n")
      }


    if(output) return(d)





}



add_orderOcc <- function(d="data/data_2019-06-06_clean_loc_sunset.csv",output=FALSE) {

    d="data/data_2019-06-06_clean_loc_sunset.csv";output=FALSE
    library(data.table)
    library(dplyr)
    if(class(d)[1]=="character") d <- fread(d)

    d$AGE2 <- ifelse(is.na(d$AGE),"U",ifelse(d$AGE %in% c("JUVENILE","IMMATURE"),"JUV","AD"))
    d$SEXE[is.na(d$SEXE)] <- "U"

    dd <- d[,c("pk_data","INSEE","DATE_NIGHT_POSIX","periode","diff_sunset_heure","TAXON","SEXE","AGE2")]
    dd <- dd[!(is.na(diff_sunset_heure)),,]
    dd <- dd[order(INSEE,DATE_NIGHT_POSIX,diff_sunset_heure,TAXON)]

    dd <- dd[,orderIndSp:= 1:.N,by=.(INSEE,DATE_NIGHT_POSIX,periode,TAXON)]

    ddmax <- aggregate(orderIndSp ~INSEE+DATE_NIGHT_POSIX+TAXON, dd,max)
    colnames(ddmax)[ncol(ddmax)] <- "ntotSp"

    dd <- inner_join(dd,ddmax)

    dd$propOrderIndSp <- dd$orderIndSp/dd$ntotSp


    dd <- inner_join(dd,dw)

    dd5 <- data.table(subset(dd,ntotSp>5))
    dd5 <- dd5[,proporderIndSp_med := propOrderIndSp>0.5,by=.(INSEE,DATE_NIGHT_POSIX,periode,TAXON)]
    dd5 <- dd5[,proporderIndSp_med := ropOrderIndSp>0.5,by=.(INSEE,DATE_NIGHT_POSIX,periode,TAXON)]

    dd5med <- aggregate(diff_sunset_heure~INSEE+DATE_NIGHT_POSIX+periode+TAXON,dd5[propOrderIndSp>0.5,,],min)
    dd5med$variable <- "mediane"
    dd5ICinf50 <- aggregate(diff_sunset_heure~INSEE+DATE_NIGHT_POSIX+periode+TAXON,dd5[propOrderIndSp>0.25,,],min)
    dd5ICinf50$variable <- "ICinf50"
    dd5ICsup50 <- aggregate(diff_sunset_heure~INSEE+DATE_NIGHT_POSIX+periode+TAXON,dd5[propOrderIndSp>0.75,,],min)
    dd5ICsup50$variable <- "ICsup50"

    dd5gg <- rbind(dd5med,rbind(dd5ICinf50,dd5ICsup50))
    dd5gg <- inner_join(dd5gg,dw)

    dd5gg1 <- dd5gg[,c("INSEE","DATE_NIGHT_POSIX","periode","TAXON","diff_sunset_heure","variable","AT1")]
    colnames(dd5gg1)[ncol(dd5gg1)] <- "anomalie_temp"
    dd5gg1$window <- "1 jour"

    dd5gg1 <- dd5gg[,c("INSEE","DATE_NIGHT_POSIX","periode","TAXON","diff_sunset_heure","variable","AT1")]
    colnames(dd5gg1)[ncol(dd5gg1)] <- "anomalie_temp"
    dd5gg1$window <- "1 jour"


    dd5gg3 <- dd5gg[,c("INSEE","DATE_NIGHT_POSIX","periode","TAXON","diff_sunset_heure","variable","AT3")]
    colnames(dd5gg3)[ncol(dd5gg3)] <- "anomalie_temp"
    dd5gg3$window <- "3 jours"


    dd5gg9 <- dd5gg[,c("INSEE","DATE_NIGHT_POSIX","periode","TAXON","diff_sunset_heure","variable","AT9")]
    colnames(dd5gg9)[ncol(dd5gg9)] <- "anomalie_temp"
    dd5gg9$window <- "9 jours"

    dd5ggBis <- rbind(dd5gg1,rbind(dd5gg3,dd5gg9))

    vecSp <- unique(dd5gg$TAXON)
    vecPeriode <- c("1_juin_juillet","2_aout_octobre")
    for(sp in vecSp)  {
                                        #sp <- "Pipistrellus pipistrellus"
        dd5gg_sp <- subset(dd5ggBis,TAXON==sp & periode %in% vecPeriode)
        gg <- ggplot(dd5gg_sp,aes(x=anomalie_temp,y=diff_sunset_heure,group=variable,colour=variable)) + geom_smooth(se=FALSE)+facet_grid(periode~window,scales="free_y")
        gg <- gg + labs(title=sp,x="Anomalie de température",y="Heure après le coucher du soleil",colour="")
        gg
        ggfile <- paste0("output/pheno_temp/pheno_temp_",sp,".png")
        cat("plot -> ",ggfile,"\n")
        ggsave(ggfile,gg)

    } #END for(sp in vecSp)

}




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title get weather anomalie form sample data
##' @param dsample data or data path or NULL
##' @param first_year if NULL first year of data sample
##' @param last_year  if NULL last year of data sample
##' @param nc_local TRUE to extract value in local file
##' @param nc_extract TRUE if you want use the extract_nc_value function
##' @param nc_data  path of the Rdata of the weather data
##' @param var weather variables
##' @param dsample_colnames names of important columns in sample table defaut c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84")
##' @param output boolean export table in console
##' @param save boolean save or not the table
##' @param fileouput name of file to save the table
##' @return la table sample avec les anomalies des variables météo désiré à 1,3,9,27 et 81 jours
##' @author Romain Lorrilliere à partir d'un code de Yves Bas
get_sample_weather <- function(dsample=NULL,first_year=NULL,last_year=NULL,nc_local=TRUE,nc_extract=FALSE,nc_data=NULL,var=c("precipitation","mean_temp"),dsample_colnames=c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84"),output=TRUE,save=FALSE,fileouput=NULL) {

    library(data.table)
    library(RNetCDF)
    library(climateExtract) #https://github.com/RetoSchmucki/climateExtract
    library(ncdf4)

    ##    dsample=NULL;first_year=NULL;last_year=NULL;nc_local=TRUE;nc_extract=FALSE;nc_data=NULL;var=c("precipitation","mean_temp");dsample_colnames=c("site_id"="id_site","date"="date","longitude"="longitude_wgs84","latitude"="latitudeWGS84");output=TRUE;save=FALSE;fileoutput=NULL

    if (is.null(dsample)) {
        print("select your sample file")
        dsample <- fread(file.choose())
    } else {
        if(class(dsample)[1]=="character") dsample <- fread(dsample) else dsample <- data.table(dsample)
    }


    dsample_colnames2 <- setdiff(names(dsample_colnames),colnames(dsample))
    if(length(dsample_colnames2)>0) for(n in dsample_colnames2) dsample[,n] <-  dsample[,dsample_colnames[names(dsample_colnames)==n],with=FALSE]

    dsample$year <- format(as.Date(dsample$date),"%Y")

    if(is.null(first_year)) first_year <- min(dsample$year)
    if(is.null(last_year)) last_year <- max(dsample$year)

    lnc <- list()
    if(nc_local) {
        if(nc_extract) {
            for(v in var){
                cat(" - Variable:",v,"\n-----------------------------\n\n")
                lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = TRUE)
            } # END for(v in var){
        } else { # ELSE  if(nc_extract)
            if(is.null(nc_data)){
                print("select your nc file at Rdata format")
                ## provient de :
                ## precipitation <- extract_nc_value(2014,2019) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
                ## mean_temp <- extract_nc_value(2014,2019) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
                ## save(list=c("precipitation","mean_temp"),file="XXX.Rdata")
                load(file.choose())
            } else { # ELSE  if(is.null(lnc))
                load(nc_data)
            }# END ELSE  if(is.null(lnc))
            for(v in var)
                lnc[[v]] <-  get(v)
        } # END  ELSE  if(nc_extract)
    } else { # ELSE if(dnc_local)
        for(v in var){
            cat(" - Variable:",v,"\n-----------------------------\n\n")
            lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = FALSE, clim_variable = v, grid_size = 0.25)
        } # END for(v in var){
    }# END ELSE if(dnc_local)

    dsample$longitudeRAW <- dsample$longitude
    dsample$latitudeRAW <-  dsample$latitude
    dsample$longitude <- (floor(dsample$longitude*4)/4)+0.125
    dsample$latitude <-  (floor(dsample$latitude*4)/4)+0.125

    dsite <- unique(dsample[,c("site_id","longitude","latitude")])

    for(l in 1:length(lnc)) {

        laVar <- names(lnc)[l]
        cat("Variable météo:",laVar,"\n--------------------------------\n\n")

        point.TM <- point_grid_extract(lnc[[l]],dsite)

        Jour=yday(point.TM$date_extract)

        weather <- data.frame(matrix(NA,nrow(dsample),5))
        colnames(weather) <- paste(laVar,c("A1","A3","A9","A27","A81"),sep="_")

        for (i in 1:nrow(dsample)) {
                                        # cat(i,"")
            if (i%%100==1){print(paste(i,Sys.time()))}
            MatchLongLat=match(paste(dsample$longitude[i],dsample$latitude[i]),paste(dsite$longitude,dsite$latitude))
            MatchDate=match(dsample$date[i],as.character(point.TM$date_extract))
            T1=point.TM[MatchDate,(MatchLongLat+1)]
            D1=point.TM$date_extract[MatchDate]
            J1=yday(D1)
            J1_30=match(Jour,J1)
            N1=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J1_30)))
            weather$AT1[i]=T1-N1

            browser()
            T3=mean(point.TM[(MatchDate-2):MatchDate,(MatchLongLat+1)])
            J3=c((J1-2):J1) #last 3 days
            J3=J3-floor((J3-1)/365)*365 #to keep in 1:365 domain
            J3_30=match(Jour,J3)
            N3=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J3_30)))
            weather$AT3[i]=T3-N3

            T9=mean(point.TM[(MatchDate-8):MatchDate,(MatchLongLat+1)])
            J9=c((J1-8):J1) #last 9 days
            J9=J9-floor((J9-1)/365)*365 #to keep in 1:365 domain
            J9_30=match(Jour,J9)
            N9=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J9_30)))
            weather$AT9[i]=T9-N9

            T27=mean(point.TM[(MatchDate-26):MatchDate,(MatchLongLat+1)])
            J27=c((J1-26):J1) #last 27 days
            J27=J27-floor((J27-1)/365)*365 #to keep in 1:365 domain
            J27_30=match(Jour,J27)
            N27=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J27_30)))
            weather$AT27[i]=T27-N27

            T81=mean(point.TM[(MatchDate-80):MatchDate,(MatchLongLat+1)])
            J81=c((J1-80):J1) #last 81 days
            J81=J81-floor((J81-1)/365)*365 #to keep in 1:365 domain
            J81_30=match(Jour,J81)
            N81=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J81_30)))
            weather$AT81[i]=T81-N81
        } #END for (i in 1:nrow(dsample))
        dsample <- cbind(dsample,weather)
    } # END for(l in 1:length(lnc))

    if(save) fwrite(dsample,fileouput)


    if(output) return(dsample)

}



prepare_weatherRdata <- function(firstYear=1950,lastYear=NULL,repOut="data/") {
    if(is.null(lastYear)) lastYear <- as.numeric(format(Sys.time(),"%Y"))
    vecAn_start <- seq(firstYear,lastYear,5)
    vecAn_end <- sort(union(seq(firstYear+4,lastYear,5),lastYear))
    dAn <- data.frame(start=vecAn_start,end=vecAn_end)
    dAn$filename <- paste0(repOut,"data_meteo_temp_prec_ens_mean_0.25deg_reg_v20_",dAn$start,"-",dAn$end,".Rdata")
    print(dAn)
    flush.console()

    for(i in 1:nrow(dAn)) {
        cat("\n",paste(dAn[i,],collapse=" | "),"\n\n")
        flush.console()
        cat(" - precipitation: avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc\n")
        flush.console()
        precipitation <- extract_nc_value(dAn$start[i],dAn$end[i]) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
        cat(" - mean_temp : avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc\n")
        flush.console()
        mean_temp <- extract_nc_value(dAn$start[i],dAn$end[i]) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
        file <- dAn$filename[i]
        cat("SAVE ->",file)
        flush.console()
        save(list=c("precipitation","mean_temp"),file=file)
        cat("   DONE!\n\n")
    }

    write.csv(dAn,paste0(repOut,"table_weather_Rdata_names.csv"))
}
