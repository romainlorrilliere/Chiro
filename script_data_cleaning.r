

require(dplyr)

summary_data_morpho <- function(d) {


}




dataCleaning <- function(d=NULL,dfield=NULL,id=NULL,output="data") {
    library(dplyr)

  #  d <- NULL ;    dfield <- NULL;    id <- NULL


    if(is.null(d)) d <- read.delim("data/fichier_capt_mai2018_analysesbis.csv",sep=";",dec=".",stringsAsFactors=FALSE)

    d <- data.frame(pk_data=1:nrow(d),d,error_data="",stringsAsFactors=FALSE)

    if(is.null(dfield)) dfield <- read.csv2("library/capture_column.csv",stringsAsFactors=FALSE)

    if(is.null(id)) id <- Sys.Date()

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
    prefix_file <- paste("output/data_",id,sep="")
    file_data <- paste(prefix_file,"_clean.csv",sep="")
    file_error <- paste(prefix_file,"_error.csv",sep="")
    cat("\n\n")
    cat("  -->", file_data)
    write.csv(d,file_data,row.names=FALSE)
    cat("   DONE !\n")
    cat("  -->", file_error)
    write.csv(d_error,file_error,row.names=FALSE)
    cat("   DONE !\n")


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
commune_corrections <- function(data,com_cor=NULL) {
    ## Pour deboguage ------
    data=NULL;com_cor=NULL
    ## ----------------------
    library(data.table)
    if(is.null(data)) data <- fread("output/data_2019-06-06_clean.csv") else if(class(data)[1] == "character") data <- fread(data)
    if(is.null(com_cor)) com_cor<- fread("library/correctifs_communes.csv") else if(class(com_cor)[1] == "character") com_cor<- fread(com_cor)

    data <- data.table(data)

    data$COMMUNUE <- toupper(data$COMMUNE)


    com_cor<- fread("library/correctifs_communes.csv")
    com_cor2<- read.table("library/correctifs_communes.csv",sep=";")

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
        ## if(i==3) browser()
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
    return(data)
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
add_localite <- function(data="output/data_2019-06-06_clean.csv",file_cor="library/correctifs_communes.csv",
                         file_shape_commune = "library/CommunesCentr.shp",
                         file_out="output/data_2019-06-06_clean_loc.csv",
                         file_out_orphelin="data/data_2019-06-06_orphelin.csv",output=TRUE ){

    ## file_cor="library/correctifs_communes.csv"

    library(sf)
    library(data.table)
    library(dplyr)

    if(class(data)[1]=="character") data <- fread(data)

    data <- commune_corrections(data,file_cor)

    com_shp <- st_read(file_shape_commune)
    com_table <- data.table(st_drop_geometry(com_shp))
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

        dataloc_orphelin <- rbind(dataloc_orphelin,dataloc_region_inconnu[,newcol])
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

    cat("\nEnregistrement des données avec erreur dans la localisation:\n")
    cat(" ==> ",file_out_orphelin)
    write.csv2(dataloc_orphelin,file_out_orphelin,row.names=FALSE)
    cat("   DONE!\n")

    cat("\nEnregistrement des données:\n")
    cat(" ==> ",file_out)
    write.csv2(data_new,file_out,row.names=FALSE)
    cat("   DONE!\n")

 if(output) return(data_new)
} #END add_localite

