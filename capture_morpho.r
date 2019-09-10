

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
    ##data=NULL;com_cor=NULL
    ## ----------------------
    library(data.table)
    if(is.null(data)) data <- fread("output/data_2019-06-06_clean.csv") else if(class(data)[1] == "character") data <- fread(data)
    if(is.null(com_cor)) com_cor<- fread("library/correctifs_communes.csv") else if(class(com_cor)[1] == "character") com_cor<- fread(com_cor)

    data <- data.table(data)

    data$COMMUNUE <- toupper(data$COMMUNE)

    cat("Correctifs des noms de communes (",nrow(com_cor)," correctifs)\n\n",sep="")

    for(i in 1:nrow(com_cor)) {
      #  i=1
        from <- com_cor[i,search]
        to <- com_cor[i,replaceBy]
        sub <- com_cor[i,restrict]!=""
        strict <- com_cor[i,strictly]
        cat(i,": ",as.vector(from)," -> ", to," | subset:",sub,"  nom complet: ",strict,"\n")
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



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @return
##' @author
add_localite <- function(){

    library(sf)
    library(data.table)
    library(dplyr)


    com_shp <- st_read("library/CommunesCentr.shp")
    com_table <- st_drop_geometry(com_shp)
    com_cor <- fread("library/correctifs_communes.csv")
    data <- fread("output/data_2019-06-06_clean.csv")

    data <- commune_corrections(data)



    colloc <- c("pk_data","COMMUNE","INSEE","DEPARTEMENT","DEPARTEMENT2","REGION")
  #  dataloc <-






} #END add_localite

