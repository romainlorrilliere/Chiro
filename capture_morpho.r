

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



