#################### BirdLab ###################
######## Getting & Shaping Weather Data ########
### weather data providing by GSODR package ####

#Libraries
library(GSODR)
library(reshape)
library(dplyr)

#GSODR Data
ToGetGSODRData{
  ?get_GSOD
  GSODR_data_all <- get_GSOD(years=c(2014,2015,2016,2017,2018),country="France")
  head(GSODR_data_all)
  summary(GSODR_data_all)
  GSODR_data_all$ID <- substr(GSODR_data_all$USAF,1,6)
  write.csv(GSODR_data_all,file="data/GSODR_data_all_1.csv")
  write.csv(GSODR_data_all,file="~/BirdLab/data/files_output/GSODR_data_all_2.csv")
  GSODR_data_2 <- GSODR_data_all

  head(GSODR_data_2)
  tail(GSODR_data_2)
  summary(GSODR_data_2)

  GSODR_data_2$X <- NULL
  GSODR_data_2$WBAN <- NULL
  GSODR_data_2$STN_NAME <- NULL
  GSODR_data_2$CTRY <- NULL
  GSODR_data_2$STATE <- NULL
  GSODR_data_2$CALL <- NULL
  GSODR_data_2$BEGIN <- NULL
  GSODR_data_2$END <- NULL
  GSODR_data_2$TEMP_CNT <- NULL
  GSODR_data_2$DEWP <- NULL
  GSODR_data_2$DEWP_CNT <- NULL
  GSODR_data_2$SLP <- NULL
  GSODR_data_2$SLP_CNT <- NULL
  GSODR_data_2$STP_CNT <- NULL
  GSODR_data_2$VISIB_CNT <- NULL
  GSODR_data_2$WDSP <- NULL
  GSODR_data_2$WDSP_CNT <- NULL
  GSODR_data_2$GUST <- NULL
  GSODR_data_2$MXSPD <- NULL
  GSODR_data_2$MAX_FLAG <- NULL
  GSODR_data_2$MIN_FLAG <- NULL
  GSODR_data_2$PRCP_FLAG <- NULL
  GSODR_data_2$I_THUNDER <- NULL
  GSODR_data_2$I_TORNADO_FUNNEL <- NULL
  GSODR_data_2$EA <- NULL
  GSODR_data_2$ES <- NULL
  GSODR_data_2$RH <- NULL
  GSODR_data_2$I_RAIN_DRIZZLE <- NULL

  colnames(GSODR_data_2)[colnames(GSODR_data_2)=="YEAR"] <- "YR"
  summary(GSODR_data_2)

  write.csv(GSODR_data_2,file="~/BirdLab/data/files_output/GSODR_data_light.csv")
  GSODR_data_light <- read.csv("~/BirdLab/data/files_output/GSODR_data_light.csv",header=TRUE,sep=",")

  GSODR_data_df <- as.data.frame(GSODR_data_light)
  GSODR_data_df$I_PRCP = GSODR_data_df$PRCP
  GSODR_data_df$I_SNOW = GSODR_data_df$SNDP
  GSODR_data_df$I_PRCP[GSODR_data_df$I_PRCP>0.0] <- 1
  GSODR_data_df$I_SNOW[GSODR_data_df$I_SNOW>0.0] <- 1

  GSODR_data_df <- GSODR_data_df[,c(1,2,3,23,4,5,6,7,8,9,10,11,12,13,16,17,14,15,18,19,25,21,20,22,24)]
  colnames(GSODR_data_df)[15] <- "TEMP_MAX"
  colnames(GSODR_data_df)[16] <- "TEMP_MIN"

  write.csv(GSODR_data_df, file="~/BirdLab/data/files_output/GSODR_data_df.csv")
  GSODR_data_df <- read.csv("~/BirdLab/data/files_output/GSODR_data_df.csv",header=TRUE,sep=",")

}

#Snow Ice
ToDoLoopOnSnowIce{
  wide_snow <- reshape(GSODR_data_df[,c("STNID","YDAY","I_SNOW_ICE")],idvar="YDAY",timevar=c("STNID"),direction="wide")
  colnames(wide_snow) <- substr(colnames(wide_snow),12,nchar(colnames(wide_snow)))

  wide_snow <- wide_snow[order(wide_snow[,1]),]
  nrow(wide_snow) == wide_snow[nrow(wide_snow),1]

  wide_cum_snow <- wide_snow

  for (i in 2:nrow(wide_cum_snow)) {
    wide_cum_snow[i,] <- ifelse(wide_cum_snow[i,] == 0,
                                0,
                                ifelse(is.na(wide_cum_snow[i-1,]),
                                       1,
                                       wide_cum_snow[i-1,]+1))
  }
  colnames(wide_cum_snow)[1] <- "YDAY"
  snow_long <- reshape(wide_cum_snow,direction="long",v.names="SNOW_ICE_CUMUL",timevar="STNID",
                       varying=list(2:ncol(wide_cum_snow)),
                       times=colnames(wide_cum_snow)[-1],new.row.names = 1:1000000)

  snow_long <- snow_long[,c("STNID","YDAY","SNOW_ICE_CUMUL")]
  GSODR_data_df_s <- merge(GSODR_data_df,snow_long,by=c("STNID","YDAY"))
  summary(GSODR_data_df_s)
}

#Precipitation
ToDoLoopOnPrecipitation{
  wide_prcp <- reshape(GSODR_data_df[,c("STNID","YDAY","I_PRCP")],idvar="YDAY",timevar=c("STNID"),direction="wide")
  colnames(wide_prcp) <- substr(colnames(wide_prcp),8,nchar(colnames(wide_prcp)))

  wide_prcp <- wide_prcp[order(wide_prcp[,1]),]
  nrow(wide_prcp) == wide_prcp[nrow(wide_prcp),1]

  wide_cum_prcp <- wide_prcp

  for (i in 2:nrow(wide_cum_prcp)) {
    wide_cum_prcp[i,] <- ifelse(wide_cum_prcp[i,] == 0,
                                0,
                                ifelse(is.na(wide_cum_prcp[i-1,]),
                                       1,
                                       wide_cum_prcp[i-1,]+1))
  }

  colnames(wide_cum_prcp)[1] <- "YDAY"
  long_prcp <- reshape(wide_cum_prcp,direction="long",v.names="PRCP_CUMUL",timevar="STNID",
                       varying=list(2:ncol(wide_cum_prcp)),
                       times=colnames(wide_cum_prcp)[-1],new.row.names=1:1000000)

  long_prcp <- long_prcp[,c("STNID","YDAY","PRCP_CUMUL")]
  GSODR_data_df_sp <- merge(GSODR_data_df_s,long_prcp,by=c("STNID","YDAY"))
  summary(GSODR_data_df_sp)
}

#Fog
ToDoLoopOnFog{
  wide_fog <- reshape(GSODR_data_df[,c("STNID","YDAY","I_FOG")],idvar="YDAY",timevar=c("STNID"),direction="wide")
  colnames(wide_fog) <- substr(colnames(wide_fog),7,nchar(colnames(wide_fog)))

  wide_fog <- wide_fog[order(wide_fog[,1]),]
  nrow(wide_fog) == wide_fog[nrow(wide_fog),1]

  wide_cum_fog <- wide_fog

  for (i in 2:nrow(wide_cum_fog)) {
    wide_cum_fog[i,] <- ifelse(wide_cum_fog[i,] == 0,
                               0,
                               ifelse(is.na(wide_cum_fog[i-1,]),
                                      1,
                                      wide_cum_fog[i-1,]+1))
  }

  colnames(wide_cum_fog)[1] <- "YDAY"
  head(wide_cum_fog)
  long_fog <- reshape(wide_cum_fog,direction="long",v.names="FOG_CUMUL",timevar="STNID",
                      varying = list(2:ncol(wide_cum_fog)),
                      times = colnames(wide_cum_fog)[-1],new.row.names=1:1000000)

  long_fog <- long_fog[,c("STNID","YDAY","FOG_CUMUL")]
  GSODR_data_df_spf <- merge(GSODR_data_df_sp,long_fog,by=c("STNID","YDAY"))
  summary(GSODR_data_df_spf)
}

#Hail
ToDoLoopOnHail{
  wide_hail <- reshape(GSODR_data_df[,c("STNID","YDAY","I_HAIL")],idvar="YDAY",timevar=c("STNID"),direction="wide")
  colnames(wide_hail) <- substr(colnames(wide_hail),8,nchar(colnames(wide_hail)))

  wide_hail <- wide_hail[order(wide_hail[,1]),]
  nrow(wide_hail) == wide_hail[nrow(wide_hail),1]

  wide_cum_hail <- wide_hail

  for (i in 2:nrow(wide_cum_hail)) {
    wide_cum_hail[i,] <- ifelse(wide_cum_hail[i,] == 0,
                                0,
                                ifelse(is.na(wide_cum_hail[i-1,]),
                                       1,
                                       wide_cum_hail[i-1,]+1))
  }

  colnames(wide_cum_hail)[1] <- "YDAY"
  long_hail <- reshape(wide_cum_hail,direction="long",v.names="HAIL_CUMUL",timevar = "STNID",
                       varying=list(2:ncol(wide_cum_hail)),
                       times=colnames(wide_cum_hail)[-1],new.row.names=1:1000000)

  long_hail <- long_hail[,c("STNID","YDAY","HAIL_CUMUL")]
  GSODR_data_df_spfh <- merge(GSODR_data_df_spf,long_hail,by=c("STNID","YDAY"))
  summary(GSODR_data_df_spfh)
}

#Latency Days at 7d and 14d
ToDoLatencyDays{
wd_714 <- GSODR_data_df_spfh

SNOW_714{
  cat(nrow(wd_714),"lines\n")
  for(colVar in c("SNDP","")){
    cat(" - ","SNDP")
    tMovingWindo <- wd_714[,c("STNID","YEARMODA","SNDP")]

    for(i in 1:14){
      tMovingWindo_i <- wd_714[,c("STNID","YEARMODA","SNDP")]
      tMovingWindo_i$YEARMODA <- as.character(as.Date(tMovingWindo_i$YEARMODA)+i)
      colnames(tMovingWindo_i)[3] <- paste("SNDP","_jm_",i,sep="")
      tMovingWindo <- merge(tMovingWindo,tMovingWindo_i,by=c("STNID","YEARMODA"),all=TRUE)
    }
    tMovingWindo_colVar <- data.frame(STNID=tMovingWindo$STNID,
                                      YEARMODA=tMovingWindo$YEARMODA,
                                      colVar_7=apply(tMovingWindo[,4:10],1,mean,na.rm=TRUE),
                                      colVar_14=apply(tMovingWindo[,4:17],1,mean,na.rm=TRUE))
    colnames(tMovingWindo_colVar)[3:4] <- paste("SNDP",c("_7","_14"),sep="")

    wd_714 <- merge(wd_714,tMovingWindo_colVar,by=c("STNID","YEARMODA"),all=TRUE)
    cat(nrow(wd_714),"lines\n")
  }
}
I_SNOW_714{
  cat(nrow(wd_714),"lines\n")
  for(colVar in c("I_SNOW","")){
    cat(" - ","I_SNOW")
    tMovingWindo <- wd_714[,c("STNID","YEARMODA","I_SNOW")]

    for(i in 1:14){
      tMovingWindo_i <- wd_714[,c("STNID","YEARMODA","I_SNOW")]
      tMovingWindo_i$YEARMODA <- as.character(as.Date(tMovingWindo_i$YEARMODA)+i)
      colnames(tMovingWindo_i)[3] <- paste("I_SNOW","_jm_",i,sep="")
      tMovingWindo <- merge(tMovingWindo,tMovingWindo_i,by=c("STNID","YEARMODA"),all=TRUE)
    }
    tMovingWindo_colVar <- data.frame(STNID=tMovingWindo$STNID,
                                      YEARMODA=tMovingWindo$YEARMODA,
                                      colVar_7=apply(tMovingWindo[,4:10],1,mean,na.rm=TRUE),
                                      colVar_14=apply(tMovingWindo[,4:17],1,mean,na.rm=TRUE))
    colnames(tMovingWindo_colVar)[3:4] <- paste("I_SNOW",c("_7","_14"),sep="")

    wd_714 <- merge(wd_714,tMovingWindo_colVar,by=c("STNID","YEARMODA"),all=TRUE)
    cat(nrow(wd_714),"lines\n")
  }
}
wd_714$I_PRCP_ALL <- as.numeric((ifelse(is.na(wd_714$I_HAIL),0,wd_714$I_HAIL) + ifelse(is.na(wd_714$I_PRCP),0,wd_714$I_PRCP))>0)
wd_714$I_PRCP_ALL[(wd_714$I_PRCP_ALL==0 & (is.na(wd_714$I_HAIL) | is.na(wd_714$I_PRCP)))] <- NA
PRCP_714{
  cat(nrow(wd_714),"lines\n")
  for(colVar in c("PRCP","")){
    cat(" - ","PRCP")
    tMovingWindo <- wd_714[,c("STNID","YEARMODA","PRCP")]

    for(i in 1:14){
      tMovingWindo_i <- wd_714[,c("STNID","YEARMODA","PRCP")]
      tMovingWindo_i$YEARMODA <- as.character(as.Date(tMovingWindo_i$YEARMODA)+i)
      colnames(tMovingWindo_i)[3] <- paste("PRCP","_jm_",i,sep="")
      tMovingWindo <- merge(tMovingWindo,tMovingWindo_i,by=c("STNID","YEARMODA"),all=TRUE)
    }
    tMovingWindo_colVar <- data.frame(STNID=tMovingWindo$STNID,
                                      YEARMODA=tMovingWindo$YEARMODA,
                                      colVar_7=apply(tMovingWindo[,4:10],1,mean,na.rm=TRUE)*7,
                                      colVar_14=apply(tMovingWindo[,4:17],1,mean,na.rm=TRUE)*14)
    colnames(tMovingWindo_colVar)[3:4] <- paste("PRCP",c("_7","_14"),sep="")

    wd_714 <- merge(wd_714,tMovingWindo_colVar,by=c("STNID","YEARMODA"),all=TRUE)
    cat(nrow(wd_714),"lines\n")
  }
}
I_PRCP_714{
  cat(nrow(wd_714),"lines\n")
  for(colVar in c("I_PRCP","")){
    cat(" - ","I_PRCP")
    tMovingWindo <- wd_714[,c("STNID","YEARMODA","I_PRCP")]

    for(i in 1:14){
      tMovingWindo_i <- wd_714[,c("STNID","YEARMODA","I_PRCP")]
      tMovingWindo_i$YEARMODA <- as.character(as.Date(tMovingWindo_i$YEARMODA)+i)
      colnames(tMovingWindo_i)[3] <- paste("I_PRCP","_jm_",i,sep="")
      tMovingWindo <- merge(tMovingWindo,tMovingWindo_i,by=c("STNID","YEARMODA"),all=TRUE)
    }
    tMovingWindo_colVar <- data.frame(STNID=tMovingWindo$STNID,
                                      YEARMODA=tMovingWindo$YEARMODA,
                                      colVar_7=apply(tMovingWindo[,4:10],1,mean,na.rm=TRUE)*7,
                                      colVar_14=apply(tMovingWindo[,4:17],1,mean,na.rm=TRUE)*14)
    colnames(tMovingWindo_colVar)[3:4] <- paste("I_PRCP",c("_7","_14"),sep="")

    wd_714 <- merge(wd_714,tMovingWindo_colVar,by=c("STNID","YEARMODA"),all=TRUE)
    cat(nrow(wd_714),"lines\n")
  }
}
I_PRCP_ALL_714{
  cat(nrow(wd_714),"lines\n")
  for(colVar in c("I_PRCP_ALL","")){
    cat(" - ","I_PRCP_ALL")
    tMovingWindo <- wd_714[,c("STNID","YEARMODA","I_PRCP_ALL")]

    for(i in 1:14){
      tMovingWindo_i <- wd_714[,c("STNID","YEARMODA","I_PRCP_ALL")]
      tMovingWindo_i$YEARMODA <- as.character(as.Date(tMovingWindo_i$YEARMODA)+i)
      colnames(tMovingWindo_i)[3] <- paste("I_PRCP_ALL","_jm_",i,sep="")
      tMovingWindo <- merge(tMovingWindo,tMovingWindo_i,by=c("STNID","YEARMODA"),all=TRUE)
    }
    tMovingWindo_colVar <- data.frame(STNID=tMovingWindo$STNID,
                                      YEARMODA=tMovingWindo$YEARMODA,
                                      colVar_7=apply(tMovingWindo[,4:10],1,mean,na.rm=TRUE)*7,
                                      colVar_14=apply(tMovingWindo[,4:17],1,mean,na.rm=TRUE)*14)
    colnames(tMovingWindo_colVar)[3:4] <- paste("I_PRCP_ALL",c("_7","_14"),sep="")

    wd_714 <- merge(wd_714,tMovingWindo_colVar,by=c("STNID","YEARMODA"),all=TRUE)
    cat(nrow(wd_714),"lines\n")
  }
}
I_SNOW_ICE_714{
  cat(nrow(wd_714),"lines\n")
  for(colVar in c("I_SNOW_ICE","")){
    cat(" - ","I_SNOW_ICE")
    tMovingWindo <- wd_714[,c("STNID","YEARMODA","I_SNOW_ICE")]

    for(i in 1:14){
      tMovingWindo_i <- wd_714[,c("STNID","YEARMODA","I_SNOW_ICE")]
      tMovingWindo_i$YEARMODA <- as.character(as.Date(tMovingWindo_i$YEARMODA)+i)
      colnames(tMovingWindo_i)[3] <- paste("I_SNOW_ICE","_jm_",i,sep="")
      tMovingWindo <- merge(tMovingWindo,tMovingWindo_i,by=c("STNID","YEARMODA"),all=TRUE)
    }
    cat("tMovingWindo:", nrow(tMovingWindo),"lines\n")
    tMovingWindo_colVar <- data.frame(STNID=tMovingWindo$STNID,
                                      YEARMODA=tMovingWindo$YEARMODA,
                                      colVar_7=apply(tMovingWindo[,4:10],1,mean,na.rm=TRUE)*7,
                                      colVar_14=apply(tMovingWindo[,4:17],1,mean,na.rm=TRUE)*14)
    colnames(tMovingWindo_colVar)[3:4] <- paste("I_SNOW_ICE",c("_7","_14"),sep="")

    cat("tMovingWindo_colVar:",nrow(tMovingWindo_colVar),"lines\n")

    wd_714 <- merge(wd_714,tMovingWindo_colVar,by=c("STNID","YEARMODA"),all=TRUE)
    cat(nrow(wd_714),"lines\n")
  }
}
I_FOG_714{
  cat(nrow(wd_714),"lines\n")
  for(colVar in c("I_FOG","")){
    cat(" - ","I_FOG")
    tMovingWindo <- wd_714[,c("STNID","YEARMODA","I_FOG")]

    for(i in 1:14){
      tMovingWindo_i <- wd_714[,c("STNID","YEARMODA","I_FOG")]
      tMovingWindo_i$YEARMODA <- as.character(as.Date(tMovingWindo_i$YEARMODA)+i)
      colnames(tMovingWindo_i)[3] <- paste("I_FOG","_jm_",i,sep="")
      tMovingWindo <- merge(tMovingWindo,tMovingWindo_i,by=c("STNID","YEARMODA"),all=TRUE)
    }
    tMovingWindo_colVar <- data.frame(STNID=tMovingWindo$STNID,
                                      YEARMODA=tMovingWindo$YEARMODA,
                                      colVar_7=apply(tMovingWindo[,4:10],1,mean,na.rm=TRUE)*7,
                                      colVar_14=apply(tMovingWindo[,4:17],1,mean,na.rm=TRUE)*14)
    colnames(tMovingWindo_colVar)[3:4] <- paste("I_FOG",c("_7","_14"),sep="")

    wd_714 <- merge(wd_714,tMovingWindo_colVar,by=c("STNID","YEARMODA"),all=TRUE)
    cat(nrow(wd_714),"lines\n")
  }
}
I_HAIL_714{
  cat(nrow(wd_714),"lines\n")
  for(colVar in c("I_HAIL","")){
    cat(" - ","I_HAIL")
    tMovingWindo <- wd_714[,c("STNID","YEARMODA","I_HAIL")]

    for(i in 1:14){
      tMovingWindo_i <- wd_714[,c("STNID","YEARMODA","I_HAIL")]
      tMovingWindo_i$YEARMODA <- as.character(as.Date(tMovingWindo_i$YEARMODA)+i)
      colnames(tMovingWindo_i)[3] <- paste("I_HAIL","_jm_",i,sep="")
      tMovingWindo <- merge(tMovingWindo,tMovingWindo_i,by=c("STNID","YEARMODA"),all=TRUE)
    }
    tMovingWindo_colVar <- data.frame(STNID=tMovingWindo$STNID,
                                      YEARMODA=tMovingWindo$YEARMODA,
                                      colVar_7=apply(tMovingWindo[,4:10],1,mean,na.rm=TRUE)*7,
                                      colVar_14=apply(tMovingWindo[,4:17],1,mean,na.rm=TRUE)*14)
    colnames(tMovingWindo_colVar)[3:4] <- paste("I_HAIL",c("_7","_14"),sep="")

    wd_714 <- merge(wd_714,tMovingWindo_colVar,by=c("STNID","YEARMODA"),all=TRUE)
    cat(nrow(wd_714),"lines\n")

  }
}
}

#Weather Data With Loop
ToShapeWeatherDataFrame{
  wd_714$X.1 <- NULL
  wd_714$X <- NULL
  wd_714$USAF <- NULL
  wd_714$PRCP_7.y <- NULL
  wd_714$PRCP_14.y <- NULL
  wd_714$I_PRCP_7.y <- NULL
  wd_714$I_PRCP_14.y <- NULL
  wd_714$I_PRCP_ALL_7.y <- NULL
  wd_714$I_PRCP_ALL_14.y <- NULL
  wd_714$I_SNOW_ICE_7.y <- NULL
  wd_714$I_SNOW_ICE_14.y <- NULL
  wd_714$I_FOG_7.y <- NULL
  wd_714$I_FOG_14.y <- NULL
  wd_714$I_HAIL_7.y <- NULL
  wd_714$I_HAIL_14.y <- NULL

  wd_714$TEMP_DELTA = wd_714$TEMP_MAX - wd_714$TEMP_MIN

  colnames(wd_714)[29] <- "PRCP7"
  colnames(wd_714)[30] <- "PRCP14"
  colnames(wd_714)[31] <- "I_PRCP_7"
  colnames(wd_714)[32] <- "I_PRCP_14"
  colnames(wd_714)[33] <- "I_PRCP_ALL_7"
  colnames(wd_714)[34] <- "I_PRCP_ALL_14"
  colnames(wd_714)[35] <- "I_SNOW_ICE_7"
  colnames(wd_714)[36] <- "I_SNOW_ICE_14"
  colnames(wd_714)[37] <- "I_FOG_7"
  colnames(wd_714)[38] <- "I_FOG_14"
  colnames(wd_714)[39] <- "I_HAIL_7"
  colnames(wd_714)[40] <- "I_HAIL_14"

  wd_714 <- wd_714[,c(1,4,5,6,7,8,2,9,10,11,3,15,16,18,12,13,14,41,17,23,25,28,29,30,31,32,33,34,19,20,24,35,36,21,26,37,38,22,27,39,40)]
  wdwl <- wd_714

  write.csv(wdwl, file="~/BirdLab/data/files_output/wdwl.csv")
  wdwl <- read.csv("~/BirdLab/data/files_output/wdwl.csv",header=TRUE,sep=",")
  head(wdwl)
}
