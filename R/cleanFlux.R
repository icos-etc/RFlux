

cleanFlux <- function(path_workset, path_ecmd, path_output=NULL, FileName=NULL, plotQC=FALSE, storage=FALSE){

ec_data0 <- fread(path_workset, sep=",", header=TRUE, data.table=FALSE, na.strings=c(NA, "-9999"))
timestamp_ec_data0 <- as.POSIXct(as.character(ec_data0[,1]), format="%Y%m%d%H%M", tz="GMT")
ec_data0.xts <- xts(ec_data0, order.by=timestamp_ec_data0)[,-1]

t_start <- timestamp_ec_data0[1]
t_end <- timestamp_ec_data0[length(timestamp_ec_data0)]
t_l <- as.vector(difftime(t_end,t_start, units="mins")/30)
reg_timestamp <- t_start + seq(0,as.numeric(t_l)*1800, 1800)
reg_ts <- xts(rep(1, length(reg_timestamp)), order.by=reg_timestamp)

## Building time series of wind sector to exclude
md_tmp <- fread(path_ecmd, integer64="numeric", data.table=FALSE, na.strings=c(NA, "-9999"))
md_tmp0 <- rbind(md_tmp, replace(md_tmp[nrow(md_tmp),], 2, as.character(format(round_date(Sys.time(), "30 minutes")+60, "%Y%m%d%H%M", tz="GMT"))), make.row.names=FALSE)
site <- md_tmp[1,"SITEID"]

time_S <- as.POSIXct("200001010000", format="%Y%m%d%H%M", tz="GMT")
WS2E_c1 <- c()
WS2E_w1 <- c()
WS2E_c2 <- c()
WS2E_w2 <- c()
WS2E_c3 <- c()
WS2E_w3 <- c()

for (i in 1:(nrow(md_tmp0)-1)){
	if(nchar(as.character(md_tmp0[i,"DATE_OF_VARIATION_EF"]))==12 & nchar(as.character(md_tmp0[i+1,"DATE_OF_VARIATION_EF"]))==12){
			time_S0 <- seq(strptime(substr(as.character(md_tmp0[i,"DATE_OF_VARIATION_EF"]),1,12), format="%Y%m%d%H%M", tz="GMT"), strptime(substr(as.character(md_tmp0[i+1,"DATE_OF_VARIATION_EF"]), 1,12), format="%Y%m%d%H%M", tz="GMT")-60, by="1 min")
		}  
	if(nchar(as.character(md_tmp0[i,"DATE_OF_VARIATION_EF"]))==12 & nchar(as.character(md_tmp0[i+1,"DATE_OF_VARIATION_EF"]))==8){
			time_S0 <- seq(strptime(substr(as.character(md_tmp0[i,"DATE_OF_VARIATION_EF"]),1,12), format="%Y%m%d%H%M", tz="GMT"), strptime(substr(as.character(md_tmp0[i+1,"DATE_OF_VARIATION_EF"]), 1,8), format="%Y%m%d", tz="GMT")-60, by="1 min")
		}  
	if(nchar(as.character(md_tmp0[i,"DATE_OF_VARIATION_EF"]))==8 & nchar(as.character(md_tmp0[i+1,"DATE_OF_VARIATION_EF"]))==12){
			time_S0 <- seq(strptime(substr(as.character(md_tmp0[i,"DATE_OF_VARIATION_EF"]),1,8), format="%Y%m%d", tz="GMT"), strptime(substr(as.character(md_tmp0[i+1,"DATE_OF_VARIATION_EF"]), 1,12), format="%Y%m%d%H%M", tz="GMT")-60, by="1 min")
		}  
	if(nchar(as.character(md_tmp0[i,"DATE_OF_VARIATION_EF"]))==8 & nchar(as.character(md_tmp0[i+1,"DATE_OF_VARIATION_EF"]))==8){
			time_S0 <- seq(strptime(substr(as.character(md_tmp0[i,"DATE_OF_VARIATION_EF"]),1,8), format="%Y%m%d", tz="GMT"), strptime(substr(as.character(md_tmp0[i+1,"DATE_OF_VARIATION_EF"]), 1,8), format="%Y%m%d", tz="GMT")-60, by="1 min")
		}  
	time_S <- c(time_S, time_S0)
	WS2E_c0 <- rep(md_tmp0[i,"SA_INVALID_WIND_SECTOR_c1"], length(time_S0))
	WS2E_w0 <- rep(md_tmp0[i,"SA_INVALID_WIND_SECTOR_w1"], length(time_S0))
	WS2E_c1 <- c(WS2E_c1, WS2E_c0)
	WS2E_w1 <- c(WS2E_w1, WS2E_w0)
	WS2E_c0 <- rep(md_tmp0[i,"SA_INVALID_WIND_SECTOR_c2"], length(time_S0))
	WS2E_w0 <- rep(md_tmp0[i,"SA_INVALID_WIND_SECTOR_w2"], length(time_S0))
	WS2E_c2 <- c(WS2E_c2, WS2E_c0)
	WS2E_w2 <- c(WS2E_w2, WS2E_w0)
	WS2E_c0 <- rep(md_tmp0[i,"SA_INVALID_WIND_SECTOR_c3"], length(time_S0))
	WS2E_w0 <- rep(md_tmp0[i,"SA_INVALID_WIND_SECTOR_w3"], length(time_S0))
	WS2E_c3 <- c(WS2E_c3, WS2E_c0)
	WS2E_w3 <- c(WS2E_w3, WS2E_w0)
	}

WS2E_c1.min <- xts(WS2E_c1, order.by=time_S[-1])
WS2E_w1.min <- xts(WS2E_w1, order.by=time_S[-1])
WS2E_c2.min <- xts(WS2E_c2, order.by=time_S[-1])
WS2E_w2.min <- xts(WS2E_w2, order.by=time_S[-1])
WS2E_c3.min <- xts(WS2E_c3, order.by=time_S[-1])
WS2E_w3.min <- xts(WS2E_w3, order.by=time_S[-1])

WS2E_c1.hh <- period.apply(WS2E_c1.min , endpoints(WS2E_c1.min , "mins", 30), function(x) mode(x))
WS2E_w1.hh <- period.apply(WS2E_w1.min , endpoints(WS2E_w1.min , "mins", 30), function(x) mode(x))
WS2E_c2.hh <- period.apply(WS2E_c2.min , endpoints(WS2E_c2.min , "mins", 30), function(x) mode(x))
WS2E_w2.hh <- period.apply(WS2E_w2.min , endpoints(WS2E_w2.min , "mins", 30), function(x) mode(x))
WS2E_c3.hh <- period.apply(WS2E_c3.min , endpoints(WS2E_c3.min , "mins", 30), function(x) mode(x))
WS2E_w3.hh <- period.apply(WS2E_w3.min , endpoints(WS2E_w3.min , "mins", 30), function(x) mode(x))

hh_timestep <- round_date(time(WS2E_c1.hh), "30 minutes")

windsect2excl.xts <- xts(cbind(as.vector(WS2E_c1.hh), as.vector(WS2E_w1.hh), as.vector(WS2E_c2.hh), as.vector(WS2E_w2.hh), as.vector(WS2E_c3.hh), as.vector(WS2E_w3.hh)), order.by=hh_timestep)
colnames(windsect2excl.xts) <- c("WS2E_c1","WS2E_w1","WS2E_c2","WS2E_w2","WS2E_c3","WS2E_w3")

wind_sector_exclusion.xts <- window(windsect2excl.xts, start=t_start, end=t_end) 

ec_data <- merge(reg_ts, ec_data0.xts, wind_sector_exclusion.xts, tzone="GMT")[,-1]


N <- nrow(ec_data)
hod <- gl(48,1,N)
hodb <- gl(12,4,N)
zero_vector <- rep(0,N)
hz <- ec_data[,"acquisition_frequency"]


#################################################################################################################################################################################################################
#
# Missing data in the orignal dataset
#
#################################################################################################################################################################################################################

NEE_raw <- as.vector(ec_data[,"CO2flux"])
LE_raw <- as.vector(ec_data[,"LE"])
H_raw <- as.vector(ec_data[,"H"])

OoR_NEE_index <- c(which(NEE_raw > 70), which(NEE_raw < -100))
OoR_LE_index <- c(which(LE_raw > 1000), which(LE_raw < -300))
OoR_H_index <- c(which(H_raw > 1000), which(H_raw < -300))
OoR_NEE <- replace(replace(zero_vector, OoR_NEE_index, 2), which(is.na(ec_data[,"CO2flux"])),NA)
OoR_LE <- replace(replace(zero_vector, OoR_LE_index, 2), which(is.na(ec_data[,"LE"])),NA)
OoR_H <- replace(replace(zero_vector, OoR_H_index, 2), which(is.na(ec_data[,"H"])),NA)

H_FMR_STAT <- replace(ec_data[,"FMR_H"], which(is.na(ec_data[,"FMR_H"])), 100)
H_FMR_FLAG <- as.vector(replace(replace(replace(zero_vector, which(ec_data[,"FMR_H"]>5),1), which(ec_data[,"FMR_H"]>15),2), which(is.na(H_raw)), 2))
H_LGD_STAT <- replace(ec_data[,"LGD_H"], which(is.na(ec_data[,"LGD_H"])), 1800)
H_LGD_FLAG <- as.vector(replace(replace(replace(zero_vector, which(ec_data[,"LGD_H"]>90),1), which(ec_data[,"LGD_H"]>180),2),which(is.na(H_raw)), 2))

LE_FMR_STAT <- replace(ec_data[,"FMR_LE"], which(is.na(ec_data[,"FMR_LE"])), 100)
LE_FMR_FLAG <- as.vector(replace(replace(replace(zero_vector, which(ec_data[,"FMR_LE"]>5),1), which(ec_data[,"FMR_LE"]>15),2),which(is.na(LE_raw)), 2))
LE_LGD_STAT <- replace(ec_data[,"LGD_LE"], which(is.na(ec_data[,"LGD_LE"])), 1800)
LE_LGD_FLAG <- as.vector(replace(replace(replace(zero_vector, which(ec_data[,"LGD_LE"]>90),1), which(ec_data[,"LGD_LE"]>180),2),which(is.na(LE_raw)), 2))

FC_FMR_STAT <- replace(ec_data[,"FMR_Fc"], which(is.na(ec_data[,"FMR_Fc"])), 100)
FC_FMR_FLAG <- as.vector(replace(replace(replace(zero_vector, which(ec_data[,"FMR_Fc"]>5),1), which(ec_data[,"FMR_Fc"]>15),2),which(is.na(NEE_raw)), 2))
FC_LGD_STAT <- replace(ec_data[,"LGD_Fc"], which(is.na(ec_data[,"LGD_Fc"])), 1800)
FC_LGD_FLAG <- as.vector(replace(replace(replace(zero_vector, which(ec_data[,"LGD_Fc"]>90),1), which(ec_data[,"LGD_Fc"]>180),2),which(is.na(NEE_raw)), 2))

SA_Diag0 <- zero_vector
SA_Diag <- replace(SA_Diag0, which(ec_data[,"SADiag"]>0), 1)

GA_Diag <- ec_data[,"GADiag"]

#if(md_tmp[1,"GA_MODEL"]!="li7200_1") GA_Diag <- zero_vector
#if(md_tmp[1,"GA_MODEL"]=="li7200_1"){
#	GA_TCellDiag <- apply(cbind(ec_data[,"GA_t_out"], ec_data[,"GA_t_in"]), 1, function(x) min(x, na.rm=TRUE));
#	GA_DiagVar <- cbind(ec_data[,"GA_head_detect"], GA_TCellDiag, ec_data[,which(colnames(ec_data)=="GA_aux_in"):which(colnames(ec_data)=="GA_sync")]);
#	GA_Diag <- apply(GA_DiagVar, MARGIN=1, function(x) sum(x, na.rm=TRUE))
#	}

WDir2Exc_1 <- c()
WDir2Exc_2 <- c()
WDir2Exc_3 <- c()
WDir <- as.vector(ec_data[,"WDir"])
w1_l <- as.numeric(ifelse(ec_data[,"WS2E_c1"]-ec_data[,"WS2E_w1"]/2 <= 0, ec_data[,"WS2E_c1"]-ec_data[,"WS2E_w1"]/2+360, ec_data[,"WS2E_c1"]-ec_data[,"WS2E_w1"]/2))
w1_u <- as.numeric(ifelse(ec_data[,"WS2E_c1"]+ec_data[,"WS2E_w1"]/2 >= 360, ec_data[,"WS2E_c1"]+ec_data[,"WS2E_w1"]/2-360, ec_data[,"WS2E_c1"]+ec_data[,"WS2E_w1"]/2))
w2_l <- as.numeric(ifelse(ec_data[,"WS2E_c2"]-ec_data[,"WS2E_w2"]/2 <= 0, ec_data[,"WS2E_c2"]-ec_data[,"WS2E_w2"]/2+360, ec_data[,"WS2E_c2"]-ec_data[,"WS2E_w2"]/2))
w2_u <- as.numeric(ifelse(ec_data[,"WS2E_c2"]+ec_data[,"WS2E_w2"]/2 >= 360, ec_data[,"WS2E_c2"]+ec_data[,"WS2E_w2"]/2-360, ec_data[,"WS2E_c2"]+ec_data[,"WS2E_w2"]/2))
w3_l <- as.numeric(ifelse(ec_data[,"WS2E_c3"]-ec_data[,"WS2E_w3"]/2 <= 0, ec_data[,"WS2E_c3"]-ec_data[,"WS2E_w2"]/2+360, ec_data[,"WS2E_c3"]-ec_data[,"WS2E_w3"]/2))
w3_u <- as.numeric(ifelse(ec_data[,"WS2E_c3"]+ec_data[,"WS2E_w3"]/2 >= 360, ec_data[,"WS2E_c3"]+ec_data[,"WS2E_w2"]/2-360, ec_data[,"WS2E_c3"]+ec_data[,"WS2E_w3"]/2))
#if (w1_u-w1_l > 0 & !is.na(w1_u-w1_l)) WDir2Exc_1 <- which(WDir > w1_l & WDir < w1_u)
#if (w1_u-w1_l < 0 & !is.na(w1_u-w1_l)) WDir2Exc_1 <- which(WDir > w1_l | WDir < w1_u)
#if (w2_u-w2_l > 0 & !is.na(w2_u-w2_l)) WDir2Exc_2 <- which(WDir > w2_l & WDir < w2_u)
#if (w2_u-w2_l < 0 & !is.na(w2_u-w2_l)) WDir2Exc_2 <- which(WDir > w2_l | WDir < w2_u)
#if (w3_u-w3_l > 0 & !is.na(w3_u-w3_l)) WDir2Exc_3 <- which(WDir > w3_l & WDir < w3_u)
#if (w3_u-w3_l < 0 & !is.na(w3_u-w3_l)) WDir2Exc_3 <- which(WDir > w3_l | WDir < w3_u)

WDir2Exc_1 <- c(intersect(which(w1_u-w1_l > 0 & !is.na(w1_u-w1_l)), which(WDir > w1_l & WDir < w1_u)), intersect(which(w1_u-w1_l < 0 & !is.na(w1_u-w1_l)), which(WDir > w1_l | WDir < w1_u)))
WDir2Exc_2 <- c(intersect(which(w2_u-w2_l > 0 & !is.na(w2_u-w2_l)), which(WDir > w2_l & WDir < w2_u)), intersect(which(w2_u-w2_l < 0 & !is.na(w2_u-w2_l)), which(WDir > w2_l | WDir < w2_u)))
WDir2Exc_3 <- c(intersect(which(w3_u-w3_l > 0 & !is.na(w3_u-w3_l)), which(WDir > w3_l & WDir < w3_u)), intersect(which(w3_u-w3_l < 0 & !is.na(w3_u-w3_l)), which(WDir > w3_l | WDir < w3_u)))


WDir2Exc <- union(union(WDir2Exc_1,WDir2Exc_2),WDir2Exc_3)
length(WDir2Exc)

ACF_V_SevEr <- which(ec_data[,"ACF_v"] <= 0.5)
ACF_W_SevEr <- which(ec_data[,"ACF_w"] <= 0.5)
ACF_TS_SevEr <- which(ec_data[,"ACF_ts"] <= 0.5)
ACF_CO2_SevEr <- which(ec_data[,"ACF_co2"] <= 0.5)
ACF_H2O_SevEr <- which(ec_data[,"ACF_h2o"] <= 0.5)

ACF_V_ModEr <- which(ec_data[,"ACF_v"] > 0.5 & ec_data[,"ACF_v"] <= 0.75)
ACF_W_ModEr <- which(ec_data[,"ACF_w"] > 0.5 & ec_data[,"ACF_w"] <= 0.75)
ACF_TS_ModEr <- which(ec_data[,"ACF_ts"] > 0.5 & ec_data[,"ACF_ts"] <= 0.75)
ACF_CO2_ModEr <- which(ec_data[,"ACF_co2"] > 0.5 & ec_data[,"ACF_co2"] <= 0.75)
ACF_H2O_ModEr <- which(ec_data[,"ACF_h2o"] > 0.5 & ec_data[,"ACF_h2o"] <= 0.75)

DDI_V_SevEr <- which(ec_data[,"DDI_v"] >= hz*60*5)
DDI_W_SevEr <- which(ec_data[,"DDI_w"] >= hz*60*5)
DDI_TS_SevEr <- which(ec_data[,"DDI_ts"] >= hz*60*5)
DDI_CO2_SevEr <- which(ec_data[,"DDI_co2"] >= hz*60*5)
DDI_H2O_SevEr <- which(ec_data[,"DDI_h2o"] >= hz*60*5)

DDI_V_ModEr <- which(ec_data[,"DDI_v"] >= hz*30*5 & ec_data[,"DDI_v"] < hz*60*5)
DDI_W_ModEr <- which(ec_data[,"DDI_w"] >= hz*30*5 & ec_data[,"DDI_w"] < hz*60*5)
DDI_TS_ModEr <- which(ec_data[,"DDI_ts"] >= hz*30*5 & ec_data[,"DDI_ts"] < hz*60*5)
DDI_CO2_ModEr <- which(ec_data[,"DDI_co2"] >= hz*30*5 & ec_data[,"DDI_co2"] < hz*60*5)
DDI_H2O_ModEr <- which(ec_data[,"DDI_h2o"] >= hz*30*5 & ec_data[,"DDI_h2o"] < hz*60*5)


INT_NEE_SevEr <- union(union(union(union(union(union(union(WDir2Exc,union(which(SA_Diag!=0),which(GA_Diag!=0))), union(which(FC_FMR_FLAG==2), which(FC_LGD_FLAG==2))), OoR_NEE_index), ACF_W_SevEr), ACF_CO2_SevEr), DDI_W_SevEr), DDI_CO2_SevEr) 
INT_LE_SevEr <- union(union(union(union(union(union(union(WDir2Exc,union(which(SA_Diag!=0),which(GA_Diag!=0))), union(which(LE_FMR_FLAG==2), which(LE_LGD_FLAG==2))), OoR_LE_index), ACF_W_SevEr), ACF_H2O_SevEr), DDI_W_SevEr), DDI_H2O_SevEr)
INT_H_SevEr <-  union(union(union(union(union(union(union(WDir2Exc,which(SA_Diag!=0)), union(which(H_FMR_FLAG==2), which(H_LGD_FLAG==2))), OoR_H_index), ACF_W_SevEr), ACF_TS_SevEr), DDI_W_SevEr), DDI_TS_SevEr)

INT_NEE_ModEr <- union(union(union(union(union(which(FC_FMR_FLAG==1), which(FC_LGD_FLAG==1)), ACF_W_ModEr), ACF_CO2_ModEr), DDI_W_ModEr), DDI_CO2_ModEr)
INT_LE_ModEr <- union(union(union(union(union(which(LE_FMR_FLAG==1), which(LE_LGD_FLAG==1)), ACF_W_ModEr), ACF_H2O_ModEr), DDI_W_ModEr), DDI_H2O_ModEr)
INT_H_ModEr <-  union(union(union(union(union(which(H_FMR_FLAG==1),which(H_LGD_FLAG==1)), ACF_W_ModEr), ACF_TS_ModEr), DDI_W_ModEr), DDI_TS_ModEr)


#################################################################################################################################################################################################################
#
#  Instrumental Problem Detection by means of LSR, KID, SC, ACF and DDI tests
#
#################################################################################################################################################################################################################

LSR_H_SevEr <- which(ec_data[,"LSR_H"]< 0.99)
LSR_LE_SevEr <- which(ec_data[,"LSR_LE"]< 0.99)
LSR_NEE_SevEr <- which(ec_data[,"LSR_Fc"]< 0.99)

LSR_H_ModEr <- which(ec_data[,"LSR_H"]>=0.99 & ec_data[,"LSR_H"]<=0.995)
LSR_LE_ModEr <- which(ec_data[,"LSR_LE"]>=0.99 & ec_data[,"LSR_LE"]<=0.995)
LSR_NEE_ModEr <- which(ec_data[,"LSR_Fc"]>=0.99 & ec_data[,"LSR_Fc"]<=0.995)


KTHs <- 50
KID_V_SevEr <- which(ec_data[,"KID1_v"]>KTHs)
KID_H_SevEr <- union(which(ec_data[,"KID1_w"]>KTHs), which(ec_data[,"KID1_ts"]>KTHs))
KID_LE_SevEr <- union(which(ec_data[,"KID1_w"]>KTHs), which(ec_data[,"KID1_h2o"]>KTHs))
KID_NEE_SevEr <- union(which(ec_data[,"KID1_w"]>KTHs), which(ec_data[,"KID1_co2"]>KTHs))

KTHm <- 30
KID_V_ModEr <- which(ec_data[,"KID1_v"]>KTHm)
KID_H_ModEr <- union(which(ec_data[,"KID1_w"]>KTHm), which(ec_data[,"KID1_ts"]>KTHm))
KID_LE_ModEr <- union(which(ec_data[,"KID1_w"]>KTHm), which(ec_data[,"KID1_h2o"]>KTHm))
KID_NEE_ModEr <- union(which(ec_data[,"KID1_w"]>KTHm), which(ec_data[,"KID1_co2"]>KTHm))

SCTH1 <- hz*60*30*0.04
SCTH2 <- hz*60*30*0.01

HF_V_SevEr <- union(which(ec_data[,"HF5_v"]>SCTH1), which(ec_data[,"HF10_v"]>SCTH2))
HD_V_SevEr <- union(which(ec_data[,"HD5_v"]>SCTH1), which(ec_data[,"HD10_v"]>SCTH2))

HF_W_SevEr <- union(which(ec_data[,"HF5_w"]>SCTH1), which(ec_data[,"HF10_w"]>SCTH2))
HD_W_SevEr <- union(which(ec_data[,"HD5_w"]>SCTH1), which(ec_data[,"HD10_w"]>SCTH2))

HF_TS_SevEr <- union(which(ec_data[,"HF5_ts"]>SCTH1), which(ec_data[,"HF10_ts"]>SCTH2))
HD_TS_SevEr <- union(which(ec_data[,"HD5_ts"]>SCTH1), which(ec_data[,"HD10_ts"]>SCTH2))

HF_CO2_SevEr <- union(which(ec_data[,"HF5_co2"]>SCTH1), which(ec_data[,"HF10_co2"]>SCTH2))
HD_CO2_SevEr <- union(which(ec_data[,"HD5_co2"]>SCTH1), which(ec_data[,"HD10_co2"]>SCTH2))

HF_H2O_SevEr <- union(which(ec_data[,"HF5_h2o"]>SCTH1), which(ec_data[,"HF10_h2o"]>SCTH2))
HD_H2O_SevEr <- union(which(ec_data[,"HD5_h2o"]>SCTH1), which(ec_data[,"HD10_h2o"]>SCTH2))


SC_H_SevEr <- union(union(union(union(HF_W_SevEr, HD_W_SevEr), HF_TS_SevEr), HD_TS_SevEr), KID_H_SevEr)
SC_LE_SevEr <- union(union(union(union(HF_W_SevEr, HD_W_SevEr), HF_H2O_SevEr), HD_H2O_SevEr), KID_LE_SevEr)
SC_NEE_SevEr <- union(union(union(union(HF_W_SevEr, HD_W_SevEr), HF_CO2_SevEr), HD_CO2_SevEr), KID_NEE_SevEr)



HF_V_ModEr <- union(which(ec_data[,"HF5_v"]>SCTH1/2), which(ec_data[,"HF10_v"]>SCTH2/2))
HD_V_ModEr <- union(which(ec_data[,"HD5_v"]>SCTH1/2), which(ec_data[,"HD10_v"]>SCTH2/2))

HF_W_ModEr <- union(which(ec_data[,"HF5_w"]>SCTH1/2), which(ec_data[,"HF10_w"]>SCTH2/2))
HD_W_ModEr <- union(which(ec_data[,"HD5_w"]>SCTH1/2), which(ec_data[,"HD10_w"]>SCTH2/2))

HF_TS_ModEr <- union(which(ec_data[,"HF5_ts"]>SCTH1/2), which(ec_data[,"HF10_ts"]>SCTH2/2))
HD_TS_ModEr <- union(which(ec_data[,"HD5_ts"]>SCTH1/2), which(ec_data[,"HD10_ts"]>SCTH2/2))

HF_CO2_ModEr <- union(which(ec_data[,"HF5_co2"]>SCTH1/2), which(ec_data[,"HF10_co2"]>SCTH2/2))
HD_CO2_ModEr <- union(which(ec_data[,"HD5_co2"]>SCTH1/2), which(ec_data[,"HD10_co2"]>SCTH2/2))

HF_H2O_ModEr <- union(which(ec_data[,"HF5_h2o"]>SCTH1/2), which(ec_data[,"HF10_h2o"]>SCTH2/2))
HD_H2O_ModEr <- union(which(ec_data[,"HD5_h2o"]>SCTH1/2), which(ec_data[,"HD10_h2o"]>SCTH2/2))

SC_H_ModEr <- union(union(union(union(HF_W_ModEr, HD_W_ModEr), HF_TS_ModEr), HD_TS_ModEr), KID_H_ModEr)
SC_LE_ModEr <- union(union(union(union(HF_W_ModEr, HD_W_ModEr), HF_H2O_ModEr), HD_H2O_ModEr), KID_LE_ModEr)
SC_NEE_ModEr <- union(union(union(union(HF_W_ModEr, HD_W_ModEr), HF_CO2_ModEr), HD_CO2_ModEr), KID_NEE_ModEr)


FOOTFLAG <- replace(zero_vector, union(union(HF_V_ModEr, HD_V_ModEr), KID_V_ModEr), 1)


#################################################################################################################################################################################################################
#
# Stationarity Test
#
#################################################################################################################################################################################################################

ST_NEE_SevEr <- which(ec_data[,"M98_Fc"] > 3)
ST_LE_SevEr <- which(ec_data[,"M98_LE"] > 3)
ST_H_SevEr <- which(ec_data[,"M98_H"] > 3)

ST_NEE_ModEr <- which(ec_data[,"M98_Fc"] > 2 & ec_data[,"M98_Fc"] <= 3)
ST_LE_ModEr <- which(ec_data[,"M98_LE"] > 2 & ec_data[,"M98_LE"] <= 3)
ST_H_ModEr <- which(ec_data[,"M98_H"] > 2 & ec_data[,"M98_H"] <= 3)

#################################################################################################################################################################################################################
#
# ITC Test
#
#################################################################################################################################################################################################################

ITC_SevEr <- which(ec_data[,"itc_w"] > 100)
ITC_ModEr <- which(ec_data[,"itc_w"] > 30 & ec_data[,"itc_w"] <= 100)

#################################################################################################################################################################################################################
#
#  SevEr and ModEr indices 
#
#################################################################################################################################################################################################################

NEE_SevEr_ind <- union(union(union(union(INT_NEE_SevEr, LSR_NEE_SevEr), SC_NEE_SevEr), ITC_SevEr), ST_NEE_SevEr)
NEE_SevEr_flag <- replace(zero_vector, NEE_SevEr_ind, rep(1, length(NEE_SevEr_ind)))
length(which(NEE_SevEr_flag==1))/N*100

LE_SevEr_ind <- union(union(union(union(INT_LE_SevEr, LSR_LE_SevEr), SC_LE_SevEr), ITC_SevEr),ST_LE_SevEr)
LE_SevEr_flag <- replace(zero_vector, LE_SevEr_ind, rep(1, length(LE_SevEr_ind)))
length(which(LE_SevEr_flag==1))/N*100

H_SevEr_ind <- union(union(union(union(INT_H_SevEr, LSR_H_SevEr), SC_H_SevEr), ITC_SevEr),ST_H_SevEr)
H_SevEr_flag <- replace(zero_vector, H_SevEr_ind, rep(1, length(H_SevEr_ind)))
length(which(H_SevEr_flag==1))/N*100


NEE_ModEr_ind <- union(union(union(union(INT_NEE_ModEr, LSR_NEE_ModEr), SC_NEE_ModEr), ITC_ModEr), ST_NEE_ModEr)
NEE_ModEr_flag <- replace(replace(zero_vector, NEE_ModEr_ind, rep(1, length(NEE_ModEr_ind))), NEE_SevEr_ind, NA)
length(which(NEE_ModEr_flag==1))/N*100

LE_ModEr_ind <- union(union(union(union(INT_LE_ModEr, LSR_LE_ModEr), SC_LE_ModEr), ITC_ModEr), ST_LE_ModEr)
LE_ModEr_flag <- replace(replace(zero_vector, LE_ModEr_ind, rep(1, length(LE_ModEr_ind))), LE_SevEr_ind, NA)
length(which(LE_ModEr_flag==1))/N*100

H_ModEr_ind <- union(union(union(union(INT_H_ModEr, LSR_H_ModEr), SC_H_ModEr), ITC_ModEr), ST_H_ModEr)
H_ModEr_flag <- replace(replace(zero_vector, H_ModEr_ind, rep(1, length(H_ModEr_ind))), H_SevEr_ind, NA)
length(which(H_ModEr_flag==1))/N*100

#################################################################################################################################################################################################################
#
# Filtering for severe flags
#
#################################################################################################################################################################################################################
NEE1 <- replace(as.vector(NEE_raw), INT_NEE_SevEr, NA)
NEE2 <- replace(NEE1, LSR_NEE_SevEr, NA)
NEE3 <- replace(NEE2, SC_NEE_SevEr,NA)
NEE4 <- replace(NEE3, ITC_SevEr, NA)

if(storage==TRUE) {
	oor_Sc <- c(which(ec_data[,"CO2str"]>70), which(ec_data[,"CO2str"]< -100));
	Sc0 <- replace(as.vector(ec_data[,"CO2str"]), oor_Sc, NA)
	Sc_Outlier <- as.numeric(Boxplot(as.vector(Sc0)~hod, range=3, id.n=Inf, main="CO2 Storage - Outlier Detection", ylab=expression(NEE~~(mu*mol~~CO[2]~~m^2~s^-1)), xlab="Hour of Day", cex=.5));
	Sc <- replace(na.approx(replace(as.vector(Sc0), Sc_Outlier, NA), na.rm=FALSE), which(is.na(ec_data[,"CO2str"])), NA)
	}

if(storage==FALSE) Sc <- rep(0,N)

NEE5 <- replace(NEE4, ST_NEE_SevEr, NA) + Sc


LE1 <- replace(as.vector(LE_raw), INT_LE_SevEr, NA)
LE2 <- replace(LE1, LSR_LE_SevEr, NA)
LE3 <- replace(LE2, SC_LE_SevEr,NA)
LE4 <- replace(LE3, ITC_SevEr, NA)
LE5 <- replace(LE4, ST_LE_SevEr, NA)

H1 <- replace(as.vector(H_raw), INT_H_SevEr, NA)
H2 <- replace(H1, LSR_H_SevEr, NA)
H3 <- replace(H2, SC_H_SevEr,NA)
H4 <- replace(H3, ITC_SevEr, NA)
H5 <- replace(H4, ST_H_SevEr, NA)


#################################################################################################################################################################################################################
#
## OUTLIER DETECTION 
#
#################################################################################################################################################################################################################

if(N < 48*10) {warning(call.=FALSE, "Outlier detection procedure as described in Vitale et al (2019) is performed when data cover a period of at least 10 consecutive days")}
	
if(any(apply(matrix(NEE5, nrow=48), MARGIN=1, function(x) sum(is.na(x))>=(N/48-3)))) {warning(call.=FALSE, "Too missing values for some half-hour in NEE flux variables! Outlier detection procedure described in Vitale et al (2019) is not performed")}
if(any(apply(matrix(LE5, nrow=48), MARGIN=1, function(x) sum(is.na(x))>=(N/48-3)))) {warning(call.=FALSE, "Too missing values for some half-hour in LE flux variables! Outlier detection procedure described in Vitale et al (2019) is not performed")}
if(any(apply(matrix(H5, nrow=48), MARGIN=1, function(x) sum(is.na(x))>=(N/48-3)))) {warning(call.=FALSE, "Too missing values for some half-hour in H flux variables! Outlier detection procedure described in Vitale et al (2019) is not performed")}

NEE_cleaned <- NEE5
spike1nee <- NA
spike2nee <- NA
LE_cleaned <- LE5
spike1le <- NA
spike2le <- NA
H_cleaned <- H5
spike1h <- NA
spike2h <- NA


if(N >= 48*10 & all(apply(matrix(NEE5, nrow=48), MARGIN=1, function(x) sum(is.na(x))<(N/48-3))) & all(apply(matrix(LE5, nrow=48), MARGIN=1, function(x) sum(is.na(x))<(N/48-3))) & all(apply(matrix(H5, nrow=48), MARGIN=1, function(x) sum(is.na(x))<(N/48-3)))){
	for (flux in 1:3){
		TS <- cbind(NEE5, LE5, H5)[,flux]
		if (flux==1) {flux_type <- "NEE"; C <- 1000}
		if (flux==2) {flux_type <- "LE"; C <- 1000}
		if (flux==3) {flux_type <- "H"; C <- 1000}

		spike1 <- c()
		if(length(TS>479) & all(apply(matrix(TS, nrow=48), MARGIN=1, function(x) length(which(is.na(x))))<ceiling(N/48))){
			mod <- stlplus(log10(ts(TS, start=c(1,1), frequency=48)+C), s.window=7, t.window=48*7+1, s.degree=0, fc.degree=c(1,0), fc.window=c(48*7+1,7) , outer=20, robust=TRUE, fc.name=c("Long-Run","Short-Run"));
			signal <- as.vector(10^(apply(cbind(mod$data[,2], mod$fc[,1:2]), MARGIN=1, FUN=sum))-C);
			res <- as.vector(TS - signal);

			## SPIKE DETECTION ASSUMENDO UNA LAPLACE DENSITY
			alpha <- 0.01;
			indq <- quantile(signal, seq(0,1,.1), na.rm=TRUE);
			for (i in 1:(length(indq)-1)){
				ind <- which(signal >= indq[i] & signal <= indq[i+1])
				if(length(na.omit(res[ind]))>9) {asd <- aout.laplace(res[ind], c(median(res[ind], na.rm=TRUE), Qn(na.omit(res[ind]))), alpha); spike1 <- c(spike1, ind[which(asd$is.outlier)])}
				if(length(na.omit(res[ind]))<10) {spike1 <- c(spike1, ind[which(!is.na(res[ind]))])}## to prevent cases where missing values unable the application of aaut.laplace
			}
		}
		if (flux==1){
			spike1nee <- spike1;
			spike2nee <- c()
			for (m in 1:(length(spike1nee))){
				spike2nee[m] <- ifelse(NEE_ModEr_flag[spike1nee[m]]==0, NA, spike1nee[m])
			};
			NEE_cleaned <- replace(as.vector(TS), spike2nee, NA)
		}

		if (flux==2){
			spike1le <- spike1;
			spike2le <- c()
			for (m in 1:(length(spike1le))){
				spike2le[m] <- ifelse(LE_ModEr_flag[spike1le[m]]==0, NA, spike1le[m])
			};
			LE_cleaned <- replace(as.vector(TS), spike2le, NA)
		}

		if (flux==3){
			spike1h <- spike1;
			spike2h <- c()
			for (m in 1:(length(spike1h))){
				spike2h[m] <- ifelse(H_ModEr_flag[spike1h[m]]==0, NA, spike1h[m])
			};
			H_cleaned <- replace(as.vector(TS), spike2h, NA)
		}
	}
}


#################################################################################################################################################################################################################
#
# Plot functions
#
#################################################################################################################################################################################################################
if (plotQC==TRUE) {
	YLIM <- c(max(-100, min(NEE_cleaned, na.rm=TRUE)*1.5), min(70, max(NEE_cleaned, na.rm=TRUE)*2.5))
	if(diff(range(NEE_cleaned, na.rm=TRUE)) < 20) step <- 5
	if(diff(range(NEE_cleaned, na.rm=TRUE)) > 20) step <- 10
	if(diff(range(NEE_cleaned, na.rm=TRUE)) > 30) step <- 20

	jpeg(paste(path_output, "/",site,"_NEE_QC_Details.jpeg", sep=""), width=480*2.5, height=480*2.5)
	par(mfrow=c(7,1), mar=c(0,4,0,0), oma=c(5,3,4,4), las=1, cex.axis=1.75, cex=1.25, cex.lab=1.5)
	plot(1:N, NEE_raw, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-100,50,step))
	legend("topright", paste(round(length(which(is.na(NEE_raw)))/N*100,1), "% of missing data", sep=""), bty="n", cex=1.5, text.col=1)
	mtext(side=3, paste(site, " from ",format(time(ec_data)[1],"%Y-%m-%d", tz="GMT"), " to ",format(time(ec_data)[N],"%Y-%m-%d", tz="GMT"),  sep=""), cex=2.5, line=1, font=2)
	mtext(side=4, "(a)", cex=2.5, bty="n", font=2, line=.6)
	box(lwd=1.5)
	plot(1:N, NEE1, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-100,50,step))
	legend("topright", paste(round(length(which(is.na(NEE1)))/N*100,1), "% of missing data after EC System Malfunctions Detection", sep=""), bty="n", cex=1.5, text.col=1)
	mtext(side=4, "(b)", cex=2.5, bty="n", font=2, line=.6)
	box(lwd=1.5)
	plot(1:N, NEE2, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-100,50,step))
	legend("topright", paste(round(length(which(is.na(NEE2)))/N*100,1), "% of missing data after Low Signal Resolution Test", sep=""), bty="n", cex=1.5, text.col=1)
	mtext(side=4, "(c)", cex=2.5, bty="n", font=2, line=.6)
	box(lwd=1.5)
	plot(1:N, NEE3, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-100,50,step))
	mtext(side=2, expression(NEE~~(mu*mol~~CO[2]~~m^-2~s^-1)), las=0, cex=2.5, line=4);
	legend("topright", paste(round(length(which(is.na(NEE3)))/N*100,1), "% of missing data after Structural Changes Tests", sep=""), bty="n", cex=1.5, text.col=1)
	mtext(side=4, "(d)", cex=2.5, bty="n", font=2, line=.6)
	box(lwd=1.5)
	plot(1:N, NEE4, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-100,50,step))
	legend("topright", paste(round(length(which(is.na(NEE4)))/N*100,1), "% of missing data after Integral Turbulence Characteristics Test", sep=""), bty="n", cex=1.5, text.col=1)
	mtext(side=4, "(e)", cex=2.5, bty="n", font=2, line=.6)
	box(lwd=1.5)
	plot(1:N, NEE5, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-100,50,step))
	legend("topright", paste(round(length(which(is.na(NEE5)))/N*100,1), "% of missing data after Stationarity Test", sep=""), bty="n", cex=1.5, text.col=1)
	mtext(side=4, "(f)", cex=2.5, bty="n", font=2, line=.6)
	box(lwd=1.5)
	plot(1:N, NEE_cleaned, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-100,50,step))
	legend("topright", paste(round(length(which(is.na(NEE_cleaned)))/N*100,1), "% of missing data after removal of outliers affected by ModEr", sep=""), bty="n", cex=1.5, text.col=1)
	axis(1, at=seq(1,N,48*7), labels=format(time(ec_data), "%j")[seq(1,N,48*7)], col="white", tick=FALSE, cex=2, line=-.5)
	mtext(side=1, "DoY", cex=2.5, line=3)
	mtext(side=4, "(g)", cex=2.5, bty="n", font=2, line=.6)
	box(lwd=1.5)
	dev.off()


	YLIM <- c(max(-100, min(NEE_cleaned, na.rm=TRUE)*2), min(70, max(NEE_cleaned, na.rm=TRUE)*1.25))
	if(diff(range(NEE_cleaned, na.rm=TRUE)) < 20) step <- 5
	if(diff(range(NEE_cleaned, na.rm=TRUE)) > 20) step <- 10
	if(diff(range(NEE_cleaned, na.rm=TRUE)) > 30) step <- 20
	
	jpeg(paste(path_output,  "/", site,"_NEE_QC_Synthesis.jpeg", sep=""), width=480*8, height=480*5)
	par(mfrow=c(3,1), mar=c(0,6,0,0), omi=c(3.5,1,2,.5), las=1, cex=3, cex.axis=2, cex.lab=2, lwd=5)
	plot(1:N, NEE_raw, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-100,70,step))
	legend("bottomright", paste("NEE - missing data ", round(length(which(is.na(NEE_raw)))/N*100,1), "%", sep=""), bty="n", cex=2, text.col=1) 
	mtext(side=3, paste(site, " from ",format(time(ec_data)[1],"%Y-%m-%d", tz="GMT"), " to ",format(time(ec_data)[N],"%Y-%m-%d", tz="GMT"),  sep=""), cex=8, line=1, font=2)
	plot(1:N, replace(NEE_raw, which(NEE_SevEr_flag==1), NA), type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	points(1:N, replace(NEE_raw, which(NEE_SevEr_flag==1), NA), pch=19, cex=.25)
	axis(2,  seq(-100,70,step))
	legend("bottomright", paste(round(length(which(is.na(NEE5)))/N*100,1), "% of missing data after rejection of fluxes affected by SevEr", sep=""), bty="n", cex=2, text.col=1) 
	mtext(side=2, expression(NEE~~~(mu*mol~~CO[2]~~m^-2~s^-1)), las=0, cex=7, line=4)
	plot(1:N, NEE_cleaned, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	points(1:N, NEE_cleaned, pch=19, cex=.25)
	axis(2,  seq(-100,70,step))
	legend("bottomright", paste(round(length(which(is.na(NEE_cleaned)))/N*100,1), "% of missing data after rejection of outlying fluxes affected by ModEr", sep=""), bty="n", cex=2, text.col=1)
	axis(1, at=seq(1,N,48*7), labels=format(time(ec_data), "%j")[seq(1,N,48*7)], col="white", tick=FALSE)
	mtext(side=1, "DoY", cex=7, line=4)
	dev.off()




	YLIM <- c(max(-300,min(LE_cleaned, na.rm=TRUE)*1.1), min(1250, max(LE_cleaned, na.rm=TRUE)*2.1))
	step <- 250

	jpeg(paste(path_output, "/",site,"_LE_QC_Details.jpeg", sep=""),  width=480*2.5, height=480*2.5)
	par(mfrow=c(7,1), mar=c(0,4,0,0), oma=c(5,3,4,4), las=1, cex.axis=1.75, cex=1.25, cex.lab=1.5)
	plot(1:N, LE_raw, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(LE_raw)))/N*100,1), "% of missing data", sep=""), bty="n", cex=1.5, text.col=1)
	mtext(side=3, paste(site, " from ",format(time(ec_data)[1],"%Y-%m-%d", tz="GMT"), " to ",format(time(ec_data)[N],"%Y-%m-%d", tz="GMT"),  sep=""), cex=2.5, line=1, font=2)
	mtext(side=4, "(a)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, LE1, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(LE1)))/N*100,1), "% of missing data after EC System Malfunctions Detection", sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(b)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, LE2, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(LE2)))/N*100,1), "% of missing data after Low Signal Resolution Test", sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(c)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, LE3, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	mtext(side=2, expression(LE~~(W~~m^-2)), las=0, cex=2.5, line=4.15, adj=0.1)
	legend("topright", paste(round(length(which(is.na(LE3)))/N*100,1), "% of missing data after Structural Changes Tests",  sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(d)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, LE4, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(LE4)))/N*100,1), "% of missing data after Integral Turbulence Characteristics Test",  sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(e)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, LE5, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste("after Stationarity Test (", round(length(which(is.na(LE5)))/N*100,1), "% of missing data)", sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(f)", cex=2.5, bty="n", font=2, line=.5)
	plot(1:N, LE_cleaned, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(LE_cleaned)))/N*100,1), "% of missing data after removal of outliers affected by ModEr",  sep=""), bty="n", cex=1.5, text.col=1) 
	axis(1, at=seq(1,N,48*7), labels=format(time(ec_data), "%j")[seq(1,N,48*7)], col="white", tick=FALSE, cex=2, line=-.5)
	mtext(side=1, "DoY", cex=2.5, line=3)
	mtext(side=4, "(g)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	dev.off()

	jpeg(paste(path_output, "/", site, "_LE_QC_Synthesis.jpeg", sep=""), width=480*8, height=480*5)
	par(mfrow=c(3,1), mar=c(0,6,0,0), omi=c(3.5,1,2,.5), las=1, cex=3, cex.axis=2, cex.lab=2, lwd=5)
	plot(1:N, LE_raw, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste("LE (", round(length(which(is.na(LE_raw)))/N*100,1), "% of missing data)", sep=""), bty="n", cex=2, text.col=1) 
	mtext(side=3, paste(site, " from ",format(time(ec_data)[1],"%Y-%m-%d", tz="GMT"), " to ",format(time(ec_data)[N],"%Y-%m-%d", tz="GMT"),  sep=""), cex=8, line=1, font=2)
	plot(1:N, replace(LE_raw, which(LE_SevEr_flag==1), NA), type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	points(1:N, replace(LE_raw, which(LE_SevEr_flag==1), NA), pch=19, cex=.25)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(LE5)))/N*100,1), "% of missing data after rejection of fluxes affected by SevEr", sep=""), bty="n", cex=2, text.col=1) 
	mtext(side=2, expression(LE~~~(W~~m^-2)), las=0, cex=7, line=4)
	plot(1:N, LE_cleaned, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	points(1:N, LE_cleaned, pch=19, cex=.25)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(LE_cleaned)))/N*100,1), "% of missing data after rejection of outlying fluxes affected by ModEr", sep=""), bty="n", cex=2, text.col=1)
	axis(1, at=seq(1,N,48*7), labels=format(time(ec_data), "%j")[seq(1,N,48*7)], col="white", tick=FALSE)
	mtext(side=1, "DoY", cex=7, line=4)
	dev.off()

	YLIM <- c(max(-300, min(H_cleaned, na.rm=TRUE)*1.1), min(1250, max(H_cleaned, na.rm=TRUE))*2.2)
	step <- 250

	jpeg(paste(path_output, "/",site,"_H_QC_Details.jpeg", sep=""), width=480*2.5, height=480*2.5)
	par(mfrow=c(7,1), mar=c(0,4,0,0), oma=c(5,3,4,4), las=1, cex.axis=1.75, cex=1.25, cex.lab=1.5)
	plot(1:N, H_raw, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(H_raw)))/N*100,1), "% of missing data", sep=""), bty="n", cex=1.5, text.col=1)
	#mtext(side=3, paste(site, " from ",format(time(ec_data)[1],"%Y-%m-%d", tz="GMT"), " to ",format(time(ec_data)[N],"%Y-%m-%d", tz="GMT"),  sep=""), cex=2.5, line=1, font=2)
	mtext(side=4, "(a)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, H1, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(H1)))/N*100,1), "% of missing data after EC System Malfunctions Detection", sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(b)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, H2, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(H2)))/N*100,1), "% of missing data after Low Signal Resolution Test", sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(c)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, H3, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	mtext(side=2, expression(H~~(W~~m^-2)), las=0, cex=2.5, line=4.5, adj=0.1)
	legend("topright", paste(round(length(which(is.na(H3)))/N*100,1), "% of missing data after Structural Changes Tests", sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(d)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, H4, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(H4)))/N*100,1), "% of missing data after Integral Turbulence Characteristics Test", sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(e)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	plot(1:N, H5, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(H5)))/N*100,1), "% of missing data after Stationarity Test", sep=""), bty="n", cex=1.5, text.col=1) 
	mtext(side=4, "(f)", cex=2.5, bty="n", font=2, line=.5)
	plot(1:N, H_cleaned, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(H_cleaned)))/N*100,1), "% of missing data after removal of outliers affected by ModEr", sep=""), bty="n", cex=1.5, text.col=1) 
	axis(1, at=seq(1,N,48*7), labels=format(time(ec_data), "%j")[seq(1,N,48*7)], col="white", tick=FALSE, cex=2, line=-.5)
	mtext(side=1, "DoY", cex=2.5, line=3)
	mtext(side=4, "(g)", cex=2.5, bty="n", font=2, line=.5)
	box(lwd=1.5)
	dev.off()

	jpeg(paste(path_output, "/", site, "_H_QC_Synthesis.jpeg", sep=""), width=480*8, height=480*5)
	par(mfrow=c(3,1), mar=c(0,6,0,0), omi=c(3.5,1,2,.5), las=1, cex=3, cex.axis=2, cex.lab=2, lwd=5)
	plot(1:N, H_raw, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste("H (", round(length(which(is.na(H_raw)))/N*100,1), "% of missing data", sep=""), bty="n", cex=2, text.col=1) 
	mtext(side=3, paste(site, " from ",format(time(ec_data)[1],"%Y-%m-%d", tz="GMT"), " to ",format(time(ec_data)[N],"%Y-%m-%d", tz="GMT"),  sep=""), cex=8, line=1, font=2)
	plot(1:N, replace(H_raw, which(H_SevEr_flag==1), NA), type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	points(1:N, replace(H_raw, which(H_SevEr_flag==1), NA), pch=19, cex=.25)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(H5)))/N*100,1), "% of missing data after rejection of fluxes affected by SevEr", sep=""), bty="n", cex=2, text.col=1) 
	mtext(side=2, expression(H~~~(W~~m^-2)), las=0, cex=7, line=4)
	plot(1:N, H_cleaned, type="l", xaxt="n", ylab="", xlab="", yaxt="n", main=NULL, ylim=YLIM)
	points(1:N, H_cleaned, pch=19, cex=.25)
	axis(2,  seq(-250,1000,step))
	legend("topright", paste(round(length(which(is.na(H_cleaned)))/N*100,1), "% of missing data after rejection outlying fluxes affected by ModEr", sep=""), bty="n", cex=2, text.col=1)
	axis(1, at=seq(1,N,48*7), labels=format(time(ec_data), "%j")[seq(1,N,48*7)], col="white", tick=FALSE)
	mtext(side=1, "DoY", cex=7, line=4)
	dev.off()

}



#################################################################################################################################################################################################################
#
# Exporting Cleaning DataSet 
#
#################################################################################################################################################################################################################

SIZE <- hz*60*30

nee_ind_miss <- union(which(FC_FMR_FLAG==2), which(FC_LGD_FLAG==2))
le_ind_miss <- union(which(LE_FMR_FLAG==2), which(LE_LGD_FLAG==2))
h_ind_miss <- union(which(H_FMR_FLAG==2), which(H_LGD_FLAG==2))

set2exp <- data.frame(
"TIMESTAMP_START" = as.numeric(format(time(ec_data), "%Y%m%d%H%M", tz="GMT")),
"TIMESTAMP_END" = as.numeric(format(time(ec_data)+1800, "%Y%m%d%H%M", tz="GMT")),

"H_UNCLEANED" = as.vector(replace(ec_data[,"H"], h_ind_miss, NA)),
"H" = as.vector(H_cleaned),
"H_DATA_FLAG" = replace(replace(replace(zero_vector, H_SevEr_ind, 2), spike2h, 1), h_ind_miss, 2),

"LE_UNCLEANED" = as.vector(replace(ec_data[,"LE"], le_ind_miss, NA)),
"LE" = as.vector(LE_cleaned),
"LE_DATA_FLAG" = replace(replace(replace(zero_vector, LE_SevEr_ind, 2), spike2le, 1), le_ind_miss, 2),

"FC" = as.vector(replace(ec_data[,"CO2flux"], nee_ind_miss, NA)),
"SC" = replace(Sc, nee_ind_miss, NA), #as.vector(replace(ec_data[,"CO2str"], nee_ind_miss, NA)),
"NEE_UNCLEANED" = as.vector(replace(ec_data[,"CO2flux"]+Sc, nee_ind_miss, NA)), #as.vector(replace(ec_data[,"CO2flux"]+ec_data[,"CO2str"], nee_ind_miss, NA)),
"NEE" = as.vector(NEE_cleaned),
"NEE_DATA_FLAG" = replace(replace(replace(zero_vector, NEE_SevEr_ind, 2), spike2nee, 1), nee_ind_miss, 2),

"H_OUTLYING_FLAG" = as.vector(replace(replace(zero_vector,spike1h,1), h_ind_miss, NA)),
"LE_OUTLYING_FLAG" = as.vector(replace(replace(zero_vector,spike1le,1), le_ind_miss, NA)),
"NEE_OUTLYING_FLAG" = as.vector(replace(replace(zero_vector,spike1nee,1), nee_ind_miss, NA)),

"H_OOR_FLAG" = as.vector(OoR_H), 
"LE_OOR_FLAG" = as.vector(OoR_LE), 
"NEE_OOR_FLAG" = as.vector(OoR_NEE), 

"H_FMR_STAT" = round(as.vector(H_FMR_STAT),1),
"H_FMR_FLAG" = H_FMR_FLAG,
"H_LGD_STAT" = round(as.vector(H_LGD_STAT),0),
"H_LGD_FLAG" = H_LGD_FLAG,
"LE_FMR_STAT" = round(as.vector(LE_FMR_STAT),1),
"LE_FMR_FLAG" = LE_FMR_FLAG,
"LE_LGD_STAT" = round(as.vector(LE_LGD_STAT),0),
"LE_LGD_FLAG" = LE_LGD_FLAG,
"FC_FMR_STAT" = round(as.vector(FC_FMR_STAT),1),
"FC_FMR_FLAG" = FC_FMR_FLAG,
"FC_LGD_STAT" = round(as.vector(FC_LGD_STAT),0),
"FC_LGD_FLAG" = FC_LGD_FLAG,

"SA_DIAG_FLAG" = as.vector(replace(replace(zero_vector, which(SA_Diag!=0), 2), h_ind_miss, NA)),
"GA_DIAG_FLAG" = as.vector(replace(replace(zero_vector, which(GA_Diag!=0), 2), union(le_ind_miss, nee_ind_miss), NA)),

"WD" = as.vector(replace(ec_data[,"WDir"], h_ind_miss, NA)),
"WSECT_FLAG" = as.vector(replace(replace(zero_vector, WDir2Exc, 2), h_ind_miss, NA)),

"H_LSR_STAT" = as.vector(replace(ec_data[,"LSR_H"], h_ind_miss, NA)),
"H_LSR_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"LSR_H"]<=0.995), 1), which(ec_data[,"LSR_H"]<0.99), 2), h_ind_miss, NA)),
"LE_LSR_STAT" = as.vector(replace(ec_data[,"LSR_LE"], le_ind_miss, NA)),
"LE_LSR_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"LSR_LE"]<=0.995), 1), which(ec_data[,"LSR_LE"]<0.99), 2), le_ind_miss, NA)),
"FC_LSR_STAT" = as.vector(replace(ec_data[,"LSR_Fc"], nee_ind_miss, NA)),
"FC_LSR_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"LSR_Fc"]<=0.995), 1), which(ec_data[,"LSR_Fc"]<0.99), 2), nee_ind_miss, NA)),

"W_HF5_STAT" = as.vector(replace(round(ec_data[,"HF5_w"]/(hz*3*6),1), h_ind_miss, NA)),
"W_HF5_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HF5_w"]>SCTH1/2), 1), which(ec_data[,"HF5_w"]>SCTH1), 2), h_ind_miss, NA)),
"W_HF10_STAT" = as.vector(replace(round(ec_data[,"HF10_w"]/(hz*3*6),1), h_ind_miss, NA)),
"W_HF10_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HF10_w"]>SCTH2/2), 1), which(ec_data[,"HF10_w"]>SCTH2), 2), h_ind_miss, NA)),
"W_HD5_STAT" = as.vector(replace(round(ec_data[,"HD5_w"]/(hz*3*6),1), h_ind_miss, NA)),
"W_HD5_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HD5_w"]>SCTH1/2), 1), which(ec_data[,"HD5_w"]>SCTH1), 2), h_ind_miss, NA)),
"W_HD10_STAT" = as.vector(replace(round(ec_data[,"HD10_w"]/(hz*3*6),1), h_ind_miss, NA)),
"W_HD10_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HD10_w"]>SCTH2/2), 1), which(ec_data[,"HD10_w"]>SCTH2), 2), h_ind_miss, NA)),

"W_ACF_STAT" =as.vector(replace(round(ec_data[,"ACF_w"],2), h_ind_miss, NA)),
"W_ACF_FLAG" = as.vector(replace(replace(replace(zero_vector, ACF_W_ModEr, 1), ACF_W_SevEr, 2), h_ind_miss, NA)),
"W_DDI_STAT" =as.vector(replace(round(ec_data[,"DDI_w"],2), h_ind_miss, NA)),
"W_DDI_FLAG" = as.vector(replace(replace(replace(zero_vector, DDI_W_ModEr, 1), DDI_W_SevEr, 2), h_ind_miss, NA)),

"T_SONIC_HF5_STAT" = as.vector(replace(round(ec_data[,"HF5_ts"]/(hz*3*6),1), h_ind_miss, NA)),
"T_SONIC_HF5_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HF5_ts"]>SCTH1/2), 1), which(ec_data[,"HF5_ts"]>SCTH1), 2), h_ind_miss, NA)),
"T_SONIC_HF10_STAT" = as.vector(replace(round(ec_data[,"HF10_ts"]/(hz*3*6),1), h_ind_miss, NA)),
"T_SONIC_HF10_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HF10_ts"]>SCTH2/2), 1), which(ec_data[,"HF10_ts"]>SCTH2), 2), h_ind_miss, NA)),
"T_SONIC_HD5_STAT" = as.vector(replace(round(ec_data[,"HD5_ts"]/(hz*3*6),1), h_ind_miss, NA)),
"T_SONIC_HD5_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HD5_ts"]>SCTH1/2), 1), which(ec_data[,"HD5_ts"]>SCTH1), 2), h_ind_miss, NA)),
"T_SONIC_HD10_STAT" = as.vector(replace(round(ec_data[,"HD10_ts"]/(hz*3*6),1), h_ind_miss, NA)),
"T_SONIC_HD10_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HD10_ts"]>SCTH2/2), 1), which(ec_data[,"HD10_ts"]>SCTH2), 2), h_ind_miss, NA)),

"T_SONIC_ACF_STAT" =as.vector(replace(round(ec_data[,"ACF_ts"],2), h_ind_miss, NA)),
"T_SONIC_ACF_FLAG" = as.vector(replace(replace(replace(zero_vector, ACF_TS_ModEr, 1), ACF_TS_SevEr, 2), h_ind_miss, NA)),
"T_SONIC_DDI_STAT" =as.vector(replace(round(ec_data[,"DDI_ts"],2), h_ind_miss, NA)),
"T_SONIC_DDI_FLAG" = as.vector(replace(replace(replace(zero_vector, DDI_TS_ModEr, 1), DDI_TS_SevEr, 2), h_ind_miss, NA)),

"H2O_HF5_STAT" = as.vector(replace(round(ec_data[,"HF5_h2o"]/(hz*3*6),1), le_ind_miss, NA)),
"H2O_HF5_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HF5_h2o"]>SCTH1/2), 1), which(ec_data[,"HF5_h2o"]>SCTH1), 2), le_ind_miss, NA)),
"H2O_HF10_STAT" = as.vector(replace(round(ec_data[,"HF10_h2o"]/(hz*3*6),1), le_ind_miss, NA)),
"H2O_HF10_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HF10_h2o"]>SCTH2/2), 1), which(ec_data[,"HF10_h2o"]>SCTH2), 2), le_ind_miss, NA)),
"H2O_HD5_STAT" = as.vector(replace(round(ec_data[,"HD5_h2o"]/(hz*3*6),1), le_ind_miss, NA)),
"H2O_HD5_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HD5_h2o"]>SCTH1/2), 1), which(ec_data[,"HD5_h2o"]>SCTH1), 2), le_ind_miss, NA)),
"H2O_HD10_STAT" = as.vector(replace(round(ec_data[,"HD10_h2o"]/(hz*3*6),1), le_ind_miss, NA)),
"H2O_HD10_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HD10_h2o"]>SCTH2/2), 1), which(ec_data[,"HD10_h2o"]>SCTH2), 2), le_ind_miss, NA)),

"H2O_ACF_STAT" =as.vector(replace(round(ec_data[,"ACF_h2o"],2), le_ind_miss, NA)),
"H2O_ACF_FLAG" = as.vector(replace(replace(replace(zero_vector, ACF_H2O_ModEr, 1), ACF_H2O_SevEr, 2), le_ind_miss, NA)),
"H2O_DDI_STAT" =as.vector(replace(round(ec_data[,"DDI_h2o"],2), le_ind_miss, NA)),
"H2O_DDI_FLAG" = as.vector(replace(replace(replace(zero_vector, DDI_H2O_ModEr, 1), DDI_H2O_SevEr, 2), le_ind_miss, NA)),

"CO2_HF5_STAT" = as.vector(replace(round(ec_data[,"HF5_co2"]/(hz*3*6),1), nee_ind_miss, NA)),
"CO2_HF5_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HF5_co2"]>SCTH1/2), 1), which(ec_data[,"HF5_co2"]>SCTH1), 2), nee_ind_miss, NA)),
"CO2_HF10_STAT" = as.vector(replace(round(ec_data[,"HF10_co2"]/(hz*3*6),1), nee_ind_miss, NA)),
"CO2_HF10_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HF10_co2"]>SCTH2/2), 1), which(ec_data[,"HF10_co2"]>SCTH2), 2), nee_ind_miss, NA)),
"CO2_HD5_STAT" = as.vector(replace(round(ec_data[,"HD5_co2"]/(hz*3*6),1), nee_ind_miss, NA)),
"CO2_HD5_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HD5_co2"]>SCTH1/2), 1), which(ec_data[,"HD5_co2"]>SCTH1), 2), nee_ind_miss, NA)),
"CO2_HD10_STAT" = as.vector(replace(round(ec_data[,"HD10_co2"]/(hz*3*6),1), nee_ind_miss, NA)),
"CO2_HD10_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"HD10_co2"]>SCTH2/2), 1), which(ec_data[,"HD10_co2"]>SCTH2), 2), nee_ind_miss, NA)),

"CO2_ACF_STAT" =as.vector(replace(round(ec_data[,"ACF_co2"],2), nee_ind_miss, NA)),
"CO2_ACF_FLAG" = as.vector(replace(replace(replace(zero_vector, ACF_CO2_ModEr, 1), ACF_CO2_SevEr, 2), nee_ind_miss, NA)),
"CO2_DDI_STAT" =as.vector(replace(round(ec_data[,"DDI_co2"],2), nee_ind_miss, NA)),
"CO2_DDI_FLAG" = as.vector(replace(replace(replace(zero_vector, DDI_CO2_ModEr, 1), DDI_CO2_SevEr, 2), nee_ind_miss, NA)),

"W_KID_STAT" = as.vector(replace(ec_data[,"KID1_w"], h_ind_miss, NA)),
"W_KID_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"KID1_w"]>30), 1), which(ec_data[,"KID1_w"]>50), 2), h_ind_miss, NA)),
"T_SONIC_KID_STAT" = as.vector(replace(ec_data[,"KID1_ts"], h_ind_miss, NA)),
"T_SONIC_KID_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"KID1_ts"]>30), 1), which(ec_data[,"KID1_ts"]>50), 2), h_ind_miss, NA)),
"H2O_KID_STAT" = as.vector(replace(ec_data[,"KID1_h2o"], le_ind_miss, NA)),
"H2O_KID_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"KID1_h2o"]>30), 1), which(ec_data[,"KID1_h2o"]>50), 2), le_ind_miss, NA)),
"CO2_KID_STAT" = as.vector(replace(ec_data[,"KID1_co2"], nee_ind_miss, NA)),
"CO2_KID_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"KID1_co2"]>30), 1), which(ec_data[,"KID1_co2"]>50), 2), nee_ind_miss, NA)),

"ITC_STAT" = as.vector(replace(ec_data[,"itc_w"], h_ind_miss, NA)),
"ITC_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"itc_w"]>30), 1), which(ec_data[,"itc_w"]>100), 2), h_ind_miss, NA)),

"H_SCF_STAT" = as.vector(replace(ec_data[,"Hscf"], h_ind_miss, NA)),
"LE_SCF_STAT" = as.vector(replace(ec_data[,"H2Oscf"], le_ind_miss, NA)),
"FC_SCF_STAT" = as.vector(replace(ec_data[,"CO2scf"], nee_ind_miss, NA)),

"H_M98_STAT" = as.vector(replace(ec_data[,"M98_H"], h_ind_miss, NA)),
"H_M98_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"M98_H"]>2), 1), which(ec_data[,"M98_H"]>3), 2), h_ind_miss, NA)),

"LE_M98_STAT" = as.vector(replace(ec_data[,"M98_LE"], le_ind_miss, NA)),
"LE_M98_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"M98_LE"]>2), 1), which(ec_data[,"M98_LE"]>3), 2), le_ind_miss, NA)),

"FC_M98_STAT" = as.vector(replace(ec_data[,"M98_Fc"], nee_ind_miss, NA)),
"FC_M98_FLAG" = as.vector(replace(replace(replace(zero_vector, which(ec_data[,"M98_Fc"]>2), 1), which(ec_data[,"M98_Fc"]>3), 2), nee_ind_miss, NA)),

#"H_SSITC_TEST" = as.vector(replace(ec_data[,"qcH"], h_ind_miss, NA)),
#"LE_SSITC_TEST" = as.vector(replace(ec_data[,"qcLE"], le_ind_miss, NA)),
#"NEE_SSITC_TEST" = as.vector(replace(ec_data[,"qcCO2flux"], nee_ind_miss, NA)),

"CO2" = as.vector(replace(ec_data[,"CO2mf"], nee_ind_miss, NA)),
"CO2_SIGMA" = as.vector(replace(sqrt(ec_data[,"co2_var"]), nee_ind_miss, NA)),
"H2O" = as.vector(replace(ec_data[,"H2Omf"], le_ind_miss, NA)),
"H2O_SIGMA" = as.vector(replace(sqrt(ec_data[,"h2o_var"]), le_ind_miss, NA)),
"T_SONIC" = as.vector(replace(ec_data[,"TSonic"], h_ind_miss, NA)),
"T_SONIC_SIGMA" = as.vector(replace(sqrt(ec_data[,"ts_var"]), h_ind_miss, NA)),
"WS" = as.vector(replace(ec_data[,"WSpeed"], h_ind_miss, NA)),
"USTAR" = as.vector(replace(ec_data[,"ustar"], h_ind_miss, NA)),
"W_SIGMA" = as.vector(replace(sqrt(ec_data[,"w_var"]), h_ind_miss, NA)),
"U_SIGMA" = as.vector(replace(sqrt(ec_data[,"u_var"]), h_ind_miss, NA)),
"V_SIGMA" = as.vector(replace(sqrt(ec_data[,"v_var"]), h_ind_miss, NA)),
"ZL" = as.vector(replace(ec_data[,"Stability"], h_ind_miss, NA)),
"MO_LENGTH" = as.vector(replace(ec_data[,"MOL"], h_ind_miss, NA)),
"AT" = as.vector(replace(ec_data[,"AT"], h_ind_miss, NA)),
"AP" = as.vector(replace(ec_data[,"AP"], h_ind_miss, NA)),
"RHO" = as.vector(replace(ec_data[,"rho"], h_ind_miss, NA)),
"CP" = as.vector(replace(ec_data[,"cp"], h_ind_miss, NA)),
"CANOPY_HEIGHT" = as.vector(ec_data[,"canopy_height"]),
"SA_HEIGHT" = as.vector(ec_data[,"SA_height"]),
"SA_NORTH_OFFSET" = as.vector(ec_data[,"SA_NorthOffset"]),
"SA_INVALID_WIND_SECTOR_c1" = as.vector(ec_data[,"WS2E_c1"]),
"SA_INVALID_WIND_SECTOR_w1" = as.vector(ec_data[,"WS2E_w1"]),
"SA_INVALID_WIND_SECTOR_c2" = as.vector(ec_data[,"WS2E_c2"]),
"SA_INVALID_WIND_SECTOR_w2" = as.vector(ec_data[,"WS2E_w2"]),
"SA_INVALID_WIND_SECTOR_c3" = as.vector(ec_data[,"WS2E_c3"]),
"SA_INVALID_WIND_SECTOR_w3" = as.vector(ec_data[,"WS2E_w3"]))

if(!is.null(path_output) & !is.null(FileName)) fwrite(set2exp, paste0(path_output, "/", FileName, ".csv"), sep=",", quote=FALSE, col.names=TRUE, row.names=FALSE, na="-9999")
return(set2exp)
}
