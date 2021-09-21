
qcStat <- function(path_rawdata, ext_tstamp=c("START", "END"), path_output=NULL, FileName=NULL){

if(ext_tstamp=="START") tstamp <- as.character(format(strptime(str_sub(path_rawdata, -20, -9), format="%Y%m%d%H%M", tz="GMT"), "%Y%m%d%H%M", tz="GMT"))
if(ext_tstamp=="END") tstamp <- as.character(format(strptime(str_sub(path_rawdata, -20, -9), format="%Y%m%d%H%M", tz="GMT")-1800, "%Y%m%d%H%M", tz="GMT"))

##################################################################################################################################################################################################################################################
#	Raw Data Processing
##################################################################################################################################################################################################################################################

raw_data <- fread(path_rawdata, sep=",", header=TRUE, data.table=FALSE, na.strings=c("-9999"))
n <- nrow(raw_data)
ifelse(n > 19000, HZ <- 20, HZ <- 10)
N <- max(HZ*60*30, n)

ifelse(n<N, delta <- N-n, delta <- 0)
ifelse(length(which(is.na(raw_data$W+raw_data$T_SONIC)))==n, {fmr_h <- 100; lgd_h <- 1800}, {stat_h <- statsNA(raw_data$W+raw_data$T_SONIC, print_only=FALSE); fmr_h <- (stat_h$number_NAs+delta)/N*100; lgd_h <- max(delta, stat_h$longest_na_gap,na.rm=TRUE)/HZ})
ifelse(length(which(is.na(raw_data$W+raw_data$CO2)))==n, {fmr_fc <- 100; lgd_fc <- 1800}, {stat_fc <- statsNA(raw_data$W+raw_data$CO2, print_only=FALSE); fmr_fc <- (stat_fc$number_NAs+delta)/N*100; lgd_fc <- max(delta, stat_fc$nlongest_na_gap, na.rm=TRUE)/HZ})
ifelse(length(which(is.na(raw_data$W+raw_data$H2O)))==n, {fmr_le <- 100; lgd_le <- 1800}, {stat_le <- statsNA(raw_data$W+raw_data$H2O, print_only=FALSE); fmr_le <- (stat_le$number_NAs+delta)/N*100; lgd_le <- max(delta, stat_le$longest_na_gap, na.rm=TRUE)/HZ})


	L <- 25 ## lag max for LSR test
	ifelse(is.null(raw_data$V) | length(which(is.na(raw_data$V)))>N*0.95, 
	{IPT_v <- rep(NA,10)},
	{IPT_v <- inst_prob_test(raw_data$V)})
	
	ifelse(fmr_h > 15 | lgd_h > 180, 
	{D0_h <- NA; lrt_h <- NA; IPT_w <- rep(NA,10); IPT_ts <- rep(NA,10); M98_h <- NA; COV_wts <- NA; SADiag <- NA}, 
	{
	ind_w <- which(diff(raw_data$W)==0)+1;
	ind_ts <- which(diff(raw_data$T_SONIC)==0)+1;
	D0_h <- max(length(ind_w), length(ind_ts));
	CORori <- ccf(raw_data$W, raw_data$T_SONIC, na.action=na.pass, L, plot=FALSE)$acf;
	ifelse(D0_h < N*0.9, {ifelse(D0_h > 0, {
	CORsub <- ccf(replace(raw_data$W, ind_w, NA), replace(raw_data$T_SONIC, ind_ts, NA), na.action=na.pass, L, plot=FALSE)$acf;
	lrt_h <- summary(lm(CORori~CORsub-1))$r.squared},
	lrt_h <- 1)}, lrt_h <- -1);

	IPT_w <- inst_prob_test(raw_data$W);
	IPT_ts <- inst_prob_test(raw_data$T_SONIC);
	M98_h <- mahrt(data.frame(raw_data$W, raw_data$T_SONIC));
	COV_wts <- cov(raw_data$W, raw_data$T_SONIC, use="complete.obs");
	SADiag <- length(which(raw_data$SA_DIAG==0))
	})

	ifelse(fmr_fc > 15 | lgd_fc > 180,
	{D0_fc <- NA; lrt_fc <- NA; IPT_co2 <- rep(NA,10); M98_fc <- NA; COV_wco2 <- NA}, 
	{
	ind_w <- which(diff(raw_data$W)==0)+1;
	ind_co2 <- which(diff(raw_data$CO2)==0)+1;
	D0_fc <- max(length(ind_w),length(ind_co2));
	CORori <- ccf(raw_data$W, raw_data$CO2, na.action=na.pass, L, plot=FALSE)$acf;
	ifelse(D0_fc < N*0.9, {ifelse(D0_fc > 0, {
	CORsub <- ccf(replace(raw_data$W, ind_w, NA), replace(raw_data$CO2, ind_co2, NA), na.action=na.pass, L, plot=FALSE)$acf;
	lrt_fc <- summary(lm(CORori~CORsub-1))$r.squared},
	lrt_fc <- 1)}, lrt_fc <- -1);

	IPT_co2 <- inst_prob_test(raw_data$CO2);
	M98_fc <- mahrt(data.frame(raw_data$W,raw_data$CO2));
	COV_wco2 <- cov(raw_data$W,raw_data$CO2, use="complete.obs")
	})

	ifelse(fmr_le > 15 | lgd_le > 180,
	{D0_le <- NA; lrt_le <- NA; IPT_h2o <- rep(NA,10); M98_le <- NA; COV_wh2o <- NA}, 
	{
	ind_w <- which(diff(raw_data$W)==0)+1;
	ind_h2o <- which(diff(raw_data$H2O)==0)+1;
	D0_le <- max(length(ind_w), length(ind_h2o));
	CORori <-  ccf(raw_data$W, raw_data$H2O, na.action=na.pass, L, plot=FALSE)$acf;
	ifelse(D0_le < N*0.9, {ifelse(D0_le > 0, {
	CORsub <- ccf(replace(raw_data$W, ind_w, NA), replace(raw_data$H2O, ind_h2o, NA), na.action=na.pass, L, plot=FALSE)$acf;
	lrt_le <- summary(lm(CORori~CORsub-1))$r.squared},
	lrt_le <- 1)}, lrt_le <- -1);

	IPT_h2o <- inst_prob_test(raw_data$H2O);
	M98_le <- mahrt(data.frame(raw_data$W,raw_data$H2O));
	COV_wh2o <- cov(raw_data$W,raw_data$H2O, use="complete.obs")
	})


	results <- unlist(c(tstamp, SADiag, fmr_h, lgd_h, fmr_fc, lgd_fc, fmr_le, lgd_le,
	IPT_v, 
	IPT_w,
	IPT_ts,  COV_wts, D0_h, lrt_h, M98_h,
	IPT_co2, COV_wco2, D0_fc, lrt_fc, M98_fc,
	IPT_h2o, COV_wh2o, D0_le, lrt_le, M98_le), use.names=FALSE)

	names(results) <- c("TSTAMP", "SADiag", "FMR_H", "LGD_H", "FMR_Fc", "LGD_Fc", "FMR_LE", "LGD_LE", 
						paste(c("Skew", "Kurt", "KID0", "KID1", "HF5",  "HF10",  "HD5", "HD10", "ACF", "DDI"),"_v",sep=""),
						paste(c("Skew", "Kurt", "KID0", "KID1", "HF5",  "HF10",  "HD5", "HD10", "ACF", "DDI"),"_w",sep=""),
						paste(c("Skew", "Kurt", "KID0", "KID1", "HF5",  "HF10",  "HD5", "HD10", "ACF", "DDI"),"_ts",sep=""), "COV_wts", "N0_H", "LSR_H", "M98_H",
						paste(c("Skew", "Kurt", "KID0", "KID1", "HF5",  "HF10",  "HD5", "HD10", "ACF", "DDI"),"_co2",sep=""), "COV_wco2", "N0_Fc", "LSR_Fc", "M98_Fc",
						paste(c("Skew", "Kurt", "KID0", "KID1", "HF5",  "HF10",  "HD5", "HD10", "ACF", "DDI"),"_h2o",sep=""), "COV_wh2o", "N0_LE", "LSR_LE", "M98_LE")

if(!is.null(path_output) & !is.null(FileName)) write.table(t(results), paste0(path_output, "/", FileName,".csv"), quote=FALSE, sep=",", row.names=FALSE)
	
return(results)
}
