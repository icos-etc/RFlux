
ecworkset <- function(path_EPout, path_EPqc, path_EPmd, path_QCstat, path_output=NULL, FileName=NULL){

EPout <- fread(path_EPout, header=FALSE, skip=3, na.strings=c("-9999", "-9999.0"), data.table=FALSE)
colnames(EPout) <- as.vector(scan(path_EPout, nlines=1, skip=1, sep=",", what="character"))
timestamp_EPout <- as.POSIXct(paste(EPout[,2], EPout[,3], sep=" "), format="%Y-%m-%d %H:%M", tz="GMT")-1800

EPqc <- fread(path_EPqc, header=FALSE, skip=3, na.strings=c("-9999", "-9999.0"), data.table=FALSE)
colnames(EPqc) <- as.vector(scan(path_EPqc, nlines=1, skip=1, sep=",", what="character"))
timestamp_EPqc <- as.POSIXct(paste(EPqc[,2], EPqc[,3], sep=" "), format="%Y-%m-%d %H:%M", tz="GMT")-1800

EPmd <- fread(path_EPmd, header=FALSE, skip=1, na.strings=c("-9999", "-9999.0"), data.table=FALSE)
colnames(EPmd) <- as.vector(scan(path_EPmd, nlines=1, skip=0, sep=",", what="character"))
timestamp_EPmd <- as.POSIXct(paste(EPmd[,2], EPmd[,3], sep=" "), format="%Y-%m-%d %H:%M", tz="GMT")-1800

QCstat <- fread(path_QCstat, header=TRUE, data.table=FALSE, na.strings=c(NA, "-9999"))
timestamp_QCstat <- as.POSIXct(as.character(QCstat[,1]), format="%Y%m%d%H%M", tz="GMT")
QCstat.xts <- xts(QCstat[,-1], order.by=timestamp_QCstat)

VarSelOut <- c(
which(colnames(EPout)=="daytime"),
which(colnames(EPout)=="file_records"),
which(colnames(EPout)=="used_records"),
which(colnames(EPout)=="(z-d)/L"),
which(colnames(EPout)=="H"),
which(colnames(EPout)=="qc_H"),
which(colnames(EPout)=="rand_err_H"),
which(colnames(EPout)=="LE"),
which(colnames(EPout)=="qc_LE"),
which(colnames(EPout)=="rand_err_LE"),
which(colnames(EPout)=="co2_flux"),
which(colnames(EPout)=="qc_co2_flux"),
which(colnames(EPout)=="rand_err_co2_flux"),
which(colnames(EPout)=="co2_strg"),
which(colnames(EPout)=="u*"),
which(colnames(EPout)=="u_rot"),
which(colnames(EPout)=="w_rot"),
which(colnames(EPout)=="wind_dir"),
which(colnames(EPout)=="wind_speed"),
which(colnames(EPout)=="u_var"),
which(colnames(EPout)=="v_var"),
which(colnames(EPout)=="w_var"),
which(colnames(EPout)=="L"),
which(colnames(EPout)=="sonic_temperature"),
which(colnames(EPout)=="air_temperature"),
which(colnames(EPout)=="air_pressure"),
which(colnames(EPout)=="air_density"),
which(colnames(EPout)=="air_heat_capacity"),
which(colnames(EPout)=="ts_var"),
which(colnames(EPout)=="co2_mole_fraction"),
which(colnames(EPout)=="co2_var"),
which(colnames(EPout)=="h2o_mole_fraction"),
which(colnames(EPout)=="h2o_var"),
which(colnames(EPout)=="co2_scf"),
which(colnames(EPout)=="h2o_scf"),
which(colnames(EPout)=="H_scf"))


VarSelQC <- c(
which(colnames(EPqc)=="dev(w)")[1], ## Instationary Test
which(colnames(EPqc)=="dev(ts)")[1],
which(colnames(EPqc)=="dev(co2)"),
which(colnames(EPqc)=="dev(h2o)"),
which(colnames(EPqc)=="dev(w/ts)"),
which(colnames(EPqc)=="dev(w/co2)"),
which(colnames(EPqc)=="dev(w/h2o)"),
which(colnames(EPqc)=="dev(w)")[2]) ## ITC

VarSelMD <- c(
which(colnames(EPmd)=="acquisition_frequency"),
which(colnames(EPmd)=="canopy_height"),
which(colnames(EPmd)=="master_sonic_height"),
which(colnames(EPmd)=="master_sonic_north_offset"))

############################################################################################################################################################################################################################
#
### IRGA Diagnostic Management
#
############################################################################################################################################################################################################################
IRGA <- substr(EPmd[1,"co2_irga_model"],1,6)

if(IRGA=="li7200"){
	GA_TCellDiag <- apply(cbind(EPout[,"t_out_LI-7200"], EPout[,"t_in_LI-7200"]), 1, function(x) min(x, na.rm=TRUE));
	GA_DiagVar <- cbind(EPout[,"head_detect_LI-7200"], GA_TCellDiag, EPout[,"aux_in_LI-7200"], EPout[,"delta_p_LI-7200"], EPout[,"chopper_LI-7200"], EPout[,"detector_LI-7200"], EPout[,"pll_LI-7200"], EPout[,"sync_LI-7200"]);
	GA_Diag <- apply(GA_DiagVar, MARGIN=1, function(x) sum(x, na.rm=TRUE))
	}

if(IRGA=="li7500"){
	GA_DiagVar <- cbind(EPout[,"chopper_LI-7500"], EPout[,"detector_LI-7500"], EPout[,"pll_LI-7500"], EPout[,"sync_LI-7500"]);
	GA_Diag <- apply(GA_DiagVar, MARGIN=1, function(x) sum(x, na.rm=TRUE))
	}

GA_Diag.xts <- xts(GA_Diag,  order.by=timestamp_EPout)
if (IRGA!="li7200" & IRGA!="li7500") GA_Diag.xts <- xts(rep(0, length(timestamp_EPout)), order.by=timestamp_EPout)

############################################################################################################################################################################################################################

EPout.xts <- xts(EPout[,VarSelOut], order.by=timestamp_EPout)
EPqc.xts <- xts(EPqc[,VarSelQC], order.by=timestamp_EPqc)
EPmd.xts <- xts(EPmd[,VarSelMD], order.by=timestamp_EPmd)
QCstat.xts <- xts(QCstat[,-1], order.by=timestamp_QCstat)


ecworkset.xts <- merge(EPout.xts, GA_Diag.xts, EPqc.xts, EPmd.xts, QCstat.xts) 
set2exp <- data.frame(format(time(ecworkset.xts),"%Y%m%d%H%M", tz="GMT"), coredata(ecworkset.xts))
colnames(set2exp) <- c("TIMESTAMP", "DNtime", "file_records", "used_records","Stability","H", "qcH", "ruH", "LE", "qcLE", "ruLE", "CO2flux", "qcCO2flux", "ruCO2", "CO2str", "ustar", "U","W","WDir", "WSpeed", "u_var", "v_var", "w_var", "MOL", "TSonic", "AT", "AP", "rho", "cp", "ts_var", "CO2mf", "co2_var", "H2Omf", "h2o_var", "CO2scf", "H2Oscf", "Hscf", "GADiag", "dw","dts","dco2","dh2o","dwts","dwco2","dwh2o","itc_w", "acquisition_frequency", "canopy_height", "SA_height", "SA_NorthOffset", colnames(QCstat)[-1])


if(!is.null(path_output) & !is.null(FileName)) fwrite(set2exp, paste0(path_output, "/", FileName, ".csv"), sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE, na="-9999")

return(set2exp)
}
