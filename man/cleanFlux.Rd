\name{cleanFlux}
\alias{cleanFlux}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cleaning eddy covariane flux measurements
}
\description{
This is the main function of the RFlux library. It performs the data cleaning procedure described by Vitale et al (2019).
}
\usage{
cleanFlux(path_workset, path_ecmd, path_output=NULL, FileName=NULL,
          plotQC=FALSE, storage=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_workset}{
  path where the workset file generated from function \code{ecworkset} is stored.
}
  \item{path_ecmd}{
  path where the eddy covariance metadata file (\emph{CC-xxx}_ecmd.csv) is stored.
}
  \item{path_output}{
  path where the output file will be stored. Default is NULL.
}
  \item{FileName}{
  file name for the output file. Deafult is NULL.
}
  \item{plotQC}{
  Logical. Should the details of data cleaning procedure be saved. If TRUE two .jpeg files (Details and Synthesis) for each flux variables will be stored in the path specified by path_output. Default is FALSE.
}
  \item{storage}{
  Logical. Should the Net Ecosystem Exchange flux take into account the CO2 storage term. Default is FALSE.
}
}
\details{
Comparing statistics estimated by \code{\link{qcStat}} with two threshold values, each test returns one of 3 possible statements:
\describe{
\item{SevEr:}{if the test provides strong evidence about the presence of a specific source of systematic error.}
\item{ModEr:}{if the test provides only weak evidence about the presence of a specific source of systematic error.}
\item{NoEr:}{if the test does not provide evidence about the presence of a specific source of systematic error.}
}
The data cleaning is based on a two-step procedure. In the first stage, fluxes that inherited at least one SevEr statement are rejected, while fluxes that inherited no SevEr statements and any number of ModEr statements are retained. In the second stage, flux data that inherited no SevEr statement are subject to an outlier detection procedure and only flux data that are both detected as outlier and inherited at least a ModEr statement are conclusively rejected. This implies that data points that inherited any number of ModEr statements but were not detected to be outliers, as well as outliers which showed no evidence of systematic errors, are retained in the dataset and can be used for any analysis or modeling purposes.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a dataframe containing:
\item{TIMESTAMP_START}{ISO timestamp start of averaging period (format: \emph{yyyymmddHHMM}).}
\item{TIMESTAMP_END}{ISO timestamp end of averaging period (format: \emph{yyyymmddHHMM}).}
\item{H_UNCLEANED}{Sensible heat turbulent flux (no storage correction, uncleaned).}
\item{H}{Sensible heat turbulent flux (no storage correction, cleaned).}
\item{H_DATA_FLAG}{Flag for H (0: observed flux for which any quality control (QC) tests provided negligible evidences of error; 1: outlying flux rejected because at least one of the QC tests provided a moderate evidence of error; 2: flux removed because at least one of the QC test provided a severe evidence of error).}
\item{LE_UNCLEANED}{Latent heat turbulent flux (no storage correction, uncleaned).}
\item{LE}{Latent heat turbulent flux (no storage correction, cleaned).}
\item{LE_DATA_FLAG}{Flag for LE (0: observed flux for which any quality control (QC) tests provided negligible evidences of error; 1: outlying flux rejected because at least one of the QC tests provided a moderate evidence of error; 2: flux removed because at least one of the QC test provided a severe evidence of error).}
\item{FC}{Carbon Dioxide (CO2) turbulent flux (no storage correction).}
\item{SC}{Carbon Dioxide (CO2) storage flux.}
\item{NEE_UNCLEANED}{Net Ecosystem Exchange (uncleaned).}
\item{NEE}{Net Ecosystem Exchange (cleaned).}
\item{NEE_DATA_FLAG}{Flag for NEE (i.e., 0: observed flux for which any quality control (QC) tests provided negligible evidences of error; 1: outlying flux rejected because at least one of the QC tests provided a moderate evidence of error; 2: flux removed because at least one of the QC test provided a severe evidence of error).}
\item{H_OUTLYING_FLAG}{Flag for H denoting outliers (0: no outlying flux; 1: outlying flux).}
\item{LE_OUTLYING_FLAG}{Flag for LE denoting outliers (0: no outlying flux; 1: outlying flux).}
\item{NEE_OUTLYING_FLAG}{Flag for NEE denoting outliers (0: no outlying flux; 1: outlying flux).}
\item{H_OOR_FLAG}{Flag for H denoting values out of the physically plausible range (0: within range; 2: out of range).}
\item{LE_OOR_FLAG}{Flag for LE denoting values out of the physically plausible range (0: within range; 2: out of range).}
\item{NEE_OOR_FLAG}{Flag for NEE denoting values out of the physically plausible range (0: within range; 2: out of range).}
\item{H_FMR_STAT}{Fraction of Missing Records in raw, high-frequency, data used for H flux estimation.}
\item{H_FMR_FLAG}{Flag for the FMR test for H (0: negligible evidences of error, IF FMR<5; 1: moderate evidences of error, 1 IF 5\eqn{\le}FMR\eqn{\le}15; 2: severe evidences of error, 2 IF FMR>15).}
\item{H_LGD_STAT}{Longest Gap Duration in raw, high-frequency, data used for H flux estimation.}
\item{H_LGD_FLAG}{Flag for the LGD test for H (0: negligible evidences of error, IF LGD<90; 1: moderate evidences of error, IF 90\eqn{\le}LGD\eqn{\le}180; 2: severe evidences of error, IF LGD>180).}
\item{LE_FMR_STAT}{Fraction of Missing Records in raw, high-frequency, data used for LE flux estimation.}
\item{LE_FMR_FLAG}{Flag for the FMR test for LE (0: negligible evidences of error, IF FMR<5; 1, moderate evidences of error, IF 5\eqn{\le}FMR\eqn{\le}15; 2, severe evidences of error, IF FMR>15).}
\item{LE_LGD_STAT}{Longest Gap Duration in raw, high-frequency, data used for LE flux estimation.}
\item{LE_LGD_FLAG}{Flag for the LGD test for LE (0: negligible evidences of error, IF LGD<90; 1: moderate evidences of error, IF 90\eqn{\le}LGD\eqn{\le}180; 2: severe evidences of error, IF LGD>180).}
\item{FC_FMR_STAT}{Fraction of Missing Records in raw, high-frequency, data used for FC flux estimation.}
\item{FC_FMR_FLAG}{Flag for the FMR test for FC (0: negligible evidences of error, IF FMR<5; 1: moderate evidences of error, IF 5\eqn{\le}FMR\eqn{\le}15; 2: severe evidences of error, IF FMR>15).}
\item{FC_LGD_STAT}{Longest Gap Duration in raw, high-frequency, data used for FC flux estimation.}
\item{FC_LGD_FLAG}{Flag for the LGD test for FC (0: negligible evidences of error, IF LGD<90; 1: moderate evidences of error, IF 90\eqn{\le}LGD\eqn{\le}180; 2: severe evidences of error, IF LGD>180).}
\item{SA_DIAG_FLAG}{Flag for Sonic Anemometer (SA) instrumental diagnostics (0: negligible evidences of error; 2: severe evidences of error).}
\item{GA_DIAG_FLAG}{Flag for gas analyzer (GA) instrumental diagnostics (0: negligible evidences of error; 2: severe evidences of error).}
\item{WD}{Wind direction.}
\item{WSECT_FLAG}{Footprint quality flag indicating periods when wind was blowing from directions known to significantly affect the turbulent flow (0: negligible evidences of error; 2: severe evidences of error).}
\item{H_LSR_STAT}{Statistic of the Low Signal Resolution test for H.}
\item{H_LSR_FLAG}{Flag for the LSR test for H (0: negligible evidences of error, IF LSR_STAT>0.995; 1: moderate evidences of error, IF 0.99\eqn{\le}LSR_STAT\eqn{\le}0.995; 2: severe evidences of error, IF LSR_STAT<0.99).}
\item{LE_LSR_STAT}{Statistic of the Low Signal Resolution test for LE.}
\item{LE_LSR_FLAG}{Flag for the LSR test for LE (0: negligible evidences of error, IF LSR_STAT>0.995; 1: moderate evidences of error, IF 0.99\eqn{\le}LSR_STAT\eqn{\le}0.995; 2: severe evidences of error, IF LSR_STAT<0.99).}
\item{FC_LSR_STAT}{Statistic of the Low Signal Resolution test for FC.}
\item{FC_LSR_FLAG}{Flag for the LSR test for FC (0: negligible evidences of error, IF LSR_STAT>0.995; 1: moderate evidences of error, IF 0.99 \eqn{\le}LSR_STAT\eqn{\le}0.995; 2: severe evidences of error, IF LSR_STAT<0.99).}
\item{W_HF5_STAT}{Statistic of the homogeneity test applied on vertical wind velocity fluctuations (percentage of data exceeding \eqn{\mu+/-5\sigma}).}
\item{W_HF5_FLAG}{Flag for the homogeneity test applied on vertical wind velocity fluctuations (0: negligible evidences of error, IF HF5_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}HF5_STAT\eqn{\le}4; 2: severe evidences of error, IF HF5_STAT>4).}
\item{W_HF10_STAT}{Statistic of the homogeneity test applied on vertical wind velocity fluctuations (percentage of data exceeding \eqn{\mu+/-10\sigma}).}
\item{W_HF10_FLAG}{Flag for the homogeneity test applied on vertical wind velocity fluctuations (0: negligible evidences of error, IF HF10_STAT<0.5; 1: moderate evidences of error, IF 0.5\eqn{\le}HF10_STAT\eqn{\le}1; 2: severe evidences of error, IF HF10_STAT>1).}
\item{W_HD5_STAT}{Statistic of the homogeneity test applied on differenced vertical wind velocity (percentage of data exceeding \eqn{\mu+/-5\sigma}).}
\item{W_HD5_FLAG}{Flag for the homogeneity test applied on differenced vertical wind velocity (0: negligible evidences of error, IF HD5_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}HD5_STAT\eqn{\le}4; 2: severe evidences of error, IF HD5_STAT>4).}
\item{W_HD10_STAT}{Statistic of the homogeneity test applied on differenced vertical wind velocity (percentage of data exceeding \eqn{\mu+/-10\sigma}).}
\item{W_HD10_FLAG}{Flag for the homogeneity test applied on differenced vertical wind velocity (0: negligible evidences of error, IF HD10_STAT<0.5; 1: moderate evidences of error, IF 0.5\eqn{\le}HD10_STAT\eqn{\le}1; 2: severe evidences of error, IF HD10_STAT>1).}
\item{W_ACF_STAT}{Autocorrelation at lag 1 for vertical wind velocity.}
\item{W_ACF_FLAG}{Flag for the autocorrelation at lag 1 for vertical wind velocity (0: negligible evidences of error, IF W_ACF_STAT>0.75; 1: moderate evidences of error, IF 0.5<W_ACF_STAT\eqn{\le}0.75; 2: severe evidences of error, IF W_ACF_STAT\eqn{\le}0.5).}
\item{W_DDI_STAT}{Maximum number of binned data points for vertical wind velocity (number of bins proportional to the sample size).}
\item{W_DDI_FLAG}{Flag for the W_DDI_STAT (0: negligible evidences of error, IF W_DDI_STAT<hz*60*2.5; 1: moderate evidences of error, IF \eqn{hz*60*2.5\le}W_DDI_STAT<\eqn{hz*60*5}; 2: severe evidences of error, IF W_DDI_STAT\eqn{\ge hz*60*5}, where hz denoting the acquisition frequency).}
\item{T_SONIC_HF5_STAT}{Statistic of the homogeneity test applied on sonic temperature fluctuations (percentage of data exceeding \eqn{\mu+/-5\sigma}).}
\item{T_SONIC_HF5_FLAG}{Flag for the homogeneity test applied on sonic temperature fluctuations (0: negligible evidences of error, IF HF5_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}HF5_STAT\eqn{\le}4; 2: severe evidences of error, IF HF5_STAT>4).}
\item{T_SONIC_HF10_STAT}{Statistic of the homogeneity test applied on sonic temperature fluctuations (percentage of data exceeding \eqn{\mu+/-10\sigma}).}
\item{T_SONIC_HF10_FLAG}{Flag for the homogeneity test applied on sonic temperature fluctuations (0: negligible evidences of error, IF HF10_STAT<0.5; 1: moderate evidences of error, IF 0.5\eqn{\le}HF10_STAT\eqn{\le}1; 2: severe evidences of error, IF HF10_STAT>1).}
\item{T_SONIC_HD5_STAT}{Statistic of the homogeneity test applied on differenced sonic temperature (percentage of data exceeding \eqn{\mu+/-5\sigma}).}
\item{T_SONIC_HD5_FLAG}{Flag for the homogeneity test applied on differenced sonic temperature (0: negligible evidences of error, IF HD5_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}HD5_STAT\eqn{\le}4; 2: severe evidences of error, IF HD5_STAT>4).}
\item{T_SONIC_HD10_STAT}{Statistic of the homogeneity test applied on differenced sonic temperature (percentage of data exceeding \eqn{\mu+/-10\sigma}).} 
\item{T_SONIC_HD10_FLAG}{Flag for the homogeneity test applied on differenced sonic temperature (0: negligible evidences of error, IF HD10_STAT<0.5; 1: moderate evidences of error, IF 0.5\eqn{\le}HD10_STAT\eqn{\le}1; 2: severe evidences of error, IF HD10_STAT>1).}
\item{T_SONIC_ACF_STAT}{Autocorrelation at lag 1 for sonic temperature.}
\item{T_SONIC_ACF_FLAG}{Flag for the autocorrelation at lag 1 for sonic temperature (0: negligible evidences of error, IF T_SONIC_ACF_STAT>0.75; 1: moderate evidences of error, IF 0.5<T_SONIC_ACF_STAT\eqn{\le}0.75; 2: severe evidences of error, IF T_SONIC_ACF_STAT\eqn{\le}0.5).}
\item{T_SONIC_DDI_STAT}{Maximum number of binned data points for sonic temperature (number of bins proportional to the sample size).}
\item{T_SONIC_DDI_FLAG}{Flag for the T_SONIC_DDI_STAT (0: negligible evidences of error, IF T_SPNIC_DDI_STAT<\eqn{hz*60*2.5}; 1: moderate evidences of error, IF \eqn{hz*60*2.5 \le}T_SONIC_DDI_STAT<\eqn{hz*60*5}; 2: severe evidences of error, IF T_SONIC_DDI_STAT\eqn{\ge hz*60*5}, where hz denoting the acquisition frequency).}
\item{H2O_HF5_STAT}{Statistic of the homogeneity test applied on water vapor fluctuations (percentage of data exceeding \eqn{\mu+/-5\sigma}).}
\item{H2O_HF5_FLAG}{Flag for the homogeneity test applied on water vapor fluctuations (0: negligible evidences of error, IF HF5_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}HF5_STAT\eqn{\le}4; 2: severe evidences of error, IF HF5_STAT>4).}
\item{H2O_HF10_STAT}{Statistic of the homogeneity test applied on water vapor fluctuations (percentage of data exceeding \eqn{\mu+/-10\sigma}).}
\item{H2O_HF10_FLAG}{Flag for the homogeneity test applied on water vapor fluctuations (0: negligible evidences of error, IF HF10_STAT<0.5; 1: moderate evidences of error, IF 0.5\eqn{\le}HF10_STAT\eqn{\le}1; 2: severe evidences of error, IF HF10_STAT>1).} 
\item{H2O_HD5_STAT}{Statistic of the homogeneity test applied on differenced water vapor (percentage of data exceeding \eqn{\mu+/-5\sigma}).}
\item{H2O_HD5_FLAG}{Flag for the homogeneity test applied on differenced water vapor (0: negligible evidences of error, IF HD5_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}HD5_STAT\eqn{\le}4; 2: severe evidences of error, IF HD5_STAT>4).}
\item{H2O_HD10_STAT}{Statistic of the homogeneity test applied on differenced water vapor (percentage of data exceeding \eqn{\mu+/-10\sigma}).}
\item{H2O_HD10_FLAG}{Flag for the homogeneity test applied on differenced water vapor (0: negligible evidences of error, IF HD10_STAT<0.5; 1: moderate evidences of error, IF 0.5\eqn{\le}HD10_STAT\eqn{\le}1; 2: severe evidences of error, IF HD10_STAT>1).}
\item{H2O_ACF_STAT}{Autocorrelation at lag 1 for water vapor.}
\item{H2O_ACF_FLAG}{Flag for the autocorrelation at lag 1 for water vapor (0: negligible evidences of error, IF H2O_ACF_STAT>0.75; 1: moderate evidences of error, IF 0.5<H2O_ACF_STAT\eqn{\le}0.75; 2: severe evidences of error, IF H2O_ACF_STAT\eqn{\le}0.5).}
\item{H2O_DDI_STAT}{Maximum number of binned data points for water vapor (number of bins proportional to the sample size).}
\item{H2O_DDI_FLAG}{Flag for the H2O_DDI_STAT (0: negligible evidences of error, IF H2O_DDI_STAT<hz*60*2.5; 1: moderate evidences of error, IF \eqn{hz*60*2.5 \le}H2O_DDI_STAT<\eqn{hz*60*5}; 2: severe evidences of error, IF H2O_DDI_STAT\eqn{\ge hz*60*5}, where hz denoting the acquisition frequency).}
\item{CO2_HF5_STAT}{Statistic of the homogeneity test applied on carbon dioxide fluctuations (percentage of data exceeding \eqn{\mu+/-5\sigma}).}
\item{CO2_HF5_FLAG}{Flag for the homogeneity test applied on carbon dioxide fluctuations (0: negligible evidences of error, IF HF5_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}HF5_STAT\eqn{\le}4; 2: severe evidences of error, IF HF5_STAT>4).}
\item{CO2_HF10_STAT}{Statistic of the homogeneity test applied on carbon dioxide fluctuations (percentage of data exceeding \eqn{\mu+/-10\sigma}).}
\item{CO2_HF10_FLAG}{Flag for the homogeneity test applied on carbon dioxide fluctuations (0: negligible evidences of error, IF HF10_STAT<0.5; 1: moderate evidences of error, IF 0.5\eqn{\le}HF10_STAT\eqn{\le}1; 2: severe evidences of error, IF HF10_STAT>1).}
\item{CO2_HD5_STAT}{Statistic of the homogeneity test applied on differenced carbon dioxide (percentage of data exceeding \eqn{\mu+/-5\sigma}).}
\item{CO2_HD5_FLAG}{Flag for the homogeneity test applied on differenced carbon dioxide (0: negligible evidences of error, IF HD5_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}HD5_STAT\eqn{\le}4; 2: severe evidences of error, IF HD5_STAT>4).}
\item{CO2_HD10_STAT}{Statistic of the homogeneity test applied on differenced carbon dioxide (percentage of data exceeding \eqn{\mu+/-10\sigma}).}
\item{CO2_HD10_FLAG}{Flag for the homogeneity test applied on differenced carbon dioxide (0: negligible evidences of error, IF HD10_STAT<0.5; 1: moderate evidences of error, IF 0.5\eqn{\le}HD10_STAT\eqn{\le}1; 2: severe evidences of error, IF HD10_STAT>1).}
\item{CO2_ACF_STAT}{Autocorrelation at lag 1 for carbon dioxide.}
\item{CO2_ACF_FLAG}{Flag for the autocorrelation at lag 1 for carbon dioxide (0: negligible evidences of error, IF CO2_ACF_STAT>0.75; 1: moderate evidences of error, IF 0.5<CO2_ACF_STAT\eqn{\le}0.75; 2: severe evidences of error, IF CO2_ACF_STAT\eqn{\le}0.5).}
\item{CO2_DDI_STAT}{Maximum number of binned data points for carbon dioxide (number of bins proportional to the sample size).}
\item{CO2_DDI_FLAG}{Flag for the CO2_DDI_STAT (0: negligible evidences of error, IF CO2_DDI_STAT<\eqn{hz*60*2.5}; 1: moderate evidences of error, IF \eqn{hz*60*2.5 \le}CO2_DDI_STAT<\eqn{hz*60*5}; 2: severe evidences of error, IF CO2_DDI_STAT\eqn{\ge hz*60*5}, where hz denoting the acquisition frequency).}
\item{W_KID_STAT}{Kurtosis Index of Differenced vertical wind velocity.}
\item{W_KID_FLAG}{Flag for the W_KID_STAT (0: negligible evidences of error, IF KID_STAT<30; 1: moderate evidences of error, IF 30\eqn{\le}KID_STAT\eqn{\le}50; 2: severe evidences of error, IF KID_STAT>50).}
\item{T_SONIC_KID_STAT}{Kurtosis Index of Differenced sonic temperature.}
\item{T_SONIC_KID_FLAG}{Flag for the T_SONIC_KID_STAT (0: negligible evidences of error, IF KID_STAT<30; 1: moderate evidences of error, IF 30\eqn{\le}KID_STAT\eqn{\le}50; 2: severe evidences of error, IF KID_STAT>50).}
\item{H2O_KID_STAT}{Kurtosis Index of Differenced water vapor.}
\item{H2O_KID_FLAG}{Flag for the H2O_KID_STAT (0: negligible evidences of error, IF KID_STAT<30; 1: moderate evidences of error, IF 30\eqn{\le}KID_STAT\eqn{\le}50; 2: severe evidences of error, IF KID_STAT>50).}
\item{CO2_KID_STAT}{Kurtosis Index of Differenced carbon dioxide.}
\item{CO2_KID_FLAG}{Flag for the CO2_KID_STAT (0: negligible evidences of error, IF KID_STAT<30; 1: moderate evidences of error, IF 30\eqn{\le}KID_STAT\eqn{\le}50; 2: severe evidences of error, IF KID_STAT>50).}
\item{ITC_STAT}{Statistic of the Integral Turbulence Characteristics test (Foken and Wichura, 1996).}
\item{ITC_FLAG}{Flag for the ITC test (0: negligible evidences of error, IF ITC_STAT<30; 1: moderate evidences of error, IF 30\eqn{\le}ITC_STAT\eqn{\le}50; 2: severe evidences of error, IF ITC_STAT>50).}
\item{H_SCF_STAT}{Spectral correction factor for H.}
\item{LE_SCF_STAT}{Spectral correction factor for LE.}
\item{FC_SCF_STAT}{Spectral correction factor for FC.}
\item{H_M98_STAT}{Statistic of the nonstationarity ratio test by Mahrt (1998) for H.}
\item{H_M98_FLAG}{Flag of the H_M98_STAT (0: negligible evidences of error, IF M_98_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}M98_STAT\eqn{\le}3; 2: severe evidences of error, IF M98_STAT>3).}
\item{LE_M98_STAT}{Statistic of the nonstationarity ratio test by Mahrt (1998) for LE.}
\item{LE_M98_FLAG}{Flag of the LE_M98_STAT (0: negligible evidences of error, IF M_98_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}M98_STAT\eqn{\le}3; 2: severe evidences of error, IF M98_STAT>3).}
\item{FC_M98_STAT}{Statistic of the nonstationarity ratio test by Mahrt (1998) for FC.}
\item{FC_M98_FLAG}{Flag of the FC_M98_STAT (0: negligible evidences of error, IF M_98_STAT<2; 1: moderate evidences of error, IF 2\eqn{\le}M98_STAT\eqn{\le}3; 2: severe evidences of error, IF M98_STAT>3).}
\item{CO2}{Carbon Dioxide (CO2) in mole fraction of wet air.}
\item{CO2_SIGMA}{Standard deviation of carbon dioxide in mole fraction of wet air.}
\item{H2O}{Water (H2O) vapor mole fraction.}
\item{H2O_SIGMA}{Standard deviation of water vapor mole fraction.}
\item{T_SONIC}{Sonic temperature.}
\item{T_SONIC_SIGMA}{Standard deviation of sonic temperature.}
\item{WS}{Wind speed.}
\item{USTAR}{Friction velocity.}
\item{W_SIGMA}{Standard deviation of vertical velocity fluctuations.}
\item{U_SIGMA}{Standard deviation of lateral velocity fluctuations (towards main-wind direction after coordinates rotation).}
\item{V_SIGMA}{Standard deviation of lateral velocity fluctuations (cross main-wind direction after coordinates rotation).}
\item{ZL}{Monin-Obukhov stability parameter.}
\item{MO_LENGTH}{Monin-Obukhov length.}
\item{AT}{Air temperature.}
\item{AP}{Air pressure.}
\item{RHO}{Air density.}
\item{CP}{Air heat capacity.}
\item{CANOPY_HEIGHT}{Canopy height.}
\item{SA_HEIGHT}{Sonic anemometer height.}
\item{SA_NORTH_OFFSET}{Sonic anemometer north offset.}
\item{INVALID_WIND_SECTOR_c1}{Center of the first invalid wind sector, if any.}
\item{INVALID_WIND_SECTOR_w1}{Width of the first invalid wind sector, if any.}
\item{INVALID_WIND_SECTOR_c2}{Center of the second invalid wind sector, if any.}
\item{INVALID_WIND_SECTOR_w2}{Width of the second invalid wind sector, if any.}
\item{INVALID_WIND_SECTOR_c3}{Center of the third invalid wind sector, if any.}
\item{INVALID_WIND_SECTOR_w3}{Width of the third invalid wind sector, if any.}
}

\references{
%% ~put references to the literature/web site here ~
Vitale, D. Fratini, G. Bilancia, M. Nicolini, G. Sabbatini, S. Papale, D.
A robust data cleaning procedure for eddy covariance flux measurements,
Biogeosciences, 17, 1367--1391, doi: \url{https://doi.org/10.5194/bg-17-1367-2020}, 2020.
}

\author{
Domenico Vitale
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\donttest{
PATH_WORKSET <- system.file("extdata", "ecworkset_example.csv", package = "RFlux")
PATH_ECMD <- system.file("extdata", "DE-HoH_ecmd.csv", package = "RFlux")

cleanset <- cleanFlux(path_workset=PATH_WORKSET,
                      path_ecmd=PATH_ECMD,
                      path_output=NULL,
                      FileName=NULL,
                      plotQC=FALSE,
                      storage=TRUE)
str(cleanset)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}% use one of  RShowDoc("KEYWORDS")
