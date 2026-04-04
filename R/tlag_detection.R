

tlag_detection <- function (scalar_var, tsonic_var, w_var, mfreq, wdt=5, model = "ar", LAG.MAX=mfreq*10, lws=0, uws=5, Rboot, plot.it=FALSE,...) 
{

   filter.mod = function(x, model) {
    	x <- x - mean(x, na.rm=TRUE)
        if (length(model$Delta) >= 1) 
            x = stats::filter(x, filter = c(1, -model$Delta), method = "convolution", sides = 1)
        if (length(model$theta) >= 1 && any(model$theta != 0)) 
            x = stats::filter(na.omit(x), filter = -model$theta, method = "recursive", sides = 1)
        if (length(model$phi) >= 1 && any(model$phi != 0)) 
            x = stats::filter(x, filter = c(1, -model$phi), method = "convolution", sides = 1)
        x
    }


	set <- na.omit(cbind(na.approx(scalar_var, na.rm=FALSE), na.approx(tsonic_var, na.rm=FALSE), na.approx(w_var, na.rm=FALSE)))
        
	if (model=="ar"){
	if (bvr.test(set[,1])$p.val<0.01 & bvr.test(set[,2])$p.val<0.01 & bvr.test(set[,3])$p.val<0.01) {
	x <- set[,1];
	y <- set[,2];
	z <- set[,3]
	};
	if (bvr.test(set[,1])$p.val>=0.01 | bvr.test(set[,2])$p.val>=0.01 | bvr.test(set[,3])$p.val>=0.01) {
	x <- diff(set[,1]);
	y <- diff(set[,2]);
	z <- diff(set[,3])
	};
	ar.resx = ar(x, aic=TRUE, order.max=floor(10^2*log10(length(x))));
	ar.resy = ar(y, aic=TRUE, order.max=floor(10^2*log10(length(x))));
	ar.resz = ar(z, aic=TRUE, order.max=floor(10^2*log10(length(x))));
    x1 = stats::filter(x, filter = c(1, -ar.resx$ar), method = "convolution", sides = 1);
    y1 = stats::filter(y, filter = c(1, -ar.resx$ar), method = "convolution", sides = 1);
    z1 = stats::filter(z, filter = c(1, -ar.resx$ar), method = "convolution", sides = 1);
    x2 = stats::filter(x, filter = c(1, -ar.resy$ar), method = "convolution", sides = 1);
    y2 = stats::filter(y, filter = c(1, -ar.resy$ar), method = "convolution", sides = 1)
    z2 = stats::filter(z, filter = c(1, -ar.resy$ar), method = "convolution", sides = 1)
    x3 = stats::filter(x, filter = c(1, -ar.resz$ar), method = "convolution", sides = 1);
    y3 = stats::filter(y, filter = c(1, -ar.resz$ar), method = "convolution", sides = 1);
    z3 = stats::filter(z, filter = c(1, -ar.resz$ar), method = "convolution", sides = 1);
   	}


	if (model=="arima"){
	if (bvr.test(set[,1])$p.val<0.01 & bvr.test(set[,2])$p.val<0.01 & bvr.test(set[,3])$p.val<0.01) {
	x <- set[,1];
	y <- set[,2];
	z <- set[,3]
	};
	if (bvr.test(set[,1])$p.val>=0.01 | bvr.test(set[,2])$p.val>=0.01 | bvr.test(set[,3])$p.val>=0.01) {
	x <- diff(set[,1]);
	y <- diff(set[,2]);
	z <- diff(set[,3])
	};
	mod1 <- forecast::auto.arima(x, d=0);
	mod2 <- forecast::auto.arima(y, d=0);
	mod3 <- forecast::auto.arima(z, d=0);
    x1 = filter.mod(x, model = mod1$model);
    y1 = filter.mod(y, model = mod1$model);
    z1 = filter.mod(z, model = mod1$model);
    x2 = filter.mod(x, model = mod2$model);
    y2 = filter.mod(y, model = mod2$model);
    x3 = filter.mod(x, model = mod3$model);
    z3 = filter.mod(z, model = mod3$model)
 	}
	
    
## Maximum Covariance Procedure    
	ccf_mcw <- as.vector(ccf(x = as.vector(detrend(set[,1])), y = as.vector(detrend(set[,3])), lag.max=LAG.MAX, plot=FALSE, type="covariance", na.action=na.pass)$acf)
	ccf_mct <- as.vector(ccf(x = as.vector(detrend(set[,1])), y = as.vector(detrend(set[,2])), lag.max=LAG.MAX, plot=FALSE, type="covariance", na.action=na.pass)$acf)

	tl_mcw <- which.max(abs(ccf_mcw))
	tl_mct <- which.max(abs(ccf_mct))
	
	tl_mcw_win <- which.max(abs(ccf_mcw)[(LAG.MAX+lws*mfreq+1):(LAG.MAX+uws*mfreq+1)])+LAG.MAX+lws*mfreq
	tl_mct_win <- which.max(abs(ccf_mct)[(LAG.MAX+lws*mfreq+1):(LAG.MAX+uws*mfreq+1)])+LAG.MAX+lws*mfreq

	cov_mcw <- ccf_mcw[tl_mcw]
	cov_mct <- ccf_mcw[tl_mct]

	cov_mcw_win <- ccf_mcw[tl_mcw_win]
	cov_mct_win <- ccf_mcw[tl_mct_win]

## PreWhitening - Standard procedure
	ccf_pww <- ccf(x1, z1, na.action=na.pass, plot=FALSE, lag.max=LAG.MAX)$acf
	ccf_pwt <- ccf(x1, y1, na.action=na.pass, plot=FALSE, lag.max=LAG.MAX)$acf

	tl_pww <- which.max(abs(ccf_pww))
	tl_pwt <- which.max(abs(ccf_pwt))

	tl_pww_win <- which.max(abs(ccf_pww)[(LAG.MAX+lws*mfreq+1):(LAG.MAX+uws*mfreq+1)])+LAG.MAX+lws*mfreq
	tl_pwt_win <- which.max(abs(ccf_pwt)[(LAG.MAX+lws*mfreq+1):(LAG.MAX+uws*mfreq+1)])+LAG.MAX+lws*mfreq

	cor_pww <- ccf_pww[tl_pww]
	cor_pwt <- ccf_pwt[tl_pwt]

	cor_pww_win <- ccf_pww[tl_pww_win]
	cor_pwt_win <- ccf_pwt[tl_pwt_win]

	cov_pww <- ccf_mcw[tl_pww]
	cov_pwt <- ccf_mcw[tl_pwt]



## PreWhitening + BOOTSTRAPPING + Smoothing
	parallel.option <- if (.Platform$OS.type == "windows") "snow" else "multicore"
	nc <- detectCores()-1

 	bootccf_cw <- tsboot(cbind(x1,z1), function(x) ccf(x[,1],x[,2],na.action=na.pass, plot=FALSE, lag.max=LAG.MAX)$acf, R=Rboot, sim="fixed", l=LAG.MAX*2, parallel=parallel.option, ncpus=nc);
 	ccf_cw <- unlist(apply(bootccf_cw$t, MARGIN=2, function(x) mean(x, na.rm=TRUE)));
    ccfs_cw <- unlist(zoo::na.locf(zoo::na.locf(rollapply(ccf_cw, width=wdt, FUN="mean", fill=NA) , na.rm=FALSE), fromLast=TRUE));
  	ccfs_cw1 <- unlist(apply(bootccf_cw$t, MARGIN=1, function(x) which.max(abs(as.vector(zoo::na.locf(zoo::na.locf(rollapply(x, width=wdt, FUN="mean", fill=NA), na.rm=FALSE), fromLast=TRUE))))));

 	ccfs_cw2 <- unlist(apply(bootccf_cw$t, MARGIN=1, function(x) which.max(abs(as.vector(zoo::na.locf(zoo::na.locf(rollapply(x, width=wdt, FUN="mean", fill=NA), na.rm=FALSE), fromLast=TRUE))[(LAG.MAX+lws*mfreq+1):(LAG.MAX+uws*mfreq+1)]))+LAG.MAX+lws*mfreq));
    hdis_cw <- HDInterval::hdi(ccfs_cw1, credMass=.95);
	hdis_cw2 <- HDInterval::hdi(ccfs_cw2, credMass=.95);

 	bootccf_ct <- tsboot(cbind(x1,y1), function(x) ccf(x[,1],x[,2],na.action=na.pass, plot=FALSE, lag.max=LAG.MAX)$acf, R=Rboot, sim="fixed", l=LAG.MAX*2, parallel=parallel.option, ncpus=nc);
 	ccf_ct <- unlist(apply(bootccf_ct$t, MARGIN=2, function(x) mean(x, na.rm=TRUE)));
    ccfs_ct <- unlist(zoo::na.locf(zoo::na.locf(rollapply(ccf_ct, width=wdt, FUN="mean", fill=NA) , na.rm=FALSE), fromLast=TRUE));
 	ccfs_ct1 <- unlist(apply(bootccf_ct$t, MARGIN=1, function(x) which.max(abs(as.vector(zoo::na.locf(zoo::na.locf(rollapply(x, width=wdt, FUN="mean", fill=NA), na.rm=FALSE), fromLast=TRUE))))));
 	ccfs_ct2 <- unlist(apply(bootccf_ct$t, MARGIN=1, function(x) which.max(abs(as.vector(zoo::na.locf(zoo::na.locf(rollapply(x, width=wdt, FUN="mean", fill=NA), na.rm=FALSE), fromLast=TRUE))[(LAG.MAX+lws*mfreq+1):(LAG.MAX+uws*mfreq+1)]))+LAG.MAX+lws*mfreq));
    hdis_ct <- HDInterval::hdi(ccfs_ct1, credMass=.95);
    hdis_ct2 <- HDInterval::hdi(ccfs_ct2, credMass=.95);
    
 	bootccf_wc <- tsboot(cbind(x3,z3), function(x) ccf(x[,1],x[,2],na.action=na.pass, plot=FALSE, lag.max=LAG.MAX)$acf, R=Rboot, sim="fixed", l=LAG.MAX*2, parallel=parallel.option, ncpus=nc);
 	ccf_wc <- unlist(apply(bootccf_wc$t, MARGIN=2, function(x) mean(x, na.rm=TRUE)));
    ccfs_wc <- unlist(zoo::na.locf(zoo::na.locf(rollapply(ccf_wc, width=wdt, FUN="mean", fill=NA) , na.rm=FALSE), fromLast=TRUE))
 	ccfs_wc1 <- unlist(apply(bootccf_wc$t, MARGIN=1, function(x) which.max(abs(as.vector(zoo::na.locf(zoo::na.locf(rollapply(x, width=wdt, FUN="mean", fill=NA), na.rm=FALSE), fromLast=TRUE))))));
 	ccfs_wc2 <- unlist(apply(bootccf_wc$t, MARGIN=1, function(x) which.max(abs(as.vector(zoo::na.locf(zoo::na.locf(rollapply(x, width=wdt, FUN="mean", fill=NA), na.rm=FALSE), fromLast=TRUE))[(LAG.MAX+lws*mfreq+1):(LAG.MAX+uws*mfreq+1)]))+LAG.MAX+lws*mfreq));
    hdis_wc <- HDInterval::hdi(ccfs_wc1, credMass=.95);
 	hdis_wc2 <- HDInterval::hdi(ccfs_wc2, credMass=.95);


 	bootccf_tc <- tsboot(cbind(x2,y2), function(x) ccf(x[,1],x[,2],na.action=na.pass, plot=FALSE, lag.max=LAG.MAX)$acf, R=Rboot, sim="fixed", l=LAG.MAX*2, parallel=parallel.option, ncpus=nc);
 	ccf_tc <- unlist(apply(bootccf_tc$t, MARGIN=2, function(x) mean(x, na.rm=TRUE)));
    ccfs_tc <- unlist(zoo::na.locf(zoo::na.locf(rollapply(ccf_tc, width=wdt, FUN="mean", fill=NA) , na.rm=FALSE), fromLast=TRUE));
 	ccfs_tc1 <- unlist(apply(bootccf_tc$t, MARGIN=1, function(x) which.max(abs(as.vector(zoo::na.locf(zoo::na.locf(rollapply(x, width=wdt, FUN="mean", fill=NA), na.rm=FALSE), fromLast=TRUE))))));
 	ccfs_tc2 <- unlist(apply(bootccf_tc$t, MARGIN=1, function(x) which.max(abs(as.vector(zoo::na.locf(zoo::na.locf(rollapply(x, width=wdt, FUN="mean", fill=NA), na.rm=FALSE), fromLast=TRUE))[(LAG.MAX+lws*mfreq+1):(LAG.MAX+uws*mfreq+1)]))+LAG.MAX+lws*mfreq));
    hdis_tc <- HDInterval::hdi(ccfs_tc1, credMass=.95);
 	hdis_tc2 <- HDInterval::hdi(ccfs_tc2, credMass=.95);


	maps <- round(c(bayestestR::map_estimate(abs(ccfs_ct1+rnorm(length(ccfs_ct1),0,0.0001)))$MAP_Estimate, bayestestR::map_estimate(abs(ccfs_cw1+rnorm(length(ccfs_cw1),0,0.0001)))$MAP_Estimate, bayestestR::map_estimate(abs(ccfs_tc1+rnorm(length(ccfs_tc1),0,0.0001)))$MAP_Estimate, bayestestR::map_estimate(abs(ccfs_wc1+rnorm(length(ccfs_wc1),0,0.0001)))$MAP_Estimate),0)

	maps2 <- round(c(bayestestR::map_estimate(abs(ccfs_ct2+rnorm(length(ccfs_ct2),0,0.0001)))$MAP_Estimate, bayestestR::map_estimate(abs(ccfs_cw2+rnorm(length(ccfs_cw2),0,0.0001)))$MAP_Estimate, bayestestR::map_estimate(abs(ccfs_tc2+rnorm(length(ccfs_tc2),0,0.0001)))$MAP_Estimate, bayestestR::map_estimate(abs(ccfs_wc2+rnorm(length(ccfs_wc2),0,0.0001)))$MAP_Estimate),0)
     
	
    corr_est_s <- c(ccfs_ct[maps[1]], ccfs_cw[maps[2]], ccfs_tc[maps[3]], ccfs_wc[maps[4]]) 
    corr_ind <- which.max(abs(corr_est_s))
    corr_max <- corr_est_s[corr_ind]
	peak_ref <- maps[corr_ind]
	if (corr_ind==1) hdis <- as.vector(hdis_ct)
	if (corr_ind==2) hdis <- as.vector(hdis_cw)
	if (corr_ind==3) hdis <- as.vector(hdis_tc)
	if (corr_ind==4) hdis <- as.vector(hdis_wc)
	
	cov_pwb <- ccf_mcw[peak_ref]

    corr_est_s2 <- c(ccfs_ct[maps2[1]], ccfs_cw[maps2[2]], ccfs_tc[maps2[3]], ccfs_wc[maps2[4]]) 
    corr_ind2 <- which.max(abs(corr_est_s2))
    corr_max2 <- corr_est_s2[corr_ind2]
	peak_ref2 <- maps2[corr_ind2]
	if (corr_ind2==1) hdis2 <- as.vector(hdis_ct2)
	if (corr_ind2==2) hdis2 <- as.vector(hdis_cw2)
	if (corr_ind2==3) hdis2 <- as.vector(hdis_tc2)
	if (corr_ind2==4) hdis2 <- as.vector(hdis_wc2)
	    
  
    cov_est_w <- ccf_mcw[tl_mcw]
    cov_est_ts <- ccf_mcw[tl_mct]
    cov_est_s <-  c(ccf_mcw[maps[1]], ccf_mcw[maps[2]], ccf_mcw[maps[3]], ccf_mcw[maps[4]])
    

## STATS v3
	cred_win_ct <- max(c(1, as.vector(hdis_ct)[1] - floor(wdt/2))) : min(c(LAG.MAX*2+1, as.vector(hdis_ct)[2] + floor(wdt/2)))
	cred_win_cw <- max(c(1, as.vector(hdis_cw)[1] - floor(wdt/2))) : min(c(LAG.MAX*2+1, as.vector(hdis_cw)[2] + floor(wdt/2)))
	cred_win_tc <- max(c(1, as.vector(hdis_tc)[1] - floor(wdt/2))) : min(c(LAG.MAX*2+1, as.vector(hdis_tc)[2] + floor(wdt/2)))
	cred_win_wc <- max(c(1, as.vector(hdis_wc)[1] - floor(wdt/2))) : min(c(LAG.MAX*2+1, as.vector(hdis_wc)[2] + floor(wdt/2)))

	peak_win_ct <- which.max(abs(ccf_ct[cred_win_ct])) + cred_win_ct[1] - 1
	peak_win_cw <- which.max(abs(ccf_cw[cred_win_cw])) + cred_win_cw[1] - 1
	peak_win_tc <- which.max(abs(ccf_tc[cred_win_tc])) + cred_win_tc[1] - 1
	peak_win_wc <- which.max(abs(ccf_wc[cred_win_wc])) + cred_win_wc[1] - 1

	peak_ind <- which.max(c(abs(ccf_ct)[peak_win_ct], abs(ccf_cw)[peak_win_cw], abs(ccf_tc)[peak_win_tc], abs(ccf_wc)[peak_win_wc]))
	peak_ref3 <- c(peak_win_ct, peak_win_cw, peak_win_tc, peak_win_wc)[peak_ind]
	

## PLOT
    if (plot.it) {
	par(mfrow=c(3,2), mar=c(5,4,2,1), oma=c(1,1,5,0.5), las=0, cex.axis=1.3, cex.lab=1.3)
	plot((-LAG.MAX:LAG.MAX), ccf_mcw, ylab="cross-cov (c,w)", xlab="Lag (sec)", type="h", col="grey68", ylim=c(min(ccf_mcw*1.05,0),max(ccf_mcw*1.05,0)), xaxt="n")
	axis(side=1, at=seq(-LAG.MAX,LAG.MAX,40), labels=seq(-LAG.MAX,LAG.MAX,40)/mfreq)  
    polygon(x=c(hdis[1]: hdis[2], hdis[2]: hdis[1])-LAG.MAX-1, y=c(ccf_mcw[hdis[1]: hdis[2]],rep(0,hdis[2]-hdis[1]+1)), col="lightblue", border="lightblue")
    segments(x0=peak_ref - LAG.MAX - 1, y0=0, y1=ccf_mcw[peak_ref], col=2, lwd=2)
    segments(x0=tl_mcw - LAG.MAX - 1, y0=0, y1=ccf_mcw[tl_mcw], col=1, lwd=1)
    mtext(side=3, line=.5, adj=0, paste0("Peak at ", (tl_mcw-LAG.MAX-1)/mfreq, " sec"), cex=1.1) 
    mtext(side=3, line=.5, adj=1, "a", cex=1.5, font=2) 
	box(lwd=1.5)
	mtext(side=3, line=3, paste0("Time lag at: ", (peak_ref-LAG.MAX-1)/mfreq, " sec"), cex=1.25, col=2)

	plot((-LAG.MAX:LAG.MAX), ccf_mct, ylab="cross-cov (c,t)", xlab="Lag (sec)", type="h", col="grey68", ylim=c(min(ccf_mct*1.05,0),max(ccf_mct*1.05,0)), xaxt="n")    
	axis(side=1, at=seq(-LAG.MAX,LAG.MAX,40), labels=seq(-LAG.MAX,LAG.MAX,40)/mfreq)  
    polygon(x=c(hdis[1]:hdis[2], hdis[2]:hdis[1])-LAG.MAX-1, y=c(ccf_mct[hdis[1]:hdis[2]],rep(0,hdis[2]-hdis[1]+1)), col="lightblue", border="lightblue")
    segments(x0=peak_ref - LAG.MAX - 1, y0=0, y1=ccf_mct[peak_ref], col=2, lwd=2)
    segments(x0=tl_mct - LAG.MAX - 1, y0=0, y1=ccf_mct[tl_mct], col=1, lwd=1)
    mtext(side=3, line=.5, adj=0, paste0("Peak at ", (tl_mct-LAG.MAX-1)/mfreq, " sec"), cex=1.1) 
    mtext(side=3, line=.5, adj=1, "b", cex=1.5, font=2) 
	box(lwd=1.5)
	mtext(side=3, line=3, paste0(.95*100, "% HDI: [", (hdis[1]-LAG.MAX-1)/mfreq, "; ", (hdis[2]-LAG.MAX-1)/mfreq, "] sec"), cex=1.25, col=2)
	
	plot((-LAG.MAX:LAG.MAX), ccf_cw, ylab="pwb cross-cor (c,w)", xlab="Lag (sec)", type="h", col="grey68", ylim=c(min(ccf_cw,-4/sqrt(length(x))),max(ccf_cw,4/sqrt(length(x)))), xaxt="n")    
    lines((-LAG.MAX:LAG.MAX), ccfs_cw, col=1, lwd=2)
	axis(side=1, at=seq(-LAG.MAX,LAG.MAX,40), labels=seq(-LAG.MAX,LAG.MAX,40)/mfreq)  
    abline(h=c(-3.291,3.291)/sqrt(length(x)*13), col=4, lty=2, lwd=2)
    points(x=maps[2] - LAG.MAX - 1, y=min(ccf_cw,-4/sqrt(length(x))), pch=24, col=1, cex=1.25, bg="red")
    mtext(side=3, line=1, adj=1, "c", cex=1.5, font=2) 
   	mtext(side=3, line=.5, adj=0, paste0("Peak at ", (maps[2]-LAG.MAX-1)/mfreq ," sec"), cex=1.1) 
	box(lwd=1.5)

	plot((-LAG.MAX:LAG.MAX), ccf_ct, ylab="pwb cross-cor (c,t)", xlab="Lag (sec)", type="h", col="grey68", ylim=c(min(ccf_ct,-4/sqrt(length(x))),max(ccf_ct,4/sqrt(length(x)))), xaxt="n")    
    lines((-LAG.MAX:LAG.MAX), ccfs_ct, col=1, lwd=2)
	axis(side=1, at=seq(-LAG.MAX,LAG.MAX,40), labels=seq(-LAG.MAX,LAG.MAX,40)/mfreq)  
    abline(h=c(-3.291,3.291)/sqrt(length(x)*13), col=4, lty=2, lwd=2)
    points(x=maps[1] - LAG.MAX - 1, y=min(ccf_ct,-4/sqrt(length(x))), pch=24, col=1, cex=1.25, bg="red")
	mtext(side=3, line=1, adj=1, "d", cex=1.5, font=2) 
   	mtext(side=3, line=.5, adj=0, paste0("Peak at ", (maps[1]-LAG.MAX-1)/mfreq ," sec"), cex=1.1) 
	box(lwd=1.5)

	plot((-LAG.MAX:LAG.MAX), ccf_wc, ylab="pwb cross-cor (w,c)", xlab="Lag (sec)", type="h", col="grey68", ylim=c(min(ccf_wc,-4/sqrt(length(x))),max(ccf_wc,4/sqrt(length(x)))), xaxt="n")    
    lines((-LAG.MAX:LAG.MAX), ccfs_wc, col=1, lwd=2)
	axis(side=1, at=seq(-LAG.MAX,LAG.MAX,40), labels=seq(-LAG.MAX,LAG.MAX,40)/mfreq)  
    abline(h=c(-3.291,3.291)/sqrt(length(x)*13), col=4, lty=2, lwd=2)
    points(x=maps[4] - LAG.MAX - 1, y=min(ccf_wc,-4/sqrt(length(x))), pch=24, col=1, cex=1.25, bg="red")
	mtext(side=3, line=1, adj=1, "e", cex=1.5, font=2) 
   	mtext(side=3, line=.5, adj=0, paste0("Peak at ", (maps[4]-LAG.MAX-1)/mfreq ," sec"), cex=1.1) 
	box(lwd=1.5)

 	plot((-LAG.MAX:LAG.MAX), ccf_tc, ylab="pwb cross-cor (t,c)", xlab="Lag (sec)", type="h", col="grey68", ylim=c(min(ccf_tc,-4/sqrt(length(x))),max(ccf_tc,4/sqrt(length(x)))), xaxt="n")    
    lines((-LAG.MAX:LAG.MAX), ccfs_tc, col=1, lwd=2)
	axis(side=1, at=seq(-LAG.MAX,LAG.MAX,40), labels=seq(-LAG.MAX,LAG.MAX,40)/mfreq)  
    abline(h=c(-3.291,3.291)/sqrt(length(x)*13), col=4, lty=2, lwd=2)
    points(x=maps[3] - LAG.MAX - 1, y=min(ccf_tc,-4/sqrt(length(x))), pch=24, col=1, cex=1.25, bg="red")
    mtext(side=3, line=1, adj=1, "f", cex=1.5, font=2) 
   	mtext(side=3, line=.5, adj=0, paste0("Peak at ", (maps[3]-LAG.MAX-1)/mfreq ," sec"), cex=1.1) 
	box(lwd=1.5)
   }
 
    return(list(
    "mcw" = tl_mcw - LAG.MAX - 1,
    "mct" = tl_mct - LAG.MAX-1,
    "mcw_win" = tl_mcw_win - LAG.MAX-1,
    "mct_win" = tl_mct_win - LAG.MAX-1,
    "pww" = tl_pww - LAG.MAX-1,
    "pwt" = tl_pwt - LAG.MAX-1,  
    "pwb" = peak_ref - LAG.MAX-1,
    "pwb_lci"= hdis[1] - LAG.MAX-1,
    "pwb_uci"= hdis[2] - LAG.MAX-1,
    "cor_pww" = cor_pww,
    "cor_pwt" = cor_pwt,
    "cor_pwb" = corr_max,
    "cov_mcw" = cov_mcw,
    "cov_mct" = cov_mct,
    "cov_mcw_win" = cov_mcw_win,
    "cov_mct_win" = cov_mct_win,
    "cov_pww" = cov_pww,
    "cov_pwt" = cov_pwt,
    "cov_pwb" = cov_pwb  
    ))
}
