# Analysing wing-beat frequency/ activity from Acc data


# Load in data ------

# Acc data
# Individual ACC records
load("acc.dat.df.Rdata")

# Acc events (i.e. sequences)
load("acc.rec.df.Rdata")

# Load flight summary data
load("flight_details.RData")

# GPS data
load("points.detailed.RData")

# Subset above (only those with acc data)
flights.acc <- flight.details[flight.details$flight_id_combined %in% acc.dat.df$flight_id_combined,]

# Merge command??
points.acc <- merge(points.detailed, acc.rec.df, by = c("device_info_serial", "date_time"))
# ?merge



# Look at some examples ----

# Acc records where gull in flight:
points.acc$vg <- sqrt(points.acc$vg_v*points.acc$vg_v + points.acc$vg_u*points.acc$vg_u)

hist(points.acc$vg)

points.acc_vg3 <- points.acc$vg >3

# i <- 150
z <- NULL
# hist(z, breaks = 100)
for(i in 1:sum(points.acc_vg3, na.rm = TRUE)){
  
  for(i in 1:2000){
    
  device_info_i <- points.acc$device_info_serial[points.acc_vg3][i]
  date_time_i <- points.acc$date_time[points.acc_vg3][i]
  
  
  acc_dat.i <- acc.dat.df[acc.dat.df$device_info_serial == device_info_i &
                            acc.dat.df$date_time == date_time_i,]
  
  
  # plot(acc_dat.i$z_acceleration, type = "l")
  
  if(any(is.na(acc_dat.i$z_acceleration))) {z[i] <- NA }else{
    
    
    z_acc_cen <- acc_dat.i$z_acceleration - mean(abs(acc_dat.i$z_acceleration))
    # plot(z_acc_cen, type = "l")
    
    z_acc_cen_norm <- z_acc_cen/max(abs(z_acc_cen))
    # plot(z_acc_cen_norm, type = "l")
    # ?spec.ar
    x <- spec.ar(z_acc_cen, log="no", plot = FALSE)
    
    max.spec <- max(unlist(x[['spec']]))
    freq.max <- unlist(x[['spec']]) == max.spec
    # str(x['freq'])
    freq <- unlist(x['freq'])[freq.max[1]]
    spec.freq <- freq*20
    z[i] <- spec.freq
    
  }
  
  
}
# summary(is.na(z))
  
  hist(z, breaks = 1000)

# str(x)
# x <- (arma.spec(ar = c(1,-.9), log="no"))
# unlist(x['freq'])[max(unlist(x['spec']))]*20








library("signal")

plot(acc_dat.i$z_acceleration)


?specgram

x <- specgram(z_acc_cen, Fs = 20)

plot(x)
str(x)

?spec

?ts

z_acc_cen_ts <- ts(z_acc_cen, start = 0, end = length(z_acc_cen)/20 )

spectrum(z_acc_cen, method = "pgram")

spectrum(z_acc_cen, method = "ar")

spec.ar(z_acc_cen)


plot(z_acc_cen)

install.packages("astsa")

library("astsa")

mvspec(z_acc_cen, spans=c(5,5), plot=TRUE, taper=.1, log="no")
mvspec(z_acc_cen)

# ?arma.spec
# spec.ar(z_acc_cen, log="no")
x <- (arma.spec(ar = c(1,-.9), log="no"))
x$freq[max(x$spec)]*20

# 0.16*20

spectrum(z_acc_cen)
?spectrum
?arma.spec
plot(z_acc_cen_norm, type = "l")
per = abs(fft(z_acc_cen_norm))^2/length(z_acc_cen_norm)  # Mod() and abs() same here 
# if you want a nice picture: 
freq = (1:length(z_acc_cen_norm)-1)/length(z_acc_cen_norm)
plot(freq, per, type="h")
per

?fft

z <- fft(fft(z_acc_cen_norm), inverse = TRUE)/length(z_acc_cen_norm)
plot(z)
?fft

x <- 1:4
fft(x)
fft(fft(x), inverse = TRUE)/length(x)


get.trajectory <- function(X.k,ts,acq.freq) {
  
  N   <- length(ts)
  i   <- complex(real = 0, imaginary = 1)
  x.n <- rep(0,N)           # create vector to keep the trajectory
  ks  <- 0:(length(X.k)-1)
  
  for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
    x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
  }
  
  x.n * acq.freq 
}


get.trajectory(1,z_acc_cen_norm,20)



install.packages("seewave")
library("seewave")
meanspec(z_acc_cen_norm, f = 20, wl = 20)

fpeaks(z_acc_cen_norm, f = 20, nmax = 8)





library("astsa")

plot(z_acc_cen_norm, type = "l")
mvspec(z_acc_cen_norm, spans=c(5,5), plot=TRUE, taper=.1, log="no")
mvspec(z_acc_cen_norm)


spec.ar(z_acc_cen_norm, log="no")
arma.spec(ar = c(1,-.9), log="no")



f = abs(fft(z_acc_cen_norm))
pos = Ordering[-f, 1][[1]]; (*the position of the first Maximal value*)  
fr = Abs[Fourier[pdata Exp[2 Pi I (pos - 2) N[Range[0, n - 1]]/n], 
                 FourierParameters -> {0, 2/n}]];
frpos = Ordering[-fr, 1][[1]];

N[(pos - 2 + 2 (frpos - 1)/n)]
