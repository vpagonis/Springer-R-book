# Original Mathematica program by Vasilis Pagonis
# R version written by Johannes Friedrich, 2018
# The code reproduces Fig 2 of Pagonis and Kulp (2010)
rm(list = ls(all = TRUE)) # empties the environment
library("plot3D")
library("FNN")
## Define Parameters ----
sideX <- 200e-9 # lenght of quader in m
sideX_nm <- sideX*1e9 # length of quader in nm
s_tun <- 3e15
alpha <- 4e9
N_pts <- 100
clusters <- 50
rho_prime <- 1e-5

N_centers <- as.integer(rho_prime * sideX^3 *3 *alpha^3/(4*pi))
rho <- N_centers/ sideX^3
all_times_matrix <- matrix(NA,nrow = N_pts,ncol = clusters)
## Run MC -----
for(j in 1:clusters) {
  if(j %% 100 == 0) print(j)
  n_pts <- N_pts
  n_centers <- N_centers
  xyz_traps <- data.frame(
    x = sample(1:sideX_nm, N_pts, replace = TRUE),
    y = sample(1:sideX_nm, N_pts, replace = TRUE),
    z = sample(1:sideX_nm, N_pts, replace = TRUE)
  )
  xyz_centers <- data.frame(
    x = sample(1:sideX_nm, n_centers, replace = TRUE),
    y = sample(1:sideX_nm, n_centers, replace = TRUE),
    z = sample(1:sideX_nm, n_centers, replace = TRUE)
  )
  ### r calc distances ------
  for(i in 1:(n_pts)){
    ## find next neighbours with package FNN
    dist <- FNN::get.knnx(data = as.matrix(xyz_centers),
                          query = as.matrix(xyz_traps),
                          k = 1)
    all_dist <- as.data.frame(dist)
    P <- runif(n = length(all_dist$nn.dist), min = 0, max = 1)
    # P <- runif(n = 1, min = 0, max = 1)
    recomb_time <- - s_tun^(-1) * exp(alpha * all_dist$nn.dist * 
                                        1e-9) * log(1-P)
    e_remove <- which.min(recomb_time)
    h_remove <- all_dist$nn.index[e_remove]
    ##remove index from data.frame
    xyz_centers <- xyz_centers[-h_remove,]
    xyz_traps <- xyz_traps[-e_remove,]
    all_times_matrix[i,j] <- recomb_time[e_remove]
  } # end n_pts loop
  all_times_matrix[,j] <- cumsum(all_times_matrix[,j])
} ## end cluster-loop
all_times_matrix <- log10(all_times_matrix)
### plot results ------
times_avg <- rowMeans(all_times_matrix)
matplot(x = all_times_matrix,
        y = (N_pts-1):0,xlim = c(-10,20), col = "grey",
        ylab = "remaining electrons",
        xlab = "log10(times)",pch = 1)
points(
  x = times_avg,  y = (N_pts-1):0, col = "blue")
sd <- apply(all_times_matrix, 1, sd)
sd_error <- sd/sqrt(N_pts)
## plot error bars
arrows(times_avg-sd_error,
       (N_pts-1):0,
       times_avg+sd_error,
       length=0.05,angle=90, code=3)
t <- 10^seq(-15,20,1)
lines(
  x = log10(t),
  y = N_pts * exp(-rho_prime * log(1.8 * s_tun * t)^3),
  col = "red",lwd=3)
legend("topright",bty="n",
       legend = c("500 MC runs", "MC average", 
                  "analytical solution"),
       col = c("grey", "blue", "red"),
       pch = c(1,1,NA),lwd = c(NA,NA,2))