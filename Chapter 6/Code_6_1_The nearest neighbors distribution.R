# Fig 1 in Pagonis and Kulp paper
# Original Mathematica Program written by V Pagonis
# R  version written by Johannes Friedrich
rm(list = ls(all = TRUE)) # empties the environment
library("plot3D")
library("FNN")
## Define Parameters ----
sideX <- 100e-9 # lenght of quader in m
sideX_nm <- sideX*1e9 # length of quader in nm
N_pts <- 50
alpha <- 9e9
N_centers <- 300
rho <- N_centers/sideX^3
r_prime <- function(r) (4*pi*rho/3)^(1/3) * r * 1e-9
xyz_traps <- data.frame(
  x = sample(1:sideX_nm, N_pts, replace = TRUE),
  y = sample(1:sideX_nm, N_pts, replace = TRUE),
  z = sample(1:sideX_nm, N_pts, replace = TRUE))
xyz_centers <- data.frame(
  x = sample(1:sideX_nm, N_centers, replace = TRUE),
  y = sample(1:sideX_nm, N_centers, replace = TRUE),
  z = sample(1:sideX_nm, N_centers, replace = TRUE))
par(mfrow=c(1,2))
plot3D::scatter3D(xyz_centers$x, #plot centers (blue)
                  xyz_centers$y,cex=1,
                  xyz_centers$z, bty = "g", pch = 1, theta = 30,
                  phi = 30, col = "blue")
plot3D::scatter3D(xyz_traps$x, # add traps (red)
                  xyz_traps$y, cex=1,
                  xyz_traps$z, bty = "g", pch = 17,theta = 30, 
                  phi = 30, col = "red",add = TRUE)
legend("topright",bty="n","(a)")
## find nearest neighbour
dist <- FNN::get.knnx(data = as.matrix(xyz_centers),
                      query = as.matrix(xyz_traps),k = 1)
## plot histogram
distance_nm<-as.vector(dist$nn.dist)
hist(distance_nm,xlim=c(0,22),main=" ")
## calc analytical solution
r <- seq(0, 20, 0.1)
distr_ana <- 3 * 8 * r_prime(r)^2 * exp(-(r_prime(r)^3))
## plot analytical solution
lines(x = r,  y = distr_ana)
legend("topright",bty="n","(b)")