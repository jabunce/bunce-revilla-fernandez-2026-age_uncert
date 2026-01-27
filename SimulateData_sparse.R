

##### prior predictive simulations from composite model, multiple groups

# post <- readRDS("post_USMatsi.RDS")

# #Nsims <- 30 # number of trajectories to simulate
# #Nages <- 25 # number of times to measure each individual (1 measurement per year)
# #pNages <- 2 # number of times to measure each indiv in temporally sparse dataset
# #intAges <- 2 # number of year between measurements in sparse dataset

# muQ <- rep(0, times=5) 
# muK <- rep(0, times=5) 
# muH <- rep(0, times=5) 
# muI <- rep(0, times=5) 

# for ( z in 1:5 ){
#   muQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) # Eth=1 is Berkeley, Eth=2 is Matsigenka
#   muK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
#   muH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
#   muI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
# } # for z



# priors come from this model fit to female Berkeley data, from conception
muQ1 <- 0.0843787 
muQ2 <- 0.1575626  
muQ3 <- 0.3040393  
muQ4 <- 0.04864028  
muQ5 <- 0.122695  

muK1 <- 81.60203  
muK2 <- 12.34654  
muK3 <- 1.565312  
muK4 <- 10.79656   
muK5 <- 8.873717  

muH1 <- 57.45683  
muH2 <- 10.46488 
muH3 <- 2.623266 
muH4 <- 6.05946 
muH5 <- 6.232012 

muI1 <- 0
muI2 <- 0.1215579 
muI3 <- 0.7427817 
muI4 <- 2.542764 
muI5 <- 9.626345 
muI6 <- 20.00029 # max age for start of last process


muQ <- rep(0, times=5) 
muK <- rep(0, times=5) 
muH <- rep(0, times=5) 
muI <- rep(0, times=5)

for ( z in 1:5 ){
  muQ[z] <- eval( parse( text = paste("muQ", z, sep="") ) ) 
  muK[z] <- eval( parse( text = paste("muK", z, sep="") ) )
  muH[z] <- eval( parse( text = paste("muH", z, sep="") ) )
  muI[z] <- eval( parse( text = paste("muI", z+1, sep="") ) )
} # for z




# transformed
muLQ1 <- log(muQ1)         # assumes muQ1 is the median on the original (un-logged) scale
muLQ2 <- log(muQ2)
muLQ3 <- log(muQ3)
muLQ4 <- log(muQ4)        
muLQ5 <- log(muQ5)

muLK1 <- log(muK1) 
muLK2 <- log(muK2)
muLK3 <- log(muK3)
muLK4 <- log(muK4)
muLK5 <- log(muK5)

muR1 <- muH1 - 0.5*muK1
muR2 <- muH2 - 0.5*muK2
muR3 <- muH3 - 0.5*muK3
muR4 <- muH4 - 0.5*muK4
muR5 <- muH5 - 0.5*muK5

muLI1 <- 0
muLI2 <- log(muI2)
muLI3 <- log(muI3)
muLI4 <- log(muI4)
muLI5 <- log(muI5)
muLIM <- log(muI6)




### Population mean parameter values for simulated dataset: model fit to Matsigenka girls from conception

# Eth_group <- 2 # group to use for group mean trajectory, 1=Berkeley, 2=Matsigenka
# Eth_indiv <- 1 # group to use for individual level variance about group mean trajectory

# SmQ <- rep(0, times=5) 
# SmK <- rep(0, times=5) 
# SmH <- rep(0, times=5) 
# SmI <- rep(0, times=5) 

# for ( z in 1:5 ){
#   SmQ[z] <- mean( pull(post, paste("mQ[", Eth_group, ",", z, "]", sep="")) ) # Eth=2 is Matsigenka
#   SmK[z] <- mean( pull(post, paste("mK[", Eth_group, ",", z, "]", sep="")) )
#   SmH[z] <- mean( pull(post, paste("mH[", Eth_group, ",", z, "]", sep="")) )
#   SmI[z] <- mean( pull(post, paste("mI[", Eth_group, ",", z, "]", sep="")) )
# } # for z


SmuQ1 <- 0.08454261
SmuQ2 <- 0.1661457 
SmuQ3 <- 0.3062495
SmuQ4 <- 0.04726658 
SmuQ5 <- 0.1238895 

SmuK1 <- 81.9413 
SmuK2 <- 12.56496 
SmuK3 <- 1.556003
SmuK4 <- 10.27372 
SmuK5 <- 8.587737 

SmuH1 <- 57.61856 
SmuH2 <- 10.56665
SmuH3 <- 2.531036 
SmuH4 <- 5.787023
SmuH5 <- 5.988818 

SmuI1 <- 0
SmuI2 <- 0.1218582 
SmuI3 <- 0.7337505 
SmuI4 <- 2.531476 
SmuI5 <- 9.79557 
SmuI6 <- 19.99984 # max age for start of last process


SmQ <- rep(0, times=5) 
SmK <- rep(0, times=5) 
SmH <- rep(0, times=5) 
SmI <- rep(0, times=5) 

for ( z in 1:5 ){
  SmQ[z] <- eval( parse( text = paste("SmuQ", z, sep="") ) ) 
  SmK[z] <- eval( parse( text = paste("SmuK", z, sep="") ) )
  SmH[z] <- eval( parse( text = paste("SmuH", z, sep="") ) )
  SmI[z] <- eval( parse( text = paste("SmuI", z+1, sep="") ) )
} # for z



# transformed
SmLQ <- rep(0, times=5) 
SmLK <- rep(0, times=5) 
SmR  <- rep(0, times=5) 
SmLI <- rep(0, times=5) 

for ( z in 1:5 ){
  SmLQ[z] <- log(SmQ[z])
  SmLK[z] <- log(SmK[z])
  SmR[z] <- SmH[z] - 0.5*SmK[z]
  SmLI[z] <- log(SmI[z])
} # for z


SmuLQ1 <- log(SmuQ1)         # assumes SmuQ1 is the median on the original (un-logged) scale
SmuLQ2 <- log(SmuQ2)
SmuLQ3 <- log(SmuQ3)
SmuLQ4 <- log(SmuQ4)        
SmuLQ5 <- log(SmuQ5)

SmuLK1 <- log(SmuK1) 
SmuLK2 <- log(SmuK2)
SmuLK3 <- log(SmuK3)
SmuLK4 <- log(SmuK4)
SmuLK5 <- log(SmuK5)

SmuR1 <- SmuH1 - 0.5*SmuK1
SmuR2 <- SmuH2 - 0.5*SmuK2
SmuR3 <- SmuH3 - 0.5*SmuK3
SmuR4 <- SmuH4 - 0.5*SmuK4
SmuR5 <- SmuH5 - 0.5*SmuK5

SmuLI1 <- 0
SmuLI2 <- log(SmuI2)
SmuLI3 <- log(SmuI3)
SmuLI4 <- log(SmuI4)
SmuLI5 <- log(SmuI5)
SmuLIM <- log(SmuI6)


                    

# individual offsets to the group-mean trajectory

# I_cov_mat <- array(data=0, dim=c(20,20))

# for ( x in 1:20 ){
#   for ( y in 1:20 ){
#     I_cov_mat[x,y] <- mean( pull(post, paste("I_cov_mat[", Eth_indiv, ",", x, ",", y, "]", sep="")) ) # variance/covariance matrix among parameters from fit model, Eth=1 Berkeley, Eth=2 Matsigenka
#   } # for y
# } # for x
# saveRDS(I_cov_mat, "I_cov_mat.RDS")

I_cov_mat <- readRDS("I_cov_mat.RDS")



# indiv offsets to the transformed parameter values
mIndOffsetLQ <- matrix(0, nrow=Nsims, ncol=5) 
mIndOffsetLK <- matrix(0, nrow=Nsims, ncol=5) 
mIndOffsetR  <- matrix(0, nrow=Nsims, ncol=5) 
mIndOffsetLI <- matrix(0, nrow=Nsims, ncol=5)

SiLQ1 <- rep(0, times=Nsims)
SiLQ2 <- rep(0, times=Nsims)
SiLQ3 <- rep(0, times=Nsims)
SiLQ4 <- rep(0, times=Nsims)
SiLQ5 <- rep(0, times=Nsims)

SiLK1 <- rep(0, times=Nsims)
SiLK2 <- rep(0, times=Nsims)
SiLK3 <- rep(0, times=Nsims)
SiLK4 <- rep(0, times=Nsims)
SiLK5 <- rep(0, times=Nsims)

SiR1 <- rep(0, times=Nsims)
SiR2 <- rep(0, times=Nsims)
SiR3 <- rep(0, times=Nsims)
SiR4 <- rep(0, times=Nsims)
SiR5 <- rep(0, times=Nsims)

SiLI1 <- rep(0, times=Nsims)
SiLI2 <- rep(0, times=Nsims)
SiLI3 <- rep(0, times=Nsims)
SiLI4 <- rep(0, times=Nsims)
SiLI5 <- rep(0, times=Nsims)
SiLIM <- rep(0, times=Nsims)


# un-transformed parameter values for each individual
sQ <- matrix(0, nrow=Nsims, ncol=5) 
sK <- matrix(0, nrow=Nsims, ncol=5)
sH <- matrix(0, nrow=Nsims, ncol=5) 
sI <- matrix(0, nrow=Nsims, ncol=5) 

sQ1 <- rep(0, times=Nsims)
sQ2 <- rep(0, times=Nsims)
sQ3 <- rep(0, times=Nsims)
sQ4 <- rep(0, times=Nsims)
sQ5 <- rep(0, times=Nsims)

sK1 <- rep(0, times=Nsims)
sK2 <- rep(0, times=Nsims)
sK3 <- rep(0, times=Nsims)
sK4 <- rep(0, times=Nsims)
sK5 <- rep(0, times=Nsims)

sH1 <- rep(0, times=Nsims)
sH2 <- rep(0, times=Nsims)
sH3 <- rep(0, times=Nsims)
sH4 <- rep(0, times=Nsims)
sH5 <- rep(0, times=Nsims)

sI1 <- rep(0, times=Nsims)
sI2 <- rep(0, times=Nsims)
sI3 <- rep(0, times=Nsims)
sI4 <- rep(0, times=Nsims)
sI5 <- rep(0, times=Nsims)
sIM <- rep(0, times=Nsims)


# simulated dataset
fullSimData <- as.data.frame( matrix(0, nrow=(Nsims*Nages), ncol=4) )
names(fullSimData)[1:4] <- c("ID", "ageSinceConception", "height", "CellWeightg")


n <- 1  # row of dataset 

for ( j in 1:Nsims ){ 

  for ( z in 1:5 ){
    mIndOffsetLQ[j,z] <- mvrnorm( n=1, mu=rep(0,length(I_cov_mat[1,])), Sigma=I_cov_mat )[ z      ] # sample indiv offsets to each parameter given covariance among parameters 
    mIndOffsetLK[j,z] <- mvrnorm( n=1, mu=rep(0,length(I_cov_mat[1,])), Sigma=I_cov_mat )[ z + 5  ]
    mIndOffsetR[j,z]  <- mvrnorm( n=1, mu=rep(0,length(I_cov_mat[1,])), Sigma=I_cov_mat )[ z + 10 ]
    mIndOffsetLI[j,z] <- mvrnorm( n=1, mu=rep(0,length(I_cov_mat[1,])), Sigma=I_cov_mat )[ z + 15 ]
  } # for z

  SiLQ1[j] <- mIndOffsetLQ[j,1]
  SiLQ2[j] <- mIndOffsetLQ[j,2]
  SiLQ3[j] <- mIndOffsetLQ[j,3]
  SiLQ4[j] <- mIndOffsetLQ[j,4]
  SiLQ5[j] <- mIndOffsetLQ[j,5]

  SiLK1[j] <- mIndOffsetLK[j,1]
  SiLK2[j] <- mIndOffsetLK[j,2]
  SiLK3[j] <- mIndOffsetLK[j,3]
  SiLK4[j] <- mIndOffsetLK[j,4]
  SiLK5[j] <- mIndOffsetLK[j,5]

  SiR1[j] <- mIndOffsetR[j,1]
  SiR2[j] <- mIndOffsetR[j,2]
  SiR3[j] <- mIndOffsetR[j,3]
  SiR4[j] <- mIndOffsetR[j,4]
  SiR5[j] <- mIndOffsetR[j,5]

  SiLI1[j] <- 0.00000001
  SiLI2[j] <- mIndOffsetLI[j,1]
  SiLI3[j] <- mIndOffsetLI[j,2]
  SiLI4[j] <- mIndOffsetLI[j,3]
  SiLI5[j] <- mIndOffsetLI[j,4]
  SiLIM[j] <- mIndOffsetLI[j,5]


  # get each individual's un-transformed parameter values
  for (z in 1:5) {
      sQ[j,z] = exp( SmLQ[z] + mIndOffsetLQ[j,z] );
      sK[j,z] = exp( SmLK[z] + mIndOffsetLK[j,z] );
      sH[j,z] = sK[j,z]/2 + SmR[z] + mIndOffsetR[j,z];
      sI[j,z] = exp( SmLI[z] + mIndOffsetLI[j,z] );
    } # for z

  sQ1[j] <- sQ[j,1]
  sQ2[j] <- sQ[j,2]
  sQ3[j] <- sQ[j,3]
  sQ4[j] <- sQ[j,4]
  sQ5[j] <- sQ[j,5]

  sK1[j] <- sK[j,1]
  sK2[j] <- sK[j,2]
  sK3[j] <- sK[j,3]
  sK4[j] <- sK[j,4]
  sK5[j] <- sK[j,5]

  sH1[j] <- sH[j,1]
  sH2[j] <- sH[j,2]
  sH3[j] <- sH[j,3]
  sH4[j] <- sH[j,4]
  sH5[j] <- sH[j,5]

  sI1[j] <- 0.00000001
  sI2[j] <- sI[j,1]
  sI3[j] <- sI[j,2]
  sI4[j] <- sI[j,3]
  sI5[j] <- sI[j,4]
  sIM[j] <- sI[j,5]


  
  # simulate heights and weights for each indiv, ages 1 to 25 years since conception
  for ( a in 1:Nages ){
    fullSimData[n,1] <-  j # ID
    fullSimData[n,2] <-  a # age since conception
    fullSimData[n,3] <- ifelse( a <= sI[j,1], 0.012 + ( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1]),

                        ifelse( a <= sI[j,2], 0.012 + ( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1]) +
                                                      ( 2*sH[j,2]/sK[j,2] * ( 1 - exp(sK[j,2]*sQ[j,2]*( sI[j,1] - a )/( 1 + 2*sQ[j,2] )) ) )^(1/sQ[j,2]),

                        ifelse( a <= sI[j,3], 0.012 + ( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1]) +
                                                      ( 2*sH[j,2]/sK[j,2] * ( 1 - exp(sK[j,2]*sQ[j,2]*( sI[j,1] - a )/( 1 + 2*sQ[j,2] )) ) )^(1/sQ[j,2]) +
                                                      ( 2*sH[j,3]/sK[j,3] * ( 1 - exp(sK[j,3]*sQ[j,3]*( sI[j,2] - a )/( 1 + 2*sQ[j,3] )) ) )^(1/sQ[j,3]),

                        ifelse( a <= sI[j,4], 0.012 + ( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1]) +
                                                      ( 2*sH[j,2]/sK[j,2] * ( 1 - exp(sK[j,2]*sQ[j,2]*( sI[j,1] - a )/( 1 + 2*sQ[j,2] )) ) )^(1/sQ[j,2]) +
                                                      ( 2*sH[j,3]/sK[j,3] * ( 1 - exp(sK[j,3]*sQ[j,3]*( sI[j,2] - a )/( 1 + 2*sQ[j,3] )) ) )^(1/sQ[j,3]) +
                                                      ( 2*sH[j,4]/sK[j,4] * ( 1 - exp(sK[j,4]*sQ[j,4]*( sI[j,3] - a )/( 1 + 2*sQ[j,4] )) ) )^(1/sQ[j,4]),

                                              0.012 + ( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1]) +
                                                      ( 2*sH[j,2]/sK[j,2] * ( 1 - exp(sK[j,2]*sQ[j,2]*( sI[j,1] - a )/( 1 + 2*sQ[j,2] )) ) )^(1/sQ[j,2]) +
                                                      ( 2*sH[j,3]/sK[j,3] * ( 1 - exp(sK[j,3]*sQ[j,3]*( sI[j,2] - a )/( 1 + 2*sQ[j,3] )) ) )^(1/sQ[j,3]) +
                                                      ( 2*sH[j,4]/sK[j,4] * ( 1 - exp(sK[j,4]*sQ[j,4]*( sI[j,3] - a )/( 1 + 2*sQ[j,4] )) ) )^(1/sQ[j,4]) +
                                                      ( 2*sH[j,5]/sK[j,5] * ( 1 - exp(sK[j,5]*sQ[j,5]*( sI[j,4] - a )/( 1 + 2*sQ[j,5] )) ) )^(1/sQ[j,5])
                        ) ) ) )

    fullSimData[n,4] <- ifelse( a <= sI[j,1], 3.6e-9 + pi*( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1] + 2),

                        ifelse( a <= sI[j,2], 3.6e-9 + pi*( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1] + 2) +
                                                       pi*( 2*sH[j,2]/sK[j,2] * ( 1 - exp(sK[j,2]*sQ[j,2]*( sI[j,1] - a )/( 1 + 2*sQ[j,2] )) ) )^(1/sQ[j,2] + 2),

                        ifelse( a <= sI[j,3], 3.6e-9 + pi*( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1] + 2) +
                                                       pi*( 2*sH[j,2]/sK[j,2] * ( 1 - exp(sK[j,2]*sQ[j,2]*( sI[j,1] - a )/( 1 + 2*sQ[j,2] )) ) )^(1/sQ[j,2] + 2) +
                                                       pi*( 2*sH[j,3]/sK[j,3] * ( 1 - exp(sK[j,3]*sQ[j,3]*( sI[j,2] - a )/( 1 + 2*sQ[j,3] )) ) )^(1/sQ[j,3] + 2),

                        ifelse( a <= sI[j,4], 3.6e-9 + pi*( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1] + 2) +
                                                       pi*( 2*sH[j,2]/sK[j,2] * ( 1 - exp(sK[j,2]*sQ[j,2]*( sI[j,1] - a )/( 1 + 2*sQ[j,2] )) ) )^(1/sQ[j,2] + 2) +
                                                       pi*( 2*sH[j,3]/sK[j,3] * ( 1 - exp(sK[j,3]*sQ[j,3]*( sI[j,2] - a )/( 1 + 2*sQ[j,3] )) ) )^(1/sQ[j,3] + 2) +
                                                       pi*( 2*sH[j,4]/sK[j,4] * ( 1 - exp(sK[j,4]*sQ[j,4]*( sI[j,3] - a )/( 1 + 2*sQ[j,4] )) ) )^(1/sQ[j,4] + 2),                                                                                                                                                                  

                                             3.6e-9 + pi*( 2*sH[j,1]/sK[j,1] * ( 1 - exp(sK[j,1]*sQ[j,1]*( 0      - a )/( 1 + 2*sQ[j,1] )) ) )^(1/sQ[j,1] + 2) +
                                                      pi*( 2*sH[j,2]/sK[j,2] * ( 1 - exp(sK[j,2]*sQ[j,2]*( sI[j,1] - a )/( 1 + 2*sQ[j,2] )) ) )^(1/sQ[j,2] + 2) +
                                                      pi*( 2*sH[j,3]/sK[j,3] * ( 1 - exp(sK[j,3]*sQ[j,3]*( sI[j,2] - a )/( 1 + 2*sQ[j,3] )) ) )^(1/sQ[j,3] + 2) +
                                                      pi*( 2*sH[j,4]/sK[j,4] * ( 1 - exp(sK[j,4]*sQ[j,4]*( sI[j,3] - a )/( 1 + 2*sQ[j,4] )) ) )^(1/sQ[j,4] + 2) +
                                                      pi*( 2*sH[j,5]/sK[j,5] * ( 1 - exp(sK[j,5]*sQ[j,5]*( sI[j,4] - a )/( 1 + 2*sQ[j,5] )) ) )^(1/sQ[j,5] + 2)

                        ) ) ) )
    
    n <- n + 1 # row of dataset

  } # for a
} # for j



# add columns for birth and interview years
fullSimData$interview_year <- fullSimData$ageSinceConception + 2000
fullSimData$BirthYear <- 2000 + 0.75
fullSimData$BirthYearUp <- 2000 + 0.75
fullSimData$BirthYearLo <- 2000 + 0.75


# make column marking first observation
fullSimData <- fullSimData[order(fullSimData$ID, fullSimData$interview_year),] #order by ID, then by interview year
fullSimData$firstObs <- 1


for ( x in 1:( nrow(fullSimData) - 1 ) ) {
  if ( fullSimData$ID[x] == fullSimData$ID[x+1] & 
       fullSimData$interview_year[x] < fullSimData$interview_year[x+1] ) {

       fullSimData$firstObs[x+1] <- 0
  } #if

} #for x

fullSimData$firstObs
fullSimData[1:5,]


# add age uncertainty
fullSimData$ObsBirthYear <- NA

for ( x in 1:nrow(fullSimData) ) {

  if (fullSimData$firstObs[x] == 1) {
    fullSimData$ObsBirthYear[x] <- rnorm( n=1, mean=fullSimData$BirthYear[x], sd=xsd*(fullSimData$ageSinceConception[x] - 0.75) ) #sd = xsd*(age since birth)
  } else {
    fullSimData$ObsBirthYear[x] <- fullSimData$ObsBirthYear[x-1]
  } # else
} # for





################################# make cross-sectional dataset

xSimData <- as.data.frame(matrix(NA, nrow=Nsims, ncol=ncol(fullSimData)) )
names(xSimData) <- names(fullSimData)

sampAge <- sample(x=1:Nages, size=Nsims, replace=TRUE) # random observation age for each individual to pull out
indivID <- unique(fullSimData$ID)

index <- 1
for ( x in 1:nrow(fullSimData) ) {
  if ( fullSimData$ID[x] == indivID[index] & 
       fullSimData$ageSinceConception[x] == sampAge[index] ) {

       xSimData[index,] <- fullSimData[x,]
       if ( index < length(indivID) ) {
         index <- index + 1
       } # if
  } #if
} #for x

xSimData$firstObs <- 1


# add age uncertainty to cross-sectional dataset

xSimData$ObsBirthYear <- rnorm( n=nrow(xSimData), mean=xSimData$BirthYear, sd=xsd*(xSimData$ageSinceConception - 0.75) ) #sd = xsd*(age since birth)

for ( x in 1:nrow(xSimData) ) {
  if ( xSimData$ObsBirthYear[x] > xSimData$interview_year[x] ) {
    xSimData$ObsBirthYear[x] <- xSimData$interview_year[x]      # avoid negative observed ages
  } # if

  # if ( (xSimData$interview_year[x] - xSimData$ObsBirthYear[x]) > 25) {
  #   xSimData$ObsBirthYear[x] <- xSimData$BirthYear[x] + (xSimData$BirthYear[x] - xSimData$ObsBirthYear[x])    # avoid observed ages > 25, i.e., when ObsBirthYear < BirthYear then age will be inflated
  # } # if

} # for


#mean(xSimData$ObsBirthYear - xSimData$BirthYear)

#xSimData$interview_year - xSimData$ObsBirthYear


################################ make temporally-sparse dataset


pSimData <- as.data.frame(matrix(NA, nrow=Nsims*pNages, ncol=ncol(xSimData)) )
names(pSimData) <- names(xSimData)

# use sampAge and indivID from above
#sampAge <- sample(x=1:Nages, size=Nsims, replace=TRUE) # random observation age (first observation) for each individual to pull out
#indivID <- unique(fullSimData$ID)

#pNages <- 5 # number of times to measure each individual
#intAges <- 2 # number of years between measurements

IDindex <- 1
rowindex <- 1
for ( x in 1:nrow(fullSimData) ) {
  if ( fullSimData$ID[x] == indivID[IDindex] & 
       fullSimData$ageSinceConception[x] == sampAge[IDindex] ) {

    pSimData[rowindex,] <- fullSimData[x,]
    pSimData$ObsBirthYear[rowindex] <- xSimData$ObsBirthYear[IDindex]


    if ( ( fullSimData$ageSinceConception[x] + pNages*intAges ) <= 25 ) {                   #( xSimData$interview_year[IDindex] - xSimData$ObsBirthYear[IDindex] + intAges ) <= 25
      
      for ( z in 1:(pNages - 1) ) {                                                  # pNages = number of times to measure each individual
        pSimData[rowindex + z,] <- fullSimData[x + z*intAges,]                       # intAges = number of years between measurements
        pSimData$ObsBirthYear[rowindex + z] <- pSimData$ObsBirthYear[rowindex]
        pSimData$firstObs[rowindex] <- 1
        pSimData$firstObs[rowindex + z] <- 0
      } #for z 

    } else {

      for ( z in 1:(pNages - 1) ) {                                                  
        pSimData[rowindex + z,] <- fullSimData[x - z*intAges,]                       
        pSimData$ObsBirthYear[rowindex + z] <- pSimData$ObsBirthYear[rowindex]
        pSimData$firstObs[rowindex] <- 0
        pSimData$firstObs[rowindex + z] <- 0
      } #for z 

      pSimData$firstObs[rowindex + z] <- 1

    } # else


    if ( IDindex < length(indivID) ) {
      IDindex <- IDindex + 1
      rowindex <- rowindex + pNages
    } # if

  } #if
} #for x

pSimData <- pSimData[order(pSimData$ID, pSimData$ageSinceConception),] #sort by ID then by age



