

# function to calculate proportions of phases for weighting by age
propparam <- function(x=1, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                           K1=1, K2=1, K3=1, K4=1, K5=1,
                           H1=1, H2=1, H3=1, H4=1, H5=1,
                           I1=1, I2=1, I3=1, I4=1){
      
      I0 <- rep(0, times=length(I1))

      # total height grown since egg size at age x
      totHeight <- ifelse( x <= I1, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1),

                   ifelse( x <= I2, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2),

                   ifelse( x <= I3, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3),

                   ifelse( x <= I4, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3) +
                                            ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4),

                                    0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3) +
                                            ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4) +
                                            ( 2*H5/K5 * ( 1 - exp(K5*Q5*( I4 - x )/( 1 + 2*Q5 )) ) )^(1/Q5)

                   ) ) ) ) - 0.012

      # heights of each process at time x
      Height1 <- ifelse( x >= I0 , ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1), 0) 

      Height2 <- ifelse( x >= I1 , ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2), 0) 

      Height3 <- ifelse( x >= I2 , ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3), 0) 

      Height4 <- ifelse( x >= I3 , ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4), 0) 

      Height5 <- ifelse( x >= I4 , ( 2*H5/K5 * ( 1 - exp(K5*Q5*( I4 - x )/( 1 + 2*Q5 )) ) )^(1/Q5), 0) 

      # vector of proportions of total height contributed by each process at time x
      p <- rbind(Height1/totHeight,
                 Height2/totHeight,
                 Height3/totHeight,
                 Height4/totHeight,
                 Height5/totHeight)


      # max asymptotic height of each process
      maxHeight1 <- (2*H1/K1)^(1/Q1)
      maxHeight2 <- (2*H2/K2)^(1/Q2)
      maxHeight3 <- (2*H3/K3)^(1/Q3)
      maxHeight4 <- (2*H4/K4)^(1/Q4)
      maxHeight5 <- (2*H5/K5)^(1/Q5)

      # vector of proportions of original metabolic activity for each process at time x.
      # On average, cells lose 75% of their metabolic activity by the time an individual growth process finishes at asymptote, or 75% of cells convert to things that don't divide.
      # Mostly because active bone marrow converted to marrow fat, and bone cells converted to bone matrix.
      m <- rbind( 1 - 0.75*(Height1/maxHeight1),
                  1 - 0.75*(Height2/maxHeight2),
                  1 - 0.75*(Height3/maxHeight3),
                  1 - 0.75*(Height4/maxHeight4),
                  1 - 0.75*(Height5/maxHeight5) )


      return( list(p=p,m=m) )

} # propparam


vpropparam <- Vectorize(propparam) # vectorize the function so that it can take vectors as arguments


# e <- 5

# Q1 <- Q1ber[1:e]
# Q2 <- Q2ber[1:e]
# Q3 <- Q3ber[1:e]
# Q4 <- Q4ber[1:e]
# Q5 <- Q5ber[1:e]

# K1 <- K1ber[1:e]
# K2 <- K2ber[1:e]
# K3 <- K3ber[1:e]
# K4 <- K4ber[1:e]
# K5 <- K5ber[1:e]

# H1 <- H1ber[1:e]
# H2 <- H2ber[1:e]
# H3 <- H3ber[1:e]
# H4 <- H4ber[1:e]
# H5 <- H5ber[1:e]

# I1 <- I1ber[1:e]
# I2 <- I2ber[1:e]
# I3 <- I3ber[1:e]
# I4 <- I4ber[1:e]

# d <- 5

# p <- matrix(unlist(vpropparam(x=d, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                                    K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                                    H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                                    I1=I1, I2=I2, I3=I3, I4=I4)["p",]), nrow=5, byrow=FALSE)
# p
# colSums(p)


# m <- matrix(unlist(vpropparam(x=d, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                                    K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                                    H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                                    I1=I1, I2=I2, I3=I3, I4=I4)["m",]), nrow=5, byrow=FALSE)
# m
# colSums(m)




########## create lists of parameter posteriors


##### true simulated values

mean_param_list_true <- list( SmuQ1, SmuQ2, SmuQ3, SmuQ4, SmuQ5,
                              SmuK1, SmuK2, SmuK3, SmuK4, SmuK5,
                              SmuH1, SmuH2, SmuH3, SmuH4, SmuH5,  # original H in units of g/cm^2 of skin surface of function=~5% of body mass (**DO NOT need to multiply by (cm^2 skin surface area)/(g body mass)*1/0.05, it will be multiplied later after running the functions **). Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                              SmuI1, SmuI2, SmuI3, SmuI4, SmuI5 
                            )

names(mean_param_list_true) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                  "K1", "K2", "K3", "K4", "K5",
                                  "H1", "H2", "H3", "H4", "H5",
                                  "I1", "I2", "I3", "I4", "I5"
                                )

#str(mean_param_list_true)




##### prior values

mean_param_list_prior <- list( muQ1, muQ2, muQ3, muQ4, muQ5,
                               muK1, muK2, muK3, muK4, muK5,
                               muH1, muH2, muH3, muH4, muH5,  # original H in units of g/cm^2 of skin surface of function=~5% of body mass (**DO NOT need to multiply by (cm^2 skin surface area)/(g body mass)*1/0.05 **). Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                               muI1, muI2, muI3, muI4, muI5 
                             )

names(mean_param_list_prior) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                  "K1", "K2", "K3", "K4", "K5",
                                  "H1", "H2", "H3", "H4", "H5",
                                  "I1", "I2", "I3", "I4", "I5"
                                )

#str(mean_param_list_true)




######## 20 individuals, measured 5 times, 2 year intervals

post6_s10_1 <- readRDS(modpost_name6) 
post6_s10_2 <- readRDS(modpost_name7) 
post6_s10_3 <- readRDS(modpost_name8) 
post6_s10_4 <- readRDS(modpost_name9) 
post6_s10_5 <- readRDS(modpost_name10) 


post <- post6_s10_1

mean_param_list_s10_1 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s10_1) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s10_2

mean_param_list_s10_2 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s10_2) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s10_3

mean_param_list_s10_3 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s10_3) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s10_4

mean_param_list_s10_4 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s10_4) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )


##

post <- post6_s10_5

mean_param_list_s10_5 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s10_5) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

 rm(post6_s10_1, post6_s10_2, post6_s10_3, post6_s10_4, post6_s10_5) #(list = ls(all=TRUE))





######## 30 full data

post6_s200_1 <- readRDS(modpost_name11) 
post6_s200_2 <- readRDS(modpost_name12) 
post6_s200_3 <- readRDS(modpost_name13) 
post6_s200_4 <- readRDS(modpost_name14) 
post6_s200_5 <- readRDS(modpost_name15) 

post <- post6_s200_1

mean_param_list_s200_1 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s200_1) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s200_2

mean_param_list_s200_2 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s200_2) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s200_3

mean_param_list_s200_3 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s200_3) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s200_4

mean_param_list_s200_4 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s200_4) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )


##

post <- post6_s200_5

mean_param_list_s200_5 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s200_5) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

 rm(post6_s200_1, post6_s200_2, post6_s200_3, post6_s200_4, post6_s200_5) #(list = ls(all=TRUE))







age_seq <- seq(from=(0.1), to=(26.75), by=0.1)  #c(0.1,1,2)
num_samps <- length(mean_param_list_s10_1$Q1)


# true params
tQ_true <- rep(NA, times=length(age_seq))
tK_true <- rep(NA, times=length(age_seq))
tH_true <- rep(NA, times=length(age_seq))

# prior params
tQ_prior <- rep(NA, times=length(age_seq))
tK_prior <- rep(NA, times=length(age_seq))
tH_prior <- rep(NA, times=length(age_seq))



# 20 individuals, measured 5 times, 2 year intervals
tQ_s10_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tQ_s10_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tQ_s10_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tQ_s10_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tQ_s10_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

tK_s10_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tK_s10_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tK_s10_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tK_s10_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tK_s10_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

tH_s10_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tH_s10_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tH_s10_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tH_s10_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tH_s10_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

conQ_s10_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conQ_s10_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conQ_s10_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conQ_s10_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conQ_s10_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

conK_s10_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conK_s10_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conK_s10_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conK_s10_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conK_s10_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

conH_s10_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conH_s10_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conH_s10_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conH_s10_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conH_s10_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)



# 30 full data
tQ_s200_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tQ_s200_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tQ_s200_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tQ_s200_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tQ_s200_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

tK_s200_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tK_s200_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tK_s200_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tK_s200_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tK_s200_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

tH_s200_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tH_s200_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tH_s200_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tH_s200_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
tH_s200_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

conQ_s200_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conQ_s200_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conQ_s200_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conQ_s200_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conQ_s200_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

conK_s200_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conK_s200_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conK_s200_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conK_s200_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conK_s200_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)

conH_s200_1 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conH_s200_2 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conH_s200_3 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conH_s200_4 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)
conH_s200_5 <- matrix(data=NA, nrow=length(age_seq), ncol=num_samps)




for ( t in 1:length(age_seq) ){

      # true parameters
      plist <- mean_param_list_true
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      # don't multiply Q's by m. Metabolism doesn't affect allometry. H and K remain fairly constant after adolescence and adulthood, as reporterd for human metabolism
      tQ_true[t] <- P[1]*plist$Q1 + P[2]*plist$Q2 + P[3]*plist$Q3 + P[4]*plist$Q4 + P[5]*plist$Q5
      tK_true[t] <- P[1]*M[1]*plist$K1 + P[2]*M[2]*plist$K2 + P[3]*M[3]*plist$K3 + P[4]*M[4]*plist$K4 + P[5]*M[5]*plist$K5
      # original H in units of g/cm^2 of 5% of total body skin surface. Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
      tH_true[t] <- P[1]*M[1]*plist$H1*(2/32) + P[2]*M[2]*plist$H2*(2/32) + P[3]*M[3]*plist$H3*(2/32) + P[4]*M[4]*plist$H4*(2/32) + P[5]*M[5]*plist$H5*(2/32)


      # prior parameters
      plist <- mean_param_list_prior
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      # don't multiply Q's by m. Metabolism doesn't affect allometry. H and K remain fairly constant after adolescence and adulthood, as reporterd for human metabolism
      tQ_prior[t] <- P[1]*plist$Q1 + P[2]*plist$Q2 + P[3]*plist$Q3 + P[4]*plist$Q4 + P[5]*plist$Q5
      tK_prior[t] <- P[1]*M[1]*plist$K1 + P[2]*M[2]*plist$K2 + P[3]*M[3]*plist$K3 + P[4]*M[4]*plist$K4 + P[5]*M[5]*plist$K5
      # original H in units of g/cm^2 of 5% of total body skin surface. Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
      tH_prior[t] <- P[1]*M[1]*plist$H1*(2/32) + P[2]*M[2]*plist$H2*(2/32) + P[3]*M[3]*plist$H3*(2/32) + P[4]*M[4]*plist$H4*(2/32) + P[5]*M[5]*plist$H5*(2/32)



      ###################################### 20 individuals, measured 5 times, 2 year intervals
      plist <- mean_param_list_s10_1
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s10_1[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s10_1[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s10_1[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s10_1[t,] <- tQ_s10_1[t,] - tQ_true[t]
      conK_s10_1[t,] <- tK_s10_1[t,] - tK_true[t]
      conH_s10_1[t,] <- tH_s10_1[t,] - tH_true[t]


      plist <- mean_param_list_s10_2
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s10_2[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s10_2[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s10_2[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s10_2[t,] <- tQ_s10_2[t,] - tQ_true[t]
      conK_s10_2[t,] <- tK_s10_2[t,] - tK_true[t]
      conH_s10_2[t,] <- tH_s10_2[t,] - tH_true[t]


      plist <- mean_param_list_s10_3
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s10_3[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s10_3[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s10_3[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s10_3[t,] <- tQ_s10_3[t,] - tQ_true[t]
      conK_s10_3[t,] <- tK_s10_3[t,] - tK_true[t]
      conH_s10_3[t,] <- tH_s10_3[t,] - tH_true[t]


      plist <- mean_param_list_s10_4
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s10_4[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s10_4[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s10_4[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s10_4[t,] <- tQ_s10_4[t,] - tQ_true[t]
      conK_s10_4[t,] <- tK_s10_4[t,] - tK_true[t]
      conH_s10_4[t,] <- tH_s10_4[t,] - tH_true[t]


      plist <- mean_param_list_s10_5
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s10_5[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s10_5[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s10_5[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s10_5[t,] <- tQ_s10_5[t,] - tQ_true[t]
      conK_s10_5[t,] <- tK_s10_5[t,] - tK_true[t]
      conH_s10_5[t,] <- tH_s10_5[t,] - tH_true[t]




      ###################################### 30 full data
      plist <- mean_param_list_s200_1
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s200_1[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s200_1[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s200_1[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s200_1[t,] <- tQ_s200_1[t,] - tQ_true[t]
      conK_s200_1[t,] <- tK_s200_1[t,] - tK_true[t]
      conH_s200_1[t,] <- tH_s200_1[t,] - tH_true[t]


      plist <- mean_param_list_s200_2
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s200_2[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s200_2[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s200_2[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s200_2[t,] <- tQ_s200_2[t,] - tQ_true[t]
      conK_s200_2[t,] <- tK_s200_2[t,] - tK_true[t]
      conH_s200_2[t,] <- tH_s200_2[t,] - tH_true[t]


      plist <- mean_param_list_s200_3
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s200_3[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s200_3[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s200_3[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s200_3[t,] <- tQ_s200_3[t,] - tQ_true[t]
      conK_s200_3[t,] <- tK_s200_3[t,] - tK_true[t]
      conH_s200_3[t,] <- tH_s200_3[t,] - tH_true[t]


      plist <- mean_param_list_s200_4
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s200_4[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s200_4[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s200_4[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s200_4[t,] <- tQ_s200_4[t,] - tQ_true[t]
      conK_s200_4[t,] <- tK_s200_4[t,] - tK_true[t]
      conH_s200_4[t,] <- tH_s200_4[t,] - tH_true[t]


      plist <- mean_param_list_s200_5
      funct_res <- vpropparam(x=age_seq[t],Q1=plist$Q1, Q2=plist$Q2, Q3=plist$Q3, Q4=plist$Q4, Q5=plist$Q5,
                                           K1=plist$K1, K2=plist$K2, K3=plist$K3, K4=plist$K4, K5=plist$K5,
                                           H1=plist$H1, H2=plist$H2, H3=plist$H3, H4=plist$H4, H5=plist$H5,
                                           I1=plist$I1, I2=plist$I2, I3=plist$I3, I4=plist$I4)
      P <- matrix(unlist(funct_res["p",]), nrow=5, byrow=FALSE)
      M <- matrix(unlist(funct_res["m",]), nrow=5, byrow=FALSE)
      tQ_s200_5[t,] <- P[1,]*plist$Q1 + P[2,]*plist$Q2 + P[3,]*plist$Q3 + P[4,]*plist$Q4 + P[5,]*plist$Q5
      tK_s200_5[t,] <- P[1,]*M[1,]*plist$K1 + P[2,]*M[2,]*plist$K2 + P[3,]*M[3,]*plist$K3 + P[4,]*M[4,]*plist$K4 + P[5,]*M[5,]*plist$K5
      tH_s200_5[t,] <- P[1,]*M[1,]*plist$H1*(2/32) + P[2,]*M[2,]*plist$H2*(2/32) + P[3,]*M[3,]*plist$H3*(2/32) + P[4,]*M[4,]*plist$H4*(2/32) + P[5,]*M[5,]*plist$H5*(2/32)
      conQ_s200_5[t,] <- tQ_s200_5[t,] - tQ_true[t]
      conK_s200_5[t,] <- tK_s200_5[t,] - tK_true[t]
      conH_s200_5[t,] <- tH_s200_5[t,] - tH_true[t]


} # for t





################################################


# line colors and sizes
colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)

BerIndivTraj_lwd <- 0.25
BerIndivTraj_col <- colorlist["2.4"]
BerIndivTraj_lty <- 1

MatIndivTraj_lwd <- 0.25
MatIndivTraj_col <- colorlist["1.4"]
MatIndivTraj_lty <- 1


BerMeanTraj_lwd <- 3
BerMeanTraj_col <- colorlist["2.1"]
BerMeanTraj_lty <- 1

MatMeanTraj_lwd <- 3
MatMeanTraj_col <- colorlist["1.1"]
MatMeanTraj_lty <- 1


BerArea_col <- colorlist["2.3"]

MatArea_col <- colorlist["1.3"]

ConLine_lwd <- 3
ConLine_col <- "black"
ConArea_col <- grey(0.5,alpha=0.5)

ZerLine_lwd <- 1
ZerLine_lty <- "33"       #lty: first number in string is dash length, second is white space length
ZerLine_col <- "black"






pdf(file="./Plots/params_combined_sims_l.pdf",
    height=10, width=10)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6
                      ),
        nrow=3, ncol=2, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1,1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 8)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# 20 individuals, measured 5 times, 2 year intervals  ###########################################################################################################################################################################

################ Q

 #set up plot
  #par(mar=c(5, 6, 2, 6), oma = c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0.07, 0.18), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.10,0.18,0.02), las=1 )
  axis( side=1, at=seq(0,25,5), labels=NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)


  ints <-  t(apply(tQ_s10_1, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tQ_s10_2, 1, HPDI, prob=0.9)) 
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tQ_s10_3, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tQ_s10_4, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tQ_s10_5, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)


  lines(x = age_seq,
        y = tQ_true,
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  lines(x = age_seq,
        y = rowMeans(tQ_s10_1),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tQ_s10_2),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tQ_s10_3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tQ_s10_4),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tQ_s10_5),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,0.2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.005,0.005+0.05), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.005, 0.005), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conQ_s10_1, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conQ_s10_2, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conQ_s10_3, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conQ_s10_4, 1, HPDI, prob=0.9)) 
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conQ_s10_5, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = age_seq,
        y = rowMeans(conQ_s10_1),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conQ_s10_2),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conQ_s10_3),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conQ_s10_4),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conQ_s10_5),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(age_seq),30),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



  par(xpd=TRUE)
  #text("Contrast", x=33, y=0, srt=270, las=3)



#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("Simulation", "Estimates", "Contrast")
xcoords <- c(0, 6, 12) # 0, 7, 23
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=0.3, y=15.2,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerMeanTraj_col, MatMeanTraj_col, ConLine_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=1.8,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = -0.35,
       ybottom = 13.3,
       xright = 21.91,
       ytop = 14.9,
       lwd=1)


# row labels

text("20 idv, 5 ms, 2 int", x=5, y=11.5, cex=2.5)
text("30 idv, 25 ms, 1 int", x=16, y=11.5, cex=2.5)

text(expression(paste(bolditalic("q"))), x=-4, y=5, cex=3)

text(expression(paste(bolditalic("K"))), x=-4, y=-8, cex=3)
text("(g/g)", x=-4, y=-10, cex=2.5)

text(expression(paste(bolditalic("H"))), x=-4, y=-21, cex=3)
text(expression(paste("(g/cm"^2,")", sep="")), x=-4, y=-23, cex=2.5)


text("Age since conception (years)", x=11, y=-31, cex=2.5)

text("birth", x=0.5, y=-29.3, cex=1.75)
text("birth", x=12, y=-29.3, cex=1.75)

text("Contrast", x=24.5, y=-8, cex=2.5, srt=270)


par(xpd=FALSE)


################### K

plot( x=0, y=60, type="n", ylim=c(4,24), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(8,24,4), labels=seq(8,24,4), las=1 )
  axis( side=1, at=seq(0,25,5), labels = NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(age_seq > mina))

  ints <-  t(apply(tK_s10_1, 1, HPDI, prob=0.9))[,]  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tK_s10_2, 1, HPDI, prob=0.9))[,]  
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tK_s10_3, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tK_s10_4, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tK_s10_5, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)


  lines(x = age_seq,
        y = tK_true,
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  lines(x = age_seq,
        y = rowMeans(tK_s10_1),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tK_s10_2),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tK_s10_3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tK_s10_4),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tK_s10_5),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(4,50),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.7-0.2,0.2+5), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.4, 0.4), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conK_s10_1, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conK_s10_2, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conK_s10_3, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conK_s10_4, 1, HPDI, prob=0.9)) 
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conK_s10_5, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = age_seq,
        y = rowMeans(conK_s10_1),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conK_s10_2),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conK_s10_3),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conK_s10_4),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conK_s10_5),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(age_seq),30),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



################ H


plot( x=0, y=60, type="n", ylim=c(0.33-0.15,1), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.3,1,0.1), labels=seq(0.3,1,0.1), las=1 )
  axis( side=1, at=seq(0,25,5), labels=seq(0,25,5), las=1 )
  box(col = "black") 
  par(xpd=FALSE)

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(age_seq > mina))

  ints <-  t(apply(tH_s10_1, 1, HPDI, prob=0.9))[,]  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tH_s10_2, 1, HPDI, prob=0.9))[,]  
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tH_s10_3, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tH_s10_4, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tH_s10_5, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)


  lines(x = age_seq,
        y = tH_true,
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  lines(x = age_seq,
        y = rowMeans(tH_s10_1),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tH_s10_2),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tH_s10_3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tH_s10_4),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tH_s10_5),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0.02), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH_s10_1, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conH_s10_2, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conH_s10_3, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conH_s10_4, 1, HPDI, prob=0.9)) 
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conH_s10_5, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = age_seq,
        y = rowMeans(conH_s10_1),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conH_s10_2),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conH_s10_3),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conH_s10_4),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conH_s10_5),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(age_seq),30),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region




# 30 full data  ###########################################################################################################################################################################

################ Q

 #set up plot
  #par(mar=c(5, 6, 2, 6), oma = c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0.07, 0.18), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.10,0.18,0.02), labels=NA, las=1 )
  axis( side=1, at=seq(0,25,5), labels=NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)


  ints <-  t(apply(tQ_s200_1, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tQ_s200_2, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tQ_s200_3, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tQ_s200_4, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tQ_s200_5, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)


  lines(x = age_seq,
        y = tQ_true,
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  lines(x = age_seq,
        y = rowMeans(tQ_s200_1),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tQ_s200_2),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tQ_s200_3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tQ_s200_4),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tQ_s200_5),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,0.2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.005,0.005+0.05), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.005, 0, 0.005), labels=c(-0.005, NA, 0.005), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conQ_s200_1, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conQ_s200_2, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conQ_s200_3, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conQ_s200_4, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conQ_s200_5, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = age_seq,
        y = rowMeans(conQ_s200_1),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conQ_s200_2),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conQ_s200_3),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conQ_s200_4),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conQ_s200_5),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(age_seq),27),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region




################### K

plot( x=0, y=60, type="n", ylim=c(4,24), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(8,24,4), labels=NA, las=1 )
  axis( side=1, at=seq(0,25,5), labels = NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(age_seq > mina))

  ints <-  t(apply(tK_s200_1, 1, HPDI, prob=0.9))[,]  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tK_s200_2, 1, HPDI, prob=0.9))[,]  
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tK_s200_3, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tK_s200_4, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tK_s200_5, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)


  lines(x = age_seq,
        y = tK_true,
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  lines(x = age_seq,
        y = rowMeans(tK_s200_1),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tK_s200_2),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tK_s200_3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tK_s200_4),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tK_s200_5),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(4,50),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.7-0.2,0.2+5), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.4, 0, 0.4), labels=c(-0.4, NA, 0.4), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conK_s200_1, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conK_s200_2, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conK_s200_3, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conK_s200_4, 1, HPDI, prob=0.9)) 
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conK_s200_5, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = age_seq,
        y = rowMeans(conK_s200_1),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conK_s200_2),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conK_s200_3),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conK_s200_4),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conK_s200_5),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(age_seq),27),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



################ H


plot( x=0, y=60, type="n", ylim=c(0.33-0.15,1), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.3,1,0.1), labels=NA, las=1 )
  axis( side=1, at=seq(0,25,5), labels=seq(0,25,5), las=1 )
  box(col = "black") 
  par(xpd=FALSE)

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(age_seq > mina))

  ints <-  t(apply(tH_s200_1, 1, HPDI, prob=0.9))[,]  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tH_s200_2, 1, HPDI, prob=0.9))[,]  
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tH_s200_3, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tH_s200_4, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)
  ints <-  t(apply(tH_s200_5, 1, HPDI, prob=0.9))[,] 
  polygon(c(age_seq[which(age_seq >= mina)], rev(age_seq[which(age_seq >= mina)])), c(ints[indexmina:length(age_seq),2], rev(ints[indexmina:length(age_seq),1])), 
        col = MatArea_col, border=NA)


  lines(x = age_seq,
        y = tH_true,
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  lines(x = age_seq,
        y = rowMeans(tH_s200_1),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tH_s200_2),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tH_s200_3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tH_s200_4),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)
  lines(x = age_seq,
        y = rowMeans(tH_s200_5),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0, 0.02), labels=c(-0.02, NA, 0.02), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH_s200_1, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conH_s200_2, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conH_s200_3, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conH_s200_4, 1, HPDI, prob=0.9)) 
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)
  ints <-  t(apply(conH_s200_5, 1, HPDI, prob=0.9))  
  polygon(c(age_seq, rev(age_seq)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = age_seq,
        y = rowMeans(conH_s200_1),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conH_s200_2),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conH_s200_3),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conH_s200_4),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)
  lines(x = age_seq,
        y = rowMeans(conH_s200_5),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(age_seq),27),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region


graphics.off()





