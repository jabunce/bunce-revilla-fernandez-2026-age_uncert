

##################################################################################


######## function to determine max velocity and age at max velocity during puberty
maxagevel <- function(Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                      K1=1, K2=1, K3=1, K4=1, K5=1,
                      H1=1, H2=1, H3=1, H4=1, H5=1,
                      I1=1, I2=1, I3=1, I4=1){


    secderiv <- function(x, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                            K1=1, K2=1, K3=1, K4=1, K5=1,
                            H1=1, H2=1, H3=1, H4=1, H5=1,
                            I1=1, I2=1, I3=1, I4=1){

      I0 <- rep(0, times=length(I1))

      ifelse( x < I1, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ),

      ifelse( x < I2, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ),

      ifelse( x < I3, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ),

      ifelse( x < I4, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ) +
                      2^(1/Q4)*( 1/( 1 + 2*Q4 )^2*H4^2*Q4*( 1/Q4 - 1 )*exp((2*K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 2 ) - 1/( 1 + 2*Q4 )^2*H4*K4*Q4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))))^( 1/Q4 - 1 ) ),

                      2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ) +
                      2^(1/Q4)*( 1/( 1 + 2*Q4 )^2*H4^2*Q4*( 1/Q4 - 1 )*exp((2*K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 2 ) - 1/( 1 + 2*Q4 )^2*H4*K4*Q4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))))^( 1/Q4 - 1 ) ) +
                      2^(1/Q5)*( 1/( 1 + 2*Q5 )^2*H5^2*Q5*( 1/Q5 - 1 )*exp((2*K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 )) ))^( 1/Q5 - 2 ) - 1/( 1 + 2*Q5 )^2*H5*K5*Q5*exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))))^( 1/Q5 - 1 ) ) 
      ) ) ) ) 
    } # secderiv


    # find childhood age at which height velocity starts to increase for puberty
    a <- seq(from=10, to=24, by=0.1) # choose "from" such that velocity is increasing at puberty
    lowerlim <- 10.5
    found <- FALSE
    for ( t in 1:length(a) ){
        if ( found == FALSE ) {
            if ( secderiv(x=a[t], Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                  K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                  H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                  I1=I1, I2=I2, I3=I3, I4=I4) > 0 ) {
                found <- TRUE
                lowerlim <- a[t]
            } # if
        } # if
    } # for t 

    tmax_sol <- uniroot( secderiv, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                   I1=I1, I2=I2, I3=I3, I4=I4, lower=lowerlim, upper=25 )   # root between t=10.5 and t=21, to ignore high fetal/infant growth rate. Choose lower bound where slope of velocity is positive, because upper bound has negative slope.


    firderiv <- function(x=1, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                              K1=1, K2=1, K3=1, K4=1, K5=1,
                              H1=1, H2=1, H3=1, H4=1, H5=1,
                              I1=1, I2=1, I3=1, I4=1){

      I0 <- rep(0, times=length(I1))

      ifelse( x < I1, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ),

      ifelse( x < I2, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ),

      ifelse( x < I3, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ),

      ifelse( x < I4, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ) +
                      1/( 1 + 2*Q4 )*2^(1/Q4)*H4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 1 ),

                      1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ) +
                      1/( 1 + 2*Q4 )*2^(1/Q4)*H4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 1 ) +
                      1/( 1 + 2*Q5 )*2^(1/Q5)*H5*exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 )) ))^( 1/Q5 - 1 )
      ) ) ) )  
    } # firderiv


#   curve(firderiv(x, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                     K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                     H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                     I1=I1, I2=I2, I3=I3, I4=I4), from=5, to=21)
#   curve(secderiv(x, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                     K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                     H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                     I1=I1, I2=I2, I3=I3, I4=I4), from=5, to=21, add = TRUE, col = "red")


    return( list(agemax=tmax_sol$root, maxvel=firderiv(x=tmax_sol$root,Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                                                       K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                                                       H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                                                       I1=I1, I2=I2, I3=I3, I4=I4)) )
} # maxagevel


vmaxagevel <- Vectorize(maxagevel) # vectorize the function so that it can take vectors as arguments


# Q1 <- SmuQ1
# Q2 <- SmuQ2
# Q3 <- SmuQ3
# Q4 <- SmuQ4
# Q5 <- SmuQ5

# K1 <- SmuK1
# K2 <- SmuK2
# K3 <- SmuK3
# K4 <- SmuK4
# K5 <- SmuK5

# H1 <- SmuH1
# H2 <- SmuH2
# H3 <- SmuH3
# H4 <- SmuH4
# H5 <- SmuH5

# I1 <- SmuI2
# I2 <- SmuI3
# I3 <- SmuI4
# I4 <- SmuI5

# post <- post6_y30_1
# x <- 1
# Q1 <- Q1mat[1:x]
# Q2 <- Q2mat[1:x]
# Q3 <- Q3mat[1:x]
# Q4 <- Q4mat[1:x]
# Q5 <- Q5mat[1:x]

# K1 <- K1mat[1:x]
# K2 <- K2mat[1:x]
# K3 <- K3mat[1:x]
# K4 <- K4mat[1:x]
# K5 <- K5mat[1:x]

# H1 <- H1mat[1:x]
# H2 <- H2mat[1:x]
# H3 <- H3mat[1:x]
# H4 <- H4mat[1:x]
# H5 <- H5mat[1:x]

# I1 <- I1mat[1:x]
# I2 <- I2mat[1:x]
# I3 <- I3mat[1:x]
# I4 <- I4mat[1:x]


# unlist(vmaxagevel(Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                   I1=I1, I2=I2, I3=I3, I4=I4)["agemax",])

# unlist(vmaxagevel(Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                   I1=I1, I2=I2, I3=I3, I4=I4)["maxvel",])



# unlist(vmaxagevel(Q1=SmuQ1, Q2=SmuQ2, Q3=SmuQ3, Q4=SmuQ4, Q5=SmuQ5,
#                   K1=SmuK1, K2=SmuK2, K3=SmuK3, K4=SmuK4, K5=SmuK5,
#                   H1=SmuH1, H2=SmuH2, H3=SmuH3, H4=SmuH4, H5=SmuH5,
#                   I1=SmuI1, I2=SmuI2, I3=SmuI3, I4=SmuI4)["agemax",])

# unlist(vmaxagevel(Q1=SmuQ1, Q2=SmuQ2, Q3=SmuQ3, Q4=SmuQ4, Q5=SmuQ5,
#                   K1=SmuK1, K2=SmuK2, K3=SmuK3, K4=SmuK4, K5=SmuK5,
#                   H1=SmuH1, H2=SmuH2, H3=SmuH3, H4=SmuH4, H5=SmuH5,
#                   I1=SmuI1, I2=SmuI2, I3=SmuI3, I4=SmuI4)["maxvel",])



######## function to determine x-axis range for plotting
xrange <- function( D1=c(1,2,3), D2=c(2,3,4) ){

    # determine which distribution is to the right of the other
    if (mean(D1) > mean(D2)) {DR <- D1} else {DR <- D2} 
    if (mean(D1) > mean(D2)) {DL <- D2} else {DL <- D1}
    
    # boundaries for plotting
    Lbound <- HPDI(DL,0.9)[1] - 0.5*( mean(DL) - HPDI(DL,0.9)[1] )
    Rbound <- HPDI(DR,0.9)[2] + 0.5*( HPDI(DL,0.9)[2] - mean(DL) )

    # axis labels
    Llab <- HPDI(DL,0.9)[1]
    Rlab <- HPDI(DR,0.9)[2]

    x <- formatC(x=(Rlab - Llab), format = "f", digits = 10)
    decimals <- attr(regexpr("(?<=\\.)0+|$", x, perl = TRUE), "match.length") #number of zeros after decimal point before first integer

    Llabr <- round(Llab, digits=decimals+1)
    Rlabr <- round(Rlab, digits=decimals+1)

    if ( Llabr > Lbound && Rlabr < Rbound ) {
        Llabo <- Llabr
        Rlabo <- Rlabr
    } else {
        Llabo <- formatC(x=Llab, format = "f", digits = 3)
        Rlabo <- formatC(x=Rlab, format = "f", digits = 3)
    } #else

    return( list(Lbound=Lbound, Rbound=Rbound, Llab=Llabo, Rlab=Rlabo) )
} # xrange


#D1 <- (2*H1mat/K1mat)^(1/Q1mat)
#D2 <- (2*H1ber/K1ber)^(1/Q1ber)
#unlist(xrange(D1=D1, D2=D2)$Lbound)



######## function to determine x-axis range for plotting contrasts
xrangecon <- function( D1=c(1,2,3) ){
    
    # boundaries for plotting
    if ( abs(HPDI(D1,0.9)[1]) > abs(HPDI(D1,0.9)[2]) ) {
        bound <- abs(HPDI(D1,0.9)[1]) + 0.2*abs(HPDI(D1,0.9)[1])
        lab <- abs(HPDI(D1,0.9)[1])
    } else { 
        bound <- abs(HPDI(D1,0.9)[2]) + 0.2*abs(HPDI(D1,0.9)[2])
        lab <- abs(HPDI(D1,0.9)[2])
    } #else

    x <- formatC(x=lab, format = "f", digits = 10)
    decimals <- attr(regexpr("(?<=\\.)0+|$", x, perl = TRUE), "match.length") #number of zeros after decimal point before first integer

    labr <- round(lab, digits=decimals+1)

    if ( labr < bound ) {
        Llab <- -1*labr
        Rlab <- labr
    } else {
        labo <- 
        Llab <- formatC(x=-1*lab, format = "f", digits = 3)
        Rlab <- formatC(x=lab, format = "f", digits = 3)
    } #else

    Lbound <- -1*bound
    Rbound <- bound

    return( list(Lbound=Lbound, Rbound=Rbound, Llab=Llab, Rlab=Rlab) )
} # xrangecon


#D1 <- contr_char_list$ber.matMaxHeight1
#xrangecon(D1=D1)




######## true simulated characteristics


mean_char_list_true <- list(
                       (2*SmuH1/SmuK1)^(1/SmuQ1) +
                       (2*SmuH2/SmuK2)^(1/SmuQ2) +
                       (2*SmuH3/SmuK3)^(1/SmuQ3) +
                       (2*SmuH4/SmuK4)^(1/SmuQ4) +
                       (2*SmuH5/SmuK5)^(1/SmuQ5),

                       unlist(vmaxagevel(Q1=SmuQ1, Q2=SmuQ2, Q3=SmuQ3, Q4=SmuQ4, Q5=SmuQ5,
                                         K1=SmuK1, K2=SmuK2, K3=SmuK3, K4=SmuK4, K5=SmuK5,
                                         H1=SmuH1, H2=SmuH2, H3=SmuH3, H4=SmuH4, H5=SmuH5,
                                         I1=SmuI2, I2=SmuI3, I3=SmuI4, I4=SmuI5)["agemax",]),

                       unlist(vmaxagevel(Q1=SmuQ1, Q2=SmuQ2, Q3=SmuQ3, Q4=SmuQ4, Q5=SmuQ5,
                                         K1=SmuK1, K2=SmuK2, K3=SmuK3, K4=SmuK4, K5=SmuK5,
                                         H1=SmuH1, H2=SmuH2, H3=SmuH3, H4=SmuH4, H5=SmuH5,
                                         I1=SmuI2, I2=SmuI3, I3=SmuI4, I4=SmuI5)["maxvel",])
                      )

names(mean_char_list_true) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)



######## prior simulated characteristics

mean_char_list_pri <- list(
                       (2*muH1/muK1)^(1/muQ1) +
                       (2*muH2/muK2)^(1/muQ2) +
                       (2*muH3/muK3)^(1/muQ3) +
                       (2*muH4/muK4)^(1/muQ4) +
                       (2*muH5/muK5)^(1/muQ5),

                       unlist(vmaxagevel(Q1=muQ1, Q2=muQ2, Q3=muQ3, Q4=muQ4, Q5=muQ5,
                                         K1=muK1, K2=muK2, K3=muK3, K4=muK4, K5=muK5,
                                         H1=muH1, H2=muH2, H3=muH3, H4=muH4, H5=muH5,
                                         I1=muI2, I2=muI3, I3=muI4, I4=muI5)["agemax",]),

                       unlist(vmaxagevel(Q1=muQ1, Q2=muQ2, Q3=muQ3, Q4=muQ4, Q5=muQ5,
                                         K1=muK1, K2=muK2, K3=muK3, K4=muK4, K5=muK5,
                                         H1=muH1, H2=muH2, H3=muH3, H4=muH4, H5=muH5,
                                         I1=muI2, I2=muI3, I3=muI4, I4=muI5)["maxvel",])
                      )

names(mean_char_list_pri) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )




######## 100 x-sectional data, estimated age

post6_s10_1 <- readRDS(modpost_name1) 
post6_s10_2 <- readRDS(modpost_name2) 
post6_s10_3 <- readRDS(modpost_name3) 
post6_s10_4 <- readRDS(modpost_name4) 
post6_s10_5 <- readRDS(modpost_name5)

post <- post6_s10_1

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s10_1 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s10_1) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s10_2

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s10_2 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s10_2) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s10_3

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s10_3 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s10_3) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s10_4

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s10_4 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s10_4) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s10_5

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s10_5 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s10_5) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

rm(post6_s10_1, post6_s10_2, post6_s10_3, post6_s10_4, post6_s10_5) #(list = ls(all=TRUE))





######## 100 x-sectional data, estimated age, no weight

post6_s100_1 <- readRDS(modpost_name6) 
post6_s100_2 <- readRDS(modpost_name7)
post6_s100_3 <- readRDS(modpost_name8) 
post6_s100_4 <- readRDS(modpost_name9) 
post6_s100_5 <- readRDS(modpost_name10) 


post <- post6_s100_1

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s100_1 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s100_1) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s100_2

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s100_2 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s100_2) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s100_3

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s100_3 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s100_3) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s100_4

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s100_4 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s100_4) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s100_5

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s100_5 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s100_5) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)
rm(post6_s100_1, post6_s100_2, post6_s100_3, post6_s100_4, post6_s100_5) #(list = ls(all=TRUE))




######## 50 sparse data, estimated age

post6_s200_1 <- readRDS(modpost_name11) 
post6_s200_2 <- readRDS(modpost_name12) 
post6_s200_3 <- readRDS(modpost_name13) 
post6_s200_4 <- readRDS(modpost_name14) 
post6_s200_5 <- readRDS(modpost_name15) 

post <- post6_s200_1

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s200_1 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s200_1) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s200_2

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s200_2 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s200_2) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s200_3

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s200_3 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s200_3) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s200_4

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s200_4 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s200_4) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)

##

post <- post6_s200_5

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]" 
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

mean_char_list_s200_5 <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )

names(mean_char_list_s200_5) <- c(
                          "matMaxHeight",
                          "matMaxVelAge",
                          "matMaxVel"
                        )

#str(mean_char_list_f)
rm(post6_s200_1, post6_s200_2, post6_s200_3, post6_s200_4, post6_s200_5) #(list = ls(all=TRUE))






######## plotting colors

colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


# area and line colors
PriLine_lwd <- 4.5
PriLine_col <- colorlist["2.2"] #"dark red" #grey(0.7)
PriLine_lty <- 1

BerLine_lwd <- 6 #4.5
BerLine_col <- "black" #colorlist["2.1"]

MatLine_lwd <- 4.5
MatLine_col <- colorlist["1.1"]

BerArea_col <- colorlist["2.3"]

MatArea_col <- colorlist["1.3"]

ConLine_lwd <- 4.5
ConLine_col <- "black"

ConArea_col <- grey(0.5)

ZerLine_lwd <- 4
ZerLine_lty <- "11"       #lty: first number in string is dash length, second is white space length
ZerLine_col <- "black"


cex_axis <- 1.75 # size of x-axis labels



pdf(file="./Plots/Chars_sims_100.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 4, 7, 10, 13, 16, 19, 0, 0, # column 1
                        2, 5, 8, 11, 14, 17, 20, 0, 0, # column 2
                        0, 0, 0, 0,  0,  0,  0,  0, 0,  # column 3 empty
                        3, 6, 9, 12, 15, 18, 21, 0, 0, # column 4
                        0, 0, 0, 0,  0,  0,  0,  0, 0, # column 5
                        0, 0, 0, 0,  0,  0,  0,  0, 0, # column 6 empty
                        0, 0, 0, 0,  0,  0,  0,  0, 0, # column 7
                        0, 0, 0, 0,  0,  0,  0,  0, 0, # column 8
                        0, 0, 0, 0,  0,  0,  0,  0, 0, # column 9 empty
                        0, 0, 0, 0,  0,  0,  0,  0, 0, # column 10
                        0, 0, 0, 0,  0,  0,  0,  0, 0  # column 11
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=c( rep(1,times=7), 0.0001, 0.0001 ),
        #widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
        widths=c(1, 1, 0.0002, 1, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



# 100 x-sectional data, estimated age ###########################################################################################################################################################################

mean_char_list <- list( unlist( mean_char_list_s10_1[1]),
                        unlist( mean_char_list_s10_2[1]),
                        unlist( mean_char_list_s10_3[1]),
                        unlist( mean_char_list_s10_4[1]),
                        unlist( mean_char_list_s10_5[1]),

                        unlist( mean_char_list_s10_1[2]),
                        unlist( mean_char_list_s10_2[2]),
                        unlist( mean_char_list_s10_3[2]),
                        unlist( mean_char_list_s10_4[2]),
                        unlist( mean_char_list_s10_5[2]),

                        unlist( mean_char_list_s10_1[3]),
                        unlist( mean_char_list_s10_2[3]),
                        unlist( mean_char_list_s10_3[3]),
                        unlist( mean_char_list_s10_4[3]),
                        unlist( mean_char_list_s10_5[3])
                       )

MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2,3,4,5)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[1])) ), mean( as.numeric(unlist(mean_char_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[2])) ), mean( as.numeric(unlist(mean_char_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[3])) ), mean( as.numeric(unlist(mean_char_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[4])) ), mean( as.numeric(unlist(mean_char_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[5])) ), mean( as.numeric(unlist(mean_char_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

lines(x=c( mean( mean_char_list_true$matMaxHeight ), mean( mean_char_list_true$matMaxHeight ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( mean_char_list_pri$matMaxHeight ), mean( mean_char_list_pri$matMaxHeight ) ),
      y=c(0,0.75), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty)



axis(side=1,
     at=c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




#set up new plot for legend placement
par(new=TRUE) # add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("Sim mean value", "Prior value", "Model estimates")
xcoords <- c(0, 6, 10.2) #4.2, 4) #2nd expands distance between second and third columns  
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 8 #6 # moves second and third columns right

  legend(x=4, y=16,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerLine_col, PriLine_col, MatArea_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=3,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = 3.5,
       ybottom = 11.5,
       xright = 26,
       ytop = 16,
       lwd=1)


# row labels
text("Max height (cm)", x=5, y=8.5, cex=3)
text("Age at max velocity (years)", x=16, y=8.5, cex=3)
text("Max velocity (cm/year)", x=27.5, y=8.5, cex=3)

text("100 idv, est age", x=-3.5, y=1, cex=3)
text("100 idv, est age, no wt", x=-3.5, y=-13, cex=3)
text("50 idv, est age, 2 meas", x=-3.5, y=-27, cex=3)


par(xpd=FALSE)



#graphics.off()


MaxVelAge_post_plot <- denschart3( mean_char_list[c(6,7,8,9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[6]),D2=unlist(mean_char_list[7]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[6]),D2=unlist(mean_char_list[7]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[6])) ), mean( as.numeric(unlist(mean_char_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[7])) ), mean( as.numeric(unlist(mean_char_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[8])) ), mean( as.numeric(unlist(mean_char_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[9])) ), mean( as.numeric(unlist(mean_char_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[10])) ), mean( as.numeric(unlist(mean_char_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

lines(x=c( mean( mean_char_list_true$matMaxVelAge ), mean( mean_char_list_true$matMaxVelAge ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( mean_char_list_pri$matMaxVelAge ), mean( mean_char_list_pri$matMaxVelAge ) ),
      y=c(0,0.75), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty)


axis(side=1,
     at=c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)


MaxVel_post_plot <- denschart3( mean_char_list[c(11,12,13,14,15)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[11]),D2=unlist(mean_char_list[12]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[11]),D2=unlist(mean_char_list[12]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[11])) ), mean( as.numeric(unlist(mean_char_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[12])) ), mean( as.numeric(unlist(mean_char_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[13])) ), mean( as.numeric(unlist(mean_char_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[14])) ), mean( as.numeric(unlist(mean_char_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[15])) ), mean( as.numeric(unlist(mean_char_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

lines(x=c( mean( mean_char_list_true$matMaxVel ), mean( mean_char_list_true$matMaxVel ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( mean_char_list_pri$matMaxVel ), mean( mean_char_list_pri$matMaxVel ) ),
      y=c(0,0.75), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty)


axis(side=1,
     at=c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)







# 100 x-sectional data, estimated age, no weight ###########################################################################################################################################################################

mean_char_list <- list( unlist( mean_char_list_s100_1[1]),
                        unlist( mean_char_list_s100_2[1]),
                        unlist( mean_char_list_s100_3[1]),
                        unlist( mean_char_list_s100_4[1]),
                        unlist( mean_char_list_s100_5[1]),

                        unlist( mean_char_list_s100_1[2]),
                        unlist( mean_char_list_s100_2[2]),
                        unlist( mean_char_list_s100_3[2]),
                        unlist( mean_char_list_s100_4[2]),
                        unlist( mean_char_list_s100_5[2]),

                        unlist( mean_char_list_s100_1[3]),
                        unlist( mean_char_list_s100_2[3]),
                        unlist( mean_char_list_s100_3[3]),
                        unlist( mean_char_list_s100_4[3]),
                        unlist( mean_char_list_s100_5[3])
                       )




MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2,3,4,5)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[1])) ), mean( as.numeric(unlist(mean_char_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[2])) ), mean( as.numeric(unlist(mean_char_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[3])) ), mean( as.numeric(unlist(mean_char_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[4])) ), mean( as.numeric(unlist(mean_char_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[5])) ), mean( as.numeric(unlist(mean_char_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

par(xpd=NA) # clip to device region
lines(x=c( mean( mean_char_list_true$matMaxHeight ), mean( mean_char_list_true$matMaxHeight ) ),
      y=c(0.2,1.525), col=BerLine_col, lwd=BerLine_lwd, lty=1, lend=1)
lines(x=c( mean( mean_char_list_pri$matMaxHeight ), mean( mean_char_list_pri$matMaxHeight ) ),
      y=c(0.2,1.525), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty, lend=1)
par(xpd=FALSE) # clip to plot region


axis(side=1,
     at=c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)




par(xpd=FALSE)


MaxVelAge_post_plot <- denschart3( mean_char_list[c(6,7,8,9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[6]),D2=unlist(mean_char_list[7]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[6]),D2=unlist(mean_char_list[7]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[6])) ), mean( as.numeric(unlist(mean_char_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[7])) ), mean( as.numeric(unlist(mean_char_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[8])) ), mean( as.numeric(unlist(mean_char_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[9])) ), mean( as.numeric(unlist(mean_char_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[10])) ), mean( as.numeric(unlist(mean_char_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

par(xpd=NA) # clip to device region
lines(x=c( mean( mean_char_list_true$matMaxVelAge ), mean( mean_char_list_true$matMaxVelAge ) ),
      y=c(0.2,1.525), col=BerLine_col, lwd=BerLine_lwd, lty=1, lend=1)
lines(x=c( mean( mean_char_list_pri$matMaxVelAge ), mean( mean_char_list_pri$matMaxVelAge ) ),
      y=c(0.2,1.525), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty, lend=1)
par(xpd=FALSE) # clip to plot region


axis(side=1,
     at=c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)


MaxVel_post_plot <- denschart3( mean_char_list[c(11,12,13,14,15)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[11]),D2=unlist(mean_char_list[12]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[11]),D2=unlist(mean_char_list[12]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[11])) ), mean( as.numeric(unlist(mean_char_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[12])) ), mean( as.numeric(unlist(mean_char_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[13])) ), mean( as.numeric(unlist(mean_char_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[14])) ), mean( as.numeric(unlist(mean_char_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[15])) ), mean( as.numeric(unlist(mean_char_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

par(xpd=NA) # clip to device region
lines(x=c( mean( mean_char_list_true$matMaxVel ), mean( mean_char_list_true$matMaxVel ) ),
      y=c(0.2,1.525), col=BerLine_col, lwd=BerLine_lwd, lty=1, lend=1)
lines(x=c( mean( mean_char_list_pri$matMaxVel ), mean( mean_char_list_pri$matMaxVel ) ),
      y=c(0.2,1.525), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty, lend=1)
par(xpd=FALSE) # clip to plot region


axis(side=1,
     at=c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)





# 50 sparse data, estimated age ###########################################################################################################################################################################

mean_char_list <- list( unlist( mean_char_list_s200_1[1]),
                        unlist( mean_char_list_s200_2[1]),
                        unlist( mean_char_list_s200_3[1]),
                        unlist( mean_char_list_s200_4[1]),
                        unlist( mean_char_list_s200_5[1]),

                        unlist( mean_char_list_s200_1[2]),
                        unlist( mean_char_list_s200_2[2]),
                        unlist( mean_char_list_s200_3[2]),
                        unlist( mean_char_list_s200_4[2]),
                        unlist( mean_char_list_s200_5[2]),

                        unlist( mean_char_list_s200_1[3]),
                        unlist( mean_char_list_s200_2[3]),
                        unlist( mean_char_list_s200_3[3]),
                        unlist( mean_char_list_s200_4[3]),
                        unlist( mean_char_list_s200_5[3])
                       )




MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2,3,4,5)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[1])) ), mean( as.numeric(unlist(mean_char_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[2])) ), mean( as.numeric(unlist(mean_char_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[3])) ), mean( as.numeric(unlist(mean_char_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[4])) ), mean( as.numeric(unlist(mean_char_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[5])) ), mean( as.numeric(unlist(mean_char_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

par(xpd=NA) # clip to device region
lines(x=c( mean( mean_char_list_true$matMaxHeight ), mean( mean_char_list_true$matMaxHeight ) ),
      y=c(0.2,1.525), col=BerLine_col, lwd=BerLine_lwd, lty=1, lend=1)
lines(x=c( mean( mean_char_list_pri$matMaxHeight ), mean( mean_char_list_pri$matMaxHeight ) ),
      y=c(0.2,1.525), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty, lend=1)
par(xpd=FALSE) # clip to plot region


axis(side=1,
     at=c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(150,168),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)




par(xpd=FALSE)


MaxVelAge_post_plot <- denschart3( mean_char_list[c(6,7,8,9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[6]),D2=unlist(mean_char_list[7]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[6]),D2=unlist(mean_char_list[7]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[6])) ), mean( as.numeric(unlist(mean_char_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[7])) ), mean( as.numeric(unlist(mean_char_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[8])) ), mean( as.numeric(unlist(mean_char_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[9])) ), mean( as.numeric(unlist(mean_char_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[10])) ), mean( as.numeric(unlist(mean_char_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

par(xpd=NA) # clip to device region
lines(x=c( mean( mean_char_list_true$matMaxVelAge ), mean( mean_char_list_true$matMaxVelAge ) ),
      y=c(0.2,1.525), col=BerLine_col, lwd=BerLine_lwd, lty=1, lend=1)
lines(x=c( mean( mean_char_list_pri$matMaxVelAge ), mean( mean_char_list_pri$matMaxVelAge ) ),
      y=c(0.2,1.525), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty, lend=1)
par(xpd=FALSE) # clip to plot region


axis(side=1,
     at=c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(11.7,12.2),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)


MaxVel_post_plot <- denschart3( mean_char_list[c(11,12,13,14,15)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA, NA, NA, NA, NA
                    ),
            colorHPDI=c( NA, NA, NA, NA, NA #MatArea_col
                        ),
            polyborder=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                        ),
            polyborderHPDI=c( MatLine_col, MatLine_col, MatLine_col, MatLine_col, MatLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[11]),D2=unlist(mean_char_list[12]) )$Lbound,
                  #   xrange( D1=unlist(mean_char_list[11]),D2=unlist(mean_char_list[12]) )$Rbound ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5)
 )


lines(x=c( mean( as.numeric(unlist(mean_char_list[11])) ), mean( as.numeric(unlist(mean_char_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[12])) ), mean( as.numeric(unlist(mean_char_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[13])) ), mean( as.numeric(unlist(mean_char_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[14])) ), mean( as.numeric(unlist(mean_char_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_char_list[15])) ), mean( as.numeric(unlist(mean_char_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      

par(xpd=NA) # clip to device region
lines(x=c( mean( mean_char_list_true$matMaxVel ), mean( mean_char_list_true$matMaxVel ) ),
      y=c(0.2,1.525), col=BerLine_col, lwd=BerLine_lwd, lty=1, lend=1)
lines(x=c( mean( mean_char_list_pri$matMaxVel ), mean( mean_char_list_pri$matMaxVel ) ),
      y=c(0.2,1.525), col=PriLine_col, lwd=PriLine_lwd, lty=PriLine_lty, lend=1)
par(xpd=FALSE) # clip to plot region


axis(side=1,
     at=c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c(7.25,8.5),#c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                #xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)




graphics.off()











