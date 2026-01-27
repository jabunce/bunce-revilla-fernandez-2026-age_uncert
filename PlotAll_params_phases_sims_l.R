

######################################### Fig #########################################



########  need to run Functions for densplot3 function





######## function to determine x-axis range for plotting
xrange <- function( D1=c(1,2,3), D2=c(2,3,4) ){

    # determine which distribution is to the right of the other
    if (mean(D1) > mean(D2)) {
        DR <- D1
        DL <- D2
    } else {
        DR <- D2
        DL <- D1
    } #else
    
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
        Llabo <- formatC(x=Llab, format = "f", digits = decimals+3)
        Rlabo <- formatC(x=Rlab, format = "f", digits = decimals+3)
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
        Llab <- formatC(x=-1*lab, format = "f", digits = decimals+3)
        Rlab <- formatC(x=lab, format = "f", digits = decimals+3)
    } #else

    Lbound <- -1*bound
    Rbound <- bound

    return( list(Lbound=Lbound, Rbound=Rbound, Llab=Llab, Rlab=Rlab) )
} # xrangecon


#D1 <- contr_param_list$ber.matMaxHeight1
#xrangecon(D1=D1)






########## create lists of parameter posteriors


##### true simulated values

mean_param_list_true <- list( SmuQ1, SmuQ2, SmuQ3, SmuQ4, SmuQ5,
                              SmuK1, SmuK2, SmuK3, SmuK4, SmuK5,
                              SmuH1*(2/32), SmuH2*(2/32), SmuH3*(2/32), SmuH4*(2/32), SmuH5*(2/32),  # original H in units of g/cm^2 of skin surface of function=~5% of body mass (**DO NOT need to multiply by (cm^2 skin surface area)/(g body mass)*1/0.05 **). Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                              SmuI1, SmuI2, SmuI3, SmuI4, SmuI5 
                            )

names(mean_param_list_true) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                  "K1", "K2", "K3", "K4", "K5",
                                  "H1", "H2", "H3", "H4", "H5",
                                  "I1", "I2", "I3", "I4", "I5"
                                )

#str(mean_param_list_true)




######## 10 individuals, measured 10 times, 1 year intervals

post6_y30_1 <- readRDS(modpost_name1)
post6_y30_2 <- readRDS(modpost_name2)
post6_y30_3 <- readRDS(modpost_name3) 
post6_y30_4 <- readRDS(modpost_name4)
post6_y30_5 <- readRDS(modpost_name5) 


post <- post6_y30_1

mean_param_list_y30_1 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                               post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                               (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                               0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                              )
names(mean_param_list_y30_1) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                   "K1", "K2", "K3", "K4", "K5",
                                   "H1", "H2", "H3", "H4", "H5",
                                   "I1", "I2", "I3", "I4", "I5"
                                  )


post <- post6_y30_2

mean_param_list_y30_2 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                               post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                               (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                               0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                              )
names(mean_param_list_y30_2) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                   "K1", "K2", "K3", "K4", "K5",
                                   "H1", "H2", "H3", "H4", "H5",
                                   "I1", "I2", "I3", "I4", "I5"
                                  )

##

post <- post6_y30_3

mean_param_list_y30_3 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                               post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                               (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                               0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                              )
names(mean_param_list_y30_3) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                   "K1", "K2", "K3", "K4", "K5",
                                   "H1", "H2", "H3", "H4", "H5",
                                   "I1", "I2", "I3", "I4", "I5"
                                  )

##

post <- post6_y30_4

mean_param_list_y30_4 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                               post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                               (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                               0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                              )
names(mean_param_list_y30_4) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                   "K1", "K2", "K3", "K4", "K5",
                                   "H1", "H2", "H3", "H4", "H5",
                                   "I1", "I2", "I3", "I4", "I5"
                                  )

##

post <- post6_y30_5

mean_param_list_y30_5 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                               post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                               (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                               0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                              )
names(mean_param_list_y30_5) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                   "K1", "K2", "K3", "K4", "K5",
                                   "H1", "H2", "H3", "H4", "H5",
                                   "I1", "I2", "I3", "I4", "I5"
                                  )


rm(post6_y30_1, post6_y30_2, post6_y30_3, post6_y30_4, post6_y30_5) #(list = ls(all=TRUE))






######## 20 individuals, measured 5 times, 2 year intervals

post6_d5_1 <- readRDS(modpost_name6) 
post6_d5_2 <- readRDS(modpost_name7)
post6_d5_3 <- readRDS(modpost_name8) 
post6_d5_4 <- readRDS(modpost_name9) 
post6_d5_5 <- readRDS(modpost_name10)  


post <- post6_d5_1

mean_param_list_d5_1 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                              post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                              (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                              0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                             )

names(mean_param_list_d5_1) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                  "K1", "K2", "K3", "K4", "K5",
                                  "H1", "H2", "H3", "H4", "H5",
                                  "I1", "I2", "I3", "I4", "I5"
                                 )

##

post <- post6_d5_2

mean_param_list_d5_2 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                              post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                              (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                              0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                             )

names(mean_param_list_d5_2) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                  "K1", "K2", "K3", "K4", "K5",
                                  "H1", "H2", "H3", "H4", "H5",
                                  "I1", "I2", "I3", "I4", "I5"
                                 )

##

post <- post6_d5_3

mean_param_list_d5_3 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                              post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                              (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                              0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                             )

names(mean_param_list_d5_3) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                  "K1", "K2", "K3", "K4", "K5",
                                  "H1", "H2", "H3", "H4", "H5",
                                  "I1", "I2", "I3", "I4", "I5"
                                 )

##

post <- post6_d5_4

mean_param_list_d5_4 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                              post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                              (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                              0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                             )

names(mean_param_list_d5_4) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                  "K1", "K2", "K3", "K4", "K5",
                                  "H1", "H2", "H3", "H4", "H5",
                                  "I1", "I2", "I3", "I4", "I5"
                                 )

##

post <- post6_d5_5

mean_param_list_d5_5 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                              post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                              (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                              0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                             )

names(mean_param_list_d5_5) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                  "K1", "K2", "K3", "K4", "K5",
                                  "H1", "H2", "H3", "H4", "H5",
                                  "I1", "I2", "I3", "I4", "I5"
                                 )



rm(post6_d5_1, post6_d5_2, post6_d5_3, post6_d5_4, post6_d5_5) #(list = ls(all=TRUE))





######## 30 full data

post6_s100_1 <- readRDS(modpost_name11) 
post6_s100_2 <- readRDS(modpost_name12) 
post6_s100_3 <- readRDS(modpost_name13) 
post6_s100_4 <- readRDS(modpost_name14) 
post6_s100_5 <- readRDS(modpost_name15) 


post <- post6_s100_1

mean_param_list_s100_1 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s100_1) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s100_2

mean_param_list_s100_2 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s100_2) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s100_3

mean_param_list_s100_3 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s100_3) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

##

post <- post6_s100_4

mean_param_list_s100_4 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s100_4) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )


##

post <- post6_s100_5

mean_param_list_s100_5 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                                post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                                (2/32)*post$"mH[2,1]", (2/32)*post$"mH[2,2]", (2/32)*post$"mH[2,3]", (2/32)*post$"mH[2,4]", (2/32)*post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s100_5) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )


rm(post6_s100_1, post6_s100_2, post6_s100_3, post6_s100_4, post6_s100_5) #(list = ls(all=TRUE))






######## define plotting colors


colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


# area and line colors
BerLine_lwd <- 6 #4.5
BerLine_col <- colorlist["2.2"]

MatLine_lwd <- 4.5
MatLine_col <- colorlist["1.2"]

BerArea_col <- colorlist["2.2"]

MatArea_col <- colorlist["1.2"]

ConLine_lwd <- 4.5
ConLine_col <- "black"

ConArea_col <- grey(0.5)

ZerLine_lwd <- 4
ZerLine_lty <- "11"       #lty: first number in string is dash length, second is white space length
ZerLine_col <- "black"







pdf(file="./Plots/Params_phases_combined_sims_l.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.0002, 1,1, 0.0001, 0.0001,0.0001, 0.0001, 0.0001,0.0001)
        #widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels



# 10 individuals, measured 10 times, 1 year intervals ###########################################################################################################################################################################


mean_param_list <- list( unlist( mean_param_list_y30_1[1]), # Q1
                         unlist( mean_param_list_y30_2[1]),
                         unlist( mean_param_list_y30_3[1]),
                         unlist( mean_param_list_y30_4[1]),
                         unlist( mean_param_list_y30_5[1]),

                         unlist( mean_param_list_y30_1[6]), # K1
                         unlist( mean_param_list_y30_2[6]),
                         unlist( mean_param_list_y30_3[6]),
                         unlist( mean_param_list_y30_4[6]),
                         unlist( mean_param_list_y30_5[6]),

                         unlist( mean_param_list_y30_1[11]), # H1
                         unlist( mean_param_list_y30_2[11]),
                         unlist( mean_param_list_y30_3[11]),
                         unlist( mean_param_list_y30_4[11]),
                         unlist( mean_param_list_y30_5[11])
                        )


Q1_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.083,0.087), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q1 ), mean( mean_param_list_true$Q1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.083, 0.087),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.083, 0.087), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)







#set up new plot for legend placement
par(new=TRUE) # add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=F, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("Sim value", "Estimate runs", "")
xcoords <- c(0, 8, 23)#c(0, 7, 23)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 6 #1 #0 # so replace element 1 with a finite number (any will do)

  legend(x=6, y=21,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerLine_col, MatLine_col, "white"),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=3,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = 5.3,
       ybottom = 15,
       xright = 26,
       ytop = 20,
       lwd=1)


# row labels
text(expression(bold("In utero")), x=-5, y=11, cex=3.75)
text(expression(bold("Infancy")), x=-5, y=-41, cex=3.75)
text(expression(bold("Early childhood")), x=-5, y=-89, cex=3.75)

text(expression(paste(bolditalic("q")[1])), x=-5, y=0, cex=3.75)
text(expression(paste(bolditalic("K")[1])), x=-5, y=-15, cex=3.75)
text("(g/g)", x=-5, y=-19, cex=3)
text(expression(paste(bolditalic("H")[1])), x=-5, y=-30, cex=3.75)
text(expression(paste("(g/cm"^2,")", sep="")), x=-5, y=-34, cex=3)


text(expression(paste(bolditalic("q")[2])), x=-5, y=-47.5, cex=3.75)
text(expression(paste(bolditalic("K")[2])), x=-5, y=-62.5, cex=3.75)
text("(g/g)", x=-5, y=-66.5, cex=3)
text(expression(paste(bolditalic("H")[2])), x=-5, y=-77.5, cex=3.75)
text(expression(paste("(g/cm"^2,")", sep="")), x=-5, y=-81.5, cex=3)

text(expression(paste(bolditalic("q")[3])), x=-5, y=-95.5, cex=3.75)
text(expression(paste(bolditalic("K")[3])), x=-5, y=-110.5, cex=3.75)
text("(g/g)", x=-5, y=-114.5, cex=3)
text(expression(paste(bolditalic("H")[3])), x=-5, y=-125.5, cex=3.75)
text(expression(paste("(g/cm"^2,")", sep="")), x=-5, y=-129.5, cex=3)


text("10 idv, 10 ms, 1 int", x=5, y=11, cex=3.65)
text("20 idv, 5 ms, 2 int", x=17, y=11, cex=3.65)
text("30 idv, 25 ms, 1 int", x=28, y=11, cex=3.65)



# horizontal lines
lines(x=c(-9.5, 34),
      y=c(-38,-38),
      col="black", lwd=4, lty=1)

lines(x=c(-9.5,34),
      y=c(-85.5,-85.5),
      col="black", lwd=4, lty=1)



par(xpd=FALSE)



#graphics.off()





K1_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(80, 84), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K1 ), mean( mean_param_list_true$K1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(80, 84),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(80, 84), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H1_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(3.5, 3.7), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H1 ), mean( mean_param_list_true$H1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(3.5, 3.7),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(3.5, 3.7), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_y30_1[2]), # Q2
                         unlist( mean_param_list_y30_2[2]),
                         unlist( mean_param_list_y30_3[2]),
                         unlist( mean_param_list_y30_4[2]),
                         unlist( mean_param_list_y30_5[2]),

                         unlist( mean_param_list_y30_1[7]), # K2
                         unlist( mean_param_list_y30_2[7]),
                         unlist( mean_param_list_y30_3[7]),
                         unlist( mean_param_list_y30_4[7]),
                         unlist( mean_param_list_y30_5[7]),

                         unlist( mean_param_list_y30_1[12]), # H2
                         unlist( mean_param_list_y30_2[12]),
                         unlist( mean_param_list_y30_3[12]),
                         unlist( mean_param_list_y30_4[12]),
                         unlist( mean_param_list_y30_5[12])
                        )


Q2_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.15,0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q2 ), mean( mean_param_list_true$Q2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.15, 0.17),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.15, 0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K2_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(12.1, 12.8), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K2 ), mean( mean_param_list_true$K2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(12.1, 12.8),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(12.1, 12.8), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H2_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.65, 0.67), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H2 ), mean( mean_param_list_true$H2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.65, 0.67),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.65, 0.67), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_y30_1[3]), # Q3
                         unlist( mean_param_list_y30_2[3]),
                         unlist( mean_param_list_y30_3[3]),
                         unlist( mean_param_list_y30_4[3]),
                         unlist( mean_param_list_y30_5[3]),

                         unlist( mean_param_list_y30_1[8]), # K3
                         unlist( mean_param_list_y30_2[8]),
                         unlist( mean_param_list_y30_3[8]),
                         unlist( mean_param_list_y30_4[8]),
                         unlist( mean_param_list_y30_5[8]),

                         unlist( mean_param_list_y30_1[13]), # H3
                         unlist( mean_param_list_y30_2[13]),
                         unlist( mean_param_list_y30_3[13]),
                         unlist( mean_param_list_y30_4[13]),
                         unlist( mean_param_list_y30_5[13])
                        )


Q3_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.3,0.32), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q3 ), mean( mean_param_list_true$Q3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.3, 0.32),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.15, 0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K3_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(1.53, 1.62), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K3 ), mean( mean_param_list_true$K3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(1.53, 1.62),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(1.53, 1.62), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H3_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.157, 0.165), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H3 ), mean( mean_param_list_true$H3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.157, 0.165),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.157, 0.165), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)


#graphics.off()








# 20 individuals, measured 5 times, 2 year intervals ###########################################################################################################################################################################

mean_param_list <- list( unlist( mean_param_list_d5_1[1]), # Q1
                         unlist( mean_param_list_d5_2[1]),
                         unlist( mean_param_list_d5_3[1]),
                         unlist( mean_param_list_d5_4[1]),
                         unlist( mean_param_list_d5_5[1]),

                         unlist( mean_param_list_d5_1[6]), # K1
                         unlist( mean_param_list_d5_2[6]),
                         unlist( mean_param_list_d5_3[6]),
                         unlist( mean_param_list_d5_4[6]),
                         unlist( mean_param_list_d5_5[6]),

                         unlist( mean_param_list_d5_1[11]), # H1
                         unlist( mean_param_list_d5_2[11]),
                         unlist( mean_param_list_d5_3[11]),
                         unlist( mean_param_list_d5_4[11]),
                         unlist( mean_param_list_d5_5[11])
                        )


Q1_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.083,0.087), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q1 ), mean( mean_param_list_true$Q1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.083, 0.087),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.083, 0.087), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



K1_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(80, 84), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K1 ), mean( mean_param_list_true$K1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(80, 84),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(80, 84), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H1_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(3.5, 3.7), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H1 ), mean( mean_param_list_true$H1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(3.5, 3.7),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(3.5, 3.7), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_d5_1[2]), # Q2
                         unlist( mean_param_list_d5_2[2]),
                         unlist( mean_param_list_d5_3[2]),
                         unlist( mean_param_list_d5_4[2]),
                         unlist( mean_param_list_d5_5[2]),

                         unlist( mean_param_list_d5_1[7]), # K2
                         unlist( mean_param_list_d5_2[7]),
                         unlist( mean_param_list_d5_3[7]),
                         unlist( mean_param_list_d5_4[7]),
                         unlist( mean_param_list_d5_5[7]),

                         unlist( mean_param_list_d5_1[12]), # H2
                         unlist( mean_param_list_d5_2[12]),
                         unlist( mean_param_list_d5_3[12]),
                         unlist( mean_param_list_d5_4[12]),
                         unlist( mean_param_list_d5_5[12])
                        )


Q2_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.15,0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q2 ), mean( mean_param_list_true$Q2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.15, 0.17),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.15, 0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K2_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(12.1, 12.8), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K2 ), mean( mean_param_list_true$K2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(12.1, 12.8),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(12.1, 12.8), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H2_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.65, 0.67), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H2 ), mean( mean_param_list_true$H2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.65, 0.67),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.65, 0.67), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_d5_1[3]), # Q3
                         unlist( mean_param_list_d5_2[3]),
                         unlist( mean_param_list_d5_3[3]),
                         unlist( mean_param_list_d5_4[3]),
                         unlist( mean_param_list_d5_5[3]),

                         unlist( mean_param_list_d5_1[8]), # K3
                         unlist( mean_param_list_d5_2[8]),
                         unlist( mean_param_list_d5_3[8]),
                         unlist( mean_param_list_d5_4[8]),
                         unlist( mean_param_list_d5_5[8]),

                         unlist( mean_param_list_d5_1[13]), # H3
                         unlist( mean_param_list_d5_2[13]),
                         unlist( mean_param_list_d5_3[13]),
                         unlist( mean_param_list_d5_4[13]),
                         unlist( mean_param_list_d5_5[13])
                        )


Q3_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.3,0.32), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q3 ), mean( mean_param_list_true$Q3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.3, 0.32),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.15, 0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K3_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(1.53, 1.62), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K3 ), mean( mean_param_list_true$K3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(1.53, 1.62),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(1.53, 1.62), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H3_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.157, 0.165), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H3 ), mean( mean_param_list_true$H3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.157, 0.165),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.157, 0.165), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)





# 30 full data ###########################################################################################################################################################################

mean_param_list <- list( unlist( mean_param_list_s100_1[1]), # Q1
                         unlist( mean_param_list_s100_2[1]),
                         unlist( mean_param_list_s100_3[1]),
                         unlist( mean_param_list_s100_4[1]),
                         unlist( mean_param_list_s100_5[1]),

                         unlist( mean_param_list_s100_1[6]), # K1
                         unlist( mean_param_list_s100_2[6]),
                         unlist( mean_param_list_s100_3[6]),
                         unlist( mean_param_list_s100_4[6]),
                         unlist( mean_param_list_s100_5[6]),

                         unlist( mean_param_list_s100_1[11]), # H1
                         unlist( mean_param_list_s100_2[11]),
                         unlist( mean_param_list_s100_3[11]),
                         unlist( mean_param_list_s100_4[11]),
                         unlist( mean_param_list_s100_5[11])
                        )


Q1_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.083,0.087), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q1 ), mean( mean_param_list_true$Q1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.083, 0.087),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.083, 0.087), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



K1_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(80, 84), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K1 ), mean( mean_param_list_true$K1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(80, 84),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(80, 84), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H1_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(3.5, 3.7), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H1 ), mean( mean_param_list_true$H1 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(3.5, 3.7),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(3.5, 3.7), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_s100_1[2]), # Q2
                         unlist( mean_param_list_s100_2[2]),
                         unlist( mean_param_list_s100_3[2]),
                         unlist( mean_param_list_s100_4[2]),
                         unlist( mean_param_list_s100_5[2]),

                         unlist( mean_param_list_s100_1[7]), # K2
                         unlist( mean_param_list_s100_2[7]),
                         unlist( mean_param_list_s100_3[7]),
                         unlist( mean_param_list_s100_4[7]),
                         unlist( mean_param_list_s100_5[7]),

                         unlist( mean_param_list_s100_1[12]), # H2
                         unlist( mean_param_list_s100_2[12]),
                         unlist( mean_param_list_s100_3[12]),
                         unlist( mean_param_list_s100_4[12]),
                         unlist( mean_param_list_s100_5[12])
                        )


Q2_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.15,0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q2 ), mean( mean_param_list_true$Q2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.15, 0.17),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.15, 0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K2_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(12.1, 12.8), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K2 ), mean( mean_param_list_true$K2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(12.1, 12.8),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(12.1, 12.8), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H2_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.65, 0.67), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H2 ), mean( mean_param_list_true$H2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.65, 0.67),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.65, 0.67), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_s100_1[3]), # Q3
                         unlist( mean_param_list_s100_2[3]),
                         unlist( mean_param_list_s100_3[3]),
                         unlist( mean_param_list_s100_4[3]),
                         unlist( mean_param_list_s100_5[3]),

                         unlist( mean_param_list_s100_1[8]), # K3
                         unlist( mean_param_list_s100_2[8]),
                         unlist( mean_param_list_s100_3[8]),
                         unlist( mean_param_list_s100_4[8]),
                         unlist( mean_param_list_s100_5[8]),

                         unlist( mean_param_list_s100_1[13]), # H3
                         unlist( mean_param_list_s100_2[13]),
                         unlist( mean_param_list_s100_3[13]),
                         unlist( mean_param_list_s100_4[13]),
                         unlist( mean_param_list_s100_5[13])
                        )


Q3_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.3,0.32), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q3 ), mean( mean_param_list_true$Q3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.3, 0.32),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.15, 0.17), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K3_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(1.53, 1.62), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K3 ), mean( mean_param_list_true$K3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(1.53, 1.62),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(1.53, 1.62), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H3_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.157, 0.165), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H3 ), mean( mean_param_list_true$H3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.157, 0.165),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.157, 0.165), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)



graphics.off()














###########################################################################################################################################
###########################################################################################################################################


pdf(file="./Plots/Params_phases_combined_sims2_l.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6, 0, 0, 0, 7, 8, 9,10,11,12,0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       13,14,15,16,17,18, 0, 0, 0,19,20,21,22,23,24,0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.0002, 1,1, 0.0001, 0.0001,0.0001, 0.0001, 0.0001,0.0001)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels



# 10 individuals, measured 10 times, 1 year intervals ###########################################################################################################################################################################


mean_param_list <- list( unlist( mean_param_list_y30_1[4]), # Q4
                         unlist( mean_param_list_y30_2[4]),
                         unlist( mean_param_list_y30_3[4]),
                         unlist( mean_param_list_y30_4[4]),
                         unlist( mean_param_list_y30_5[4]),

                         unlist( mean_param_list_y30_1[9]), # K4
                         unlist( mean_param_list_y30_2[9]),
                         unlist( mean_param_list_y30_3[9]),
                         unlist( mean_param_list_y30_4[9]),
                         unlist( mean_param_list_y30_5[9]),

                         unlist( mean_param_list_y30_1[14]), # H4
                         unlist( mean_param_list_y30_2[14]),
                         unlist( mean_param_list_y30_3[14]),
                         unlist( mean_param_list_y30_4[14]),
                         unlist( mean_param_list_y30_5[14])
                        )


Q4_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.046,0.05), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q4 ), mean( mean_param_list_true$Q4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.046,0.05),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.046,0.05), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




#set up new plot for legend placement
par(new=TRUE) # add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=F, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# # legend
# legtext <- c("Real value", "Estimate runs", "")
# xcoords <- c(0, 8, 23)#c(0, 7, 23)
# secondvector <- (1:length(legtext))-1
# textwidths <- xcoords/secondvector # this works for all but the first element
# textwidths[1] <- 6 #1 #0 # so replace element 1 with a finite number (any will do)

#   legend(x=12, y=21,
#          #inset=c(0,-0.5),       # inset is distance from x and y margins
#          text.width=textwidths,  
#          legend=legtext,
#          bty="n",
#          bg="white",
#          col=c("red", MatArea_col, "white"),
#          lty=c(1,1,1), 
#          lwd=c(9,9,9),
#          cex=3,
#          x.intersp=0.5,
#          seg.len=1.5,
#          horiz=TRUE)

#   rect(xleft = 11,
#        ybottom = 15,
#        xright = 30.5,
#        ytop = 20,
#        lwd=1)


# row labels
text(expression(bold("Late childhood")), x=-5, y=11, cex=3.75)
text(expression(bold("Adolescence")), x=-5, y=-41, cex=3.75)
#text(expression(bold("Early childhood")), x=-5, y=-89, cex=3.75)

text(expression(paste(bolditalic("q")[4])), x=-5, y=0, cex=3.75)
text(expression(paste(bolditalic("K")[4])), x=-5, y=-15, cex=3.75)
text("(g/g)", x=-5, y=-19, cex=3)
text(expression(paste(bolditalic("H")[4])), x=-5, y=-30, cex=3.75)
text(expression(paste("(g/cm"^2,")", sep="")), x=-5, y=-34, cex=3)


text(expression(paste(bolditalic("q")[5])), x=-5, y=-47.5, cex=3.75)
text(expression(paste(bolditalic("K")[5])), x=-5, y=-62.5, cex=3.75)
text("(g/g)", x=-5, y=-66.5, cex=3)
text(expression(paste(bolditalic("H")[5])), x=-5, y=-77.5, cex=3.75)
text(expression(paste("(g/cm"^2,")", sep="")), x=-5, y=-81.5, cex=3)

# text(expression(paste(bolditalic("q")[3])), x=-5, y=-95.5, cex=3.75)
# text(expression(paste(bolditalic("K")[3])), x=-5, y=-110.5, cex=3.75)
# text("(g/g)", x=-5, y=-114.5, cex=3)
# text(expression(paste(bolditalic("H")[3])), x=-5, y=-125.5, cex=3.75)
# text("(g/cm)", x=-5, y=-129.5, cex=3)


text("10 idv, 10 ms, 1 int", x=5, y=11, cex=3.65)
text("20 idv, 5 ms, 2 int", x=17, y=11, cex=3.65)
text("30 idv, 25 ms, 1 int", x=28, y=11, cex=3.65)



# horizontal lines
lines(x=c(-9.5, 34),
      y=c(-38,-38),
      col="black", lwd=4, lty=1)

# lines(x=c(-9.5,46),
#       y=c(-85.5,-85.5),
#       col="black", lwd=4, lty=1)



par(xpd=FALSE)




K4_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(10, 11), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K4 ), mean( mean_param_list_true$K4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(10, 11),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(10, 11), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H4_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.35, 0.38), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H4 ), mean( mean_param_list_true$H4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.35, 0.38),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.35, 0.38), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_y30_1[5]), # Q5
                         unlist( mean_param_list_y30_2[5]),
                         unlist( mean_param_list_y30_3[5]),
                         unlist( mean_param_list_y30_4[5]),
                         unlist( mean_param_list_y30_5[5]),

                         unlist( mean_param_list_y30_1[10]), # K5
                         unlist( mean_param_list_y30_2[10]),
                         unlist( mean_param_list_y30_3[10]),
                         unlist( mean_param_list_y30_4[10]),
                         unlist( mean_param_list_y30_5[10]),

                         unlist( mean_param_list_y30_1[15]), # H5
                         unlist( mean_param_list_y30_2[15]),
                         unlist( mean_param_list_y30_3[15]),
                         unlist( mean_param_list_y30_4[15]),
                         unlist( mean_param_list_y30_5[15])
                        )


Q5_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.118,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q5 ), mean( mean_param_list_true$Q5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.118,0.126),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.118,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K5_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(8.4, 9.1), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K5 ), mean( mean_param_list_true$K5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(8.4, 9.1),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(8.4, 9.1), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H5_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.36, 0.4), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H5 ), mean( mean_param_list_true$H5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.36, 0.4),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.36, 0.4), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)





# 20 individuals, measured 5 times, 2 year intervals ###########################################################################################################################################################################

mean_param_list <- list( unlist( mean_param_list_d5_1[4]), # Q4
                         unlist( mean_param_list_d5_2[4]),
                         unlist( mean_param_list_d5_3[4]),
                         unlist( mean_param_list_d5_4[4]),
                         unlist( mean_param_list_d5_5[4]),

                         unlist( mean_param_list_d5_1[9]), # K4
                         unlist( mean_param_list_d5_2[9]),
                         unlist( mean_param_list_d5_3[9]),
                         unlist( mean_param_list_d5_4[9]),
                         unlist( mean_param_list_d5_5[9]),

                         unlist( mean_param_list_d5_1[14]), # H4
                         unlist( mean_param_list_d5_2[14]),
                         unlist( mean_param_list_d5_3[14]),
                         unlist( mean_param_list_d5_4[14]),
                         unlist( mean_param_list_d5_5[14])
                        )


Q4_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.046,0.05), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q4 ), mean( mean_param_list_true$Q4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.046,0.05),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.046,0.05), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



K4_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(10, 11), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K4 ), mean( mean_param_list_true$K4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(10, 11),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(10, 11), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H4_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.35, 0.38), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H4 ), mean( mean_param_list_true$H4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.35, 0.38),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.35, 0.38), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_d5_1[5]), # Q5
                         unlist( mean_param_list_d5_2[5]),
                         unlist( mean_param_list_d5_3[5]),
                         unlist( mean_param_list_d5_4[5]),
                         unlist( mean_param_list_d5_5[5]),

                         unlist( mean_param_list_d5_1[10]), # K2
                         unlist( mean_param_list_d5_2[10]),
                         unlist( mean_param_list_d5_3[10]),
                         unlist( mean_param_list_d5_4[10]),
                         unlist( mean_param_list_d5_5[10]),

                         unlist( mean_param_list_d5_1[15]), # H2
                         unlist( mean_param_list_d5_2[15]),
                         unlist( mean_param_list_d5_3[15]),
                         unlist( mean_param_list_d5_4[15]),
                         unlist( mean_param_list_d5_5[15])
                        )


Q5_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.118,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q5 ), mean( mean_param_list_true$Q5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.118,0.126),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.118,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K5_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(8.4, 9.1), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K5 ), mean( mean_param_list_true$K5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(8.4, 9.1),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(8.4, 9.1), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H5_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.36, 0.4), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H5 ), mean( mean_param_list_true$H5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.36, 0.4),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.36, 0.4), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)







# 30 full data ###########################################################################################################################################################################

mean_param_list <- list( unlist( mean_param_list_s100_1[4]), # Q4
                         unlist( mean_param_list_s100_2[4]),
                         unlist( mean_param_list_s100_3[4]),
                         unlist( mean_param_list_s100_4[4]),
                         unlist( mean_param_list_s100_5[4]),

                         unlist( mean_param_list_s100_1[9]), # K4
                         unlist( mean_param_list_s100_2[9]),
                         unlist( mean_param_list_s100_3[9]),
                         unlist( mean_param_list_s100_4[9]),
                         unlist( mean_param_list_s100_5[9]),

                         unlist( mean_param_list_s100_1[14]), # H4
                         unlist( mean_param_list_s100_2[14]),
                         unlist( mean_param_list_s100_3[14]),
                         unlist( mean_param_list_s100_4[14]),
                         unlist( mean_param_list_s100_5[14])
                        )


Q4_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.046,0.05), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q4 ), mean( mean_param_list_true$Q4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.046,0.05),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.046,0.05), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



K4_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(10, 11), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K4 ), mean( mean_param_list_true$K4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(10, 11),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(10, 11), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H4_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.35, 0.38), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H4 ), mean( mean_param_list_true$H4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.35, 0.38),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.35, 0.38), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




mean_param_list <- list( unlist( mean_param_list_s100_1[5]), # Q5
                         unlist( mean_param_list_s100_2[5]),
                         unlist( mean_param_list_s100_3[5]),
                         unlist( mean_param_list_s100_4[5]),
                         unlist( mean_param_list_s100_5[5]),

                         unlist( mean_param_list_s100_1[10]), # K5
                         unlist( mean_param_list_s100_2[10]),
                         unlist( mean_param_list_s100_3[10]),
                         unlist( mean_param_list_s100_4[10]),
                         unlist( mean_param_list_s100_5[10]),

                         unlist( mean_param_list_s100_1[15]), # H5
                         unlist( mean_param_list_s100_2[15]),
                         unlist( mean_param_list_s100_3[15]),
                         unlist( mean_param_list_s100_4[15]),
                         unlist( mean_param_list_s100_5[15])
                        )


Q5_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.118,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[1])) ), mean( as.numeric(unlist(mean_param_list[1])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[2])) ), mean( as.numeric(unlist(mean_param_list[2])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[3])) ), mean( as.numeric(unlist(mean_param_list[3])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[4])) ), mean( as.numeric(unlist(mean_param_list[4])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[5])) ), mean( as.numeric(unlist(mean_param_list[5])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$Q5 ), mean( mean_param_list_true$Q5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.118,0.126),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.118,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K5_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(8.4, 9.1), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[6])) ), mean( as.numeric(unlist(mean_param_list[6])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[7])) ), mean( as.numeric(unlist(mean_param_list[7])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[8])) ), mean( as.numeric(unlist(mean_param_list[8])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[9])) ), mean( as.numeric(unlist(mean_param_list[9])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[10])) ), mean( as.numeric(unlist(mean_param_list[10])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$K5 ), mean( mean_param_list_true$K5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(8.4, 9.1),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(8.4, 9.1), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



H5_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(0.36, 0.4), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[11])) ), mean( as.numeric(unlist(mean_param_list[11])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[12])) ), mean( as.numeric(unlist(mean_param_list[12])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[13])) ), mean( as.numeric(unlist(mean_param_list[13])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[14])) ), mean( as.numeric(unlist(mean_param_list[14])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[15])) ), mean( as.numeric(unlist(mean_param_list[15])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$H5 ), mean( mean_param_list_true$H5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.36, 0.4),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.36, 0.4), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



graphics.off()






