


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





########## create lists of parameter posteriors


##### true simulated values

mean_param_list_true <- list( SmuQ1, SmuQ2, SmuQ3, SmuQ4, SmuQ5,
                              SmuK1, SmuK2, SmuK3, SmuK4, SmuK5,
                              SmuH1, SmuH2, SmuH3, SmuH4, SmuH5,  # original H in units of g/cm^2 of skin surface of function=~5% of body mass (**DO NOT need to multiply by (cm^2 skin surface area)/(g body mass)*1/0.05 **). Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
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
                               post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                               0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                              )
names(mean_param_list_y30_1) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                   "K1", "K2", "K3", "K4", "K5",
                                   "H1", "H2", "H3", "H4", "H5",
                                   "I1", "I2", "I3", "I4", "I5"
                                  )

##

post <- post6_y30_2

mean_param_list_y30_2 <- list( post$"mQ[2,1]", post$"mQ[2,2]", post$"mQ[2,3]", post$"mQ[2,4]", post$"mQ[2,5]",
                               post$"mK[2,1]", post$"mK[2,2]", post$"mK[2,3]", post$"mK[2,4]", post$"mK[2,5]",
                               post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                               post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                               post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                               post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                              post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                              post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                              post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                              post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                              post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
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
                                post$"mH[2,1]", post$"mH[2,2]", post$"mH[2,3]", post$"mH[2,4]", post$"mH[2,5]",
                                0,              post$"mI[2,1]", post$"mI[2,2]", post$"mI[2,3]", post$"mI[2,4]"
                               )

names(mean_param_list_s100_5) <- c( "Q1", "Q2", "Q3", "Q4", "Q5",
                                    "K1", "K2", "K3", "K4", "K5",
                                    "H1", "H2", "H3", "H4", "H5",
                                    "I1", "I2", "I3", "I4", "I5"
                                   )

 rm(post6_s100_1, post6_s100_2, post6_s100_3, post6_s100_4, post6_s100_5) #(list = ls(all=TRUE))





#################################################################################################################################

############### I






######## plotting colors

colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


# area and line colors
BerLine_lwd <- 4.5
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





pdf(file="./Plots/I_combined_l.pdf",
    height=20, width=20)

layout( matrix(data=c(  1, 5, 9,  13, 0,  0,  0,  0, 0, # column 1
                        2, 6, 10, 14, 0,  0,  0,  0, 0, # column 2
                        0, 0, 0,  0,  0,  0,  0,  0, 0,  # column 3 empty
                        3, 7, 11, 15, 0,  0,  0,  0, 0, # column 4
                        4, 8, 12, 16, 0,  0,  0,  0, 0, # column 5
                        0, 0, 0,  0,  0,  0,  0,  0, 0, # column 6 empty
                        0, 0, 0,  0,  0,  0,  0,  0, 0, # column 7
                        0, 0, 0,  0,  0,  0,  0,  0, 0, # column 8
                        0, 0, 0,  0,  0,  0,  0,  0, 0, # column 9 empty
                        0, 0, 0,  0,  0,  0,  0,  0, 0, # column 10
                        0, 0, 0,  0,  0,  0,  0,  0, 0  # column 11
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=c( rep(1,times=4), rep(0.0001, times=5) ),
        #widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
        widths=c(1, 1, 0.0002, 1, 1, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(10, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


cex_axis <- 1.75 # size of x-axis labels
age_seq <- seq(from=(0.1), to=(26.75), by=0.1)  #c(0.1,1,2)


# 10 individuals, measured 10 times, 1 year intervals ###########################################################################################################################################################################


mean_param_list <- list( unlist( mean_param_list_y30_1[17]), # I2
                         unlist( mean_param_list_y30_2[17]),
                         unlist( mean_param_list_y30_3[17]),
                         unlist( mean_param_list_y30_4[17]),
                         unlist( mean_param_list_y30_5[17]),

                         unlist( mean_param_list_y30_1[18]), # I3
                         unlist( mean_param_list_y30_2[18]),
                         unlist( mean_param_list_y30_3[18]),
                         unlist( mean_param_list_y30_4[18]),
                         unlist( mean_param_list_y30_5[18]),

                         unlist( mean_param_list_y30_1[19]), # I4
                         unlist( mean_param_list_y30_2[19]),
                         unlist( mean_param_list_y30_3[19]),
                         unlist( mean_param_list_y30_4[19]),
                         unlist( mean_param_list_y30_5[19]),

                         unlist( mean_param_list_y30_1[20]), # I5
                         unlist( mean_param_list_y30_2[20]),
                         unlist( mean_param_list_y30_3[20]),
                         unlist( mean_param_list_y30_4[20]),
                         unlist( mean_param_list_y30_5[20])
                        )


I2_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.117,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I2 ), mean( mean_param_list_true$I2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.117,0.126),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.117,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)







#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("Simulation value", "Estimated runs", "")
xcoords <- c(0, 7, 23)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=10, y=13.5,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerArea_col, MatArea_col, "white"),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=3,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = 9,
       ybottom = 11,
       xright = 30,
       ytop = 13.7,
       lwd=1)

# column labels
text(expression(paste(bolditalic("i")[2])), x=5, y=8.5, cex=3.75)
text(expression(paste(bolditalic("i")[3])), x=16, y=8.5, cex=3.75)
text(expression(paste(bolditalic("i")[4])), x=27.5, y=8.5, cex=3.75)
text(expression(paste(bolditalic("i")[5])), x=39, y=8.5, cex=3.75)

# row labels
text("10 idv, 10 ms, 1 int", x=-4.5, y=1, cex=3)
text("20 idv, 5 ms, 2 int", x=-4.5, y=-11.5, cex=3)
text("30 idv, 25 ms, 1 int", x=-4.5, y=-24, cex=3)
#text("single, 10 no weight", x=-4.5, y=-36.5, cex=3)

text("Age since conception (years)", x=23, y=-29, cex=3.75)



par(xpd=FALSE)





I3_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(0.7,0.78), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I3 ), mean( mean_param_list_true$I3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.7,0.78),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.7,0.78), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




I4_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(2.45,2.65), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I4 ), mean( mean_param_list_true$I4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(2.45,2.65),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(2.45,2.65), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




I5_post_plot <- denschart3( mean_param_list[c(16,17,18,19,20)],
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
            xlim= c(9.5,10.5), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[16])) ), mean( as.numeric(unlist(mean_param_list[16])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[17])) ), mean( as.numeric(unlist(mean_param_list[17])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[18])) ), mean( as.numeric(unlist(mean_param_list[18])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[19])) ), mean( as.numeric(unlist(mean_param_list[19])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[20])) ), mean( as.numeric(unlist(mean_param_list[20])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$I5 ), mean( mean_param_list_true$I5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(9.5,10.5),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(9.5,10.5), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)






# 20 individuals, measured 5 times, 2 year intervals ###########################################################################################################################################################################


mean_param_list <- list( unlist( mean_param_list_d5_1[17]), # I2
                         unlist( mean_param_list_d5_2[17]),
                         unlist( mean_param_list_d5_3[17]),
                         unlist( mean_param_list_d5_4[17]),
                         unlist( mean_param_list_d5_5[17]),

                         unlist( mean_param_list_d5_1[18]), # I3
                         unlist( mean_param_list_d5_2[18]),
                         unlist( mean_param_list_d5_3[18]),
                         unlist( mean_param_list_d5_4[18]),
                         unlist( mean_param_list_d5_5[18]),

                         unlist( mean_param_list_d5_1[19]), # I4
                         unlist( mean_param_list_d5_2[19]),
                         unlist( mean_param_list_d5_3[19]),
                         unlist( mean_param_list_d5_4[19]),
                         unlist( mean_param_list_d5_5[19]),

                         unlist( mean_param_list_d5_1[20]), # I5
                         unlist( mean_param_list_d5_2[20]),
                         unlist( mean_param_list_d5_3[20]),
                         unlist( mean_param_list_d5_4[20]),
                         unlist( mean_param_list_d5_5[20])
                        )


I2_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.117,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I2 ), mean( mean_param_list_true$I2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.117,0.126),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.117,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)





I3_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(0.7,0.78), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I3 ), mean( mean_param_list_true$I3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.7,0.78),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.7,0.78), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




I4_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(2.45,2.65), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I4 ), mean( mean_param_list_true$I4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(2.45,2.65),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(2.45,2.65), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




I5_post_plot <- denschart3( mean_param_list[c(16,17,18,19,20)],
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
            xlim= c(9.5,10.5), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[16])) ), mean( as.numeric(unlist(mean_param_list[16])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[17])) ), mean( as.numeric(unlist(mean_param_list[17])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[18])) ), mean( as.numeric(unlist(mean_param_list[18])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[19])) ), mean( as.numeric(unlist(mean_param_list[19])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[20])) ), mean( as.numeric(unlist(mean_param_list[20])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$I5 ), mean( mean_param_list_true$I5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(9.5,10.5),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(9.5,10.5), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)






# 30 full data ###########################################################################################################################################################################


mean_param_list <- list( unlist( mean_param_list_s100_1[17]), # I2
                         unlist( mean_param_list_s100_2[17]),
                         unlist( mean_param_list_s100_3[17]),
                         unlist( mean_param_list_s100_4[17]),
                         unlist( mean_param_list_s100_5[17]),

                         unlist( mean_param_list_s100_1[18]), # I3
                         unlist( mean_param_list_s100_2[18]),
                         unlist( mean_param_list_s100_3[18]),
                         unlist( mean_param_list_s100_4[18]),
                         unlist( mean_param_list_s100_5[18]),

                         unlist( mean_param_list_s100_1[19]), # I4
                         unlist( mean_param_list_s100_2[19]),
                         unlist( mean_param_list_s100_3[19]),
                         unlist( mean_param_list_s100_4[19]),
                         unlist( mean_param_list_s100_5[19]),

                         unlist( mean_param_list_s100_1[20]), # I5
                         unlist( mean_param_list_s100_2[20]),
                         unlist( mean_param_list_s100_3[20]),
                         unlist( mean_param_list_s100_4[20]),
                         unlist( mean_param_list_s100_5[20])
                        )


I2_post_plot <- denschart3( mean_param_list[c(1,2,3,4,5)],
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
            xlim= c(0.117,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I2 ), mean( mean_param_list_true$I2 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.117,0.126),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.117,0.126), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)





I3_post_plot <- denschart3( mean_param_list[c(6,7,8,9,10)],
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
            xlim= c(0.7,0.78), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I3 ), mean( mean_param_list_true$I3 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(0.7,0.78),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.7,0.78), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




I4_post_plot <- denschart3( mean_param_list[c(11,12,13,14,15)],
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
            xlim= c(2.45,2.65), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
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
lines(x=c( mean( mean_param_list_true$I4 ), mean( mean_param_list_true$I4 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(2.45,2.65),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(2.45,2.65), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




I5_post_plot <- denschart3( mean_param_list[c(16,17,18,19,20)],
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
            xlim= c(9.5,10.5), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                  #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5, 0.5, 0.5, 0.5, 0.5
                      )
 )


lines(x=c( mean( as.numeric(unlist(mean_param_list[16])) ), mean( as.numeric(unlist(mean_param_list[16])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[17])) ), mean( as.numeric(unlist(mean_param_list[17])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[18])) ), mean( as.numeric(unlist(mean_param_list[18])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[19])) ), mean( as.numeric(unlist(mean_param_list[19])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( as.numeric(unlist(mean_param_list[20])) ), mean( as.numeric(unlist(mean_param_list[20])) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)      
lines(x=c( mean( mean_param_list_true$I5 ), mean( mean_param_list_true$I5 ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c(9.5,10.5),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(9.5,10.5), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



graphics.off()






