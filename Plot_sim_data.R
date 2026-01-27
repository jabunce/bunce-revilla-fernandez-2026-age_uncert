
############################

# In RunAll.R : Nsims <- 30, SimData <- fullSimData

############################

colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.75)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


colorlist2 <- hcl.colors(n=11, palette="Green-Brown",
                        alpha=0.75)
names(colorlist2) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist2)


# point and line colors and sizes
BerPoint_lwd <- 0.5
BerPoint_col <- colorlist["2.3"]
BerPoint_cex <- 0.5
BerPoint_pch <- 1

MatPoint_lwd <- 0.5
MatPoint_col <- "black" #colorlist["1.3"]
MatPoint_cex <- 0.5
MatPoint_pch <- 1


BerIndivTraj_lwd <- 0.25
BerIndivTraj_col <- colorlist["2.4"]
BerIndivTraj_lty <- 1

MatIndivTraj_lwd <- 0.25
MatIndivTraj_col <- grey(0.8) #colorlist["1.4"]
MatIndivTraj_lty <- 1


BerMeanTraj_lwd <- 3
BerMeanTraj_col <- colorlist["2.2"]
BerMeanTraj_lty <- 1

MatMeanTraj_lwd <- 3
MatMeanTraj_col <- "black" # colorlist["1.1"]
MatMeanTraj_lty <- 1


BerVelTraj_lwd <- 2
BerVelTraj_col <- colorlist2["2.2"]
BerVelTrajArea_col <- colorlist2["2.3"]
BerVelTraj_lty <- 1

MatVelTraj_lwd <- 2
MatVelTraj_col <- colorlist2["1.2"]
MatVelTrajArea_col <- colorlist2["1.4"]
MatVelTraj_lty <- 1






pdf(file="./Plots/Sim_data_fhw.pdf",
    height=10, width=10)
par(mfcol = c(2, 2)) # mfcol fills by column, mfrow fills by row 
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age
  

  #### Height
  #### cumulative trajectories, full simulated dataset

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,180), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels = FALSE )
  axis( side=2, at=seq( 0, 180, by=30 ), labels = TRUE )



  # plot simulated individual trajectories
  for ( z in 1:Nsims ){

    lines(x = x,
          y = ifelse( x <= sI2[z], 0.012 + ( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z]),

              ifelse( x <= sI3[z], 0.012 + ( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z]) +
                                          ( 2*sH2[z]/sK2[z] * ( 1 - exp(sK2[z]*sQ2[z]*( sI2[z] - x )/( 1 + 2*sQ2[z] )) ) )^(1/sQ2[z]),

              ifelse( x <= sI4[z], 0.012 + ( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z]) +
                                          ( 2*sH2[z]/sK2[z] * ( 1 - exp(sK2[z]*sQ2[z]*( sI2[z] - x )/( 1 + 2*sQ2[z] )) ) )^(1/sQ2[z]) +
                                          ( 2*sH3[z]/sK3[z] * ( 1 - exp(sK3[z]*sQ3[z]*( sI3[z] - x )/( 1 + 2*sQ3[z] )) ) )^(1/sQ3[z]),

              ifelse( x <= sI5[z], 0.012 + ( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z]) +
                                          ( 2*sH2[z]/sK2[z] * ( 1 - exp(sK2[z]*sQ2[z]*( sI2[z] - x )/( 1 + 2*sQ2[z] )) ) )^(1/sQ2[z]) +
                                          ( 2*sH3[z]/sK3[z] * ( 1 - exp(sK3[z]*sQ3[z]*( sI3[z] - x )/( 1 + 2*sQ3[z] )) ) )^(1/sQ3[z]) +
                                          ( 2*sH4[z]/sK4[z] * ( 1 - exp(sK4[z]*sQ4[z]*( sI4[z] - x )/( 1 + 2*sQ4[z] )) ) )^(1/sQ4[z]),

                                  0.012 + ( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z]) +
                                          ( 2*sH2[z]/sK2[z] * ( 1 - exp(sK2[z]*sQ2[z]*( sI2[z] - x )/( 1 + 2*sQ2[z] )) ) )^(1/sQ2[z]) +
                                          ( 2*sH3[z]/sK3[z] * ( 1 - exp(sK3[z]*sQ3[z]*( sI3[z] - x )/( 1 + 2*sQ3[z] )) ) )^(1/sQ3[z]) +
                                          ( 2*sH4[z]/sK4[z] * ( 1 - exp(sK4[z]*sQ4[z]*( sI4[z] - x )/( 1 + 2*sQ4[z] )) ) )^(1/sQ4[z]) +
                                          ( 2*sH5[z]/sK5[z] * ( 1 - exp(sK5[z]*sQ5[z]*( sI5[z] - x )/( 1 + 2*sQ5[z] )) ) )^(1/sQ5[z])
              ) ) ) ),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

  } #for z


  # group mean trajectory
    lines(x = x,
          y = ifelse( x <= SmuI2, 0.012 + ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1),

              ifelse( x <= SmuI3, 0.012 + ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1) +
                                          ( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2),

              ifelse( x <= SmuI4, 0.012 + ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1) +
                                          ( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2) +
                                          ( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3),

              ifelse( x <= SmuI5, 0.012 + ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1) +
                                          ( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2) +
                                          ( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3) +
                                          ( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4),

                                  0.012 + ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1) +
                                          ( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2) +
                                          ( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3) +
                                          ( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4) +
                                          ( 2*SmuH5/SmuK5 * ( 1 - exp(SmuK5*SmuQ5*( SmuI5 - x )/( 1 + 2*SmuQ5 )) ) )^(1/SmuQ5)
              ) ) ) ),
          col="black", lwd=3, lty=1)



  # prior trajectory
    lines(x = x,
          y = ifelse( x <= muI2, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1),

              ifelse( x <= muI3, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2),

              ifelse( x <= muI4, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3),

              ifelse( x <= muI5, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3) +
                                         ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4),

                                 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3) +
                                         ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4) +
                                         ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5)
              ) ) ) ),
          col=BerMeanTraj_col, lwd=3, lty=1)

  points(x=fullSimData$ageSinceConception, y=fullSimData$height, lwd=0.5, col="black", cex=0.5)



  # group mean trajectory components
  # infant
  lines(x = x,
        y = ifelse( x > SmuI1, ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1), NA),
          col="black", #colorlist["1.1"],
          lwd=3, lty=1)

  # child 1
  lines(x = x,
        y = ifelse( x > SmuI2, ( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2), NA),
        col="black", #"plum4",
        lwd=3, lty=1)

  # child 2
  lines(x = x,
        y = ifelse( x > SmuI3, ( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3), NA),
        col="black", #colorlist2["1.1"],
        lwd=3, lty=1)

  # child 3
  lines(x = x,
        y = ifelse( x > SmuI4, ( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4), NA),
        col="black", #colorlist2["2.1"],
        lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ifelse( x > SmuI5, ( 2*SmuH5/SmuK5 * ( 1 - exp(SmuK5*SmuQ5*( SmuI5 - x )/( 1 + 2*SmuQ5 )) ) )^(1/SmuQ5), NA),
        col="black", #colorlist["2.1"],
        lwd=3, lty=1)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) # add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), labels=FALSE, las=0, srt=270 )
  text( x=28.75, y=seq( 0, 16, by=4 ), labels=seq( 0, 16, by=4 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE) # clip to plot region


  # Prior

  y_primeBer = ifelse( x < muI[1], 1/( 1 + 2*muQ[1] )*2^(1/muQ[1])*muH[1]*exp((muK[1]*muQ[1]*( 0    - x ))/( 1 + 2*muQ[1] ))*(muH[1]/muK[1]*( 1 - exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] )) ))^( 1/muQ[1] - 1 ),

             ifelse( x < muI[2], 1/( 1 + 2*muQ[1] )*2^(1/muQ[1])*muH[1]*exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] ))*(muH[1]/muK[1]*( 1 - exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] )) ))^( 1/muQ[1] - 1 ) +
                                1/( 1 + 2*muQ[2] )*2^(1/muQ[2])*muH[2]*exp((muK[2]*muQ[2]*( muI[1] - x ))/( 1 + 2*muQ[2] ))*(muH[2]/muK[2]*( 1 - exp((muK[2]*muQ[2]*( muI[1] - x ))/( 1 + 2*muQ[2] )) ))^( 1/muQ[2] - 1 ),

             ifelse( x < muI[3], 1/( 1 + 2*muQ[1] )*2^(1/muQ[1])*muH[1]*exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] ))*(muH[1]/muK[1]*( 1 - exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] )) ))^( 1/muQ[1] - 1 ) +
                                1/( 1 + 2*muQ[2] )*2^(1/muQ[2])*muH[2]*exp((muK[2]*muQ[2]*( muI[1] - x ))/( 1 + 2*muQ[2] ))*(muH[2]/muK[2]*( 1 - exp((muK[2]*muQ[2]*( muI[1] - x ))/( 1 + 2*muQ[2] )) ))^( 1/muQ[2] - 1 ) +
                                1/( 1 + 2*muQ[3] )*2^(1/muQ[3])*muH[3]*exp((muK[3]*muQ[3]*( muI[2] - x ))/( 1 + 2*muQ[3] ))*(muH[3]/muK[3]*( 1 - exp((muK[3]*muQ[3]*( muI[2] - x ))/( 1 + 2*muQ[3] )) ))^( 1/muQ[3] - 1 ),

             ifelse( x < muI[4], 1/( 1 + 2*muQ[1] )*2^(1/muQ[1])*muH[1]*exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] ))*(muH[1]/muK[1]*( 1 - exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] )) ))^( 1/muQ[1] - 1 ) +
                                1/( 1 + 2*muQ[2] )*2^(1/muQ[2])*muH[2]*exp((muK[2]*muQ[2]*( muI[1] - x ))/( 1 + 2*muQ[2] ))*(muH[2]/muK[2]*( 1 - exp((muK[2]*muQ[2]*( muI[1] - x ))/( 1 + 2*muQ[2] )) ))^( 1/muQ[2] - 1 ) +
                                1/( 1 + 2*muQ[3] )*2^(1/muQ[3])*muH[3]*exp((muK[3]*muQ[3]*( muI[2] - x ))/( 1 + 2*muQ[3] ))*(muH[3]/muK[3]*( 1 - exp((muK[3]*muQ[3]*( muI[2] - x ))/( 1 + 2*muQ[3] )) ))^( 1/muQ[3] - 1 ) +
                                1/( 1 + 2*muQ[4] )*2^(1/muQ[4])*muH[4]*exp((muK[4]*muQ[4]*( muI[3] - x ))/( 1 + 2*muQ[4] ))*(muH[4]/muK[4]*( 1 - exp((muK[4]*muQ[4]*( muI[3] - x ))/( 1 + 2*muQ[4] )) ))^( 1/muQ[4] - 1 ),

                                1/( 1 + 2*muQ[1] )*2^(1/muQ[1])*muH[1]*exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] ))*(muH[1]/muK[1]*( 1 - exp((muK[1]*muQ[1]*( 0     - x ))/( 1 + 2*muQ[1] )) ))^( 1/muQ[1] - 1 ) +
                                1/( 1 + 2*muQ[2] )*2^(1/muQ[2])*muH[2]*exp((muK[2]*muQ[2]*( muI[1] - x ))/( 1 + 2*muQ[2] ))*(muH[2]/muK[2]*( 1 - exp((muK[2]*muQ[2]*( muI[1] - x ))/( 1 + 2*muQ[2] )) ))^( 1/muQ[2] - 1 ) +
                                1/( 1 + 2*muQ[3] )*2^(1/muQ[3])*muH[3]*exp((muK[3]*muQ[3]*( muI[2] - x ))/( 1 + 2*muQ[3] ))*(muH[3]/muK[3]*( 1 - exp((muK[3]*muQ[3]*( muI[2] - x ))/( 1 + 2*muQ[3] )) ))^( 1/muQ[3] - 1 ) +
                                1/( 1 + 2*muQ[4] )*2^(1/muQ[4])*muH[4]*exp((muK[4]*muQ[4]*( muI[3] - x ))/( 1 + 2*muQ[4] ))*(muH[4]/muK[4]*( 1 - exp((muK[4]*muQ[4]*( muI[3] - x ))/( 1 + 2*muQ[4] )) ))^( 1/muQ[4] - 1 ) +
                                1/( 1 + 2*muQ[5] )*2^(1/muQ[5])*muH[5]*exp((muK[5]*muQ[5]*( muI[4] - x ))/( 1 + 2*muQ[5] ))*(muH[5]/muK[5]*( 1 - exp((muK[5]*muQ[5]*( muI[4] - x ))/( 1 + 2*muQ[5] )) ))^( 1/muQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Sim data
  
  y_primeMat = ifelse( x < SmI[1], 1/( 1 + 2*SmQ[1] )*2^(1/SmQ[1])*SmH[1]*exp((SmK[1]*SmQ[1]*( 0    - x ))/( 1 + 2*SmQ[1] ))*(SmH[1]/SmK[1]*( 1 - exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] )) ))^( 1/SmQ[1] - 1 ),

             ifelse( x < SmI[2], 1/( 1 + 2*SmQ[1] )*2^(1/SmQ[1])*SmH[1]*exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] ))*(SmH[1]/SmK[1]*( 1 - exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] )) ))^( 1/SmQ[1] - 1 ) +
                                1/( 1 + 2*SmQ[2] )*2^(1/SmQ[2])*SmH[2]*exp((SmK[2]*SmQ[2]*( SmI[1] - x ))/( 1 + 2*SmQ[2] ))*(SmH[2]/SmK[2]*( 1 - exp((SmK[2]*SmQ[2]*( SmI[1] - x ))/( 1 + 2*SmQ[2] )) ))^( 1/SmQ[2] - 1 ),

             ifelse( x < SmI[3], 1/( 1 + 2*SmQ[1] )*2^(1/SmQ[1])*SmH[1]*exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] ))*(SmH[1]/SmK[1]*( 1 - exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] )) ))^( 1/SmQ[1] - 1 ) +
                                1/( 1 + 2*SmQ[2] )*2^(1/SmQ[2])*SmH[2]*exp((SmK[2]*SmQ[2]*( SmI[1] - x ))/( 1 + 2*SmQ[2] ))*(SmH[2]/SmK[2]*( 1 - exp((SmK[2]*SmQ[2]*( SmI[1] - x ))/( 1 + 2*SmQ[2] )) ))^( 1/SmQ[2] - 1 ) +
                                1/( 1 + 2*SmQ[3] )*2^(1/SmQ[3])*SmH[3]*exp((SmK[3]*SmQ[3]*( SmI[2] - x ))/( 1 + 2*SmQ[3] ))*(SmH[3]/SmK[3]*( 1 - exp((SmK[3]*SmQ[3]*( SmI[2] - x ))/( 1 + 2*SmQ[3] )) ))^( 1/SmQ[3] - 1 ),

             ifelse( x < SmI[4], 1/( 1 + 2*SmQ[1] )*2^(1/SmQ[1])*SmH[1]*exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] ))*(SmH[1]/SmK[1]*( 1 - exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] )) ))^( 1/SmQ[1] - 1 ) +
                                1/( 1 + 2*SmQ[2] )*2^(1/SmQ[2])*SmH[2]*exp((SmK[2]*SmQ[2]*( SmI[1] - x ))/( 1 + 2*SmQ[2] ))*(SmH[2]/SmK[2]*( 1 - exp((SmK[2]*SmQ[2]*( SmI[1] - x ))/( 1 + 2*SmQ[2] )) ))^( 1/SmQ[2] - 1 ) +
                                1/( 1 + 2*SmQ[3] )*2^(1/SmQ[3])*SmH[3]*exp((SmK[3]*SmQ[3]*( SmI[2] - x ))/( 1 + 2*SmQ[3] ))*(SmH[3]/SmK[3]*( 1 - exp((SmK[3]*SmQ[3]*( SmI[2] - x ))/( 1 + 2*SmQ[3] )) ))^( 1/SmQ[3] - 1 ) +
                                1/( 1 + 2*SmQ[4] )*2^(1/SmQ[4])*SmH[4]*exp((SmK[4]*SmQ[4]*( SmI[3] - x ))/( 1 + 2*SmQ[4] ))*(SmH[4]/SmK[4]*( 1 - exp((SmK[4]*SmQ[4]*( SmI[3] - x ))/( 1 + 2*SmQ[4] )) ))^( 1/SmQ[4] - 1 ),

                                1/( 1 + 2*SmQ[1] )*2^(1/SmQ[1])*SmH[1]*exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] ))*(SmH[1]/SmK[1]*( 1 - exp((SmK[1]*SmQ[1]*( 0     - x ))/( 1 + 2*SmQ[1] )) ))^( 1/SmQ[1] - 1 ) +
                                1/( 1 + 2*SmQ[2] )*2^(1/SmQ[2])*SmH[2]*exp((SmK[2]*SmQ[2]*( SmI[1] - x ))/( 1 + 2*SmQ[2] ))*(SmH[2]/SmK[2]*( 1 - exp((SmK[2]*SmQ[2]*( SmI[1] - x ))/( 1 + 2*SmQ[2] )) ))^( 1/SmQ[2] - 1 ) +
                                1/( 1 + 2*SmQ[3] )*2^(1/SmQ[3])*SmH[3]*exp((SmK[3]*SmQ[3]*( SmI[2] - x ))/( 1 + 2*SmQ[3] ))*(SmH[3]/SmK[3]*( 1 - exp((SmK[3]*SmQ[3]*( SmI[2] - x ))/( 1 + 2*SmQ[3] )) ))^( 1/SmQ[3] - 1 ) +
                                1/( 1 + 2*SmQ[4] )*2^(1/SmQ[4])*SmH[4]*exp((SmK[4]*SmQ[4]*( SmI[3] - x ))/( 1 + 2*SmQ[4] ))*(SmH[4]/SmK[4]*( 1 - exp((SmK[4]*SmQ[4]*( SmI[3] - x ))/( 1 + 2*SmQ[4] )) ))^( 1/SmQ[4] - 1 ) +
                                1/( 1 + 2*SmQ[5] )*2^(1/SmQ[5])*SmH[5]*exp((SmK[5]*SmQ[5]*( SmI[4] - x ))/( 1 + 2*SmQ[5] ))*(SmH[5]/SmK[5]*( 1 - exp((SmK[5]*SmQ[5]*( SmI[4] - x ))/( 1 + 2*SmQ[5] )) ))^( 1/SmQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)

  par(xpd=NA) # clip plotting to device region
  text("Height (cm)", x=-6.5, y=8, srt=90, las=3, cex=1.8)
  text("Growth velocity (cm/year)", x=32, y=8, srt=270, las=3, cex=1.8)




###


#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region

# legend
  # set horizontal spacing for legend text

  legtext <- c("Sim data",
               "Sim indiv trajectory" #, 
               #"Mean of estimate runs",
               #"Prior trajectory"
               )
  xcoords <- c(0,
               4.5 #,  #4 #8.5 # moves third item right
               #8.5, #7.5 #24.2 # moves last item right
               #15  #36.8 # moves last three items together right
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 3 #2 # moves last three items right  # so replace element 1 with a finite number (any will do)



  legend(x=-1.4, y=13.1,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatPoint_col,
               #BerMeanTraj_col,
               MatIndivTraj_col #, 
               #PriorTraj_col
              ),
         merge=FALSE,
         pch=c(MatPoint_pch,
               NA #, 
               #NA, 
               #NA
               ),
         lty=c(0,
               #BerMeanTraj_lty,
               MatIndivTraj_lty #,
               #PriorTraj_lty
               ),
         lwd=c(MatPoint_lwd,
               #BerMeanTraj_lwd,
               MatIndivTraj_lwd #,
               #PriorTraj_lwd
               ),
         seg.len=2 )


  legtext <-c("Sim mean trajectory",
              "Prior trajectory" #,
               #"Estimate run",
               #""
               )
  xcoords <- c(0,
               4.5 #,
               #8.5,
               #15
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 3 # so replace element 1 with a finite number (any will do)


  legend(x=-1.4, y=12.4,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatMeanTraj_col,
               BerMeanTraj_col #,
               #MatIndivTraj_col,
               #NA
              ),
         merge=FALSE,
         pch=c(NA, #MatPoint_pch,
               NA #, 
               #NA, 
               #NA
               ),
         lty=c(MatMeanTraj_lty,
               BerMeanTraj_lty #,
               #MatIndivTraj_lty,
               #NA
               ),
         lwd=c(MatMeanTraj_lwd,
               BerMeanTraj_lwd #,
               #MatIndivTraj_lwd,
               #NA
               ),
         seg.len=2 )


  legtext <-c("Sim mean velocity",
              "Prior velocity" #,
               #"Estimate run",
               #""
               )
  xcoords <- c(0,
               4.5 #,
               #8.5,
               #15
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 3 # so replace element 1 with a finite number (any will do)


  legend(x=-1.4, y=11.7,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatVelTraj_col,
               BerVelTraj_col #,
               #MatIndivTraj_col,
               #NA
              ),
         merge=FALSE,
         pch=c(NA, #MatPoint_pch,
               NA #, 
               #NA, 
               #NA
               ),
         lty=c(MatVelTraj_lty,
               BerVelTraj_lty #,
               #MatIndivTraj_lty,
               #NA
               ),
         lwd=c(MatVelTraj_lwd,
               BerVelTraj_lwd #,
               #MatIndivTraj_lwd,
               #NA
               ),
         seg.len=2 )


  rect(xleft = -1.6,
       ybottom = 10.7,
       xright = 11.7,
       ytop = 12.95,
       lwd=1)





  #### Weight

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,3000), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels = TRUE )
  axis( side=2, at=seq( 0, 3000, by=1000 ), labels = TRUE )
  par(xpd=FALSE) # clip plotting to plot region


  # plot simulated individual trajectories
  for ( z in 1:Nsims ){

    lines(x = x,
          y = ifelse( x < sI2[z], 3.6e-9 + pi*( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z] + 2),

              ifelse( x < sI3[z], 3.6e-9 + pi*( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z] + 2) +
                                           pi*( 2*sH2[z]/sK2[z] * ( 1 - exp(sK2[z]*sQ2[z]*( sI2[z] - x )/( 1 + 2*sQ2[z] )) ) )^(1/sQ2[z] + 2),

              ifelse( x < sI4[z], 3.6e-9 + pi*( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z] + 2) +
                                           pi*( 2*sH2[z]/sK2[z] * ( 1 - exp(sK2[z]*sQ2[z]*( sI2[z] - x )/( 1 + 2*sQ2[z] )) ) )^(1/sQ2[z] + 2) +
                                           pi*( 2*sH3[z]/sK3[z] * ( 1 - exp(sK3[z]*sQ3[z]*( sI3[z] - x )/( 1 + 2*sQ3[z] )) ) )^(1/sQ3[z] + 2),

              ifelse( x < sI5[z], 3.6e-9 + pi*( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z] + 2) +
                                           pi*( 2*sH2[z]/sK2[z] * ( 1 - exp(sK2[z]*sQ2[z]*( sI2[z] - x )/( 1 + 2*sQ2[z] )) ) )^(1/sQ2[z] + 2) +
                                           pi*( 2*sH3[z]/sK3[z] * ( 1 - exp(sK3[z]*sQ3[z]*( sI3[z] - x )/( 1 + 2*sQ3[z] )) ) )^(1/sQ3[z] + 2) +
                                           pi*( 2*sH4[z]/sK4[z] * ( 1 - exp(sK4[z]*sQ4[z]*( sI4[z] - x )/( 1 + 2*sQ4[z] )) ) )^(1/sQ4[z] + 2),                                                                                                                                                                  

                                  3.6e-9 + pi*( 2*sH1[z]/sK1[z] * ( 1 - exp(sK1[z]*sQ1[z]*( sI1[z] - x )/( 1 + 2*sQ1[z] )) ) )^(1/sQ1[z] + 2) +
                                           pi*( 2*sH2[z]/sK2[z] * ( 1 - exp(sK2[z]*sQ2[z]*( sI2[z] - x )/( 1 + 2*sQ2[z] )) ) )^(1/sQ2[z] + 2) +
                                           pi*( 2*sH3[z]/sK3[z] * ( 1 - exp(sK3[z]*sQ3[z]*( sI3[z] - x )/( 1 + 2*sQ3[z] )) ) )^(1/sQ3[z] + 2) +
                                           pi*( 2*sH4[z]/sK4[z] * ( 1 - exp(sK4[z]*sQ4[z]*( sI4[z] - x )/( 1 + 2*sQ4[z] )) ) )^(1/sQ4[z] + 2) +
                                           pi*( 2*sH5[z]/sK5[z] * ( 1 - exp(sK5[z]*sQ5[z]*( sI5[z] - x )/( 1 + 2*sQ5[z] )) ) )^(1/sQ5[z] + 2)
              ) ) ) ), 
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

  } #for z


  # group mean trajectory
    lines(x = x,
          y = ifelse( x < SmuI2, 3.6e-9 + pi*( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1 + 2),

              ifelse( x < SmuI3, 3.6e-9 + pi*( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1 + 2) +
                                          pi*( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2 + 2),

              ifelse( x < SmuI4, 3.6e-9 + pi*( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1 + 2) +
                                          pi*( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2 + 2) +
                                          pi*( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3 + 2),

              ifelse( x < SmuI5, 3.6e-9 + pi*( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1 + 2) +
                                          pi*( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2 + 2) +
                                          pi*( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3 + 2) +
                                          pi*( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4 + 2),                                                                                                                                                              

                                 3.6e-9 + pi*( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1 + 2) +
                                          pi*( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2 + 2) +
                                          pi*( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3 + 2) +
                                          pi*( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4 + 2) +
                                          pi*( 2*SmuH5/SmuK5 * ( 1 - exp(SmuK5*SmuQ5*( SmuI5 - x )/( 1 + 2*SmuQ5 )) ) )^(1/SmuQ5 + 2) 

              ) ) ) ),
          col="black", lwd=3, lty=1)


  # prior trajectory
    lines(x = x,
          y = ifelse( x < muI2, 3.6e-9 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2),

              ifelse( x < muI3, 3.6e-9 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2) +
                                         pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2),

              ifelse( x < muI4, 3.6e-9 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2) +
                                         pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2) +
                                         pi*( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3 + 2),

              ifelse( x < muI5, 3.6e-9 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2) +
                                         pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2) +
                                         pi*( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3 + 2) +
                                         pi*( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4 + 2),                                                                                                                                                              

                                3.6e-9 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2) +
                                         pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2) +
                                         pi*( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3 + 2) +
                                         pi*( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4 + 2) +
                                         pi*( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5 + 2) 

              ) ) ) ),
          col=BerMeanTraj_col, lwd=3, lty=1)

  points(x=fullSimData$ageSinceConception, y=fullSimData$CellWeightg, lwd=0.5, col="black", cex=0.5)



  # group mean trajectory components

  # 1
  lines(x = x,
        y = ifelse( x > SmuI1, pi*( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1 + 2), NA),
        col="black", #colorlist["1.1"],
        lwd=3, lty=1)

  # 2
  lines(x = x,
        y = ifelse( x > SmuI2, pi*( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2 + 2), NA),
        col="black", #"plum4",
        lwd=3, lty=1)

  # 3
  lines(x = x,
        y = ifelse( x > SmuI3, pi*( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3 + 2), NA),
        col="black", #colorlist2["1.1"],
        lwd=3, lty=1)

  # 4
  lines(x = x,
        y = ifelse( x > SmuI4, pi*( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4 + 2), NA),
        col="black", #colorlist2["2.1"],
        lwd=3, lty=1)

  # 5
  lines(x = x,
        y = ifelse( x > SmuI5, pi*( 2*SmuH5/SmuK5 * ( 1 - exp(SmuK5*SmuQ5*( SmuI5 - x )/( 1 + 2*SmuQ5 )) ) )^(1/SmuQ5 + 2), NA),
        col="black", #colorlist["2.1"],
        lwd=3, lty=1)

  


  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) # add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=100 ), labels=FALSE )
  text( x=28.75, y=seq( 0, 400, by=100 ), labels=seq( 0, 400, by=100 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE) # clip to plot region


  # prior

  y_primeBer = ifelse( x < muI[1], (pi*muH[1]*( 2*muQ[1] + 1 )*2^( 2 + 1/muQ[1] ))/( 1 + 2*muQ[1] )*exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] ))*( muH[1]/muK[1]*( 1 - exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] )) ) )^( 1 + 1/muQ[1] ),

            ifelse( x < muI[2], (pi*muH[1]*( 2*muQ[1] + 1 )*2^( 2 + 1/muQ[1] ))/( 1 + 2*muQ[1] )*exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] ))*( muH[1]/muK[1]*( 1 - exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] )) ) )^( 1 + 1/muQ[1] ) + 
                               (pi*muH[2]*( 2*muQ[2] + 1 )*2^( 2 + 1/muQ[2] ))/( 1 + 2*muQ[2] )*exp(muK[2]*muQ[2]*( muI[1] - x )/( 1 + 2*muQ[2] ))*( muH[2]/muK[2]*( 1 - exp(muK[2]*muQ[2]*( muI[1] - x )/( 1 + 2*muQ[2] )) ) )^( 1 + 1/muQ[2] ),

            ifelse( x < muI[3], (pi*muH[1]*( 2*muQ[1] + 1 )*2^( 2 + 1/muQ[1] ))/( 1 + 2*muQ[1] )*exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] ))*( muH[1]/muK[1]*( 1 - exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] )) ) )^( 1 + 1/muQ[1] ) + 
                               (pi*muH[2]*( 2*muQ[2] + 1 )*2^( 2 + 1/muQ[2] ))/( 1 + 2*muQ[2] )*exp(muK[2]*muQ[2]*( muI[1] - x )/( 1 + 2*muQ[2] ))*( muH[2]/muK[2]*( 1 - exp(muK[2]*muQ[2]*( muI[1] - x )/( 1 + 2*muQ[2] )) ) )^( 1 + 1/muQ[2] ) +
                               (pi*muH[3]*( 2*muQ[3] + 1 )*2^( 2 + 1/muQ[3] ))/( 1 + 2*muQ[3] )*exp(muK[3]*muQ[3]*( muI[2] - x )/( 1 + 2*muQ[3] ))*( muH[3]/muK[3]*( 1 - exp(muK[3]*muQ[3]*( muI[2] - x )/( 1 + 2*muQ[3] )) ) )^( 1 + 1/muQ[3] ),

            ifelse( x < muI[4], (pi*muH[1]*( 2*muQ[1] + 1 )*2^( 2 + 1/muQ[1] ))/( 1 + 2*muQ[1] )*exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] ))*( muH[1]/muK[1]*( 1 - exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] )) ) )^( 1 + 1/muQ[1] ) + 
                               (pi*muH[2]*( 2*muQ[2] + 1 )*2^( 2 + 1/muQ[2] ))/( 1 + 2*muQ[2] )*exp(muK[2]*muQ[2]*( muI[1] - x )/( 1 + 2*muQ[2] ))*( muH[2]/muK[2]*( 1 - exp(muK[2]*muQ[2]*( muI[1] - x )/( 1 + 2*muQ[2] )) ) )^( 1 + 1/muQ[2] ) +
                               (pi*muH[3]*( 2*muQ[3] + 1 )*2^( 2 + 1/muQ[3] ))/( 1 + 2*muQ[3] )*exp(muK[3]*muQ[3]*( muI[2] - x )/( 1 + 2*muQ[3] ))*( muH[3]/muK[3]*( 1 - exp(muK[3]*muQ[3]*( muI[2] - x )/( 1 + 2*muQ[3] )) ) )^( 1 + 1/muQ[3] ) +
                               (pi*muH[4]*( 2*muQ[4] + 1 )*2^( 2 + 1/muQ[4] ))/( 1 + 2*muQ[4] )*exp(muK[4]*muQ[4]*( muI[3] - x )/( 1 + 2*muQ[4] ))*( muH[4]/muK[4]*( 1 - exp(muK[4]*muQ[4]*( muI[3] - x )/( 1 + 2*muQ[4] )) ) )^( 1 + 1/muQ[4] ),

                              (pi*muH[1]*( 2*muQ[1] + 1 )*2^( 2 + 1/muQ[1] ))/( 1 + 2*muQ[1] )*exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] ))*( muH[1]/muK[1]*( 1 - exp(muK[1]*muQ[1]*( 0     - x )/( 1 + 2*muQ[1] )) ) )^( 1 + 1/muQ[1] ) + 
                              (pi*muH[2]*( 2*muQ[2] + 1 )*2^( 2 + 1/muQ[2] ))/( 1 + 2*muQ[2] )*exp(muK[2]*muQ[2]*( muI[1] - x )/( 1 + 2*muQ[2] ))*( muH[2]/muK[2]*( 1 - exp(muK[2]*muQ[2]*( muI[1] - x )/( 1 + 2*muQ[2] )) ) )^( 1 + 1/muQ[2] ) +
                              (pi*muH[3]*( 2*muQ[3] + 1 )*2^( 2 + 1/muQ[3] ))/( 1 + 2*muQ[3] )*exp(muK[3]*muQ[3]*( muI[2] - x )/( 1 + 2*muQ[3] ))*( muH[3]/muK[3]*( 1 - exp(muK[3]*muQ[3]*( muI[2] - x )/( 1 + 2*muQ[3] )) ) )^( 1 + 1/muQ[3] ) +
                              (pi*muH[4]*( 2*muQ[4] + 1 )*2^( 2 + 1/muQ[4] ))/( 1 + 2*muQ[4] )*exp(muK[4]*muQ[4]*( muI[3] - x )/( 1 + 2*muQ[4] ))*( muH[4]/muK[4]*( 1 - exp(muK[4]*muQ[4]*( muI[3] - x )/( 1 + 2*muQ[4] )) ) )^( 1 + 1/muQ[4] ) +
                              (pi*muH[5]*( 2*muQ[5] + 1 )*2^( 2 + 1/muQ[5] ))/( 1 + 2*muQ[5] )*exp(muK[5]*muQ[5]*( muI[4] - x )/( 1 + 2*muQ[5] ))*( muH[5]/muK[5]*( 1 - exp(muK[5]*muQ[5]*( muI[4] - x )/( 1 + 2*muQ[5] )) ) )^( 1 + 1/muQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Sim data

  y_primeMat = ifelse( x < SmI[1], (pi*SmH[1]*( 2*SmQ[1] + 1 )*2^( 2 + 1/SmQ[1] ))/( 1 + 2*SmQ[1] )*exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] ))*( SmH[1]/SmK[1]*( 1 - exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] )) ) )^( 1 + 1/SmQ[1] ),

            ifelse( x < SmI[2], (pi*SmH[1]*( 2*SmQ[1] + 1 )*2^( 2 + 1/SmQ[1] ))/( 1 + 2*SmQ[1] )*exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] ))*( SmH[1]/SmK[1]*( 1 - exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] )) ) )^( 1 + 1/SmQ[1] ) + 
                               (pi*SmH[2]*( 2*SmQ[2] + 1 )*2^( 2 + 1/SmQ[2] ))/( 1 + 2*SmQ[2] )*exp(SmK[2]*SmQ[2]*( SmI[1] - x )/( 1 + 2*SmQ[2] ))*( SmH[2]/SmK[2]*( 1 - exp(SmK[2]*SmQ[2]*( SmI[1] - x )/( 1 + 2*SmQ[2] )) ) )^( 1 + 1/SmQ[2] ),

            ifelse( x < SmI[3], (pi*SmH[1]*( 2*SmQ[1] + 1 )*2^( 2 + 1/SmQ[1] ))/( 1 + 2*SmQ[1] )*exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] ))*( SmH[1]/SmK[1]*( 1 - exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] )) ) )^( 1 + 1/SmQ[1] ) + 
                               (pi*SmH[2]*( 2*SmQ[2] + 1 )*2^( 2 + 1/SmQ[2] ))/( 1 + 2*SmQ[2] )*exp(SmK[2]*SmQ[2]*( SmI[1] - x )/( 1 + 2*SmQ[2] ))*( SmH[2]/SmK[2]*( 1 - exp(SmK[2]*SmQ[2]*( SmI[1] - x )/( 1 + 2*SmQ[2] )) ) )^( 1 + 1/SmQ[2] ) +
                               (pi*SmH[3]*( 2*SmQ[3] + 1 )*2^( 2 + 1/SmQ[3] ))/( 1 + 2*SmQ[3] )*exp(SmK[3]*SmQ[3]*( SmI[2] - x )/( 1 + 2*SmQ[3] ))*( SmH[3]/SmK[3]*( 1 - exp(SmK[3]*SmQ[3]*( SmI[2] - x )/( 1 + 2*SmQ[3] )) ) )^( 1 + 1/SmQ[3] ),

            ifelse( x < SmI[4], (pi*SmH[1]*( 2*SmQ[1] + 1 )*2^( 2 + 1/SmQ[1] ))/( 1 + 2*SmQ[1] )*exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] ))*( SmH[1]/SmK[1]*( 1 - exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] )) ) )^( 1 + 1/SmQ[1] ) + 
                               (pi*SmH[2]*( 2*SmQ[2] + 1 )*2^( 2 + 1/SmQ[2] ))/( 1 + 2*SmQ[2] )*exp(SmK[2]*SmQ[2]*( SmI[1] - x )/( 1 + 2*SmQ[2] ))*( SmH[2]/SmK[2]*( 1 - exp(SmK[2]*SmQ[2]*( SmI[1] - x )/( 1 + 2*SmQ[2] )) ) )^( 1 + 1/SmQ[2] ) +
                               (pi*SmH[3]*( 2*SmQ[3] + 1 )*2^( 2 + 1/SmQ[3] ))/( 1 + 2*SmQ[3] )*exp(SmK[3]*SmQ[3]*( SmI[2] - x )/( 1 + 2*SmQ[3] ))*( SmH[3]/SmK[3]*( 1 - exp(SmK[3]*SmQ[3]*( SmI[2] - x )/( 1 + 2*SmQ[3] )) ) )^( 1 + 1/SmQ[3] ) +
                               (pi*SmH[4]*( 2*SmQ[4] + 1 )*2^( 2 + 1/SmQ[4] ))/( 1 + 2*SmQ[4] )*exp(SmK[4]*SmQ[4]*( SmI[3] - x )/( 1 + 2*SmQ[4] ))*( SmH[4]/SmK[4]*( 1 - exp(SmK[4]*SmQ[4]*( SmI[3] - x )/( 1 + 2*SmQ[4] )) ) )^( 1 + 1/SmQ[4] ),

                              (pi*SmH[1]*( 2*SmQ[1] + 1 )*2^( 2 + 1/SmQ[1] ))/( 1 + 2*SmQ[1] )*exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] ))*( SmH[1]/SmK[1]*( 1 - exp(SmK[1]*SmQ[1]*( 0     - x )/( 1 + 2*SmQ[1] )) ) )^( 1 + 1/SmQ[1] ) + 
                              (pi*SmH[2]*( 2*SmQ[2] + 1 )*2^( 2 + 1/SmQ[2] ))/( 1 + 2*SmQ[2] )*exp(SmK[2]*SmQ[2]*( SmI[1] - x )/( 1 + 2*SmQ[2] ))*( SmH[2]/SmK[2]*( 1 - exp(SmK[2]*SmQ[2]*( SmI[1] - x )/( 1 + 2*SmQ[2] )) ) )^( 1 + 1/SmQ[2] ) +
                              (pi*SmH[3]*( 2*SmQ[3] + 1 )*2^( 2 + 1/SmQ[3] ))/( 1 + 2*SmQ[3] )*exp(SmK[3]*SmQ[3]*( SmI[2] - x )/( 1 + 2*SmQ[3] ))*( SmH[3]/SmK[3]*( 1 - exp(SmK[3]*SmQ[3]*( SmI[2] - x )/( 1 + 2*SmQ[3] )) ) )^( 1 + 1/SmQ[3] ) +
                              (pi*SmH[4]*( 2*SmQ[4] + 1 )*2^( 2 + 1/SmQ[4] ))/( 1 + 2*SmQ[4] )*exp(SmK[4]*SmQ[4]*( SmI[3] - x )/( 1 + 2*SmQ[4] ))*( SmH[4]/SmK[4]*( 1 - exp(SmK[4]*SmQ[4]*( SmI[3] - x )/( 1 + 2*SmQ[4] )) ) )^( 1 + 1/SmQ[4] ) +
                              (pi*SmH[5]*( 2*SmQ[5] + 1 )*2^( 2 + 1/SmQ[5] ))/( 1 + 2*SmQ[5] )*exp(SmK[5]*SmQ[5]*( SmI[4] - x )/( 1 + 2*SmQ[5] ))*( SmH[5]/SmK[5]*( 1 - exp(SmK[5]*SmQ[5]*( SmI[4] - x )/( 1 + 2*SmQ[5] )) ) )^( 1 + 1/SmQ[5] )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)




  # new plot for figure labels 
  par(new=TRUE) #add to existing plot
  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  par(xpd=NA) # clip plotting to device region

  text("Growth velocity (g/year)", x=32, y=8, srt=270, las=3, cex=1.8)
  text("Skeletal cell weight (g)", x=-6.5, y=8, srt=90, las=3, cex=1.8)
  text("Age since conception (years)", x=13, y=-4, srt=0, las=1, cex=1.8) # las 1 horizonital, 3 vertical


graphics.off()









