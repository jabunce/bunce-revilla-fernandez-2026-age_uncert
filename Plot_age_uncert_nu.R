

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
BerPoint_col <- "black" #colorlist["2.3"]
BerPoint_cex <- 0.75
BerPoint_pch <- 16 #1

MatPoint_lwd <- 1
MatPoint_col <- "blue" #colorlist["1.2"]
MatPoint_cex <- 0.75
MatPoint_pch <- 1


BerIndivTraj_lwd <- 0.25
BerIndivTraj_col <- colorlist["2.4"]
BerIndivTraj_lty <- 1

MatIndivTraj_lwd <- 0.25
MatIndivTraj_col <- grey(0.8) #colorlist["1.4"]
MatIndivTraj_lty <- 1


BerMeanTraj_lwd <- 3
BerMeanTraj_col <- "blue" # colorlist["2.1"]
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






pdf(file="./Plots/Sim_age_uncert_fh_nu.pdf",
    height=10, width=10)
par(mfcol = c(2, 2)) # mfcol fills by column, mfrow fills by row 
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



  #### x-sectional, 100 indivs

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,180), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels = TRUE )
  axis( side=2, at=seq( 0, 180, by=30 ), labels = TRUE )


  paramList <- readRDS(parlist_name2)

  Nsims <- 100   # number of trajectories to simulate

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


  # plot simulated individual trajectories
  for ( z in 1:Nsims ){

    sQ1[z] <- paramList[[5]][z,1] #sQ[j,1]
    sQ2[z] <- paramList[[5]][z,2]
    sQ3[z] <- paramList[[5]][z,3]
    sQ4[z] <- paramList[[5]][z,4]
    sQ5[z] <- paramList[[5]][z,5]

    sK1[z] <- paramList[[6]][z,1] #sK[j,1]
    sK2[z] <- paramList[[6]][z,2]
    sK3[z] <- paramList[[6]][z,3]
    sK4[z] <- paramList[[6]][z,4]
    sK5[z] <- paramList[[6]][z,5]

    sH1[z] <- paramList[[7]][z,1] #sH[j,1]
    sH2[z] <- paramList[[7]][z,2]
    sH3[z] <- paramList[[7]][z,3]
    sH4[z] <- paramList[[7]][z,4]
    sH5[z] <- paramList[[7]][z,5]

    sI1[z] <- 0.00000001
    sI2[z] <- paramList[[8]][z,1] #sI[j,1]
    sI3[z] <- paramList[[8]][z,2]
    sI4[z] <- paramList[[8]][z,3]
    sI5[z] <- paramList[[8]][z,4]
    sIM[z] <- paramList[[8]][z,5]


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
          col=grey(0.8), lwd=0.25, lty=1)

  } #for z


  # Matsigenka group mean trajectory

  SmuQ1 <- paramList[[1]][1] #SmQ[1] #0.08454261
  SmuQ2 <- paramList[[1]][2] 
  SmuQ3 <- paramList[[1]][3]
  SmuQ4 <- paramList[[1]][4] 
  SmuQ5 <- paramList[[1]][5] 

  SmuK1 <- paramList[[2]][1] #SmK[1] #81.9413 
  SmuK2 <- paramList[[2]][2] #SmK[2] #12.56496 
  SmuK3 <- paramList[[2]][3]
  SmuK4 <- paramList[[2]][4] 
  SmuK5 <- paramList[[2]][5] 

  SmuH1 <- paramList[[3]][1] #SmH[1] #57.61856 
  SmuH2 <- paramList[[3]][2]
  SmuH3 <- paramList[[3]][3] 
  SmuH4 <- paramList[[3]][4]
  SmuH5 <- paramList[[3]][5] 

  SmuI1 <- 0
  SmuI2 <- paramList[[4]][1] #SmI[1] #0.1218582 
  SmuI3 <- paramList[[4]][2] 
  SmuI4 <- paramList[[4]][3] 
  SmuI5 <- paramList[[4]][4] 
  SmuIM <- paramList[[4]][5] # max age for start of last process

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
  
 


  # plot real age points

  Com.fem <- readRDS(comfem_name2)
  
  N <- nrow(Com.fem)                    # number of observations
  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  for ( n in 1:N ){
      if ( Com.fem$Ethnicity[n] == 2 && Com.fem$firstObs[n] != -1 ) {            # if simulated data and not conception
        points( x=Com.fem$Age[n],
                y=Com.fem$Height[n],
                lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=16 )
      } # if
  } # for N




  # plot observed age points
  
  N <- nrow(Com.fem)                    # number of observations
  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  for ( n in 1:N ){
      if ( Com.fem$Ethnicity[n] == 2 && Com.fem$firstObs[n] != -1 ) {            # if simulated data and not conception
        points( x=(Com.fem$interview_year[n] - Com.fem$ConcepYear[n]),
                y=Com.fem$Height[n],
                lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch )

             lines( x=c( Com.fem$Age[n], (Com.fem$interview_year[n] - Com.fem$ConcepYear[n]) ), # link the points
                    y=c(Com.fem$Height[n], Com.fem$Height[n]), lwd=0.5, lty=1, col="black")
      } # if
  } # for N




  # new plot for figure labels 
  par(new=TRUE) #add to existing plot
  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  par(xpd=NA) # clip plotting to device region

  text("Age since conception (years)", x=13, y=-4, srt=0, las=1, cex=1.8) # las 1 horizonital, 3 vertical
  text("Height (cm)", x=-6.5, y=8, srt=90, las=3, cex=1.8)






###


#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region

# legend
  # set horizontal spacing for legend text

  legtext <- c("Sim data",
               "", #"Observed data", 
               "Observed data"#,
               #"Prior trajectory"
               )
  xcoords <- c(0,
               1,#1.7, #1.5 #4.5,  #4 #8.5 # moves third item right
               1 #4.6 #4 #8.5#, #7.5 #24.2 # moves last item right
               #15  #36.8 # moves last three items together right
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 1 #2 # moves last three items right  # so replace element 1 with a finite number (any will do)



  legend(x=-1.5, #-3.25, #-2.4
         y=12.4, #13.1,
         ncol=3,
         cex=1.3, #1.3
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(BerPoint_col,
               "white", #"red",
               MatPoint_col #, 
               #PriorTraj_col
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               MatPoint_pch, 
               MatPoint_pch 
               #NA
               ),
         lty=c(0,
               0,
               0 #MatIndivTraj_lty #,
               #PriorTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               MatPoint_lwd,
               MatPoint_lwd #,
               #PriorTraj_lwd
               ),
         seg.len=2 )


  legtext <-c("Sim mean trajectory",
              "Sim indiv trajectory" #,
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


  legend(x=-2.4, y=11.7,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatMeanTraj_col,
               MatIndivTraj_col #,
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
               MatIndivTraj_lty #,
               #MatIndivTraj_lty,
               #NA
               ),
         lwd=c(MatMeanTraj_lwd,
               MatIndivTraj_lwd + 1 #,
               #MatIndivTraj_lwd,
               #NA
               ),
         seg.len=2 )


  rect(xleft = -2.6,
       ybottom = 10.7,
       xright = 10.4,
       ytop = 12.2,
       lwd=1)




graphics.off()


rm(post)


