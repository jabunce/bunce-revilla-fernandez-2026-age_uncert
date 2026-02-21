

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
PriorTraj_lwd <- 3
PriorTraj_col <- colorlist["2.2"] #"dark red" #grey(0.7)
PriorTraj_lty <- 1

BerPoint_lwd <- 1.5 #0.5
BerPoint_col <- "black"#colorlist["2.3"]
BerPoint_cex <- 1 #0.5
BerPoint_pch <- 1

MatPoint_lwd <- 1.5 #0.5
MatPoint_col <- colorlist["1.2"]
MatPoint_cex <- 1 #0.5
MatPoint_pch <- 1


BerIndivTraj_lwd <- 1.5 #0.25
BerIndivTraj_col <- "black" #colorlist["2.4"]
BerIndivTraj_lty <- 1

MatIndivTraj_lwd <- 1.5 #0.25
MatIndivTraj_col <- colorlist["1.2"]
MatIndivTraj_lty <- 1


BerMeanTraj_lwd <- 3
BerMeanTraj_col <- "black" #colorlist["2.1"]
BerMeanTraj_lty <- 1

MatMeanTraj_lwd <- 3
MatMeanTraj_col <- colorlist["1.1"]
MatMeanTraj_lty <- 1


BerVelTraj_lwd <- 3
BerVelTraj_col <- colorlist2["2.2"]
BerVelTrajArea_col <- colorlist2["2.3"]
BerVelTraj_lty <- 1

MatVelTraj_lwd <- 1.5
MatVelTraj_col <- colorlist2["1.2"]
MatVelTrajArea_col <- colorlist2["1.4"]
MatVelTraj_lty <- 1



pdf(file="./Plots/Combined_plots_h_traj_100.pdf",
    height=20, width=10)
layout( mat=matrix( c(1,2,6,4,5,3,7,8), 4, 2, byrow = FALSE), heights=c(1,1), widths=c(1,1) )
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 5, 5)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age



################### 100 x-sectional data, estimated age

  runs <- 5
  post1 <- readRDS(modpost_name1)
  post2 <- readRDS(modpost_name2)
  post3 <- readRDS(modpost_name3)
  post4 <- readRDS(modpost_name4)
  post5 <- readRDS(modpost_name5)


  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,180), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=FALSE )
  axis( side=2, at=seq( 0, 180, by=30), labels=TRUE )
  par(xpd=FALSE) # clip to plot region




  # plot estimated age points

  Com.fem <- readRDS(comfem_name1)

  N <- nrow(Com.fem)                    # number of observations
  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  for ( n in 1:N ){
      if ( Com.fem$Ethnicity[n] == 2 && Com.fem$firstObs[n] != -1 ) {            # if simulated data and not conception
        points( x=(Com.fem$interview_year[n] - Com.fem$ConcepYear[n]),
                y=Com.fem$Height[n],
                lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch )
      } # if
  } # for N




  # prior
  # composite trajectory
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
          col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)



  # prior component functions
  # infant
  lines(x = x,
        y = ifelse( x > muI1, ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > muI2, ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > muI3, ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > muI4, ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > muI5, ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  

  # true trajectory

  # simulated group mean trajectory
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
          col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # true component functions
  # infant
  lines(x = x,
        y = ifelse( x > SmuI1, ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1), NA),
          col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > SmuI2, ( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > SmuI3, ( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > SmuI4, ( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > SmuI5, ( 2*SmuH5/SmuK5 * ( 1 - exp(SmuK5*SmuQ5*( SmuI5 - x )/( 1 + 2*SmuQ5 )) ) )^(1/SmuQ5), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


#p1 <- recordPlot()
#graphics.off()




  # Separate simulated trajectory runs

  mmQ <- matrix(0, nrow=runs, ncol=5) 
  mmK <- matrix(0, nrow=runs, ncol=5) 
  mmH <- matrix(0, nrow=runs, ncol=5) 
  mmI <- matrix(0, nrow=runs, ncol=5) 

  for ( y in 1:runs ){
    for ( z in 1:5 ){
      mmQ[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mQ[", 2, ",", z, "]", sep="")) ) 
      mmK[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mK[", 2, ",", z, "]", sep="")) )
      mmH[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mH[", 2, ",", z, "]", sep="")) )
      mmI[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mI[", 2, ",", z, "]", sep="")) )
    } # for z
  } # for y

  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  for ( r in 1:runs ){ 

    for ( z in 1:5 ){
      Q[z] <- mmQ[r,z]
      K[z] <- mmK[r,z]
      H[z] <- mmH[r,z]
      I[z] <- mmI[r,z]
    } # for z



    lines(x = x,
          y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

              ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

              ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

              ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                         ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                         ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                         ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)


    # plot individual component functions

    # infant
    lines(x = x,
          y = ifelse( x > 0,    ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 1
    lines(x = x,
          y = ifelse( x > I[1], ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 2
    lines(x = x,
          y = ifelse( x > I[2], ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 3
    lines(x = x,
          y = ifelse( x > I[3], ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # adolescent
    lines(x = x,
          y = ifelse( x > I[4], ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)


  } #for r



 text(3, 170, "100 individuals, est age", cex = 1.35, adj=0)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) # add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), labels=FALSE, las=0, srt=270 )
  text( x=28.75, y=seq( 0, 16, by=4 ), labels=seq( 0, 16, by=4 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE) # clip to plot region


  # Prior

  y_primeBer = ifelse( x <= muI2, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ),

               ifelse( x <= muI3, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ),

               ifelse( x <= muI4, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ),

               ifelse( x <= muI5, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ) +
                                  1/( 1 + 2*muQ4 )*2^(1/muQ4)*muH4*exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 ))*(muH4/muK4*( 1 - exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 )) ))^( 1/muQ4 - 1 ),

                                  1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ) +
                                  1/( 1 + 2*muQ4 )*2^(1/muQ4)*muH4*exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 ))*(muH4/muK4*( 1 - exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 )) ))^( 1/muQ4 - 1 ) +
                                  1/( 1 + 2*muQ5 )*2^(1/muQ5)*muH5*exp((muK5*muQ5*( muI5 - x ))/( 1 + 2*muQ5 ))*(muH5/muK5*( 1 - exp((muK5*muQ5*( muI5 - x ))/( 1 + 2*muQ5 )) ))^( 1/muQ5 - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)



  # real mean velocity 

  y_primeBer = ifelse( x <= SmuI2, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ),

               ifelse( x <= SmuI3, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ),

               ifelse( x <= SmuI4, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ),

               ifelse( x <= SmuI5, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ) +
                                  1/( 1 + 2*SmuQ4 )*2^(1/SmuQ4)*SmuH4*exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 ))*(SmuH4/SmuK4*( 1 - exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 )) ))^( 1/SmuQ4 - 1 ),

                                  1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ) +
                                  1/( 1 + 2*SmuQ4 )*2^(1/SmuQ4)*SmuH4*exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 ))*(SmuH4/SmuK4*( 1 - exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 )) ))^( 1/SmuQ4 - 1 ) +
                                  1/( 1 + 2*SmuQ5 )*2^(1/SmuQ5)*SmuH5*exp((SmuK5*SmuQ5*( SmuI5 - x ))/( 1 + 2*SmuQ5 ))*(SmuH5/SmuK5*( 1 - exp((SmuK5*SmuQ5*( SmuI5 - x ))/( 1 + 2*SmuQ5 )) ))^( 1/SmuQ5 - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeBer, col="black", lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)





  # Separate simulated velocity runs

  for ( r in 1:runs ){ 

    for ( z in 1:5 ){
      Q[z] <- mmQ[r,z]
      K[z] <- mmK[r,z]
      H[z] <- mmH[r,z]
      I[z] <- mmI[r,z]
    } # for z


  
    y_primeMat = ifelse( x <= I[1], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0    - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ),

                 ifelse( x <= I[2], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ),

                 ifelse( x <= I[3], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ),

                 ifelse( x <= I[4], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ) +
                                    1/( 1 + 2*Q[4] )*2^(1/Q[4])*H[4]*exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] ))*(H[4]/K[4]*( 1 - exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] )) ))^( 1/Q[4] - 1 ),

                                    1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ) +
                                    1/( 1 + 2*Q[4] )*2^(1/Q[4])*H[4]*exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] ))*(H[4]/K[4]*( 1 - exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] )) ))^( 1/Q[4] - 1 ) +
                                    1/( 1 + 2*Q[5] )*2^(1/Q[5])*H[5]*exp((K[5]*Q[5]*( I[4] - x ))/( 1 + 2*Q[5] ))*(H[5]/K[5]*( 1 - exp((K[5]*Q[5]*( I[4] - x ))/( 1 + 2*Q[5] )) ))^( 1/Q[5] - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)

  } #for r



 rm(post1, post2, post3, post4, post5) #(list = ls(all=TRUE))




#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region

# legend
    legtext <- c("Observed data",
               "Sim mean trajectory/velocity", 
               "Estimated mean trajectory",
               "Prior trajectory",
               "Estimated mean velocity",
               "Prior velocity"
               )

    colors <- c(MatPoint_col,
               BerMeanTraj_col,
               MatIndivTraj_col,
               PriorTraj_col,
               MatVelTraj_col,
               BerVelTraj_col
               )

    points <- c(MatPoint_pch,
                NA,
                NA,
                NA,
                NA,
                NA
               )

    ltype <- c(0,
               BerMeanTraj_lty,
               MatIndivTraj_lty,
               PriorTraj_lty,
               MatVelTraj_lty,
               BerVelTraj_lty
               )

    lwidth <- c(MatPoint_lwd,
                BerMeanTraj_lwd,
                MatIndivTraj_lwd,
                PriorTraj_lwd,
                MatVelTraj_lwd,
                BerVelTraj_lwd
                )


  legend(x=12, y=8.7,
         horiz=FALSE,
         ncol=1,
         cex=1.6,
         #text.width=textwidths,
         legend=legtext,
         bty="o",
         box.lty=1,
         box.lwd=1,
         box.col="black",
         #bg="white",
         col=colors,
         merge=FALSE,
         pch=points,
         lty=ltype,
         lwd=lwidth,
         seg.len=2 )



  #par(xpd=NA) # clip plotting to device region


  text("Age since conception (years)", x = 11, y = -13.5, cex = 2)
  text("Height (cm)", x = -2, y = -0.7, srt=90, cex = 2)
  text("Growth velocity (cm/year)", x = 23.5, y = -0.7, srt=270, cex = 2)









################### 100 x-sectional data, estimated age, no weight

  runs <- 5
  post1 <- readRDS(modpost_name6)
  post2 <- readRDS(modpost_name7)
  post3 <- readRDS(modpost_name8)
  post4 <- readRDS(modpost_name9)
  post5 <- readRDS(modpost_name10)


  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,180), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 180, by=30), labels=TRUE )
  par(xpd=FALSE) # clip to plot region



  # plot observed data

  Com.fem <- readRDS(comfem_name2)
  
  N <- nrow(Com.fem)                    # number of observations
  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  for ( n in 1:N ){
      if ( Com.fem$Ethnicity[n] == 2 && Com.fem$firstObs[n] != -1 ) {            # if simulated data and not conception
        points( x=(Com.fem$interview_year[n] - Com.fem$ConcepYear[n]),
                y=Com.fem$Height[n],
                lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch )
      } # if
  } # for N




  # prior
  # composite trajectory
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
          col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)



  # prior component functions
  # infant
  lines(x = x,
        y = ifelse( x > muI1, ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > muI2, ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > muI3, ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > muI4, ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > muI5, ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  

  # true trajectory

  # simulated group mean trajectory
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
          col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # true component functions
  # infant
  lines(x = x,
        y = ifelse( x > SmuI1, ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1), NA),
          col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > SmuI2, ( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > SmuI3, ( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > SmuI4, ( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > SmuI5, ( 2*SmuH5/SmuK5 * ( 1 - exp(SmuK5*SmuQ5*( SmuI5 - x )/( 1 + 2*SmuQ5 )) ) )^(1/SmuQ5), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


#p1 <- recordPlot()
#graphics.off()


  # Separate simulated trajectory runs

  mmQ <- matrix(0, nrow=runs, ncol=5) 
  mmK <- matrix(0, nrow=runs, ncol=5) 
  mmH <- matrix(0, nrow=runs, ncol=5) 
  mmI <- matrix(0, nrow=runs, ncol=5) 

  for ( y in 1:runs ){
    for ( z in 1:5 ){
      mmQ[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mQ[", 2, ",", z, "]", sep="")) ) 
      mmK[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mK[", 2, ",", z, "]", sep="")) )
      mmH[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mH[", 2, ",", z, "]", sep="")) )
      mmI[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mI[", 2, ",", z, "]", sep="")) )
    } # for z
  } # for y

  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  for ( r in 1:runs ){ 

    for ( z in 1:5 ){
      Q[z] <- mmQ[r,z]
      K[z] <- mmK[r,z]
      H[z] <- mmH[r,z]
      I[z] <- mmI[r,z]
    } # for z



    lines(x = x,
          y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

              ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

              ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

              ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                         ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                         ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                         ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)



    # plot individual component functions

    # infant
    lines(x = x,
          y = ifelse( x > 0,    ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 1
    lines(x = x,
          y = ifelse( x > I[1], ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 2
    lines(x = x,
          y = ifelse( x > I[2], ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 3
    lines(x = x,
          y = ifelse( x > I[3], ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # adolescent
    lines(x = x,
          y = ifelse( x > I[4], ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

  } #for r



 text(3, 170, "100 individuals, est age, no weight", cex = 1.35, adj=0)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) # add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), labels=FALSE, las=0, srt=270 )
  #text( x=28.75, y=seq( 0, 16, by=4 ), labels=seq( 0, 16, by=4 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE) # clip to plot region


  # Prior

  y_primeBer = ifelse( x <= muI2, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ),

               ifelse( x <= muI3, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ),

               ifelse( x <= muI4, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ),

               ifelse( x <= muI5, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ) +
                                  1/( 1 + 2*muQ4 )*2^(1/muQ4)*muH4*exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 ))*(muH4/muK4*( 1 - exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 )) ))^( 1/muQ4 - 1 ),

                                  1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ) +
                                  1/( 1 + 2*muQ4 )*2^(1/muQ4)*muH4*exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 ))*(muH4/muK4*( 1 - exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 )) ))^( 1/muQ4 - 1 ) +
                                  1/( 1 + 2*muQ5 )*2^(1/muQ5)*muH5*exp((muK5*muQ5*( muI5 - x ))/( 1 + 2*muQ5 ))*(muH5/muK5*( 1 - exp((muK5*muQ5*( muI5 - x ))/( 1 + 2*muQ5 )) ))^( 1/muQ5 - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)




  # real mean velocity 

  y_primeBer = ifelse( x <= SmuI2, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ),

               ifelse( x <= SmuI3, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ),

               ifelse( x <= SmuI4, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ),

               ifelse( x <= SmuI5, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ) +
                                  1/( 1 + 2*SmuQ4 )*2^(1/SmuQ4)*SmuH4*exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 ))*(SmuH4/SmuK4*( 1 - exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 )) ))^( 1/SmuQ4 - 1 ),

                                  1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ) +
                                  1/( 1 + 2*SmuQ4 )*2^(1/SmuQ4)*SmuH4*exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 ))*(SmuH4/SmuK4*( 1 - exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 )) ))^( 1/SmuQ4 - 1 ) +
                                  1/( 1 + 2*SmuQ5 )*2^(1/SmuQ5)*SmuH5*exp((SmuK5*SmuQ5*( SmuI5 - x ))/( 1 + 2*SmuQ5 ))*(SmuH5/SmuK5*( 1 - exp((SmuK5*SmuQ5*( SmuI5 - x ))/( 1 + 2*SmuQ5 )) ))^( 1/SmuQ5 - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeBer, col="black", lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)





  # Separate simulated velocity runs

  for ( r in 1:runs ){ 

    for ( z in 1:5 ){
      Q[z] <- mmQ[r,z]
      K[z] <- mmK[r,z]
      H[z] <- mmH[r,z]
      I[z] <- mmI[r,z]
    } # for z


  
    y_primeMat = ifelse( x <= I[1], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0    - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ),

                 ifelse( x <= I[2], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ),

                 ifelse( x <= I[3], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ),

                 ifelse( x <= I[4], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ) +
                                    1/( 1 + 2*Q[4] )*2^(1/Q[4])*H[4]*exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] ))*(H[4]/K[4]*( 1 - exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] )) ))^( 1/Q[4] - 1 ),

                                    1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ) +
                                    1/( 1 + 2*Q[4] )*2^(1/Q[4])*H[4]*exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] ))*(H[4]/K[4]*( 1 - exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] )) ))^( 1/Q[4] - 1 ) +
                                    1/( 1 + 2*Q[5] )*2^(1/Q[5])*H[5]*exp((K[5]*Q[5]*( I[4] - x ))/( 1 + 2*Q[5] ))*(H[5]/K[5]*( 1 - exp((K[5]*Q[5]*( I[4] - x ))/( 1 + 2*Q[5] )) ))^( 1/Q[5] - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)

  } #for r



 rm(post1, post2, post3, post4, post5) #(list = ls(all=TRUE))






################### 50 sparse data, estimated age

  runs <- 5
  post1 <- readRDS(modpost_name11)
  post2 <- readRDS(modpost_name12)
  post3 <- readRDS(modpost_name13)
  post4 <- readRDS(modpost_name14)
  post5 <- readRDS(modpost_name15)

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,180), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 180, by=30), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


  # plot observed data

  Com.fem <- readRDS(comfem_name3)
  
  Mat.fem <- Com.fem[which(Com.fem$Ethnicity == 2 & Com.fem$Age != 0),] # pull out Matsigenks measures, not conception
  Mat.fem.PID <- unique(Mat.fem$PID)
  Mat.fem.sort <- Mat.fem[order(Mat.fem$PID, Mat.fem$Age),] # order by ID then by age
  Mat.fem.sort$ObsAge <- Mat.fem.sort$interview_year - Mat.fem.sort$ConcepYear # add observed age column

  for ( i in 1:length(Mat.fem.PID) ) {

    lines(x=Mat.fem.sort[which(Mat.fem.sort$PID == Mat.fem.PID[i]),"ObsAge"],
          y=Mat.fem.sort[which(Mat.fem.sort$PID == Mat.fem.PID[i]),"Height"],
          col="black", lwd=0.5, type="l")

  } # for

  points( x=Mat.fem.sort$ObsAge,
          y=Mat.fem.sort$Height,
          lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch )




  # prior
  # composite trajectory
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
          col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)



  # prior component functions
  # infant
  lines(x = x,
        y = ifelse( x > muI1, ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > muI2, ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > muI3, ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > muI4, ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > muI5, ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5), NA),
        col=PriorTraj_col, lwd=PriorTraj_lwd, lty=PriorTraj_lty)

  

  # true trajectory

  # simulated group mean trajectory
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
          col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # true component functions
  # infant
  lines(x = x,
        y = ifelse( x > SmuI1, ( 2*SmuH1/SmuK1 * ( 1 - exp(SmuK1*SmuQ1*( SmuI1 - x )/( 1 + 2*SmuQ1 )) ) )^(1/SmuQ1), NA),
          col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > SmuI2, ( 2*SmuH2/SmuK2 * ( 1 - exp(SmuK2*SmuQ2*( SmuI2 - x )/( 1 + 2*SmuQ2 )) ) )^(1/SmuQ2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > SmuI3, ( 2*SmuH3/SmuK3 * ( 1 - exp(SmuK3*SmuQ3*( SmuI3 - x )/( 1 + 2*SmuQ3 )) ) )^(1/SmuQ3), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > SmuI4, ( 2*SmuH4/SmuK4 * ( 1 - exp(SmuK4*SmuQ4*( SmuI4 - x )/( 1 + 2*SmuQ4 )) ) )^(1/SmuQ4), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > SmuI5, ( 2*SmuH5/SmuK5 * ( 1 - exp(SmuK5*SmuQ5*( SmuI5 - x )/( 1 + 2*SmuQ5 )) ) )^(1/SmuQ5), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


#p1 <- recordPlot()
#graphics.off()


  # Separate simulated trajectory runs

  mmQ <- matrix(0, nrow=runs, ncol=5) 
  mmK <- matrix(0, nrow=runs, ncol=5) 
  mmH <- matrix(0, nrow=runs, ncol=5) 
  mmI <- matrix(0, nrow=runs, ncol=5) 

  for ( y in 1:runs ){
    for ( z in 1:5 ){
      mmQ[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mQ[", 2, ",", z, "]", sep="")) ) 
      mmK[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mK[", 2, ",", z, "]", sep="")) )
      mmH[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mH[", 2, ",", z, "]", sep="")) )
      mmI[y,z] <- mean( pull(eval(as.symbol(paste("post", y, sep=""))), paste("mI[", 2, ",", z, "]", sep="")) )
    } # for z
  } # for y

  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  for ( r in 1:runs ){ 

    for ( z in 1:5 ){
      Q[z] <- mmQ[r,z]
      K[z] <- mmK[r,z]
      H[z] <- mmH[r,z]
      I[z] <- mmI[r,z]
    } # for z



    lines(x = x,
          y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

              ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

              ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

              ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                         ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                         ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                         ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                         ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                         ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)



    # plot individual component functions

    # infant
    lines(x = x,
          y = ifelse( x > 0,    ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 1
    lines(x = x,
          y = ifelse( x > I[1], ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 2
    lines(x = x,
          y = ifelse( x > I[2], ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # child 3
    lines(x = x,
          y = ifelse( x > I[3], ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    # adolescent
    lines(x = x,
          y = ifelse( x > I[4], ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5]), NA),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

  } #for r




 text(3, 170, "50 individuals, measured twice, est age", cex = 1.35, adj=0)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) # add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), labels=FALSE, las=0, srt=270 )
  text( x=28.75, y=seq( 0, 16, by=4 ), labels=seq( 0, 16, by=4 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE) # clip to plot region


  # Prior

  y_primeBer = ifelse( x <= muI2, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ),

               ifelse( x <= muI3, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ),

               ifelse( x <= muI4, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ),

               ifelse( x <= muI5, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ) +
                                  1/( 1 + 2*muQ4 )*2^(1/muQ4)*muH4*exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 ))*(muH4/muK4*( 1 - exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 )) ))^( 1/muQ4 - 1 ),

                                  1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( muI1 - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                                  1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI2 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                                  1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI3 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ) +
                                  1/( 1 + 2*muQ4 )*2^(1/muQ4)*muH4*exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 ))*(muH4/muK4*( 1 - exp((muK4*muQ4*( muI4 - x ))/( 1 + 2*muQ4 )) ))^( 1/muQ4 - 1 ) +
                                  1/( 1 + 2*muQ5 )*2^(1/muQ5)*muH5*exp((muK5*muQ5*( muI5 - x ))/( 1 + 2*muQ5 ))*(muH5/muK5*( 1 - exp((muK5*muQ5*( muI5 - x ))/( 1 + 2*muQ5 )) ))^( 1/muQ5 - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)





  # real mean velocity 

  y_primeBer = ifelse( x <= SmuI2, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ),

               ifelse( x <= SmuI3, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ),

               ifelse( x <= SmuI4, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ),

               ifelse( x <= SmuI5, 1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ) +
                                  1/( 1 + 2*SmuQ4 )*2^(1/SmuQ4)*SmuH4*exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 ))*(SmuH4/SmuK4*( 1 - exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 )) ))^( 1/SmuQ4 - 1 ),

                                  1/( 1 + 2*SmuQ1 )*2^(1/SmuQ1)*SmuH1*exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 ))*(SmuH1/SmuK1*( 1 - exp((SmuK1*SmuQ1*( SmuI1 - x ))/( 1 + 2*SmuQ1 )) ))^( 1/SmuQ1 - 1 ) +
                                  1/( 1 + 2*SmuQ2 )*2^(1/SmuQ2)*SmuH2*exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 ))*(SmuH2/SmuK2*( 1 - exp((SmuK2*SmuQ2*( SmuI2 - x ))/( 1 + 2*SmuQ2 )) ))^( 1/SmuQ2 - 1 ) +
                                  1/( 1 + 2*SmuQ3 )*2^(1/SmuQ3)*SmuH3*exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 ))*(SmuH3/SmuK3*( 1 - exp((SmuK3*SmuQ3*( SmuI3 - x ))/( 1 + 2*SmuQ3 )) ))^( 1/SmuQ3 - 1 ) +
                                  1/( 1 + 2*SmuQ4 )*2^(1/SmuQ4)*SmuH4*exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 ))*(SmuH4/SmuK4*( 1 - exp((SmuK4*SmuQ4*( SmuI4 - x ))/( 1 + 2*SmuQ4 )) ))^( 1/SmuQ4 - 1 ) +
                                  1/( 1 + 2*SmuQ5 )*2^(1/SmuQ5)*SmuH5*exp((SmuK5*SmuQ5*( SmuI5 - x ))/( 1 + 2*SmuQ5 ))*(SmuH5/SmuK5*( 1 - exp((SmuK5*SmuQ5*( SmuI5 - x ))/( 1 + 2*SmuQ5 )) ))^( 1/SmuQ5 - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeBer, col="black", lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)






  # Separate simulated velocity runs

  for ( r in 1:runs ){ 

    for ( z in 1:5 ){
      Q[z] <- mmQ[r,z]
      K[z] <- mmK[r,z]
      H[z] <- mmH[r,z]
      I[z] <- mmI[r,z]
    } # for z


  
    y_primeMat = ifelse( x <= I[1], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0    - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ),

                 ifelse( x <= I[2], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ),

                 ifelse( x <= I[3], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ),

                 ifelse( x <= I[4], 1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ) +
                                    1/( 1 + 2*Q[4] )*2^(1/Q[4])*H[4]*exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] ))*(H[4]/K[4]*( 1 - exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] )) ))^( 1/Q[4] - 1 ),

                                    1/( 1 + 2*Q[1] )*2^(1/Q[1])*H[1]*exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] ))*(H[1]/K[1]*( 1 - exp((K[1]*Q[1]*( 0     - x ))/( 1 + 2*Q[1] )) ))^( 1/Q[1] - 1 ) +
                                    1/( 1 + 2*Q[2] )*2^(1/Q[2])*H[2]*exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] ))*(H[2]/K[2]*( 1 - exp((K[2]*Q[2]*( I[1] - x ))/( 1 + 2*Q[2] )) ))^( 1/Q[2] - 1 ) +
                                    1/( 1 + 2*Q[3] )*2^(1/Q[3])*H[3]*exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] ))*(H[3]/K[3]*( 1 - exp((K[3]*Q[3]*( I[2] - x ))/( 1 + 2*Q[3] )) ))^( 1/Q[3] - 1 ) +
                                    1/( 1 + 2*Q[4] )*2^(1/Q[4])*H[4]*exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] ))*(H[4]/K[4]*( 1 - exp((K[4]*Q[4]*( I[3] - x ))/( 1 + 2*Q[4] )) ))^( 1/Q[4] - 1 ) +
                                    1/( 1 + 2*Q[5] )*2^(1/Q[5])*H[5]*exp((K[5]*Q[5]*( I[4] - x ))/( 1 + 2*Q[5] ))*(H[5]/K[5]*( 1 - exp((K[5]*Q[5]*( I[4] - x ))/( 1 + 2*Q[5] )) ))^( 1/Q[5] - 1 )
               ) ) ) )                
    lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)

  } #for r



 rm(post1, post2, post3, post4, post5) #(list = ls(all=TRUE))




graphics.off()





