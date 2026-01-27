############### Berkeley data
# Age in years, height in cm, weight in kg


Ber.raw <- read.csv(file="./Data/Berkeley.csv", header=TRUE)

names(Ber.raw)
dim(Ber.raw)


#### Create categorical variable for sex and ethnicity, and extract useful columns

Ber.clean <- Ber.raw
names(Ber.clean)[1] <- "PID"
Ber.clean$Sex.1m <- revalue(Ber.clean$Sex, c("male"="1", "female"="2"))
Ber.clean$Ethnicity <- 1
names(Ber.clean)
Ber.clean = Ber.clean [, c(1,13,12,3,8,5,6,7,9,10)]
Ber.clean[1:10,]
names(Ber.clean)
dim(Ber.clean)


#### Remove all people with any NAs

Ber.clean <- Ber.clean[which(Ber.clean$height_cm != "NA" &
                             Ber.clean$weight_kg != "NA" &
                             Ber.clean$w_underline == "FALSE" &
                             Ber.clean$w_parentheses == "FALSE" &
                             Ber.clean$h_underline == "FALSE" &
                             Ber.clean$h_parentheses == "FALSE"), ]
names(Ber.clean)[4:6] <- c("Age","Height","Weight")
Ber.clean = Ber.clean[, 1:6]
Ber.clean[1:10,]
dim(Ber.clean)


# #plot Berkeley indiv height trajectories
# pdf(file="./Plots/Berkeley_indiv_height_traj.pdf",
#   height=10, width=12)
# par(mfrow=c(2,2), oma=c(5,7,5,5), mar=c(3,2,2,2))

# for ( Indiv in 1:length(unique(Ber.clean$PID)) ) {
#   plot( x=Ber.clean[which( Ber.clean$PID == unique(Ber.clean$PID)[Indiv] ), "Age"],
#         y=Ber.clean[which( Ber.clean$PID == unique(Ber.clean$PID)[Indiv] ), "Height"],
#         type="b",
#       col="red", ylim=c(0,200), xlim=c(-1,25), 
#       ylab="",
#       xlab="",
#       main=c(unique(Ber.clean$PID)[Indiv]),
#       cex.main=1.2,
#       cex.axis=1.2
#       #xaxp=c(-2, 2, 2),
#       #yaxp=c(0, 1, 2)
#       )
# } #for
# mtext(text="Height (cm)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
# mtext(text="Age since birth (years)", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
# graphics.off()

# Remove height outliers (probable measurement error)
Ber.clean[which(Ber.clean$PID == 304),]
Ber.clean[which(Ber.clean$PID == 304 & Ber.clean$Age == 5), "Height"] <- NA
Ber.clean[which(Ber.clean$PID == 304),]


# #plot Berkeley indiv weight trajectories
# pdf(file="./Plots/Berkeley_indiv_weight_traj.pdf", 
#   height=10, width=12)
# par(mfrow=c(2,2), oma=c(5,7,5,5), mar=c(3,2,2,2))

# for ( Indiv in 1:length(unique(Ber.clean$PID)) ) {
#   plot( x=Ber.clean[which( Ber.clean$PID == unique(Ber.clean$PID)[Indiv] ), "Age"],
#         y=Ber.clean[which( Ber.clean$PID == unique(Ber.clean$PID)[Indiv] ), "Weight"],
#         type="b",
#       col="red", ylim=c(0,130), xlim=c(-1,25), 
#       ylab="",
#       xlab="",
#       main=c(unique(Ber.clean$PID)[Indiv]),
#       cex.main=1.2,
#       cex.axis=1.2
#       #xaxp=c(-2, 2, 2),
#       #yaxp=c(0, 1, 2)
#       )
# } #for
# mtext(text="Weight (kg)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
# mtext(text="Age since birth (years)", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
# graphics.off()

Ber.clean <- Ber.clean[complete.cases(Ber.clean), ]

# add columns for age uncertainty
Ber.clean$interview_year <- Ber.clean$Age + 1928 # + 0.75 #1
Ber.clean$BirthYear <- 1928
Ber.clean$BirthYearUp <- 1928 # 0.75 #-1
Ber.clean$BirthYearLo <- 1928 # 0.75 #-1
#Ber.clean$firstObs <- -1

# make column marking first observation
Ber.clean <- Ber.clean[order(Ber.clean$PID, Ber.clean$interview_year),] #order by PID, then by interview year
Ber.clean$firstObs <- 1


for ( x in 1:( nrow(Ber.clean) - 1 ) ) {
  if ( Ber.clean$PID[x] == Ber.clean$PID[x+1] & 
       Ber.clean$interview_year[x] < Ber.clean$interview_year[x+1] ) {

       Ber.clean$firstObs[x+1] <- 0
  } #if

} #for x

Ber.clean$firstObs
Ber.clean[1:30,]


###################################################################

# Simulated data

#SimData <- xSimData # which simulated dataset

Mat.clean <- cbind( SimData[,1] + 3000,        # to keep PIDs different from Berkeley
                   2, # Ethnicity, Berkeley=1
                   2, # Sex.1m
                   SimData[,2:5],
                   SimData[,10],
                   SimData[,7:9] )

names(Mat.clean) <- c("PID", "Ethnicity", "Sex.1m", "Age", "Height", "Weight", "interview_year", "BirthYear", "BirthYearUp", "BirthYearLo", "firstObs")

#### Make height, weight, age, ethnicity, and years numeric
Mat.clean$Height <- as.numeric(Mat.clean$Height)
Mat.clean$Weight <- as.numeric(Mat.clean$Weight)
Mat.clean$Ethnicity <- as.numeric(Mat.clean$Ethnicity)
Mat.clean$Age <- as.numeric(Mat.clean$Age)
Mat.clean$interview_year <- as.numeric(Mat.clean$interview_year)
Mat.clean$BirthYear <- as.numeric(Mat.clean$BirthYear)
Mat.clean$BirthYearUp <- as.numeric(Mat.clean$BirthYearUp)
Mat.clean$BirthYearLo <- as.numeric(Mat.clean$BirthYearLo)



#### Add column for weight of cells contributing to longitudinal growth
Ber.clean$Weightg <- 1000*Ber.clean$Weight
Ber.clean$CellWeightg <- 0.01*(5 + 95*exp(-4*(Ber.clean$Age + 0.75)))*Ber.clean$Weightg

Mat.clean$Weightg <- 1000
Mat.clean$CellWeightg <- Mat.clean$Weight # simulated weight is the skeletal cell weight in grams




#### Keep only people younger than or equal to 25 yrs

Mat.sub <- Mat.clean[which(Mat.clean$Age <= 25),]
dim(Mat.sub)


##### Extend age range to 25 by duplicating last height and weight entries

Ber.exp <- Ber.clean
for ( x in 1:8 ) { # 18 years + 8 = 26
  for ( y in 1:length(unique(Ber.exp$PID)) ) {
    Ber.exp[nrow(Ber.exp)+1,] <- c( unique(Ber.exp$PID)[y],
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Ethnicity"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Sex.1m"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Age"],1)) + 1,
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Height"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Weight"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"interview_year"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"BirthYear"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"BirthYearUp"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"BirthYearLo"],1)),
                                    0, #firstObs
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Weightg"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"CellWeightg"],1))
                                   )
  } # for y
} # for x

Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[6]),]



##### Set height and weight to ~0 at ~conception (0.75 years before birth)
for ( y in 1:length(unique(Ber.exp$PID)) ) {
  Ber.exp[nrow(Ber.exp)+1,] <- c( unique(Ber.exp$PID)[y],
                                  as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Ethnicity"],1)),
                                  as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Sex.1m"],1)),
                                  -0.75,        # Age
                                  0.012,        # height
                                  1.02e-9,      # weight in Kg
                                  as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"BirthYear"],1)), # - 0.74999, #interview year = year of conception
                                  as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"BirthYear"],1)),
                                  as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"BirthYearUp"],1)),
                                  as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"BirthYearLo"],1)),
                                  -1, #firstObs
                                  1.02e-6,      # weight in g
                                  1.02e-6       # cell weight in g
                                 )
} # for y

Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[6]),]



Mat.exp <- Mat.sub          # in simulated dataset, age is already measured since conception 
for ( y in 1:length(unique(Mat.exp$PID)) ) {
  Mat.exp[nrow(Mat.exp)+1,] <- c( unique(Mat.exp$PID)[y],
                                  as.numeric(tail(Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[y]),"Ethnicity"],1)),
                                  as.numeric(tail(Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[y]),"Sex.1m"],1)),
                                  0,            # Age
                                  0.012,        # height
                                  1.02e-9,      # weight in Kg
                                  as.numeric(tail(Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[y]),"BirthYear"],1)), # - 0.74999, #interview year = year of conception, but must be a little bigger b/c year of conception will estimated from Normal with mean 0
                                  as.numeric(tail(Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[y]),"BirthYear"],1)),
                                  as.numeric(tail(Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[y]),"BirthYearUp"],1)),
                                  as.numeric(tail(Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[y]),"BirthYearLo"],1)),
                                  -1, #firstObs
                                  1.02e-6,      # weight in g
                                  1.02e-6       # cell weight in g
                                 )
} # for y

Mat.exp$Ethnicity <- as.numeric(Mat.exp$Ethnicity)
Mat.exp$Sex.1m <- as.numeric(Mat.exp$Sex.1m)
Mat.exp$Age <- as.numeric(Mat.exp$Age)
Mat.exp$Height <- as.numeric(Mat.exp$Height)
Mat.exp$Weight <- as.numeric(Mat.exp$Weight)
Mat.exp$Weightg <- as.numeric(Mat.exp$Weightg)
Mat.exp$CellWeightg <- as.numeric(Mat.exp$CellWeightg)
Mat.exp$interview_year <- as.numeric(Mat.exp$interview_year)
Mat.exp$BirthYear <- as.numeric(Mat.exp$BirthYear)
Mat.exp$BirthYearUp <- as.numeric(Mat.exp$BirthYearUp)
Mat.exp$BirthYearLo <- as.numeric(Mat.exp$BirthYearLo)
Mat.exp$firstObs <- as.numeric(Mat.exp$firstObs)

Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[6]),]


#### Add gestational time to age
Ber.exp$TotAge <- as.numeric(Ber.exp$Age) + 0.75
Mat.exp$TotAge <- as.numeric(Mat.exp$Age) # in simulated dataset, age is already measured since conception

Ber.exp$ConcepYear <- as.numeric(Ber.exp$BirthYear) - 0.75
Mat.exp$ConcepYear <- as.numeric(Mat.exp$BirthYear) - 0.75


for ( x in 1:nrow(Ber.exp) ) {
  if ( Ber.exp$ConcepYear[x] >= Ber.exp$interview_year[x] ) {
    Ber.exp$ConcepYear[x] <- Ber.exp$interview_year[x] - 0.0001     # avoid negative observed ages
  } # if
} # for

for ( x in 1:nrow(Mat.exp) ) {
  if ( Mat.exp$ConcepYear[x] >= Mat.exp$interview_year[x] ) {
    Mat.exp$ConcepYear[x] <- Mat.exp$interview_year[x] - 0.0001     # avoid negative observed ages
  } # if
} # for




#### Split dataset by sex
Ber.fem <- Ber.exp[which(Ber.exp$Sex.1m == 2),]
Ber.mal <- Ber.exp[which(Ber.exp$Sex.1m == 1),]
dim(Ber.fem)
dim(Ber.mal)

Mat.fem <- Mat.exp[which(Mat.exp$Sex.1m == 2),]
Mat.mal <- Mat.exp[which(Mat.exp$Sex.1m == 1),]
dim(Mat.fem)
dim(Mat.mal)


#### Make consecutive IDs
num_femB <- length( unique(Ber.fem$PID) )
Ber.fem <- Ber.fem[order(Ber.fem$PID),] #order by PID
Ber.fem$ID <- as.numeric( factor(Ber.fem$PID, levels=unique(Ber.fem$PID)) ) #trick to assign consecutive numbers to IDs

num_malB <- length( unique(Ber.mal$PID) )
Ber.mal <- Ber.mal[order(Ber.mal$PID),]
Ber.mal$ID <- as.numeric( factor(Ber.mal$PID, levels=unique(Ber.mal$PID)) )


num_femM <- length( unique(Mat.fem$PID) )
Mat.fem <- Mat.fem[order(Mat.fem$PID),]
Mat.fem$ID <- as.numeric( factor(Mat.fem$PID, levels=unique(Mat.fem$PID)) )

num_malM <- length( unique(Mat.mal$PID) )
Mat.mal <- Mat.mal[order(Mat.mal$PID),]
Mat.mal$ID <- as.numeric( factor(Mat.mal$PID, levels=unique(Mat.mal$PID)) )



#### Combine ethnic groups and re-number consecutive IDs

Com.fem <- rbind(Ber.fem,Mat.fem)
Com.fem$ID <- as.numeric( factor(Com.fem$PID, levels=unique(Com.fem$PID)) )

Com.mal <- rbind(Ber.mal,Mat.mal)
Com.mal$ID <- as.numeric( factor(Com.mal$PID, levels=unique(Com.mal$PID)) )




# #plot female cell weight
# pdf(file="./Plots/fem_cell_weight.pdf",
#   height=5, width=5)
# plot( x=Ber.fem$Age, y=Ber.fem$CellWeightg,
#       type="p", ylim=c(0,5000), xlim=c(0,26),
#       xlab="Age since birth (yrs)", ylab="Weight (g)",
#       cex=0.5, col="red" )
# points(x=Mat.fem$Age, y=Mat.fem$CellWeightg, lwd=1, col="blue", cex=0.5)
# graphics.off()

# #plot male cell weight
# pdf(file="./Plots/mal_cell_weight.pdf",
#   height=5, width=5)
# plot( x=Ber.mal$Age, y=Ber.mal$CellWeightg,
#       type="p", ylim=c(0,5000), xlim=c(0,26),
#       xlab="Age since birth (yrs)", ylab="Weight (g)",
#       cex=0.5, col="red" )
# points(x=Mat.mal$Age, y=Mat.mal$CellWeightg, lwd=1, col="blue", cex=0.5)
# graphics.off()


# datset characteristics

# number of individuals measured
num_femM
num_femB
num_malM
num_malB

