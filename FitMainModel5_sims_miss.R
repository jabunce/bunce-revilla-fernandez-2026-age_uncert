############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


N <- nrow(Com.fem)                      # number of observations
J <- length(unique(Com.fem$ID))         # number of people measured
E <- length(unique(Com.fem$Ethnicity))  # number of ethnic groups
EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID
agecalc <- Com.fem[which(Com.fem$firstObs == 1), c("ID", "interview_year", "ConcepYear")] # first observation year and recorded year of conception for each individual
length(unique(Com.fem$PID)) # num rows of agecalc should match this: number of indivs


Com.fem$CellWeightg_miss <- Com.fem$CellWeightg
Com.fem[which(Com.fem$Ethnicity==2), "CellWeightg_miss"] <-  1e10 #NA
w_missidx <- which(Com.fem$CellWeightg_miss==1e10)   #which( is.na( Com.fem$CellWeightg_miss ) )
w_num_miss <- length(w_missidx)



data_list7 <- list(
  N = nrow(Com.fem),                      # number of observations
  J = length(unique(Com.fem$ID)),         # number of people measured
  E = length(unique(Com.fem$Ethnicity)),  # number of ethnic groups
  ID = Com.fem$ID,                        # vector of individual IDs for each height observation
  age = Com.fem$TotAge,                   # vector of ages for each observation (in years since conception)
  height = Com.fem$Height,                # vector of observations (in cm)
  weight = Com.fem$CellWeightg_miss,           # vector of 5% of weight observations (in g)
  ethnicity = Com.fem$Ethnicity,          # vector of ethnicity for each observation (1=Ber, 2=Mat)
  ethIndiv = EthID[,2],                   # vector of ethnicity for each person (1=Ber, 2=Mat)

  # for age uncertainty
  obsyear = Com.fem$interview_year,        # year for each observation
  sdID = (agecalc[,2] - agecalc[,3])*xsd,  # stdev of Normal around recorded conception year for each individual = (recorded age)*0.1, must be non-zero 
  recConcepYear = agecalc[,3],             # recorded year of conception for each individual
  firstObs = Com.fem$firstObs,             # indicator if is first observation


  # means and stdevs from PriorPredictGroupStdev5
  meansL = c(muLQ1,muLQ2,muLQ3,muLQ4,muLQ5,
             muLK1,muLK2,muLK3,muLK4,muLK5,
             muR1, muR2, muR3, muR4, muR5,
             muLI2,muLI3,muLI4,muLI5,muLIM),

  gexpL = c(gexpLQ1,gexpLQ2,gexpLQ3,gexpLQ4,gexpLQ5,
            gexpLK1,gexpLK2,gexpLK3,gexpLK4,gexpLK5,
            gexpR1, gexpR2, gexpR3, gexpR4, gexpR5,
            gexpLI2,gexpLI3,gexpLI4,gexpLI5,gexpLIM),

  iexpL = c(iexpLQ1,iexpLQ2,iexpLQ3,iexpLQ4,iexpLQ5,
            iexpLK1,iexpLK2,iexpLK3,iexpLK4,iexpLK5,
            iexpR1, iexpR2, iexpR3, iexpR4, iexpR5,
            iexpLI2,iexpLI3,iexpLI4,iexpLI5,iexpLIM),

  ihsdL = c(ihsdLQ1,ihsdLQ2,ihsdLQ3,ihsdLQ4,ihsdLQ5,
            ihsdLK1,ihsdLK2,ihsdLK3,ihsdLK4,ihsdLK5,
            ihsdR1, ihsdR2, ihsdR3, ihsdR4, ihsdR5,
            ihsdLI2,ihsdLI3,ihsdLI4,ihsdLI5,ihsdLIM),

  CholEtaI = CholEtaI,
  CholEtaG = CholEtaG,

  muLsigma_h = muLsigma_h,
  muLsigma_w = muLsigma_w,

  w_num_miss = w_num_miss,
  w_missidx = w_missidx
)

start_list7 <- list(
  zGrp = matrix(0, nrow=E, ncol=20),               # ethnicity offset z-scores to each parameter: E-array of column 20-vectors, one vector for each ethnicity
  offsetGroupStdevs = c(gexpLQ1,gexpLQ2,gexpLQ3,gexpLQ4,gexpLQ5,
                        gexpLK1,gexpLK2,gexpLK3,gexpLK4,gexpLK5,
                        gexpR1, gexpR2, gexpR3, gexpR4, gexpR5,
                        gexpLI2,gexpLI3,gexpLI4,gexpLI5,gexpLIM),   # stdevs for mean (across indivs) offsets to the overall mean (across groups) trajectory for each group, for each of the 20 parameters, on log scale
  L_G = array(data=0, dim=c(20,20)),           # cholesky factor 20x20 matrix, for correlation matrix for mean offsets

  zInd = matrix(0, nrow=J, ncol=20),          # individual offset z-scores to each parameter: J-array of column 20-vectors, one vector for each individual
  offsetIndivStdevs = array( c( c(iexpLQ1,iexpLQ2,iexpLQ3,iexpLQ4,iexpLQ5,
                                  iexpLK1,iexpLK2,iexpLK3,iexpLK4,iexpLK5,
                                  iexpR1, iexpR2, iexpR3, iexpR4, iexpR5,
                                  iexpLI2,iexpLI3,iexpLI4,iexpLI5,iexpLIM),
                                c(iexpLQ1,iexpLQ2,iexpLQ3,iexpLQ4,iexpLQ5,
                                  iexpLK1,iexpLK2,iexpLK3,iexpLK4,iexpLK5,
                                  iexpR1, iexpR2, iexpR3, iexpR4, iexpR5,
                                  iexpLI2,iexpLI3,iexpLI4,iexpLI5,iexpLIM)
                              ), dim=c(2,20)),   # stdevs for mean (across individuals) offsets to the mean group trajectory for each individual, for each of the 20 parameters
  L_I = array(data=0, dim=c(E,20,20)),           # cholesky factor 20x20 matrix, for correlation matrix for mean offsets

  Lsigma_h = muLsigma_h,
  Lsigma_w = muLsigma_w,

  concepyear = agecalc[,3],

  log_weight_impute = rep(5, times=w_num_miss)
)



############################### Run Stan model

model <- cmdstan_model(model_file[3])

m7 <- model$sample(
        seed=1,
        data=data_list7, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list7), num_chains),
        max_treedepth=15, #15 default treedepth is 10
        adapt_delta=0.99
      )

m7$save_object(file = modfit_name) #save stan output so you don't have re-run model
#m7 <- readRDS(modfit_name)


print(n=500, as_tibble(
      m7$summary(c("mean", "sd", "rhat", "ess_bulk"),
                 variables=c(
                              "lp__",
                              "weight_impute",
                              "Lsigma_h",
                              "Lsigma_w",
                              "mQ",
                              "mK",
                              "mH",
                              "mI",
                              "GrpOffset",
                              "offsetGroupStdevs",
                              "offsetIndivStdevs",
                              "concepyear"))
))

post7 <- m7$draws(format = "df", , inc_warmup = FALSE)
#str(post7, list.len=100)
saveRDS(post7, modpost_name)
#post7 <- readRDS(modpost_name)
#post6$"GrpOffset[1]"

paramList <- list(SmQ, SmK, SmH, SmI, sQ, sK, sH, sI) # posterior samples for parameters of fit model
saveRDS(paramList, parlist_name) 
saveRDS(Com.fem, comfem_name) # simulated data to which model is fit




fitmod <- m7$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
#str(fitmod)

fitmod[is.na(fitmod)] <- 0 # get rid of NAs so you can print

fitmodb <- m7$draws(format = "draws_list", inc_warmup = TRUE) # array format keeps chains separate for plotting
#str(fitmodb)

color_scheme_set("mix-blue-red")



#look at all traces, in Plots folder
pdf(file=trace_name,
  height=3, width=8)
par(mfrow=c(2,1))

    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="Lsigma_h", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="Lsigma_w", iter1 = samps/2))


    for ( z in 1:20 ){
        print(mcmc_trace(fitmod, pars=paste("offsetGroupStdevs[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z


    for ( z1 in 1:E ){
        for ( z2 in 1:20 ){
          #nm <- paste("offsetIndivStdevs[", z1, ",", z2, "]", sep="")
          #for ( z3 in 1:num_chains ){
            #if ( !is.na( fitmodb[[z3]][[nm]][1] ) ) {
              print(mcmc_trace(fitmod, pars=paste("offsetIndivStdevs[", z1, ",", z2, "]", sep=""), n_warmup = samps/2 ))
            #} # if
          #} # for z3
        } # for z2
    } # for z1


    for ( z1 in 1:20 ){
      for ( z2 in 1:20 ){
        print(mcmc_trace(fitmod, pars=paste("L_G[", z1, ",", z2, "]", sep=""), n_warmup = samps/2 ))
      } # for z2
    } # for z1


    # for ( z1 in 1:E ){
    #     for ( z2 in 1:20 ){
    #       for ( z3 in 1:20 ){
    #         #nm <- paste("L_I[", z1, ",", z2, ",", z3, "]", sep="")
    #         #for ( z4 in 1:num_chains ){                             # plot for each variable for each chain (looks like duplicated plots)
    #          # if ( !is.na( fitmodb[[1]][[nm]][1] ) ) {              # indices are: [chain 1][variable name][first sample]
    #             print(mcmc_trace(fitmod, pars=paste("L_I[", z1, ",", z2, ",", z3, "]", sep=""), n_warmup = samps/2 ))
    #           #} # if
    #         #} # for z4
    #       } # for z3
    #     } # for z2
    # } # for z1


    for ( z in 1:nrow(agecalc) ){
        print(mcmc_trace(fitmod, pars=paste("concepyear[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z

    for ( z in 1:w_num_miss ){
        print(mcmc_trace(fitmod, pars=paste("weight_impute[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z

graphics.off()

