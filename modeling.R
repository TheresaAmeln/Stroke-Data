##########################################################
#           Modeling the Data with mHMMbayes             #
##########################################################

library(mHMMbayes)


### Loading the Filtered Data ###

load("data.Rdata")


### Creating the Data Matrix - Robust and Non-Robust ###

# little changes on the data frame
agegroups <- cut(data$age, c(18, 64, 74, 84, max(data$age)), include.lowest=TRUE)
data_fac <- data.frame(year = as.factor(data$year),
                       gender = data$gender,
                       age = agegroups,
                       mRS = data$mRS,
                       fibril = data$fibril,
                       severity = data$severity,
                       NIHSS_group = data$NIHSS_group)

# summarize factor combinations
data_mean <- aggregate(data_fac$severity, 
                       list(year = data_fac$year, 
                            gender = data_fac$gender, 
                            age = data_fac$age, 
                            mRS = data_fac$mRS, 
                            fibril = data_fac$fibril), 
                       mean)
colnames(data_mean)[6] <- "sev_mean"

# making it robust
data_mean_rob <- aggregate(data_fac$severity, 
                           list(year = data_fac$year, 
                                gender = data_fac$gender, 
                                age = data_fac$age, 
                                mRS = data_fac$mRS, 
                                fibril = data_fac$fibril), 
                           mean, trim = 0.1)
colnames(data_mean_rob)[6] <- "sev_mean"


### creating data matrix

# not robust
data_mean$NIHSS_group <- cut(data_mean$sev_mean, 
                             c(0, 4, 8, 12, 20, 42), #new NIHSS groups (12,20] & (20,42]
                             include.lowest=TRUE)
save(list = "data_mean", file = "data_mean.RData")

data_m <- matrix(c(as.numeric(levels(data_mean$year))[data_mean$year],
                   as.numeric(data_mean$NIHSS_group), 
                   as.numeric(data_mean$gender), 
                   as.numeric(data_mean$fibril), 
                   as.numeric(data_mean$age),
                   as.numeric(data_mean$mRS)), 
                 nrow = nrow(data_mean))
colnames(data_m) <- c("year", "NIHSS_group", "gender", "fibril", "agegroup", "mRS")

# define ID: according to gender, fibril, agegroup, mRS
IDvar <- NULL
setID <- 0
datasort <- NULL
for (i in 1:2){ # gender
  for (j in 1:2){ # fibril
    for (k in 1:4){ # age
      for(l in 1:6){ # mRS
        setID <- setID+1
        sel <- ((data_m[,"gender"] == i) & (data_m[,"fibril"] == j) & (data_m[,"agegroup"] == k) 
                & (data_m[,"mRS"] == l))
        #               print(sum(sel))
        if (sum(sel) > 0){
          IDvar <- c(IDvar, rep(setID, sum(sel)))
          datasort <- rbind(datasort, data_m[sel,])
        }
      }
    }
  }
}

datasort[,"year"] <- IDvar
data_m <- datasort
colnames(data_m) <- c("id", "NIHSS_group", "gender", "fibril", "agegroup", "mRS")

# save(list = "data_m", file = "data_m.RData")

# data_m[data_m[,"id"] == 77,] hat nur 3 Beobachtungen. Problem?

# robust one
data_mean_rob$NIHSS_group <- cut(data_mean_rob$sev_mean, 
                                 c(0, 4, 8, 12, 20, 42), #new NIHSS groups (12,20] & (20,42]
                                 include.lowest=TRUE)
save(list = "data_mean_rob", file = "data_mean_rob.RData")

data_m_rob <- matrix(c(as.numeric(levels(data_mean_rob$year))[data_mean_rob$year],
                       as.numeric(data_mean_rob$NIHSS_group), 
                       as.numeric(data_mean_rob$gender), 
                       as.numeric(data_mean_rob$fibril), 
                       as.numeric(data_mean_rob$age),
                       as.numeric(data_mean_rob$mRS)), 
                     nrow = nrow(data_mean_rob))
colnames(data_m_rob) <- c("year", "NIHSS_group", "gender", "fibril", "agegroup", "mRS")

# define ID: according to gender, fibril, agegroup, mRS
IDvar <- NULL
setID <- 0
datasort_rob <- NULL
for (i in 1:2){ # gender
  for (j in 1:2){ # fibril
    for (k in 1:4){ # age
      for(l in 1:6){ # mRS
        setID <- setID+1
        sel <- ((data_m_rob[,"gender"] == i) & (data_m_rob[,"fibril"] == j) & (data_m_rob[,"agegroup"] == k) 
                & (data_m_rob[,"mRS"] == l))
        #               print(sum(sel))
        if (sum(sel) > 0){
          IDvar <- c(IDvar, rep(setID, sum(sel)))
          datasort_rob <- rbind(datasort_rob, data_m_rob[sel,])
        }
      }
    }
  }
}

datasort_rob[,"year"] <- IDvar
data_m_rob <- datasort_rob
colnames(data_m_rob) <- c("id", "NIHSS_group", "gender", "fibril", "agegroup", "mRS")

save(list = "data_m_rob", file = "data_m_rob.RData")


##########################################################
#                      Modeling                          #
##########################################################


### Function for Model Fitting with Different Numbers of States ###

n_dep <- 5  # number of dependent variables (hidden states)
q_emiss <- c(5, 2, 2, 4, 6) # number of states of dependent variables

for(i in 6:10){
  # specifying general model properties
  m <- i  # number of states used
  # specifying starting values
  start_TM <- diag((1-m/10),m) # starting vals for transition matrix
  start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- (m/(10*(m-1)))
  #  start_TM <- matrix(rep(0.5, m*m), nrow = m)
  
  EM_NIHSS <- matrix(rep(0.2, 5*m), ncol = 5)
  EM_gender <- matrix(rep(0.5, 2*m), ncol = 2)
  EM_fibril <- matrix(rep(0.5, 2*m), ncol = 2)
  EM_agegroup <- matrix(rep(0.25, 4*m), ncol = 4)
  EM_mRS <- matrix(rep(1/6, 6*m), ncol = 6)
  
  start_EM <- list(EM_NIHSS, EM_gender, EM_fibril, EM_agegroup, EM_mRS)
  
  # fitting the model
  set.seed(123)
  res <- mHMM(s_data = data_m, 
              gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
              start_val = c(list(start_TM), start_EM),
              mcmc = list(J = 1000, burn_in = 200))
  save(list = "res", file = paste("mHMMres", i, ".RData", sep = ""))
}


### 2-States Model with Different Starting Values ###

# specifying general model properties
n_dep <- 5  # number of dependent variables (hidden states)
q_emiss <- c(5, 2, 2, 4, 6) # number of states of dependent variables
m <- 2  # number of states used

# specifying starting values
# start_TM <- diag((1-m/10),m) # starting vals for transition matrix
# start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- (m/(10*(m-1)))
start_TM <- matrix(rep(0.5, 4), nrow = 2)

# EM_NIHSS <- matrix(rep(0.2, 5*m), ncol = 5)
EM_NIHSS <- matrix(rep(c(0.15, 0.2, 0.3, 0.2, 0.15), 2), ncol = 5, byrow = TRUE)
# EM_gender <- matrix(rep(0.5, 2*m), ncol = 2)
EM_gender <- matrix(c(0.4, 0.6, 0.6, 0.4), ncol = 2)
# EM_fibril <- matrix(rep(0.5, 2*m), ncol = 2)
EM_fibril <- matrix(c(0.6, 0.4, 0.4, 0.6), ncol = 2)
# EM_agegroup <- matrix(rep(0.25, 4*m), ncol = 4)
EM_agegroup <- matrix(rep(c(0.2, 0.3, 0.3, 0.2), 2), ncol = 4, byrow = TRUE)
# EM_mRS <- matrix(rep(1/6, 6*m), ncol = 6)
EM_mRS <- matrix(rep(c(0.2, 0.15, 0.15, 0.2, 0.15, 0.15), 2), ncol = 6, byrow = TRUE)

start_EM <- list(EM_NIHSS, EM_gender, EM_fibril, EM_agegroup, EM_mRS)

# fitting the model - robust
set.seed(123)
res <- mHMM(s_data = data_m_rob, 
            gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
            start_val = c(list(start_TM), start_EM),
            mcmc = list(J = 1000, burn_in = 200))
save(list = "res", file = "mHMMres_rob2.RData")

# fitting the model - non-robust
set.seed(123)
res <- mHMM(s_data = data_m, 
            gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
            start_val = c(list(start_TM), start_EM),
            mcmc = list(J = 1000, burn_in = 200))
save(list = "res", file = "mHMMres2_2.RData")