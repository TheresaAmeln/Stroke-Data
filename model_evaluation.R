##############################################################
#                Evaluation of the mHMM()                    #
##############################################################

library(mHMMbayes)
library(alluvial)
library(RColorBrewer)
library(ggplot2)


### Loading the Data Matrix and Model Results ###

load("mHMMres2.RData")
mHMMres2 <- res # AIC: 119.1248

load("mHMMres2_2.RData")
mHMMres2_2 <- res # AIC: 119.4938

load("mHMMres3.RData")
mHMMres3 <- res # AIC: 161.8206

load("mHMMres4.RData")
mHMMres4 <- res # AIC: 207.274

load("mHMMres5.RData")
mHMMres5 <- res # AIC: 255.7412

load("mHMMres6.RData")
mHMMres6 <- res # AIC: 305.3817

load("mHMMres7.RData")
mHMMres7 <- res # AIC: 361.6826

load("mHMMres8.RData")
mHMMres8 <- res # AIC: 416.0884

load("mHMMres9.RData")
mHMMres9 <- res # AIC: 482.2348

load("mHMMres10.RData")
mHMMres10 <- res # AIC: 536.7116

load("mHMMres_rob.RData")
mHMMres_rob <- res

load("mHMMres_rob2.RData")
mHMMres_rob2 <- res

load("data_m.Rdata")

load("data_m_rob.Rdata")


#################################################
#                     Plots                     #
#################################################


### AIC Plot ###

mHMM_aic <- c(119.1248, 161.8206, 207.274, 255.7412, 305.3817, 361.6826, 416.0884, 482.2348, 536.7116)
ggplot(mapping = aes(x = 2:10, y = mHMM_aic)) +
  geom_line(size = 1.3, color = cols[2]) +
  geom_point(size = 2, color = "black") +
  ylim(0, 600) + 
  labs(x = "Number of Hidden States", y = "AIC")


### Trace Plot - 2-State Non-Robust Model ###

par(oma = c(4, 1, 1, 1), mfrow = c(2, 1), mar = c(5, 4, 1, 1))
for(i in 1:m){
  plot(x = 1:1000, y = mHMMres2$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q],
       ylim = c(0, 1), yaxt = "n", type = "l", ylab = "Transition probability", 
       xlab = "Iteration", col = cols[1], cex.lab = 1.3, lwd = 1.5)
  axis(2, at = seq(0, 1, 0.2), las = 2)
  lines(x = 1:1000, y = mHMMres2_2$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], 
        col = cols[2], lwd = 1.5)
  legend("topright",
         legend = paste("Gender = ", levels(data$gender)[q], "in State", i),
         bty = "n")
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("Starting value set 1", "Starting value set 2"), 
       col = cols[c(1,2)], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')


### Trace Plot - 2-State Robust Model ###

par(oma = c(4, 1, 1, 1), mfrow = c(2, 1), mar = c(5, 4, 1, 1))
for(i in 1:m){
  plot(x = 1:1000, y = mHMMres_rob$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q],
       ylim = c(0, 1), yaxt = "n", type = "l", ylab = "Transition probability", 
       xlab = "Iteration", col = cols[1], cex.lab = 1.3, lwd = 1.5)
  axis(2, at = seq(0, 1, 0.2), las = 2)
  lines(x = 1:1000, y = mHMMres_rob2$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], 
        col = cols[2], lwd = 1.5)
  legend("topright",
         legend = paste("Gender = ", levels(data$gender)[q], "in State", i),
         bty = "n")
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("Starting value set 1", "Starting value set 2"), 
       col = cols[c(1,2)], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')


### Posterior Densities for the Emission Distribution Probabilities ###

# little changes in the plot-function
myplot.mHMM <- function(x, component = "gamma", dep = 1, col, cat_lab,
                        dep_lab, lwd1 = 2, lwd2 = 1, lty1 = 1, lty2 = 3,
                        legend_cex, burn_in, ...){
  #  if (!is.mHMM(x)){
  #    stop("The input object x should be from the class mHMM, obtained with the function mHMM.")
  #  }
  if (component != "gamma" & component != "emiss"){
    stop("The input specified under component should be a string, restrectid to state either gamma or emiss.")
  }
  object <- x
  input   <- x$input
  n_subj  <- input$n_subj
  if (missing(burn_in)){
    burn_in <- input$burn_in
  }
  J       <- input$J
  if (burn_in >= (J-1)){
    stop(paste("The specified burn in period should be at least 2 points smaller
               compared to the number of iterations J, J =", J))
  }
  old_par <- graphics::par(no.readonly =TRUE)
  on.exit(graphics::par(old_par))
  m       <- input$m
  q_emiss <- input$q_emiss
  n_dep   <- input$n_dep
  
  if(component == "gamma"){
    if (missing(col)){
      state_col <- grDevices::rainbow(m)
    } else {
      state_col <- col
    }
    if(m > 3){
      graphics::par(mfrow = c(2,ceiling(m/2)), mar = c(4,2,3,1) + 0.1, mgp = c(2,1,0))
    } else {
      graphics::par(mfrow = c(1,m), mar = c(4,2,3,1) + 0.1, mgp = c(2,1,0))
    }
    for(i in 1:m){
      max <- 0
      for(j in 1:m){
        new <- max(stats::density(object$gamma_prob_bar[burn_in:J, m * (i-1) + j])$y)
        if(new > max){max <- new}
      }
      graphics::plot.default(x = 1, ylim = c(0, max), xlim = c(0,1), type = "n", cex = .8,  main =
                               paste("From state", i, "to state ..."), yaxt = "n", ylab = "",
                             xlab = "Transition probability", ...)
      graphics::title(ylab="Density", line=.5)
      for(j in 1:m){
        graphics::lines(stats::density(object$gamma_prob_bar[burn_in:J,m * (i-1) + j]),
                        type = "l", col = state_col[j], lwd = lwd1, lty = lty1)
        for(s in 1:n_subj){
          graphics::lines(stats::density(object$PD_subj[[s]][burn_in:J,(sum(q_emiss * m) + m * (i-1) + j)]),
                          type = "l", col = state_col[j], lwd = lwd2, lty = lty2)
        }
      }
      graphics::legend("topright", col = state_col, legend = paste("To state", 1:m),
                       bty = 'n', lty = 1, lwd = 2, cex = .8)
    }
  } else if (component == "emiss"){
    if (missing(cat_lab)){
      cat_lab <- paste("Category", 1:q_emiss[dep])
    }
    if (missing(dep_lab)){
      dep_lab <- input$dep_labels[dep]
    }
    start <- c(0, q_emiss * m)
    start2 <- c(0, seq(from = (q_emiss[dep]-1) * 2, to = (q_emiss[dep]-1) * 2 * m, by = (q_emiss[dep]-1) * 2))
    if (missing(col)){
      cat_col <- grDevices::rainbow(q_emiss[dep])
    } else {
      cat_col <- col
    }
    if(m > 3){
      graphics::par(mfrow = c(2,ceiling(m/2)), mar = c(4,2,3,1) + 0.1, mgp = c(2,1,0))
    } else {
      graphics::par(mfrow = c(1,m), mar = c(4,2,3,1) + 0.1, mgp = c(2,1,0))
    }
    for(i in 1:m){
      # determining the scale of the y axis
      max <- 0
      for(q in 1:q_emiss[dep]){
        new <- max(stats::density(object$emiss_prob_bar[[dep]][burn_in:J,q_emiss[dep] * (i-1) + q])$y)
        if(new > max){max <- new}
      }
      # set plotting area
      graphics::plot.default(x = 1, ylim = c(0, max), xlim = c(0,1), type = "n",
                             main = paste(dep_lab, ", state", i),
                             yaxt = "n", ylab = "", xlab = "Conditional probability", ...)
      graphics::title(ylab="Density", line=.5)
      for(q in 1:q_emiss[dep]){
        # add density curve for population level posterior distribution
        graphics::lines(stats::density(object$emiss_prob_bar[[dep]][burn_in:J,q_emiss[dep] * (i-1) + q]),
                        type = "l", col = cat_col[q], lwd = lwd1, lty = lty1)
        # add density curves for subject posterior distributions
        for(s in 1:10){
          graphics::lines(stats::density(object$PD_subj[[s]][burn_in:J,(sum(start[1:dep])
                                                                        + (i-1)*q_emiss[dep] + q)]),
                          type = "l", col = cat_col[q], lwd = lwd2, lty = lty2)
        }
      }
      #      graphics::legend("topright", col = cat_col, legend = cat_lab, bty = 'n', lty = 1, lwd = 2, cex = .7)
    }
  }
}

# NIHSS - non-robust
myplot.mHMM(mHMMres2, component = "emiss", dep = 1, col = cols[1:5],
            dep_lab = c("NIHSS group"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[0,4]", "(4,8]", "(8,12]", "(12,20]", "(20,42]"), 
       col = cols[1:5], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# NIHSS - robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))

myplot.mHMM(mHMMres_rob, component = "emiss", dep = 1, col = cols[1:5],
            dep_lab = c("NIHSS group"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[0,4]", "(4,8]", "(8,12]", "(12,20]", "(20,42]"), 
       col = cols[1:5], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# gender - non-robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))
myplot.mHMM(mHMMres2, component = "emiss", dep = 2, col = cols[c(1,2)], 
            dep_lab = c("Gender"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("Male", "Female"), 
       col = cols[c(1,2)], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# gender - robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))
myplot.mHMM(mHMMres_rob, component = "emiss", dep = 2, col = cols[c(1,2)], 
            dep_lab = c("Gender"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("Male", "Female"), 
       col = cols[c(1,2)], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# fibril - non-robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))
myplot.mHMM(mHMMres2, component = "emiss", dep = 3, col = cols[c(1,2)],
            dep_lab = c("Atrial fibrillation"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("Nein", "Ja"), 
       col = cols[c(1,2)], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# fibril - robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))
myplot.mHMM(mHMMres_rob, component = "emiss", dep = 3, col = cols[c(1,2)],
            dep_lab = c("Atrial fibrillation"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("Nein", "Ja"), 
       col = cols[c(1,2)], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# agegroup - non-robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))
myplot.mHMM(mHMMres2, component = "emiss", dep = 4, col = cols[1:4], 
            dep_lab = c("Age group"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[18,64]", "(64,74]", "(74,84]", ">84"), 
       col = cols[1:4], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# agegroup - robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))
myplot.mHMM(mHMMres_rob, component = "emiss", dep = 4, col = cols[1:4], 
            dep_lab = c("Age group"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[18,64]", "(64,74]", "(74,84]", ">84"), 
       col = cols[1:4], lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# mRS - non-robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))
myplot.mHMM(mHMMres2, component = "emiss", dep = 5, col = cols, 
            dep_lab = c("mRS"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("0", "1", "2", "3", "4", "5"), 
       col = cols, lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')

# mRS - robust
par(oma = c(4, 1, 1, 1), mar = c(4, 4, 1, 1))
myplot.mHMM(mHMMres_rob, component = "emiss", dep = 5, col = cols, 
            dep_lab = c("mRS"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("0", "1", "2", "3", "4", "5"), 
       col = cols, lty = 1, lwd = 1.5, xpd = TRUE, horiz = TRUE, 
       cex = 1, seg.len = 2, bty = 'n')


### State Transition Probability Matrix ###

# non-robust
plot(obtain_gamma(mHMMres2), col = cols[c(1,2)])

# robust
plot(obtain_gamma(mHMMres_rob), col = cols[c(1,2)])


### Density Plots for the States ###

# densitiy plot state 1
emiss_post <- mHMMres2$emiss_prob_bar

par(oma = c(7, 1, 1, 1), mar = c(4, 4, 1, 1))
plot(density(emiss_post$NIHSS_group[,1]), xlim = c(0,1), ylim = c(0, 35), 
     col = brewer.pal(7, "RdPu")[2], lwd = 2, main = "", xlab = "", lty = 1)
lines(density(emiss_post$NIHSS_group[,2]), col = brewer.pal(7, "RdPu")[3], lwd = 2, lty = 2) #purple
lines(density(emiss_post$NIHSS_group[,3]), col = brewer.pal(7, "RdPu")[5], lwd = 2, lty = 3)
lines(density(emiss_post$NIHSS_group[,4]), col = brewer.pal(7, "RdPu")[6], lwd = 2, lty = 4)
lines(density(emiss_post$NIHSS_group[,5]), col = brewer.pal(7, "RdPu")[7], lwd = 2, lty = 5)
lines(density(emiss_post$gender[,1]), col = brewer.pal(3, "YlOrRd")[2], lwd = 2, lty = 1) #red/orange
lines(density(emiss_post$gender[,2]), col = brewer.pal(3, "YlOrRd")[3], lwd = 2, lty = 2)
lines(density(emiss_post$fibril[,1]), col = brewer.pal(5, "YlGnBu")[3], lwd = 2, lty = 1) #blue
lines(density(emiss_post$fibril[,2]), col = brewer.pal(5, "YlGnBu")[5], lwd = 2, lty = 2)
lines(density(emiss_post$agegroup[,1]), col = brewer.pal(5, "YlGn")[2], lwd = 2, lty = 1) #green
lines(density(emiss_post$agegroup[,2]), col = brewer.pal(5, "YlGn")[3], lwd = 2, lty = 2)
lines(density(emiss_post$agegroup[,3]), col = brewer.pal(5, "YlGn")[4], lwd = 2, lty = 3)
lines(density(emiss_post$agegroup[,4]), col = brewer.pal(5, "YlGn")[5], lwd = 2, lty = 4)
lines(density(emiss_post$mRS[,1]), col = brewer.pal(10, "BrBG")[1], lwd = 2, lty = 1) #brown/green
lines(density(emiss_post$mRS[,2]), col = brewer.pal(10, "BrBG")[2], lwd = 2, lty = 2)
lines(density(emiss_post$mRS[,3]), col = brewer.pal(10, "BrBG")[4], lwd = 2, lty = 3)
lines(density(emiss_post$mRS[,4]), col = brewer.pal(10, "BrBG")[7], lwd = 2, lty = 4)
lines(density(emiss_post$mRS[,5]), col = brewer.pal(10, "BrBG")[9], lwd = 2, lty = 5)
lines(density(emiss_post$mRS[,6]), col = brewer.pal(10, "BrBG")[10], lwd = 2, lty = 6)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("bottom", legend = c(paste("NIHSS = ", levels(data$NIHSS_group)), paste("gender = ", levels(data$gender)), paste("fibril = ", levels(data$fibril)), paste("age group = ", c("[18,64]", "(64,74]", "(74,84]", ">84")), paste("mRS = ", levels(data$mRS))),
       lty = c(1:5, 1:2, 1:2, 1:4, 1:6), 
       col = c(brewer.pal(7, "RdPu")[c(1,3,5,6,7)], brewer.pal(3, "YlOrRd")[2:3], 
               brewer.pal(5, "YlGnBu")[c(3,5)], brewer.pal(5, "YlGn")[2:5], 
               brewer.pal(10, "BrBG")[c(1,2,4,7,9,10)]),
       lwd = 2, bty = "n", xpd = TRUE, seg.len = 2, ncol = 3)

# density plot state 2
emiss_post <- mHMMres2$emiss_prob_bar

par(oma = c(7, 1, 1, 1), mar = c(4, 4, 1, 1))
plot(density(emiss_post$NIHSS_group[,6]), xlim = c(0,1), ylim = c(0, 35), 
     col = brewer.pal(7, "RdPu")[2], lwd = 2, main = "", xlab = "", lty = 1)
lines(density(emiss_post$NIHSS_group[,7]), col = brewer.pal(7, "RdPu")[3], lwd = 2, lty = 2) #purple
lines(density(emiss_post$NIHSS_group[,8]), col = brewer.pal(7, "RdPu")[5], lwd = 2, lty = 3)
lines(density(emiss_post$NIHSS_group[,9]), col = brewer.pal(7, "RdPu")[6], lwd = 2, lty = 4)
lines(density(emiss_post$NIHSS_group[,10]), col = brewer.pal(7, "RdPu")[7], lwd = 2, lty = 5)
lines(density(emiss_post$gender[,3]), col = brewer.pal(3, "YlOrRd")[2], lwd = 2, lty = 1) #red/orange
lines(density(emiss_post$gender[,4]), col = brewer.pal(3, "YlOrRd")[3], lwd = 2, lty = 2)
lines(density(emiss_post$fibril[,3]), col = brewer.pal(5, "YlGnBu")[3], lwd = 2, lty = 1) #blue
lines(density(emiss_post$fibril[,4]), col = brewer.pal(5, "YlGnBu")[5], lwd = 2, lty = 2)
lines(density(emiss_post$agegroup[,5]), col = brewer.pal(5, "YlGn")[2], lwd = 2, lty = 1) #green
lines(density(emiss_post$agegroup[,6]), col = brewer.pal(5, "YlGn")[3], lwd = 2, lty = 2)
lines(density(emiss_post$agegroup[,7]), col = brewer.pal(5, "YlGn")[4], lwd = 2, lty = 3)
lines(density(emiss_post$agegroup[,8]), col = brewer.pal(5, "YlGn")[5], lwd = 2, lty = 4)
lines(density(emiss_post$mRS[,7]), col = brewer.pal(10, "BrBG")[1], lwd = 2, lty = 1) #brown/green
lines(density(emiss_post$mRS[,8]), col = brewer.pal(10, "BrBG")[2], lwd = 2, lty = 2)
lines(density(emiss_post$mRS[,9]), col = brewer.pal(10, "BrBG")[4], lwd = 2, lty = 3)
lines(density(emiss_post$mRS[,10]), col = brewer.pal(10, "BrBG")[7], lwd = 2, lty = 4)
lines(density(emiss_post$mRS[,11]), col = brewer.pal(10, "BrBG")[9], lwd = 2, lty = 5)
lines(density(emiss_post$mRS[,12]), col = brewer.pal(10, "BrBG")[10], lwd = 2, lty = 6)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("bottom", legend = c(paste("NIHSS = ", levels(data$NIHSS_group)), paste("gender = ", levels(data$gender)), paste("fibril = ", levels(data$fibril)), paste("age group = ", c("[18,64]", "(64,74]", "(74,84]", ">84")), paste("mRS = ", levels(data$mRS))),
       lty = c(1:5, 1:2, 1:2, 1:4, 1:6), 
       col = c(brewer.pal(7, "RdPu")[c(1,3,5,6,7)], brewer.pal(3, "YlOrRd")[2:3], 
               brewer.pal(5, "YlGnBu")[c(3,5)], brewer.pal(5, "YlGn")[2:5], 
               brewer.pal(10, "BrBG")[c(1,2,4,7,9,10)]),
       lwd = 2, bty = "n", xpd = TRUE, seg.len = 2, ncol = 3)


### All Transition and Density Plots ###

gamma_pop2 <- obtain_gamma(mHMMres2)
gamma_subj2 <- obtain_gamma(mHMMres2, level = "subject")

plot(mHMMres2, component = "emiss", dep = 1, col = col[-4], cex.main = 0.7)
plot(mHMMres2, component = "emiss", dep = 2, col = col[c(1,6)], cex.main = 0.7)
plot(mHMMres2, component = "emiss", dep = 3, col = col[c(1,6)], cex.main = 0.7)
plot(mHMMres2, component = "emiss", dep = 4, col = col[-(3:4)], cex.main = 0.7)
plot(mHMMres2, component = "emiss", dep = 5, col = col, cex.main = 0.7)
for (i in 1:96){
  par(mar=c(5,5,1,5))
  plot(gamma_subj2, subj_nr = i, col = col[c(1,6)])
  title(paste("NIHSS Group",data_m[i,2],", Gender",data_m[i,3],", Fibril",data_m[i,4],
              ", Age Group", data_m[i,5],", mRS",data_m[i,6]), cex.main = 0.7)
}