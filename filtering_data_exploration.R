#############################################################
#        Stroke Data from Gesundheit Österreich GmbH        #
#############################################################


# loading the data
load("namedData.RData")

dat <- datDE
namen <- namenDE


###################################################
#             Filtering of the Data               #
###################################################

# DM data are incomplete for 2020, however the last download was done in August, if we upload the updated version I think we could end up with 13 thousands of rows
# DM for this moment we will deal only with data from 2005 to 2019 (incl.)

dat <- dat[which(dat$jahr >= 2005 & dat$jahr<=2019),]	# without incomplete years

dat <- dat[which(!is.na(dat$i2005)),]	    # omit NAs for gender

dat <- dat[which(!is.na(dat$i4020)),]     # no NAs for severity score

dat <- dat[which(!is.na(dat$i6001)),]     # no NAs for mRS (added by Theresa)

dat <- dat[which(is.na(dat$i2031)),] 	    # specifically for people living in Austria
dat <- dat[-which(dat$i2032=="im Ausland"),]

dat <- dat[which(dat$Alter >= 18),]       # older than 18

dat <- dat[which(dat$i2006 == "Ja"),]	    # only "Akute SA"   

dat <- dat[which(dat$i7001=="Ischämie"),]	# Syndrom existing, not bleeding

dat <- dat[which(dat$i9003 == "Nein" & dat$i16008=="Ja"),]  # only patients who are having a first stroke

# dat$jahr <- factor(dat$jahr)   # if required 


########################################
# Variables to be used in the analysis #
########################################

# year
names(dat)[which(names(dat)=="jahr")]  <- "year"

# gender 
names(dat)[which(names(dat)=="i2005")]  <- "gender"
levels(dat$gender) <- c("Male", "Female") 

# age
names(dat)[which(names(dat)=="Alter")]  <- "age"

# severity score - NIHSS - the most important variable - response variable
names(dat)[which(names(dat)=="i4020")] <- "severity"

# Severity groups (Schweregrad-Gruppen)
# not has been decided yet if groupping variable or every single value of severity ("severity" variable) will be used 
# let's try it with both?
dat$NIHSS_group <- cut(dat$severity, c(0, 4, 8, 12, 20, 42), include.lowest=TRUE)

# Atrial Fibrillation (Vorhofflimmern)
dat$i9006[dat$i9006=="Ja, bereits bekannt"]<-"Ja"
dat$i9006[dat$i9006=="de novo (EKG)"] <-"Ja"
dat$i9006[dat$i9006=="Unbekannt"] <-"Nein"
names(dat)[which(names(dat)=="i9006")] <- "fibril"  
dat$fibril <- droplevels(dat$fibril)

# pre-stroke disability - mRS (Rankinskala vor Insult)
dat$mRS <- dat$i6001
# 0 - No symptoms.
# 1 - No significant disability. Able to carry out all usual activities, despite some symptoms.
# 2 - Slight disability. Able to look after own affairs without assistance, but unable to carry out all previous activities.
# 3 - Moderate disability. Requires some help, but able to walk unassisted.
# 4 - Moderately severe disability. Unable to attend to own bodily needs without assistance, and unable to walk unassisted.
# 5 - Severe disability. Requires constant nursing care and attention, bedridden, incontinent.
# 6 - Dead.


var <- c("year", "gender", "age", "mRS", "fibril", "severity", "NIHSS_group")  

data <- dat[,var]

# TACS - Klinical Syndrom
dat$TACS <- rep("Nein", nrow(dat))
dat$TACS[dat$i7010=="TACS"] <- "Ja"

var <- c("year", "gender", "age", "mRS", "fibril", "severity", "NIHSS_group", "TACS")  
data <- dat[,var]

save(list = "data", file = "data.RData")


############################################################
#                   Data Exploration                       #
############################################################

library(ggplot2)
library(plyr)
library(dplyr)
#library(tidyverse)
library(RColorBrewer)

cols <- brewer.pal(8, "Set1")[-c(3,6)]
cols <- c(cols[1:2], cols[4], cols[3], cols[6], cols[5])


### Percentage of Females in Strokes ###

female_perc <-  ddply(data,.(year), 
                      function(x) with(x,
                                       data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
par(mar = c(6, 4, 1, 2))
female_perc %>%
  ggplot(aes(x = year, y = perc)) +
  geom_area(col = "black", lwd = 1, fill = cols[2]) +
  ylim(0,100) +
  geom_hline(yintercept = 50, col = "dimgrey", lty = "11", lwd = 1) +
  scale_x_continuous(breaks = female_perc$year) +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y = "Percentage (\\%)")


### Percentage of Females in Age Groups ###

agegroups <- cut(data$age, c(18, 64, 74, 84, max(data$age)), include.lowest=TRUE)

female_agegroups1 <- ddply(data[agegroups == "[18,64]",],.(year), 
                           function(x) with(x, 
                                            data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
female_agegroups2 <- ddply(data[agegroups == "(64,74]",],.(year), 
                           function(x) with(x, 
                                            data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
female_agegroups3 <- ddply(data[agegroups == "(74,84]",],.(year), 
                           function(x) with(x, 
                                            data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
female_agegroups4 <- ddply(data[agegroups == "(84,110]",],.(year), 
                           function(x) with(x, 
                                            data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))

female_agegroups <- data.frame(female_agegroups1$year, 
                               female_agegroups1$perc,
                               female_agegroups2$perc,
                               female_agegroups3$perc,
                               female_agegroups4$perc)
names(female_agegroups) <- c("year", "[18,64]", "(64,74]", "(74,84]", "(84,110]")

# ltys <- c("dotdash", "dashed", "twodash", "solid")
par(mar = c(6, 4, 1, 2))
plot(female_agegroups$year, female_agegroups$`[18,64]`, type = "b", 
     col = cols[1], pch = 2, lwd = 1.5, ylim = c(0,100), xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "Percentage (\\%)", cex.lab = 1.2)
lines(female_agegroups$year, female_agegroups$`(64,74]`, type = "b",
      col = cols[2], pch = 3, lwd = 1.5)
lines(female_agegroups$year, female_agegroups$`(74,84]`, type = "b",
      col = cols[3], pch = 4, lwd = 1.5)
lines(female_agegroups$year, female_agegroups$`(84,110]`, type = "b",
      col = cols[4], pch = 5, lwd = 1.5)
abline(c(50,0), lty = "11", col = "dimgrey", lwd = 2)
axis(1, min(female_agegroups$year):max(female_agegroups$year), las = 2)
axis(2, seq(0, 100, by = 10), las = 2)
#legend("topright", c("[18,64]", "(64,74]", "(74,84]", ">84"), col = cols, lty = ltys, lwd = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[18,64]", "(64,74]", "(74,84]", "$>84$"), col = cols[1:4],
       pch = 2:5, lwd = 1.5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 2, bty = 'n')


### Percentage of Females in NIHSS Groups ###

female_nihss0.4 <- ddply(data[data$NIHSS_group == "[0,4]",],.(year), 
                         function(x) with(x, 
                                          data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
female_nihss4.8 <- ddply(data[data$NIHSS_group == "(4,8]",],.(year), 
                         function(x) with(x, 
                                          data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
female_nihss8.12 <- ddply(data[data$NIHSS_group == "(8,12]",],.(year), 
                          function(x) with(x, 
                                           data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
female_nihss12.20 <- ddply(data[data$NIHSS_group == "(12,20]",],.(year), 
                           function(x) with(x, 
                                            data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
female_nihss20.42 <- ddply(data[data$NIHSS_group == "(20,42]",],.(year), 
                           function(x) with(x, 
                                            data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))

female_nihss <- data.frame(female_nihss0.4$year, 
                           female_nihss0.4$perc,
                           female_nihss4.8$perc,
                           female_nihss8.12$perc,
                           female_nihss12.20$perc,
                           female_nihss20.42$perc)
names(female_nihss) <- c("year", "[0,4]", "(4,8]", "(8,12]", "(12,20]", "(20,42]")

# ltys2 <- c("dotdash", "dashed", "twodash", "solid", "longdash")

par(mar = c(6, 4, 1, 2))
plot(female_nihss$year, female_nihss$`[0,4]`, type = "b", 
     col = cols[1], pch = 2, lwd = 1.5, ylim = c(0,100), xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "Percentage (\\%)", cex.lab = 1.3)
lines(female_nihss$year, female_nihss$`(4,8]`, type = "b", pch = 3,
      col = cols[2], lwd = 1.5)
lines(female_nihss$year, female_nihss$`(8,12]`, type = "b", pch = 4,
      col = cols[3], lwd = 1.5)
lines(female_nihss$year, female_nihss$`(12,20]`, type = "b", pch = 5,
      col = cols[4], lwd = 1.5)
lines(female_nihss$year, female_nihss$`(20,42]`, type = "b", pch = 6,
      col = cols[5], lwd = 1.5)
abline(c(50,0), lty = "11", col = "dimgrey", lwd = 2)
axis(1, min(female_nihss$year):max(female_nihss$year), las = 2)
axis(2, seq(0, 100, by = 10), las = 2)
#legend("topright", c("[0,4]", "(4,8]", "(8,12]", "(12,20]", "(20,42]"), col = cols2, lty = ltys2, lwd = 1.5)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[0,4]", "(4,8]", "(8,12]", "(12,20]", "(20,42]"), col = cols[1:5],
       pch = 2:6, lwd = 1.5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 2, bty = 'n')


### Percentage of Females in Age- and NIHSS Groups ###

# filter data for agegroups
agegroup1 <- filter(data, agegroups == "[18,64]")
agegroup2 <- filter(data, agegroups == "(64,74]")
agegroup3 <- filter(data, agegroups == "(74,84]")
agegroup4 <- filter(data, agegroups == "(84,110]")

# calculate the percentage of females for different NIHSS and age groups
nihss_agegroup1 <- list()
nihss_agegroup2 <- list()
nihss_agegroup3 <- list()
nihss_agegroup4 <- list()

for(i in c("[0,4]", "(4,8]", "(8,12]", "(12,20]", "(20,42]")){
  nihss_agegroup1[[i]] <- ddply(agegroup1[agegroup1$NIHSS_group == i,],.(year), 
                                function(x) with(x, 
                                                 data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
  nihss_agegroup2[[i]] <- ddply(agegroup2[agegroup2$NIHSS_group == i,],.(year), 
                                function(x) with(x, 
                                                 data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
  nihss_agegroup3[[i]] <- ddply(agegroup3[agegroup3$NIHSS_group == i,],.(year), 
                                function(x) with(x, 
                                                 data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
  nihss_agegroup4[[i]] <- ddply(agegroup4[agegroup4$NIHSS_group == i,],.(year), 
                                function(x) with(x, 
                                                 data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
}

# plot
par(oma = c(4, 1, 1, 1), mfrow = c(2, 2), mar = c(5, 4, 1, 1))

plot(nihss_agegroup1$`[0,4]`$year, nihss_agegroup1$`[0,4]`$perc, type = "b", cex.lab = 1.3,
     col = cols[1], pch = 2, lwd = 1.5, ylim = c(0,100), xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "Percentage (\\%)")
lines(nihss_agegroup1$`(4,8]`$year, nihss_agegroup1$`(4,8]`$perc, type = "b",
      col = cols[2], pch = 3, lwd = 1.5)
lines(nihss_agegroup1$`(8,12]`$year, nihss_agegroup1$`(8,12]`$perc, type = "b",
      col = cols[3], pch = 4, lwd = 1.5)
lines(nihss_agegroup1$`(12,20]`$year, nihss_agegroup1$`(12,20]`$perc, type = "b",
      col = cols[4], pch = 5, lwd = 1.5)
lines(nihss_agegroup1$`(20,42]`$year, nihss_agegroup1$`(20,42]`$perc, type = "b",
      col = cols[5], pch = 6, lwd = 1.5)
abline(c(50,0), lty = "11", col = "dimgrey", lwd = 2)
axis(1, min(nihss_agegroup1$`[0,4]`$year):max(nihss_agegroup1$`[0,4]`$year), las = 2, cex.axis = 1.1)
axis(2, seq(0, 100, by = 10), las = 2, cex.axis = 1.1)
legend("topright", "Age Group [18,64]", bty = "n", cex = 1.3)

plot(nihss_agegroup2$`[0,4]`$year, nihss_agegroup2$`[0,4]`$perc, type = "b", cex.lab = 1.3,
     col = cols[1], pch = 2, lwd = 1.5, ylim = c(0,100), xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "Percentage (\\%)")
lines(nihss_agegroup2$`(4,8]`$year, nihss_agegroup2$`(4,8]`$perc, type = "b",
      col = cols[2], pch = 3, lwd = 1.5)
lines(nihss_agegroup2$`(8,12]`$year, nihss_agegroup2$`(8,12]`$perc, type = "b",
      col = cols[3], pch = 4, lwd = 1.5)
lines(nihss_agegroup2$`(12,20]`$year, nihss_agegroup2$`(12,20]`$perc, type = "b",
      col = cols[4], pch = 5, lwd = 1.5)
lines(nihss_agegroup2$`(20,42]`$year, nihss_agegroup2$`(20,42]`$perc, type = "b",
      col = cols[5], pch = 6, lwd = 1.5)
abline(c(50,0), lty = "11", col = "dimgrey", lwd = 2)
axis(1, min(nihss_agegroup1$`[0,4]`$year):max(nihss_agegroup1$`[0,4]`$year), las = 2, cex.axis = 1.1)
axis(2, seq(0, 100, by = 10), las = 2, cex.axis = 1.1)
legend("topright", "Age Group (64,74]", bty = "n", cex = 1.3)

plot(nihss_agegroup3$`[0,4]`$year, nihss_agegroup3$`[0,4]`$perc, type = "b", cex.lab = 1.3,
     col = cols[1], pch = 2, lwd = 1.5, ylim = c(0,100), xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "Percentage (\\%)")
lines(nihss_agegroup3$`(4,8]`$year, nihss_agegroup3$`(4,8]`$perc, type = "b",
      col = cols[2], pch = 3, lwd = 1.5)
lines(nihss_agegroup3$`(8,12]`$year, nihss_agegroup3$`(8,12]`$perc, type = "b",
      col = cols[3], pch = 4, lwd = 1.5)
lines(nihss_agegroup3$`(12,20]`$year, nihss_agegroup3$`(12,20]`$perc, type = "b",
      col = cols[4], pch = 5, lwd = 1.5)
lines(nihss_agegroup3$`(20,42]`$year, nihss_agegroup3$`(20,42]`$perc, type = "b",
      col = cols[5], pch = 6, lwd = 1.5)
abline(c(50,0), lty = "11", col = "dimgrey", lwd = 2)
axis(1, min(nihss_agegroup1$`[0,4]`$year):max(nihss_agegroup1$`[0,4]`$year), las = 2, cex.axis = 1.1)
axis(2, seq(0, 100, by = 10), las = 2, cex.axis = 1.1)
legend("topright", "Age Group (74,84]", bty = "n", cex = 1.3)

plot(nihss_agegroup4$`[0,4]`$year, nihss_agegroup4$`[0,4]`$perc, type = "b", cex.lab = 1.3,
     col = cols[1], pch = 2, lwd = 1.5, ylim = c(0,100), xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "Percentage (\\%)")
lines(nihss_agegroup4$`(4,8]`$year, nihss_agegroup4$`(4,8]`$perc, type = "b",
      col = cols[2], pch = 3, lwd = 1.5)
lines(nihss_agegroup4$`(8,12]`$year, nihss_agegroup4$`(8,12]`$perc, type = "b",
      col = cols[3], pch = 4, lwd = 1.5)
lines(nihss_agegroup4$`(12,20]`$year, nihss_agegroup4$`(12,20]`$perc, type = "b",
      col = cols[4], pch = 5, lwd = 1.5)
lines(nihss_agegroup4$`(20,42]`$year, nihss_agegroup4$`(20,42]`$perc, type = "b",
      col = cols[5], pch = 6, lwd = 1.5)
abline(c(50,0), lty = "11", col = "dimgrey", lwd = 2)
axis(1, min(nihss_agegroup1$`[0,4]`$year):max(nihss_agegroup1$`[0,4]`$year), las = 2, cex.axis = 1.1)
axis(2, seq(0, 100, by = 10), las = 2, cex.axis = 1.1)
legend("topright", "Age Group $>84$", bty = "n", cex = 1.3)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[0,4]", "(4,8]", "(4,12]", "(12,20]", "(20,42]"), col = cols[1:5], pch = 2:6, 
       lwd = 2, xpd = TRUE, horiz = TRUE, seg.len = 2, bty = 'n', cex = 1.3)


### Severity in Age Groups - Female ###

females <- filter(data, data$gender == "Female")
agegroups_female <- cut(females$age, c(18, 64, 74, 84, max(females$age)), include.lowest=TRUE)

par(mar = c(6, 4, 1, 2))
females %>%
  ggplot(aes(x = as.factor(year), y = severity)) +
  geom_boxplot(aes(fill = agegroups_female), outlier.size = 0.5, colour = "dimgrey") +
  scale_fill_manual(values=cols[1:4]) +
  #  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "NIHSS Score", fill = "Age Groups") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))


### Severity in Age Groups - Male ###

males <- filter(data, data$gender == "Male")
agegroups_male <- cut(males$age, c(18, 64, 74, 84, max(males$age)), include.lowest=TRUE)

par(mar = c(6, 4, 1, 2))
males %>%
  ggplot(aes(x = as.factor(year), y = severity)) +
  geom_boxplot(aes(fill = agegroups_male), outlier.size = 0.5, colour = "dimgrey") +
  scale_fill_manual(values=cols[1:4]) +
  #  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "NIHSS Score", fill = "Age Groups") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))


### Percentage of fibril == "Ja" in Strokes ###

# percentage of fibril "Ja" over the years
fibril_perc <-  ddply(data,.(year), 
                      function(x) with(x,
                                       data.frame(perc = 100*round(sum(fibril == "Ja")/length(fibril),2))))

par(mar = c(6, 4, 1, 2))
fibril_perc %>%
  ggplot(aes(x = year, y = perc)) +
  geom_area(col = "black", lwd = 1, fill = cols[2]) +
  ylim(0,100) +
  geom_hline(yintercept = 50, col = "dimgrey", lty = "11", lwd = 1) +
  scale_x_continuous(breaks = fibril_perc$year) +
  labs(x = "Year", y = "Percentage (\\%)") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))


### NIHSS Score Means with Smoothing Splines ###

# sort data after years
data_sort <- data[order(data$year),]

# create matrix for s_data
age_sort <- cut(data_sort$age, c(18, 64, 74, 84, 111), include.lowest = TRUE,
                labels = c("1", "2", "3", "4")) # create age groups
data_m <- matrix(c((data_sort$year), 
                   as.numeric(data_sort$severity),
                   as.numeric(data_sort$gender), 
                   as.numeric(data_sort$fibril), 
                   as.numeric(age_sort)), 
                 nrow = nrow(data_sort))
colnames(data_m) <- c("year", "severity", "gender", "fibril", "agegroup")

# Compute changes over the years:
IDvar <- NULL
setID <- 0
mydata <- NULL
years <- unique(data_m[,"year"])
for (i in 1:2){ # gender
  for (j in 1:2){ # fibril
    for (k in 1:4){ # age
      setID <- setID + 1
      for (l in years){ # year
        sel <- ((data_m[,"gender"] == i) & (data_m[,"fibril"] == j) & (data_m[,"agegroup"] == k)
                & (data_m[,"year"] == l))
        #        print(sum(sel))
        if (sum(sel) > 0){
          IDvar <- c(IDvar, rep(setID, sum(sel)))
          mydata <- rbind(mydata, cbind(data_m[sel,], l))
        }
      }
    }
  }
}
mydata[,"year"] <- IDvar
colnames(mydata) <- c("id", "severity", "gender", "fibril", "agegroup", "year")

# averages over years:
mydata.ave <- NULL
ind <- 0
for (i in 1:16){
  for (l in years){
    ind <- ind + 1
    sel <- (mydata[,"year"] == l) & (mydata[,"id"] == i)
    mydata.ave <- rbind(mydata.ave, round(apply(mydata[sel,], 2, mean), 2))
  }
}

myplot <- function(mydata.ave, i, plotnew = TRUE, yvec = c(0,15),...){
  sel <- mydata.ave[,"id"] == i
  xax <- sort(unique(mydata.ave[sel, "year"]))
  if (plotnew){
    plot(xax, mydata.ave[sel, 2], xlab = "Year", ylab = "NIHSS Score", 
         main = paste0("Gender = ", ifelse(unique(mydata.ave[sel, "gender"]) == 1, "Male", "Female"), ", Fibril = ", ifelse(unique(mydata.ave[sel, "fibril"]) == 1, "No", "Yes")),
         cex.lab = 1.3, lwd = 1.5, ylim = yvec, xaxt = "n", yaxt = "n",...)
    axis(1, min(years):max(years), las = 2)
    axis(2, seq(0, 42, by = 5), las = 2)
  }
  else{
    points(xax, mydata.ave[sel, 2],...)
  }
  lines(smooth.spline(xax, mydata.ave[sel, 2], df = 3.8),...)
  #  legend("topright", paste0("Gender = ", ifelse(unique(mydata.ave[sel, "gender"]) == 1, "Male", "Female"), ", Fibril = ", ifelse(unique(mydata.ave[sel, "fibril"]) == 1, "No", "Yes")), bty = "n")
}

par(oma = c(4, 1, 1, 1), mfrow = c(2, 2), mar = c(5, 4, 1, 1))
for (i in 1:4){
  for (j in 1:4){
    ind <- (i-1) * 4 + j
    if (j == 1){
      myplot(mydata.ave, ind, plotnew = TRUE, col = cols[j], pch = j+1)
    }
    else{
      myplot(mydata.ave, ind, plotnew = FALSE, col = cols[j], pch = j+1)
    }
  }
  #  legend("topright", legend = c("[18,64]", "(64,74]", "(74,84]", ">84"), lty = 1, col = cols, pch = 2:5)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[18,64]", "(64,74]", "(74,84]", ">84"), col = cols[1:4], lty = 1, 
       pch = 2:5, lwd = 1.5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 2, bty = 'n')


### NIHSS Score 20%-trimmed Means with Smoothing Splines ###

mydata.trimmed.mean <- NULL
ind <- 0
for (i in 1:16){
  for (l in years){
    ind <- ind + 1
    sel <- (mydata[,"year"] == l) & (mydata[,"id"] == i)
    mydata.trimmed.mean <- rbind(mydata.trimmed.mean, apply(mydata[sel,], 2, mean, trim = 0.1))
  }
}

par(oma = c(4, 1, 1, 1), mfrow = c(2, 2), mar = c(5, 4, 1, 1))
for (i in 1:4){
  for (j in 1:4){
    ind <- (i-1) * 4 + j
    if (j == 1){
      myplot(mydata.trimmed.mean, ind, plotnew = TRUE, col = cols[j], pch = j+1)
    }
    else{
      myplot(mydata.trimmed.mean, ind, plotnew = FALSE, col = cols[j], pch = j+1)
    }
  }
  #  legend("topright", legend = c("[18,64]", "(64,74]", "(74,84]", ">84"), lty = 1, col = cols, pch = 2:5)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[18,64]", "(64,74]", "(74,84]", ">84"), col = cols[1:4], lty = 1, 
       pch = 2:5, lwd = 1.5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 2, bty = 'n')


### NIHSS Score 85th Percentiles with Smoothing Splines ###

mydata.quant85 <- NULL
ind <- 0
for (i in 1:16){
  for (l in years){
    ind <- ind + 1
    sel <- (mydata[,"year"] == l) & (mydata[,"id"] == i)
    mydata.quant85 <- rbind(mydata.quant85, apply(mydata[sel,], 2, quantile, probs = 0.85))
  }
}

par(oma = c(4, 1, 1, 1), mfrow = c(2, 2), mar = c(5, 4, 1, 1))
for (i in 1:4){
  for (j in 1:4){
    ind <- (i-1) * 4 + j
    if (j == 1){
      myplot(mydata.quant85, ind, plotnew = TRUE, yvec = c(0,25), col = cols[j], pch = j+1)
    }
    else{
      myplot(mydata.quant85, ind, plotnew = FALSE, yvec = c(0,25), col = cols[j], pch = j+1)
    }
  }
  #  legend("topright", legend = c("[18,64]", "(64,74]", "(74,84]", ">84"), lty = 1, col = cols, pch = 2:5)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[18,64]", "(64,74]", "(74,84]", ">84"), col = cols, lty = 1, 
       pch = 2:5, lwd = 1.5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 2, bty = 'n')


### Percentage of Stroke Patients in mRS Groups ###

# percentage of mRS values
mRS_count <- table(data$mRS, data$year)
yearsum <- apply(mRS_count, 2, sum)
perc <- as.data.frame(t(mRS_count)/yearsum*100)
colnames(perc) <- c("year", "mRS", "perc")

ggplot(perc, aes(x = year, y = perc, fill = mRS)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  ylab("Percentage (%)") +
  scale_x_discrete(name = "Year", limits = unique(perc$year)) +
  scale_fill_manual(values=cols) +
  #  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))


### Percentage of Stroke Patients in mRS and Age Groups ###

# mRS values in age groups
agegroups <- cut(data$age, c(18, 64, 74, 84, max(data$age)), include.lowest = TRUE)
perc_group <- group_by(cbind(data, agegroups), year, .add = TRUE)
perc_group <- group_by(perc_group, agegroups, .add = TRUE)
perc_group <- group_by(perc_group, mRS, .add = TRUE)
perc_count <- as.data.frame(summarise(perc_group, n = n()))

for(i in 1:360){
  perc_count$perc[i] <- perc_count$n[i]/sum(perc_count[perc_count$agegroups == perc_count$agegroups[i] & perc_count$year == perc_count$year[i], "n"])*100
}

ggplot(perc_count, aes(x = year, y = perc, fill = mRS)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  facet_wrap(~ agegroups) +
  ylab("Percentage (%)") +
  scale_x_discrete(name = "Year", limits = unique(perc_count$year)) +
  scale_fill_manual(values=cols) +
  #  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust=1))


### NIHSS Score in mRS Groups ###

data %>%
  ggplot(aes(x = as.factor(year), y = severity)) +
  geom_boxplot(aes(fill = mRS), outlier.size = 0.5, colour = "dimgrey") +
  scale_fill_manual(values=cols) +
  #  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "NIHSS Score") +
  theme(legend.position="bottom", text = element_text(size = 12), 
        axis.text.x = element_text(angle = 90, hjust=1))


### Percentage of TACS in Strokes ###

tacs_perc <- ddply(data,.(year), 
                   function(x) with(x,
                                    data.frame(perc = 100*round(sum(TACS == "Ja")/length(TACS),2))))
par(mar = c(6, 4, 1, 2))
tacs_perc %>%
  ggplot(aes(x = year, y = perc)) +
  geom_area(col = "black", lwd = 1, fill = cols[2]) +
  ylim(0,100) +
  geom_hline(yintercept = 50, col = "dimgrey", lty = "11", lwd = 1) +
  scale_x_continuous(breaks = fibril_perc$year) +
  labs(x = "Year", y = "Percentage (\\%)") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))


### Percentage of Females with TACS ###

female_tacs <- ddply(data[data$TACS == "Ja",],.(year), 
                     function(x) with(x,
                                      data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))

female_tacs %>%
  ggplot(aes(x = year, y = perc)) +
  geom_area(col = "black", lwd = 1, fill = cols[2]) +
  ylim(0,100) +
  geom_hline(yintercept = 50, col = "dimgrey", lty = "11", lwd = 1) +
  scale_x_continuous(breaks = fibril_perc$year) +
  labs(x = "Year", y = "Percentage (\\%)") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))


### Percentage of TACS in Age Groups ###

tacs_agegroups1 <- ddply(data[agegroups == "[18,64]",],.(year), 
                         function(x) with(x,
                                          data.frame(perc = 100*round(sum(TACS == "Ja")/length(TACS),2))))
tacs_agegroups2 <- ddply(data[agegroups == "(64,74]",],.(year), 
                         function(x) with(x,
                                          data.frame(perc = 100*round(sum(TACS == "Ja")/length(TACS),2))))
tacs_agegroups3 <- ddply(data[agegroups == "(74,84]",],.(year), 
                         function(x) with(x,
                                          data.frame(perc = 100*round(sum(TACS == "Ja")/length(TACS),2))))
tacs_agegroups4 <- ddply(data[agegroups == "(84,110]",],.(year), 
                         function(x) with(x,
                                          data.frame(perc = 100*round(sum(TACS == "Ja")/length(TACS),2))))

par(mar = c(6, 4, 1, 2))
plot(tacs_agegroups1$year, tacs_agegroups1$perc, type = "b", pch = 2, 
     col = cols[1], lwd = 1.5, ylim = c(0,100), xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "Percentage (\\%)", cex.lab = 1.2)
lines(tacs_agegroups2$year, tacs_agegroups2$perc, type = "b", pch = 3,
      col = cols[2], lwd = 1.5)
lines(tacs_agegroups3$year, tacs_agegroups3$perc, type = "b", pch = 4,
      col = cols[3], lwd = 1.5)
lines(tacs_agegroups4$year, tacs_agegroups4$perc, type = "b", pch = 5,
      col = cols[4], lwd = 1.5)
abline(c(50,0), lty = "11", col = "dimgrey", lwd = 2)
axis(1, min(tacs_agegroups1$year):max(tacs_agegroups1$year), las = 2)
axis(2, seq(0, 100, by = 10), las = 2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[18,64]", "(64,74]", "(74,84]", "$>84$"), col = cols[1:4],
       pch = 2:5, lwd = 1.5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 2, bty = 'n')


### Percentage of Female TACS in Age Groups ###

tacs_yes <- data[data$TACS == "Ja",]
agegroups_tacs <- cut(tacs_yes$age, c(18, 64, 74, 84, max(tacs_yes$age)), include.lowest=TRUE)

tacs_yes_agegroup1 <- ddply(tacs_yes[agegroups_tacs == "[18,64]",],.(year), 
                            function(x) with(x,
                                             data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
tacs_yes_agegroup2 <- ddply(tacs_yes[agegroups_tacs == "(64,74]",],.(year), 
                            function(x) with(x,
                                             data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
tacs_yes_agegroup3 <- ddply(tacs_yes[agegroups_tacs == "(74,84]",],.(year), 
                            function(x) with(x,
                                             data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))
tacs_yes_agegroup4 <- ddply(tacs_yes[agegroups_tacs == "(84,110]",],.(year), 
                            function(x) with(x,
                                             data.frame(perc = 100*round(sum(gender == "Female")/length(gender),2))))

par(mar = c(6, 4, 1, 2))
plot(tacs_yes_agegroup1$year, tacs_yes_agegroup1$perc, type = "b", pch = 2, 
     col = cols[1], lwd = 1.5, ylim = c(0,100), xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "Percentage (\\%)", cex.lab = 1.2)
lines(tacs_yes_agegroup2$year, tacs_yes_agegroup2$perc, type = "b", pch = 3,
      col = cols[2], lwd = 1.5)
lines(tacs_yes_agegroup3$year, tacs_yes_agegroup3$perc, type = "b", pch = 4,
      col = cols[3], lwd = 1.5)
lines(tacs_yes_agegroup4$year, tacs_yes_agegroup4$perc, type = "b", pch = 5,
      col = cols[4], lwd = 1.5)
abline(c(50,0), lty = "11", col = "dimgrey", lwd = 2)
axis(1, min(tacs_yes_agegroup1$year):max(tacs_yes_agegroup1$year), las = 2)
axis(2, seq(0, 100, by = 10), las = 2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("[18,64]", "(64,74]", "(74,84]", "$>84$"), col = cols[1:4],
       pch = 2:5, lwd = 1.5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 2, bty = 'n')
