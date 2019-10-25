## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load package and data, include = FALSE------------------------------
library(mHMMbayes)
nonverbal <- data.frame(nonverbal)
head(nonverbal)
old_par <- graphics::par(no.readonly =TRUE)

## ----plot observed data, fig.width = 7.2, fig.height = 3.5, echo = FALSE----
# set labels and colors for the observed behavioral categorical outcomes
library(RColorBrewer)
Voc_lab <- c("Not Speaking", "Speaking", "Back channeling")
Look_lab <-  c("Not looking", "Looking")
Voc_col <- c(brewer.pal(3,"PuBuGn")[c(1,3,2)])
Look_col <- c(brewer.pal(3,"YlOrRd")[-3])
cols = list(Voc_col, Look_col, Voc_col, Look_col)

time_s  <- seq(1,900)
couple1 <- cbind(nonverbal[nonverbal$id == 1,], time_s)

par(mar = c(4.3, 6.6, 2.1, 1.1))
plot(x = 1, xlim = c(0,300), ylim = c(0.5,6), type = "n", las = 1, xlab = "Time in minutes", xaxt = "n", yaxt = "n", ylab = "")
axis(2, at = seq(1,4), tick = FALSE, labels = c("P_vocalizing", "P_Looking", "T_vocalizing", "T_Looking"), las = 1)
axis(1, at = seq(0,300,60), tick = TRUE, las = 1, labels = FALSE)
axis(1, at = seq(0,300,60), tick = FALSE, las = 1, labels = seq(1,6,1))
abline(v = seq(0,300,60), col = "gray85")

for(j in 2:5){
  for(i in 1:max(nonverbal[,j])){
    points(x = couple1$time_s[1:300][couple1[1:300,j] == i], 
           y = rep(j-1, sum(couple1[1:300,j] == i)), 
           pch = "|", col = cols[[j-1]][i])
  }
}

legend("topright", bty = "n", fill = Voc_col, legend = Voc_lab)
legend("topleft", bty = "n", fill = Look_col, legend = Look_lab)

graphics::par(old_par)


## ----settings and load 2 state model, include = FALSE--------------------
# specifying general model properties:
m <- 2
n_dep <- 4
q_emiss <- c(3, 2, 3, 2)

# specifying starting values
start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM <- list(matrix(c(0.05, 0.90, 0.05, 
                          0.90, 0.05, 0.05), byrow = TRUE,
                         nrow = m, ncol = q_emiss[1]), # vocalizing patient
                  matrix(c(0.1, 0.9, 
                           0.1, 0.9), byrow = TRUE, nrow = m,
                         ncol = q_emiss[2]), # looking patient
                  matrix(c(0.90, 0.05, 0.05, 
                           0.05, 0.90, 0.05), byrow = TRUE,
                         nrow = m, ncol = q_emiss[3]), # vocalizing therapist
                  matrix(c(0.1, 0.9, 
                           0.1, 0.9), byrow = TRUE, nrow = m,
                         ncol = q_emiss[4])) # looking therapist

load("nonv_2st_1000it.rda")
out_2st <- out1

## ----show specifying 2 state model, eval = FALSE-------------------------
#  library(mHMMbayes)
#  # specifying general model properties:
#  m <- 2
#  n_dep <- 4
#  q_emiss <- c(3, 2, 3, 2)
#  
#  # specifying starting values
#  start_TM <- diag(.8, m)
#  start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
#  start_EM <- list(matrix(c(0.05, 0.90, 0.05,
#                            0.90, 0.05, 0.05), byrow = TRUE,
#                           nrow = m, ncol = q_emiss[1]), # vocalizing patient
#                    matrix(c(0.1, 0.9,
#                             0.1, 0.9), byrow = TRUE, nrow = m,
#                           ncol = q_emiss[2]), # looking patient
#                    matrix(c(0.90, 0.05, 0.05,
#                             0.05, 0.90, 0.05), byrow = TRUE,
#                           nrow = m, ncol = q_emiss[3]), # vocalizing therapist
#                    matrix(c(0.1, 0.9,
#                             0.1, 0.9), byrow = TRUE, nrow = m,
#                           ncol = q_emiss[4])) # looking therapist

## ----show fitting 2 state model, eval = FALSE----------------------------
#  # Run a model without covariate(s) and default priors:
#  set.seed(14532)
#  out_2st <- mHMM(s_data = nonverbal,
#                      gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
#                      start_val = c(list(start_TM), start_EM),
#                      mcmc = list(J = 1000, burn_in = 200))
#  

## ----show print model----------------------------------------------------
out_2st

## ----show summary model--------------------------------------------------
summary(out_2st)

## ----show obtain gamma function------------------------------------------
# When not specified, level defaults to "group"
gamma_pop <- obtain_gamma(out_2st)
gamma_pop

# To obtain the subject specific parameter estimates:
gamma_subj <- obtain_gamma(out_2st, level = "subject")
gamma_subj

## ----show plot posterior densities, fig.width = 7.2, fig.height = 4------
library(RColorBrewer)
Voc_col <- c(brewer.pal(3,"PuBuGn")[c(1,3,2)])
Voc_lab <- c("Not Speaking", "Speaking", "Back channeling")

plot(out_2st, component = "emiss", dep = 1, col = Voc_col, 
     parameter = "emiss", dep_lab = c("Patient vocalizing"), cat_lab = Voc_lab)

## ----show plot transition prob, fig.show='hold'--------------------------
# Transition probabilities at the group level and for subject number 1, respectively:
plot(gamma_pop, col = rep(rev(brewer.pal(3,"PiYG"))[-2], each = m))
plot(gamma_subj, subj_nr = 1, col = rep(rev(brewer.pal(3,"PiYG"))[-2], each = m))

## ----load 3 and 4 state models, include = FALSE--------------------------
# load("nonv_3st_1000it.rda")
# out_3st <- out2
load("nonv_4st_1000it.rda")
out_4st <- out3

## ----show 4 state model, fig.width = 5, fig.height = 3-------------------
summary(out_4st)

m <- 4
plot(obtain_gamma(out_4st), cex = .5, col = rep(rev(brewer.pal(5,"PiYG"))[-3], each = m))

## ----using viterbi algorithm---------------------------------------------
state_seq <- vit_mHMM(out_2st, s_data = nonverbal)
 head(state_seq)

## ----plotting observed data plus inferred states, fig.width = 7.2, fig.height = 4, echo = FALSE----
# set labels and colors for the observed behavioral categorical outcomes
Voc_lab <- c("Not Speaking", "Speaking", "Back channeling")
Look_lab <-  c("Not looking", "Looking")
Voc_col <- c(brewer.pal(3,"PuBuGn")[c(1,3,2)])
Look_col <- c(brewer.pal(3,"YlOrRd")[-3])
cols = list(Voc_col, Look_col, Voc_col, Look_col)

State_col <- c(rev(brewer.pal(3,"PiYG"))[-2])

time_s  <- seq(1,900)
couple1 <- cbind(nonverbal[nonverbal$id == 1,], time_s)

par(mar = c(4.3, 6.6, 2.1, 1.1))
plot(x = 1, xlim = c(0,300), ylim = c(-0.5,6), type = "n", las = 1, xlab = "Time in minutes", xaxt = "n", yaxt = "n", ylab = "")
axis(2, at = seq(0,4), tick = FALSE, labels = c("State", "P_vocalizing", "P_Looking", "T_vocalizing", "T_Looking"), las = 1)
axis(1, at = seq(0,300,60), tick = TRUE, las = 1, labels = FALSE)
axis(1, at = seq(0,300,60), tick = FALSE, las = 1, labels = seq(1,6,1))
abline(v = seq(0,300,60), col = "gray85")

for(j in 2:5){
  for(i in 1:max(nonverbal[,j])){
    points(x = couple1$time_s[1:300][couple1[1:300,j] == i], 
           y = rep(j-1, sum(couple1[1:300,j] == i)), 
           pch = "|", col = cols[[j-1]][i])
  }
}
for(i in 1:2){
  points(x = couple1$time_s[1:300][state_seq[1:300,1] == i], 
           y = rep(0, sum(state_seq[1:300,1] == i)), 
           pch = "|", col = State_col[i])
}

legend("topright", bty = "n", fill = c(Look_col, "white", Voc_col), legend = c(Look_lab, "",Voc_lab),
       ncol = 2, border = c(rep("black", 2), "white", rep("black", 3)))
legend("topleft", bty = "n", fill = State_col, legend = c("State 1", "State 2"))

graphics::par(old_par)


## ----loading model convergence, include = FALSE--------------------------
# specifying general model properties
m <-2
n_dep <- 4
q_emiss <- c(3, 2, 3, 2)

# specifying different starting values
start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM_b <- list(matrix(c(0.2, 0.6, 0.2,
                            0.6, 0.2, 0.2), byrow = TRUE,
                        nrow = m, ncol = q_emiss[1]), # vocalizing patient
                 matrix(c(0.4, 0.6,
                          0.4, 0.6), byrow = TRUE, nrow = m,
                        ncol = q_emiss[2]), # looking patient
                 matrix(c(0.6, 0.2, 0.2,
                          0.2, 0.6, 0.2), byrow = TRUE,
                        nrow = m, ncol = q_emiss[3]), # vocalizing therapist
                 matrix(c(0.4, 0.6,
                          0.4, 0.6), byrow = TRUE, nrow = m,
                        ncol = q_emiss[4])) # looking therapist

load("nonv_2stb_1000it.rda")
out_2st_b <- out1b

## ----showing model convergence I, eval= FALSE----------------------------
#  # specifying general model properties
#  m <-2
#  n_dep <- 4
#  q_emiss <- c(3, 2, 3, 2)
#  
#  # specifying different starting values
#  start_TM <- diag(.8, m)
#  start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
#  start_EM_b <- list(matrix(c(0.2, 0.6, 0.2,
#                              0.6, 0.2, 0.2), byrow = TRUE,
#                          nrow = m, ncol = q_emiss[1]), # vocalizing patient
#                   matrix(c(0.4, 0.6,
#                            0.4, 0.6), byrow = TRUE, nrow = m,
#                          ncol = q_emiss[2]), # looking patient
#                   matrix(c(0.6, 0.2, 0.2,
#                            0.2, 0.6, 0.2), byrow = TRUE,
#                          nrow = m, ncol = q_emiss[3]), # vocalizing therapist
#                   matrix(c(0.4, 0.6,
#                            0.4, 0.6), byrow = TRUE, nrow = m,
#                          ncol = q_emiss[4])) # looking therapist
#  
#  # Run a model identical to out_2st, but with different starting values:
#  set.seed(9843)
#  out_2st_b <- mHMM(s_data = nonverbal,
#                        gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
#                        start_val = c(list(start_TM), start_EM),
#                        mcmc = list(J = 1000, burn_in = 200))
#  

## ----showing model convergence II trace plots, fig.width = 7.2, fig.height = 7----
par(mfrow = c(m,q_emiss[2]))
for(i in 1:m){
  for(q in 1:q_emiss[2]){
     plot(x = 1:1000, y = out_2st$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], 
          ylim = c(0,1.4), yaxt = 'n', type = "l", ylab = "Transition probability",
          xlab = "Iteration", main = paste("Patient", Look_lab[q], "in state", i), col = "#8da0cb") 
    axis(2, at = seq(0,1, .2), las = 2)
    lines(x = 1:1000, y = out_2st_b$emiss_prob_bar[[2]][,(i-1) * q_emiss[2] + q], col = "#e78ac3")
    legend("topright", col = c("#8da0cb", "#e78ac3"), lwd = 2, 
           legend = c("Starting value set 1", "Starting value set 2"), bty = "n")
  }
}

## ---- include = FALSE----------------------------------------------------
graphics::par(old_par)

