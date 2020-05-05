library(MDP2)

K <- 3                      # Max queue length options + the option of 0 cars
num_states <- 1:(2*K^4)     # Number of states
F <- 4                      # Number of lanes
k <- c(rep(0,F),1)          # Number of cars in each row. Initially 0 in each row.
N <- 150                    # Number of periods
lights <- c('G1R2','Y1R2', 'R11R2', 'R1R22', 'R1Y2', 'R1G2')  # All combinations of lights

# Does any new cars arrive?
rho <- c(0.8,0.8,0.8,0.8)  # Workload per lane
q <- rho/2   # Probability of the arrival of a car lane f

# Cost function
cost <- function(k) {
  sum(k[1:F], na.rm = FALSE)  # use negative numbers since the MDP optimize based on rewards
}

# Transition probabilities. Input state i and action i
l_13 <- data.frame(light_id = c(0,1), flow = paste0("f=", 1:F))  # status of the light. Flow 1 and 3 has green light
l_24 <- data.frame(light_id = c(1,0), flow = paste0("f=", 1:F))  # Flow 2 and 4 has green light

# Creating states
L <- data.frame(Num = 1:length(lights), Light = lights)  # light states
car_states <- expand.grid('Flow 1' = 0:(K-1), 'Flow 2' = 0:(K-1), 'Flow 3' = 0:(K-1), 'Flow 4' = 0:(K-1), 'Light' = 1)  # car states

# Create state as a combination of light and car states
tot_states <- matrix(nrow=nrow(car_states)*length(lights),ncol=5)
colnames(tot_states) <- c(flow = paste0("Flow", 1:F), 'Light')
for (i in (1:nrow(car_states))) {
  y <- max(1,(i-1)*nrow(L)+1)
  for (j in (1:nrow(L))) {
    for (ii in (1:F)) {
      tot_states[y+(j-1),ii] <- car_states[i,ii]  # put in car flow
    }
    tot_states[y+(j-1),F+1] <- j  # put in light status
  }
}

# To naming of all possible states later
states_label <- data.frame(label = paste('i =', 0:(nrow(tot_states)-1)))

transPr <- function(a,k) {
  prob <- c(rep(0,nrow(tot_states)))  # Save total probabilities here
  f <- c(rep(0,F))  # Save flow probabilities here
  if (k[F+1] == 3) {  # R1R2 --> G1R2
    for (i in 1:nrow(tot_states)) {
      if ((tot_states[i,F+1] == 1)) {
        f <- c(rep(0,F))  # Save flow probabilities here
        for (ii in 1:F) {
          if (tot_states[i,ii] == (K-1) & k[ii] == (K-1)) f[ii] <- 1
          else if (tot_states[i,ii] == k[ii]+1) f[ii] <- q[ii]
          else if (tot_states[i,ii] == k[ii]) f[ii] <- 1-q[ii]
        }
        y <- prod(f)  # Transition probability to state i
        prob[i] <- y
      }
    }
  } else if (k[F+1] == 4) {  # R1R2 --> R1G2
    for (i in 1:nrow(tot_states)) {
      if ((tot_states[i,F+1] == 6)) {
        f <- c(rep(0,F))  # Save flow probabilities here
        for (ii in 1:F) {
          if (tot_states[i,ii] == (K-1) & k[ii] == (K-1)) f[ii] <- 1
          else if (tot_states[i,ii] == k[ii]+1) f[ii] <- q[ii]
          else if (tot_states[i,ii] == k[ii]) f[ii] <- 1-q[ii]
        }
        y <- prod(f)  # Transition probability to state i
        prob[i] <- y
      }
    }
  } else if (k[F+1] == 2) {  # Y1R2
    for (i in 1:nrow(tot_states)) {
      if ((tot_states[i,F+1] == 3)) {  # Check if state i's light state is possible
        f <- c(rep(0,F))  # Save flow probabilities here
        for (ii in 1:F) {
          if (ii==1 || ii==3) {
            if (tot_states[i,ii] == 0 & k[ii] == 0) f[ii] <- 1
            else if (tot_states[i,ii] == k[ii]) f[ii] <- q[ii]
            else if (tot_states[i,ii] == k[ii]-1) f[ii] <- 1-q[ii]
          }
          if (ii==2 || ii==4) {
            if (tot_states[i,ii] == (K-1) & k[ii] == (K-1)) f[ii] <- 1
            else if (tot_states[i,ii] == k[ii]+1) f[ii] <- q[ii]
            else if (tot_states[i,ii] == k[ii]) f[ii] <- 1-q[ii]
          }
        }
        y <- prod(f)  # Transition probability to state i
        prob[i] <- y
      }
    }
  } else if (k[F+1] == 5) {  # R1Y2
    for (i in 1:nrow(tot_states)) {
      if ((tot_states[i,F+1] == 4)) {
        f <- c(rep(0,F))  # Save flow probabilities here
        for (ii in 1:F) {
          if (ii==1 || ii==3) {  # Flows has red light
            if (tot_states[i,ii] == (K-1) & k[ii] == (K-1)) f[ii] <- 1
            else if (tot_states[i,ii] == k[ii]+1) f[ii] <- q[ii]
            else if (tot_states[i,ii] == k[ii]) f[ii] <- 1-q[ii]
          }
          if (ii==2 || ii==4) {  # Flows has green or yellow light
            if (tot_states[i,ii] == 0 & k[ii] == 0) f[ii] <- 1
            else if (tot_states[i,ii] == k[ii]) f[ii] <- q[ii]
            else if (tot_states[i,ii] == k[ii]-1) f[ii] <- 1-q[ii]
          }
        }
        y <- prod(f)  # Transition probability to state i
        prob[i] <- y
      }
    }
  } else if (a==0) {  # Do not change light
    if (k[F+1] == 1) {   # G1R2
      for (i in 1:nrow(tot_states)) {
        if ((tot_states[i,F+1]) ==  1){  # Check if state i's light state is possible
          f <- c(rep(0,F))  # Save flow probabilities here
          for (ii in 1:F) {
            if (ii==1 || ii==3) {  # Flows with green or yellow light
              if (tot_states[i,ii] == 0 & k[ii] == 0) f[ii] <- 1
              else if (tot_states[i,ii] == k[ii]) f[ii] <- q[ii]
              else if (tot_states[i,ii] == k[ii]-1) f[ii] <- 1-q[ii]
            }
            if (ii==2 || ii==4) {  # Flows with red light
              if (tot_states[i,ii] == (K-1) & k[ii] == (K-1)) f[ii] <- 1
              else if (tot_states[i,ii] == k[ii]+1) f[ii] <- q[ii]
              else if (tot_states[i,ii] == k[ii]) f[ii] <- 1-q[ii]
            }
          }
          y <- prod(f)  # Transition probability to state i
          prob[i] <- y
        }
      }
    }
    if (k[F+1] == 6) {  # R1G2
      for (i in 1:nrow(tot_states)) {
        if ((tot_states[i,F+1] == 6)) {
          f <- c(rep(0,F))  # Save flow probabilities here
          for (ii in 1:F) {
            if (ii==1 || ii==3) {
              if (tot_states[i,ii] == (K-1) & k[ii] == (K-1)) f[ii] <- 1
              else if (tot_states[i,ii] == k[ii]+1) f[ii] <- q[ii]
              else if (tot_states[i,ii] == k[ii]) f[ii] <- 1-q[ii]
            }
            if (ii==2 || ii==4) {
              if (tot_states[i,ii] == 0 & k[ii] == 0) f[ii] <- 1
              else if (tot_states[i,ii] == k[ii]) f[ii] <- q[ii]
              else if (tot_states[i,ii] == k[ii]-1) f[ii] <- 1-q[ii]
            }
          }
          y <- prod(f)  # Transition probability to state i
          prob[i] <- y
        }
      }
    }
    
  } else if (a==1) {  # Change light
    if (k[F+1] == 1) {  # G1R1 --> 
      for (i in 1:nrow(tot_states)) {
        if ((tot_states[i,F+1] == 2)) {
          f <- c(rep(0,F))  # Save flow probabilities here
          for (ii in 1:F) {
            if (ii==1 || ii==3) {
              if (tot_states[i,ii] == 0 & k[ii] == 0) f[ii] <- 1
              else if (tot_states[i,ii] == k[ii]) f[ii] <- q[ii]
              else if (tot_states[i,ii] == k[ii]-1) f[ii] <- 1-q[ii]
            }
            if (ii==2 || ii==4) {
              if (tot_states[i,ii] == (K-1) & k[ii] == (K-1)) f[ii] <- 1
              else if (tot_states[i,ii] == k[ii]+1) f[ii] <- q[ii]
              else if (tot_states[i,ii] == k[ii]) f[ii] <- 1-q[ii]
            }
          }
          y <- prod(f)  # Transition probability to state i
          prob[i] <- y
        }
      }
    }
    if (k[F+1] == 6) {  # R1G2
      for (i in 1:nrow(tot_states)) {
        if ((tot_states[i,F+1] == 5)) {
          f <- c(rep(0,F))  # Save flow probabilities here
          for (ii in 1:F) {
            if (ii==1 || ii==3) {  # Red light
              if (tot_states[i,ii] == (K-1) & k[ii] == (K-1)) f[ii] <- 1
              else if (tot_states[i,ii] == k[ii]+1) f[ii] <- q[ii]
              else if (tot_states[i,ii] == k[ii]) f[ii] <- 1-q[ii]
            }
            if (ii==2 || ii==4) {  # Green light
              if (tot_states[i,ii] == 0 & k[ii] == 0) f[ii] <- 1
              else if (tot_states[i,ii] == k[ii]) f[ii] <- q[ii]
              else if (tot_states[i,ii] == k[ii]-1) f[ii] <- 1-q[ii]
            }
          }
          y <- prod(f)  # Transition probability to state i
          prob[i] <- y
        }
      }
    }
  }
  
  id <- NULL
  pr <- NULL
  for (i in 1:length(prob)) {  # Find ID's and PR's
    if (prob[i] > 0) {
      id <- c(id, i-1)
      pr <- c(pr, prob[i])
    }
  }
  return(list(pr = pr, id = id))
}

w <- binaryMDPWriter("traffic_light2_")
w$setWeights(c("Duration", "Cost"))
w$process()
  w$stage()
    for (ii in 1:nrow(tot_states)) {
      w$state(label = states_label$label[ii])
      ss <- unname(unlist(tot_states[ii,], use.names = FALSE))  # Find relevant queue according to state number in tot_states
      dat <- transPr(0,ss)
      w$action(label = "No_light_change", weights=c(1,-cost(ss)), pr=dat$pr, id=dat$id, end=TRUE)  # Negative cost because MDP2 solves for revenue
      
      dat <- transPr(1,ss)
      w$action(label = "Light_change", weights=c(1,-cost(ss)), pr=dat$pr, id=dat$id, end=TRUE)  # Negative cost because MDP2 solves for revenue
      w$endState()
    }
  w$endStage()
w$endProcess()
w$closeWriter()

mdp2 <-loadMDP("traffic_light2_")

# Diverse test af MDP
info <- infoMDP(mdp2, withHarc = T)
info$actionDF

# Plot state-expanded hypergraph
gId <- 1:(486*2)
states <- data.frame(sId = info$stateDF$sId, gId = c(gId[seq_along(gId) %% 2 == 0], gId[seq_along(gId) %% 2 > 0]), label = info$stateDF$label)
actions <- cbind(info$harcDF, info$actionDF$label, lwd = 1, lty = 1, col = "blue", highlight = F)
pdf(file = "hypergf.pdf", width = 10, height = 200, family = "Times")
plotHypergraph(gridDim = c(487,2), states, actions, rady = 0.0005)
dev.off()

# Is it unichain?
policyIteAve(mdp2,"Cost", "Duration")
getPolicy(mdp2)

# Seems to work
valueIte(mdp2, "Cost", "Duration", maxIte = 10000, discountFactor = 0.95)
getPolicy(mdp2)
