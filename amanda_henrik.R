library(MDP2)

K <- 3                      # Max queue length options + the option of 0 cars
num_states <- 1:(2*K^4)     # Number of states
F <- 4                      # Number of lanes
k <- rep(0,F)               # Number of cars in each row. Initially 0 in each row.
N <- 150                    # Number of periods

# States
states <- data.frame(light_id = c(0,1), label = paste0("f=", 1:F), stringsAsFactors = FALSE)
# If light_id = 0 then the light is red, if light_id = 1 then the light i green in flow i

# Does any new cars arrive?
rho <- c(0.8,0.8,0.8,0.8)  # Workload per lane
q <- rho/2   # Probability of the arrival of a car lane f

# The function to be used to check where cars arrived
car_arrival <- function() {
   for (i in 1:F) {
      if (k[i] < K) {
         if (rbinom(1,1,q[i]) == 1) {
            k[i] <<- k[i]+1   # Update outside function scope
         }
      }
   }
}

# Cost function
cost <- function(k) {
   sum(k, na.rm = FALSE)  # use negative numbers since the MDP optimize based on rewards
}

# Transition probabilities. Input state i and action i
l_13 <- data.frame(light_id = c(0,1), flow = paste0("f=", 1:F))  # status of the light. Flow 1 and 3 has green light
l_24 <- data.frame(light_id = c(1,0), flow = paste0("f=", 1:F))  # Flow 2 and 4 has green light
mat_x_values <- matrix(0:(K-1), nrow = 1, ncol = K)

tot_states <- expand.grid('Flow 1' = 0:(K-1), 'Flow 2' = 0:(K-1), 'Flow 3' = 0:(K-1), 'Flow 4' = 0:(K-1))  # All combinations of states
states_label <- data.frame(label = paste('i =', 1:nrow(tot_states)), stringsAsFactors = FALSE)  # All possible states

# NECESSARY FUNCTION TO FIND transPr
# Find id number
find_id <- function(pos_states) {
   id <- c()
   for (i in 1:nrow(pos_states)) {
      for (j in 1:nrow(tot_states)) {
         if (all(tot_states[j,]==pos_states[i,])) id <- c(id,j)
      }
   }
   return(id)
}

# Find pr from mat
find_pr <- function(pos_states, mat) {
   pr <- c()
   for (i in 1:nrow(pos_states)) {
      x <- c()  # List of probabilities
      for (j in 1:ncol(pos_states)) {
         y <- pos_states[i,j]
         x <- c(x, mat[j,y+1])
      }
      pr <- c(pr, prod(x))
   }
   return(pr)
}

# TransPr
transPr <- function(l,k) {
   # l: list to define states of light
   # k: queue
   # f: flow number
   # Return: pr: sandsynligheden for at komme i det tilsvarende state, som er angivet i id
   mat <- matrix(0, nrow = length(k), ncol = K)
   for (i in 1:length(k)) {
      if (l[i,1]==0) {                 # Flow has green light
         if (k[i]==0) {                 # If minimum number of cars in flow
            mat[i,k[i]+1] <- 1
            #} else if (k[i]==K-1) {        # If maximum number of cars in flow (unødvendigt)
            #mat[i,k[i]+1] <- q[i]
            #mat[i,k[i]] <- 1-q[i] 
         } else {
            mat[i,k[i]+1] <- q[i]        # k_f = k_f'
            mat[i,max(0,k[i])] <- 1-q[i] # k_f = max(0,k_f-1)
         }
      }
      if (l[i,1]==1) {                 # Flow has red light
         if (k[i]==K-1) {
            mat[i,k[i]+1] <- 1
         } else {
            mat[i,k[i]+2] <- q[i]        # k_f' = k_f+1
            mat[i,k[i]+1] <- 1-q[i]      # k_f' = k_f+1
         }
      }
   }
   p <- list()
   for (i in 1:length(k)) {                   # Possible combinations of states
      v <- c()
      for (j in 1:K) {
         if (mat[i,j] > 0) {
            v <- c(v,j-1)
         }
      }
      p <- c(p,list(v))
   }
   pos_states <- expand.grid(c(p[[1]]), c(p[[2]]), c(p[[3]]), c(p[[4]]))
   return(list(pr = find_pr(pos_states, mat), id = find_id(pos_states)))
}

# The MDP model

# Test: Define dummy action
w<-binaryMDPWriter("traffic_light_")
w$setWeights(c("Duration", "Cost"))
w$process()
   w$stage()
      for (ii in 1:nrow(tot_states)) {
         w$state(label = states_label$label[ii])
      #    # ss <- unlist(tot_states[ii,], use.names = FALSE)  # Find relevant queue according to state number in tot_states
      #    # dat <- transPr(l_13,ss)
      #    # w$action(label = "G_1,R_2", weights=c(1,-cost(ss)), pr=dat$pr, id=dat$id, end=TRUE)
         
         w$action(label = "G_1,R_2", weights=c(1,3), pr=1, id=0, end=TRUE)
      #    
      #    # dat <- transPr(l_24,ss)
      #    # w$action(label = "R_1,G_2", weights=c(1,-cost(ss)), pr=dat$pr, id=dat$id, end=TRUE)
      #    # w$endAction() should not be here since you have end = TRUE in the previos line
         w$endState() # should be inside the loop
      }
   w$endStage()
w$endProcess()
w$closeWriter()      
mdp <-loadMDP("traffic_light_")
mdp
# Diverse test af MDP
info <- infoMDP(mdp)
info$actionDF
info$stateDF


# Test: Define actions with dummy pr and id
w<-binaryMDPWriter("traffic_light_")
w$setWeights(c("Duration", "Cost"))
w$process()
w$stage()
for (ii in 1:nrow(tot_states)) {
   w$state(label = states_label$label[ii])
      ss <- unlist(tot_states[ii,], use.names = FALSE)  # Find relevant queue according to state number in tot_states
      dat <- transPr(l_13,ss)
      w$action(label = "G_1,R_2", weights=c(1,-cost(ss)), pr=1, id=0, end=TRUE)
      
      dat <- transPr(l_24,ss)
      w$action(label = "R_1,G_2", weights=c(1,-cost(ss)), pr=1, id=0, end=TRUE)
      #w$endAction() should not be here since you have end = TRUE in the previos line
   w$endState() # should be inside the loop
}
w$endStage()
w$endProcess()
w$closeWriter()      
mdp <-loadMDP("traffic_light_")
mdp
# Diverse test af MDP
info <- infoMDP(mdp)
info$actionDF
info$stateDF
## seems to work, i.e error must be in pr and id


# Test: check pr and id (since 81 states id must be in range 0-80)
w<-binaryMDPWriter("traffic_light_")
w$setWeights(c("Duration", "Cost"))
w$process()
w$stage()
for (ii in 1:nrow(tot_states)) {
   w$state(label = states_label$label[ii])
   ss <- unlist(tot_states[ii,], use.names = FALSE)  # Find relevant queue according to state number in tot_states
   dat <- transPr(l_13,ss)
   print(sum(dat$pr)); print(dat$id)
   # w$action(label = "G_1,R_2", weights=c(1,-cost(ss)), pr=dat$pr, id=dat$id, end=TRUE)
   
   dat <- transPr(l_24,ss)
   print(sum(dat$pr)); print(dat$id)
   # w$action(label = "R_1,G_2", weights=c(1,-cost(ss)), pr=dat$pr, id=dat$id, end=TRUE)
   w$endState() # should be inside the loop
}
w$endStage()
w$endProcess()
w$closeWriter() 
# that id goes to 81 and not 80. Remember ids start from 0!
# Happy coding



mdp <-loadMDP("traffic_light_")
mdp
# Diverse test af MDP
info <- infoMDP(mdp)
info$actionDF
info$stateDF

policyIteAve(mdp, "Duration","Cost")

getPolicy(mdp)