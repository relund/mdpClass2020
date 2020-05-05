library(MDP2)

h= matrix(c( 0.000, 0.000, 0.000, 0.000, 0.000, 1.000, 0.000, 0.770, 0.176, 0.000, 0.020,
             0.034, 0.000, 0.117, 0.677, 0.058, 0.075, 0.073, 0.000, 0.067, 0.200, 0.279, 0.233, 0.221,
             0.000, 0.000, 0.167, 0.167, 0.240, 0.426, 0.000, 0.000, 0.000, 0.000, 0.000, 1.000), # the data elements 
          nrow=6,              # number of rows 
          ncol=6,              # number of columns 
          byrow = TRUE) 

l=matrix(c(0,0,0,1,0,0,0,1,0.0021,0.0025,0.0041,0.9913,0.1005,0.0913,0.1170,0.6912,0.3508,0.2786,0.2554,0.1152,0,0,0,1), nrow=6,              # number of rows 
         ncol=4,              # number of columns 
         byrow = TRUE)


trans_W<-matrix(rep(0,len=144),nrow=6)

trans_W[1 ,]<-c(h[1,1]*l[1,1],h[1,1]*l[1,2],h[1,1]*l[1,3],h[1,1]*l[1,4],h[1,2]*l[2,1],h[1,2]*l[2,2],h[1,2]*l[2,3],h[1,2]*l[2,4],h[1,3]*l[3,1],h[1,3]*l[3,2],h[1,3]*l[3,3],h[1,3]*l[3,4],h[1,4]*l[4,1],h[1,4]*l[4,2],h[1,4]*l[4,3],h[1,4]*l[4,4],h[1,5]*l[5,1],h[1,5]*l[5,2],h[1,5]*l[5,3],h[1,5]*l[5,4],h[1,6]*l[6,1],h[1,6]*l[6,2],h[1,6]*l[6,3],h[1,6]*l[6,4])
trans_W[2, ]<-c(h[2,1]*l[1,1],h[2,1]*l[1,2],h[2,1]*l[1,3],h[2,1]*l[1,4],h[2,2]*l[2,1],h[2,2]*l[2,2],h[2,2]*l[2,3],h[2,2]*l[2,4],h[2,3]*l[3,1],h[2,3]*l[3,2],h[2,3]*l[3,3],h[2,3]*l[3,4],h[2,4]*l[4,1],h[2,4]*l[4,2],h[2,4]*l[4,3],h[2,4]*l[4,4],h[2,5]*l[5,1],h[2,5]*l[5,2],h[2,5]*l[5,3],h[2,5]*l[5,4],h[2,6]*l[6,1],h[2,6]*l[6,2],h[2,6]*l[6,3],h[2,6]*l[6,4])
trans_W[3, ]<-c(h[3,1]*l[1,1],h[3,1]*l[1,2],h[3,1]*l[1,3],h[3,1]*l[1,4],h[3,2]*l[2,1],h[3,2]*l[2,2],h[3,2]*l[2,3],h[3,2]*l[2,4],h[3,3]*l[3,1],h[3,3]*l[3,2],h[3,3]*l[3,3],h[3,3]*l[3,4],h[3,4]*l[4,1],h[3,4]*l[4,2],h[3,4]*l[4,3],h[3,4]*l[4,4],h[3,5]*l[5,1],h[3,5]*l[5,2],h[3,5]*l[5,3],h[3,5]*l[5,4],h[3,6]*l[6,1],h[3,6]*l[6,2],h[3,6]*l[6,3],h[3,6]*l[6,4])
trans_W[4, ]<-c(h[4,1]*l[1,1],h[4,1]*l[1,2],h[4,1]*l[1,3],h[4,1]*l[1,4],h[4,2]*l[2,1],h[4,2]*l[2,2],h[4,2]*l[2,3],h[4,2]*l[2,4],h[4,3]*l[3,1],h[4,3]*l[3,2],h[4,3]*l[3,3],h[4,3]*l[3,4],h[4,4]*l[4,1],h[4,4]*l[4,2],h[4,4]*l[4,3],h[4,4]*l[4,4],h[4,5]*l[5,1],h[4,5]*l[5,2],h[4,5]*l[5,3],h[4,5]*l[5,4],h[4,6]*l[6,1],h[4,6]*l[6,2],h[4,6]*l[6,3],h[4,6]*l[6,4])
trans_W[5, ]<-c(h[5,1]*l[1,1],h[5,1]*l[1,2],h[5,1]*l[1,3],h[5,1]*l[1,4],h[5,2]*l[2,1],h[5,2]*l[2,2],h[5,2]*l[2,3],h[5,2]*l[2,4],h[5,3]*l[3,1],h[5,3]*l[3,2],h[5,3]*l[3,3],h[5,3]*l[3,4],h[5,4]*l[4,1],h[5,4]*l[4,2],h[5,4]*l[4,3],h[5,4]*l[4,4],h[5,5]*l[5,1],h[5,5]*l[5,2],h[5,5]*l[5,3],h[5,5]*l[5,4],h[5,6]*l[6,1],h[5,6]*l[6,2],h[5,6]*l[6,3],h[5,6]*l[6,4])
trans_W[6, ]<-c(h[6,1]*l[1,1],h[6,1]*l[1,2],h[6,1]*l[1,3],h[6,1]*l[1,4],h[6,2]*l[2,1],h[6,2]*l[2,2],h[6,2]*l[2,3],h[6,2]*l[2,4],h[6,3]*l[3,1],h[6,3]*l[3,2],h[6,3]*l[3,3],h[6,3]*l[3,4],h[6,4]*l[4,1],h[6,4]*l[4,2],h[6,4]*l[4,3],h[6,4]*l[4,4],h[6,5]*l[5,1],h[6,5]*l[5,2],h[6,5]*l[5,3],h[6,5]*l[5,4],h[6,6]*l[6,1],h[6,6]*l[6,2],h[6,6]*l[6,3],h[6,6]*l[6,4])

trans_W

trans_T<-matrix(rep(0,len=144),nrow=6)
trans_T[, 2]<-c(1,1,1,1,1,0)
trans_T[6,24]<-1
trans_T

r_w=c(0,1,1,1,1,0)
r_t=c(0,20,15,10,5,0)

R=cbind(r_w,r_t)

states<-c("transplant",1,2,3,4,"dead")

PList <- list(trans_W,trans_T)
# mdp<-list(PList = list(trans_W,trans_T, CMat = R))
# mdp
# build model
# D <- mdp$CMat
# D[, ] = 1 # set length of each stage to 1 (since not a semi-MDP) 

## Try to build the MDP in steps

# Step 1: dummy actions (seems to work)
w<-binaryMDPWriter()
w$setWeights(c("Duration", "Net reward"))
w$process()
   w$stage()
      for (i in 1:length(R[,1])) {
         w$state(label=states[i])
            w$action(label="W", id=0, pr = 1, weights = c(1,1), end=T)
            w$action(label="T", id=0, pr = 1, weights = c(1,1), end=T)
         w$endState()
      }
   w$endStage()
w$endProcess() # Missing parenthesis here!
w$closeWriter()
mdp<-loadMDP()

# Step 2: model with output instead of actions (since 6 states the id of states must be 0, 1, ..., 5)
w<-binaryMDPWriter()
w$setWeights(c("Duration", "Net reward"))
w$process()
w$stage()
for (i in 1:length(R[,1])) {
   w$state(label=states[i])
   id=which(PList[[1]][i ,]>0)
   cat("idW:", id, "  "); cat("pr sum:", sum(PList[[1]][i,id]), "\n")
   # w$action(label="W", id=id-1,pr = PList[[1]][i,id], weights = c(1,R[i,1]), end=T)
   id=which(PList[[2]][i,]>0)
   cat("idT:", id, "  "); cat("pr sum:", sum(PList[[1]][i,id]), "\n")
   # w$action(label="T", id = id-1, pr = PList[[2]][i,id], weights = c(1,R[i,2]), end=T)
   w$endState()
}
w$endStage()
w$endProcess()
w$closeWriter()
# mdp<-loadMDP()

# Your ids must be in the range 0:5!







mdp<-loadMDP()
infoMDP(mdp, withDF = T)$actionDF

policyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.99)
getPolicy(mdp)
