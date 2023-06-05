#Ackley function
ackley<- function(ch){
  d <- length(ch)
  a=20
  b=0.2
  c=2*pi
  sum1 <- sum(ch^2)
  sum2 <- sum(cos(c*ch))
  
  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)
  
  y <- term1 + term2 + a + exp(1)
  return(y)
}

#Rastrigin function
rastrigin <- function(ch)
{
  d <- length(ch)
  
  sum <- sum(ch^2 - 10*cos(2*pi*ch))
  
  y <- 10*d + sum
  return(y)
}

#Function for smallest value of Ackley function
evalMinAckley<-function(dimensions){
  minimal=1000
  for(i in 1:1000){
    randomized<-runif(dimensions,min=-32.768,max=32.768)
    numb<-ackley(randomized)
    if(numb<minimal){
      minimal=numb
    }
  }
  return(minimal)
}

#Function for smallest value of Rastrigin function
evalMinRastrigin<-function(dimensions){
  minimal=1000
  for(i in 1:1000){
    randomized<-runif(dimensions,min=-5.12,max=5.12)
    numb<-rastrigin(randomized)
    if(numb<minimal){
      minimal=numb
    }
  }
  return(minimal)
}

###wektory z wynikami
outputAckley2dim<-replicate(50,evalMinAckley(2))
outputAckley2dim
boxplot(outputAckley2dim)

outputRastrigin2dim<-replicate(50,evalMinRastrigin(2))
outputRastrigin2dim
boxplot(outputRastrigin2dim)

outputAckley10dim<-replicate(50,evalMinAckley(10))
outputAckley10dim
boxplot(outputAckley10dim)

outputRastrigin10dim<-replicate(50,evalMinRastrigin(10))
outputRastrigin10dim
boxplot(outputRastrigin10dim)

outputAckley20dim<-replicate(50,evalMinAckley(20))
outputAckley20dim
boxplot(outputAckley20dim)

outputRastrigin20dim<-replicate(50,evalMinRastrigin(20))
outputRastrigin20dim
boxplot(outputRastrigin20dim)
###średnie

meanAckley2dim<-mean(outputAckley2dim)
meanAckley2dim
sigmaAckley2dim<-sd(outputAckley2dim)
sigmaAckley2dim

meanRastrigin2dim<-mean(outputRastrigin2dim)
meanRastrigin2dim
sigmaRastrigin2dim<-sd(outputRastrigin2dim)
sigmaRastrigin2dim

meanAckley10dim<-mean(outputAckley10dim)
meanAckley10dim
sigmaAckley10dim<-sd(outputAckley10dim)
sigmaAckley10dim

meanRastrigin10dim<-mean(outputRastrigin10dim)
meanRastrigin10dim
sigmaRastrigin10dim<-sd(outputRastrigin10dim)
sigmaRastrigin10dim

meanAckley20dim<-mean(outputAckley20dim)
meanAckley20dim
sigmaAckley20dim<-sd(outputAckley20dim)
sigmaAckley20dim


meanRastrigin20dim<-mean(outputRastrigin20dim)
meanRastrigin20dim
sigmaRastrigin20dim<-sd(outputRastrigin20dim)
sigmaRastrigin20dim


Ack2D95<-round(meanAckley2dim+c(-1,1)*sigmaAckley2dim/sqrt(32)*qnorm(.975),5)
Ack2D95

Ack10D95<-round(meanAckley10dim+c(-1,1)*sigmaAckley10dim/sqrt(32)*qnorm(.975),5)
Ack10D95

Ack20D95<-round(meanAckley20dim+c(-1,1)*sigmaAckley20dim/sqrt(32)*qnorm(.975),5)
Ack20D95

Rast2D95<-round(meanRastrigin2dim+c(-1,1)*sigmaRastrigin2dim/sqrt(32)*qnorm(.975),5)
Rast2D95

Rast10D95<-round(meanRastrigin10dim+c(-1,1)*sigmaRastrigin10dim/sqrt(32)*qnorm(.975),5)
Rast10D95

Rast20D95<-round(meanRastrigin20dim+c(-1,1)*sigmaRastrigin20dim/sqrt(32)*qnorm(.975),5)
Rast20D95


t.test(outputAckley2dim,mu=0)
t.test(outputAckley10dim,mu=0)
t.test(outputAckley20dim, mu=0)
t.test(outputRastrigin2dim,mu=0)
t.test(outputRastrigin10dim,mu=0)
t.test(outputRastrigin20dim,mu=0)
