#Fitness function for Ackley function
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
  return(-y)
}



#Fitness function for Rastrigin function
rastrigin <- function(ch)
{
  d <- length(ch)
  
  sum <- sum(ch^2 - 10*cos(2*pi*ch))
  
  y <- 10*d + sum
  return(-y)
}




#Algorytm genetyczny dla funkcji Ackley'a i dwóch wymiarów
gaMinAckley2Dim<-ga(type="real-valued",
                    fitness=ackley,
                    lower=rep(-32.768,2),
                    upper=rep(32.768,2),
                    maxiter = 1000,
                    maxFitness = 0)

summary(gaMinAckley2Dim)
plot(gaMinAckley2Dim)
bestch<-gaMinAckley2Dim@fitnessValue
print(bestch*(-1))


#Algorytm genetyczny dla funkcji Ackley'a i dziesięciu wymiarów
gaMinAckley10Dim<-ga(type="real-valued",
                     fitness=ackley,
                     lower=rep(-32.768,10),
                     upper=rep(32.768,10),
                     maxiter = 1000,
                     maxFitness = 0)

#Algorytm genetyczny dla funkcji Ackley'a i dwudziestu wymiarów
gaMinAckley20Dim<-ga(type="real-valued",
                     fitness=ackley,
                     lower=rep(-32.768,20),
                     upper=rep(32.768,20),
                     maxiter = 1000,
                     maxFitness = 0)

#Algorytm genetyczny dla funkcji Rastrigina i dwóch wymiarów
gaMinRastrigin2Dim<-ga(type="real-valued",
                       fitness=rastrigin,
                       lower=rep(-5.12,2),
                       upper=rep(5.12,2),
                       maxiter = 1000,
                       maxFitness = 0)

summary(gaMinRastrigin2Dim)
plot(gaMinRastrigin2Dim)
bestch<-gaMinRastrigin2Dim@fitnessValue
print(bestch*(-1))

#Algorytm genetyczny dla funkcji Rastrigina i dziesięciu wymiarów
gaMinRastrigin10Dim<-ga(type="real-valued",
                        fitness=rastrigin,
                        lower=rep(-5.12,10),
                        upper=rep(5.12,10),
                        maxiter = 1000,
                        maxFitness = 0)

#Algorytm genetyczny dla funkcji Rastrigina i dwudziestu wymiarów
gaMinRastrigin20Dim<-ga(type="real-valued",
                        fitness=rastrigin,
                        lower=rep(-5.12,20),
                        upper=rep(5.12,20),
                        maxiter = 1000,
                        maxFitness = 0)



###wektory z wynikami oraz algorytmami genetycznymi
###Funkcja Ackleya w dwóch wymiarach
valuesAckley2dim<-rep(0,50)
for(i in 1:50){
  gaMinAckley2Dim<-ga(type="real-valued",
                      fitness=ackley,
                      lower=rep(-32.768,2),
                      upper=rep(32.768,2),
                      maxiter = 1000,
                      maxFitness = 0)
  bestch<-gaMinAckley2Dim@fitnessValue
  valuesAckley2dim[i]<-bestch*(-1)
}
valuesAckley2dim
hist(valuesAckley2dim)
###Funkcja Rastrigina w dwóch wymiarach
valuesRastrigin2dim<-rep(0,50)
for(i in 1:50){
  gaMinRastrigin2Dim<-ga(type="real-valued",
                         fitness=rastrigin,
                         lower=rep(-5.12,2),
                         upper=rep(5.12,2),
                         maxiter = 1000,
                         maxFitness = 0)
  bestch<-gaMinRastrigin2Dim@fitnessValue
  valuesRastrigin2dim[i]<-bestch*(-1)
}
valuesRastrigin2dim
boxplot(valuesRastrigin2dim)

###Funkcja Ackleya w dziesięciu wymiarach
valuesAckley10dim<-rep(0,50)
for(i in 1:50){
  gaMinAckley10Dim<-ga(type="real-valued",
                       fitness=ackley,
                       lower=rep(-32.768,10),
                       upper=rep(32.768,10),
                       maxiter = 1000,
                       maxFitness = 0)
  bestch<-gaMinAckley10Dim@fitnessValue
  valuesAckley10dim[i]<-bestch*(-1)
}
valuesAckley10dim
boxplot(valuesAckley10dim)
###Funkcja Rastrigina w dziesięciu wymiarach
valuesRastrigin10dim<-rep(0,50)
for(i in 1:50){
  gaMinRastrigin10Dim<-ga(type="real-valued",
                          fitness=rastrigin,
                          lower=rep(-5.12,10),
                          upper=rep(5.12,10),
                          maxiter = 1000,
                          maxFitness = 0)
  bestch<-gaMinRastrigin10Dim@fitnessValue
  valuesRastrigin10dim[i]<-bestch*(-1)
}
valuesRastrigin10dim
boxplot(valuesRastrigin10dim)
###Funkcja Ackleya w dwudziestu wymiarach
valuesAckley20dim<-rep(0,50)
for(i in 1:50){
  gaMinAckley20Dim<-ga(type="real-valued",
                       fitness=ackley,
                       lower=rep(-32.768,20),
                       upper=rep(32.768,20),
                       maxiter = 1000,
                       maxFitness = 0)
  bestch<-gaMinAckley20Dim@fitnessValue
  valuesAckley20dim[i]<-bestch*(-1)
}
valuesAckley20dim
boxplot(valuesAckley20dim)
###Funkcja Rastrigina w dwudziestu wymiarach
valuesRastrigin20dim<-rep(0,50)
for(i in 1:50){
  gaMinRastrigin20Dim<-ga(type="real-valued",
                          fitness=rastrigin,
                          lower=rep(-5.12,20),
                          upper=rep(5.12,20),
                          maxiter = 1000,
                          maxFitness = 0)
  bestch<-gaMinRastrigin20Dim@fitnessValue
  valuesRastrigin20dim[i]<-bestch*(-1)
}
valuesRastrigin20dim
boxplot(valuesRastrigin20dim)
###średnie

meanAckley2dim<-mean(valuesAckley2dim)
meanAckley2dim
sigmaAckley2dim<-sd(valuesAckley2dim)
sigmaAckley2dim

meanRastrigin2dim<-mean(valuesRastrigin2dim)
meanRastrigin2dim
sigmaRastrigin2dim<-sd(valuesRastrigin2dim)
sigmaRastrigin2dim


meanAckley10dim<-mean(valuesAckley10dim)
meanAckley10dim
sigmaAckley10dim<-sd(valuesAckley10dim)
sigmaAckley10dim

meanRastrigin10dim<-mean(valuesRastrigin10dim)
meanRastrigin10dim
sigmaRastrigin10dim<-sd(valuesRastrigin10dim)
sigmaRastrigin10dim

meanAckley20dim<-mean(valuesAckley20dim)
meanAckley20dim
sigmaAckley20dim<-sd(valuesAckley20dim)
sigmaAckley20dim

meanRastrigin20dim<-mean(valuesRastrigin20dim)
meanRastrigin20dim
sigmaRastrigin20dim<-sd(valuesRastrigin20dim)
sigmaRastrigin20dim


Ack2D95<-round(meanAckley2dim+c(-1,1)*sigmaAckley2dim/sqrt(32)*qnorm(.975),5)
Ack2D95

Ack10D95<-round(meanAckley10dim+c(-1,1)*sigmaAckley10dim/sqrt(32)*qnorm(.975),5)
Ack10D95

Ack20D95<-round(meanAckley20dim+c(-1,1)*sigmaAckley20dim/sqrt(32)*qnorm(.975),5)
Ack20D95

Rast2D95<-round(meanRastrigin2dim+c(-1,1)*sigmaRastrigin2dim/sqrt(32)*qnorm(.975),7)
Rast2D95

Rast10D95<-round(meanRastrigin10dim+c(-1,1)*sigmaRastrigin10dim/sqrt(32)*qnorm(.975),5)
Rast10D95

Rast20D95<-round(meanRastrigin20dim+c(-1,1)*sigmaRastrigin20dim/sqrt(32)*qnorm(.975),5)
Rast20D95



####Testy

t.test(valuesAckley2dim,mu=0)
t.test(valuesAckley10dim,mu=0)
t.test(valuesAckley20dim, mu=0)
t.test(valuesRastrigin2dim,mu=0)
t.test(valuesRastrigin10dim,mu=0)
t.test(valuesRastrigin20dim,mu=0)
