library(ggplot2)
library(dplyr)
library(dgof)
rm(list=ls())

#### working on our datasets####
setwd("C:\Users\hp\OneDrive\Documents\Projects\Bayesian Statistics Project 2")
dataset = read.csv("curry21_22.csv")
View(dataset)
sized_data = dataset$X3P
sized_data = na.omit(sized_data)
sized_data = data.frame(sized_data)
# lambda_prior = mean(sized_data$sized_data)
# lambda_prior
dataset2 = read.csv("curry22_23.csv")
req_data = dataset2$X3P
req_data = na.omit(req_data)
req_data = data.frame(req_data)
sum(req_data$req_data)
mean(req_data$req_data)
var(req_data$req_data)
n = nrow(req_data)
#####prior ######
mat = matrix(ncol = 1, nrow = 22)
prior = data.frame(mat)

i=1
for(x in seq(1, nrow(sized_data), by=3)) {
  tmp = mean(sized_data[c(x,x+1,x+2),])
  prior[i,] = mean(tmp)
  i= i+1
}
prior = na.omit(prior)
hist(prior$mat)
mean_prior = mean(prior$mat)
var_prior = var(prior$mat)
table(prior$mat)
beta = mean(prior$mat)/var(prior$mat)
alpha = beta*mean(prior$mat)
mean_prior = alpha/beta
mean_prior/beta
# to compare our prior to gamma(a,b) distribution 
x_dgamma <- seq(0, 10, by=0.1)   
y_dgamma <- dgamma(x_dgamma, shape = alpha,rate = beta) 

plot(x_dgamma,y_dgamma,col="green")
plot(density(prior$mat)$x, density(prior$mat)$y,col="yellow")
polygon(x = c(min(density(prior$mat)$x), density(prior$mat)$x, max(density(prior$mat)$x)),
        y = c(0, density(prior$mat)$y, 0),
        col = rgb(1,1,0, alpha = 0.3))
legend(8,0.15,legend=c( "prior"), fill=c("yellow"))

#### likelihood ####

# max(req_data$req_data)
# mat = matrix(ncol = 1, nrow = 10)
# fact = data.frame(mat)
# fact[1,1] = 1
# for(x in 2:10){
#   fact[x,1] = fact[x-1,1]*x
# }
# tmp = 1
# for(x in 1:56){
#   if(req_data$req_data[x]==0) tmp = tmp + 1
#   else tmp = tmp+fact[req_data$req_data[x],1]
# }
# likelihood <- function(lam, tmp) {
#   n = 56
#   sum = 273
#   return (exp(-n*lam)*((lam)^sum)/tmp)
# }
# mat = matrix(ncol = 1, nrow = 60)
# likelihood_v = data.frame(mat)
# for(lam in 1:60){
#   likelihood_v[lam,1] = likelihood(lam,tmp)
# }
# likelihood_v[,1] = scale(likelihood_v[,1])
# plot(1:60, likelihood_v$mat)
likelihood_v = dpois(n*mean(req_data$req_data), n*seq(0,10,by=0.1)) #N LAMBDA 
##### posterior ####
new_alpha = alpha+sum(req_data$req_data)
new_beta = beta + nrow(req_data)
y_dgamma2 = dgamma(x_dgamma, shape = new_alpha, rate = new_beta)
plot(x_dgamma,y_dgamma2, type="b")

mean_post = new_alpha/new_beta
var_post = mean_post/new_beta
###combined plots#####
plot(seq(0,10,by=0.1), n/5*likelihood_v/sum(likelihood_v), type = "b", col="light green")
polygon(x = c(min(seq(0,10,by=0.1)), seq(0,10,by=0.1), max(seq(0,10,by=0.1))),
        y = c(0, n/5*likelihood_v/sum(likelihood_v), 0),
        col = rgb(0, 1, 0, alpha = 0.3))


lines(density(prior$mat)$x, density(prior$mat)$y, col = "light yellow")
polygon(x = c(min(density(prior$mat)$x), density(prior$mat)$x, max(density(prior$mat)$x)),
        y = c(0, density(prior$mat)$y, 0),
        col = rgb(1,1,0, alpha = 0.5))
# legend(8,1.2,legend=c( "prior","likelihood"), fill=c( "yellow","light green"))
lines(x_dgamma,y_dgamma2, type="b",col="blue")
polygon(x = c(min(x_dgamma), x_dgamma, max(x_dgamma)),
        y = c(0, y_dgamma2, 0),
        col = rgb(0,0,128/255,alpha=0.8))
#legend(8,1.2,legend=c("posterior"), fill=c("blue"))

legend(6.5,1.2,legend=c("posterior", "prior","likelihood"), fill=c("blue", "yellow","light green"))



# plot(seq(0,10,by=0.1), n/5*likelihood_v/sum(likelihood_v), type = "b", col="light green")
# lines(x_dgamma,y_dgamma2, type="b",col="Light BLUE")
