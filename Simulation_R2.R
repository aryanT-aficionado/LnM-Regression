library(mfp)
data("bodyfat")

n = nrow(bodyfat)
Y = bodyfat$brozek
X = cbind(rep(1,n), bodyfat$weight)

betas = solve(t(X)%*%X)%*%t(X)%*%Y

fitted_values = X%*%betas

sse = sum((Y - fitted_values)^2)
ss_t = sum((Y - mean(Y))^2)

1 - sse/ss_t

add = 30
r2 = numeric(30)
r2_adj = numeric(30)
for(j in 1:add){
  
  
  x = rnorm(n, 0, 3)
  X = cbind(X, x)
  betas = solve(t(X)%*%X)%*%t(X)%*%Y
  p = ncol(X)-1
  
  fitted_values = X%*%betas
  
  sse = sum((Y - fitted_values)^2)
  ss_t = sum((Y - mean(Y))^2)
  
  r2[j] = 1 - sse/ss_t
  r2_adj[j] = 1 - (sse/(n-p-1))/(ss_t/(n-1))
  
}


plot(r2, type = "l")
plot(r2_adj, type = "l")
