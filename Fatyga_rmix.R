rmix <- function(n, family_1, par_1, family_2, par_2, p){
  eval(parse(text = paste0("rozkladzik1 <- r", family_1)))
  eval(parse(text = paste0("rozkladzik2 <- r", family_2)))
  
  index <- sample(c(1,2), n, prob = c(p, 1-p),replace = T)
  
  sample_mix <- c()
  for(i in c(1:n)){
    if(index[i] == 1){
      if(length(par_1) == 2){
      sample_mix[i] <- rozkladzik1(1, par_1[1], par_1[2])
      }
      else if(length(par_1) == 1){
        sample_mix[i] <- rozkladzik1(1, par_1)
      }
    }
    else if(index[i] == 2){
      if(length(par_2) == 2){
        sample_mix[i] <- rozkladzik2(1, par_2[1], par_2[2])
      }
      else if(length(par_2) == 1){
        sample_mix[i] <- rozkladzik2(1, par_2)
      }
    }
  }
  return(sample_mix)
}
