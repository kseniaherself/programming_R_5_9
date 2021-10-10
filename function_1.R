v <- f() 
  for (i in 700:1000) { 
    if (i %% 43){ 
      next 
    } 
    v <- f(v,i)
  }
print(f) 
