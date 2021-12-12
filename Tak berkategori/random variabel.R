library(Ryacas)

x = ysym("x^2")

?yac_str()

yac_str("Solve(x^2 == 4,x)") %>% y_rmvars() %>% yac_str()

x = ysym("x")
y = ysym("y")

Solve <- function(str_expr, thd){
  a = yac_expr(paste("Solve(",str_expr,",",thd,")")) %>% 
    y_rmvars() %>% yac_str() %>% as_r()
  return(a)
}
#-----------------------------------------------------------------------
library(rSymPy)
library(MASS)

sympyStart()
x = Var("x")
y = Var("y")

ubah = function(str){
  return(eval(parse(text=str)))
}

randvar = function(expr, xmin, xmax, ymin, ymax, new_exp="1",thd="x", ds="kontinu"){
  new = paste("(",new_exp,")")
  hasil = c() ; p = c("x", "y", "x**2", "y**2", "x*y", new)

  if(ds == 'kontinu') op = 'integrate'
  else op = "sum"
  
  for(i in 1:length(p)){
      if(tolower(thd) == "x")
        hasil[i] = sympy(paste(op,"((",p[i],")*(",expr,"),(x,",xmin,",",xmax,"),(y,",ymin,",",ymax,"))"))
      else if(tolower(thd) == "y")
        hasil[i] = sympy(paste(op,"((",p[i],")*(",expr,"),(y,",ymin,",",ymax,"),(x,",xmin,",",xmax,"))"))
  }
  varx = ubah(hasil[3])-ubah(hasil[1])^2
  vary = ubah(hasil[4])-ubah(hasil[2])^2
  covxy = ubah(hasil[5])-ubah(hasil[1])*ubah(hasil[2])
  p = covxy/(sqrt(varx)*sqrt(vary))
  
  fx = sympy(paste(op,"((",expr,"),(y,",ymin,",",ymax,"))"))
  fy = sympy(paste(op,"((",expr,"),(x,",xmin,",",xmax,"))"))
  
  fx1y = sympy(paste0("simplify(",expr,"/(",op,"((",expr,"),(x,",xmin,",",xmax,"))))"))
  fy1x = sympy(paste0("simplify(",expr,"/(",op,"((",expr,"),(y,",ymin,",",ymax,"))))"))
  
  Ex1y = sympy(op,"(x*(",fx1y,"), (x, ",xmin,",",xmax,"))")
  Ex21y = sympy(op,"((x**2)*(",fx1y,"), (x, ",xmin,",",xmax,"))")
  Ey1x = sympy(op,"(y*(",fy1x,"), (y, ",ymin,",",ymax,"))")
  Ey21x = sympy(op,"((y**2)*(",fy1x,"), (y, ",ymin,",",ymax,"))")
  
  n = gsub(" ", "", paste("E(",toupper(new_exp),")"))
  name = c("E(X)","E(Y)","E(X^2)","E(Y^2)", "E(XY)", n, "Var(X)", "Var(Y)", "Cov(X,Y)",
            "Pxy","f(x)", "f(y)", "f(x|y)", "f(y|x)", "E(X|y)", "E(X^2|y)","E(Y|x)", "E(Y^2|x)")
  hasil = c(hasil, c(varx, vary, covxy, p,  fx, fy, fx1y, fy1x, Ex1y, Ex21y, Ey1x, Ey21x))
  
  output = data.frame(name, Nilai = hasil)
  
  return(output)
}


expr = "x*(1 + 3*y**2)/4"
randvar(expr,0,2,0,1, "x-y","x") 

s = sympy("diff(2*y**2, y)")
s
fractions(0.0760416666666667)

u = Var("u")
sympy("solve(Eq( (x - 0.5)**2 ,y), x)")
sympy("simplify( (-sqrt(u)+1/2)**3 )")


sympy("simplify( (-(u**(1/2)) + (1/2))**3 )")
