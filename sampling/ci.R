ci.rata = function(n, rata, std, ci, type="t", N=NA){
  ci = ci + (1-ci)/2
  if(!is.na(N)) fpc = sqrt((N-n)/(N-1))
  else fpc = 1
  
  if(type=="z"){
    z = round(qnorm(ci), 4)
    mof = z*std/sqrt(n)*fpc 
    print(paste("z    :",z))
  }else{
    t = round(qt(ci, n-1), 4)
    mof = t*std/sqrt(n)*fpc 
    print(paste("t    :",t))
  }
  print(rata - mof)
  print(rata + mof)
}
#---------------------------------------------------------
ci.var = function(n, var, ci, N=NA){
  ci = ci + (1-ci)/2
  if(!is.na(N)) fpc = (N-n)/(N-1)
  else fpc = 1
  
  xup = round(qchisq(1-ci, n-1), 4)
  up = (n-1)*var/(xup*fpc)
  xdw = round(qchisq(ci, n-1), 4)
  dw = (n-1)*var/(xdw*fpc)
  
  print(paste("x bawah  :",xdw))
  print(paste("x atas   :",xup))
  print("interval varians")
  print(dw)
  print(up)
  print("interval simpangan baku")
  print(sqrt(dw))
  print(sqrt(up))
}
#---------------------------------------------------------
ci.rata2 <- function(n1,n2,x1,x2,s1,s2,ci, type){
  ci = ci + (1-ci)/2
  if(type==1){
    z = round(qnorm(ci), 4)
    e = z*sqrt((s1^2/n1)+(s2^2/n2)) 
    print(paste("z    :",z))
    
  }else if(type==2){
    v = n1+n2-2
    sp2 = ((n1-1)*s1^2 + (n2-1)*s2^2)/v
    t = round(qt(ci, v), 3)
    e = t*sqrt(sp2)*sqrt((1/n1)+(1/n2))
    print(paste("sp^2 :",sp2))
    print(paste("v    :",v))
    print(paste("t    :",t))
    
  }else if(type==3){
    v = (s1^2/n1 + s2^2/n2)^2 / ( ((s1^2/n1)^2/(n1-1)) + ((s2^2/n2)^2/(n2-1)) ) 
    v = round(v, 0)
    t = round(qt(ci, v), 3)
    e = t*sqrt((s1^2/n1)+(s2^2/n2))
    print(paste("v    :",v))
    print(paste("t    :",t))
    
  }else if(type==4){
    v = n1-1
    t = round(qt(ci, v), 3)
    e = t*(s1/sqrt(n1))
    print(paste("v    :",v))
    print(paste("t    :",t))
    
  }
  print((x1-x2) - round(e, 3))
  print((x1-x2) + round(e, 3))
}
#___________________________________________________________________________
ci.rasiovar <- function(n1,s1_2, n2,s2_2, ci){
  ci = ci + (1-ci)/2
  
  f1 = qf(1-ci, n1-1, n2-1, lower.tail = F)
  f2 = qf(1-ci, n2-1, n1-1, lower.tail = F)
  dw = round(s1_2/f1/s2_2, 3)
  up = round((s1_2/s2_2)*f2, 3)
  print(paste("f bawah  :",f1))
  print(paste("f atas   :",f2))
  
  print(dw)
  print(up)
  print("interval simpangan baku")
  print(sqrt(dw))
  print(sqrt(up))
}

#___________________________________________________________________________
ci.proporsi <- function(x1,n1 ,x2=NA,n2=NA, ci){
  ci = ci + (1-ci)/2
  if(is.na(x2)){
    z = round(qnorm(ci), 4)
    pcap = x1/n1
    sd = sqrt(pcap*(1-pcap)/n1)
    e = z*sd
    print(paste("z    :",z))
    print(pcap-e)
    print(pcap+e)
  }else{
    z = round(qnorm(ci), 4)
    pcap1 = x1/n1
    pcap2 = x2/n2
    sd = sqrt((pcap1*(1-pcap1)/n1) + (pcap2*(1-pcap2)/n2))
    e = z*sd
    print(paste("z    :",z))
    print((pcap1-pcap2)-e)
    print((pcap1-pcap2)+e)
  }
}
#----------------------------------------------------------------


c = c(1.46, 0.66, 0.67, 0.94, 1.45, 1.15, 1.39, 1.76)
ci.var(8, var(c), ci=0.95, 38)

ci.rata(15, mean(d), sd(d), 0.9, "t", 80)
ci.rata(30, 780, 40, 0.96)

interval_proporsi(28, 460, 56, 2248, 0.96)
a = c(.430, .266, .567, .531, .707, .716, 0.651, 0.589, 0.469, 0.723)
b = c(.415 ,.238 ,.390 ,.410 ,.605 ,.609 ,.632 ,.523 ,.411 ,.612)
b

ci.var(13, 3.271, 0.95)
ci.rata(13, 18.97, 3.271, 0.95, "t")

c = c(0.51,1.10,0.88,0.75,0.92,0.65,1.00,0.12,0.31,0.18,0.63,0.57,0.15)
mean(c)
sd(c)
var(c)*13

ci.var(21, 16, 0.99)
ci.rata2()