test <- function(n1,n2,x1,x2,s1,s2,ci, type){
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

interval_varians <- function(n1,s1_2, n2=NA,s2_2=NA, ci){
  ci = ci + (1-ci)/2
  if(is.na(n2)){
    xup = qchisq(1-ci, n1-1)
    up = round((n1-1)*s1_2/xup ,3)
    xdw = qchisq(ci, n1-1)
    dw = round((n1-1)*s1_2/xdw, 3)
    print(paste("x bawah  :",xdw))
    print(paste("x atas   :",xup))
  }else{
    f1 = qf(1-ci, n1-1, n2-1, lower.tail = F)
    f2 = qf(1-ci, n2-1, n1-1, lower.tail = F)
    dw = round(s1_2/f1/s2_2, 3)
    up = round((s1_2/s2_2)*f2, 3)
    print(paste("f bawah  :",f1))
    print(paste("f atas   :",f2))
  }
  print(dw)
  print(up)
  print("interval simpangan baku")
  print(sqrt(dw))
  print(sqrt(up))
}

#___________________________________________________________________________
interval_proporsi <- function(x1,n1 ,x2=NA,n2=NA, ci){
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
#___________________________________________________________________________
test(12, 10,85, 81 , 4, 5, 0.9, 2)
test(100, 200, 12.2, 9.1, 1.1, 0.9, 0.98,1)
test(12, 18 , 84, 77, 4, 6, 0.99, 2)

Tn = c(0.12, 0.53, 0.28, 0.37, 0.47, 0.43, 0.46, 0.42, 0.38, 0.43)
n = c(0.26, 0.43, 0.47, 0.49, 0.52, 0.75, 0.79, 0.86, 0.62, 0.46)
test(10,10, mean(n), mean(Tn), 0.187, 0.116, 0.95, 2)

test(16,14, 19, 17, 1.34, 1.225, 0.99, 2)
test(12,10, 16,11, 1,0.8, 0.9, 2)
test(12,12, 36300,38100, 5000,6100, 0.95, 2)

a = c(103, 94, 110, 87, 98)
b = c(97, 82, 123, 92, 175, 88, 118)
ci.rata2(length(b),length(a), mean(b), mean(a), sd(b), sd(a), 0.90, 3)

a = c(38, 23, 35, 41, 44, 29, 37, 31, 38)
b = c(45, 25, 31, 38, 50, 33, 36, 40, 43)
ci.rata2(9,9, mean(b-a),0, sd(b-a),0, 0.95, 4)

a = c(34400, 45500, 36700, 32000, 48400, 32800, 38100, 30100)
b = c(36700, 46800, 37700, 31100, 47800, 36400, 38900, 31500)
ci.rata2(8,0, mean(a-b),0, sd(a-b),0, 0.99, 4)


interval_varians(10, 0.286, ci=0.95)
interval_varians(15, 3.07^2, 12, 0.8^2, 0.98, 2)
interval_varians(20,16,ci=0.98)
interval_varians(12, 1.5^2, ci=0.9)
interval_varians(8, 0.9^2,ci=0.99)
interval_varians(20, 2.45^2, ci=0.95)
interval_varians(12, 5000^2, 12, 6100^2, 0.95)
interval_varians(length(b),var(b), length(a), var(a), 0.9)
interval_varians(12,1,10,0.8^2,0.98)

interval_proporsi(340,500,ci=0.95)
interval_proporsi(75,1500,80,2000,0.9)
interval_proporsi(114,200,ci=0.96)
interval_proporsi(86,500,ci=0.9)
interval_proporsi(34,40,ci=0.95)
interval_proporsi(120,500,98,500,ci=0.9)
interval_proporsi(250,1000,275,1000,0.95)

test(25,36,80,75,5,3,0.94,3)

n = c(10.2, 9.7, 10.1, 10.3, 10.1, 9.8, 9.9, 10.4, 10.3, 9.8)
length(n)
mean(n)
var(n)
sd(n)

interval_varians(40, 1.6^2, 36, 2.1^2, 0.95)


interval_varians(12, 5000^2, 12, 6100^2, 0.90)
