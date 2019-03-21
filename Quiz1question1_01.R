# Fixed interest rate
bond.price= function(coupon,NumofPayments,interest,face){
  periods= seq(0.5,NumofPayments/2,by= 0.5)
  price= cumsum(coupon*exp((-interest)*periods))
  price= price[NumofPayments]+ face*exp((-interest)*(NumofPayments/2))
  
}

output= bond.price(100,10,0.1,1000)
output

# Changeable interest rate
bond.price= function(coupon,NumofPayments,interest,face){
  
t<- c(seq(0.5,NumofPayments/2,by= 0.5))
bond.price= sum(coupon *exp(-y*t))+ face*exp(-(NumofPayments/2)*y[length(y)])

}
output=bond.price(6,4,c(0.03,0.04,0.05,0.055),100)
output

