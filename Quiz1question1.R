# Fixed interest rate
bond.price= function(coupon,NumofPayments,interest,face){
  periods= seq(0.5,NumofPayments/2,by= 0.5)
  price= cumsum(coupon*exp((-interest)*periods))
  price= price[NumofPayments]+ face*exp((-interest)*(NumofPayments/2))
  
}

output= bond.price(100,10,0.1,1000)
output

# Changeable interest rate
coupon= 100
NumofPayments= 10
face= 1000
t<- c(seq(0.5,NumofPayments/2,by= 0.5))
y<- c(0.1,0.05,0.1,0.2,0.05,0.1,0.05,0.1,0.1,0.1)
bond.price= sum(coupon *exp(-y*t))+ face*exp(-(NumofPayments/2)*0.1)
bond.price


