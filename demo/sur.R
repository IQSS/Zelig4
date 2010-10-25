data(grunfeld)

formula<-list(mu1=Ige~Fge+Cge, mu2=Iw~Fw+Cw)
user.prompt() 
z.out<-zelig(formula=formula,model="sur",data=grunfeld)
user.prompt()
summary(z.out)
user.prompt() 
 
 x.out <- setx(z.out)
user.prompt() 
 
 s.out <- sim(z.out,x=x.out)
user.prompt() 
  
summary(s.out)
user.prompt()

plot(s.out)
