#R code for the regression analyses and Figure 3
#must install rethinking package for the analyses from https://github.com/rmcelreath/rethinking
library(rethinking)


dat <- list(
	S=d$S, #outcome variable
	V=standardize(d$svi), #social vunerability index
	u4a=as.factor(d$urban4a), #urbanicity variable
	R=standardize(d$fm_ratio), #sex ratio
	Gln=log(d$crude_g+1), #exposure
	Gsq=sqrt(d$crude_g),#exposure with square root transformation
	state=as.factor(d$state), # state variable
	L=d$lag, #spatial lag of exposure
	O=standardize(d$gun), #gun ownership
	H=as.factor(d$honor), # honor culture dummy
	N=nrow(d) 
	)


# main model (equation from Section 2.2.3) (change Gln to Gsq for the square root transformation results)

m <- ulam(
	alist(
	S~dgampois(lambda,phi),
	log(lambda) <- a[u4a] + s[state]+ bg*Gln +bv*V +br*R +bo*O +bl*L +h[H],
	a[u4a]~dnorm(2.5,0.5),
	h[H]~dnorm(2.5,0.5),
	s[state]~dnorm(mu,sigma),
	c(bv,br,bg,bo,bl,mu)~dnorm(0,0.5),
	phi~dexp(0.5),
	sigma~dhalfnorm(0,0.5)	
),data=dat, chains=4, cores=4, iter=4000, log_lik=T,control=list(adapt_delta=0.95))

#model with random slopes for urbanicity levels (results not shown in publication)
mrs <- ulam(
	alist(
	S~dgampois(lambda,phi),
	log(lambda) <- a[u4a] + s[state]+ bg[u4a]*Gln +bv*V +br*R +bo*O +bl*L + h[H],
	a[u4a]~dnorm(2.5,0.5),
	h[H]~dnorm(2.5,0.5),
	s[state]~dnorm(mu,sigma),
	bg[u4a]~dnorm(0,0.5),
	c(bv,br,bo,bl,mu)~dnorm(0,0.5),
	phi~dexp(0.5),
	sigma~dhalfnorm(0,0.5)	
),data=dat, chains=4, cores=4, iter=1000, log_lik=T,control=list(adapt_delta=0.95))


#Figure 3########################
#Figure 3a########
post <- extract.samples(m)
dens(exp(post$bg) , show.zero=T, show.HPDI=0.89)

#Figure 3b###########
ns <- 100
G_seq <- seq(from=0 ,to=3.7, length.out=ns)

colors_grey <- c("grey17", 
            "grey46", 
            "grey65",
            "grey88") 


 plot(dat$Gln,dat$S,
     pch = 16, cex=0.5, xlab="Cumulative gun violence (magnitude)", ylab="Suicide mortality per 100,000 p-yrs",
          col = colors_grey[factor(dat$u4a)])

legend("topright",
       legend = c("urban", "suburban", "micro-medium metro ", "rural"),
       pch = 16,
       col = colors_grey) 

#urban
lambda1 <-function(G) exp( post$a[,1] + post$bg*G) 
mu1<-sapply(G_seq,lambda1)
lmu1 <- apply(mu1,2,mean)
lci1 <- apply(mu1,2,PI)
lines(G_seq,lmu1,lty=1, lwd=1)
shade(lci1, G_seq, xpd=T)

#suburban
lambda2 <-function(G) exp( post$a[,2] + post$bg*G) 
mu2<-sapply(G_seq,lambda2)
lmu2 <- apply(mu2,2,mean)
lci2 <- apply(mu2,2,PI)
lines(G_seq,lmu2,lty=2, lwd=1)
shade(lci2, G_seq, xpd=T)

#micro
lambda3 <-function(G) exp( post$a[,3] + post$bg*G) 
mu3<-sapply(G_seq,lambda3)
lmu3 <- apply(mu3,2,mean)
lci3 <- apply(mu3,2,PI)
lines(G_seq,lmu3,lty=3, lwd=1)
shade(lci3, G_seq, xpd=T)

#rural
lambda4 <-function(G) exp( post$a[,4] + post$bg*G) 
mu4<-sapply(G_seq,lambda4)
lmu4 <- apply(mu4,2,mean)
lci4 <- apply(mu4,2,PI)
lines(G_seq,lmu4,lty=4, lwd=1)
shade(lci4, G_seq, xpd=T)