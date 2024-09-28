#figure 1##
library(usmap)
library(ggplot2)

dmapS<-data.frame(fips=d$fips, suicide=d$S)

d$Gln <- log(d$crude_g+1)
dmapG<-data.frame(fips=d$fips, gun=d$Gln)

#figure 1a
 plot_usmap(
  regions = "counties",  data = dmapS, values = "suicide", color= "black") +
   scale_fill_continuous(name = "Mortality per 100k", low = "white", high = "red", label=scales::comma)+
    theme(legend.position = "right")

#figure 1b
 plot_usmap(
  regions = "counties",  data = dmapG, values = "gun", color= "black") +
 scale_fill_continuous(name = "Magnitude", low = "white", high = "darkblue", label=scales::comma)+ 
  theme(legend.position = "right")

#figure 2##
library(rethinking)
library(dagitty)
dag<-dagitty( "dag {
	G->S
	G<-R->S
	G<-V->S
	G<-U->S
	G<-u->S
	U->V
	G<->N->S
	G<-O->S
	U->O
	H->O
	G<-H->S
	G [exposure]
	S [outcome]
	u [unobserved]
}")

coordinates(dag)<-list(x=c(H=0.3,G=0,S=1,R=0.5,U=0.7,V=0.2,u=0.5, O=0.1, N=0.8 ),
							y=c(H=0.15,G=0,S=0,R=-0.1,U=-0.3,V=-0.3,u=0.2,  O=0.1, N=0.15))
							
drawdag(dag)

