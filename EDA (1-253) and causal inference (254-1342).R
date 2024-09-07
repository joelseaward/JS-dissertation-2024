##### loading things
install.packages("pcalg")
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
BiocManager::install("graph")
BiocManager::install("RBGL")
library("pcalg")
library("Rgraphviz")
load("laligatraining. data all 5 mins")
colnames(laliga.training.data.all.5.mins)[colnames(laliga.training.data.all.5.mins)=="Home.final.third.actions"]="Home.final.third.PSCs"
colnames(laliga.training.data.all.5.mins)[colnames(laliga.training.data.all.5.mins)=="Away.final.third.actions"]="Away.final.third.PSCs"
colnames(laliga.training.data.all.5.mins)[colnames(laliga.training.data.all.5.mins)=="Home.shots.per.final.third.action"]="Home.shots.per.final.third.PSC"
colnames(laliga.training.data.all.5.mins)[colnames(laliga.training.data.all.5.mins)=="Away.shots.per.final.third.action"]="Away.shots.per.final.third.PSC"
laliga.data.all.5.mins=laliga.training.data.all.5.mins
attach(laliga.data.all.5.mins)
training.index=which(Match%in%unique(Match)[1:600])
validation.index=which(Match%in%unique(Match)[601:843])
detach(laliga.data.all.5.mins)
laliga.training.data.all.5.mins=laliga.data.all.5.mins[training.index,]
laliga.validation.data.all.5.mins=laliga.data.all.5.mins[validation.index,]
attach(laliga.training.data.all.5.mins)
install.packages("corrplot")
library("corrplot")
install.packages("igraph")
library("igraph")
install.packages("CondIndTests")
library("CondIndTests")
install.packages("kpcalg")
library("kpcalg")
install.packages("miic")
library("miic")
install.packages("bnlearn")
library("bnlearn")
install.packages("tidyverse")
library("tidyverse")
install.packages("lmtest")
library("lmtest")
install.packages("xtable")
library("xtable")
install.packages("ppcor")
library("ppcor")

##########################
##### exploratory analysis
##########################

##### time in play
par(mfrow=c(1,1))
hist(time.in.play,
     main="5 minute periods",
     xlab="Time in play (s)",
     col=c(rep("red",10),rep("darkseagreen4",20)),
     breaks=seq(from=0,to=300,by=10),
)
legend("topright", 
       legend=c("Under 100","Over 100"), 
       fill=c("red","darkseagreen4"),)
quantile(time.in.play,0.01)
detach(laliga.training.data.all.5.mins)
laliga.training.data.all.5.mins=laliga.training.data.all.5.mins[laliga.training.data.all.5.mins$time.in.play>100,]
laliga.validation.data.all.5.mins=laliga.validation.data.all.5.mins[laliga.validation.data.all.5.mins$time.in.play>100,]
attach(laliga.training.data.all.5.mins)
write.csv(laliga.training.data.all.5.mins,file="laliga. training data all 5 mins restricted tip")  # for use in python
hist(time.in.play,
     main="Restricted to time in play over 100 seconds",
     xlab="Time in play (s)",
     col="darkseagreen4",
     xlim=c(0,300),
     breaks=20)

##### distributions, can be used on any variable

par(mfrow=c(2,2))
qqnorm(time.in.play,
       main="Time in play")
qqnorm(Home.possession,
       main="Home possession")
qqnorm(Home.passes.attempted,
       main="Home passes attempted")
qqnorm(Home.xG[Home.xG>0],
       main="Home.xG")
sqrd.Home.pass.completion.rate=(Home.pass.completion.rate)^2
qqnorm(Home.pass.completion.rate)
qqnorm(sqrd.Home.pass.completion.rate)
qqnorm(Home.xG)

par(mfrow=c(1,1))

normal.test.plot=function(x){
  parmfrow=c(2,1)
  hist(x)
  qqnorm(x)
}

poisson.test.plot=function(x,y){
  hist(x,freq=FALSE,
       ylim=c(0,0.1),
       main=print(y),
       xlab=NULL,
       col="darkseagreen4")
  m=mean(x)
  y=vector(length=max(x)+11)
  for(i in 1:(max(x)+11)){
    y[i]=dpois(i-1,m)}
  barplot(y,name.arg=0:(max(x)+10), 
          col=rgb(1,0,0,0.4),border="red",space=0,add=TRUE)
  legend("topright", 
         legend=c("Denisty histogram","Poisson PMF"), 
         fill=c("darkseagreen4",rgb(1,0,0,0.4)), 
         border=c("black","red"))
}

norm.test.plot=function(x,y){
  hist(x,
       ylim=c(0,0.1),
       main=print(y))
  m=mean(x)
  v=var(x)
  y=vector(length=max(x)+11)
  for(i in 1:(max(x)+11)){
    y[i]=length(x)*dnorm(i-1,m,sqrt(v))}
  lines(0:(max(x)+10),y)
}

poisson.test.plot(Home.passes.attempted[Home.Team=="Barcelona"],"Home passes attempted by Barcelona")
mean(Home.passes.attempted[Home.Team=="Barcelona"])
var(Home.passes.attempted[Home.Team=="Barcelona"])

##### team impact
tapply(Home.possession,Home.Team,summary)
most.featured.teams=function(n,home){
  if(home==TRUE){
    periods=vector(length=length(unique(Home.Team)))
    for(i in 1:length(unique(Home.Team))){
      periods[i]=length(which(Home.Team==unique(Home.Team)[i]))}
    x=cbind(unique(Home.Team),periods)
    x=x[order(periods,decreasing=TRUE),]}else{
      periods=vector(length=length(unique(Away.Team)))
      for(i in 1:length(unique(Away.Team))){
        periods[i]=length(which(Away.Team==unique(Away.Team)[i]))}
      x=cbind(unique(Away.Team),periods)
      x=x[order(periods,decreasing=TRUE),]}
  return(x[1:n,])
}
most.featured.home=most.featured.teams(16,TRUE)
most.featured.away=most.featured.teams(16,FALSE)

plotter=function(teams,variable){
  par(mfrow=c(4,4))
  par(mar=c(4.1,1,2.5,1))
  for(i in 1:length(unique(teams))){
    hist(variable[Home.Team==unique(teams)[i]],
         xlab=unique(teams)[i],
         breaks=20,
         main=NULL,
         col="darkseagreen4")
  }}
plotter(most.featured.home[,1],Home.possession)
plotter(most.featured.home[,1],Home.passes.attempted)
plotter(most.featured.home[,1],Home.avg.pass.length)

##### kruskal wallis

kruskal.test(x=Home.possession,g=Home.Team) ##### clearly significant 
kruskal.test(x=Home.passes.attempted,g=Home.Team)
kruskal.test(x=Home.avg.pass.length,g=Home.Team)

##### feature selection

Home.goals.up=Home.current.goals.in.match-Away.current.goals.in.match
correlations.spearman=function(home,data){
  names=colnames(data)
  correlation=vector(length=length(names))
  if(home==TRUE){
    xG.home.column=which(colnames(data)=="Home.xG")
    for(i in 1:length(names)){
      column=which(colnames(data)==names[i])
      correlation[i]=cor.test(data[,xG.home.column],data[,column],method="spearman")$p.value}}
  else{xG.away.column=which(colnames(data)=="Away.xG")
  for(i in 1:length(names)){
    column=which(colnames(data)==names[i])
    correlation[i]=cor.test(data[,xG.away.column],data[,column],method="spearman")$p.value}}
  return(cbind(names,correlation))
}
corrs.1=correlations.spearman(home=TRUE,laliga.training.data.all.5.mins[Home.Team=="Barcelona",7:86])
to.keep.1=as.vector(colnames(laliga.training.data.all.5.mins)[which(as.numeric(corrs.1[,2])<0.05)+6])
corrs.2=correlations.spearman(home=FALSE,laliga.training.data.all.5.mins[Home.Team=="Barcelona",7:86])
to.keep.2=as.vector(colnames(laliga.training.data.all.5.mins)[which(as.numeric(corrs.2[,2])<0.05)+6])
corrs.3=correlations.spearman(home=TRUE,laliga.training.data.all.5.mins[Away.Team=="Barcelona",7:86])
to.keep.3=as.vector(colnames(laliga.training.data.all.5.mins)[which(as.numeric(corrs.1[,2])<0.05)+6])
corrs.4=correlations.spearman(home=FALSE,laliga.training.data.all.5.mins[Away.Team=="Barcelona",7:86])
to.keep.4=as.vector(colnames(laliga.training.data.all.5.mins)[which(as.numeric(corrs.2[,2])<0.05)+6])
to.keep=union(union(to.keep.1,to.keep.2),union(to.keep.1,to.keep.2))

cor.test(Home.goals.up[which(Home.Team=="Barcelona")],Home.xG[which(Home.Team=="Barcelona")],method="spearman")
cor.test(Home.goals.up[which(Home.Team=="Barcelona")],Away.xG[which(Home.Team=="Barcelona")],method="spearman")
cor.test(Home.goals.up[which(Away.Team=="Barcelona")],Home.xG[which(Away.Team=="Barcelona")],method="spearman")
cor.test(Home.goals.up[which(Away.Team=="Barcelona")],Away.xG[which(Away.Team=="Barcelona")],method="spearman")
plot(Home.goals.up[which(Away.Team=="Barcelona")],Away.xG[which(Away.Team=="Barcelona")])
plot(Home.goals.up[which(Home.Team=="Barcelona")],Away.xG[which(Home.Team=="Barcelona")])

corrs.1.num=sapply(corrs.1[,2],FUN=as.numeric)

max.relation=function(data){
  names=colnames(data)
  xG.home.column=which(colnames(data)=="Home.xG") 
  xG.away.column=which(colnames(data)=="Away.xG")
  max.correlation=vector(length=length(names))
  for(i in 7:length(names)){
    column=which(colnames(data)==names[i])
    max.correlation[i]=max(abs(cor(data[data$Home.Team=="Barcelona",xG.home.column],data[data$Home.Team=="Barcelona",column],method="spearman")),
                           abs(cor(data[data$Home.Team=="Barcelona",xG.away.column],data[data$Home.Team=="Barcelona",column],method="spearman")),
                           abs(cor(data[data$Away.Team=="Barcelona",xG.home.column],data[data$Away.Team=="Barcelona",column],method="spearman")),
                           abs(cor(data[data$Away.Team=="Barcelona",xG.away.column],data[data$Away.Team=="Barcelona",column],method="spearman")))
  }
  return(cbind(names,max.correlation))
}

max.corrs=max.relation(laliga.training.data.all.5.mins)
quantile(as.numeric(max.corrs[,2]),0.5)
max.corrs[which(max.corrs[,2]>0.15),1]

##### Plots which are in report

par(mfrow=c(1,3))
cor.matrix.1=t(cor(Home.xG[Home.Team=="Barcelona"],laliga.training.data.all.5.mins[Home.Team=="Barcelona",9:36],
                   method="spearman"))
cor.matrix.2=t(cor(Home.xG[Home.Team=="Barcelona"],laliga.training.data.all.5.mins[Home.Team=="Barcelona",37:59],
                   method="spearman"))
cor.matrix.3=t(cor(Home.xG[Home.Team=="Barcelona"],laliga.training.data.all.5.mins[Home.Team=="Barcelona",c(60:74,77:86)],
                   method="spearman"))
corrplot(cor.matrix.1,
         method="color",
         addCoef.col = "black",
         cl.pos="n",
         number.cex=0.75,
         tl.cex=0.7,
         tl.col="darkseagreen4")
corrplot(cor.matrix.2,
         method="color",
         addCoef.col = "black",
         cl.pos="n",
         number.cex=0.75,
         tl.cex=0.7,
         tl.col="darkseagreen4")
corrplot(cor.matrix.3,
         method="color",
         addCoef.col = "black",
         cl.pos="n",
         number.cex=0.75,
         tl.cex=0.7,
         tl.col="darkseagreen4")

#####

cor.matrix=t(cor(Home.xG[Home.Team=="Barcelona"],laliga.training.data.all.5.mins[Home.Team=="Barcelona",7:86],
                 method="spearman"))
no.cor=which(abs(cor.matrix[,1])<0.01)

#####



################################
####### Causal Discovery #######
################################

##### chosen variables

variables=c( "Home.Team"              ,                          "Away.Team",
             "time.in.play"                                    ,
             "Home.possession"                                    ,
             "Home.passes.attempted"                                  ,"Away.passes.attempted"                                 ,
             "Home.passes.completed"                              ,"Away.passes.completed"                                 ,
             #     "Home.pass.completion.rate"                                  ,"Away.pass.completion.rate"                             ,
             "Away.avg.pass.length"                                   ,"Home.avg.pass.length"                                 ,
             "Home.crosses"                                          ,"Away.crosses"                                           ,
             "Home.corners"                                          ,"Away.corners"                                           ,
             "Home.shots"                                            ,"Away.shots" ,
             #      "Home.goals"                                            ,"Away.goals"                                             ,
             #      "Home.final.third.PSCs"                              ,"Away.final.third.PSCs"                               ,
             "Home.proportion.of.possession.spent.in.final.third"   ,"Away.proportion.of.possession.spent.in.final.third"    ,
             #     "Home.average.possession.length"                        ,"Away.average.possession.length"                         ,
             #     "Home.shots.per.final.third.PSC"                     ,"Away.shots.per.final.third.PSC"                      ,
             "Home.pass.progression"                                 ,"Away.pass.progression"                                  ,
             #     "Home.carries"                                          ,"Away.carries"                                           ,
             "Home.carry.progression"                                ,"Away.carry.progression"                                 ,
             "Home.average.xG.per.shot"                              ,"Away.average.xG.per.shot")#                           ,
#    "Home.passing.tempo"                                    ,"Away.passing.tempo")#                                     ,
#     "Home.progression.per.completed.pass"                   ,"Away.progression.per.completed.pass"                    ,
#    "Home.progression.per.carry"     ,                       "Away.progression.per.carry"        ,
#     "Home.xG"                                               ,"Away.xG")
variable.abbreviations=c( "HT",                  "AT"     ,
                          "T"                                                       ,
                          "HPos"                                    ,
                          "HPA"                                  ,"APA"                                 ,
                          "HPC"                              ,"APC"                                 ,
                          #              "HPR"                                  ,"APR"                             ,
                          "APL"                                   ,"HPL"                                 ,
                          "HCross"                                          ,"ACross"                                           ,
                          "HCorn"                                          ,"ACorn"                                           ,
                          "HS"                                            ,"AS" ,
                          #             "HG"                                            ,"AG"                                             ,
                          #             "HFTA"                              ,"AFTA"                               ,
                          "HPosFT"                            , "APosFT"                             ,
                          #             "HPosL"                        ,"APosL"                         ,
                          #            "HSpFTA"                     ,"ASpFTA"                      ,
                          "HPP"                                 ,"APP"                                  ,
                          #             "HC"                                          ,"AC"                                           ,
                          "HCP"                                ,"ACP"                                 ,
                          "HxGpS"                              ,"AxGpS"  )#                        ,
#         "HPT"                                    ,"APT")#                                     ,
#            "HPpP"                   ,"APpP"                    ,
#             "HPpC"     ,                       "APpC"        ,
#             "HxG"                                               ,"AxG")

#############################
###########  MIIC  ##########
#############################

##### method 1

miic.data.1=laliga.training.data.all.5.mins[,variables]
miic.data.1$Home.crosses=as.factor(miic.data.1$Home.crosses) 
miic.data.1$Away.crosses=as.factor(miic.data.1$Away.crosses)
miic.data.1$Home.corners=as.factor(miic.data.1$Home.corners) 
miic.data.1$Away.corners=as.factor(miic.data.1$Away.corners)
miic.data.1$Home.shots=as.factor(miic.data.1$Home.shots) 
miic.data.1$Away.shots=as.factor(miic.data.1$Away.shots)
colnames(miic.data.1)=variable.abbreviations
miic.fit=miic(miic.data.1,latent="yes")
plot(miic.fit,
     edge.arrow.size = 0.5,
     edge.width=0.001,
     vertex.size = 15,
     vertex.color = "lightblue",
     vertex.label.cex = 0.5)#,
# edge.color="black")

##### ADJUSTMENTS ##### for method 2
variables.updated=c( "Home.Team"              ,                          "Away.Team",
                     # "time.in.play"                                    ,
                     "Home.possession"                                    ,
                     "Home.passes.attempted"                                  ,"Away.passes.attempted"                                 ,
                     "Home.passes.completed"                              ,"Away.passes.completed"                                 ,
                     #     "Home.pass.completion.rate"                                  ,"Away.pass.completion.rate"                             ,
                     "Away.avg.pass.length"                                   ,"Home.avg.pass.length"                                 ,
                     "Home.crosses"                                          ,"Away.crosses"                                           ,
                     "Home.corners"                                          ,"Away.corners"                                           ,
                     "Home.shots"                                            ,"Away.shots" ,
                     #      "Home.goals"                                            ,"Away.goals"                                             ,
                     #      "Home.final.third.PSCs"                              ,"Away.final.third.PSCs"                               ,
                     "Home.proportion.of.possession.spent.in.final.third"   ,"Away.proportion.of.possession.spent.in.final.third"    ,
                     # "Home.average.possession.length"                        ,"Away.average.possession.length"                         ,
                     #     "Home.shots.per.final.third.PSC"                     ,"Away.shots.per.final.third.PSC"                      ,
                     "Home.pass.progression"                                 ,"Away.pass.progression"                                  ,
                     #     "Home.carries"                                          ,"Away.carries"                                           ,
                     "Home.carry.progression"                                ,"Away.carry.progression"                                 ,
                     "Home.average.xG.per.shot"                              ,"Away.average.xG.per.shot")#                           ,
#  "Home.passing.tempo"                                    ,"Away.passing.tempo")#                                     ,
#     "Home.progression.per.completed.pass"                   ,"Away.progression.per.completed.pass"                    ,
#    "Home.progression.per.carry"     ,                       "Away.progression.per.carry"        ,
#     "Home.xG"                                               ,"Away.xG")
variable.abbreviations.updated=c( "HT",                  "AT"     ,
                                  #   "T"                                                       ,
                                  "HPos"                                    ,
                                  "HPA"                                  ,"APA"                                 ,
                                  "HPC"                              ,"APC"                                 ,
                                  #              "HPR"                                  ,"APR"                             ,
                                  "APL"                                   ,"HPL"                                 ,
                                  "HCross"                                          ,"ACross"                                           ,
                                  "HCorn"                                          ,"ACorn"                                           ,
                                  "HS"                                            ,"AS" ,
                                  #             "HG"                                            ,"AG"                                             ,
                                  #             "HFTA"                              ,"AFTA"                               ,
                                  "HPosFT"                            , "APosFT"                             ,
                                  #"HPosL"                        ,"APosL"                         ,
                                  #            "HSpFTA"                     ,"ASpFTA"                      ,
                                  "HPP"                                 ,"APP"                                  ,
                                  #             "HC"                                          ,"AC"                                           ,
                                  "HCP"                                ,"ACP"                                 ,
                                  "HxGpS"                              ,"AxGpS" )#                         ,
#"HPT"                                    ,"APT")#                                     ,
#            "HPpP"                   ,"APpP"                    ,
#             "HPpC"     ,                       "APpC"        ,
#             "HxG"                                               ,"AxG")

miic.data.2=laliga.training.data.all.5.mins[,variables.updated]
miic.data.2[,-c(1,2,3,8,9,16,17,22,23)]=miic.data.2[,-c(1,2,3,8,9,16,17,22,23)]/laliga.training.data.all.5.mins$time.in.play
colnames(miic.data.2)=variable.abbreviations.updated
miic.data.divided.by.time=miic.data.2
miic.fit.divided.by.time=miic(miic.data.2,latent="yes")
plot(miic.fit.divided.by.time,
     edge.arrow.size = 0.5,
     edge.width=0.001,
     vertex.size = 15,
     vertex.color = "lightblue",
     vertex.label.cex = 0.5,
     edge.color="black")
###
variables.updated.no.teams=variables.updated[-c(1,2)]
variable.abbreviations.updated.no.teams=variable.abbreviations.updated[-c(1,2)]
miic.data.2=miic.data.2[,-c(1,2)]
for(i in 1:length(variables.updated.no.teams)){
  if((variable.abbreviations.updated.no.teams[i]!="HT")&(variable.abbreviations.updated.no.teams[i]!="AT")&(variable.abbreviations.updated.no.teams[i]!="T")){
    miic.data.2[,which(colnames(miic.data.2)==variable.abbreviations.updated.no.teams[i])]=residuals(lm(miic.data.2[,which(colnames(miic.data.2)==variable.abbreviations.updated.no.teams[i])]~laliga.training.data.all.5.mins$Home.Team+laliga.training.data.all.5.mins$Away.Team))}
}
### standardise
miic.data.2.scaled=scale(miic.data.2)
scale.mean=attr(miic.data.2.scaled,"scaled:center")
scale.sd=attr(miic.data.2.scaled,"scaled:scale")
miic.data.2.scaled=as.data.frame(lapply(as.data.frame(miic.data.2.scaled),round,digits=10))         # have to round or problems with miic as it only takes first _ digits when looking at factors, but takes each level as unique values, so get multiple levels which are the same. 10 digits is plenty anyway
###
par(mfrow=c(3,3))
for(i in 1:length(variables.updated.no.teams)){
  qqnorm(miic.data.2.scaled[,which(variables.updated.no.teams==variables.updated.no.teams[i])],
         main=variables.updated.no.teams[i]) }  # some variables are now gaussian
###
miic.fit.2=miic(miic.data.2.scaled,latent="yes")
plot(miic.fit.2,
     edge.arrow.size = 0.5,
     edge.width=0.001,
     vertex.size = 15,
     vertex.color = "lightblue",
     vertex.label.cex = 0.5,
     edge.color="black")

##### Now separate home and away
##### Though we have no assumed causal sufficiency, we still allow MIIC to search for latent variables. This is to check our assumption and if home variables are confounded by away variables or vice-versa.

miic.data.home.with.team=miic.data.1[c(1,2,3,4,5,7,10,11,13,15,17,19,21,23)]
miic.data.away.with.team=miic.data.1[c(1,2,3,4,6,8,9,12,14,16,18,20,22,24)]
miic.fit.home.with.team=miic(miic.data.home.with.team,latent="yes")
miic.fit.away.with.team=miic(miic.data.away.with.team,latent="yes")
par(mfrow=c(1,2))
par(mar=c(1,0,1,0))
theta=seq(0,2*pi,length.out=15)
custom.layout.1=matrix(c(
  cos(theta[1]), sin(theta[1]),  
  cos(theta[2]), sin(theta[2]),  
  cos(theta[3]), sin(theta[3]),  
  cos(theta[4]), sin(theta[4]), 
  cos(theta[5]), sin(theta[5]), 
  cos(theta[6]), sin(theta[6]), 
  cos(theta[7]), sin(theta[7]),
  cos(theta[8]), sin(theta[8]),
  cos(theta[9]), sin(theta[9]),
  cos(theta[10]), sin(theta[10]),
  cos(theta[11]),sin(theta[11]),
  cos(theta[12]),sin(theta[12]),
  cos(theta[13]),sin(theta[13]),
  cos(theta[14]),sin(theta[14])),
  nrow = 14, byrow = TRUE)
custom.layout.2=matrix(c(
  cos(theta[1]), sin(theta[1]),  
  cos(theta[2]), sin(theta[2]),  
  cos(theta[5]), sin(theta[5]),  
  cos(theta[3]), sin(theta[3]), 
  cos(theta[6]), sin(theta[6]), 
  cos(theta[7]), sin(theta[7]), 
  cos(theta[8]), sin(theta[8]),
  cos(theta[12]), sin(theta[12]),
  cos(theta[10]), sin(theta[10]),
  cos(theta[9]), sin(theta[9]),
  cos(theta[11]),sin(theta[11]),
  cos(theta[13]),sin(theta[13]),
  cos(theta[4]),sin(theta[4]),
  cos(theta[14]),sin(theta[14])), nrow = 14, byrow = TRUE)
plot(miic.fit.home.with.team,
     edge.arrow.size = 0.5,
     edge.width=0.001,
     vertex.size = 25,
     vertex.color = "lightblue",
     vertex.label.cex = 0.5,
     edge.color="black",layout=custom.layout.1)
title(main="Home",line=-4,adj=0.5,cex.main=1.5)
abline(v=1.2,col="black",lwd = 2)
plot(miic.fit.away.with.team,
     edge.arrow.size = 0.5,
     edge.width=0.001,
     vertex.size = 25,
     vertex.color = "lightblue",
     vertex.label.cex = 0.5,
     edge.color="black",layout=custom.layout.2)
title(main = "Away", line = -4, adj = 0.5, cex.main = 1.5)
View(cbind(miic.fit.home.with.team$all.edges.summary[,c(1,2,9,10,15)],miic.fit.away.with.team$all.edges.summary[,c(1,2,9,10,15)]))
View(miic.fit$all.edges.summary[,c(1,2,9,10,15)])
### COMBINING
APos=100-miic.data.1$HPos
Time=c(miic.data.1$T,miic.data.1$T)
Team=c(miic.data.1$HT,miic.data.1$AT)
Opp=c(miic.data.1$AT,miic.data.1$HT)
HorA=c(rep("H",10694),rep("A",10694))
Pos=c(miic.data.1$HPos,APos)
PA=c(miic.data.1$HPA,miic.data.1$APA)
PC=c(miic.data.1$HPC,miic.data.1$APC)
PL=c(miic.data.1$HPL,miic.data.1$APL)
Cross=c(miic.data.1$HCross,miic.data.1$ACross)
Corn=c(miic.data.1$HCorn,miic.data.1$ACorn)
S=c(miic.data.1$HS,miic.data.1$AS)
PosFT=c(miic.data.1$HPosFT,miic.data.1$APosFT)
PP=c(miic.data.1$HPP,miic.data.1$APP)
CP=c(miic.data.1$HCP,miic.data.1$APC)
xGpS=c(miic.data.1$HxGpS,miic.data.1$AxGpS)
mixed.data=as.data.frame(cbind(Time,Team,Opp,HorA,Pos,PA,PC,PL,Cross,Corn,S,PosFT,PP,CP,xGpS))
mixed.data$Pos=as.numeric(mixed.data$Pos)
mixed.data$Time=as.numeric(mixed.data$Time)
mixed.data$PA=as.numeric(mixed.data$PA)
mixed.data$PC=as.numeric(mixed.data$PC)
mixed.data$PL=as.numeric(mixed.data$PL)
mixed.data$PosFT=as.numeric(mixed.data$PosFT)
mixed.data$PP=as.numeric(mixed.data$PP)
mixed.data$CP=as.numeric(mixed.data$CP)
mixed.data$xGpS=as.numeric(mixed.data$xGpS)
mixed.data$Cross=as.factor(mixed.data$Cross)
mixed.data$Corn=as.factor(mixed.data$Corn)
mixed.data$S=as.factor(mixed.data$S)
par(mfrow=c(1,1))
miic.mixed.fit=miic(mixed.data,latent="yes")
plot(miic.mixed.fit,
     edge.arrow.size = 0.5,
     edge.width=0.001,
     vertex.size = 15,
     vertex.color = "lightblue",
     vertex.label.cex = 0.5,
     edge.color="black")
View(miic.mixed.fit$all.edges.summary[,c(1,2,9,10,15)])

miic(mixed.data[,c("PL","PC","PA","Team","PP")],latent="yes")
pcor(mixed.data[,-c(2,3,4)])

######################
##### VALIDATION #####
######################

final.variable.abbreviations=c("HCorn","HPosFT","HCross","HPP","HPos","ACorn","APosFT","ACross","APP","HS","AS","HxGpS","AxGpS")
final.data=miic.data.1[,final.variable.abbreviations]
final.data=as.data.frame(cbind(final.data,laliga.training.data.all.5.mins$Home.xG,laliga.training.data.all.5.mins$Away.xG,laliga.training.data.all.5.mins$Match,laliga.training.data.all.5.mins$Period,laliga.training.data.all.5.mins$Home.goals,laliga.training.data.all.5.mins$Away.goals))
colnames(final.data)=c(final.variable.abbreviations,"Home.xG","Away.xG","Match","Period","Home.goals","Away.goals")
final.data[,"HCorn"]=as.numeric(as.character(final.data[,"HCorn"]))
final.data[,"ACorn"]=as.numeric(as.character(final.data[,"ACorn"]))
final.data[,"HCross"]=as.numeric(as.character(final.data[,"HCross"]))
final.data[,"ACross"]=as.numeric(as.character(final.data[,"ACross"]))
final.data[,"HS"]=as.numeric(as.character(final.data[,"HS"]))
final.data[,"AS"]=as.numeric(as.character(final.data[,"AS"]))
final.data[,"Home.xG"]=as.numeric(final.data[,"Home.xG"])
final.data[,"Away.xG"]=as.numeric(final.data[,"Away.xG"])

##### setting up data, most columns should be the same as mixed.data, but recalculate to ensure consistent with xG
APos2=100-final.data$HPos
Pos2=c(final.data$HPos,APos2)
Cross2=c(final.data$HCross,final.data$ACross)
Corn2=c(final.data$HCorn,final.data$ACorn)
S2=c(final.data$HS,final.data$AS)
PosFT2=c(final.data$HPosFT,final.data$APosFT)
PP2=c(final.data$HPP,final.data$APP)
xGpS2=c(final.data$HxGpS,final.data$AxGpS)
xG2=c(final.data$Home.xG,final.data$Away.xG)
mixed.data.final=as.data.frame(cbind(Pos2,Cross2,Corn2,S2,PosFT2,PP2,xGpS2,xG2))
colnames(mixed.data.final)=c("Pos","Cross","Corn","S","PosFT","PP","xGpS","xG")
mixed.data.final$Pos=as.numeric(mixed.data.final$Pos)
mixed.data.final$PosFT=as.numeric(mixed.data.final$PosFT)
mixed.data.final$PP=as.numeric(mixed.data.final$PP)
mixed.data.final$xGpS=as.numeric(mixed.data.final$xGpS)
mixed.data.final$Cross=as.numeric(mixed.data.final$Cross)
mixed.data.final$Corn=as.numeric(mixed.data.final$Corn)
mixed.data.final$S=as.numeric(mixed.data.final$S)
mixed.data.final$xG=as.numeric(mixed.data.final$xG)
#####
validation.data=laliga.validation.data.all.5.mins[,variables]
colnames(validation.data)=variable.abbreviations
validation.data=validation.data[,final.variable.abbreviations]
final.validation.data=as.data.frame(cbind(validation.data,laliga.validation.data.all.5.mins$Home.xG,laliga.validation.data.all.5.mins$Away.xG,laliga.validation.data.all.5.mins$Match,laliga.validation.data.all.5.mins$Period,laliga.validation.data.all.5.mins$Home.goals,laliga.validation.data.all.5.mins$Away.goals))
colnames(final.validation.data)=c(final.variable.abbreviations,"Home.xG","Away.xG","Match","Period","Home.goals","Away.goals")
final.validation.data[,"HCorn"]=as.numeric(as.character(final.validation.data[,"HCorn"]))
final.validation.data[,"ACorn"]=as.numeric(as.character(final.validation.data[,"ACorn"]))
final.validation.data[,"HCross"]=as.numeric(as.character(final.validation.data[,"HCross"]))
final.validation.data[,"ACross"]=as.numeric(as.character(final.validation.data[,"ACross"]))
final.validation.data[,"HS"]=as.numeric(as.character(final.validation.data[,"HS"]))
final.validation.data[,"AS"]=as.numeric(as.character(final.validation.data[,"AS"]))
final.validation.data[,"Home.xG"]=as.numeric(final.validation.data[,"Home.xG"])
final.validation.data[,"Away.xG"]=as.numeric(final.validation.data[,"Away.xG"])

#####
APos3=100-final.validation.data$HPos
Pos3=c(final.validation.data$HPos,APos3)
Cross3=c(final.validation.data$HCross,final.validation.data$ACross)
Corn3=c(final.validation.data$HCorn,final.validation.data$ACorn)
S3=c(final.validation.data$HS,final.validation.data$AS)
PosFT3=c(final.validation.data$HPosFT,final.validation.data$APosFT)
PP3=c(final.validation.data$HPP,final.validation.data$APP)
xGpS3=c(final.validation.data$HxGpS,final.validation.data$AxGpS)
xG3=c(final.validation.data$Home.xG,final.validation.data$Away.xG)
mixed.validation.data.final=as.data.frame(cbind(Pos3,Cross3,Corn3,S3,PosFT3,PP3,xGpS3,xG3))
colnames(mixed.validation.data.final)=c("Pos","Cross","Corn","S","PosFT","PP","xGpS","xG")
mixed.validation.data.final$Pos=as.numeric(mixed.validation.data.final$Pos)
mixed.validation.data.final$PosFT=as.numeric(mixed.validation.data.final$PosFT)
mixed.validation.data.final$PP=as.numeric(mixed.validation.data.final$PP)
mixed.validation.data.final$xGpS=as.numeric(mixed.validation.data.final$xGpS)
mixed.validation.data.final$Cross=as.numeric(mixed.validation.data.final$Cross)
mixed.validation.data.final$Corn=as.numeric(mixed.validation.data.final$Corn)
mixed.validation.data.final$S=as.numeric(mixed.validation.data.final$S)
mixed.validation.data.final$xG=as.numeric(mixed.validation.data.final$xG)

##### xG per shot roughly same mean for each number of shots 
c(mean(mixed.data.final$xGpS[which(mixed.data.final$S==1)]),sd(mixed.data.final$xGpS[which(mixed.data.final$S==1)]))
c(mean(mixed.data.final$xGpS[which(mixed.data.final$S==2)]),sd(mixed.data.final$xGpS[which(mixed.data.final$S==2)]))
c(mean(mixed.data.final$xGpS[which(mixed.data.final$S==3)]),sd(mixed.data.final$xGpS[which(mixed.data.final$S==3)]))
c(mean(mixed.data.final$xGpS[which(mixed.data.final$S==4)]),sd(mixed.data.final$xGpS[which(mixed.data.final$S==4)]))
c(mean(mixed.data.final$xGpS[which(mixed.data.final$S==5)]),sd(mixed.data.final$xGpS[which(mixed.data.final$S==5)]))
c(mean(mixed.data.final$xGpS[which(mixed.data.final$S==6)]),sd(mixed.data.final$xGpS[which(mixed.data.final$S==6)]))
c(mean(mixed.data.final$xGpS[which(mixed.data.final$S==7)]),sd(mixed.data.final$xGpS[which(mixed.data.final$S==7)]))
c(mean(mixed.data.final$xGpS[which(mixed.data.final$S==8)]),sd(mixed.data.final$xGpS[which(mixed.data.final$S==8)]))
##### models 
xGpSlm=lm(xGpS~Cross+PosFT+Pos+PP,data=mixed.data.final)
Sglm=glm(S~Corn+Cross+PosFT+Pos+PP,data=mixed.data.final,family="poisson")
xGlm=lm(xG~Corn+Cross+PosFT+Pos+PP,data=mixed.data.final)
RSSxG=sum(residuals(xGlm)^2)
xG.predicted.training=(fitted.values(xGpSlm))*(fitted.values(Sglm))
RSSxGmodified=sum((xG.predicted.training-mixed.data.final$xG)^2)
SSxGval=sum((predict(xGlm,newdata=mixed.validation.data.final,type="response")-mixed.validation.data.final$xG)^2)
xG.predicted.validation=predict(xGpSlm,newdata=mixed.validation.data.final,type="response")*predict(Sglm,newdata=mixed.validation.data.final,type="response")
SSxGmodifiedval=sum((xG.predicted.validation-mixed.validation.data.final$xG)^2)

##### Functions for predicting results
set.seed(123)
match.counter=function(data){
  Home.goals=rep(0,length(unique(data$Match)))
  Away.goals=rep(0,length(unique(data$Match)))
  Result=vector(length=length(unique(data$Match)))
  for(i in 1:length(unique(data$Match))){
    for(j in c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19)){
      index=which((data$Match==unique(data$Match)[i])&(data$Period==j))
      if(length(index)==1){
        home.data=data[,c("HPos","HCorn","HCross","HPosFT","HPP")]
        away.data=data[,c("HPos","ACorn","ACross","APosFT","APP")]
        away.data$HPos=100-away.data$HPos
        colnames(home.data)=c("Pos","Corn","Cross","PosFT","PP")
        colnames(away.data)=c("Pos","Corn","Cross","PosFT","PP")
        H.av.xG=predict(xGpSlm,newdata=home.data[index,],type="response")
        A.av.xG=predict(xGpSlm,newdata=away.data[index,],type="response")
        H.S.mean=predict(Sglm,newdata=home.data[index,],type="response")
        A.S.mean=predict(Sglm,newdata=away.data[index,],type="response")
        H.S=rpois(1,H.S.mean)
        A.S=rpois(1,A.S.mean)
        H.G=rbinom(1,H.S,max(0,H.av.xG))
        A.G=rbinom(1,A.S,max(0,A.av.xG))
        Home.goals[i]=Home.goals[i]+H.G
        Away.goals[i]=Away.goals[i]+A.G}
    }
    if(Home.goals[i]>Away.goals[i]){Result[i]="HW"} 
    if(Home.goals[i]<Away.goals[i]){Result[i]="AW"} 
    if(Home.goals[i]==Away.goals[i]){Result[i]="D"} 
  }
  return(Result)
}
match.counter.actual=function(data){
  Home.goals=rep(0,length(unique(data$Match)))
  Away.goals=rep(0,length(unique(data$Match)))
  Result=vector(length=length(unique(data$Match)))
  for(i in 1:length(unique(data$Match))){
    for(j in c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19)){
      index=which((data$Match==unique(data$Match)[i])&(data$Period==j))
      if(length(index)==1){
        H.G=rbinom(1,data$HS[index],data$HxGpS[index])
        A.G=rbinom(1,data$AS[index],data$AxGpS[index])
        Home.goals[i]=Home.goals[i]+H.G
        Away.goals[i]=Away.goals[i]+A.G}
    }
    if(Home.goals[i]>Away.goals[i]){Result[i]="HW"} 
    if(Home.goals[i]<Away.goals[i]){Result[i]="AW"} 
    if(Home.goals[i]==Away.goals[i]){Result[i]="D"} 
  }
  return(Result)
}
set.seed(123)
match.counter.2=function(data){
  Home.goals=rep(0,length(unique(data$Match)))
  Away.goals=rep(0,length(unique(data$Match)))
  Result=vector(length=length(unique(data$Match)))
  for(i in 1:length(unique(data$Match))){
    for(j in c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19)){
      index=which((data$Match==unique(data$Match)[i])&(data$Period==j))
      if(length(index)==1){
        home.data=data[,c("HPos","HCorn","HCross","HPosFT","HPP")]
        away.data=data[,c("HPos","ACorn","ACross","APosFT","APP")]
        away.data$HPos=100-away.data$HPos
        colnames(home.data)=c("Pos","Corn","Cross","PosFT","PP")
        colnames(away.data)=c("Pos","Corn","Cross","PosFT","PP")
        H.xG=predict(xGlm,newdata=home.data[index,],type="response")
        A.xG=predict(xGlm,newdata=away.data[index,],type="response")
        H.G=rpois(1,max(0,H.xG))
        A.G=rpois(1,max(0,A.xG))
        Home.goals[i]=Home.goals[i]+H.G
        Away.goals[i]=Away.goals[i]+A.G}
    }
    if(Home.goals[i]>Away.goals[i]){Result[i]="HW"} 
    if(Home.goals[i]<Away.goals[i]){Result[i]="AW"} 
    if(Home.goals[i]==Away.goals[i]){Result[i]="D"} 
  }
  return(Result)
}
match.counter.actual.2=function(data){
  Home.goals=rep(0,length(unique(data$Match)))
  Away.goals=rep(0,length(unique(data$Match)))
  Result=vector(length=length(unique(data$Match)))
  for(i in 1:length(unique(data$Match))){
    for(j in c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19)){
      index=which((data$Match==unique(data$Match)[i])&(data$Period==j))
      if(length(index)==1){
        H.G=rpois(1,data$Home.xG[index])
        A.G=rpois(1,data$Away.xG[index])
        Home.goals[i]=Home.goals[i]+H.G
        Away.goals[i]=Away.goals[i]+A.G}
    }
    if(Home.goals[i]>Away.goals[i]){Result[i]="HW"} 
    if(Home.goals[i]<Away.goals[i]){Result[i]="AW"} 
    if(Home.goals[i]==Away.goals[i]){Result[i]="D"} 
  }
  return(Result)
}
match.counter.true=function(data){
  Home.goals=rep(0,length(unique(data$Match)))
  Away.goals=rep(0,length(unique(data$Match)))
  Result=vector(length=length(unique(data$Match)))
  for(i in 1:length(unique(data$Match))){
    for(j in c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19)){
      index=which((data$Match==unique(data$Match)[i])&(data$Period==j))
      if(length(index)==1){
        H.G=rep(0,1)
        A.G=rep(0,1)
        H.G=data$Home.goals[index]
        A.G=data$Away.goals[index]
        Home.goals[i]=Home.goals[i]+H.G
        Away.goals[i]=Away.goals[i]+A.G}
    }
    if(Home.goals[i]>Away.goals[i]){Result[i]="H"} 
    if(Home.goals[i]<Away.goals[i]){Result[i]="A"} 
    if(Home.goals[i]==Away.goals[i]){Result[i]="D"} 
  }
  return(Result)
}
match.outcomes=function(data){
  Hwins=rep(0,length(unique(data$Match)))
  Awins=rep(0,length(unique(data$Match)))
  Draws=rep(0,length(unique(data$Match)))
  for(i in 1:100){
    x=match.counter(data=data)
    for(j in 1:length(unique(data$Match))){
      if(x[j]=="HW"){Hwins[j]=Hwins[j]+1}
      if(x[j]=="AW"){Awins[j]=Awins[j]+1}
      if(x[j]=="D"){Draws[j]=Draws[j]+1}
    }}
  tab=rbind(t(Hwins/100),t(Awins/100),t(Draws/100))
  colnames(tab)=unique(data$Match)
  rownames(tab)=c("H","A","D")
  return(tab)
}
match.outcomes.actual=function(data){
  Hwins=rep(0,length(unique(data$Match)))
  Awins=rep(0,length(unique(data$Match)))
  Draws=rep(0,length(unique(data$Match)))
  for(i in 1:100){
    x=match.counter.actual(data=data)
    for(j in 1:length(unique(data$Match))){
      if(x[j]=="HW"){Hwins[j]=Hwins[j]+1}
      if(x[j]=="AW"){Awins[j]=Awins[j]+1}
      if(x[j]=="D"){Draws[j]=Draws[j]+1}
    }}
  tab=rbind(t(Hwins/100),t(Awins/100),t(Draws/100))
  colnames(tab)=unique(data$Match)
  rownames(tab)=c("H","A","D")
  return(tab)
}
match.outcomes.2=function(data){
  Hwins=rep(0,length(unique(data$Match)))
  Awins=rep(0,length(unique(data$Match)))
  Draws=rep(0,length(unique(data$Match)))
  for(i in 1:100){
    x=match.counter.2(data=data)
    for(j in 1:length(unique(data$Match))){
      if(x[j]=="HW"){Hwins[j]=Hwins[j]+1}
      if(x[j]=="AW"){Awins[j]=Awins[j]+1}
      if(x[j]=="D"){Draws[j]=Draws[j]+1}
    }}
  tab=rbind(t(Hwins/100),t(Awins/100),t(Draws/100))
  colnames(tab)=unique(data$Match)
  rownames(tab)=c("H","A","D")
  return(tab)
}
match.outcomes.actual.2=function(data){
  Hwins=rep(0,length(unique(data$Match)))
  Awins=rep(0,length(unique(data$Match)))
  Draws=rep(0,length(unique(data$Match)))
  for(i in 1:100){
    x=match.counter.actual.2(data=data)
    for(j in 1:length(unique(data$Match))){
      if(x[j]=="HW"){Hwins[j]=Hwins[j]+1}
      if(x[j]=="AW"){Awins[j]=Awins[j]+1}
      if(x[j]=="D"){Draws[j]=Draws[j]+1}
    }}
  tab=rbind(t(Hwins/100),t(Awins/100),t(Draws/100))
  colnames(tab)=unique(data$Match)
  rownames(tab)=c("H","A","D")
  return(tab)
}
#### predictions on validation set
match.outcomes.predicted.xG=match.outcomes(final.validation.data)
match.outcomes.actual.xG=match.outcomes.actual(final.validation.data)
match.outcomes.true=match.counter.true(final.validation.data)
match.result.predicted.xG=apply(match.outcomes.predicted.xG, 2, function(col) {
  rownames(match.outcomes.predicted.xG)[which.max(col)]
})
match.result.actual.xG=apply(match.outcomes.actual.xG, 2, function(col) {
  rownames(match.outcomes.actual.xG)[which.max(col)]
})
View(cbind(match.result.actual.xG,match.result.predicted.xG,match.outcomes.true))
sum(match.result.actual.xG==match.result.predicted.xG)/length(match.outcomes.true)
sum(match.result.actual.xG==match.outcomes.true)/length(match.outcomes.true)
sum(match.result.predicted.xG==match.outcomes.true)/length(match.outcomes.true)
(length(which(((match.result.actual.xG=="H")&(match.outcomes.true=="A"))|((match.result.actual.xG=="A")&(match.outcomes.true=="H"))))/length(match.outcomes.true))
(length(which(((match.result.predicted.xG=="H")&(match.outcomes.true=="A"))|((match.result.predicted.xG=="A")&(match.outcomes.true=="H"))))/length(match.outcomes.true))
###
match.outcomes.predicted.xG.2=match.outcomes.2(final.validation.data)
match.outcomes.actual.xG.2=match.outcomes.actual.2(final.validation.data)
match.result.predicted.xG.2=apply(match.outcomes.predicted.xG.2, 2, function(col) {
  rownames(match.outcomes.predicted.xG.2)[which.max(col)]
})
match.result.actual.xG.2=apply(match.outcomes.actual.xG.2, 2, function(col) {
  rownames(match.outcomes.actual.xG.2)[which.max(col)]
})
View(cbind(match.result.actual.xG.2,match.result.predicted.xG.2,match.outcomes.true))
sum(match.result.actual.xG.2==match.result.predicted.xG.2)/length(match.outcomes.true)
sum(match.result.actual.xG.2==match.outcomes.true)/length(match.outcomes.true)
sum(match.result.predicted.xG.2==match.outcomes.true)/length(match.outcomes.true)
(length(which(((match.result.actual.xG.2=="H")&(match.outcomes.true=="A"))|((match.result.actual.xG.2=="A")&(match.outcomes.true=="H"))))/length(match.outcomes.true))
(length(which(((match.result.predicted.xG.2=="H")&(match.outcomes.true=="A"))|((match.result.predicted.xG.2=="A")&(match.outcomes.true=="H"))))/length(match.outcomes.true))
length(which(match.result.predicted.xG.2=="D"))/length(match.outcomes.true)
length(which(match.result.predicted.xG=="D"))/length(match.outcomes.true)
##### predictions on training set

training.outcomes.predicted.xG=match.outcomes(final.data)
training.outcomes.actual.xG=match.outcomes.actual(final.data)
training.outcomes.true=match.counter.true(final.data)
training.result.predicted.xG=apply(training.outcomes.predicted.xG, 2, function(col) {
  rownames(training.outcomes.predicted.xG)[which.max(col)]
})
training.result.actual.xG=apply(training.outcomes.actual.xG, 2, function(col) {
  rownames(training.outcomes.actual.xG)[which.max(col)]
})
View(cbind(training.result.actual.xG,training.result.predicted.xG,training.outcomes.true))
sum(training.result.actual.xG==training.result.predicted.xG)/length(training.outcomes.true)
sum(training.result.actual.xG==training.outcomes.true)/length(training.outcomes.true)
sum(training.result.predicted.xG==training.outcomes.true)/length(training.outcomes.true)
(length(which(((training.result.actual.xG=="H")&(training.outcomes.true=="A"))|((training.result.actual.xG=="A")&(training.outcomes.true=="H"))))/length(training.outcomes.true))
(length(which(((training.result.predicted.xG=="H")&(training.outcomes.true=="A"))|((training.result.predicted.xG=="A")&(training.outcomes.true=="H"))))/length(training.outcomes.true))
###
training.outcomes.predicted.xG.2=match.outcomes.2(final.data)
training.outcomes.actual.xG.2=match.outcomes.actual.2(final.data)
training.result.predicted.xG.2=apply(training.outcomes.predicted.xG.2, 2, function(col) {
  rownames(training.outcomes.predicted.xG.2)[which.max(col)]
})
training.result.actual.xG.2=apply(training.outcomes.actual.xG.2, 2, function(col) {
  rownames(training.outcomes.actual.xG.2)[which.max(col)]
})
View(cbind(training.result.actual.xG.2,training.result.predicted.xG.2,training.outcomes.true))
sum(training.result.actual.xG.2==training.result.predicted.xG.2)/length(training.outcomes.true)
sum(training.result.actual.xG.2==training.outcomes.true)/length(training.outcomes.true)
sum(training.result.predicted.xG.2==training.outcomes.true)/length(training.outcomes.true)
(length(which(((training.result.actual.xG.2=="H")&(training.outcomes.true=="A"))|((training.result.actual.xG.2=="A")&(training.outcomes.true=="H"))))/length(training.outcomes.true))
(length(which(((training.result.predicted.xG.2=="H")&(training.outcomes.true=="A"))|((training.result.predicted.xG.2=="A")&(training.outcomes.true=="H"))))/length(training.outcomes.true))
#####

##################################
##### TESTING INDEPENDENCIES #####
##################################
miic(laliga.validation.data.all.5.mins[,c("Home.Team","Home.xG","Home.passes.completed","Home.passes.attempted","Home.avg.pass.length","time.in.play")],latent="yes")
miic(laliga.validation.data.all.5.mins[,c("Away.Team","Away.xG","Away.passes.completed","Away.passes.attempted","Away.avg.pass.length","time.in.play")],latent="yes")
miic(laliga.validation.data.all.5.mins[,c("Home.Team","Home.xG","Home.passes.completed")],latent="yes")
miic(laliga.validation.data.all.5.mins[,c("Away.Team","Away.xG","Away.passes.completed")],latent="yes")
valid.data.=laliga.validation.data.all.5.mins[,c("Home.xG","Away.xG","Home.pass.progression","Away.pass.progression","Home.corners","Away.corners",
                                                 "Home.proportion.of.possession.spent.in.final.third","Away.proportion.of.possession.spent.in.final.third",
                                                 "Home.crosses","Away.crosses","Home.possession")]
colnames(valid.data.)=c("HxG","AxG","HPP","APP","HCorn","ACorn",
                        "HPosFT","APosFT",
                        "HCross","ACross","HPos")
valid.data.numeric=valid.data.
valid.data.$HCorn=as.factor(valid.data.$HCorn)
valid.data.$ACorn=as.factor(valid.data.$ACorn)
valid.data.$HCross=as.factor(valid.data.$HCross)
valid.data.$ACross=as.factor(valid.data.$ACross)

miic.valid=miic(valid.data.,latent="yes")
plot(miic.valid,
     edge.arrow.size = 0.5,
     edge.width=0.001,
     vertex.size = 15,
     vertex.color = "lightblue",
     vertex.label.cex = 0.5,
     edge.color="black")
valid.data.numeric$HCorn=valid.data.numeric$HCorn+runif(4310,-0.001,0.001)
valid.data.numeric$ACorn=valid.data.numeric$ACorn+runif(4310,-0.001,0.001)
valid.data.numeric$HCross=valid.data.numeric$HCross+runif(4310,-0.001,0.001)
valid.data.numeric$ACross=valid.data.numeric$ACross+runif(4310,-0.001,0.001)

suffStat = list(data=valid.data.numeric, ic.method="hsic.perm")
set.seed=(123)
kernelCItest(x=which(colnames(valid.data.numeric)=="HxG"),y=which(colnames(valid.data.numeric)=="AxG"),S=which((colnames(valid.data.numeric)!="HxG")&(colnames(valid.data.numeric)!="AxG")),suffStat=suffStat)

####################################
##### DISCRETIZATION AND MODEL #####
####################################

midpoint.of.interval=function(interval_string){
  trimmed_string=substring(interval_string,2,nchar(interval_string)-1)
  bounds=strsplit(trimmed_string,",")[[1]]
  bounds=trimws(bounds)
  lower_bound=as.numeric(bounds[1])
  upper_bound=as.numeric(bounds[2])
  midpoint=(lower_bound+upper_bound)/2
  return(midpoint)
}

valid.discretization.data=laliga.validation.data.all.5.mins[,c(variables,"Home.xG","Away.xG")]
colnames(valid.discretization.data)=c(variable.abbreviations,"Home.xG","Away.xG")
VAR.discrete=discretize(rbind(miic.data.1[,c("T","HPA","APA","HPC","APC","HPos","APL","HPL","HPosFT","APosFT","HPP","APP","HCP","ACP")],valid.discretization.data[,c("T","HPA","APA","HPC","APC","HPos","APL","HPL","HPosFT","APosFT","HPP","APP","HCP","ACP")]),method="quantile",breaks=10)
VAR.discrete=apply(VAR.discrete,c(1,2),midpoint.of.interval)
VAR.discrete=as.data.frame(lapply(as.data.frame(VAR.discrete),as.factor))
discretize.xGpS.data=rbind(miic.data.1[,c("HxGpS","AxGpS")],valid.discretization.data[,c("HxGpS","AxGpS")])
quantiles.HxGpS=quantile(discretize.xGpS.data$HxGpS[discretize.xGpS.data$HxGpS!=0],probs=seq(0,1,length.out=10))
breaks.HxGpS=c(-Inf, 0, quantiles.HxGpS[-1]) 
midpoints.HxGpS=(breaks.HxGpS[-length(breaks.HxGpS)]+breaks.HxGpS[-1])/2
labels.HxGpS=c("0",formatC(midpoints.HxGpS[-1],format="fg",digits=3))
discretized.HxGpS=cut(discretize.xGpS.data$HxGpS,breaks=breaks.HxGpS,include.lowest=TRUE,labels=labels.HxGpS)
quantiles.AxGpS=quantile(discretize.xGpS.data$AxGpS[discretize.xGpS.data$AxGpS!=0],probs=seq(0,1,length.out=10))
breaks.AxGpS=c(-Inf, 0, quantiles.AxGpS[-1]) 
midpoints.AxGpS=(breaks.AxGpS[-length(breaks.AxGpS)]+breaks.AxGpS[-1])/2
labels.AxGpS=c("0",formatC(midpoints.AxGpS[-1],format="fg",digits=3))
discretized.AxGpS=cut(discretize.xGpS.data$AxGpS,breaks=breaks.AxGpS,include.lowest=TRUE,labels=labels.AxGpS)
discretize.xG.data=rbind(laliga.training.data.all.5.mins[,c("Home.xG","Away.xG")],valid.discretization.data[,c("Home.xG","Away.xG")])
quantiles.Home.xG=quantile(discretize.xG.data$Home.xG[discretize.xG.data$Home.xG!=0],probs=seq(0,1,length.out=10))
breaks.Home.xG=c(-Inf, 0, quantiles.Home.xG[-1]) 
midpoints.home.xG=(breaks.Home.xG[-length(breaks.Home.xG)]+breaks.Home.xG[-1])/2
labels.Home.xG=c("0",formatC(midpoints.home.xG[-1],format="fg",digits=3))
discretized.Home.xG=cut(discretize.xG.data$Home.xG,breaks=breaks.Home.xG,include.lowest=TRUE,labels=labels.Home.xG)
quantiles.Away.xG=quantile(discretize.xG.data$Away.xG[discretize.xG.data$Away.xG!=0],probs=seq(0,1,length.out=10))
breaks.Away.xG=c(-Inf, 0, quantiles.Away.xG[-1]) 
midpoints.away.xG=(breaks.Away.xG[-length(breaks.Away.xG)]+breaks.Away.xG[-1])/2
labels.Away.xG=c("0",formatC(midpoints.away.xG[-1],format="fg",digits=3))
discretized.Away.xG=cut(discretize.xG.data$Away.xG,breaks=breaks.Away.xG,include.lowest=TRUE,labels=labels.Away.xG)
discrete.data=as.data.frame(cbind(as.factor(c(laliga.training.data.all.5.mins$Home.Team,laliga.validation.data.all.5.mins$Home.Team)),as.factor(c(laliga.training.data.all.5.mins$Away.Team,laliga.validation.data.all.5.mins$Away.Team)),VAR.discrete,rbind(miic.data.1[,c("HCross","ACross","HCorn","ACorn","HS","AS")],valid.discretization.data[,c("HCross","ACross","HCorn","ACorn","HS","AS")]),discretized.HxGpS,discretized.AxGpS,discretized.Home.xG,discretized.Away.xG))
colnames(discrete.data)[colnames(discrete.data)=="discretized.HxGpS"]="HxGpS"
colnames(discrete.data)[colnames(discrete.data)=="discretized.AxGpS"]="AxGpS"
colnames(discrete.data)[colnames(discrete.data)=="discretized.Home.xG"]="HxG"
colnames(discrete.data)[colnames(discrete.data)=="discretized.Away.xG"]="AxG"
colnames(discrete.data)[1]="HT"
colnames(discrete.data)[2]="AT"
discrete.data.train=discrete.data[1:10694,]
discrete.data.valid=discrete.data[10695:15004,]
dag.factorisation="[T][HT][AT][HPL|HT][APL|AT][HPA|T:HT][APA|T:AT][HPC|HPL:HPA:HT][APC|APL:APA:AT][HCP|HPC][ACP|APC][HPos|HPC:APC][HPP|HPC:APL:HPL][APP|APC:HPL:APL][HPosFT|HPos:HPP:HPA][APosFT|HPos:APP:APA][HCross|HPosFT:HPos:HPA][ACross|APosFT:HPos:APA][HCorn|HPosFT:HPos:T][ACorn|APosFT:HPos:T][HS|HCorn:HPosFT:HPos][AS|ACorn:APosFT:HPos][HxGpS|HS:HCross:HPP][AxGpS|AS:ACross:APP][HxG|HS:HxGpS][AxG|AS:AxGpS]" 

dag=model2network(dag.factorisation)
dag.fit=bn.fit(dag,discrete.data.train)
set.seed(123)
mse.train.HxGpS=sum((as.numeric(as.character(predict(dag.fit,node="HxGpS",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HxGpS)))^2)/length(discrete.data.train$HxGpS)
na=which(is.na(predict(dag.fit,node="HxGpS",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.HxGpS=sum((as.numeric(as.character(predict(dag.fit,node="HxGpS",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$HxGpS[-na])))^2)/length(discrete.data.valid$HxGpS[-na])
bin.correct.valid.HxGpS=length(which(predict(dag.fit,node="HxGpS",data=discrete.data.valid)[-na]==discrete.data.valid$HxGpS[-na]))/length(discrete.data.valid$HxGpS[-na])

mse.train.AxGpS=sum((as.numeric(as.character(predict(dag.fit,node="AxGpS",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$AxGpS)))^2)/length(discrete.data.train$AxGpS)
na=which(is.na(predict(dag.fit,node="AxGpS",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.AxGpS=sum((as.numeric(as.character(predict(dag.fit,node="AxGpS",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$AxGpS[-na])))^2)/length(discrete.data.valid$AxGpS[-na])
bin.correct.valid.AxGpS=length(which(predict(dag.fit,node="AxGpS",data=discrete.data.valid)[-na]==discrete.data.valid$AxGpS[-na]))/length(discrete.data.valid$AxGpS[-na])

mse.train.HS=sum((as.numeric(as.character(predict(dag.fit,node="HS",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HS)))^2)/length(discrete.data.train$HS)
na=which(is.na(predict(dag.fit,node="HS",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.HS=sum((as.numeric(as.character(predict(dag.fit,node="HS",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$HS[-na])))^2)/length(discrete.data.valid$HS[-na])
bin.correct.valid.HS=length(which(predict(dag.fit,node="HS",data=discrete.data.valid)[-na]==discrete.data.valid$HS[-na]))/length(discrete.data.valid$HS[-na])

mse.train.AS=sum((as.numeric(as.character(predict(dag.fit,node="AS",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$AS)))^2)/length(discrete.data.train$AS)
na=which(is.na(predict(dag.fit,node="AS",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.AS=sum((as.numeric(as.character(predict(dag.fit,node="AS",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$AS[-na])))^2)/length(discrete.data.valid$AS[-na])
bin.correct.valid.AS=length(which(predict(dag.fit,node="AS",data=discrete.data.valid)[-na]==discrete.data.valid$AS[-na]))/length(discrete.data.valid$AS[-na])

mse.train.HxG=sum((as.numeric(as.character(predict(dag.fit,node="HxG",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HxG)))^2)/length(discrete.data.train$HxG)
na=which(is.na(predict(dag.fit,node="HxG",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.HxG=sum((as.numeric(as.character(predict(dag.fit,node="HxG",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$HxG[-na])))^2)/length(discrete.data.valid$HxG[-na])
bin.correct.valid.HxG=length(which(predict(dag.fit,node="HxG",data=discrete.data.valid)[-na]==discrete.data.valid$HxG[-na]))/length(discrete.data.valid$HxG[-na])

mse.train.AxG=sum((as.numeric(as.character(predict(dag.fit,node="AxG",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$AxG)))^2)/length(discrete.data.train$AxG)
na=which(is.na(predict(dag.fit,node="AxG",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.AxG=sum((as.numeric(as.character(predict(dag.fit,node="AxG",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$AxG[-na])))^2)/length(discrete.data.valid$AxG[-na])
bin.correct.valid.AxG=length(which(predict(dag.fit,node="AxG",data=discrete.data.valid)[-na]==discrete.data.valid$AxG[-na]))/length(discrete.data.valid$AxG[-na])

mse.train.HPos=sum((as.numeric(as.character(predict(dag.fit,node="HPos",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HPos)))^2)/length(discrete.data.train$HPos)
na=which(is.na(predict(dag.fit,node="HPos",data=discrete.data.valid)))  ## is empty
mse.valid.HPos=sum((as.numeric(as.character(predict(dag.fit,node="HPos",data=discrete.data.valid)))-as.numeric(as.character(discrete.data.valid$HPos)))^2)/length(discrete.data.valid$HPos)
bin.correct.valid.HPos=length(which(predict(dag.fit,node="HPos",data=discrete.data.valid)==discrete.data.valid$HPos))/length(discrete.data.valid$HPos)

mse.train.HCorn=sum((as.numeric(as.character(predict(dag.fit,node="HCorn",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HCorn)))^2)/length(discrete.data.train$HCorn)
na=which(is.na(predict(dag.fit,node="HCorn",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.HCorn=sum((as.numeric(as.character(predict(dag.fit,node="HCorn",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$HCorn[-na])))^2)/length(discrete.data.valid$HCorn[-na])
bin.correct.valid.HCorn=length(which(predict(dag.fit,node="HCorn",data=discrete.data.valid)[-na]==discrete.data.valid$HCorn[-na]))/length(discrete.data.valid$HCorn[-na])

mse.train.ACorn=sum((as.numeric(as.character(predict(dag.fit,node="ACorn",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$ACorn)))^2)/length(discrete.data.train$ACorn)
na=which(is.na(predict(dag.fit,node="ACorn",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.ACorn=sum((as.numeric(as.character(predict(dag.fit,node="ACorn",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$ACorn[-na])))^2)/length(discrete.data.valid$ACorn[-na])
bin.correct.valid.ACorn=length(which(predict(dag.fit,node="ACorn",data=discrete.data.valid)[-na]==discrete.data.valid$ACorn[-na]))/length(discrete.data.valid$ACorn[-na])

mse.train.HCross=sum((as.numeric(as.character(predict(dag.fit,node="HCross",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HCross)))^2)/length(discrete.data.train$HCross)
na=which(is.na(predict(dag.fit,node="HCross",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.HCross=sum((as.numeric(as.character(predict(dag.fit,node="HCross",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$HCross[-na])))^2)/length(discrete.data.valid$HCross[-na])
bin.correct.valid.HCross=length(which(predict(dag.fit,node="HCross",data=discrete.data.valid)[-na]==discrete.data.valid$HCross[-na]))/length(discrete.data.valid$HCross[-na])

mse.train.ACross=sum((as.numeric(as.character(predict(dag.fit,node="ACross",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$ACross)))^2)/length(discrete.data.train$ACross)
na=which(is.na(predict(dag.fit,node="ACross",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.ACross=sum((as.numeric(as.character(predict(dag.fit,node="ACross",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$ACross[-na])))^2)/length(discrete.data.valid$ACross[-na])
bin.correct.valid.ACross=length(which(predict(dag.fit,node="ACross",data=discrete.data.valid)[-na]==discrete.data.valid$ACross[-na]))/length(discrete.data.valid$ACross[-na])

mse.train.HPP=sum((as.numeric(as.character(predict(dag.fit,node="HPP",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HPP)))^2)/length(discrete.data.train$HPP)
na=which(is.na(predict(dag.fit,node="HPP",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.HPP=sum((as.numeric(as.character(predict(dag.fit,node="HPP",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$HPP[-na])))^2)/length(discrete.data.valid$HPP[-na])
bin.correct.valid.HPP=length(which(predict(dag.fit,node="HPP",data=discrete.data.valid)[-na]==discrete.data.valid$HPP[-na]))/length(discrete.data.valid$HPP[-na])

mse.train.APP=sum((as.numeric(as.character(predict(dag.fit,node="APP",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$APP)))^2)/length(discrete.data.train$APP)
na=which(is.na(predict(dag.fit,node="APP",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.APP=sum((as.numeric(as.character(predict(dag.fit,node="APP",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$APP[-na])))^2)/length(discrete.data.valid$APP[-na])
bin.correct.valid.APP=length(which(predict(dag.fit,node="APP",data=discrete.data.valid)[-na]==discrete.data.valid$APP[-na]))/length(discrete.data.valid$APP[-na])

mse.train.HPosFT=sum((as.numeric(as.character(predict(dag.fit,node="HPosFT",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HPosFT)))^2)/length(discrete.data.train$HPosFT)
na=which(is.na(predict(dag.fit,node="HPosFT",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.HPosFT=sum((as.numeric(as.character(predict(dag.fit,node="HPosFT",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$HPosFT[-na])))^2)/length(discrete.data.valid$HPosFT[-na])
bin.correct.valid.HPosFT=length(which(predict(dag.fit,node="HPosFT",data=discrete.data.valid)[-na]==discrete.data.valid$HPosFT[-na]))/length(discrete.data.valid$HPosFT[-na])

mse.train.APosFT=sum((as.numeric(as.character(predict(dag.fit,node="APosFT",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$APosFT)))^2)/length(discrete.data.train$APosFT)
na=which(is.na(predict(dag.fit,node="APosFT",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.APosFT=sum((as.numeric(as.character(predict(dag.fit,node="APosFT",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$APosFT[-na])))^2)/length(discrete.data.valid$APosFT[-na])
bin.correct.valid.APosFT=length(which(predict(dag.fit,node="APosFT",data=discrete.data.valid)[-na]==discrete.data.valid$APosFT[-na]))/length(discrete.data.valid$APosFT[-na])

mse.train.HPC=sum((as.numeric(as.character(predict(dag.fit,node="HPC",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HPC)))^2)/length(discrete.data.train$HPC)
na=which(is.na(predict(dag.fit,node="HPC",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.HPC=sum((as.numeric(as.character(predict(dag.fit,node="HPC",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$HPC[-na])))^2)/length(discrete.data.valid$HPC[-na])
bin.correct.valid.HPC=length(which(predict(dag.fit,node="HPC",data=discrete.data.valid)[-na]==discrete.data.valid$HPC[-na]))/length(discrete.data.valid$HPC[-na])

mse.train.APC=sum((as.numeric(as.character(predict(dag.fit,node="APC",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$APC)))^2)/length(discrete.data.train$APC)
na=which(is.na(predict(dag.fit,node="APC",data=discrete.data.valid)))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
mse.valid.APC=sum((as.numeric(as.character(predict(dag.fit,node="APC",data=discrete.data.valid)[-na]))-as.numeric(as.character(discrete.data.valid$APC[-na])))^2)/length(discrete.data.valid$APC[-na])
bin.correct.valid.APC=length(which(predict(dag.fit,node="APC",data=discrete.data.valid)[-na]==discrete.data.valid$APC[-na]))/length(discrete.data.valid$APC[-na])

mse.train.HPA=sum((as.numeric(as.character(predict(dag.fit,node="HPA",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HPA)))^2)/length(discrete.data.train$HPA)
na=which(is.na(predict(dag.fit,node="HPA",data=discrete.data.valid)))  ##is empty
mse.valid.HPA=sum((as.numeric(as.character(predict(dag.fit,node="HPA",data=discrete.data.valid)))-as.numeric(as.character(discrete.data.valid$HPA)))^2)/length(discrete.data.valid$HPA)
bin.correct.valid.HPA=length(which(predict(dag.fit,node="HPA",data=discrete.data.valid)==discrete.data.valid$HPA))/length(discrete.data.valid$HPA)

mse.train.APA=sum((as.numeric(as.character(predict(dag.fit,node="APA",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$APA)))^2)/length(discrete.data.train$APA)
na=which(is.na(predict(dag.fit,node="APA",data=discrete.data.valid)))  ##empty
mse.valid.APA=sum((as.numeric(as.character(predict(dag.fit,node="APA",data=discrete.data.valid)))-as.numeric(as.character(discrete.data.valid$APA)))^2)/length(discrete.data.valid$APA)
bin.correct.valid.APA=length(which(predict(dag.fit,node="APA",data=discrete.data.valid)==discrete.data.valid$APA))/length(discrete.data.valid$APA)

mse.train.HCP=sum((as.numeric(as.character(predict(dag.fit,node="HCP",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HCP)))^2)/length(discrete.data.train$HCP)
na=which(is.na(predict(dag.fit,node="HCP",data=discrete.data.valid)))  ##empty
mse.valid.HCP=sum((as.numeric(as.character(predict(dag.fit,node="HCP",data=discrete.data.valid)))-as.numeric(as.character(discrete.data.valid$HCP)))^2)/length(discrete.data.valid$HCP)
bin.correct.valid.HCP=length(which(predict(dag.fit,node="HCP",data=discrete.data.valid)==discrete.data.valid$HCP))/length(discrete.data.valid$HCP)

mse.train.ACP=sum((as.numeric(as.character(predict(dag.fit,node="ACP",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$ACP)))^2)/length(discrete.data.train$ACP)
na=which(is.na(predict(dag.fit,node="ACP",data=discrete.data.valid)))  ##empty
mse.valid.ACP=sum((as.numeric(as.character(predict(dag.fit,node="ACP",data=discrete.data.valid)))-as.numeric(as.character(discrete.data.valid$ACP)))^2)/length(discrete.data.valid$ACP)
bin.correct.valid.ACP=length(which(predict(dag.fit,node="ACP",data=discrete.data.valid)==discrete.data.valid$ACP))/length(discrete.data.valid$ACP)

mse.train.HPL=sum((as.numeric(as.character(predict(dag.fit,node="HPL",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$HPL)))^2)/length(discrete.data.train$HPL)
na=which(is.na(predict(dag.fit,node="HPL",data=discrete.data.valid)))  ##empty
mse.valid.HPL=sum((as.numeric(as.character(predict(dag.fit,node="HPL",data=discrete.data.valid)))-as.numeric(as.character(discrete.data.valid$HPL)))^2)/length(discrete.data.valid$HPL)
bin.correct.valid.HCP=length(which(predict(dag.fit,node="HPL",data=discrete.data.valid)==discrete.data.valid$HPL))/length(discrete.data.valid$HPL)

mse.train.APL=sum((as.numeric(as.character(predict(dag.fit,node="APL",data=discrete.data.train)))-as.numeric(as.character(discrete.data.train$APL)))^2)/length(discrete.data.train$APL)
na=which(is.na(predict(dag.fit,node="APL",data=discrete.data.valid)))  ##empty
mse.valid.APL=sum((as.numeric(as.character(predict(dag.fit,node="APL",data=discrete.data.valid)))-as.numeric(as.character(discrete.data.valid$APL)))^2)/length(discrete.data.valid$APL)
bin.correct.valid.APL=length(which(predict(dag.fit,node="APL",data=discrete.data.valid)==discrete.data.valid$APL))/length(discrete.data.valid$APL)

Variable=c("HxGpS","AxGpS","HS","AS","HCorn","ACorn","HPosFT","APosFT","HCross","ACross","HPP","APP","HPos","HPC","APC","HPA","APA","HPL","APL","HCP","ACP")
train.MSE=c(mse.train.HxGpS,mse.train.AxGpS,mse.train.HS,mse.train.AS,mse.train.HCorn,mse.train.ACorn,mse.train.HPosFT,mse.train.APosFT,mse.train.HCross,mse.train.ACross,mse.train.HPP,mse.train.APP,mse.train.HPos,mse.train.HPC,mse.train.APC,mse.train.HPA,mse.train.APA,mse.train.HPL,mse.train.APL,mse.train.HCP,mse.train.ACP)
valid.MSE=c(mse.valid.HxGpS,mse.valid.AxGpS,mse.valid.HS,mse.valid.AS,mse.valid.HCorn,mse.valid.ACorn,mse.valid.HPosFT,mse.valid.APosFT,mse.valid.HCross,mse.valid.ACross,mse.valid.HPP,mse.valid.APP,mse.valid.HPos,mse.valid.HPC,mse.valid.APC,mse.valid.HPA,mse.valid.APA,mse.valid.HPL,mse.valid.APL,mse.valid.HCP,mse.valid.ACP)
train.MSE=round(train.MSE,digits=3)
valid.MSE=round(valid.MSE,digits=3)

MSE.results=as.data.frame(cbind(Variable,train.MSE,valid.MSE))
colnames(MSE.results)=c("Variable","Training MSE","Validation MSE")

output.file="MSE.tables.tex"
sink(output.file)
  latex.tab=xtable(MSE.results)
  print(latex.tab,include.rownames=FALSE,booktabs=TRUE)
sink()


output.file="dag.tables.tex"
sink(output.file)
for(node in names(dag.fit)){
  tab=as.data.frame(as.table(dag.fit[[node]]$prob))
  latex.tab=xtable(tab,caption=paste("Conditional Probability Table for",node))
  print(latex.tab,include.rownames=FALSE,booktabs=TRUE)}
sink()

###############
##### DBN #####
###############

current.period.data=cbind(laliga.training.data.all.5.mins[-1,c("Match","Period")],miic.data.1[-1,])
previous.period.data=miic.data.1[-10694,]
colnames(previous.period.data)=c("HT.prev","AT.prev","T.prev","HPos.prev","HPA.prev","APA.prev","HPC.prev","APC.prev","APL.prev","HPL.prev","HCross.prev","ACross.prev","HCorn.prev","ACorn.prev","HS.prev","AS.prev","HPosFT.prev","APosFT.prev","HPP.prev","APP.prev","HCP.prev","ACP.prev","HxGpS.prev","AxGpS.prev")
data.with.lag=as.data.frame(cbind(current.period.data,previous.period.data))
bad.indices=NULL
for(i in 1:length(data.with.lag$Period)){
  if(as.numeric(data.with.lag$Period[i])>10){data.with.lag$Period[i]=as.numeric(data.with.lag$Period[i])-1}}
for(i in 2:length(data.with.lag$Period)){
  if(as.numeric(data.with.lag$Period[i])!=(as.numeric(data.with.lag$Period[i-1])+1)){    ##new match or removed periods
    bad.indices=c(bad.indices,i)
  }}
data.with.lag=data.with.lag[-bad.indices,]

L.HxGpS.lag.2=miic(data.with.lag[,c("HxGpS","HCross","HS","HPP","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.AxGpS.lag.2=miic(data.with.lag[,c("AxGpS","ACross","AS","APP","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HS.lag.2=miic(data.with.lag[,c("HS","HCorn","HPosFT","HPos","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.AS.lag.2=miic(data.with.lag[,c("AS","ACorn","APosFT","HPos","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HPosFT.lag.2=miic(data.with.lag[,c("HPosFT","HPP","HPA","HPos","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.APosFT.lag.2=miic(data.with.lag[,c("APosFT","APP","APA","HPos","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HCross.lag.2=miic(data.with.lag[,c("HCross","HPosFT","HPA","HPos","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.ACross.lag.2=miic(data.with.lag[,c("ACross","APosFT","APA","HPos","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HCorn.lag.2=miic(data.with.lag[,c("HCorn","HPosFT","T","HPos","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.ACorn.lag.2=miic(data.with.lag[,c("ACorn","APosFT","T","HPos","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HPos.lag.2=miic(data.with.lag[,c("HPos","HPC","APC","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HPP.lag.2=miic(data.with.lag[,c("HPP","HPC","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.APP.lag.2=miic(data.with.lag[,c("APP","APC","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HPC.lag.2=miic(data.with.lag[,c("HPC","HPA","HPL","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.APC.lag.2=miic(data.with.lag[,c("APC","APA","APL","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HCP.lag.2=miic(data.with.lag[,c("HCP","HPC","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.ACP.lag.2=miic(data.with.lag[,c("ACP","APC","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HPA.lag.2=miic(data.with.lag[,c("HPA","T","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.APA.lag.2=miic(data.with.lag[,c("APA","T","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.HPL.lag.2=miic(data.with.lag[,c("HPL","HxGpS.prev","HCross.prev","HCP.prev","HCorn.prev","HS.prev","HPos.prev","AxGpS.prev","HPP.prev","HPC.prev","HPA.prev","HPL.prev","T.prev","AS.prev","ACorn.prev","APosFT.prev","ACross.prev","ACP.prev","APP.prev","APA.prev","APC.prev","APL.prev","HT.prev","AT.prev")],latent="yes")
L.APL.lag.2=miic(data.with.lag[,c("APL","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")
L.T.lag.2=miic(data.with.lag[,c("T","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev","HT.prev","AT.prev")],latent="yes")

plot(L.HxGpS.lag.2)   #clear but team
plot(L.AxGpS.lag.2)   #clear but team
plot(L.HS.lag.2)      #clear
plot(L.AS.lag.2)      #clear
plot(L.HPosFT.lag.2)  #one edge with HposFT.prev
plot(L.APosFT.lag.2)  #clear
plot(L.HCross.lag.2)  #clear
plot(L.ACross.lag.2)  #clear
plot(L.HCorn.lag.2)   #clear
plot(L.ACorn.lag.2)   #clear
plot(L.HPP.lag.2)     #edge with HPP.prev
plot(L.APP.lag.2)     #edge with HPL.prev
plot(L.HPC.lag.2)     #edge with HPC.prev
plot(L.APC.lag.2)     #edge with ACP.prev
plot(L.HCP.lag.2)     #clear
plot(L.ACP.lag.2)     #edge with ACP.prev
plot(L.HPA.lag.2)     #edge with HPC.prev and ACP.prev
plot(L.APA.lag.2)     #edge with APC.prev and ACP.prev
plot(L.HPL.lag.2)     #edge with HPL.prev
plot(L.APL.lag.2)     #edge with APL.prev
plot(L.HPos.lag.2)    #clear
plot(L.T.lag.2)       #clear

All.lag=miic(data.with.lag[c("AxGpS","ACross","ACP","ACorn","AS","HPos","HxGpS","APP","APC","APA","APL","T","HS","HCorn","HPosFT","HCross","HCP","HPP","HPA","HPC","HPL","HT","AT","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev")],latent="yes")

View(All.lag$all.edges.summary)

L.HPC.lag.2.for.sign=miic(data.with.lag[,c("HPC","HPA","HPL","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev")],latent="yes")
L.APC.lag.2.for.sign=miic(data.with.lag[,c("APC","APA","APL","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev")],latent="yes")
L.HPA.lag.2.for.sign=miic(data.with.lag[,c("HPA","T","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev")],latent="yes")
L.APA.lag.2.for.sign=miic(data.with.lag[,c("APA","T","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HxGpS.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev")],latent="yes")
L.HPL.lag.2.for.sign=miic(data.with.lag[,c("HPL","HxGpS.prev","HCross.prev","HCP.prev","HCorn.prev","HS.prev","HPos.prev","AxGpS.prev","HPP.prev","HPC.prev","HPA.prev","HPL.prev","T.prev","AS.prev","ACorn.prev","APosFT.prev","ACross.prev","ACP.prev","APP.prev","APA.prev","APC.prev","APL.prev")],latent="yes")
L.APL.lag.2.for.sign=miic(data.with.lag[,c("APL","AxGpS.prev","ACross.prev","ACP.prev","ACorn.prev","AS.prev","HPos.prev","HxGpS.prev","APP.prev","APC.prev","APA.prev","APL.prev","T.prev","HS.prev","HCorn.prev","HPosFT.prev","HCross.prev","HCP.prev","HPP.prev","HPA.prev","HPC.prev","HPL.prev")],latent="yes")

##### DBN fit
current.period.discrete.data=cbind(laliga.training.data.all.5.mins[-1,c("Match","Period")],discrete.data.train[-1,])
previous.period.discrete.data=discrete.data.train[-10694,-c(1,2)]
colnames(previous.period.discrete.data)=c("T.prev","HPA.prev","APA.prev","HPC.prev","APC.prev","HPos.prev","APL.prev","HPL.prev","HPosFT.prev","APosFT.prev","HPP.prev","APP.prev","HCP.prev","ACP.prev","HCross.prev","ACross.prev","HCorn.prev","ACorn.prev","HS.prev","AS.prev","HxGpS.prev","AxGpS.prev","HxG.prev","AxG.prev")
discrete.data.with.lag=as.data.frame(cbind(current.period.discrete.data,previous.period.discrete.data))
bad.indices.discrete=NULL
for(i in 1:length(discrete.data.with.lag$Period)){
  if(as.numeric(discrete.data.with.lag$Period[i])>10){discrete.data.with.lag$Period[i]=as.numeric(discrete.data.with.lag$Period[i])-1}}
for(i in 2:length(discrete.data.with.lag$Period)){
  if(as.numeric(discrete.data.with.lag$Period[i])!=(as.numeric(discrete.data.with.lag$Period[i-1])+1)){    ##new match or removed periods
    bad.indices.discrete=c(bad.indices.discrete,i)
  }}
discrete.data.with.lag=discrete.data.with.lag[-bad.indices.discrete,]

dag.dynamic=model2network(paste0("[T.prev][HT][AT][HPL.prev|HT][APL.prev|AT][HPA.prev|T.prev:HT][APA.prev|T.prev:AT][HPC.prev|HPL.prev:HPA.prev:HT][APC.prev|APL.prev:APA.prev:AT][HCP.prev|HPC.prev][ACP.prev|APC.prev][HPos.prev|HPC.prev:APC.prev][HPP.prev|HPC.prev:APL.prev:HPL.prev][APP.prev|APC.prev:HPL.prev:APL.prev][HPosFT.prev|HPos.prev:HPP.prev:HPA.prev][APosFT.prev|HPos.prev:APP.prev:APA.prev][HCross.prev|HPosFT.prev:HPos.prev:HPA.prev][ACross.prev|APosFT.prev:HPos.prev:APA.prev][HCorn.prev|HPosFT.prev:HPos.prev:T.prev][ACorn.prev|APosFT.prev:HPos.prev:T.prev][HS.prev|HCorn.prev:HPosFT.prev:HPos.prev][AS.prev|ACorn.prev:APosFT.prev:HPos.prev][HxGpS.prev|HS.prev:HCross.prev:HPP.prev][AxGpS.prev|AS.prev:ACross.prev:APP.prev][HxG.prev|HS.prev:HxGpS.prev][AxG.prev|AS.prev:AxGpS.prev][T|T.prev][HPL|HPL.prev:HT][APL|APL.prev:AT][HPA|T:HT:HPC.prev][APA|T:AT:APC.prev][HPC|HPL:HPA:HPC.prev:HT][APC|APL:APA:APC.prev:AT][HCP|HPC:HCP.prev][ACP|APC:ACP.prev][HPos|HPC:APC][HPP|HPC:APL:HPL:HPP.prev][APP|APC:HPL:APL:APP.prev][HPosFT|HPos:HPP:HPA:HPosFT.prev][APosFT|HPos:APP:APA:APosFT.prev][HCross|HPosFT:HPos:HPA][ACross|APosFT:HPos:APA][HCorn|HPosFT:HPos:T][ACorn|APosFT:HPos:T][HS|HCorn:HPosFT:HPos][AS|ACorn:APosFT:HPos][HxGpS|HS:HCross:HPP][AxGpS|AS:ACross:APP][HxG|HS:HxGpS][AxG|AS:AxGpS]"))
dag.fit.dynamic=bn.fit(dag.dynamic,discrete.data.with.lag[,-c(1,2)])

##### validation of dbn

current.period.discrete.data.valid=cbind(laliga.validation.data.all.5.mins[-1,c("Match","Period")],discrete.data.valid[-1,])
previous.period.discrete.data.valid=discrete.data.valid[-4310,-c(1,2)]
colnames(previous.period.discrete.data.valid)=c("T.prev","HPA.prev","APA.prev","HPC.prev","APC.prev","HPos.prev","APL.prev","HPL.prev","HPosFT.prev","APosFT.prev","HPP.prev","APP.prev","HCP.prev","ACP.prev","HCross.prev","ACross.prev","HCorn.prev","ACorn.prev","HS.prev","AS.prev","HxGpS.prev","AxGpS.prev","HxG.prev","AxG.prev")
discrete.data.with.lag.valid=as.data.frame(cbind(current.period.discrete.data.valid,previous.period.discrete.data.valid))
bad.indices.discrete.valid=NULL
for(i in 1:length(discrete.data.with.lag.valid$Period)){
  if(as.numeric(discrete.data.with.lag.valid$Period[i])>10){discrete.data.with.lag.valid$Period[i]=as.numeric(discrete.data.with.lag.valid$Period[i])-1}}
for(i in 2:length(discrete.data.with.lag.valid$Period)){
  if(as.numeric(discrete.data.with.lag.valid$Period[i])!=(as.numeric(discrete.data.with.lag.valid$Period[i-1])+1)){    ##new match or removed periods
    bad.indices.discrete.valid=c(bad.indices.discrete.valid,i)
  }}
discrete.data.with.lag.valid=discrete.data.with.lag.valid[-bad.indices.discrete.valid,]
set.seed(123)
dyn.mse.train.HxGpS=sum((as.numeric(as.character(predict(dag.fit,node="HxGpS",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HxGpS)))^2)/length(discrete.data.with.lag$HxGpS)
na=which(is.na(predict(dag.fit,node="HxGpS",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.HxGpS=sum((as.numeric(as.character(predict(dag.fit,node="HxGpS",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$HxGpS[-na])))^2)/length(discrete.data.with.lag.valid$HxGpS[-na])
dyn.bin.correct.valid.HxGpS=length(which(predict(dag.fit,node="HxGpS",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$HxGpS[-na]))/length(discrete.data.with.lag.valid$HxGpS[-na])

dyn.mse.train.AxGpS=sum((as.numeric(as.character(predict(dag.fit,node="AxGpS",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$AxGpS)))^2)/length(discrete.data.with.lag$AxGpS)
na=which(is.na(predict(dag.fit,node="AxGpS",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.AxGpS=sum((as.numeric(as.character(predict(dag.fit,node="AxGpS",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$AxGpS[-na])))^2)/length(discrete.data.with.lag.valid$AxGpS[-na])
dyn.bin.correct.valid.AxGpS=length(which(predict(dag.fit,node="AxGpS",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$AxGpS[-na]))/length(discrete.data.with.lag.valid$AxGpS[-na])

dyn.mse.train.HS=sum((as.numeric(as.character(predict(dag.fit,node="HS",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HS)))^2)/length(discrete.data.with.lag$HS)
na=which(is.na(predict(dag.fit,node="HS",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.HS=sum((as.numeric(as.character(predict(dag.fit,node="HS",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$HS[-na])))^2)/length(discrete.data.with.lag.valid$HS[-na])
dyn.bin.correct.valid.HS=length(which(predict(dag.fit,node="HS",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$HS[-na]))/length(discrete.data.with.lag.valid$HS[-na])

dyn.mse.train.AS=sum((as.numeric(as.character(predict(dag.fit,node="AS",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$AS)))^2)/length(discrete.data.with.lag$AS)
na=which(is.na(predict(dag.fit,node="AS",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.AS=sum((as.numeric(as.character(predict(dag.fit,node="AS",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$AS[-na])))^2)/length(discrete.data.with.lag.valid$AS[-na])
dyn.bin.correct.valid.AS=length(which(predict(dag.fit,node="AS",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$AS[-na]))/length(discrete.data.with.lag.valid$AS[-na])

dyn.mse.train.HxG=sum((as.numeric(as.character(predict(dag.fit,node="HxG",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HxG)))^2)/length(discrete.data.with.lag$HxG)
na=which(is.na(predict(dag.fit,node="HxG",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.HxG=sum((as.numeric(as.character(predict(dag.fit,node="HxG",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$HxG[-na])))^2)/length(discrete.data.with.lag.valid$HxG[-na])
dyn.bin.correct.valid.HxG=length(which(predict(dag.fit,node="HxG",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$HxG[-na]))/length(discrete.data.with.lag.valid$HxG[-na])

dyn.mse.train.AxG=sum((as.numeric(as.character(predict(dag.fit,node="AxG",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$AxG)))^2)/length(discrete.data.with.lag$AxG)
na=which(is.na(predict(dag.fit,node="AxG",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.AxG=sum((as.numeric(as.character(predict(dag.fit,node="AxG",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$AxG[-na])))^2)/length(discrete.data.with.lag.valid$AxG[-na])
dyn.bin.correct.valid.AxG=length(which(predict(dag.fit,node="AxG",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$AxG[-na]))/length(discrete.data.with.lag.valid$AxG[-na])

dyn.mse.train.HPos=sum((as.numeric(as.character(predict(dag.fit,node="HPos",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HPos)))^2)/length(discrete.data.with.lag$HPos)
na=which(is.na(predict(dag.fit,node="HPos",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##empty
dyn.mse.valid.HPos=sum((as.numeric(as.character(predict(dag.fit,node="HPos",data=discrete.data.with.lag.valid[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag.valid$HPos)))^2)/length(discrete.data.with.lag.valid$HPos)
dyn.bin.correct.valid.HPos=length(which(predict(dag.fit,node="HPos",data=discrete.data.with.lag.valid[,-c(1,2)])==discrete.data.with.lag.valid$HPos))/length(discrete.data.with.lag.valid$HPos)

dyn.mse.train.HCorn=sum((as.numeric(as.character(predict(dag.fit,node="HCorn",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HCorn)))^2)/length(discrete.data.with.lag$HCorn)
na=which(is.na(predict(dag.fit,node="HCorn",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.HCorn=sum((as.numeric(as.character(predict(dag.fit,node="HCorn",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$HCorn[-na])))^2)/length(discrete.data.with.lag.valid$HCorn[-na])
dyn.bin.correct.valid.HCorn=length(which(predict(dag.fit,node="HCorn",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$HCorn[-na]))/length(discrete.data.with.lag.valid$HCorn[-na])

dyn.mse.train.ACorn=sum((as.numeric(as.character(predict(dag.fit,node="ACorn",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$ACorn)))^2)/length(discrete.data.with.lag$ACorn)
na=which(is.na(predict(dag.fit,node="ACorn",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.ACorn=sum((as.numeric(as.character(predict(dag.fit,node="ACorn",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$ACorn[-na])))^2)/length(discrete.data.with.lag.valid$ACorn[-na])
dyn.bin.correct.valid.ACorn=length(which(predict(dag.fit,node="ACorn",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$ACorn[-na]))/length(discrete.data.with.lag.valid$ACorn[-na])

dyn.mse.train.HCross=sum((as.numeric(as.character(predict(dag.fit,node="HCross",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HCross)))^2)/length(discrete.data.with.lag$HCross)
na=which(is.na(predict(dag.fit,node="HCross",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.HCross=sum((as.numeric(as.character(predict(dag.fit,node="HCross",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$HCross[-na])))^2)/length(discrete.data.with.lag.valid$HCross[-na])
dyn.bin.correct.valid.HCross=length(which(predict(dag.fit,node="HCross",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$HCross[-na]))/length(discrete.data.with.lag.valid$HCross[-na])

dyn.mse.train.ACross=sum((as.numeric(as.character(predict(dag.fit,node="ACross",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$ACross)))^2)/length(discrete.data.with.lag$ACross)
na=which(is.na(predict(dag.fit,node="ACross",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.ACross=sum((as.numeric(as.character(predict(dag.fit,node="ACross",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$ACross[-na])))^2)/length(discrete.data.with.lag.valid$ACross[-na])
dyn.bin.correct.valid.ACross=length(which(predict(dag.fit,node="ACross",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$ACross[-na]))/length(discrete.data.with.lag.valid$ACross[-na])

dyn.mse.train.HPP=sum((as.numeric(as.character(predict(dag.fit,node="HPP",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HPP)))^2)/length(discrete.data.with.lag$HPP)
na=which(is.na(predict(dag.fit,node="HPP",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.HPP=sum((as.numeric(as.character(predict(dag.fit,node="HPP",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$HPP[-na])))^2)/length(discrete.data.with.lag.valid$HPP[-na])
dyn.bin.correct.valid.HPP=length(which(predict(dag.fit,node="HPP",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$HPP[-na]))/length(discrete.data.with.lag.valid$HPP[-na])

dyn.mse.train.APP=sum((as.numeric(as.character(predict(dag.fit,node="APP",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$APP)))^2)/length(discrete.data.with.lag$APP)
na=which(is.na(predict(dag.fit,node="APP",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.APP=sum((as.numeric(as.character(predict(dag.fit,node="APP",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$APP[-na])))^2)/length(discrete.data.with.lag.valid$APP[-na])
dyn.bin.correct.valid.APP=length(which(predict(dag.fit,node="APP",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$APP[-na]))/length(discrete.data.with.lag.valid$APP[-na])

dyn.mse.train.HPosFT=sum((as.numeric(as.character(predict(dag.fit,node="HPosFT",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HPosFT)))^2)/length(discrete.data.with.lag$HPosFT)
na=which(is.na(predict(dag.fit,node="HPosFT",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.HPosFT=sum((as.numeric(as.character(predict(dag.fit,node="HPosFT",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$HPosFT[-na])))^2)/length(discrete.data.with.lag.valid$HPosFT[-na])
dyn.bin.correct.valid.HPosFT=length(which(predict(dag.fit,node="HPosFT",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$HPosFT[-na]))/length(discrete.data.with.lag.valid$HPosFT[-na])

dyn.mse.train.APosFT=sum((as.numeric(as.character(predict(dag.fit,node="APosFT",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$APosFT)))^2)/length(discrete.data.with.lag$APosFT)
na=which(is.na(predict(dag.fit,node="APosFT",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.APosFT=sum((as.numeric(as.character(predict(dag.fit,node="APosFT",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$APosFT[-na])))^2)/length(discrete.data.with.lag.valid$APosFT[-na])
dyn.bin.correct.valid.APosFT=length(which(predict(dag.fit,node="APosFT",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$APosFT[-na]))/length(discrete.data.with.lag.valid$APosFT[-na])

dyn.mse.train.HPC=sum((as.numeric(as.character(predict(dag.fit,node="HPC",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HPC)))^2)/length(discrete.data.with.lag$HPC)
na=which(is.na(predict(dag.fit,node="HPC",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.HPC=sum((as.numeric(as.character(predict(dag.fit,node="HPC",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$HPC[-na])))^2)/length(discrete.data.with.lag.valid$HPC[-na])
dyn.bin.correct.valid.HPC=length(which(predict(dag.fit,node="HPC",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$HPC[-na]))/length(discrete.data.with.lag.valid$HPC[-na])

dyn.mse.train.APC=sum((as.numeric(as.character(predict(dag.fit,node="APC",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$APC)))^2)/length(discrete.data.with.lag$APC)
na=which(is.na(predict(dag.fit,node="APC",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##some predictions cannot be calculated as the values of the parents was not seen in training data
dyn.mse.valid.APC=sum((as.numeric(as.character(predict(dag.fit,node="APC",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]))-as.numeric(as.character(discrete.data.with.lag.valid$APC[-na])))^2)/length(discrete.data.with.lag.valid$APC[-na])
dyn.bin.correct.valid.APC=length(which(predict(dag.fit,node="APC",data=discrete.data.with.lag.valid[,-c(1,2)])[-na]==discrete.data.with.lag.valid$APC[-na]))/length(discrete.data.with.lag.valid$APC[-na])

dyn.mse.train.HPA=sum((as.numeric(as.character(predict(dag.fit,node="HPA",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HPA)))^2)/length(discrete.data.with.lag$HPA)
na=which(is.na(predict(dag.fit,node="HPA",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##empty
dyn.mse.valid.HPA=sum((as.numeric(as.character(predict(dag.fit,node="HPA",data=discrete.data.with.lag.valid[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag.valid$HPA)))^2)/length(discrete.data.with.lag.valid$HPA)
dyn.bin.correct.valid.HPA=length(which(predict(dag.fit,node="HPA",data=discrete.data.with.lag.valid[,-c(1,2)])==discrete.data.with.lag.valid$HPA))/length(discrete.data.with.lag.valid$HPA)

dyn.mse.train.APA=sum((as.numeric(as.character(predict(dag.fit,node="APA",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$APA)))^2)/length(discrete.data.with.lag$APA)
na=which(is.na(predict(dag.fit,node="APA",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##empty
dyn.mse.valid.APA=sum((as.numeric(as.character(predict(dag.fit,node="APA",data=discrete.data.with.lag.valid[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag.valid$APA)))^2)/length(discrete.data.with.lag.valid$APA)
dyn.bin.correct.valid.APA=length(which(predict(dag.fit,node="APA",data=discrete.data.with.lag.valid[,-c(1,2)])==discrete.data.with.lag.valid$APA))/length(discrete.data.with.lag.valid$APA)

dyn.mse.train.HCP=sum((as.numeric(as.character(predict(dag.fit,node="HCP",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HCP)))^2)/length(discrete.data.with.lag$HCP)
na=which(is.na(predict(dag.fit,node="HCP",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##empty
dyn.mse.valid.HCP=sum((as.numeric(as.character(predict(dag.fit,node="HCP",data=discrete.data.with.lag.valid[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag.valid$HCP)))^2)/length(discrete.data.with.lag.valid$HCP)
dyn.bin.correct.valid.HCP=length(which(predict(dag.fit,node="HCP",data=discrete.data.with.lag.valid[,-c(1,2)])==discrete.data.with.lag.valid$HCP))/length(discrete.data.with.lag.valid$HCP)

dyn.mse.train.ACP=sum((as.numeric(as.character(predict(dag.fit,node="ACP",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$ACP)))^2)/length(discrete.data.with.lag$ACP)
na=which(is.na(predict(dag.fit,node="ACP",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##empty
dyn.mse.valid.ACP=sum((as.numeric(as.character(predict(dag.fit,node="ACP",data=discrete.data.with.lag.valid[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag.valid$ACP)))^2)/length(discrete.data.with.lag.valid$ACP)
dyn.bin.correct.valid.ACP=length(which(predict(dag.fit,node="ACP",data=discrete.data.with.lag.valid[,-c(1,2)])==discrete.data.with.lag.valid$ACP))/length(discrete.data.with.lag.valid$ACP)

dyn.mse.train.HPL=sum((as.numeric(as.character(predict(dag.fit,node="HPL",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$HPL)))^2)/length(discrete.data.with.lag$HPL)
na=which(is.na(predict(dag.fit,node="HPL",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##empty
dyn.mse.valid.HPL=sum((as.numeric(as.character(predict(dag.fit,node="HPL",data=discrete.data.with.lag.valid[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag.valid$HPL)))^2)/length(discrete.data.with.lag.valid$HPL)
dyn.bin.correct.valid.HCP=length(which(predict(dag.fit,node="HPL",data=discrete.data.with.lag.valid[,-c(1,2)])==discrete.data.with.lag.valid$HPL))/length(discrete.data.with.lag.valid$HPL)

dyn.mse.train.APL=sum((as.numeric(as.character(predict(dag.fit,node="APL",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$APL)))^2)/length(discrete.data.with.lag$APL)
na=which(is.na(predict(dag.fit,node="APL",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##empty
dyn.mse.valid.APL=sum((as.numeric(as.character(predict(dag.fit,node="APL",data=discrete.data.with.lag.valid[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag.valid$APL)))^2)/length(discrete.data.with.lag.valid$APL)
dyn.bin.correct.valid.APL=length(which(predict(dag.fit,node="APL",data=discrete.data.with.lag.valid[,-c(1,2)])==discrete.data.with.lag.valid$APL))/length(discrete.data.with.lag.valid$APL)

dyn.mse.train.T=sum((as.numeric(as.character(predict(dag.fit,node="T",data=discrete.data.with.lag[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag$T)))^2)/length(discrete.data.with.lag$T)
na=which(is.na(predict(dag.fit,node="T",data=discrete.data.with.lag.valid[,-c(1,2)])))  ##empty
dyn.mse.valid.T=sum((as.numeric(as.character(predict(dag.fit,node="T",data=discrete.data.with.lag.valid[,-c(1,2)])))-as.numeric(as.character(discrete.data.with.lag.valid$T)))^2)/length(discrete.data.with.lag.valid$T)
dyn.bin.correct.valid.T=length(which(predict(dag.fit,node="T",data=discrete.data.with.lag.valid[,-c(1,2)])==discrete.data.with.lag.valid$T))/length(discrete.data.with.lag.valid$T)

Variable=c("HxGpS","AxGpS","HS","AS","HCorn","ACorn","HPosFT","APosFT","HCross","ACross","HPP","APP","HPos","HPC","APC","HPA","APA","HPL","APL","HCP","ACP","T")
dyn.train.MSE=c(dyn.mse.train.HxGpS,dyn.mse.train.AxGpS,dyn.mse.train.HS,dyn.mse.train.AS,dyn.mse.train.HCorn,dyn.mse.train.ACorn,dyn.mse.train.HPosFT,dyn.mse.train.APosFT,dyn.mse.train.HCross,dyn.mse.train.ACross,dyn.mse.train.HPP,dyn.mse.train.APP,dyn.mse.train.HPos,dyn.mse.train.HPC,dyn.mse.train.APC,dyn.mse.train.HPA,dyn.mse.train.APA,dyn.mse.train.HPL,dyn.mse.train.APL,dyn.mse.train.HCP,dyn.mse.train.ACP,dyn.mse.train.T)
dyn.valid.MSE=c(dyn.mse.valid.HxGpS,dyn.mse.valid.AxGpS,dyn.mse.valid.HS,dyn.mse.valid.AS,dyn.mse.valid.HCorn,dyn.mse.valid.ACorn,dyn.mse.valid.HPosFT,dyn.mse.valid.APosFT,dyn.mse.valid.HCross,dyn.mse.valid.ACross,dyn.mse.valid.HPP,dyn.mse.valid.APP,dyn.mse.valid.HPos,dyn.mse.valid.HPC,dyn.mse.valid.APC,dyn.mse.valid.HPA,dyn.mse.valid.APA,dyn.mse.valid.HPL,dyn.mse.valid.APL,dyn.mse.valid.HCP,dyn.mse.valid.ACP,dyn.mse.valid.T)
dyn.train.MSE=round(dyn.train.MSE,digits=3)
dyn.valid.MSE=round(dyn.valid.MSE,digits=3)

dyn.MSE.results=as.data.frame(cbind(Variable,dyn.train.MSE,dyn.valid.MSE))
colnames(dyn.MSE.results)=c("Variable","Training MSE","Validation MSE")

output.file="dyn.MSE.tables.tex"
sink(output.file)
latex.tab=xtable(dyn.MSE.results)
print(latex.tab,include.rownames=FALSE,booktabs=TRUE)
sink()















