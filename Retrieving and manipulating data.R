
##### getting data

install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)
library(tidyverse)
laliga.06.07.onwards <- FreeCompetitions() %>%
  filter(competition_id==11) %>%
  filter((season_id!=38)&(season_id!=37)&(season_id!=278))
Matches <- FreeMatches(laliga.06.07.onwards)
StatsBombData <- free_allevents(MatchesDF=Matches,Parallel=T) 
laliga.06.07.onwards.data = allclean(StatsBombData) 
save(laliga.06.07.onwards.data, file="laligatrainingdata")

#####

##### all matches

Matches=FreeMatches(FreeCompetitions())
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="AtlÃ©tico Madrid"]="Atlético Madrid"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="AtlÃ©tico Madrid"]="Atlético Madrid"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="CÃ¡diz"]="Cádiz"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="CÃ¡diz"]="Cádiz"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="Deportivo AlavÃ©s"]="Deportivo Alavés"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="Deportivo AlavÃ©s"]="Deportivo Alavés"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="MÃ¡laga"]="Málaga"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="MÃ¡laga"]="Málaga"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="LeganÃ©s"]="Leganés"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="LeganÃ©s"]="Leganés"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="RC Deportivo La CoruÃ±a"]="RC Deportivo La Coruña"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="RC Deportivo La CoruÃ±a"]="RC Deportivo La Coruña"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="GimnÃ stic Tarragona"]="Gimnàstic Tarragona"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="GimnÃ stic Tarragona"]="Gimnàstic Tarragona"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="Sporting GijÃ³n"]="Sporting Gijón"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="Sporting GijÃ³n"]="Sporting Gijón"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="AlmerÃ­a"]="Almería"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="AlmerÃ­a"]="Almería"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="HÃ©rcules"]="Hércules"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="HÃ©rcules"]="Hércules"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="CÃ³rdoba CF"]="Córdoba CF"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="CÃ³rdoba CF"]="Córdoba CF"
Matches$home_team.home_team_name[Matches$home_team.home_team_name=="CÃ¡diz"]="Cádiz"
Matches$away_team.away_team_name[Matches$away_team.away_team_name=="CÃ¡diz"]="Cádiz"
Matches$home_team.home_team_name[Matches$match_id==68336]="Gimnàstic Tarragona"
save(Matches,file="Matches")
##### loading things

load("laligatrainingdata")
load("Matches")
library("tidyverse")

##### function to remove problems in the data

data.cleaner=function(dirty.data){
  times=separate(data=dirty.data,
                 col=timestamp,
                 into=c("hr","min.2","sec"),
                 sep=c(":"))
  time=(3600*as.numeric(times$hr))+(60*as.numeric(times$min.2))+as.numeric(times$sec)
  bad.points=vector(length=dim(dirty.data)[1])
  for(i in 2:dim(dirty.data)[1]){
  bad.points[i]=((dirty.data$period[i]==dirty.data$period[i-1])&(time[i]<time[i-1]))}
  cleaned.data=dirty.data[bad.points==FALSE,]
  return(cleaned.data)}

##### function to turn data into period summaries, general idea is to go through each event, determine what it is and whta period it is in

period.summary=function(dataset,period.length){
  cleaned.dataset=data.cleaner(dataset)
  
  cleaned.dataset$location=substr(cleaned.dataset$location,3,nchar(cleaned.dataset$location)-1)

  times=separate(data=cleaned.dataset,
                 col=timestamp,
                 into=c("hr","min.2","sec"),
                 sep=c(":"))
  time=(3600*as.numeric(times$hr))+(60*as.numeric(times$min.2))+as.numeric(times$sec)
  
  attach(cleaned.dataset)
  L=period.length 
  E=length(match_id)
  M=length(unique(match_id)) # m is number of matches
  N=M*(2+90/L) # total number of periods to be examined
  p=vector(length=E)  # don't really need to set this but helpful for checking
  ind=vector(length=E) # again don't need but helpful for checking
  half=period # setting half to avoid confusion
  
  Period=rep(1:(2+90/L),M)
  Match=rep(unique(match_id),times=rep(2+90/L,length(unique(match_id))))
  Home.Team=vector(length=N)
  Away.Team=vector(length=N)
  for(j in 1:N){
    Home.Team[j]=Matches$home_team.home_team_name[which(Match[j]==Matches$match_id)]
    Away.Team[j]=Matches$away_team.away_team_name[which(Match[j]==Matches$match_id)]
  }
  p.length=rep(0,N)
  time.in.play=rep(0,N)
  h.pos=rep(0,N)
  a.pos=rep(0,N)
  Home.passes.attempted=rep(0,N)
  Home.passes.completed=rep(0,N)
  h.pass.l=rep(0,N)
  a.pass.l=rep(0,N)
  Away.passes.attempted=rep(0,N)
  Away.passes.completed=rep(0,N)
  Home.crosses=rep(0,N)
  Away.crosses=rep(0,N)
  Home.corners=rep(0,N)
  Away.corners=rep(0,N)
  Home.shots=rep(0,N)
  Away.shots=rep(0,N)
  Home.goals=rep(0,N)
  Away.goals=rep(0,N)
  Home.xG=rep(0,N)
  Away.xG=rep(0,N)
  Home.offensive.duels.won=rep(0,N)
  Home.defensive.duels.won=rep(0,N)
  Away.offensive.duels.won=rep(0,N)
  Away.defensive.duels.won=rep(0,N)
  Home.aerial.duels.won=rep(0,N)
  Away.aerial.duels.won=rep(0,N)
  Home.fouls.committed=rep(0,N)
  Away.fouls.committed=rep(0,N)
  Home.final.third.actions=rep(0,N)
  Away.final.third.actions=rep(0,N)
  Home.possessions=rep(0,N)
  Away.possessions=rep(0,N)
  Home.interceptions=rep(0,N)
  Away.interceptions=rep(0,N)
  Home.pass.progression=rep(0,N)
  Away.pass.progression=rep(0,N)
  Home.carry.progression=rep(0,N)
  Away.carry.progression=rep(0,N)
  Home.carries=rep(0,N)
  Away.carries=rep(0,N)
  Home.dribbles.past.opposition.player=rep(0,N)
  Away.dribbles.past.opposition.player=rep(0,N)
  Home.fifty.fifty.wins=rep(0,N)
  Away.fifty.fifty.wins=rep(0,N)
  Home.red.cards=rep(0,N)
  Away.red.cards=rep(0,N)
  Home.clearances=rep(0,N)
  Away.clearances=rep(0,N)
  Home.possession.gained.in.final.third=rep(0,N)
  Away.possession.gained.in.final.third=rep(0,N)
  Home.time.with.ball.in.final.third=rep(0,N)
  Away.time.with.ball.in.final.third=rep(0,N)
  Home.pass.proportion.progressive=rep(0,N)
  Away.pass.proportion.progressive=rep(0,N)
  Home.carry.proportion.progressive=rep(0,N)
  Away.carry.proportion.progressive=rep(0,N)
  Home.carry.distance=rep(0,N)
  Away.carry.distance=rep(0,N)
  Home.final.third.shots=rep(0,N)
  Away.final.third.shots=rep(0,N)
  Home.passes.with.possession=rep(0,N)
  Away.passes.with.possession=rep(0,N)
  
  for(i in 1:E){
    t=1
    if(half[i]==1){    # working out what the period should be
      p[i]=1
      min=as.numeric(minute[i])
    }
    else{
      p[i]=((90/L)/2)+2
      min=as.numeric(minute[i])-45}
    while((min>=t*L)&(p[i]!=1+(90/L)/2)&(p[i]!=2+90/L)){
      t=t+1
      p[i]=p[i]+1
    }
    ind[i]=which((Match==match_id[i])&(Period==p[i]))
    
    if(i>1){
    if(half[i-1]==1){
      if(ind[i-1]==ind[i]){
        if((!is.na(out[i-1]))|(type.name[i-1]=="Foul Committed")|(type.name[i-1]=="Injury Stoppage")
           |((play_pattern.name[i]=="From Throw In")|(play_pattern.name[i]=="From Free Kick")
             |(play_pattern.name[i]=="From Goal Kick")|(play_pattern.name[i]=="From Corner")
             |(play_pattern.name[i]=="From Kick Off"))&(play_pattern.name[i]!=play_pattern.name[i-1])){
          p.length[ind[i-1]]=p.length[ind[i-1]]+time[i]-time[i-1] 
        } else{
        p.length[ind[i-1]]=p.length[ind[i-1]]+time[i]-time[i-1]
        time.in.play[ind[i-1]]=time.in.play[ind[i-1]]+time[i]-time[i-1]
        if(Home.Team[ind[i-1]]==possession_team.name[i-1]){
          h.pos[ind[i-1]]=h.pos[ind[i-1]]+time[i]-time[i-1]
          if(!is.na(location.x[i-1])){
          if(location.x[i-1]>80){Home.time.with.ball.in.final.third[ind[i-1]]=Home.time.with.ball.in.final.third[ind[i-1]]+time[i]-time[i-1]}}
          }
        if(Away.Team[ind[i-1]]==possession_team.name[i-1]){
          a.pos[ind[i-1]]=a.pos[ind[i-1]]+time[i]-time[i-1]
          if(!is.na(location.x[i-1])){
          if(location.x[i-1]>80){Away.time.with.ball.in.final.third[ind[i-1]]=Away.time.with.ball.in.final.third[ind[i-1]]+time[i]-time[i-1]}}
          }}
      }else{
        if(half[i-1]==half[i]){ # don't need anything for added time
          if((!is.na(out[i-1]))|(type.name[i-1]=="Foul Committed")|(type.name[i-1]=="Injury Stoppage")
             |((play_pattern.name[i]=="From Throw In")|(play_pattern.name[i]=="From Free Kick")
               |(play_pattern.name[i]=="From Goal Kick")|(play_pattern.name[i]=="From Corner")
               |(play_pattern.name[i]=="From Kick Off"))&(play_pattern.name[i]!=play_pattern.name[i-1])){
            p.length[ind[i-1]]=p.length[ind[i-1]]+(p[i-1]*L*60)-time[i-1]
            p.length[ind[i]]=p.length[ind[i]]+time[i]-((p[i]-1)*L*60) 
          } else{
          p.length[ind[i-1]]=p.length[ind[i-1]]+(p[i-1]*L*60)-time[i-1]
          p.length[ind[i]]=p.length[ind[i]]+time[i]-((p[i]-1)*L*60)
          time.in.play[ind[i-1]]=time.in.play[ind[i-1]]+(p[i-1]*L*60)-time[i-1]
          time.in.play[ind[i]]=time.in.play[ind[i]]+time[i]-((p[i]-1)*L*60)
          if(Home.Team[ind[i-1]]==possession_team.name[i-1]){
            h.pos[ind[i-1]]=h.pos[ind[i-1]]+(p[i-1]*L*60)-time[i-1]
            h.pos[ind[i]]=h.pos[ind[i]]+time[i]-((p[i]-1)*L*60)
            if(!is.na(location.x[i-1])){
            if(location.x[i-1]>80){
              Home.time.with.ball.in.final.third[ind[i-1]]=Home.time.with.ball.in.final.third[ind[i-1]]+(p[i-1]*L*60)-time[i-1]
              Home.time.with.ball.in.final.third[ind[i]]=Home.time.with.ball.in.final.third[ind[i]]+time[i]-((p[i]-1)*L*60)}}
            }
          if(Away.Team[ind[i-1]]==possession_team.name[i-1]){
            a.pos[ind[i-1]]=a.pos[ind[i-1]]+(p[i-1]*L*60)-time[i-1]
            a.pos[ind[i]]=a.pos[ind[i]]+time[i]-((p[i]-1)*L*60)
          if(!is.na(location.x[i-1])){
          if(location.x[i-1]>80){
            Away.time.with.ball.in.final.third[ind[i-1]]=Away.time.with.ball.in.final.third[ind[i-1]]+(p[i-1]*L*60)-time[i-1]
            Away.time.with.ball.in.final.third[ind[i]]=Away.time.with.ball.in.final.third[ind[i]]+time[i]-((p[i]-1)*L*60)}}}
        }}else{
         p.length[ind[i]]=p.length[ind[i]]+time[i]-0   #time resets each half
         }}}
      else{
      if(ind[i-1]==ind[i]){
        if((!is.na(out[i-1]))|(type.name[i-1]=="Foul Committed")|(type.name[i-1]=="Injury Stoppage")
           |((play_pattern.name[i]=="From Throw In")|(play_pattern.name[i]=="From Free Kick")
             |(play_pattern.name[i]=="From Goal Kick")|(play_pattern.name[i]=="From Corner")
             |(play_pattern.name[i]=="From Kick Off"))&(play_pattern.name[i]!=play_pattern.name[i-1])){
          p.length[ind[i-1]]=p.length[ind[i-1]]+time[i]-time[i-1] 
        } else{
        p.length[ind[i-1]]=p.length[ind[i-1]]+time[i]-time[i-1]
        time.in.play[ind[i-1]]=time.in.play[ind[i-1]]+time[i]-time[i-1]
        if(Home.Team[ind[i-1]]==possession_team.name[i-1]){
          h.pos[ind[i-1]]=h.pos[ind[i-1]]+time[i]-time[i-1]
          if(!is.na(location.x[i-1])){
          if(location.x[i-1]>80){Home.time.with.ball.in.final.third[ind[i-1]]=Home.time.with.ball.in.final.third[ind[i-1]]+time[i]-time[i-1]}}
          }
        if(Away.Team[ind[i-1]]==possession_team.name[i-1]){
          a.pos[ind[i-1]]=a.pos[ind[i-1]]+time[i]-time[i-1]
          if(!is.na(location.x[i-1])){
          if(location.x[i-1]>80){Away.time.with.ball.in.final.third[ind[i-1]]=Away.time.with.ball.in.final.third[ind[i-1]]+time[i]-time[i-1]}}
          }}
      }else{
        if(half[i-1]==half[i]){            # don't need anything for added time 
          if((!is.na(out[i-1]))|(type.name[i-1]=="Foul Committed")|(type.name[i-1]=="Injury Stoppage")
             |((play_pattern.name[i]=="From Throw In")|(play_pattern.name[i]=="From Free Kick")
               |(play_pattern.name[i]=="From Goal Kick")|(play_pattern.name[i]=="From Corner")
               |(play_pattern.name[i]=="From Kick Off"))&(play_pattern.name[i]!=play_pattern.name[i-1])){
            p.length[ind[i-1]]=p.length[ind[i-1]]+((p[i-1]-(((90/L)/2)+1))*L*60)-time[i-1]  
            p.length[ind[i]]=p.length[ind[i]]+time[i]-((p[i]-1-(((90/L)/2)+1))*L*60)
          } else{
          p.length[ind[i-1]]=p.length[ind[i-1]]+((p[i-1]-(((90/L)/2)+1))*L*60)-time[i-1]  
          p.length[ind[i]]=p.length[ind[i]]+time[i]-((p[i]-1-(((90/L)/2)+1))*L*60)
          time.in.play[ind[i-1]]=time.in.play[ind[i-1]]+((p[i-1]-(((90/L)/2)+1))*L*60)-time[i-1]  
          time.in.play[ind[i]]=time.in.play[ind[i]]+time[i]-((p[i]-1-(((90/L)/2)+1))*L*60)
          if(Home.Team[ind[i-1]]==possession_team.name[i-1]){
            if(!is.na(location.x[i-1])){
            if(location.x[i-1]>80){
              Home.time.with.ball.in.final.third[ind[i-1]]=Home.time.with.ball.in.final.third[ind[i-1]]+((p[i-1]-(((90/L)/2)+1))*L*60)-time[i-1]
              Home.time.with.ball.in.final.third[ind[i]]=Home.time.with.ball.in.final.third[ind[i]]+time[i]-((p[i]-1-(((90/L)/2)+1))*L*60)}}
            h.pos[ind[i-1]]=h.pos[ind[i-1]]+((p[i-1]-(((90/L)/2)+1))*L*60)-time[i-1]
            h.pos[ind[i]]=h.pos[ind[i]]+time[i]-((p[i]-1-(((90/L)/2)+1))*L*60)}
          if(Away.Team[ind[i-1]]==possession_team.name[i-1]){
            if(!is.na(location.x[i-1])){
            if(location.x[i-1]>80){
              Away.time.with.ball.in.final.third[ind[i-1]]=Away.time.with.ball.in.final.third[ind[i-1]]+((p[i-1]-(((90/L)/2)+1))*L*60)-time[i-1]
              Away.time.with.ball.in.final.third[ind[i]]=Away.time.with.ball.in.final.third[ind[i]]+time[i]-((p[i]-1-(((90/L)/2)+1))*L*60)}}
            a.pos[ind[i-1]]=a.pos[ind[i-1]]+((p[i-1]-(((90/L)/2)+1))*L*60)-time[i-1]
            a.pos[ind[i]]=a.pos[ind[i]]+time[i]-((p[i]-1-(((90/L)/2)+1))*L*60)}}
        }else{
          p.length[ind[i]]=p.length[ind[i]]+time[i]-0   #time resets each half
          }}}}
    
     if(play_pattern.name[i]=="From Corner"){
       if(play_pattern.name[i]!=play_pattern.name[i-1]){
       if(Home.Team[ind[i]]==team.name[i]){
         Home.corners[ind[i]]=Home.corners[ind[i]]+1
       }else{
         Away.corners[ind[i]]=Away.corners[ind[i]]+1
       }}
     }    
    
     if(type.name[i]=="Pass"){
     if(Home.Team[ind[i]]==team.name[i]){
       h.pass.l[ind[i]]=h.pass.l[ind[i]]+pass.length[i]
       if(!is.na(pass.cross[i])){Home.crosses[ind[i]]=Home.crosses[ind[i]]+1}
       if(is.na(pass.outcome.name[i])){
         Home.passes.attempted[ind[i]]=Home.passes.attempted[ind[i]]+1
         Home.passes.completed[ind[i]]=Home.passes.completed[ind[i]]+1
         if(!((!is.na(out[i]))|(type.name[i]=="Foul Committed")|(type.name[i]=="Injury Stoppage")
            |((play_pattern.name[i+1]=="From Throw In")|(play_pattern.name[i+1]=="From Free Kick")
              |(play_pattern.name[i+1]=="From Goal Kick")|(play_pattern.name[i+1]=="From Corner")
              |(play_pattern.name[i+1]=="From Kick Off"))&(play_pattern.name[i+1]!=play_pattern.name[i]))){
         if((possession_team.name[i]==team.name[i])&(is.na(pass.aerial_won[i]))){
           Home.passes.with.possession[ind[i]]=Home.passes.with.possession[ind[i]]+1}}
         if(location.x[i]<=80){
           Home.pass.progression[ind[i]]=Home.pass.progression[ind[i]]+pass.end_location.x[i]-location.x[i]
           if((location.x[i]!=pass.end_location.x[i])|(location.y[i]!=pass.end_location.y[i])){
           Home.pass.proportion.progressive[ind[i]]=Home.pass.proportion.progressive[ind[i]]+(pass.end_location.x[i]-location.x[i])/pass.length[i]}
         }else{
           Home.pass.progression[ind[i]]=Home.pass.progression[ind[i]]-sqrt(((pass.end_location.x[i]-120)^2)+((pass.end_location.y[i]-40)^2))+sqrt(((location.x[i]-120)^2)+((location.y[i]-40)^2))
           if((location.x[i]!=pass.end_location.x[i])|(location.y[i]!=pass.end_location.y[i])){
           Home.pass.proportion.progressive[ind[i]]=Home.pass.proportion.progressive[ind[i]]+(-sqrt(((pass.end_location.x[i]-120)^2)+((pass.end_location.y[i]-40)^2))+sqrt(((location.x[i]-120)^2)+((location.y[i]-40)^2)))/pass.length[i]}
         }
       }else{
         if((pass.outcome.name[i]=="Incomplete")|(pass.outcome.name[i]=="Out")|(pass.outcome.name[i]=="Pass Offside")){
           Home.passes.attempted[ind[i]]=Home.passes.attempted[ind[i]]+1 
           if(!((!is.na(out[i]))|(type.name[i]=="Foul Committed")|(type.name[i]=="Injury Stoppage")
                |((play_pattern.name[i+1]=="From Throw In")|(play_pattern.name[i+1]=="From Free Kick")
                  |(play_pattern.name[i+1]=="From Goal Kick")|(play_pattern.name[i+1]=="From Corner")
                  |(play_pattern.name[i+1]=="From Kick Off"))&(play_pattern.name[i+1]!=play_pattern.name[i]))){
           if((possession_team.name[i]==team.name[i])&(is.na(pass.aerial_won[i]))){
           Home.passes.with.possession[ind[i]]=Home.passes.with.possession[ind[i]]+1}}
         }}}
      else{
       a.pass.l[ind[i]]=a.pass.l[ind[i]]+pass.length[i]
       if(!is.na(pass.cross[i])){Away.crosses[ind[i]]=Away.crosses[ind[i]]+1}
       if(is.na(pass.outcome.name[i])){
       Away.passes.attempted[ind[i]]=Away.passes.attempted[ind[i]]+1
       Away.passes.completed[ind[i]]=Away.passes.completed[ind[i]]+1
       if(!((!is.na(out[i]))|(type.name[i]=="Foul Committed")|(type.name[i]=="Injury Stoppage")
            |((play_pattern.name[i+1]=="From Throw In")|(play_pattern.name[i+1]=="From Free Kick")
              |(play_pattern.name[i+1]=="From Goal Kick")|(play_pattern.name[i+1]=="From Corner")
              |(play_pattern.name[i+1]=="From Kick Off"))&(play_pattern.name[i+1]!=play_pattern.name[i]))){
      if((possession_team.name[i]==team.name[i])&(is.na(pass.aerial_won[i]))){
      Away.passes.with.possession[ind[i]]=Away.passes.with.possession[ind[i]]+1}}
       if(location.x[i]<=80){
         Away.pass.progression[ind[i]]=Away.pass.progression[ind[i]]+pass.end_location.x[i]-location.x[i]
         if((location.x[i]!=pass.end_location.x[i])|(location.y[i]!=pass.end_location.y[i])){
         Away.pass.proportion.progressive[ind[i]]=Away.pass.proportion.progressive[ind[i]]+(pass.end_location.x[i]-location.x[i])/pass.length[i]}
       }else{
         Away.pass.progression[ind[i]]=Away.pass.progression[ind[i]]-sqrt(((pass.end_location.x[i]-120)^2)+((pass.end_location.y[i]-40)^2))+sqrt(((location.x[i]-120)^2)+((location.y[i]-40)^2))
         if((location.x[i]!=pass.end_location.x[i])|(location.y[i]!=pass.end_location.y[i])){
         Away.pass.proportion.progressive[ind[i]]=Away.pass.proportion.progressive[ind[i]]+(-sqrt(((pass.end_location.x[i]-120)^2)+((pass.end_location.y[i]-40)^2))+sqrt(((location.x[i]-120)^2)+((location.y[i]-40)^2)))/pass.length[i]}
       }
     }else{
       if((pass.outcome.name[i]=="Incomplete")|(pass.outcome.name[i]=="Out")|(pass.outcome.name[i]=="Pass Offside")){
         Away.passes.attempted[ind[i]]=Away.passes.attempted[ind[i]]+1 
         if(!((!is.na(out[i]))|(type.name[i]=="Foul Committed")|(type.name[i]=="Injury Stoppage")
              |((play_pattern.name[i+1]=="From Throw In")|(play_pattern.name[i+1]=="From Free Kick")
                |(play_pattern.name[i+1]=="From Goal Kick")|(play_pattern.name[i+1]=="From Corner")
                |(play_pattern.name[i+1]=="From Kick Off"))&(play_pattern.name[i+1]!=play_pattern.name[i]))){
        if((possession_team.name[i]==team.name[i])&(is.na(pass.aerial_won[i]))){
         Away.passes.with.possession[ind[i]]=Away.passes.with.possession[ind[i]]+1}}
       }}}}
    
    if(type.name[i]=="Shot"){
      if(Home.Team[ind[i]]==team.name[i]){
        Home.shots[ind[i]]=Home.shots[ind[i]]+1
        Home.xG[ind[i]]=Home.xG[ind[i]]+shot.statsbomb_xg[i]
        if(location.x[i]>80){
          Home.final.third.shots[ind[i]]=Home.final.third.shots[ind[i]]+1
        }
      }
      else{Away.shots[ind[i]]=Away.shots[ind[i]]+1
      Away.xG[ind[i]]=Away.xG[ind[i]]+shot.statsbomb_xg[i]
      if(location.x[i]>80){
        Away.final.third.shots[ind[i]]=Away.final.third.shots[ind[i]]+1
      }}
      
      if(shot.outcome.name[i]=="Goal"){
      if(Home.Team[ind[i]]==team.name[i]){
        Home.goals[ind[i]]=Home.goals[ind[i]]+1
      }
      else{Away.goals[ind[i]]=Away.goals[ind[i]]+1}
      }}
    
    if(type.name[i]=="Duel"){
      if(Home.Team[ind[i]]==team.name[i]){
        if(duel.type.name[i]=="Aerial Lost"){
        Away.aerial.duels.won[ind[i]]=Away.aerial.duels.won[ind[i]]+1}else{
          if((duel.outcome.name[i]=="Lost Out")|(duel.outcome.name[i]=="Lost")|(duel.outcome.name[i]=="Lost In Play")){
            Away.offensive.duels.won[ind[i]]=Away.offensive.duels.won[ind[i]]+1
          }else{
            Home.defensive.duels.won[ind[i]]=Home.defensive.duels.won[ind[i]]+1
          }}}
      else{if(duel.type.name[i]=="Aerial Lost"){
        Home.aerial.duels.won[ind[i]]=Home.aerial.duels.won[ind[i]]+1}else{
          if((duel.outcome.name[i]=="Lost Out")|(duel.outcome.name[i]=="Lost")|(duel.outcome.name[i]=="Lost In Play")){
            Home.offensive.duels.won[ind[i]]=Home.offensive.duels.won[ind[i]]+1
          }else{
            Away.defensive.duels.won[ind[i]]=Away.defensive.duels.won[ind[i]]+1
          }}}}
    
    if(type.name[i]=="Foul Committed"){
      if(Home.Team[ind[i]]==team.name[i]){
        Home.fouls.committed[ind[i]]=Home.fouls.committed[ind[i]]+1
      }
      else{Away.fouls.committed[ind[i]]=Away.fouls.committed[ind[i]]+1}
    }
    
    if(!is.na(location.x[i])){
      if((location.x[i]>80)&((type.name[i]=="Shot")|(type.name[i]=="Pass")|(type.name[i]=="Carry")|(type.name[i]=="Dribble"))){   #coordinates are for team of the action
      if(team.name[i]==Home.Team[ind[i]]){
      Home.final.third.actions[ind[i]]=Home.final.third.actions[ind[i]]+1
      }else{
       Away.final.third.actions[ind[i]]=Away.final.third.actions[ind[i]]+1
      }}}
    
    if(i!=1){
    if(possession_team.name[i]!=possession_team.name[i-1]){
      if(possession_team.name[i]==Home.Team[ind[i]]){
      Home.possessions[ind[i]]=Home.possessions[ind[i]]+1
      }else{Away.possessions[ind[i]]=Away.possessions[ind[i]]+1}}
    
    if(ind[i]!=ind[i-1]){
      if(possession_team.name[i]==Home.Team[ind[i]]){
        Home.possessions[ind[i]]=Home.possessions[ind[i]]+1
        if(!is.na(location.x[i])){
        if(location.x[i]>80){Home.possession.gained.in.final.third[ind[i]]=Home.possession.gained.in.final.third[ind[i]]+1}}
      }else{Away.possessions[ind[i]]=Away.possessions[ind[i]]+1
      if(!is.na(location.x[i])){
      if(location.x[i]>80){Away.possession.gained.in.final.third[ind[i]]=Away.possession.gained.in.final.third[ind[i]]+1}}}}}
    
    if(type.name[i]=="Interception"){
      if(Home.Team[ind[i]]==team.name[i]){
        Home.interceptions[ind[i]]=Home.interceptions[ind[i]]+1
      }
      else{Away.interceptions[ind[i]]=Away.interceptions[ind[i]]+1}
    }
    
    if(type.name[i]=="Carry"){
      if(Home.Team[ind[i]]==team.name[i]){
        Home.carries[ind[i]]=Home.carries[ind[i]]+1
        Home.carry.distance[ind[i]]=Home.carry.distance[ind[i]]+sqrt(((carry.end_location.x[i]-location.x[i])^2)+((carry.end_location.y[i]-location.y[i])^2))
        if(location.x[i]<=80){
        Home.carry.progression[ind[i]]=Home.carry.progression[ind[i]]+carry.end_location.x[i]-location.x[i]
        if((location.x[i]!=carry.end_location.x[i])|(location.y[i]!=carry.end_location.y[i])){
        Home.carry.proportion.progressive[ind[i]]=Home.carry.proportion.progressive[ind[i]]+(carry.end_location.x[i]-location.x[i])/sqrt(((carry.end_location.x[i]-location.x[i])^2)+((carry.end_location.y[i]-location.y[i])^2))}}else{
        Home.carry.progression[ind[i]]=Home.carry.progression[ind[i]]-sqrt(((carry.end_location.x[i]-120)^2)+((carry.end_location.y[i]-40)^2))+sqrt(((location.x[i]-120)^2)+((location.y[i]-40)^2))
        if((location.x[i]!=carry.end_location.x[i])|(location.y[i]!=carry.end_location.y[i])){
        Home.carry.proportion.progressive[ind[i]]=Home.carry.proportion.progressive[ind[i]]+(-sqrt(((carry.end_location.x[i]-120)^2)+((carry.end_location.y[i]-40)^2))+sqrt(((location.x[i]-120)^2)+((location.y[i]-40)^2)))/sqrt(((carry.end_location.x[i]-location.x[i])^2)+((carry.end_location.y[i]-location.y[i])^2))}
        }}else{
        Away.carries[ind[i]]=Away.carries[ind[i]]+1
        Away.carry.distance[ind[i]]=Away.carry.distance[ind[i]]+sqrt(((carry.end_location.x[i]-location.x[i])^2)+((carry.end_location.y[i]-location.y[i])^2))
        if(location.x[i]<=80){
          Away.carry.progression[ind[i]]=Away.carry.progression[ind[i]]+carry.end_location.x[i]-location.x[i]
          if((location.x[i]!=carry.end_location.x[i])|(location.y[i]!=carry.end_location.y[i])){
          Away.carry.proportion.progressive[ind[i]]=Away.carry.proportion.progressive[ind[i]]+(carry.end_location.x[i]-location.x[i])/sqrt(((carry.end_location.x[i]-location.x[i])^2)+((carry.end_location.y[i]-location.y[i])^2))}}else{
          Away.carry.progression[ind[i]]=Away.carry.progression[ind[i]]-sqrt(((carry.end_location.x[i]-120)^2)+((carry.end_location.y[i]-40)^2))+sqrt(((location.x[i]-120)^2)+((location.y[i]-40)^2))
          if((location.x[i]!=carry.end_location.x[i])|(location.y[i]!=carry.end_location.y[i])){
          Away.carry.proportion.progressive[ind[i]]=Away.carry.proportion.progressive[ind[i]]+(-sqrt(((carry.end_location.x[i]-120)^2)+((carry.end_location.y[i]-40)^2))+sqrt(((location.x[i]-120)^2)+((location.y[i]-40)^2)))/sqrt(((carry.end_location.x[i]-location.x[i])^2)+((carry.end_location.y[i]-location.y[i])^2))}
    }}}
    
    if(type.name[i]=="Dribbled Past"){
      if(Home.Team[ind[i]]==team.name[i]){
        Away.dribbles.past.opposition.player[ind[i]]=Away.dribbles.past.opposition.player[ind[i]]+1
      }else{
        Home.dribbles.past.opposition.player[ind[i]]=Home.dribbles.past.opposition.player[ind[i]]+1
      }}
    
    if((type.name[i]=="50/50")&(team.name[i]==possession_team.name[i])){
      if(team.name[i]==Home.Team[ind[i]]){
      if((`50_50.outcome.name`[i]=="Won")|(`50_50.outcome.name`[i]=="Success To Team")){
        Home.fifty.fifty.wins[ind[i]]=Home.fifty.fifty.wins[ind[i]]+1}else{
        Away.fifty.fifty.wins[ind[i]]=Away.fifty.fifty.wins[ind[i]]+1
        }}else{
      if((`50_50.outcome.name`[i]=="Won")|(`50_50.outcome.name`[i]=="Success To Team")){
        Away.fifty.fifty.wins[ind[i]]=Away.fifty.fifty.wins[ind[i]]+1}else{
        Home.fifty.fifty.wins[ind[i]]=Home.fifty.fifty.wins[ind[i]]+1
        }}}
    
    if(!is.na(foul_committed.card.name[i])){
      if((foul_committed.card.name[i]=="Red Card")|(foul_committed.card.name[i]=="Second Yellow")){
        if(Home.Team[ind[i]]==team.name[i]){
          Home.red.cards[ind[i]]=Home.red.cards[ind[i]]+1
        }else{
          Away.red.cards[ind[i]]=Away.red.cards[ind[i]]+1
        }}}
    
    if(!is.na(bad_behaviour.card.name[i])){
      if((bad_behaviour.card.name[i]=="Red Card")|(bad_behaviour.card.name[i]=="Second Yellow")){
        if(Home.Team[ind[i]]==team.name[i]){
          Home.red.cards[ind[i]]=Home.red.cards[ind[i]]+1
        }else{
          Away.red.cards[ind[i]]=Away.red.cards[ind[i]]+1
        }}}
    
    if(type.name[i]=="Clearance"){
      if(Home.Team[ind[i]]==team.name[i]){
        Home.clearances[ind[i]]=Home.clearances[ind[i]]+1
        if(!is.na(clearance.aerial_won[i])){
          Home.aerial.duels.won[ind[i]]=Home.aerial.duels.won[ind[i]]+1}
      }
      else{Away.clearances[ind[i]]=Away.clearances[ind[i]]+1
        if(!is.na(clearance.aerial_won[i])){
          Away.aerial.duels.won[ind[i]]=Away.aerial.duels.won[ind[i]]+1}}
    }
    
    if(type.name[i]=="Own Goal For"){
      if(Home.Team[ind[i]]==team.name[i]){
        Home.goals[ind[i]]=Home.goals[ind[i]]+1
      }
      else{Away.goals[ind[i]]=Away.goals[ind[i]]+1}
    }}
  
  Home.current.goals.in.match=vector(length=N)
  Away.current.goals.in.match=vector(length=N)
  Home.current.red.cards.in.match=vector(length=N)
  Away.current.red.cards.in.match=vector(length=N)
  for(j in 1:N){
    Home.current.goals.in.match[j]=sum(Home.goals[which((Match==Match[j])&(Period<Period[j]))])
    Away.current.goals.in.match[j]=sum(Away.goals[which((Match==Match[j])&(Period<Period[j]))])
    Home.current.red.cards.in.match[j]=sum(Home.red.cards[which((Match==Match[j])&(Period<Period[j]))])
    Away.current.red.cards.in.match[j]=sum(Away.red.cards[which((Match==Match[j])&(Period<Period[j]))])
  }
  
  Home.average.possession.length=h.pos/Home.possessions
  Away.average.possession.length=a.pos/Away.possessions
  Home.possession=100*h.pos/time.in.play
  Away.possession=100*a.pos/time.in.play
  Home.pass.completion.rate=100*Home.passes.completed/Home.passes.attempted
  Away.pass.completion.rate=100*Away.passes.completed/Away.passes.attempted
  Home.avg.pass.length=h.pass.l/Home.passes.attempted
  Away.avg.pass.length=a.pass.l/Away.passes.attempted
  Home.offensive.duel.win.rate=100*Home.offensive.duels.won/(Home.offensive.duels.won+Away.defensive.duels.won)
  Away.offensive.duel.win.rate=100*Away.offensive.duels.won/(Away.offensive.duels.won+Home.defensive.duels.won)
  Home.defensive.duel.win.rate=100-Away.offensive.duel.win.rate
  Away.defensive.duel.win.rate=100-Home.offensive.duel.win.rate
  Home.aerial.duel.win.rate=100*Home.aerial.duels.won/(Home.aerial.duels.won+Away.aerial.duels.won)
  Away.aerial.duel.win.rate=100-Home.aerial.duel.win.rate
  Home.shots.per.final.third.action=Home.final.third.shots/Home.final.third.actions
  Away.shots.per.final.third.action=Away.final.third.shots/Away.final.third.actions
  Home.fifty.fifty.win.rate=100*Home.fifty.fifty.wins/(Home.fifty.fifty.wins+Away.fifty.fifty.wins)
  Away.fifty.fifty.win.rate=100*Away.fifty.fifty.wins/(Home.fifty.fifty.wins+Away.fifty.fifty.wins)
  Home.average.xG.per.shot=Home.xG/Home.shots
  Away.average.xG.per.shot=Away.xG/Away.shots
  Home.passing.tempo=Home.passes.with.possession/h.pos
  Away.passing.tempo=Away.passes.with.possession/a.pos
  Home.proportion.of.possession.spent.in.final.third=100*Home.time.with.ball.in.final.third/h.pos
  Away.proportion.of.possession.spent.in.final.third=100*Away.time.with.ball.in.final.third/a.pos
  Home.average.carry.length=Home.carry.distance/Home.carries
  Away.average.carry.length=Away.carry.distance/Away.carries
  Home.progression.per.completed.pass=Home.pass.progression/Home.passes.completed
  Away.progression.per.completed.pass=Away.pass.progression/Away.passes.completed
  Home.average.proportion.progressive.per.completed.pass=Home.pass.proportion.progressive/Home.passes.completed
  Away.average.proportion.progressive.per.completed.pass=Away.pass.proportion.progressive/Away.passes.completed
  Home.progression.per.carry=Home.carry.progression/Home.carries
  Away.progression.per.carry=Away.carry.progression/Away.carries
  Home.average.proportion.progressive.per.carry=Home.carry.proportion.progressive/Home.carries
  Away.average.proportion.progressive.per.carry=Away.carry.proportion.progressive/Away.carries
  
  Home.average.possession.length[is.na(Home.average.possession.length)]=0
  Away.average.possession.length[is.na(Away.average.possession.length)]=0
  Home.pass.completion.rate[is.na(Home.pass.completion.rate)]=0
  Away.pass.completion.rate[is.na(Away.pass.completion.rate)]=0
  Home.possession[is.na(Home.possession)]=0
  Away.possession[is.na(Away.possession)]=0
  Home.avg.pass.length[is.na(Home.avg.pass.length)]=0
  Away.avg.pass.length[is.na(Away.avg.pass.length)]=0
  Home.offensive.duel.win.rate[is.na(Home.offensive.duel.win.rate)]=50
  Away.offensive.duel.win.rate[is.na(Away.offensive.duel.win.rate)]=50
  Home.defensive.duel.win.rate[is.na(Home.defensive.duel.win.rate)]=50
  Away.defensive.duel.win.rate[is.na(Away.defensive.duel.win.rate)]=50
  Home.aerial.duel.win.rate[is.na(Home.aerial.duel.win.rate)]=50
  Away.aerial.duel.win.rate[is.na(Away.aerial.duel.win.rate)]=50
  Home.shots.per.final.third.action[is.na(Home.shots.per.final.third.action)]=0
  Away.shots.per.final.third.action[is.na(Away.shots.per.final.third.action)]=0
  Home.fifty.fifty.win.rate[is.na(Home.fifty.fifty.win.rate)]=50
  Away.fifty.fifty.win.rate[is.na(Away.fifty.fifty.win.rate)]=50
  Home.average.xG.per.shot[is.na(Home.average.xG.per.shot)]=0
  Away.average.xG.per.shot[is.na(Away.average.xG.per.shot)]=0
  Home.proportion.of.possession.spent.in.final.third[is.na(Home.proportion.of.possession.spent.in.final.third)]=0
  Away.proportion.of.possession.spent.in.final.third[is.na(Away.proportion.of.possession.spent.in.final.third)]=0
  Home.average.carry.length[is.na(Home.average.carry.length)]=0
  Away.average.carry.length[is.na(Away.average.carry.length)]=0
  Home.progression.per.completed.pass[is.na(Home.progression.per.completed.pass)]=0
  Away.progression.per.completed.pass[is.na(Away.progression.per.completed.pass)]=0
  Home.average.proportion.progressive.per.completed.pass[is.na(Home.average.proportion.progressive.per.completed.pass)]=0
  Away.average.proportion.progressive.per.completed.pass[is.na(Away.average.proportion.progressive.per.completed.pass)]=0
  Home.progression.per.carry[is.na(Home.progression.per.carry)]=0
  Away.progression.per.carry[is.na(Away.progression.per.carry)]=0
  Home.average.proportion.progressive.per.carry[is.na(Home.average.proportion.progressive.per.carry)]=0
  Away.average.proportion.progressive.per.carry[is.na(Away.average.proportion.progressive.per.carry)]=0
  Home.passing.tempo[is.na(Home.passing.tempo)]=0
  Away.passing.tempo[is.na(Away.passing.tempo)]=0
  
  tab=cbind(Period,Match,Home.Team,Away.Team,
            p.length,time.in.play,
            h.pos,a.pos,
            Home.possession,Away.possession,
            Home.passes.attempted,Home.passes.completed,
            Home.pass.completion.rate,Home.avg.pass.length,
            Away.passes.attempted,Away.passes.completed,
            Away.pass.completion.rate,Away.avg.pass.length,
            Home.crosses,Away.crosses,
            Home.corners,Away.corners,
            Home.shots,Away.shots,
            Home.xG,Away.xG,
            Home.goals,Away.goals,
            Home.offensive.duels.won,Home.offensive.duel.win.rate,
            Away.defensive.duels.won,Away.defensive.duel.win.rate,
            Home.defensive.duels.won,Home.defensive.duel.win.rate,
            Away.offensive.duels.won,Away.offensive.duel.win.rate,
            Home.aerial.duels.won,Away.aerial.duels.won,
            Home.aerial.duel.win.rate,Away.aerial.duel.win.rate,
            Home.fouls.committed,Away.fouls.committed,
            Home.final.third.actions,Away.final.third.actions,
            Home.current.goals.in.match,Away.current.goals.in.match,
            Home.possessions,Away.possessions,
            Home.average.possession.length,Away.average.possession.length,
            Home.interceptions,Away.interceptions,
            Home.shots.per.final.third.action,Away.shots.per.final.third.action,
            Home.pass.progression,Away.pass.progression,
            Home.carries,Away.carries,
            Home.carry.progression,Away.carry.progression,
            Home.dribbles.past.opposition.player,Away.dribbles.past.opposition.player,
            Home.fifty.fifty.wins,Away.fifty.fifty.wins,
            Home.fifty.fifty.win.rate,Away.fifty.fifty.win.rate,
            Home.average.xG.per.shot,Away.average.xG.per.shot,
            Home.current.red.cards.in.match,Away.current.red.cards.in.match,
            Home.clearances,Away.clearances,
            Home.passing.tempo,Away.passing.tempo,
            Home.possession.gained.in.final.third,Away.possession.gained.in.final.third,
            Home.proportion.of.possession.spent.in.final.third,Away.proportion.of.possession.spent.in.final.third,
            Home.average.carry.length,Away.average.carry.length,
            Home.average.proportion.progressive.per.completed.pass,Away.average.proportion.progressive.per.completed.pass,
            Home.progression.per.completed.pass,Away.progression.per.completed.pass,
            Home.progression.per.carry,Away.progression.per.carry,
            Home.average.proportion.progressive.per.carry,Away.average.proportion.progressive.per.carry)
  
  detach(cleaned.dataset)
  return(tab)
}

##### test

ex.data=laliga.06.07.onwards.data[1:10001,]

eg=period.summary(ex.data,period.length=5)

##### usage on la liga data, will take approx. 30 mins

laliga.training.data.altered=period.summary(laliga.06.07.onwards.data,period.length=5)
laliga.training.data.altered=as.data.frame(laliga.training.data.altered)
laliga.training.data.all.5.mins=laliga.training.data.altered[((laliga.training.data.altered$p.length>299)&(laliga.training.data.altered$p.length<301)&(laliga.training.data.altered$Period!=10)&(laliga.training.data.altered$Period!=20)),]
laliga.training.data.all.periods=as.data.frame(cbind(laliga.training.data.altered[,c(1,2,3,4)],sapply(laliga.training.data.altered[,-c(1,2,3,4)],as.numeric)))
laliga.training.data.all.5.mins=as.data.frame(cbind(laliga.training.data.all.5.mins[,c(1,2,3,4)],sapply(laliga.training.data.all.5.mins[,-c(1,2,3,4)],as.numeric)))
save(laliga.training.data.all.periods,file="laligatraining. data all periods")
save(laliga.training.data.all.5.mins,file="laligatraining. data all 5 mins")




