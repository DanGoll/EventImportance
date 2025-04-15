################################## Match Importance Project #########################################
#
# Author:
# Sandro Heiniger
# Swiss Institute for Empirical Economic Research
# University St. Gallen
# sandro.heiniger@unisg.ch
#
# Daniel Goller
# Swiss Institute for Empirical Economic Research
# University St. Gallen
# daniel.goller@unisg.ch

######  Version: 21.06.2022


# Environment Settings ----------------------------------------------------


# General Choices
# Set working directory. Make sure that data folder is inside working directory
setwd("your_path")              


# Load packages 
library(install.load)
install_load("dplyr","tidyverse","ggplot2","viridis","scales","doSNOW","finalfit")


# Analysis setup ----------------------------------------------------------

# read the state info
state_info=read.csv("../data/front_loading.csv",header = T, stringsAsFactors = F)
colnames(state_info)=c("State","Delegates","Date")
state_info$Date=as.Date(state_info$Date,"%d.%m.%Y")
unique_dates=sort(unique(state_info$Date))

# Change Dates if hypothetical date for a state is estimated
# 
# # Rank 4
# state_info[state_info$State=="Iowa","Date"]=as.Date("2020-03-01","%Y-%m-%d")
# 
# # super tuesday
# state_info[state_info$State=="Iowa","Date"]=as.Date("2020-03-03","%Y-%m-%d")
# state_info[state_info$State=="Louisiana","Date"]=as.Date("2020-02-03","%Y-%m-%d")
# 
# # rank 20
# state_info[state_info$State=="Iowa","Date"]=as.Date("2020-03-07","%Y-%m-%d")
# state_info[state_info$State=="Louisiana","Date"]=as.Date("2020-02-03","%Y-%m-%d")
# 
# # Rank 50
# state_info[state_info$State=="Iowa","Date"]=as.Date("2020-06-13","%Y-%m-%d")
# 
# unique_dates=sort(unique(state_info$Date))


# generate candidates
N_cand=2
candidate_characteristics=c(-1,1)
candidate_FE=c(0,0.5)

# Parameters for score
alpha=-1/2
beta=1

# Parameters for estimation
N_clusters=8
N_outer=100
N_inner=5000
Max_capacity=1000000

# Type of simulation
 sim_type="regular"
# sim_type="random"
#sim_type="rank_increase"

# CHange dates if rank increase option is selected
if(sim_type=="rank_increase"){
  state_info$Date=sort(state_info$Date)[rank(state_info$Delegates, ties.method = "random")]
}
if(sim_type!="random"){
  mult_factor=max(N_outer*N_inner/Max_capacity,1)
  # replicate the data 
  state_info_mult=state_info %>%
    mutate(count =round(N_outer/N_clusters/mult_factor)) %>%
    uncount(count,.id = "outer_path")

  Sim_end_index=round(N_clusters*mult_factor)

} else {
  Sim_end_index=N_outer
}


# Start simulation
Sys.time()
start_time=Sys.time()
clusters <- parallel::makeCluster(N_clusters)
registerDoSNOW(clusters)

# parallel loop over all clusters ---------------------------------------------------
EI_values_regular=foreach(current_cluster = 1:Sim_end_index, 
                       .combine = "rbind", 
                       .inorder = F, 
                       .maxcombine = N_clusters,
                       .packages = c("tidyverse")
) %dopar% {
  
  cat(sprintf("%s  -  Cluster %s  -  started \n",Sys.time(),current_cluster),
      file=paste0("outfile_",format(start_time,"%Y_%m_%d_%H_%M"),"_",current_cluster,".txt"), append=T)
  
  # Generate random date allocation if wanted
  if(sim_type=="random"){
    state_info_mult=state_info %>% 
      mutate(outer_path=1)
    state_info_mult$Date=sample(state_info$Date, replace = F)
  }

  # generate preferences
  state_info_mult$Preferences=rnorm(nrow(state_info_mult),0,1)
  
  # Empty data frames to save results into
  all_elections_base=data.frame()
  EI_values=data.frame()
  
  for(outer_date_index in 1:length(unique_dates)){
    # filter the current date
    current_election=state_info_mult %>% 
      filter(Date==unique_dates[outer_date_index])
    current_election_info= current_election %>% select(State, Delegates, Date) %>% unique()
    N_elections=nrow(current_election_info)
    
    # Calculate spillover
    for(current_candidate in 1:N_cand){
      if(outer_date_index==1){
        current_election[,paste0("Spillover_",current_candidate)]=0
      } else {
        if(N_cand==2 & current_candidate==2){
          current_election$Spillover_2=-current_election$Spillover_1
        } else {
          all_elections_base$Votes=all_elections_base[,paste0("Votes_",current_candidate)]
          spillover=all_elections_base %>% 
            select(outer_path, State, Votes, Delegates) %>%
            unique() %>% 
            group_by(outer_path) %>% 
            summarize(Votes_share=sum(Votes)/sum(Delegates))
          spillover[,paste0("Spillover_",current_candidate)]=spillover$Votes_share-exp(candidate_FE[current_candidate])/sum(exp(candidate_FE))
          current_election=left_join(current_election,
                                     spillover %>% select(-Votes_share), by=c("outer_path"))
          all_elections_base=all_elections_base %>% select(-Votes)
          rm(spillover)
        }
      }
    }
    
    for(current_election_index in 1:N_elections){
      all_elections=all_elections_base

      # Multiply with inner_path and candidates
      current_sim=current_election %>%
        filter(State==current_election_info$State[current_election_index]) %>%
        mutate(count=N_inner*N_cand) %>%
        uncount(count,.id = "inner_path")
      current_sim$Winner=rep(1:N_cand, N_inner)

      # Evaluate the current election manually
      for(current_candidate in 1:N_cand){
        current_sim[,paste0("Votes_",current_candidate)]=
          (current_sim$Winner==current_candidate)*current_sim$Delegates
      }
      all_elections=bind_rows(all_elections,
                              current_sim %>%
                                select(State, Delegates, Date, outer_path, inner_path, Winner, starts_with(c("Votes_"))))

      # if there are multiple elections on that date, then continue at the same date, but exclude the state from the simulation
      if(N_elections>1){
        sim_start_index=outer_date_index
      } else {
        sim_start_index=outer_date_index+1
      }

      if((outer_date_index<length(unique_dates)) | (N_elections>1)){
        for(inner_date_index in sim_start_index:length(unique_dates)){
          # filter the current date and multiply with inner_path and candidates
          current_sim=merge(state_info_mult %>%
                              filter((Date==unique_dates[inner_date_index]) & (State != current_election_info$State[current_election_index])) %>%
                              mutate(count=N_inner*N_cand) %>%
                              uncount(count,.id = "inner_path"),
                            all_elections %>% select(outer_path, inner_path, Winner) %>% unique()) %>%
            arrange(State, outer_path, inner_path)

          # Calculate spillover
          for(current_candidate in 1:N_cand){
            if(inner_date_index==1){
              current_sim[,paste0("Spillover_",current_candidate)]=0
            } else {
              if(N_cand==2 & current_candidate==2){
                current_sim$Spillover_2=-current_sim$Spillover_1
              } else {
                all_elections$Votes=all_elections[,paste0("Votes_",current_candidate)]
                spillover=all_elections %>%
                  filter((State != current_election_info$State[current_election_index]) | (inner_date_index!=outer_date_index)) %>%
                  group_by(outer_path, inner_path) %>% summarize(Votes_share=sum(Votes)/sum(Delegates), .groups = "drop")
                spillover[,paste0("Spillover_",current_candidate)]=spillover$Votes_share-exp(candidate_FE[current_candidate])/sum(exp(candidate_FE))
                current_sim=left_join(current_sim, spillover %>% select(-Votes_share), by=c("outer_path","inner_path"))
                rm(spillover)
              }
            }
          }

          # calculate scores
          for(current_candidate in 1:N_cand){
            current_sim[,paste0("Score_",current_candidate)]=
              alpha*(candidate_characteristics[current_candidate]-current_sim$Preferences)^2+
                     candidate_FE[current_candidate]+beta*current_sim[,paste0("Spillover_",current_candidate)]
          }
          # Calculate probabilities
          current_sim[,paste0("Prob_",1:N_cand)]=
            exp(current_sim %>% select(starts_with("Score_")))/
            rowSums(exp(current_sim %>% select(starts_with("Score_"))))

          # Evaluate winner
          current_sim$random_value=runif(nrow(current_sim))
          if(N_cand==2){
            current_sim[,paste0("Votes_",c(1,2))]=current_sim$Delegates*
              bind_cols("Votes_1"=current_sim$Prob_1>current_sim$random_value,
                        "Votes_2"=current_sim$Prob_1<current_sim$random_value)
              
          } else {
            current_sim$Cum_Prob_0=0
            for(current_candidate in 1:N_cand){
              current_sim[,paste0("Cum_Prob_",current_candidate)]=
                current_sim[,paste0("Cum_Prob_",current_candidate-1)]+current_sim[,paste0("Prob_",current_candidate)]
              current_sim[,paste0("Votes_",current_candidate)]=
                current_sim$Delegates*
                (current_sim[,paste0("Cum_Prob_",current_candidate)]>current_sim$random_value)*
                (current_sim[,paste0("Cum_Prob_",current_candidate-1)]<current_sim$random_value)
            }
          }
          
          # Add to election results table
          all_elections=bind_rows(all_elections,
                                  current_sim %>%
                                    select(State, Delegates, Date, outer_path, inner_path, Winner, starts_with(c("Votes_")))) %>%
            group_by(outer_path, inner_path, Winner) %>%
            summarise(across(c("Delegates",starts_with("Votes_")), list(sum), .names = "{.col}"), .groups = "drop") %>%
            mutate(State="Aggregate",Date=current_election_info$Date[current_election_index])
        }
      }

      # aggregate votes together
      aggregate_votes=all_elections %>%
        group_by(outer_path, inner_path, Winner) %>%
        summarise(across(starts_with("Votes_"), list(sum), .names = "{.col}"), .groups = "drop")

      # Calculate the EI values
      for(current_candidate in 1:N_cand){
        aggregate_votes$Won=aggregate_votes[,paste0("Votes_",current_candidate)]==apply(aggregate_votes %>% select(starts_with("Votes")), 1, max)
        aggregate_cand=aggregate_votes %>% 
          group_by(outer_path, Winner) %>% 
          summarize(win_share=mean(Won), .groups="drop") %>% 
          group_by(outer_path) %>%
          summarize(EI=sum(abs(win_share-mean(win_share)))) %>%
          mutate("State"=current_election_info$State[current_election_index],
                 "Delegates"=current_election_info$Delegates[current_election_index],
                 "Date"=current_election_info$Date[current_election_index])
        EI_values=bind_rows(EI_values,
                            aggregate_cand %>% select(-outer_path))
      }
    }
    
    # Update the all_elections_base with the current election but NOT set manually
    # calculate scores
    for(current_candidate in 1:N_cand){
      current_election[,paste0("Score_",current_candidate)]=
        alpha*(candidate_characteristics[current_candidate]-current_election$Preferences)^2+
               candidate_FE[current_candidate]+beta*current_election[,paste0("Spillover_",current_candidate)]
    }
    # Calculate probabilities
    current_election[,paste0("Prob_",1:N_cand)]=
      exp(current_election %>% select(starts_with("Score_")))/
      rowSums(exp(current_election %>% select(starts_with("Score_"))))
    
    # Evaluate winner
    current_election$random_value=runif(nrow(current_election))
    if(N_cand==2){
      current_election[,paste0("Votes_",c(1,2))]=current_election$Delegates*
        bind_cols(current_election$Prob_1>current_election$random_value,
                  current_election$Prob_1<current_election$random_value)
      
    } else {
      current_election$Cum_Prob_0=0
      for(current_candidate in 1:N_cand){
      current_election[,paste0("Cum_Prob_",current_candidate)]=
        current_election[,paste0("Cum_Prob_",current_candidate-1)]+current_election[,paste0("Prob_",current_candidate)]
      current_election[,paste0("Votes_",current_candidate)]=
        current_election$Delegates*
        (current_election[,paste0("Cum_Prob_",current_candidate)]>current_election$random_value)*
        (current_election[,paste0("Cum_Prob_",current_candidate-1)]<current_election$random_value)
      }
    }
    
    for(current_State in unique(current_election$State)){
      base_add=current_election %>% 
        filter(State==current_State) %>% 
        select(State, Delegates, Date, outer_path, Preferences, starts_with(c("Spillover_","Votes_"))) %>%
        mutate(count=N_inner*N_cand) %>%
        uncount(count,.id = "inner_path")
      base_add$Winner=rep(1:N_cand, N_inner)
      # Add to election results table
      all_elections_base=bind_rows(all_elections_base,
                                   base_add %>%
                                     group_by(outer_path, inner_path, Winner) %>% 
                                     summarise(across(c("Delegates",starts_with("Votes_")), list(sum), .names = "{.col}"), .groups = "drop") %>%
                                     mutate(State="Aggregate",Date=current_election_info$Date[1]))
    }
    
    
    cat(sprintf("%s  -  Cluster %s  -  Time_index %s finished \n",Sys.time(),current_cluster, outer_date_index),
        file=paste0("outfile_",format(start_time,"%Y_%m_%d_%H_%M"),"_",current_cluster,".txt"), append=T)
    
  }
 EI_values 
}

stopCluster(clusters) 
rm(clusters)

start_time
Sys.time()

write.csv(EI_values_regular, file = "EI_values_regular.csv",row.names = F)

# Analysis of results (all) ---------------------------------------------------------------

plot_all=function(type){
  EI_values=read.csv(file = paste0("EI_values_",type,".csv"))
  EI_values$Date=as.Date(EI_values$Date,"%Y-%m-%d")
  
  state_info=read.csv("../data/front_loading.csv",header = T, stringsAsFactors = F)
  state_info$Date=as.Date(state_info$Date,"%d.%m.%Y")
  unique_dates=sort(unique(state_info$Date))
  
  EI_values_date=left_join(EI_values,data.frame("Date"=unique_dates) %>% 
                             arrange(Date) %>% 
                             mutate(Election_Rank=row_number()), by="Date")
  
  EI_scatter=EI_values_date %>% group_by(State, Delegates) %>% 
    summarise(avg_EI=mean(EI), 
              Election_Rank=mean(Election_Rank),
              .groups = "drop") %>%
    mutate(Position=rank(Election_Rank, ties.method = "average"))
  
  front_loading_scatter= ggplot(EI_scatter, aes(x=Delegates, y=avg_EI))
  
  if(type=="random"){
    front_loading_scatter=front_loading_scatter+
      geom_point(size=5)
    } else {
    front_loading_scatter=front_loading_scatter+
      geom_point(aes(color=Position), size=5)+
      labs(color="Position")
  }
  
  front_loading_scatter=front_loading_scatter+
    scale_colour_viridis_c(option = "viridis", direction=1 ) +
    geom_smooth(aes(color="black"),method = lm, formula="y~x", se = FALSE, color="black")+
    labs(x="Number of delegates",y="avg. EI")+
    theme_light()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          text=element_text(size=18),
          axis.title.x=element_text(margin=margin(10,0,0,0)),
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          panel.grid.major.x=element_blank())

  ggsave(paste0("front_loading_scatter_",type,".png"),
         front_loading_scatter,
         device = "png", width = 20, height = 12, units = "cm",dpi = 300, 
         path = paste0("../plots/front_loading/"))
  
}
plot_all(type="regular")
plot_all(type="rank_increase")
plot_all(type="random")


# Analysis of results (single states) ---------------------------------------------------------------

EI_values=read.csv(file = "EI_values_Iowa.csv")
EI_values=EI_values %>% mutate(Date=ifelse(Date=="2020-02-01","2020-02-03",Date)) %>%
  arrange(Date)
EI_values$labels=factor(EI_values$Date, 
                      levels = c("2020-02-03","2020-03-01","2020-03-03","2020-03-07","2020-06-13"),
                      labels = c("Position 1\nFebruary 3, 2020",
                                 "Position 4\nMarch 1, 2020",
                                 "Position 5 (ST)\nMarch 3, 2020",
                                 "Position 20\nMarch 7, 2020",
                                 "Position 50\nJune 13, 2020"),
                      ordered = T)
EI_values$labels_small=factor(EI_values$Date, 
                        levels = c("2020-02-03","2020-03-01","2020-03-03","2020-03-07","2020-06-13"),
                        labels = c("1",
                                   "4",
                                   "5 (ST)",
                                   "20",
                                   "50"),
                        ordered = T)

# EI boxplot by date

state_boxplot_positions=ggplot(EI_values) + 
  geom_boxplot(aes(x=labels_small, y=EI, color=State), size=0.5) +
  labs(x="Simulated position in election schedule", y="EI")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text=element_text(size=16),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        panel.grid.major.x=element_blank())+
        scale_colour_viridis_d(option = "viridis", direction=1, begin=0.2, end=0.6) 
ggsave("state_boxplot_positions.png",
       state_boxplot_positions,
       device = "png", width = 20, height = 12, units = "cm",dpi = 300, 
       path = paste0("../plots/front_loading/"))
