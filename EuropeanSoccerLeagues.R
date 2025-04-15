################################## Match Importance Project #########################################
#
# Author:
# Sandro Heiniger
# Swiss Institute for Empirical Economic Research
# University St. Gallen
# sandro.heiniger@unisg.ch
#
######  Version: 21.06.2022


# Environment Settings ----------------------------------------------------


# General Choices
setwd("your_path")              



# Load packages 
library(install.load)
install_load("dplyr","tidyverse","orf","MASS","readxl","doSNOW",)

# ORF covariates ----------------------------------------------------------

#Variables
covariates_short <- c(
  # League
  "Season_id","PL", "SA", "BL", "LL", "L1", "PO", "ED", 
  # Matchday
  "Stad_capacity", "Time", "time_min", "dist_km",
  "christ_break", "pub_holiday", "weekend",
  "bef_int_comp", "aft_int_comp", "before_int_break", "after_int_break", 
  "african_cup",  "asian_cup", "bef_euro", "after_euro", "bef_wc", "after_wc", 
  # Club info
  "H_kader", "H_newcomer", "H_plays_EL", "H_plays_CL", "H_plays_EC",
  "A_kader", "A_newcomer", "A_plays_EL", "A_plays_CL", "A_plays_EC",
  # Form 
  "H_last_1", "H_last_2", "H_last_3", "H_last_4", "H_share_points_lag",
  "A_last_1", "A_last_2", "A_last_3", "A_last_4", "A_share_points_lag",
  "H_days_since", "H_days_next", "A_days_since", "A_days_next", 
  # Squad value
  "H_Teamvalue", "H_HHI", "H_dHHI", "H_Stdv", "H_standz_teamvalue", "H_Skew",#  "H_Average", "H_Median", 
  "A_Teamvalue", "A_HHI", "A_dHHI", "A_Stdv", "A_standz_teamvalue", "A_Skew",#   "A_Average","A_Median", 
  "H_Top3Rank1214", "H_Top3Rank911", "H_Top11Rank1221", "H_StdvAverage",   
  "A_Top3Rank1214", "A_Top3Rank911", "A_Top11Rank1221", "A_StdvAverage",   
  "HA_teamvalue", "HA_standz_teamvalue", "HrA_teamvalue", "HrA_standz_teamvalue",#  "HA_average", 
  # Foot
  "H_share_right", "H_share_left", "H_share_both", 
  "A_share_right", "A_share_left", "A_share_both",
  "H_t11_share_right","H_t11_share_left", "H_t11_share_both", "H_t3_share_right", "H_t3_share_left", "H_t3_share_both",
  "A_t11_share_right","A_t11_share_left", "A_t11_share_both", "A_t3_share_right", "A_t3_share_left", "A_t3_share_both",
  # Height
  "H_mean_height", "H_min_height", "H_max_height", "H_sd_height",  
  "A_mean_height", "A_min_height", "A_max_height", "A_sd_height", 
  "H_t11_mean_height","H_t11_min_height", "H_t11_max_height", "H_t11_sd_height",
  "A_t11_mean_height","A_t11_min_height", "A_t11_max_height", "A_t11_sd_height",  
  "HA_mean_height", "HA_t11_mean_height",
  # Age
  "H_AgeTop11", "H_AgeTop11Age1221","H_Ageabove20", "H_Agemin", "H_AverageAge", 
  "A_AgeTop11", "A_AgeTop11Age1221","A_Ageabove20", "A_Agemin", "A_AverageAge", 
  "H_Stdvage", "H_StdvaveAge", "H_MedAge", "H_Agemax",                
  "A_Stdvage", "A_StdvaveAge", "A_MedAge", "A_Agemax")


#"H_position", "H_points_acc", "H_goaldiff_acc", "H_share_points", 
#"A_position", "A_points_acc", "A_goaldiff_acc", "A_share_points", 
covariates_long=c(
  "Season_id","PL", "SA", "BL", "LL", "L1", "PO", "ED", 
  # Matchday
  "Stad_capacity", "Time", "time_min", "dist_km",
  "christ_break", "pub_holiday", "weekday", "weekend",
  "bef_int_comp", "aft_int_comp", "before_int_break", "after_int_break", 
  "african_cup",  "asian_cup", "bef_euro", "after_euro", "bef_wc", "after_wc", 
  # Club info
  "H_kader", "H_newcomer", "H_plays_EL", "H_plays_CL", "H_plays_EC",
  "A_kader", "A_newcomer", "A_plays_EL", "A_plays_CL", "A_plays_EC",
  # Form 
  "H_last_1", "H_last_2", "H_last_3", "H_last_4", "H_share_points_lag",
  "A_last_1", "A_last_2", "A_last_3", "A_last_4", "A_share_points_lag",
  "H_days_since", "H_days_next", "A_days_since", "A_days_next", 
  # Squad value
  "H_Teamvalue", "H_HHI", "H_dHHI", "H_Average", "H_Stdv", "H_Median", "H_standz_teamvalue", "H_Skew",
  "A_Teamvalue", "A_HHI", "A_dHHI", "A_Average", "A_Stdv", "A_Median", "A_standz_teamvalue", "A_Skew",
  "H_Top3Rank1214", "H_Top3Rank911", "H_Top11Rank1221", "H_StdvAverage",   
  "A_Top3Rank1214", "A_Top3Rank911", "A_Top11Rank1221", "A_StdvAverage",   
  "HA_teamvalue", "HA_standz_teamvalue", "HA_average", "HrA_teamvalue", "HrA_standz_teamvalue", 
  # Foot
  "H_share_right", "H_share_left", "H_share_both", 
  "A_share_right", "A_share_left", "A_share_both",
  "H_t11_share_right","H_t11_share_left", "H_t11_share_both", "H_t3_share_right", "H_t3_share_left", "H_t3_share_both",
  "A_t11_share_right","A_t11_share_left", "A_t11_share_both", "A_t3_share_right", "A_t3_share_left", "A_t3_share_both",
  # Height
  "H_mean_height", "H_min_height", "H_max_height", "H_sd_height",  
  "A_mean_height", "A_min_height", "A_max_height", "A_sd_height", 
  "H_t11_mean_height","H_t11_min_height", "H_t11_max_height", "H_t11_sd_height",
  "A_t11_mean_height","A_t11_min_height", "A_t11_max_height", "A_t11_sd_height",  
  "HA_mean_height", "HA_t11_mean_height",
  # Age
  "H_AgeTop11", "H_AgeTop11Age1221","H_Ageabove20", "H_Agemin", "H_AverageAge", 
  "A_AgeTop11", "A_AgeTop11Age1221","A_Ageabove20", "A_Agemin", "A_AverageAge", 
  "H_Stdvage", "H_StdvaveAge", "H_MedAge", "H_Agemax",                
  "A_Stdvage", "A_StdvaveAge", "A_MedAge", "A_Agemax",                  
  # Season  
  "S_0607", "S_0708", "S_0809", "S_0910", "S_1011", "S_1112", 
  "S_1213", "S_1314", "S_1415", "S_1516", "S_1617", "S_1718", "S_1819",
  # Year
  "y_2006", "y_2007", "y_2008", "y_2009", "y_2010", "y_2011", "y_2012", 
  "y_2013", "y_2014", "y_2015", "y_2016", "y_2017", "y_2018", "y_2019" 
)


# Function definitions ----------------------------------------------------

# feature selection procedure, works with either LASSO or RF
featureselection=function(data_fs, pre_fs_covariates, methodfs, seed){
  Y <- as.matrix(data_fs$Home_points)
  X <- as.matrix(data_fs[pre_fs_covariates])
  if (methodfs=="Lasso"){
    set.seed(seed)
    outcv=cv.glmnet(X,Y,nfolds = 10)
    out=glmnet(X,y=Y,alpha=1,lambda=outcv$lambda.min,family="gaussian")
    x_sel_cvmin=X[,out$beta@i]
    selected.vars = dimnames(x_sel_cvmin)[[2]]
  } else if (methodfs=="RF"){
    forest.Y = regression_forest(X, Y, tune.parameters = "all", seed = seed)
    Y.hat = predict(forest.Y)$predictions
    
    forest.Y.varimp = variable_importance(forest.Y)
    
    selected.vars = pre_fs_covariates[which(forest.Y.varimp / mean(forest.Y.varimp) > 0.2)]
  } else {
    print("Chosen method not implemented. Either choose Lasso or RF")
  }
  return(selected.vars)
}
# loads data from a file
load_data=function(path_to_data, dat_name, selected_leagues, selected_seasons, covariates, 
                   threshold_positions, feature_selection, feature_selection_seasons, seed, 
                   add_league_dummies, add_season_dummies, split_data){
  
  # Specify additional covariates that have to be imported additionally to the covariates
  additional_variables <- c("League","hometeam","awayteam","Season","b365h","b365d","b365a","Matchday","Season_id",
                            "Home_points","Away_points", "H_points_acc","A_points_acc","H_goaldiff_acc","A_goaldiff_acc","date",
                            "League_season_index", "fthg", "ftag")
  
  dat_all = read_xls(path=paste0(path_to_data,dat_name), sheet = 1, col_names = T)
  dat_all=dat_all %>% filter(!(ED==1 & Season<2009) & !(SA==1 & Season==2006)) %>% 
    #mutate(date=strptime(paste0(date), "%d/%m/%Y"),
    mutate(date=strptime(paste0(date,"/",Time), "%d/%m/%Y/%H"),
           League=case_when(BL==1 ~ "BL",ED==1 ~ "ED",LL==1 ~ "LL",L1==1 ~ "L1",PL==1 ~ "PL",PO==1 ~ "PO",SA==1 ~ "SA"))
  
  # manually correct scores
  dat_all[dat_all$Season==2016 & dat_all$hometeam=="Bastia" & dat_all$awayteam=="Lyon",c("fthg", "ftag")] = 0
  dat_all[dat_all$Season==2016 & dat_all$hometeam=="Bastia" & dat_all$Matchday>=33,c("H_goaldiff_acc")]=dat_all[dat_all$Season==2016 & dat_all$hometeam=="Bastia" & dat_all$Matchday>=33,c("H_goaldiff_acc")]+3
  dat_all[dat_all$Season==2016 & dat_all$awayteam=="Bastia" & dat_all$Matchday>=33,c("A_goaldiff_acc")]=dat_all[dat_all$Season==2016 & dat_all$awayteam=="Bastia" & dat_all$Matchday>=33,c("A_goaldiff_acc")]+3
  dat_all[dat_all$Season==2016 & dat_all$hometeam=="Lyon" & dat_all$Matchday>=33,c("H_goaldiff_acc")]=dat_all[dat_all$Season==2016 & dat_all$hometeam=="Lyon" & dat_all$Matchday>=33,c("H_goaldiff_acc")]-3
  dat_all[dat_all$Season==2016 & dat_all$awayteam=="Lyon" & dat_all$Matchday>=33,c("A_goaldiff_acc")]=dat_all[dat_all$Season==2016 & dat_all$awayteam=="Lyon" & dat_all$Matchday>=33,c("A_goaldiff_acc")]-3
  
  # reduce to selected leagues
  dat_leagues=dat_all[which(rowSums(dat_all[,selected_leagues])==1),]
  
  # Generate league index variable
  dat_leagues$League_season_index=dat_leagues %>% group_indices(Season, League)
  
  # Use feature selection if selected
  if(!is.na(feature_selection)){
    covariates=featureselection(dat_leagues %>% filter(Season %in% feature_selection_seasons),covariates,feature_selection, seed)
    # Add league dummies
    if(add_league_dummies){
      covariates=unique(c(covariates, c("PL", "SA", "BL", "LL", "L1", "PO", "ED")))
    }
    # add season dummies
    if(add_season_dummies){
      remaining_seasons=substr(selected_seasons[!(selected_seasons %in% feature_selection_seasons)],3,4)
      covariates=unique(c(covariates, paste0("S_",remaining_seasons,as.integer(remaining_seasons)+1)))
    }
    
    # remove seasons from the sample
    dat_leagues=dat_leagues %>% filter(!(Season %in% feature_selection_seasons))
  }
  
  # Generate a running variable by game-time within season
  game_times_unique=unique(dplyr::select(dat_leagues, League_season_index, date))
  game_times_ranked = game_times_unique %>% group_by(League_season_index) %>% mutate(running_date = rank(date)) %>% ungroup
  
  dat_leagues=left_join(dat_leagues,game_times_ranked, by = c("date", "League_season_index"))
  
  if(split_data){
    # Split data in different samples according to defined share 
    correct_allocation=F
    set.seed(seed)
    N_season=length(unique(dat_leagues$Season))
    N_league=length(unique(dat_leagues$League))
    # Retry until an allocation with every league and season in orf training sample is found
    while(correct_allocation==F){
      dat_allocation=data.frame(unique(dat_leagues[,c("League_season_index","League","Season")]))
      
      dat_random=data.frame("League"=c(unique(dat_allocation$League), 
                                       sample(unique(dat_allocation$League), size = (N_season-N_league), replace = F),
                                       unique(dat_allocation$League), 
                                       sample(unique(dat_allocation$League), size = (N_season-N_league), replace = F)),
                            "Season"=c(sample(unique(dat_allocation$Season)),sample(unique(dat_allocation$Season))),
                            "data_set"=c(rep(1,N_season),rep(3,N_season)))
      
      dat_allocation = left_join(dat_allocation, dat_random, by=c("League","Season")) %>% replace_na(list(data_set=2))
      if(nrow(dat_random %>% dplyr::select(-data_set) %>% unique())==2*N_season){
        correct_allocation=T
      }
    }
    
    dat_leagues= left_join(dat_leagues, dat_allocation %>% dplyr::select(-League, -Season), by=c("League_season_index"="League_season_index"))
    
    # generate the MI covariates
    data_thresh=generate_MI_covariates(dat_leagues, threshold_positions) %>% ungroup()
    
    # Split in training and prediction sample
    dat_ORF = data_thresh %>% filter(data_set==1) %>% dplyr::select(-data_set)
    dat_MI  = data_thresh %>% filter(data_set==2) %>% dplyr::select(-data_set)
    dat_OOS = data_thresh %>% filter(data_set==3) %>% dplyr::select(-data_set) 
    
    return(list("ORF"=list("X"=dat_ORF, "Y"=dat_ORF$Home_points),
                "MI"=list("X"=dat_MI, "Y"=dat_MI$Home_points),
                "OOS"=list("X"=dat_OOS, "Y"=dat_OOS$Home_points),
                "covariates"=covariates))
  } else {
    data_thresh=generate_MI_covariates(dat_leagues, threshold_positions) %>% ungroup()
    return(list("FULL"=list("X"=data_thresh, "Y"=data_thresh$Home_points),
                "covariates"=covariates))
  }
  
}
#generate the MI covariates
generate_MI_covariates=function(data_in, threshold_positions){
  
  MI_covariates=c("MI_h_ppg_diff_up","MI_a_ppg_diff_up","MI_h_ppg_diff_down","MI_a_ppg_diff_down",
                  "MI_h_points_diff_up","MI_a_points_diff_up","MI_h_points_diff_down","MI_a_points_diff_down",
                  "MI_h_ppg_required_up","MI_a_ppg_required_up","MI_h_ppg_required_down","MI_a_ppg_required_down",
                  "MI_h_Goal_diff_pgr_up","MI_h_Goal_diff_pgr_down","MI_a_Goal_diff_pgr_up","MI_a_Goal_diff_pgr_down",
                  "MI_h_rank_range","MI_a_rank_range","MI_h_rank_rel","MI_a_rank_rel",
                  "MI_h_future_opp","MI_a_future_opp","MI_h_thresh_fixed","MI_a_thresh_fixed",
                  "MI_h_competitors","MI_a_competitors", "MI_h_games_competitors","MI_a_games_competitors",
                  "MI_match_ppg_diff","MI_match_competitor","MI_h_matchday_share","MI_a_matchday_share")
  
  #Join threshold information to the data
  data_thresh=left_join(data_in, threshold_positions, by=c("League", "Season")) %>% filter(date>Date, Team==hometeam | Team=="Regular") %>% 
    mutate(team_specific=ifelse(Team==hometeam,1,0)) %>% arrange(team_specific, Date) %>% group_by(League, Season, hometeam, running_date) %>% slice(n()) %>% 
    dplyr::select(-Date, -Team, -team_specific)
  
  data_thresh=left_join(data_thresh, threshold_positions, by=c("League", "Season")) %>% filter(date>Date, Team==awayteam | Team=="Regular") %>% 
    mutate(team_specific=ifelse(Team==awayteam,1,0)) %>% arrange(team_specific, Date) %>% group_by(League, Season, awayteam, running_date) %>% slice(n()) %>% 
    dplyr::select(-Date, -Team, -team_specific)
  
  for(current_league_season_index in unique(data_thresh$League_season_index)){
    # current league_season data
    data_thresh_current_season=data_thresh  %>% ungroup() %>% filter(League_season_index==current_league_season_index)
    # number of teams in this league_season
    N_teams=length(unique(data_thresh_current_season$hometeam))
    
    
    # first evaluate only the thresh_fixed variables
    
    for(current_game_time in 1:max(data_thresh_current_season$running_date)){
      # generate the ranking before this game_time
      current_ranking=rbind(data_thresh_current_season %>% filter(running_date<current_game_time) %>% mutate(Goal_diff=fthg-ftag) %>% 
                              dplyr::select(hometeam, Home_points, Goal_diff) %>% rename(Team=hometeam, Points=Home_points),
                            data_thresh_current_season %>% filter(running_date<current_game_time) %>% mutate(Goal_diff=ftag-fthg) %>% 
                              dplyr::select(awayteam, Away_points, Goal_diff) %>% rename(Team=awayteam, Points=Away_points)) %>% group_by(Team) %>% 
        summarize(Games=n(), ppg=sum(Points)/n(), Goal_diff=sum(Goal_diff), Points=sum(Points), .groups = 'drop')
      not_yet_played=data.frame("Team"=c(unique(data_thresh_current_season$hometeam)), "Points"=0, "Games"=0, "ppg"=0, "Goal_diff"=0)
      current_ranking=bind_rows(current_ranking, not_yet_played[!(not_yet_played$Team %in% current_ranking$Team),]) %>% arrange(-ppg, -Goal_diff, -Games, Team)
      current_ranking$Position=1:nrow(current_ranking)
      current_ranking = current_ranking %>% mutate(ppg_max=(Games*ppg+((N_teams-1)*2-Games)*3)/(N_teams-1)/2, ppg_min=Games*ppg/(N_teams-1)/2)
      
      # all data for this current game_time
      data_thresh_current_game_time=data_thresh_current_season %>% filter(running_date==current_game_time)
      
      # number of games at this current game_time
      N_games=nrow(data_thresh_current_game_time)
      
      # initialize empty data frame to be filled in loop
      current_game_time_df=data.frame()
      for(i in 1:N_games){
        # handy information for current home team
        home_team_thresholds=data_thresh_current_game_time[i,c("CH.x","CL.x","EL.x","ELP.x","NO.x","REP.x","RE.x")]
        away_team_thresholds=data_thresh_current_game_time[i,c("CH.y","CL.y","EL.y","ELP.y","NO.y","REP.y","RE.y")]
        home_team_rank=as.integer(current_ranking[which(current_ranking$Team==(data_thresh_current_game_time[i,]$hometeam)),"Position"])
        away_team_rank=as.integer(current_ranking[which(current_ranking$Team==(data_thresh_current_game_time[i,]$awayteam)),"Position"])
        
        # regular ranking
        current_ranking_h_down=current_ranking
        home_team_rank_down=home_team_rank
        
        # in case a team has to be excluded
        if(data_thresh_current_game_time[i,"Exclude_from_ranking.x"]!=""){
          if(home_team_rank > current_ranking[which(current_ranking$Team==data_thresh_current_game_time$Exclude_from_ranking.x[i]),"Position"]){
            home_team_rank_exclude=home_team_rank-1
          } else {
            home_team_rank_exclude=home_team_rank
          }
          if(home_team_group %in% c(3,4)){
            current_ranking_h_down=current_ranking %>% filter(Team !=data_thresh_current_game_time$Exclude_from_ranking.x[i])
            current_ranking_h_down$Position=1:nrow(current_ranking_h_down)
            home_team_rank_down=home_team_rank_exclude
          }
        }
        
        # Check if international place is fixed
        if(all(is.na(home_team_thresholds[,c("EL.x","ELP.x")]))){
          current_game_time_df[i,"MI_h_international_fixed"]=1
        } else{
          EL_threshold=max(home_team_thresholds[,c("EL.x","ELP.x")], na.rm = T)+1
          current_game_time_df[i,"MI_h_international_fixed"]=(home_team_rank_down<EL_threshold)*
            as.integer(max((3*(2*(N_teams-1)-current_ranking_h_down$Games[EL_threshold:nrow(current_ranking_h_down)])+
                              current_ranking_h_down$Points[EL_threshold:nrow(current_ranking_h_down)]-
                              current_ranking_h_down$Points[home_team_rank_down])/(2*(N_teams-1)-current_ranking_h_down$Games[home_team_rank_down]))<0)
        }
        current_game_time_df[i,"MI_h_EL_spot_fixed"]=(home_team_rank_down<EL_threshold)*
          as.integer(max((3*(2*(N_teams-1)-current_ranking_h_down$Games[EL_threshold:nrow(current_ranking_h_down)])+
                            current_ranking_h_down$Points[EL_threshold:nrow(current_ranking_h_down)]-
                            current_ranking_h_down$Points[home_team_rank_down])/(2*(N_teams-1)-current_ranking_h_down$Games[home_team_rank_down]))<0)
        
        # regular ranking
        current_ranking_a_down=current_ranking
        away_team_rank_down=away_team_rank
        
        # in case a team has to be excluded
        if(data_thresh_current_game_time[i,"Exclude_from_ranking.y"]!=""){
          if(away_team_rank > current_ranking[which(current_ranking$Team==data_thresh_current_game_time$Exclude_from_ranking.y[i]),"Position"]){
            away_team_rank_exclude=away_team_rank-1
          } else {
            away_team_rank_exclude=away_team_rank
          }
          if(away_team_group %in% c(3,4)){
            current_ranking_a_down=current_ranking %>% filter(Team !=data_thresh_current_game_time$Exclude_from_ranking.y[i])
            current_ranking_a_down$Position=1:nrow(current_ranking_a_down)
            away_team_rank_down=away_team_rank_exclude
          }
        }
        
        
        if(all(is.na(away_team_thresholds[,c("EL.y","ELP.y")]))){
          current_game_time_df[i,"MI_a_international_fixed"]=1
        } else{
          EL_threshold=max(away_team_thresholds[,c("EL.y","ELP.y")], na.rm = T)+1
          current_game_time_df[i,"MI_a_international_fixed"]=(away_team_rank_down<EL_threshold)*
            as.integer(max((3*(2*(N_teams-1)-current_ranking_a_down$Games[EL_threshold:nrow(current_ranking_a_down)])+
                              current_ranking_a_down$Points[EL_threshold:nrow(current_ranking_a_down)]-
                              current_ranking_a_down$Points[away_team_rank_down])/(2*(N_teams-1)-current_ranking_a_down$Games[away_team_rank_down]))<0)
        }
        current_game_time_df[i,"MI_a_EL_spot_fixed"]=(away_team_rank_down<EL_threshold)*
          as.integer(max((3*(2*(N_teams-1)-current_ranking_a_down$Games[EL_threshold:nrow(current_ranking_a_down)])+
                            current_ranking_a_down$Points[EL_threshold:nrow(current_ranking_a_down)]-
                            current_ranking_a_down$Points[away_team_rank_down])/(2*(N_teams-1)-current_ranking_a_down$Games[away_team_rank_down]))<0)
      }
      # update the season data with the MI covariates
      data_thresh_current_season[data_thresh_current_season$running_date==current_game_time,
                                 c("MI_h_international_fixed", "MI_a_international_fixed", "MI_h_EL_spot_fixed", "MI_a_EL_spot_fixed")]=
        current_game_time_df[,c("MI_h_international_fixed", "MI_a_international_fixed", "MI_h_EL_spot_fixed", "MI_a_EL_spot_fixed")]      
    }
    #View(data_thresh_current_season %>% arrange(running_date) %>% filter(MI_h_international_fixed==1) %>%  dplyr::select(hometeam, awayteam, running_date, date, MI_h_international_fixed))
    # loop over all game times in this league_season to generate the actual MI variables
    for(current_game_time in 1:max(data_thresh_current_season$running_date)){
      # generate the ranking before this game_time
      current_ranking=rbind(data_thresh_current_season %>% filter(running_date<current_game_time) %>% mutate(Goal_diff=fthg-ftag) %>% 
                              dplyr::select(hometeam, Home_points, Goal_diff) %>% rename(Team=hometeam, Points=Home_points),
                            data_thresh_current_season %>% filter(running_date<current_game_time) %>% mutate(Goal_diff=ftag-fthg) %>% 
                              dplyr::select(awayteam, Away_points, Goal_diff) %>% rename(Team=awayteam, Points=Away_points)) %>% group_by(Team) %>% 
        summarize(Games=n(), ppg=sum(Points)/n(), Goal_diff=sum(Goal_diff), Points=sum(Points), .groups = 'drop')
      not_yet_played=data.frame("Team"=c(unique(data_thresh_current_season$hometeam)), "Points"=0, "Games"=0, "ppg"=0, "Goal_diff"=0)
      current_ranking=bind_rows(current_ranking, not_yet_played[!(not_yet_played$Team %in% current_ranking$Team),]) %>% arrange(-ppg, -Goal_diff, -Games, Team)
      current_ranking$Position=1:nrow(current_ranking)
      current_ranking = current_ranking %>% mutate(ppg_max=(Games*ppg+((N_teams-1)*2-Games)*3)/(N_teams-1)/2, ppg_min=Games*ppg/(N_teams-1)/2)
      
      # all data for this current game_time
      data_thresh_current_game_time=data_thresh_current_season %>% filter(running_date==current_game_time)
      
      # number of games at this current game_time
      N_games=nrow(data_thresh_current_game_time)
      game_time_end=data_thresh_current_game_time$date[1]+60*60*36
      
      # initialize empty data frame to be filled in loop
      current_game_time_df=data.frame()
      for(i in 1:N_games){
        # handy information for current home team
        home_team_name=data_thresh_current_game_time[i,]$hometeam
        away_team_name=data_thresh_current_game_time[i,]$awayteam
        home_team_rank=as.integer(current_ranking[which(current_ranking$Team==home_team_name),"Position"])
        away_team_rank=as.integer(current_ranking[which(current_ranking$Team==away_team_name),"Position"])
        home_team_ppg=as.numeric(current_ranking[home_team_rank,"ppg"])
        home_team_ppg_min=as.numeric(current_ranking[home_team_rank,"ppg_min"])
        home_team_ppg_max=as.numeric(current_ranking[home_team_rank,"ppg_max"])
        away_team_ppg=as.numeric(current_ranking[away_team_rank,"ppg"])
        away_team_ppg_min=as.numeric(current_ranking[away_team_rank,"ppg_min"])
        away_team_ppg_max=as.numeric(current_ranking[away_team_rank,"ppg_max"])
        home_team_thresholds=data_thresh_current_game_time[i,c("CH.x","CL.x","EL.x","ELP.x","NO.x","REP.x","RE.x")]
        away_team_thresholds=data_thresh_current_game_time[i,c("CH.y","CL.y","EL.y","ELP.y","NO.y","REP.y","RE.y")]
        
        # MI variables just based on the ranking
        current_game_time_df[i,"MI_match_ppg_diff"]=home_team_ppg-away_team_ppg
        current_game_time_df[i,"MI_match_competitor"]=1-as.integer((home_team_ppg_max<away_team_ppg_min)||(home_team_ppg_min>away_team_ppg_max))
        current_game_time_df[i,"MI_h_matchday_share"]=(current_ranking[which(current_ranking$Team==home_team_name),"Games"]+1)/(N_teams-1)/2
        current_game_time_df[i,"MI_a_matchday_share"]=(current_ranking[which(current_ranking$Team==away_team_name),"Games"]+1)/(N_teams-1)/2
        
        # if a team has to be excluded, find the corresponding rank in the reduced table
        if(data_thresh_current_game_time[i,"Exclude_from_ranking.x"]!=""){
          if(home_team_rank > current_ranking[which(current_ranking$Team==data_thresh_current_game_time$Exclude_from_ranking.x[i]),"Position"]){
            home_team_rank_exclude=home_team_rank-1
          } else {
            home_team_rank_exclude=home_team_rank
          }
        } else {
          home_team_rank_exclude=home_team_rank
        }
        if(data_thresh_current_game_time[i,"Exclude_from_ranking.y"]!=""){
          if(away_team_rank > current_ranking[which(current_ranking$Team==data_thresh_current_game_time$Exclude_from_ranking.y[i]),"Position"]){
            away_team_rank_exclude=away_team_rank-1
          } else {
            away_team_rank_exclude=away_team_rank
          }
        } else {
          away_team_rank_exclude=away_team_rank
        }
        
        # Adapt thresholds if other cup finalist team is fixed
        if(data_thresh_current_game_time[i,"Possible_direct.x"]!=""){
          possible_direct=paste(data_thresh_current_game_time[i,"Possible_direct.x"])
          possible_direct_info=data_thresh_current_season %>% 
            filter(hometeam ==possible_direct | awayteam==possible_direct, date<=game_time_end) %>% 
            arrange(-running_date) %>% slice(1) 
          if(nrow(possible_direct_info)==1){
            if(possible_direct_info$hometeam==possible_direct){
              MI_international_fixed=possible_direct_info[,"MI_h_international_fixed"]
            } else {
              MI_international_fixed=possible_direct_info[,"MI_a_international_fixed"]
            }
            if(MI_international_fixed==1){
              home_team_thresholds[,c("EL.x","ELP.x")]=NA
            }
          }
        }
        if(data_thresh_current_game_time[i,"Possible_direct.y"]!=""){
          possible_direct=paste(data_thresh_current_game_time[i,"Possible_direct.y"])
          possible_direct_info=data_thresh_current_season %>% 
            filter(hometeam ==possible_direct | awayteam==possible_direct, date<=game_time_end) %>% 
            arrange(-running_date) %>% slice(1) 
          if(nrow(possible_direct_info)==1){
            if(possible_direct_info$hometeam==possible_direct){
              MI_international_fixed=possible_direct_info[,"MI_h_international_fixed"]
            } else {
              MI_international_fixed=possible_direct_info[,"MI_a_international_fixed"]
            }
            if(MI_international_fixed==1){
              away_team_thresholds[,c("EL.y","ELP.y")]=NA
            }
          }
        }    
        
        # Default value for whether an EL change in threshold is still possible
        EL_change_possible=0
        
        # Adapt threshold if both cup finalists or the winner is fixed.
        if(data_thresh_current_game_time[i,"Cup_1_1.x"]!=""){
          possible_cup_winner=paste(data_thresh_current_game_time[i,"Cup_1_1.x"])
          possible_cup_winner_info=data_thresh_current_season %>% 
            filter(hometeam ==possible_cup_winner | awayteam==possible_cup_winner, date<=game_time_end) %>% 
            arrange(-running_date) %>% slice(1) 
          if(nrow(possible_cup_winner_info)==1){
            if(possible_cup_winner_info$hometeam==possible_cup_winner){
              MI_EL_spot_fixed=possible_cup_winner_info[,"MI_h_EL_spot_fixed"]
            } else {
              MI_EL_spot_fixed=possible_cup_winner_info[,"MI_a_EL_spot_fixed"]
            }
            # if possible_cup_winner is fixed in CL/EL, then check for second finalist if there is any
            if(MI_EL_spot_fixed==1){
              if(data_thresh_current_game_time[i,"Cup_1_2.x"]!=""){
                possible_cup_winner=paste(data_thresh_current_game_time[i,"Cup_1_2.x"])
                possible_cup_winner_info=data_thresh_current_season %>% 
                  filter(hometeam ==possible_cup_winner | awayteam==possible_cup_winner, date<=game_time_end) %>% 
                  arrange(-running_date) %>% slice(1) 
                if(nrow(possible_cup_winner_info)==1){
                  if(possible_cup_winner_info$hometeam==possible_cup_winner){
                    MI_EL_spot_fixed=possible_cup_winner_info[,"MI_h_EL_spot_fixed"]
                  } else {
                    MI_EL_spot_fixed=possible_cup_winner_info[,"MI_a_EL_spot_fixed"]
                  }
                  if(MI_EL_spot_fixed==1){
                    home_team_thresholds[,c("EL.x","ELP.x")]=home_team_thresholds[,c("EL.x","ELP.x")]+1
                  } else {
                    EL_change_possible=1
                  }
                }
              } else {
                # if there is no second finalist, then adjust anyways
                home_team_thresholds[,c("EL.x","ELP.x")]=home_team_thresholds[,c("EL.x","ELP.x")]+1
              }
            } else {
              EL_change_possible=1
            }
          }
        }
        if(data_thresh_current_game_time[i,"Cup_1_1.y"]!=""){
          possible_cup_winner=paste(data_thresh_current_game_time[i,"Cup_1_1.y"])
          possible_cup_winner_info=data_thresh_current_season %>% 
            filter(hometeam ==possible_cup_winner | awayteam==possible_cup_winner, date<=game_time_end) %>% 
            arrange(-running_date) %>% slice(1) 
          if(nrow(possible_cup_winner_info)==1){
            if(possible_cup_winner_info$hometeam==possible_cup_winner){
              MI_EL_spot_fixed=possible_cup_winner_info[,"MI_h_EL_spot_fixed"]
            } else {
              MI_EL_spot_fixed=possible_cup_winner_info[,"MI_a_EL_spot_fixed"]
            }
            # if possible_cup_winner is fixed in CL/EL, then check for second finalist if there is any
            if(MI_EL_spot_fixed==1){
              if(data_thresh_current_game_time[i,"Cup_1_2.y"]!=""){
                possible_cup_winner=paste(data_thresh_current_game_time[i,"Cup_1_2.y"])
                possible_cup_winner_info=data_thresh_current_season %>% 
                  filter(hometeam ==possible_cup_winner | awayteam==possible_cup_winner, date<=game_time_end) %>% 
                  arrange(-running_date) %>% slice(1) 
                if(nrow(possible_cup_winner_info)==1){
                  if(possible_cup_winner_info$hometeam==possible_cup_winner){
                    MI_EL_spot_fixed=possible_cup_winner_info[,"MI_h_EL_spot_fixed"]
                  } else {
                    MI_EL_spot_fixed=possible_cup_winner_info[,"MI_a_EL_spot_fixed"]
                  }
                  if(MI_EL_spot_fixed==1){
                    away_team_thresholds[,c("EL.y","ELP.y")]=away_team_thresholds[,c("EL.y","ELP.y")]+1
                  } else {
                    EL_change_possible=1
                  }
                }
              } else {
                # if there is no second finalist, then adjust anyways
                away_team_thresholds[,c("EL.y","ELP.y")]=away_team_thresholds[,c("EL.y","ELP.y")]+1
              }
            }
          } else {
            EL_change_possible=1
          }
        }
        if(data_thresh_current_game_time[i,"Cup_2_1.x"]!=""){
          possible_cup_winner=paste(data_thresh_current_game_time[i,"Cup_2_1.x"])
          possible_cup_winner_info=data_thresh_current_season %>% 
            filter(hometeam ==possible_cup_winner | awayteam==possible_cup_winner, date<=game_time_end) %>% 
            arrange(-running_date) %>% slice(1) 
          if(nrow(possible_cup_winner_info)==1){
            if(possible_cup_winner_info$hometeam==possible_cup_winner){
              MI_EL_spot_fixed=possible_cup_winner_info[,"MI_h_EL_spot_fixed"]
            } else {
              MI_EL_spot_fixed=possible_cup_winner_info[,"MI_a_EL_spot_fixed"]
            }
            # if possible_cup_winner is fixed in CL/EL, then check for second finalist if there is any
            if(MI_EL_spot_fixed==1){
              if(data_thresh_current_game_time[i,"Cup_2_2.x"]!=""){
                possible_cup_winner=paste(data_thresh_current_game_time[i,"Cup_2_2.x"])
                possible_cup_winner_info=data_thresh_current_season %>% 
                  filter(hometeam ==possible_cup_winner | awayteam==possible_cup_winner, date<=game_time_end) %>% 
                  arrange(-running_date) %>% slice(1) 
                if(nrow(possible_cup_winner_info)==1){
                  if(possible_cup_winner_info$hometeam==possible_cup_winner){
                    MI_EL_spot_fixed=possible_cup_winner_info[,"MI_h_EL_spot_fixed"]
                  } else {
                    MI_EL_spot_fixed=possible_cup_winner_info[,"MI_a_EL_spot_fixed"]
                  }
                  if(MI_EL_spot_fixed==1){
                    home_team_thresholds[,c("EL.x","ELP.x")]=home_team_thresholds[,c("EL.x","ELP.x")]+1
                  } else {
                    EL_change_possible=1
                  }
                }
              } else {
                # if there is no second finalist, then adjust anyways
                home_team_thresholds[,c("EL.x","ELP.x")]=home_team_thresholds[,c("EL.x","ELP.x")]+1
              }
            }
          } else {
            EL_change_possible=1
          }
        }
        if(data_thresh_current_game_time[i,"Cup_2_1.y"]!=""){
          possible_cup_winner=paste(data_thresh_current_game_time[i,"Cup_2_1.y"])
          possible_cup_winner_info=data_thresh_current_season %>% 
            filter(hometeam ==possible_cup_winner | awayteam==possible_cup_winner, date<=game_time_end) %>% 
            arrange(-running_date) %>% slice(1) 
          if(nrow(possible_cup_winner_info)==1){
            if(possible_cup_winner_info$hometeam==possible_cup_winner){
              MI_EL_spot_fixed=possible_cup_winner_info[,"MI_h_EL_spot_fixed"]
            } else {
              MI_EL_spot_fixed=possible_cup_winner_info[,"MI_a_EL_spot_fixed"]
            }
            # if possible_cup_winner is fixed in CL/EL, then check for second finalist if there is any
            if(MI_EL_spot_fixed==1){
              if(data_thresh_current_game_time[i,"Cup_2_2.y"]!=""){
                possible_cup_winner=paste(data_thresh_current_game_time[i,"Cup_2_2.y"])
                possible_cup_winner_info=data_thresh_current_season %>% 
                  filter(hometeam ==possible_cup_winner | awayteam==possible_cup_winner, date<=game_time_end) %>% 
                  arrange(-running_date) %>% slice(1) 
                if(nrow(possible_cup_winner_info)==1){
                  if(possible_cup_winner_info$hometeam==possible_cup_winner){
                    MI_EL_spot_fixed=possible_cup_winner_info[,"MI_h_EL_spot_fixed"]
                  } else {
                    MI_EL_spot_fixed=possible_cup_winner_info[,"MI_a_EL_spot_fixed"]
                  }
                  if(MI_EL_spot_fixed==1){
                    away_team_thresholds[,c("EL.y","ELP.y")]=away_team_thresholds[,c("EL.y","ELP.y")]+1
                  } else {
                    EL_change_possible=1
                  }
                }
              } else {
                # if there is no second finalist, then adjust anyways
                away_team_thresholds[,c("EL.y","ELP.y")]=away_team_thresholds[,c("EL.y","ELP.y")]+1
              }
            }
          } else {
            EL_change_possible=1
          }
        }
        # determine the threshold group of the current home team
        home_team_group=min(which(home_team_thresholds >= c(rep(home_team_rank,2),rep(home_team_rank_exclude,2),rep(home_team_rank,3))))
        away_team_group=min(which(away_team_thresholds >= c(rep(away_team_rank,2),rep(away_team_rank_exclude,2),rep(away_team_rank,3))))
        current_game_time_df[i,"MI_h_current_thresh_group"]=home_team_group
        current_game_time_df[i,"MI_a_current_thresh_group"]=away_team_group
        
        # determine the next lower group (sometimes, values are NA as the thresholds do not exist)
        home_team_lower_group=max(1,home_team_group-1)
        while(is.na(home_team_thresholds[home_team_lower_group])){
          home_team_lower_group=home_team_lower_group-1
        }
        away_team_lower_group=max(1,away_team_group-1)
        while(is.na(away_team_thresholds[away_team_lower_group])){
          away_team_lower_group=away_team_lower_group-1
        }
        
        # regular ranking
        current_ranking_h_up=current_ranking
        home_team_rank_up=home_team_rank
        current_ranking_h_down=current_ranking
        home_team_rank_down=home_team_rank
        
        # in case a team has to be excluded
        if(data_thresh_current_game_time[i,"Exclude_from_ranking.x"]!=""){
          if(home_team_group %in% c(3,4)){
            current_ranking_h_down=current_ranking %>% filter(Team !=data_thresh_current_game_time$Exclude_from_ranking.x[i])
            current_ranking_h_down$Position=1:nrow(current_ranking_h_down)
            home_team_rank_down=home_team_rank_exclude
          }
          if(home_team_group %in% c(4,5)){
            current_ranking_h_up=current_ranking %>% filter(Team !=data_thresh_current_game_time$Exclude_from_ranking.x[i])
            current_ranking_h_up$Position=1:nrow(current_ranking_h_up)
            home_team_rank_up=home_team_rank_exclude
          }
        }
        
        # rank of next possible threshold up and down
        next_up_h=which(current_ranking_h_up$Position==home_team_thresholds[[home_team_lower_group]])
        next_down_h=which(current_ranking_h_down$Position==min(N_teams,(home_team_thresholds[[home_team_group]]+1)))
        # differences to thresholds up and down in points per game
        current_game_time_df[i,"MI_h_ppg_diff_up"]=current_ranking_h_up$ppg[next_up_h]-home_team_ppg
        current_game_time_df[i,"MI_h_ppg_diff_down"]=home_team_ppg-current_ranking_h_down$ppg[next_down_h]
        # differences to thresholds up and down in points 
        current_game_time_df[i,"MI_h_points_diff_up"]=min(current_ranking_h_up$Points[1:next_up_h])-current_ranking_h_up$Points[home_team_rank_up]
        current_game_time_df[i,"MI_h_points_diff_down"]=current_ranking_h_down$Points[home_team_rank_down]-current_ranking_h_down$Points[next_down_h]
        # required ppg in future games to pass a threshold up or down
        current_game_time_df[i,"MI_h_ppg_required_up"]=(current_game_time_df[i,"MI_h_points_diff_up"])/(2*(N_teams-1)-current_ranking_h_up$Games[home_team_rank_up])
        current_game_time_df[i,"MI_h_ppg_required_down"]=max((3*(2*(N_teams-1)-current_ranking_h_down$Games[next_down_h:nrow(current_ranking_h_down)])+current_ranking_h_down$Points[next_down_h:nrow(current_ranking_h_down)]-
                                                                current_ranking_h_down$Points[home_team_rank_down])/(2*(N_teams-1)-current_ranking_h_down$Games[home_team_rank_down]))
        if(home_team_group %in% c(4,5)){
          current_game_time_df[i,"MI_h_thresh_fixed"]=as.integer(((min(current_ranking_h_up$Points[1:(next_up_h+EL_change_possible)])-
                                                                     current_ranking_h_up$Points[home_team_rank_up])/
                                                                    (2*(N_teams-1)-current_ranking_h_up$Games[home_team_rank_up]))>3 & current_game_time_df[i,"MI_h_ppg_required_down"]<0)
        } else {
          current_game_time_df[i,"MI_h_thresh_fixed"]=as.integer(current_game_time_df[i,"MI_h_ppg_required_up"]>3 & current_game_time_df[i,"MI_h_ppg_required_down"]<0)
        }        
        # required goal_diff per game to equalize the competitor for next threshold
        current_game_time_df[i,"MI_h_Goal_diff_pgr_up"]=(max(current_ranking_h_up$Goal_diff[current_ranking_h_up$Points==min(current_ranking_h_up$Points[1:next_up_h])])-current_ranking_h_up$Goal_diff[home_team_rank_up])/
          (2*(N_teams-1)-current_ranking_h_up$Games[home_team_rank_up])
        current_game_time_df[i,"MI_h_Goal_diff_pgr_down"]=(current_ranking_h_down$Goal_diff[which.max((3*(2*(N_teams-1)-current_ranking_h_down$Games[next_down_h:nrow(current_ranking_h_down)])+current_ranking_h_down$Points[next_down_h:nrow(current_ranking_h_down)]))+(next_down_h-1)]-
                                                             current_ranking_h_down$Goal_diff[home_team_rank_down])/(2*(N_teams-1)-current_ranking_h_down$Games[home_team_rank_down])
        # relative position in the threshold 
        current_game_time_df[i,"MI_h_rank_rel"]=ifelse(home_team_rank_up==1,1,max(1,as.integer(home_team_rank_down-home_team_thresholds[home_team_lower_group]))/
                                                         (home_team_thresholds[home_team_group]-min(home_team_thresholds[home_team_lower_group],home_team_rank_up)))
        current_game_time_df[i,"MI_h_competitors"]=sum(current_ranking_h_up[current_ranking_h_up$Position<home_team_rank_up,"ppg_min"]<home_team_ppg_max)+
          sum(current_ranking_h_down[current_ranking_h_down$Position>home_team_rank_down,"ppg_max"]>home_team_ppg_min)
        
        # regular ranking
        current_ranking_a_up=current_ranking
        away_team_rank_up=away_team_rank
        current_ranking_a_down=current_ranking
        away_team_rank_down=away_team_rank
        
        # in case a team has to be excluded
        if(data_thresh_current_game_time[i,"Exclude_from_ranking.y"]!=""){
          if(away_team_group %in% c(3,4)){
            current_ranking_a_down=current_ranking %>% filter(Team !=data_thresh_current_game_time$Exclude_from_ranking.y[i])
            current_ranking_a_down$Position=1:nrow(current_ranking_a_down)
            away_team_rank_down=away_team_rank_exclude
          }
          if(away_team_group %in% c(4,5)){
            current_ranking_a_up=current_ranking %>% filter(Team !=data_thresh_current_game_time$Exclude_from_ranking.y[i])
            current_ranking_a_up$Position=1:nrow(current_ranking_a_up)
            away_team_rank_up=away_team_rank_exclude
          }
        }
        
        # rank of next possible threshold up and down
        next_up_a=which(current_ranking_a_up$Position==away_team_thresholds[[away_team_lower_group]])
        next_down_a=which(current_ranking_a_down$Position==min(N_teams,(away_team_thresholds[[away_team_group]]+1)))
        # differences to thresholds up and down in points per game
        current_game_time_df[i,"MI_a_ppg_diff_up"]=current_ranking_a_up$ppg[next_up_a]-away_team_ppg
        current_game_time_df[i,"MI_a_ppg_diff_down"]=away_team_ppg-current_ranking_a_down$ppg[next_down_a]
        # differences to thresholds up and down in points 
        current_game_time_df[i,"MI_a_points_diff_up"]=min(current_ranking_a_up$Points[1:next_up_a])-current_ranking_a_up$Points[away_team_rank_up]
        current_game_time_df[i,"MI_a_points_diff_down"]=current_ranking_a_down$Points[away_team_rank_down]-current_ranking_a_down$Points[next_down_a]
        # required ppg in future games to pass a threshold up or down
        current_game_time_df[i,"MI_a_ppg_required_up"]=current_game_time_df[i,"MI_a_points_diff_up"]/(2*(N_teams-1)-current_ranking_a_up$Games[away_team_rank_up])
        current_game_time_df[i,"MI_a_ppg_required_down"]=max((3*(2*(N_teams-1)-current_ranking_a_down$Games[next_down_a:nrow(current_ranking_a_down)])+current_ranking_a_down$Points[next_down_a:nrow(current_ranking_a_down)]-
                                                                current_ranking_a_down$Points[away_team_rank_down])/(2*(N_teams-1)-current_ranking_a_down$Games[away_team_rank_down]))
        if(away_team_group %in% c(4,5)){
          current_game_time_df[i,"MI_a_thresh_fixed"]=as.integer(((min(current_ranking_a_up$Points[1:(next_up_a+EL_change_possible)])-
                                                                     current_ranking_a_up$Points[away_team_rank_up])/
                                                                    (2*(N_teams-1)-current_ranking_a_up$Games[away_team_rank_up]))>3 & current_game_time_df[i,"MI_a_ppg_required_down"]<0)
        } else {
          current_game_time_df[i,"MI_a_thresh_fixed"]=as.integer(current_game_time_df[i,"MI_a_ppg_required_up"]>3 & current_game_time_df[i,"MI_a_ppg_required_down"]<0)
        }
        # required goal_diff per game to equalize the competitor for next threshold
        current_game_time_df[i,"MI_a_Goal_diff_pgr_up"]=(max(current_ranking_a_up$Goal_diff[current_ranking_a_up$Points==min(current_ranking_a_up$Points[1:next_up_a])])-current_ranking_a_up$Goal_diff[away_team_rank_up])/
          (2*(N_teams-1)-current_ranking_a_up$Games[away_team_rank_up])
        current_game_time_df[i,"MI_a_Goal_diff_pgr_down"]=(current_ranking_a_down$Goal_diff[which.max((3*(2*(N_teams-1)-current_ranking_a_down$Games[next_down_a:nrow(current_ranking_a_down)])+current_ranking_a_down$Points[next_down_a:nrow(current_ranking_a_down)]))+(next_down_a-1)]-
                                                             current_ranking_a_down$Goal_diff[away_team_rank_down])/(2*(N_teams-1)-current_ranking_a_down$Games[away_team_rank_down])
        # relative position in the threshold 
        current_game_time_df[i,"MI_a_rank_rel"]=ifelse(away_team_rank_up==1,1,max(1,as.integer(away_team_rank_down-away_team_thresholds[away_team_lower_group]))/
                                                         (away_team_thresholds[away_team_group]-min(away_team_thresholds[away_team_lower_group],away_team_rank_up)))
        current_game_time_df[i,"MI_a_competitors"]=sum(current_ranking_a_up[current_ranking_a_up$Position<away_team_rank_up,"ppg_min"]<away_team_ppg_max)+
          sum(current_ranking_a_down[current_ranking_a_down$Position>away_team_rank_down,"ppg_max"]>away_team_ppg_min)
        
        
        # size of the threshold
        current_game_time_df[i,"MI_h_rank_range"]=max(1,as.integer(home_team_thresholds[home_team_group]-home_team_thresholds[home_team_lower_group]))
        current_game_time_df[i,"MI_a_rank_range"]=max(1,as.integer(away_team_thresholds[away_team_group]-away_team_thresholds[away_team_lower_group]))
        
        # ppg average of future opponents
        h_opponents=data.frame("Team"=c(unlist(data_thresh_current_season %>% filter((running_date>=current_game_time) & hometeam==home_team_name) %>% dplyr::select(awayteam), use.names=FALSE) ,
                                        unlist(data_thresh_current_season %>% filter((running_date>=current_game_time) & awayteam==home_team_name) %>% dplyr::select(hometeam)), use.names=FALSE))
        current_game_time_df[i,"MI_h_future_opp"]=mean(unlist((left_join(h_opponents, current_ranking %>% dplyr::select(Team,ppg),by="Team"))["ppg"]))
        a_opponents=data.frame("Team"=c(unlist(data_thresh_current_season %>% filter((running_date>=current_game_time) & awayteam==away_team_name) %>% dplyr::select(hometeam), use.names=FALSE) ,
                                        unlist(data_thresh_current_season %>% filter((running_date>=current_game_time) & hometeam==away_team_name) %>% dplyr::select(awayteam)), use.names=FALSE))
        current_game_time_df[i,"MI_a_future_opp"]=mean(unlist((left_join(a_opponents, current_ranking %>% dplyr::select(Team,ppg),by="Team"))["ppg"]))
        
        # Share of games against direct opponents
        current_game_time_df[i,"MI_h_games_competitors"]=sum(h_opponents$Team %in% (current_ranking %>% filter((Position<home_team_rank & ppg_min<home_team_ppg_max) |(Position>home_team_rank & ppg_max>home_team_ppg_min)) %>% dplyr::select(Team))$Team)/nrow(h_opponents)
        current_game_time_df[i,"MI_a_games_competitors"]=sum(a_opponents$Team %in% (current_ranking %>% filter((Position<away_team_rank & ppg_min<away_team_ppg_max) |(Position>away_team_rank & ppg_max>away_team_ppg_min)) %>% dplyr::select(Team))$Team)/nrow(a_opponents)
        
      }
      # update the season data with the MI covariates
      data_thresh_current_season[data_thresh_current_season$running_date==current_game_time,
                                 MI_covariates]=
        current_game_time_df[,MI_covariates]
    }
    # update the general data with the MI covariates
    data_thresh[data_thresh$League_season_index==current_league_season_index,
                c(MI_covariates, "MI_h_international_fixed", "MI_a_international_fixed","MI_h_EL_spot_fixed","MI_a_EL_spot_fixed")]=
      data_thresh_current_season[,c(MI_covariates, "MI_h_international_fixed", "MI_a_international_fixed","MI_h_EL_spot_fixed","MI_a_EL_spot_fixed")]
  }
  return(data_thresh)
  
}
# extract the data for the current season from the test data set
get_season_data=function(data_all, league_season, covariates, split_data){
  additional_variables=c("date","running_date","League","Season","hometeam","awayteam","Home_points","Away_points","fthg","ftag")
  if(split_data){
    if(league_season %in% unique(data_all$MI$X$League_season_index)){
      league_season_data=data_all$MI$X %>% filter(League_season_index==league_season)
    } else if(league_season %in% unique(data_all$ORF$X$League_season_index)){
      league_season_data=data_all$ORF$X %>% filter(League_season_index==league_season)
    } else if(league_season %in% unique(data_all$OOS$X$League_season_index)){
      league_season_data=data_all$OOS$X %>% filter(League_season_index==league_season)
    }
  } else {
    league_season_data=data_all$FULL$X %>% filter(League_season_index==league_season)
  }
  return(league_season_data[,c(covariates,additional_variables)] %>% ungroup())
}
# Calculate the Shannon entropy that is used for the match importance
shannon_entropy=function(prob_density){
  return(-sum(prob_density*log(prob_density),na.rm = T))
}
# Calculate the Match importance with the shannon divergence
match_importance=function(game_day_simulation, weights, team, current_model){
  
  # Get the densities in the areas of the ranking by the thresholds for champion/CL/EL/Relegation
  Pdist_Home=game_day_simulation[[1]][[current_model]][rownames(game_day_simulation[[1]][[current_model]])==team,]
  Pdist_Draw=game_day_simulation[[2]][[current_model]][rownames(game_day_simulation[[2]][[current_model]])==team,]
  Pdist_Away=game_day_simulation[[3]][[current_model]][rownames(game_day_simulation[[3]][[current_model]])==team,]  
  
  return(shannon_entropy(Pdist_Home*weights$Prob_home+Pdist_Draw*weights$Prob_draw+Pdist_Away*weights$Prob_away)-
           sum(shannon_entropy(Pdist_Home)*weights$Prob_home,shannon_entropy(Pdist_Draw)*weights$Prob_draw,shannon_entropy(Pdist_Away)*weights$Prob_away))
}
# Simulate the remaining season from a given matchday on and calculate end-of-season probablities
simulate_season_from_matchday_game=function(MI_model, MI_models, data_season_x, covariates, sim_start_game_time, 
                                            MI_iteration_index, current_league,
                                            N_game_times, N_teams, Team_list, ranking_info_base, 
                                            current_thresholds, Nsim_split_row, game_probs, N_games, 
                                            MI_current_iteration_date){
  # store base values in objects that can be altered in every round
  ranking_info=ranking_info_base
  
  # First game time ---------------------------------------------------------
  
  game_time_summary=dplyr::select(game_probs,Home,Away) %>% ungroup()
  if(Nsim_split_row[,"Game_1"]=="Home"){
    game_time_summary$Points_Home_pred=3
    game_time_summary$Points_Away_pred=0
  } else if(Nsim_split_row[,"Game_1"]=="Draw"){
    game_time_summary$Points_Home_pred=1
    game_time_summary$Points_Away_pred=1
  } else {
    game_time_summary$Points_Home_pred=0
    game_time_summary$Points_Away_pred=3
  }
  
  game_time_summary=game_time_summary %>%
    mutate(count =length(MI_model)*Nsim_split_row$N) %>%
    uncount(count,.id = "path_row") 
  
  # get goal difference according to the results
  total_games_to_simulate=nrow(game_time_summary)
  simulated_goals=data.frame("Home_goals"=rpois(max(total_games_to_simulate,2000),1.55),
                             "Away_goals"=rpois(max(total_games_to_simulate,2000),1.16))
  home_wins=simulated_goals %>% filter(Home_goals>Away_goals)
  home_wins=home_wins[rep_len(1:nrow(home_wins),total_games_to_simulate),]
  draw=simulated_goals %>% filter(Home_goals==Away_goals)
  draw=draw[rep_len(1:nrow(draw),total_games_to_simulate),]
  away_wins=simulated_goals %>% filter(Home_goals<Away_goals)
  away_wins=away_wins[rep_len(1:nrow(away_wins),total_games_to_simulate),]
  game_time_summary[,c("fthg","ftag")]=cbind(ifelse(game_time_summary$Points_Home_pred==3,home_wins$Home_goals,
                                                    ifelse(game_time_summary$Points_Home_pred==1,draw$Home_goals,away_wins$Home_goals)),
                                             ifelse(game_time_summary$Points_Home_pred==3,home_wins$Away_goals,
                                                    ifelse(game_time_summary$Points_Home_pred==1,draw$Away_goals,away_wins$Away_goals)))
  
  # calculate the goal diffference
  game_time_summary=game_time_summary %>% mutate(Goal_diff_sim_home=fthg-ftag, Goal_diff_sim_away=ftag-fthg)
  
  
  game_time_summary$path_row=1:(length(MI_model)*Nsim_split_row$N)
  for(i in length(MI_model)){
    MI_models[[paste0('rows_',MI_model[i])]]=((i-1)*Nsim_split_row$N+1):(i*Nsim_split_row$N)
  }
  
  
  # replicate the information in the storage tables by the number of pathes
  ranking_info=list("points_cum"=as.data.frame(ranking_info$points_cum %>%
                                                 mutate(count =length(MI_model)*Nsim_split_row$N) %>%
                                                 uncount(count,.id = "path_row")),
                    "points_mat"=ranking_info$points_mat %>%
                      mutate(count =length(MI_model)*Nsim_split_row$N) %>%
                      uncount(count,.id = "path_row"),
                    "results_list"=ranking_info$results_list %>%
                      mutate(count =length(MI_model)*Nsim_split_row$N) %>%
                      uncount(count,.id = "path_row"))
  
  # Update the points in the ranking and in the matrix
  Teams_played=unique(c(game_time_summary$Home,game_time_summary$Away))
  ranking_info$results_list=bind_rows(ranking_info$results_list, game_time_summary %>% dplyr::select(Home, Away, fthg, ftag, Points_Home_pred, Points_Away_pred, path_row) %>% 
                                        rename(hometeam=Home, awayteam=Away, Home_points=Points_Home_pred, Away_points=Points_Away_pred))
  ranking_info$points_cum[ranking_info$points_cum$Team %in% Teams_played,"Matchday"]=ranking_info$points_cum[ranking_info$points_cum$Team %in% Teams_played,"Matchday"]+1
  ranking_info$points_cum=left_join(x=left_join(x=ranking_info$points_cum, y=dplyr::select(game_time_summary,Home,Points_Home_pred,Goal_diff_sim_home, path_row), by=c("Team"="Home","path_row"="path_row")),
                                    y=dplyr::select(game_time_summary,Away,Points_Away_pred, Goal_diff_sim_away, path_row), by=c("Team"="Away","path_row"="path_row")) %>%
    mutate(Points=replace_na(Points,0)+replace_na(Points_Home_pred,0)+replace_na(Points_Away_pred,0), 
           Goal_diff=replace_na(Goal_diff,0)+replace_na(Goal_diff_sim_home,0)+replace_na(Goal_diff_sim_away,0)) %>% 
    dplyr::select(-Points_Home_pred, -Points_Away_pred, -Goal_diff_sim_home, -Goal_diff_sim_away)
  points_mat=left_join(left_join(ranking_info$points_mat, game_time_summary %>% dplyr::select(Home, Points_Home_pred, path_row), by=c("Team"="Home", "path_row"="path_row")), 
                       game_time_summary %>% dplyr::select(Away, Points_Away_pred,path_row), by=c("Team"="Away", "path_row"="path_row")) %>% ungroup()
  points_mat[points_mat$Team %in% Teams_played,c("lag1","lag2","lag3","lag4")]=data.frame("lag0"=rowSums(cbind(points_mat$Points_Home_pred[points_mat$Team %in% Teams_played],
                                                                                                               points_mat$Points_Away_pred[points_mat$Team %in% Teams_played]),na.rm=T),
                                                                                          points_mat[points_mat$Team %in% Teams_played,c("lag1","lag2","lag3")])
  ranking_info$points_mat=points_mat %>% dplyr::select(-Points_Home_pred, -Points_Away_pred)
  
  # Remaining game times ----------------------------------------------------
  
  # If last matchday of the season then stop the process otherwise simulate
  if(MI_iteration_index<N_games){
    
    # If the current interaction_index is in a large game day then remove this game from the data set and repeat the game day, 
    # otherwise shift to the next game day
    if(sum(data_season_x$running_date==sim_start_game_time)>1){
      data_season_x=data_season_x %>% filter(iteration_index!=MI_iteration_index)
    } else {
      sim_start_game_time=sim_start_game_time+1
    }
    
    for(SIM_current_game_time in sim_start_game_time:N_game_times){
      data_game_time=data_season_x[which(data_season_x$running_date==SIM_current_game_time),]
      SIM_current_game_time_N_games=nrow(data_game_time)
      data_game_time=data_game_time[rep(1:SIM_current_game_time_N_games,length(MI_model)*Nsim_split_row$N),]
      data_game_time$path_row=rep(1:(length(MI_model)*Nsim_split_row$N), each=SIM_current_game_time_N_games)
      
      # Update the running data based on the past results. 
      
      X_test=data_game_time
      if("H_share_points_lag" %in% colnames(X_test)){
        X_test=left_join(dplyr::select(X_test,-H_share_points_lag), 
                         ranking_info$points_cum %>% mutate(H_share_points_lag=Points/Matchday/3) %>% dplyr::select(Team, path_row, H_share_points_lag),
                         by=c("hometeam"="Team","path_row"="path_row"))
        X_test[is.na(X_test$H_share_points_lag),"H_share_points_lag"]=0
      }
      if("A_share_points_lag" %in% colnames(X_test)){
        X_test=left_join(dplyr::select(X_test,-A_share_points_lag), 
                         ranking_info$points_cum %>% mutate(A_share_points_lag=Points/Matchday/3) %>% dplyr::select(Team, path_row, H_share_points_lag),
                         by=c("awayteam"="Team","path_row"="path_row"))
        X_test[is.na(X_test$H_share_points_lag),"H_share_points_lag"]=0
      }
      
      # Write avg points per game for the last games
      points_mat=ranking_info$points_mat %>% mutate(last_1=lag1/3, last_2=(lag1+lag2)/6, last_3=(lag1+lag2+lag3)/9, last_4=(lag1+lag2+lag3+lag4)/12)
      # Join lag values based on points_mat
      X_test=left_join(left_join(X_test,dplyr::select(points_mat,-lag1,-lag2,-lag3,-lag4), by=c("hometeam"="Team","path_row"="path_row")),
                       dplyr::select(points_mat,-lag1,-lag2,-lag3,-lag4), by=c("awayteam"="Team","path_row"="path_row"))
      # join the current matchday per team based on points_cum
      X_test=left_join(left_join(X_test, ranking_info$points_cum %>% dplyr::select(Team, Matchday, path_row) %>% rename(Matchday_lag=Matchday), by=c("hometeam"="Team","path_row"="path_row")),
                       ranking_info$points_cum %>% dplyr::select(Team, Matchday, path_row) %>% rename(Matchday_lag=Matchday), by=c("awayteam"="Team","path_row"="path_row"))
      for(i in 1:4){
        if(paste0("H_last_",i) %in% colnames(X_test)){
          X_test[,paste0("H_last_",i)]=(X_test$Matchday_lag.x>=i)*X_test[,paste0("last_",i,".x")]+0
        }
        if(paste0("A_last_",i) %in% colnames(X_test)){
          X_test[,paste0("A_last_",i)]=(X_test$Matchday_lag.y>=i)*X_test[,paste0("last_",i,".y")]+0
        }
      }
      
      data_game_time=X_test %>% dplyr::select(-last_1.x, -last_1.y, -last_2.x, -last_2.y,-last_3.x, -last_3.y, -last_4.x, -last_4.y, -Matchday_lag.x, -Matchday_lag.y)
      #Evaluate the matchday, generate an outcome and save the probabilities
      # game_time_summary=evaluate_game_time(to_evaluate_game_time=SIM_current_game_time, 
      #                                      sim_from_game_time=sim_from_game_time, 
      #                                      data_game_time=data_game_time, 
      #                                      MI_model=MI_model, 
      #                                      MI_models=MI_models,
      #                                      exp_points=F, 
      #                                      covariates=covariates) %>% ungroup()
      game_time_predictions=data.frame()
      
      # Predict the probabilities 
      for(current_model in MI_model){
        if(current_model=="probit"){
          current_predictions=predict(MI_models[['probit']], data_game_time[MI_models[['rows_probit']],attr(MI_models[['probit']]$coefficients,"names")], type = "p")
        } else if(current_model=="logit"){
          current_predictions=predict(MI_models[['logit']], data_game_time[MI_models[['rows_logit']],attr(MI_models[['logit']]$coefficients,"names")], type = "p")
        } else {
          if(nrow(data_game_time)==1){
            # somehow the predict function does not work if only one new value is predicted. SO just double the values and take the first prediction
            current_predictions=predict(MI_models[['orf']],newdata=as.matrix(bind_rows(data_game_time[MI_models[['rows_orf']],covariates],data_game_time[MI_models[['rows_orf']],covariates])))
            current_predictions=head(current_predictions$predictions,1)
          } else {
            current_predictions=predict(MI_models[['orf']],newdata=as.matrix(data_game_time[MI_models[['rows_orf']],covariates]))
            current_predictions=current_predictions$predictions
          }
        }
        game_time_predictions=bind_rows(game_time_predictions,
                                        as.data.frame(current_predictions %>%`colnames<-`(c("away", "draw", "home"))))
      }
      
      
      
        # Generate a random outcome based on the probabilities from the ORF predicitions
      random_numbers=runif(nrow(data_game_time))
      game_time_points=data.frame("home_points"=0+(random_numbers>game_time_predictions[,1])+2*(random_numbers>(1-game_time_predictions[,3])),
                                  "away_points"=3-2*(random_numbers>game_time_predictions[,1])-(random_numbers>(1-game_time_predictions[,3])))
      
      game_time_summary=data.frame("Home"=data_game_time$hometeam, 
                                   "Away"=data_game_time$awayteam, 
                                   "Points_Home_True"=data_game_time$Home_points,
                                   "Points_Away_True"=data_game_time$Away_points,
                                   "Prob_Away"=round(game_time_predictions[,1],3),
                                   "Prob_Draw"=round(game_time_predictions[,2],3),
                                   "Prob_Home"=round(game_time_predictions[,3],3),
                                   "Points_Home_pred"=game_time_points[,1],
                                   "Points_Away_pred"=game_time_points[,2]
      )

      # set the goal difference according to the results
      total_games_to_simulate=nrow(game_time_summary)
      simulated_goals=data.frame("Home_goals"=rpois(max(total_games_to_simulate,2000),1.55),
                                 "Away_goals"=rpois(max(total_games_to_simulate,2000),1.16))
      home_wins=simulated_goals %>% filter(Home_goals>Away_goals)
      home_wins=home_wins[rep_len(1:nrow(home_wins),total_games_to_simulate),]
      draw=simulated_goals %>% filter(Home_goals==Away_goals)
      draw=draw[rep_len(1:nrow(draw),total_games_to_simulate),]
      away_wins=simulated_goals %>% filter(Home_goals<Away_goals)
      away_wins=away_wins[rep_len(1:nrow(away_wins),total_games_to_simulate),]
      game_time_summary[,c("fthg","ftag")]=cbind(ifelse(game_time_summary$Points_Home_pred==3,home_wins$Home_goals,
                                                        ifelse(game_time_summary$Points_Home_pred==1,draw$Home_goals,away_wins$Home_goals)),
                                                 ifelse(game_time_summary$Points_Home_pred==3,home_wins$Away_goals,
                                                        ifelse(game_time_summary$Points_Home_pred==1,draw$Away_goals,away_wins$Away_goals)))
      
      # calculate the goal difference
      game_time_summary=game_time_summary %>% mutate(Goal_diff_sim_home=fthg-ftag, Goal_diff_sim_away=ftag-fthg)
      
      
      game_time_summary$path_row=data_game_time$path_row
      
      # Update the points in the ranking and in the matrix
      Teams_played=unique(c(game_time_summary$Home,game_time_summary$Away))
      ranking_info$results_list=bind_rows(ranking_info$results_list, game_time_summary %>% dplyr::select(Home, Away, fthg, ftag, Points_Home_pred, Points_Away_pred, path_row) %>% 
                                            rename(hometeam=Home, awayteam=Away, Home_points=Points_Home_pred, Away_points=Points_Away_pred))
      ranking_info$points_cum[ranking_info$points_cum$Team %in% Teams_played,"Matchday"]=ranking_info$points_cum[ranking_info$points_cum$Team %in% Teams_played,"Matchday"]+1
      ranking_info$points_cum=left_join(x=left_join(x=ranking_info$points_cum, y=dplyr::select(game_time_summary,Home,Points_Home_pred,Goal_diff_sim_home, path_row), by=c("Team"="Home","path_row"="path_row")),
                                        y=dplyr::select(game_time_summary,Away,Points_Away_pred, Goal_diff_sim_away, path_row), by=c("Team"="Away","path_row"="path_row")) %>%
        mutate(Points=replace_na(Points,0)+replace_na(Points_Home_pred,0)+replace_na(Points_Away_pred,0), 
               Goal_diff=replace_na(Goal_diff,0)+replace_na(Goal_diff_sim_home,0)+replace_na(Goal_diff_sim_away,0)) %>% 
        dplyr::select(-Points_Home_pred, -Points_Away_pred, -Goal_diff_sim_home, -Goal_diff_sim_away)
      points_mat=left_join(left_join(ranking_info$points_mat, game_time_summary %>% dplyr::select(Home, Points_Home_pred, path_row), by=c("Team"="Home", "path_row"="path_row")), 
                           game_time_summary %>% dplyr::select(Away, Points_Away_pred,path_row), by=c("Team"="Away", "path_row"="path_row")) %>% ungroup()
      points_mat[points_mat$Team %in% Teams_played,c("lag1","lag2","lag3","lag4")]=data.frame("lag0"=rowSums(cbind(points_mat$Points_Home_pred[points_mat$Team %in% Teams_played],
                                                                                                                   points_mat$Points_Away_pred[points_mat$Team %in% Teams_played]),na.rm=T),
                                                                                              points_mat[points_mat$Team %in% Teams_played,c("lag1","lag2","lag3")])
      ranking_info$points_mat=points_mat %>% dplyr::select(-Points_Home_pred, -Points_Away_pred)
      
    }
  }
  
  # Get the ranking ----------------------------------------------------
  
  #ranking_info$points_cum=set_ranking(ranking_info, current_league, N_teams) %>% ungroup()
  # Initialize empty additional ranking variables
  points_cum = ranking_info$points_cum %>% mutate(Goal_scored=0, Away_goals=0, H2H_Goals=0, H2H_Points=0, H2H_Goal_diff=0, H2H_Away_goals=0, Matches_won=0)
  
  # identify which teams are still in competition
  equal_points_teams=points_cum %>% group_by(path_row, Points) %>% summarize(Teams=paste(Team, collapse = ","), .groups="drop")
  points_cum=left_join(points_cum, equal_points_teams, by=c("Points","path_row")) 
  points_cum=suppressWarnings(separate(data=points_cum,col="Teams", into=paste("Teams",1:10,sep="_"), sep=","))
  number_of_na_cases=apply(points_cum[paste("Teams",1:10,sep="_")],2,function(x) sum(is.na(x)))
  points_cum=points_cum %>% dplyr::select(-paste("Teams",1:10,sep="_")[number_of_na_cases==nrow(points_cum)])
  
  columns_to_check=colnames(points_cum)[grep("Teams",colnames(points_cum))]
  for(current_column in columns_to_check){
    points_cum_current=points_cum[!is.na(points_cum[,current_column]) & points_cum$Team!=points_cum[,current_column],] 
    points_cum_current_total=bind_rows(left_join(points_cum_current, ranking_info$results_list %>% mutate(venue="Home") %>% 
                                                   rename(opponent=awayteam,goals_for=fthg, goals_against=ftag, points_for=Home_points,points_against=Away_points),
                                                 by=c("Team"="hometeam","path_row"="path_row")),
                                       left_join(points_cum_current, ranking_info$results_list %>% mutate(venue="Away") %>% 
                                                   rename(opponent=hometeam,goals_for=ftag, goals_against=fthg, points_for=Away_points,points_against=Home_points),
                                                 by=c("Team"="awayteam","path_row"="path_row")))
    
    Intermediate_1=points_cum_current_total %>% group_by(Team, path_row) %>% summarize(Goal_scored=sum(goals_for), Matches_won=sum(points_for==3), .groups="drop")
    Intermediate_2=points_cum_current_total %>% filter(venue=="Away")%>% group_by(Team, path_row) %>% summarize(Away_goals=sum(goals_for), .groups="drop")
    
    points_cum_current_base=bind_rows(left_join(points_cum_current %>% rename(col_to_join=!!current_column), 
                                                ranking_info$results_list %>% mutate(venue="Home") %>% 
                                                  rename(opponent=awayteam,goals_for=fthg, goals_against=ftag, points_for=Home_points,points_against=Away_points),
                                                by=c("Team"="hometeam","path_row"="path_row", "col_to_join"="opponent")),
                                      left_join(points_cum_current %>% rename(col_to_join=!!current_column),ranking_info$results_list %>% mutate(venue="Away") %>% 
                                                  rename(opponent=hometeam,goals_for=ftag, goals_against=fthg, points_for=Away_points,points_against=Home_points),
                                                by=c("Team"="awayteam","path_row"="path_row", "col_to_join"="opponent")))
    Intermediate_3=points_cum_current_base %>% group_by(Team, path_row) %>% 
      summarize(H2H_Points=sum(points_for), H2H_Goals=sum(goals_for), H2H_Goal_diff=sum(goals_for)-sum(goals_against), .groups="drop")
    Intermediate_4=points_cum_current_base %>% filter(venue=="Away") %>% group_by(Team, path_row) %>% summarize(H2H_Away_goals=sum(goals_for), .groups="drop")
    points_cum_current=left_join(left_join(left_join(Intermediate_1, Intermediate_2, by=c("Team","path_row")), 
                                           Intermediate_3, by=c("Team","path_row")), 
                                 Intermediate_4, by=c("Team","path_row"))
    points_cum=left_join(points_cum, points_cum_current, by=c("Team","path_row"))
    points_cum %>% dplyr::select(Goal_scored.x,Goal_scored.y) %>% rowSums(na.rm=TRUE) -> points_cum$Goal_scored
    points_cum %>% dplyr::select(Away_goals.x,Away_goals.y) %>% rowSums(na.rm=TRUE) -> points_cum$Away_goals
    points_cum %>% dplyr::select(H2H_Goals.x,H2H_Goals.y) %>% rowSums(na.rm=TRUE) -> points_cum$H2H_Goals
    points_cum %>% dplyr::select(H2H_Points.x,H2H_Points.y) %>% rowSums(na.rm=TRUE) -> points_cum$H2H_Points
    points_cum %>% dplyr::select(H2H_Goal_diff.x,H2H_Goal_diff.y) %>% rowSums(na.rm=TRUE) -> points_cum$H2H_Goal_diff
    points_cum %>% dplyr::select(H2H_Away_goals.x,H2H_Away_goals.y) %>% rowSums(na.rm=TRUE) -> points_cum$H2H_Away_goals
    points_cum %>% dplyr::select(Matches_won.x,Matches_won.y) %>% rowSums(na.rm=TRUE) -> points_cum$Matches_won
    points_cum = points_cum %>% dplyr::select(-Goal_scored.x, -Away_goals.x, -H2H_Goals.x, -H2H_Points.x, -H2H_Goal_diff.x, -H2H_Away_goals.x, -Matches_won.x,
                                              -Goal_scored.y, -Away_goals.y, -H2H_Goals.y, -H2H_Points.y, -H2H_Goal_diff.y, -H2H_Away_goals.y, -Matches_won.y)
  }
  
  if(current_league %in% c("ED","BL","PL")){
    points_cum$Position=rank(order(order(points_cum$path_row, -points_cum$Points,-points_cum$Goal_diff,-points_cum$H2H_Points, -points_cum$H2H_Goal_diff, -points_cum$H2H_Away_goals)))
  }
  if(current_league %in% c("SA","LL")){
    points_cum$Position=rank(order(order(points_cum$path_row, -points_cum$Points,-points_cum$H2H_Points, -points_cum$H2H_Goal_diff,-points_cum$Goal_diff,-points_cum$Goal_scored)))
  }
  if(current_league %in% c("PO")){
    points_cum$Position=rank(order(order(points_cum$path_row, -points_cum$Points,-points_cum$H2H_Points,-points_cum$H2H_Goal_diff,-points_cum$H2H_Away_goals,
                                         -points_cum$Goal_diff,-points_cum$Matches_won, -points_cum$Goal_scored)))
  }
  if(current_league %in% c("L1")){
    points_cum$Position=rank(order(order(points_cum$path_row, -points_cum$Points,-points_cum$Goal_diff,-points_cum$H2H_Points,-points_cum$H2H_Goal_diff,
                                         -points_cum$H2H_Goals,-points_cum$H2H_Away_goals, -points_cum$Goal_scored,-points_cum$Away_goals)))
  }
  # Correct the ranks to 1:N_teams
  points_cum$Position=(points_cum$Position-1)%%N_teams+1
  ranking_info$points_cum= points_cum %>% dplyr::select(Team, Position, Matchday, Points, Goal_diff, path_row) %>% ungroup()
  
  
  #Team_ranks=get_rank_table(MI_model, MI_models, current_thresholds, MI_current_iteration_date, Team_list, ranking_info)
  # Find the relevant threshold setting for the current team at the specified tim
  relevant_thresholds = current_thresholds %>% filter(Date<MI_current_iteration_date) %>% arrange(Date) %>% group_by(Team) %>% slice(n()) %>% ungroup()
  
  points_cum=left_join(ranking_info$points_cum %>% mutate(threshold_type=ifelse(Team %in% relevant_thresholds$Team,Team,"Regular")),
                       relevant_thresholds,
                       by=c("threshold_type"="Team"))
  
  for(join_column in c("Cup_1_1", "Cup_1_2" ,"Cup_2_1", "Cup_2_2", "Possible_direct","Exclude_from_ranking")){
    points_cum=suppressMessages(left_join(points_cum, 
                                          points_cum %>% 
                                            dplyr::select(Team, path_row,Position)%>%
                                            `colnames<-`(c(join_column, "path_row", paste0(join_column,"_rank")))))
  }
  # If cup EL teams are already qualified
  points_cum[,c("EL","ELP")]=points_cum[,c("EL","ELP")]+
    (replace_na(points_cum$Cup_1_1_rank<=
                  (points_cum[,c("EL","ELP")]+
                     (replace_na(points_cum$Cup_2_1_rank<=points_cum[,c("EL","ELP")],T)&
                        replace_na(points_cum$Cup_2_2_rank<=points_cum[,c("EL","ELP")],T)&
                        (!(is.na(points_cum$Cup_2_1_rank)&is.na(points_cum$Cup_2_2_rank))))),T)&
       replace_na(points_cum$Cup_1_2_rank<=
                    (points_cum[,c("EL","ELP")]+
                       (replace_na(points_cum$Cup_2_1_rank<=points_cum[,c("EL","ELP")],T)&
                          replace_na(points_cum$Cup_2_2_rank<=points_cum[,c("EL","ELP")],T)&
                          (!(is.na(points_cum$Cup_2_1_rank)&is.na(points_cum$Cup_2_2_rank))))),T)&
       (!(is.na(points_cum$Cup_1_1_rank)&is.na(points_cum$Cup_1_2_rank))))+
    (replace_na(points_cum$Cup_2_1_rank<=
                  (points_cum[,c("EL","ELP")]+
                     (replace_na(points_cum$Cup_1_1_rank<=points_cum[,c("EL","ELP")],T)&
                        replace_na(points_cum$Cup_1_2_rank<=points_cum[,c("EL","ELP")],T)&
                        (!(is.na(points_cum$Cup_1_1_rank)&is.na(points_cum$Cup_1_2_rank))))),T)&
       replace_na(points_cum$Cup_2_2_rank<=
                    (points_cum[,c("EL","ELP")]+
                       (replace_na(points_cum$Cup_1_1_rank<=points_cum[,c("EL","ELP")],T)&
                          replace_na(points_cum$Cup_1_2_rank<=points_cum[,c("EL","ELP")],T)&
                          (!(is.na(points_cum$Cup_1_1_rank)&is.na(points_cum$Cup_1_2_rank))))),T)&
       (!(is.na(points_cum$Cup_2_1_rank)&is.na(points_cum$Cup_2_2_rank))))
  # Remove banned teams from EL
  points_cum[,c("CL","EL","ELP")]=points_cum[,c("CL","EL","ELP")]+
    replace_na(points_cum$Exclude_from_ranking_rank<=points_cum[,c("CL","EL","ELP")],F)
  # Possible runner-up qualification
  points_cum[,c("EL","ELP")]=bind_cols(
    "EL"=ifelse(is.na(points_cum$Possible_direct_rank)|is.na(points_cum$EL),points_cum$EL,
                ifelse(points_cum$Possible_direct_rank<=points_cum$EL,points_cum$NO,points_cum$EL)),
    "ELP"=ifelse(is.na(points_cum$Possible_direct_rank)|is.na(points_cum$ELP),points_cum$ELP,
                 ifelse(points_cum$Possible_direct_rank<=points_cum$EL,points_cum$NO,points_cum$ELP))
  )
  
  # Get the corresponding threshold
  possible_thresholds=c("CH","CL","EL","ELP","NO","REP","RE")
  points_cum$Threshold=possible_thresholds[max.col(as.matrix(points_cum$Position<=(points_cum[,possible_thresholds] %>% mutate(across(everything(),~replace_na(.,0))))), "first")]
  
  #clean the results
  Team_ranks=list()
  for(current_model in MI_model){
    
    current_Team_ranks=table(points_cum %>% filter(path_row %in% MI_models[[paste0('rows_',current_model)]]) %>% dplyr::select(Team,Threshold))
    current_Team_ranks=as.data.frame.matrix(current_Team_ranks[match(Team_list, rownames(current_Team_ranks)),])
    Team_ranks[[current_model]]=current_Team_ranks/rowSums(current_Team_ranks)
  }
  # test=points_cum %>% group_by(path_row, Threshold) %>% summarize(n=n(), .groups = "drop")
  # test=test %>% pivot_wider(id_cols = "path_row", names_from = "Threshold",values_from = "n")
  # colSums(Team_ranks[[1]])*Nsim_split_row$N
  # colSums(Team_ranks[[2]])*Nsim_split_row$N
  rm(list=ls()[ls()!="Team_ranks"])
  gc()
  return(Team_ranks)
}
# Calculate the Match Importance values by simulation
SIM_MI=function(path_to_data, dat_name, selected_leagues, selected_seasons, share_orf, share_oos, 
                N_simulation, covariates, feature_selection=NA, feature_selection_seasons, 
                add_league_dummies, add_season_dummies, sample.fraction=0.67, min.node.size=10, 
                mtry=10, seed=1127, reload_data=F, num.trees=1000, split_data=F){
  
  # Testing parameters ------------------------------------------------------
  
  # Run parameters for testing
  # path_to_data="../data/"
  # dat_name="all_leagues.xls"
  # selected_leagues= c("BL","ED","LL","L1","PL","PO","SA")
  # selected_seasons= 2006:2018
  # feature_selection="Lasso"
  # feature_selection_seasons=2006:2008
  # add_season_dummies=F
  # add_league_dummies=F
  # covariates=covariates_long
  # N_simulation=7500
  # num.trees=1000
  # sample.fraction= 0.67
  # min.node.size= 5
  # mtry= 5
  # seed=1127
  # reload_data=F
  # split_data=F
  # N_iterations=2
  # MI_model=c("orf")
  # N_clusters=6
  # current_iteration=2
  # 
  # MI_iteration_index=380
  # sim_from_md=1
  # current_game=1
  # league_season=59
  # league_season_iterator=22
  # SIM_current_game_time=234
  # sim_from_game_time=235
  
  load("data_all_MI.Rdata")
  load("MI_all_seasons.Rdata")
  data_all$FULL$X = data_all$FULL$X %>%
    mutate(MI_home_orf=MI_home_iter_3_orf,
           MI_away_orf=MI_away_iter_3_orf)
  covariates=c(covariates,paste0(c("MI_home_","MI_away_"),MI_model[1]))
  
  # Load and initialize ------------------------------------------------------
  
  set.seed(seed)
  
  if(reload_data){
    # Read the threshold positions
    threshold_positions=read.csv(file=paste0(path_to_data,"Threshold_positions.csv"),
                                 sep = ",", 
                                 header = T, 
                                 blank.lines.skip = T, 
                                 fill = T, 
                                 stringsAsFactors = F,
                                 fileEncoding = "UTF-8-BOM" )
    threshold_positions= threshold_positions %>% mutate(Date=as.Date(Date,format="%d.%m.%Y"))
    
    # Load the data
    data_all=load_data(path_to_data=path_to_data,
                       dat_name=dat_name, 
                       selected_leagues=selected_leagues, 
                       selected_seasons=selected_seasons, 
                       covariates=covariates_long,
                       threshold_positions=threshold_positions,
                       feature_selection = feature_selection,
                       feature_selection_seasons = feature_selection_seasons,
                       seed = seed,
                       add_league_dummies=add_league_dummies,
                       add_season_dummies=add_season_dummies,
                       split_data = split_data)
    
    save(threshold_positions, data_all, file="data_all.Rdata")
  }  else {
    load("data_all.Rdata")
  }
  rm(add_league_dummies, add_season_dummies, dat_name, feature_selection, feature_selection_seasons, 
     path_to_data, reload_data,selected_leagues, selected_seasons)
  # update covariates after possible feature selection
  covariates=data_all$covariates
  
  # Time for loading: 30min 
  
  for(current_iteration in 1:N_iterations){
    set.seed(seed+current_iteration)
    
    # output path for current iteration
    current_path=paste0("iter_",current_iteration,"/")
    dir.create(current_path, showWarnings = FALSE)
    
    
    # Add MI to covariates after the first iteration
    if(current_iteration==2){
      covariates=c(covariates,paste0(c("MI_home_","MI_away_"),MI_model[1]))
    }
    
    # extract data for model construction
    if(split_data){
      model_data=data_all$ORF
    } else {
      model_data=data_all$FULL
    }
    
    # Fit the models
    MI_models=list()
    for(current_model in MI_model){
      if(current_model=="logit"){
        logit_formula=as.formula(paste("Y ~ ", paste(covariates, collapse= "+")))
        logit_model <- polr(logit_formula, 
                            data = bind_cols("Y"=as.factor(model_data$Y),
                                             model_data$X[,covariates]),
                            method = c("logistic"),
                            Hess = T)
        MI_models[["logit"]]=logit_model
        rm(logit_model)
      } else if (current_model=="probit"){
        
        
        probit_formula=as.formula(paste("Y ~ ", paste(covariates, collapse= "+")))
        probit_model <- polr(probit_formula, 
                             data = bind_cols("Y"=as.factor(model_data$Y),
                                              model_data$X[,covariates]),
                             method = c("probit"),
                             Hess = T)
        MI_models[["probit"]]=probit_model
        rm(probit_model)
      } else {
        model_indices=which(!is.na(model_data$X[,covariates[length(covariates)]]))
        orf_model <- orf(as.matrix(model_data$X[model_indices,covariates]), 
                         model_data$Y[model_indices], 
                         num.trees=num.trees,
                         honesty=F,
                         sample.fraction = sample.fraction,
                         min.node.size = min.node.size,
                         mtry=mtry, 
                         importance=F)
        MI_models[["orf"]]=orf_model
        rm(orf_model, model_indices)
      }
    }
    rm(model_data)
    
    #League_season information
    if(split_data){
      MI_league_seasons=c(unique(data_all$MI$X$League_season_index),unique(data_all$OOS$X$League_season_index))
      N_league_season=length(MI_league_seasons)  
    } else {
      MI_league_seasons=unique(data_all$FULL$X$League_season_index)
      N_league_season=length(MI_league_seasons)  
    }
    
    SIM_start_time_outer=Sys.time()
    clusters <- makeCluster(N_clusters)
    registerDoSNOW(clusters)
    
    # Loop over all seasons ---------------------------------------------------
    MI_all_seasons=foreach(league_season_iterator = 1:20, 
                           .combine = "rbind", 
                           .inorder = F, 
                           .packages = c("orf","dplyr","tidyr","MASS"), 
                           .maxcombine = N_clusters,
                           .export = c("get_season_data",
                                       "shannon_entropy",
                                       "simulate_season_from_matchday_game")
    ) %dopar% {
      MI_results=data.frame()
      SIM_start_time_league=Sys.time()
      
      league_season=MI_league_seasons[league_season_iterator]
      # Get the data for the current season
      
      data_season_x_cln=get_season_data(data_all, league_season, covariates, split_data) %>%
        arrange(running_date) %>% 
        ungroup()
      
      # Season information
      current_league=data_season_x_cln$League[1]
      current_season=data_season_x_cln$Season[1]
      current_thresholds=threshold_positions %>% filter(League==current_league, Season==current_season)
      
      # Generate an unique iteration index and find the min iteration index for every running date
      data_season_x_cln$iteration_index=1:nrow(data_season_x_cln)
      large_game_times=which(table(data_season_x_cln$running_date)>1)
      large_game_times_min_index=data_season_x_cln[data_season_x_cln$running_date %in% large_game_times,c("running_date","iteration_index")] %>% 
        group_by(running_date) %>% summarize(min_iteration_index=min(iteration_index), .groups="drop")
      large_game_times_min_index=left_join(data_season_x_cln %>% dplyr::select(running_date, iteration_index), large_game_times_min_index, by="running_date")
      rm(large_game_times)
      
      N_game_times=max(data_season_x_cln$running_date)
      N_games=max(data_season_x_cln$iteration_index)
      N_teams=length(unique(data_season_x_cln$hometeam))
      Team_list=unique(data_season_x_cln$hometeam)
      Nsim_split_store=list()
      
      cat(sprintf("%s  -  League %s  -  Season %s started \n",Sys.time(),current_league,current_season), 
          file=paste0(current_path,"outfile_",format(SIM_start_time_outer,"%Y_%m_%d_%H_%M"),"_",league_season,".txt"), append=T)
      
      
      # Loop over all game times ------------------------------------------------
      
      for(MI_iteration_index in N_games:(N_games-20)){
        
        # Initialize gametime -----------------------------------------------------
        original_MI_iteration_index=MI_iteration_index
        #If the first part of a large game time is met, then change the ordering
        if(!is.na(large_game_times_min_index$min_iteration_index[MI_iteration_index])){
          to_change_index=large_game_times_min_index$min_iteration_index[MI_iteration_index]
          data_season_x_temp=data_season_x_cln %>% mutate(iteration_index=ifelse(iteration_index==!!MI_iteration_index,to_change_index,
                                                                                 ifelse(iteration_index==!!to_change_index,MI_iteration_index,iteration_index)))
          MI_iteration_index=to_change_index
        } else {
          data_season_x_temp=data_season_x_cln
        }
        
        MI_current_iteration_data=data_season_x_temp %>% filter(iteration_index==MI_iteration_index)
        MI_current_iteration_date=MI_current_iteration_data$date[1]
        # Get the predictions for the games of initial gametime
        game_probs=data.frame("Home"=MI_current_iteration_data$hometeam,
                              "Away"=MI_current_iteration_data$awayteam,
                              "True_result"=ifelse(MI_current_iteration_data$Home_points==3,"Home",
                                                   ifelse(MI_current_iteration_data$Home_points==1,"Draw","Away")))
        for(current_model in MI_model){
          if(current_model=="probit"){
            game_probs_model=predict(MI_models[["probit"]], MI_current_iteration_data[,attr(MI_models[["probit"]]$coefficients,"names")], type = "p")
          } else if(current_model=="logit"){
            game_probs_model=predict(MI_models[["logit"]], MI_current_iteration_data[,attr(MI_models[["logit"]]$coefficients,"names")], type = "p")
          } else {
            # some how the predict function of orf does not work for a single row.
            game_probs_model=predict(MI_models[["orf"]],newdata=as.matrix(bind_rows(MI_current_iteration_data[covariates],
                                                                                    MI_current_iteration_data[covariates])))$predictions[1,]
          } 
          game_probs[,paste0('Prob_home_',current_model)]=game_probs_model[3]
          game_probs[,paste0('Prob_draw_',current_model)]=game_probs_model[2]
          game_probs[,paste0('Prob_away_',current_model)]=game_probs_model[1]
        }
        
        # Get the standings at current game time ----------------------------------
        
        # Update the points in the ranking and in the matrix
        if(MI_iteration_index>1){
          results_list=data_season_x_temp %>% filter(iteration_index<MI_iteration_index) %>% dplyr::select(hometeam, awayteam, fthg, ftag, running_date, Home_points, Away_points)
          intermediate_save=bind_rows(left_join(data.frame("Team"=Team_list), dplyr::select(results_list,-Away_points), by=c("Team"="hometeam")),
                                      left_join(data.frame("Team"=Team_list), dplyr::select(results_list,-Home_points), by=c("Team"="awayteam"))) %>% rowwise() %>%
            mutate(Points=ifelse(is.na(hometeam),Home_points,Away_points),
                   Goal_diff=ifelse(is.na(hometeam),fthg-ftag,ftag-fthg)) %>% dplyr::select(-Home_points, -Away_points)
          points_cum=intermediate_save %>% group_by(Team) %>% summarize(Position=0, Matchday=n()-sum(is.na(fthg)), Points=sum(Points,na.rm = T), Goal_diff=sum(Goal_diff, na.rm = T), .groups="drop")
          points_cum=points_cum[match(Team_list,points_cum$Team),]
          points_mat=intermediate_save %>% mutate(running_date=ifelse(is.na(running_date),0,running_date)) %>% group_by(Team) %>% top_n(4,running_date) %>%
            arrange(-running_date) %>% mutate(lag=1:n()) %>% 
            mutate(lag1=ifelse(lag==1,Points,0),lag2=ifelse(lag==2,Points,0),lag3=ifelse(lag==3,Points,0),lag4=ifelse(lag==4,Points,0)) %>%
            summarize(lag1=sum(lag1, na.rm = T), lag2=sum(lag2, na.rm = T),lag3=sum(lag3, na.rm = T), lag4=sum(lag4, na.rm = T), .groups="drop")                                                                            
          points_mat=points_mat[match(Team_list,points_mat$Team),]
          results_list=results_list %>% dplyr::select(-running_date)
          rm(intermediate_save)
        } else {
          # cumulated points/standings at current matchday
          points_cum =data.frame("Team"=Team_list, "Position"=0,"Matchday"=0,"Points"=0,"Goal_diff"=0, stringsAsFactors = F)
          # Used for last results to generate form
          points_mat <- data.frame("Team"=Team_list, "lag1"=0, "lag2"=0, "lag3"=0, "lag4"=0, stringsAsFactors = F)
          # long dataframe of all results in the season
          results_list=data.frame()
        }
        
        if(current_league=="L1" & current_season=="2013" & MI_iteration_index>142){
          points_cum[points_cum$Team=="Bastia",c("Points","Goal_diff")]=points_cum[points_cum$Team=="Bastia",c("Points","Goal_diff")]+c(3,2)
          points_cum[points_cum$Team=="Nantes",c("Points","Goal_diff")]=points_cum[points_cum$Team=="Nantes",c("Points","Goal_diff")]-c(3,2)
        }
        
        ranking_info_base=list("points_cum"=points_cum, "points_mat"=points_mat, "results_list"=results_list)
        rm(points_cum, points_mat, results_list)
        
        # Get the required number of simulations pre game time split --------------
        Nsim_split=data.frame("Game_1"=c("Home","Draw","Away"),"N"=c(N_simulation/3,N_simulation/3,N_simulation/3),
                              "Prob"=as.numeric(game_probs[1,paste0(c("Prob_home_","Prob_draw_","Prob_away_"),MI_model[1])]),
                              "True_path"=(c("Home","Draw","Away")==game_probs$True_result[1]))
        game_day_simulation=list()  
        
        for(game_tree_row in 1:nrow(Nsim_split)){
          # if its the true path get the results from the previous simulation
          # Check that it is not a large game time, in this case we cannot use the old result
          if((Nsim_split[game_tree_row,"True_path"]) & (MI_iteration_index!= N_games) & 
             is.na(large_game_times_min_index$min_iteration_index[MI_iteration_index])){
            game_day_simulation[[game_tree_row]]=Nsim_split_store
          } else {
            game_day_simulation[[game_tree_row]]=simulate_season_from_matchday_game(MI_model=MI_model,
                                                                                    MI_models=MI_models,
                                                                                    data_season_x=data_season_x_temp,
                                                                                    covariates=covariates,
                                                                                    sim_start_game_time=data_season_x_cln$running_date[MI_iteration_index],
                                                                                    MI_iteration_index=MI_iteration_index,
                                                                                    current_league = current_league,
                                                                                    N_game_times=N_game_times,
                                                                                    N_teams=N_teams,
                                                                                    Team_list=Team_list,
                                                                                    ranking_info_base=ranking_info_base,
                                                                                    current_thresholds=current_thresholds,
                                                                                    Nsim_split_row=Nsim_split[game_tree_row,],
                                                                                    game_probs=game_probs,
                                                                                    N_games=N_games,
                                                                                    MI_current_iteration_date=MI_current_iteration_date)
          }
        }
        
        # Get MI for game ---------------------------------------------------------
        
        MI_data=MI_current_iteration_data[,c("League","Season","hometeam","awayteam")]
        MI_data$Matchday_home=ranking_info_base$points_cum$Matchday[match(MI_current_iteration_data$hometeam,ranking_info_base$points_cum$Team)]+1
        MI_data$Matchday_away=ranking_info_base$points_cum$Matchday[match(MI_current_iteration_data$awayteam,ranking_info_base$points_cum$Team)]+1
        
        # Set the weights for the shannon distance either to the estimated probabilities or set them equally
        for(current_model in MI_model){
          MI_data[,paste0('MI_home_',current_model)]=match_importance(game_day_simulation,
                                                                      weights=game_probs[,paste0(c("Prob_home_","Prob_draw_","Prob_away_"),current_model)],
                                                                      team=MI_data$hometeam,
                                                                      current_model=current_model)
          MI_data[,paste0('MI_away_',current_model)]=match_importance(game_day_simulation,
                                                                      weights=game_probs[,paste0(c("Prob_home_","Prob_draw_","Prob_away_"),current_model)],
                                                                      team=MI_data$awayteam,
                                                                      current_model=current_model)
        }
        
        MI_results=rbind(MI_results,MI_data)
        
        for(current_model in MI_model){
          
          Nsim_split_store[[current_model]]=Reduce("+", lapply(1:3,function(j) {
            game_day_simulation[[j]][[current_model]]*game_probs[1,paste0(c('Prob_home_','Prob_draw_','Prob_away_')[j],current_model)]}))
        }      
        
        if(original_MI_iteration_index%%10==0){
          cat(sprintf("%s  -  League %s  -  Season %s  -  Gametime %s done \n",Sys.time(),current_league,current_season,original_MI_iteration_index), 
              file=paste0(current_path,"outfile_",format(SIM_start_time_outer,"%Y_%m_%d_%H_%M"),"_",league_season,".txt"), append=T)
        }
        rm(data_season_x_temp, game_day_simulation, game_probs, MI_current_iteration_data, MI_data, 
           Nsim_split, ranking_info_base, game_probs_model, game_tree_row)
        rm(current_model, MI_current_iteration_date)
        gc()
      }
      cat(sprintf("%s  -  League %s  -  Season %s completed  - %s of %s \n",Sys.time(),current_league,current_season,league_season_iterator,length(MI_league_seasons)), 
          file=paste0(current_path,"outfile_",format(SIM_start_time_outer,"%Y_%m_%d_%H_%M"),"_",league_season,".txt"), append=T)
      write.csv(MI_results, file=paste0(current_path,"MI_all_seasons_",league_season,".csv"), row.names = F)
      rm(current_thresholds, data_season_x_cln, large_game_times_min_index, Nsim_split_store, to_change_index)
      rm(current_league, current_season, Team_list)
      gc()
      MI_results
    }
    stopCluster(clusters) 
    rm(clusters)
    
    #write results to a file
    write.csv(MI_all_seasons, file=paste0(current_path,"MI_all_seasons.csv", row.names = F))
    
    # Update the covariates with the MI variables
    if(split_data){
      data_all$MI$X=left_join(data_all$MI$X %>% dplyr::select(-starts_with(c("MI_home","MI_away"))), 
                              MI_all_seasons %>% dplyr::select(-Matchday_home, -Matchday_away), 
                              by=c("League","Season","hometeam","awayteam"))
      data_all$OOS$X=left_join(data_all$OOS$X %>% dplyr::select(-starts_with(c("MI_home","MI_away"))), 
                               MI_all_seasons %>% dplyr::select(-Matchday_home, -Matchday_away), 
                               by=c("League","Season","hometeam","awayteam"))
    } else {
      data_all$FULL$X=left_join(data_all$FULL$X %>% dplyr::select(-starts_with(c("MI_home","MI_away"))), 
                                MI_all_seasons %>% dplyr::select(-Matchday_home, -Matchday_away), 
                                by=c("League","Season","hometeam","awayteam"))
    }
  }
  
  return(MI_all_seasons)
}

# Estimation of Poisson distribution --------------------------------------

# use dat_all from within load_data
test=data.frame("n"=dat_all %>% group_by(fthg) %>% summarize(n=n(), .groups = 'drop') %>% dplyr::select(n), "value"=0:10)
gf <- goodfit(test, "poisson")
plot(gf, type = "standing", scale = "raw")
gf$par
# Home lambda=1.55

test=data.frame("n"=dat_all %>% group_by(ftag) %>% summarize(n=n(), .groups = 'drop') %>% dplyr::select(n), "value"=0:9)
gf <- goodfit(test, "poisson")
plot(gf, type = "standing", scale = "raw")
gf$par
# Away lambda=1.16




