library(tidyverse)
library(skimr)
library(clipr)
library(sf)
######Define metrics

lumets<-read_csv("Data/metric_lookup.csv") %>%
  filter(MetricSubtype!="Direct") %>%
  # filter(West)
  filter(West|AW|WM)

#############Import data
gis_metrics_df<-read_csv("Data/GISmetrics/COMPLETE_gis_metrics_df_08012023.csv")

main_df_nopnw<-read_csv("NotForGit/Step1/main_df_step1.csv") %>%
  #Temporary fix while dupes are resolved
  group_by(SiteCode, CollectionDate) %>% slice_head(n=1) %>% ungroup()



# pnw_df_all<-read_csv("NotForGit/Step1/pnw_df_step1.csv") %>%
#   mutate(Database="PNW",
#          Region_DB="PNW",
#          SiteCode=as.character(SiteCode)) 
# 
# pnw_df<-pnw_df_all %>%
#   na.omit()

xwalk_df<-read_csv("NotForGit/Step1/xwalk_df.csv")


xwalk_west_df<-xwalk_df %>%
  # filter(Region %in% c("West","PNW"))  %>%
  filter(Region %in% c("West"))  %>%
  rename(SiteCode=sitecode) %>%
  inner_join(gis_metrics_df) %>%
  mutate(
    snow_strat = case_when(MeanSnowPersistence_10 < 25~"Not snow influenced",
                           T~"Snow influenced"),
    drainage_area_strat = case_when(DRNAREA_mi2<5~"Small",
                                    T~"Large"),
    precip_strat = case_when(ppt.567<30~"LessThanMedian",
                             T~"GreaterThanMedian"),
    ohwm_region2 = case_when(ohwm_region %in% c("Southern Plains","Southwest")~"South",
                             ohwm_region %in% c("Northern Plains","Northwest")~"North",
                             T~"OTHER"),
    precip_reg_strat = case_when(corps_region =="AW" & ppt.567<21~"LessThanMedian",
                                 corps_region =="AW" & ppt.567>=21~"GreaterThanMedian",
                                 corps_region =="WMVC" & ppt.567<38~"LessThanMedian",
                                 corps_region =="WMVC" & ppt.567>=38~"GreaterThanMedian",
                                 T~"OTHER"),
    all_region="West"
    
  ) %>%
  select(
    SiteCode, Class, lat, long,
    Region, Region_detail, Region_detail2,
    beta_region,
    mlra_region=corps_region,
    ohwm_region2,
    snow_strat,
    drainage_area_strat,
    precip_strat,
    precip_reg_strat,
    all_region
  )


##############
main_df <- main_df_nopnw %>%
  # bind_rows(pnw_df) %>%
  inner_join(gis_metrics_df) %>%
  inner_join(xwalk_df %>%
               select(SiteCode=sitecode, DRNAREA_mi2))

#These are the models I need to run

mod_summary<- 
  tibble(Region=c("West","AW","WM")) %>%
  crossing(
    xwalk_west_df %>%
      # filter(Region!="PNW") %>%
      select(ohwm_region2, snow_strat, drainage_area_strat, precip_reg_strat, precip_strat,beta_region,
             all_region) %>%
      unique() %>%
      pivot_longer(cols=c("all_region", "beta_region","ohwm_region2", "snow_strat", "drainage_area_strat", "precip_reg_strat", "precip_strat"), 
                   names_to = "Stratification",
                   values_to="Strata")
  ) %>%
  # bind_rows(
  #   # tibble(
  #   #   Region=c("West","AW","WM"),
  #   #   Stratification="Unstratified",
  #   #   Strata="Unstratified"
  #   # )
  # ) %>%
  crossing(IncludeGISPreds=c(T,F),
           IncludePNW=c(F)
  ) %>%
  mutate(Stratf_Strat=paste(Stratification, Strata, sep="_"),
         ModName = case_when(#IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW_GIS", sep="_"),
           IncludeGISPreds ~paste(Stratification, Strata, "GIS", sep="_"),
           # !IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW", sep="_"),
           T~paste(Stratification, Strata, sep="_")),
         Approach = case_when(#IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW_GIS", sep="_"),
           IncludeGISPreds ~paste(Stratification, "GIS", sep="_"),
           # !IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW", sep="_"),
           T~paste(Stratification,  sep="_"))) %>%
  # filter(!(Region!="West" & Stratification=="beta_region")) %>%
  filter(!(Region=="WM" & Stratification=="ohwm_region2")) %>%
  # filter(!(IncludePNW & Strata=="South")) %>%
  filter(!(Region=="West" & Stratification=="precip_reg_strat")) %>%
  filter(!(Region!="West" & Stratification=="precip_strat")) %>%
  filter(!(Region!="West" & Stratification=="beta_region")) %>%
  filter(!(Region!="West" & Stratification=="West")) %>%
  filter(Stratification !="ohwm_region2")  %>%
  filter(Stratification!="beta_region")
mod_summary$Region<-factor(mod_summary$Region, levels=c("AW","WM","West"))


all_strata<-mod_summary$Stratification %>% unique()
xwalk_long<-xwalk_west_df %>%
  select(SiteCode,  Region_detail2, all_of(all_strata)) %>%
  pivot_longer(cols= all_of(all_strata),
               names_to = "Stratification", values_to ="Strata" )

#

xwalk_west_df %>%
  filter(Class!="U") %>%
  select(Class, beta_region, ohwm_region2, snow_strat, drainage_area_strat, precip_reg_strat, precip_strat) %>%
  pivot_longer(cols=c( ohwm_region2, snow_strat, drainage_area_strat, precip_reg_strat, precip_strat),
               names_to="Regionalization",
               values_to="Region") %>%
  group_by(beta_region,Regionalization, Region, Class) %>% 
  tally() %>%
  group_by(beta_region, Regionalization, Region) %>% 
  mutate(ntot=sum(n),
         pct = n/ntot) %>%
  slice_min(pct, n=1) %>%
  arrange(pct) %>%
  select(-ntot)

##############

library(sf)
beta_sf<-read_sf("NotForGit/Shapefiles/BetaSDAM_Regions/Major_SDAM_regions.shp")
state_data <-  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

basemap_sf<-ggplot()+
  geom_sf(data=state_data)+
  # geom_sf(data=beta_sf, fill=NA, color="white")+
  geom_sf(data=beta_sf %>% filter(SDAM=="Western Mountains"), fill="lightblue", color=NA)+
  geom_sf(data=beta_sf %>% filter(SDAM=="Arid West"), fill="lightpink", color=NA, alpha=.75)+
  geom_sf(data=state_data, fill=NA, color="black")+
  coord_sf(xlim=c(-124, -102.5), ylim=c(29.5,48.5))+
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
xwalk_west_sf<-xwalk_long %>%
  inner_join(xwalk_west_df %>% select(SiteCode, lat, long)) %>%
  st_as_sf(coords=c("long","lat"), crs=4326) %>%
  mutate(Strata2 = case_when(Strata %in% c("West" ) & Region_detail2 == "AW" ~"B",
                             Strata %in% c("West" ) & Region_detail2 == "WM" ~"C",
                             Strata %in% c("Small","LessThanMedian","Arid West","Not snow influenced")~"B",
                             Strata %in% c("Large","GreaterThanMedian","Western Mountains","Snow influenced")~"C",
                             T~"OTHER"
  ))

# strata_maps<-basemap_sf +
#   geom_sf(data=xwalk_west_sf, aes(fill=Strata2), shape=21)+
#   coord_sf(xlim=c(-124, -102.5), ylim=c(29.5,48.5))+
#   # facet_wrap(~Stratification)+
#   facet_grid(Region_detail2~Stratification)+
#   scale_fill_manual(values=c("black","white"), guide="none")
# ggsave(strata_maps, filename="Figures/strata_maps.png", height=7, width=12)

xwalk_west_sf2<-
  xwalk_west_sf %>%
  filter(Stratification!="precip_strat") %>%
  bind_rows(
    xwalk_west_sf %>%
      mutate(Strata2 = case_when(Stratification=="all_region"~"A",
                                 T~Strata2),
             Region_detail2="West") %>%
      filter(Stratification!="precip_reg_strat")
  ) %>%
  mutate(Region_detail2=factor(Region_detail2, levels=c("AW","WM","West")))

strata_maps2<-
  basemap_sf +
  geom_sf(data=xwalk_west_sf2, aes(fill=Strata2), shape=21)+
  coord_sf(xlim=c(-124, -102.5), ylim=c(29.5,48.5))+
  # facet_wrap(~Stratification)+
  facet_grid(Region_detail2~Stratification)+
  scale_fill_manual(values=c("gray", "black","white"), guide="none")
ggsave(strata_maps2, filename="Figures/strata_maps2.png", height=7, width=12)


######################################

main_df2<-
  main_df %>%
  filter(SiteCode %in% xwalk_west_df$SiteCode[xwalk_west_df$Class!="U"]) %>%
  select(SiteCode, Region_DB, Database, CollectionDate, ParentGlobalID,
         all_of(lumets$Metric)) %>%
  # skim()
  # main_df %>% filter(is.na(Fish_score_NM)) %>% filter(Region_DB=="West")
  # main_df %>% filter(SiteCode %in% c("NMWM1301","WYWM1535")) %>% select(SiteCode, CollectionDate, Slope, Database)
  # select(-Fish_UpTo3) %>%
  na.omit() %>%
  inner_join(xwalk_df %>% 
               rename(SiteCode=sitecode) %>%
               select(-DRNAREA_mi2)) %>%
  filter(Class!="U") 

###Resampling to 3 visits per site

visit_tally<-main_df2 %>%
  group_by(Region_DB,SiteCode, Class) %>%
  tally() %>%
  ungroup()
visit_tally_plot<-ggplot(data=visit_tally, aes(x=n, fill=Class))+
  geom_histogram(color="black")+
  scale_x_continuous(breaks=c(0:12))+
  scale_fill_brewer(palette="RdYlBu")+
  theme_bw()+xlab("# visits")
ggsave(visit_tally_plot, filename="Figures/visit_tally_plot.png", height=6, width=6)


visit_tally$AtLeast3Samples<-visit_tally$n>=3

set.seed(1)
main_df2_GT3 <- main_df2 %>%
  filter(SiteCode %in% visit_tally$SiteCode[visit_tally$AtLeast3Samples]) %>%
  group_by(SiteCode) %>%
  slice_sample(n=3, replace=F) %>%
  ungroup() %>%
  mutate(DataType="Original")

set.seed(1)
main_df2_LT3 <- main_df2 %>%
  filter(SiteCode %in% visit_tally$SiteCode[!visit_tally$AtLeast3Samples]) %>%
  mutate(DataType="Original") %>%
  #If visited 2 times, add a random visit
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==2]) %>%
      group_by(SiteCode) %>%
      slice_sample(n=1, replace=F) %>%
      mutate(DataType="Augmented") 
  ) %>%
  #If visited once, add triple visits again
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]) %>%
      mutate(DataType="Augmented") ,
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]) %>%
      mutate(DataType="Augmented") ,
  ) %>%
  ungroup()

main_df2_LT3 %>%
  group_by(SiteCode) %>% tally() %>%
  filter(n!=3)

main_df2_GT3 %>%
  group_by(SiteCode) %>% tally() %>%
  filter(n!=3)

main_df3<-main_df2_GT3 %>%
  bind_rows(main_df2_LT3) %>%
  mutate(Class = factor(Class, levels=c("E","I","P"))) %>%
  inner_join(xwalk_west_df %>%
               select(SiteCode,
                      ohwm_region2, snow_strat,drainage_area_strat, precip_strat, precip_reg_strat))
# inner_join(    main_df %>% select(SiteCode, CollectionDate, all_of( lumets$Metric )))

setdiff(lumets$Metric, names(main_df3))

main_df3 %>%
  select(SiteCode, Class, ohwm_region) %>%
  unique() %>%
  group_by(ohwm_region, Class) %>% tally()


main_df3 %>%
  group_by(SiteCode) %>% tally() %>%
  group_by(n) %>% tally()

setdiff(main_df2$ParentGlobalID, main_df3)
main_df2$SelectedSample = main_df2$ParentGlobalID %in% main_df3$ParentGlobalID
write_csv(main_df2, "NotForGit/main_df2.csv")
write_csv(main_df3, "NotForGit/main_df3.csv")

##########################
#########MODELING
library(tidymodels)


metrics_wg<-lumets$Metric[lumets$West] #No need to differentiate ISA from ISAsubregion metrics
gis_metrics<-lumets$Metric[lumets$MetricType=="Geospatial"]



xwalk_long %>%
  inner_join(xwalk_west_df %>% select(SiteCode, Class)) %>%
  group_by(Region_detail2, Stratification, Strata, Class) %>%
  tally() %>% pivot_wider(names_from=Class, values_from = n, values_fill = 0) %>%
  write_clip()


mod_dat_sites<-lapply(1:nrow(mod_summary), function(i){
  stratf.i=mod_summary$Stratification[i]
  strat.i=mod_summary$Strata[i]
  gis.i=mod_summary$IncludeGISPreds[i]
  reg.i=mod_summary$Region[i]
  print(paste(stratf.i, strat.i))
  if(reg.i=="West")
    xdf=xwalk_long
  else
    xdf=xwalk_long %>% filter(Region_detail2==reg.i)
  
  xdf<-xdf %>%
    filter(Stratification==stratf.i & strat.i==Strata)
  xdf
})

mod_dats<-lapply(1:nrow(mod_summary), function(i){
  mysites=mod_dat_sites[[i]]$SiteCode
  gis.i=mod_summary$IncludeGISPreds[i]
  xdf=main_df3 %>%
    filter(SiteCode %in% mysites) %>% 
    select(SiteCode, Class, CollectionDate, DataType, all_of(metrics_wg))
  if(gis.i)
    xdf
  else
    xdf %>% select(names(xdf), all_of(gis_metrics))
})
# SiteCode Class CollectionDate      DataType AlgaeAbundanceMossCover_Score2



set.seed(2)
mod_dats_split<-lapply(mod_dats, function(x){  
  x2 = x %>% select(SiteCode, Class) %>% unique()
  initial_split(x2, prop=4/5, strata=Class) })

mod_dats_training<-lapply(1:nrow(mod_summary), function(i){
  x=mod_dats_split[[i]]
  x2 = training(x)
  mod_dats[[i]] %>%     filter(SiteCode %in% x2$SiteCode)
})


mod_dats_testing<-lapply(1:nrow(mod_summary), function(i){
  x=mod_dats_split[[i]]
  x2 = testing(x)
  mod_dats[[i]] %>%     filter(SiteCode %in% x2$SiteCode)
})


mod_summary$n_training<-sapply(mod_dats_split, function(x){  x %>% training() %>% nrow() })
mod_summary$n_training_P<-sapply(mod_dats_split, function(x){  x %>% training() %>% filter(Class=="P") %>% nrow() })
mod_summary$n_training_I<-sapply(mod_dats_split, function(x){  x %>% training() %>% filter(Class=="I") %>% nrow() })
mod_summary$n_training_E<-sapply(mod_dats_split, function(x){  x %>% training() %>% filter(Class=="E") %>% nrow() })
mod_summary$n_testing<-sapply(mod_dats_split, function(x){  x %>% testing() %>% nrow() })
mod_summary$n_testing_P<-sapply(mod_dats_split, function(x){  x %>% testing() %>% filter(Class=="P") %>% nrow() })
mod_summary$n_testing_I<-sapply(mod_dats_split, function(x){  x %>% testing() %>% filter(Class=="I") %>% nrow() })
mod_summary$n_testing_E<-sapply(mod_dats_split, function(x){  x %>% testing() %>% filter(Class=="E") %>% nrow() })

#####


library(randomForest)
# sapply(mod_dats, function(x) length(names(x)))


my_rfs<-lapply(1:nrow(mod_summary), function(i){
  # lapply(3:5, function(i){
  stratf.i=mod_summary$Stratification[i]
  gis.i = mod_summary$IncludeGISPreds[i]
  print(paste(i, mod_summary$ModName[i]))
  mydat = mod_dats_training[[i]] %>% select(-CollectionDate, -SiteCode, -DataType)#,
  # if(stratf.i=="all_region")
  #   mydat = mod_dats_training[[i]] %>% select(-CollectionDate, -DataType) %>%
  #   inner_join(xwalk_west_df %>% select(SiteCode, ohwm_region2, beta_region)) %>%
  #   select(-SiteCode)
  # else
  #   mydat = mod_dats_training[[i]] %>% select(-CollectionDate, -SiteCode, -DataType)#,
  if(gis.i)
    mydat = mydat
  else
    mydat = mydat %>%
    select(names(mydat) %>%
             setdiff(c(gis_metrics)))
  set.seed(200+i)
  rf.i=randomForest(Class~., 
                    data=mydat, 
                    importance=T,
                    proximity=T)
  if(gis.i)
  {
    rf.i_imp = rf.i$importance %>%
      as_tibble() %>%
      mutate(Var = row.names(rf.i$importance))
    best2var = rf.i_imp %>%
      filter(Var %in% gis_metrics) %>%
      # arrange(MeanDecreaseAccuracy)
      slice_max(MeanDecreaseAccuracy, n=2)
    mydat = mydat %>% select(names(mydat) %>%
                               setdiff(gis_metrics %>%
                                         setdiff(best2var$Var)))
    print(best2var$Var)
    set.seed(200+i)
    rf.i2=randomForest(Class~., 
                       data=mydat, 
                       importance=T,
                       proximity=T)
    rf.i2
  }
  else
    
    rf.i
})


#################


####
#Importance

rf_sum_importance<-lapply(1:nrow(mod_summary), function(i){
  gis.i=mod_summary$IncludeGISPreds[i]
  pnw.i=mod_summary$IncludePNW[i]
  # flag.i=mod_summary$FlagNoPNW[i]
  regz.i=mod_summary$Stratification[i]
  reg.i=mod_summary$Strata[i]
  mod.i=my_rfs[[i]]
  regg.i=mod_summary$Region[i]
  # print(mod.i)
  xmat=mod.i$importance
  xdf=xmat %>%
    as_tibble() %>%
    mutate(Stratification=regz.i,
           Region=regg.i,
           Strata=reg.i,
           GIS=case_when(gis.i~"GIS",T~"No GIS"),
           PNW = case_when(pnw.i~"PNW",T~"No PNW"),
           PNWtf=pnw.i,
           # Flag=flag.i,
           Metric=row.names(xmat)) %>%
    rename(P_imp=P, I_imp=I, E_imp=E)
  
}) %>% bind_rows() %>%
  mutate(MetricType=case_when(Metric %in% lumets$Metric[which(lumets$MetricType=="Geomorph")]~"Geomorphic",
                              Metric %in% lumets$Metric[which(lumets$MetricType=="Geospatial")]~"GIS",
                              Metric %in% c("mlra","beta","nwca","ohwm")~"GIS_spatial",
                              Metric %in% lumets$Metric[which(lumets$MetricType=="Hydro")]~"Hydro",
                              Metric %in% lumets$Metric[which(lumets$MetricType=="Biology")]~"Bio",
                              T~"Other" ),
         RegLabel = paste(Region, Stratification, Strata, sep="-"))

rf_sum_importance %>% group_by(MetricType) %>% tally()
imp_plot_dat<-rf_sum_importance %>%
  mutate(MeanDecreaseAccuracy=case_when(MeanDecreaseAccuracy<0~0,T~MeanDecreaseAccuracy)) %>%
  arrange(MeanDecreaseAccuracy)
imp_plot_dat$Metric<-factor(imp_plot_dat$Metric, levels=unique(imp_plot_dat$Metric))
imp_plot_dat$Region<-factor(imp_plot_dat$Region, levels=c("AW","WM","West"))

imp_plot_dat %>%
  filter(str_detect(Metric, "ISA")) %>%
  select(Metric)

met_colors_df<-imp_plot_dat %>%
  select(MetricType, Metric) %>%
  mutate(MetricColor = case_when(MetricType =="Bio"~"#4daf4a",
                                 MetricType =="Hydro"~"#377eb8",
                                 MetricType =="Geomorphic"~"#d95f02",
                                 MetricType =="GIS"~"#7570b3",
                                 
  ))


variable_importance_plot_WEST<-
  ggplot(imp_plot_dat %>% 
           group_by(RegLabel) %>%
           mutate(RankMDA = rank(MeanDecreaseAccuracy)/sum(!is.na(MeanDecreaseAccuracy))) %>%
           ungroup(), 
         aes(x=RegLabel, y=Metric, fill=RankMDA))+
  geom_tile(color="white")+
  scale_fill_viridis_c(na.value = "gray", name="Rank\nImportance")+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(.~GIS, scales="free", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
        panel.grid = element_blank())+
  # ggtitle("Variable importance")+
  xlab("")+ylab("")
ggsave(variable_importance_plot_WEST, filename="Figures/variable_importance_plot_WEST.png",
       height=7.5, width=12)

variable_importance_plot_WEST_ISAonly<- 
  ggplot(imp_plot_dat %>% 
           group_by(RegLabel) %>%
           mutate(RankMDA = rank(MeanDecreaseAccuracy)/sum(!is.na(MeanDecreaseAccuracy))) %>%
           ungroup(), 
         aes(x=RegLabel, y=Metric, fill=RankMDA))+
  geom_tile(data = . %>% filter(str_detect(Metric, "ISA") |
                                  str_detect(Metric, "perennial")),
            color="white")+
  scale_fill_viridis_c(na.value = "gray", name="Rank\nImportance")+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(.~GIS, scales="free", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
        panel.grid = element_blank())+
  # ggtitle("Variable importance")+
  xlab("")+ylab("")
ggsave(variable_importance_plot_WEST_ISAonly, filename="Figures/variable_importance_plot_WEST_ISAonly.png",
       height=7.5, width=12)

ggplot(imp_plot_dat %>% 
         
         
         group_by(RegLabel) %>%
         mutate(RankMDA = rank(MeanDecreaseAccuracy)/sum(!is.na(MeanDecreaseAccuracy))) %>%
         ungroup()%>% filter(Metric %in% gis_metrics) , 
       aes(x=RegLabel, y=Metric, fill=RankMDA))+
  geom_tile( 
            color="gray")+
  scale_fill_viridis_c(na.value = "gray", name="Rank\nImportance")+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(.~GIS, scales="free", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
        panel.grid = element_blank())+
  # ggtitle("Variable importance")+
  xlab("")+ylab("")


isa_plot_dat<-
  main_df %>%
  select(SiteCode, contains("ISA"), contains("peren")) %>%
  inner_join(xwalk_df %>% 
               select(SiteCode=sitecode, Class, Region, Region_detail)) %>%
  mutate(Region_detail = case_when(Region_detail %in% c("GP_C","GP_U","GP_N")~"NGP",
                                   Region_detail=="GP_S"~"SGP",
                                   T~Region_detail)) %>%
  pivot_longer(cols=c(contains("ISA"), contains("peren")))

isa_plot<-
ggplot(data=isa_plot_dat %>%
         filter(str_detect(name,"_taxa")) %>%
         filter(name!="ephemeral_ISA_taxa") %>%
         filter(name!="ephemeral_ISAsubregion_taxa") %>%
         filter(Class!="U") %>%
         filter(!str_detect(name, "live")) %>%
         filter(Region_detail!="CB") %>%
         mutate(Region_detail=factor(Region_detail, levels=c("AW","WM","SGP","NGP","SE","NE"))),
       aes(x=Class, y=value))+
  geom_boxplot()+
  facet_grid(name~Region_detail, scales="free")+
  theme_bw()+
  theme(strip.text.y = element_text(angle=0))
ggsave(isa_plot, filename="NotForGit/isa_plot.png", height=7, width=12)

isa_abundance_plot<-
  ggplot(data=isa_plot_dat %>%
           filter(str_detect(name,"_abundance")) %>%
           filter(name!="ephemeral_ISA_abundance") %>%
           filter(name!="ephemeral_ISAsubregion_abundance") %>%
           filter(!str_detect(name, "live")) %>%
           filter(Class!="U") %>%
           filter(Region_detail!="CB") %>%
           mutate(Region_detail=factor(Region_detail, levels=c("AW","WM","SGP","NGP","SE","NE"))),
         aes(x=Class, y=value))+
  geom_boxplot()+
  facet_grid(name~Region_detail, scales="free")+
  theme_bw()+
  theme(strip.text.y = element_text(angle=0))
ggsave(isa_abundance_plot, filename="NotForGit/isa_abundance_plot.png", height=7, width=12)

ggplot(data=isa_plot_dat %>%
         filter(str_detect(name,"_taxa")) %>%
         filter(name!="ephemeral_ISA_taxa") %>%
         filter(name!="ephemeral_ISAsubregion_taxa") %>%
         filter(Class!="U") %>%
         filter(Region_detail!="CB") %>%
         mutate(Region_detail=factor(Region_detail, levels=c("AW","WM","SGP","NGP","SE","NE"))),
       aes(x=name, y=value, fill=Class))+
  geom_boxplot()+
  facet_grid(name~Region_detail, scales="free")+
  theme_bw()+
  theme(strip.text.y = element_text(angle=0))+
  coord_flip()

###############################

west_sites<-xwalk_df$sitecode[xwalk_df$Region=="West"]

mod_summary$ErrRate_all<-  sapply(my_rfs, function(x){ x$err.rate %>% tail(1)["OOB"]})
mod_summary$ErrRate_P<-  sapply(my_rfs, function(x){ x$err.rate %>% tail(1)["P"]})
mod_summary$ErrRate_I<-  sapply(my_rfs, function(x){ x$err.rate %>% tail(1)["I"]})
mod_summary$ErrRate_E<-  sapply(my_rfs, function(x){ x$err.rate %>% tail(1)["E"]})
junk<-my_rfs[[1]]
predict(junk, type="class")

my_predicted_classes_training<-lapply(1:nrow(mod_summary), function(i){
  print(paste(i, mod_summary$ModName[i]))
  rf.i=my_rfs[[i]]
  xdf=mod_dats_training[[i]] %>%
    mutate(ModName=mod_summary$ModName[i],
           Class_predicted = predict(rf.i, type="class")) %>%
    bind_cols(
      predict(rf.i, type="prob") %>%
        as_tibble() %>%
        transmute(
          probE = E, 
          probI = I,
          probP = P,
          probALI = E+I,
          Class_pred50 = case_when(probP>=.5~"P",
                                   probI>=.5~"I",
                                   probE>=.5~"E",
                                   probALI>=.5~"ALI",
                                   T~"NMI")
          
        )
    ) %>%
    mutate( Class_pred50_best = case_when(Class_pred50 == "NMI"~Class_predicted,
                                          T~Class_pred50),
            SiteSet="Training")
})


my_predicted_classes_testing<-lapply(1:nrow(mod_summary), function(i){
  print(paste(i, mod_summary$ModName[i]))
  rf.i=my_rfs[[i]]
  xdf=mod_dats_testing[[i]] %>%
    mutate(ModName=mod_summary$ModName[i])
  xdf <-xdf %>% 
    mutate(Class_predicted = predict(rf.i, type="class", newdata=xdf))
  xdf<-xdf %>%
    bind_cols(
      predict(rf.i, type="prob", newdata=xdf) %>%
        as_tibble() %>%
        transmute(
          probE = E, 
          probI = I,
          probP = P,
          probALI = E+I,
          Class_pred50 = case_when(probP>=.5~"P",
                                   probI>=.5~"I",
                                   probE>=.5~"E",
                                   probALI>=.5~"ALI",
                                   T~"NMI")
          
        )
    ) %>%
    mutate( Class_pred50_best = case_when(Class_pred50 == "NMI"~Class_predicted,
                                          T~Class_pred50),
            SiteSet="Testing")
})


my_predicted_classes_combined<-lapply(1:length(my_predicted_classes_training), function(i){
  my_predicted_classes_training[[i]] %>%
    bind_rows(
      my_predicted_classes_testing[[i]]
    )
})

my_predicted_classes_combined[[1]] %>% group_by(Class) %>% tally()

mod_summary$NMI_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%    filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="NMI")/length(xdf$Class_pred50_best)
})

mod_summary$P_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%     filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="P")/length(xdf$Class_pred50_best)
})
mod_summary$I_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%     filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="I")/length(xdf$Class_pred50_best)
})
mod_summary$ALI_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%     filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="ALI")/length(xdf$Class_pred50_best)
})
mod_summary$E_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%     filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="E")/length(xdf$Class_pred50_best)
})

outcomes_plot_dat<-mod_summary %>%
  select(Stratification, Strata, IncludeGISPreds, ModName, Region,
         P_freq, I_freq, ALI_freq, E_freq, NMI_freq) %>%
  pivot_longer(cols=(ends_with("freq"))) %>%
  mutate(name=factor(name, levels=c("P_freq","I_freq","ALI_freq","E_freq", "NMI_freq")),
         ModName2 = case_when(IncludeGISPreds~paste(Strata,"GIS", sep="_"),
                              T~Strata),
         ModName2 = case_when(Strata %in% c("Arid West","Western Mountains") & IncludeGISPreds ~"Beta_GIS",
                              Strata %in% c("Arid West","Western Mountains") & !IncludeGISPreds ~"Beta",
                              T~ModName2),
         Region = case_when(Stratification=="beta_region" & Strata=="Arid West"~"AW",
                            Stratification=="beta_region" & Strata=="Western Mountains"~"WM",
                            T~Region) %>%
           factor(levels=c("AW","WM","West"))
  )


outcomes_plot_dat %>% group_by(name) %>% summarise(maximum = max(value))
outcomes_plot_dat %>%
  filter(ModName=="all_region_West" & name=="P_freq")
outcomes_plot_dat %>% filter(ModName2=="Beta" & name=="P_freq")
outcomes_plot_dat %>%
  filter(name=="ALI_freq" ) %>% arrange(value) %>%
  select(Stratification, Strata, IncludeGISPreds, Region, value)

outcomes_plot_dat %>%
  filter(name=="NMI_freq" ) %>% arrange(-value) %>%
  select(Stratification, Strata, IncludeGISPreds, Region, value)

outcomes_plot<-
  ggplot(data=outcomes_plot_dat, aes(x=ModName, y=value))+
  geom_bar(aes(fill=name), stat="identity") +
  scale_fill_manual(values=c("#08519c","#6baed6","#8856a7","#f768a1","orange"))+
  coord_flip()+
  xlab("")+ylab("% of samples")+
  facet_wrap(~Region)
ggsave(outcomes_plot, filename="Figures/outcomes_plot.png", height=8, width=7)

outcomes_plot_dat %>%
  filter(name=="ALI_freq") %>%
  slice_max(value, n=8)

################################################


mod_summary$Accuracy_PvIvE_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary$Accuracy_PvIvE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})


mod_summary$Accuracy_EvALI_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary$Accuracy_EvALI_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})


mod_summary$Precision_PvIvE_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(DataType=="Original") %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) 
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary$Precision_PvIvE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) 
  
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})


mod_summary$Precision_EvALI_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"))
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary$Precision_EvALI_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"))
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary$Accuracy_PnotE_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})

mod_summary$Accuracy_PnotE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})

mod_summary$Accuracy_EnotP_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})

mod_summary$Accuracy_EnotP_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})

mod_summary$Accuracy_EnotP_testing[is.na(mod_summary$Accuracy_EnotP_testing)]<-0

mod_summary_long<-mod_summary %>%
  pivot_longer(cols=c(contains("Accuracy"), contains("Precision"))) %>%
  mutate(MetricType2 = case_when(str_detect(name,"Accuracy")~"Accuracy",
                                 str_detect(name,"Precision")~"Precision",
                                 T~"Other"),
         Comparison = case_when(str_detect(name,"PvIvE")~"PvIvE",
                                str_detect(name,"EvALI")~"EvALI",
                                str_detect(name,"PnotE")~"PnotE",
                                str_detect(name,"EnotP")~"EnotP",
                                T~"Other"),
         SiteSet = case_when(str_detect(name, "training")~"Training",
                             T~"Testing"),
         Metric = paste(MetricType2, Comparison, sep="_")
  ) %>%
  select(Stratification, Strata, IncludeGISPreds, IncludePNW, ModName, SiteSet, Region,
         n_training, n_testing,n_training_E, n_testing_E, n_training_P, n_testing_P,
         MetricType2, Comparison,Metric, name, value
  ) %>%
  mutate(Strata2 = factor(Strata, levels=c("West",
                                           "Western Mountains","Arid West",
                                           "Large","Small",
                                           "Snow influenced","Not snow influenced",
                                           "GreaterThanMedian","LessThanMedian")),
         FLAG = case_when(n_training<20~"FLAG_training",
                          n_testing<5~"FLAG_testing",
                          Comparison=="PnotE" & n_training_P<5~"FLAG_training",
                          Comparison=="PnotE" & n_testing_P<5~"FLAG_testing",
                          Comparison=="EnotP" & n_training_E<5~"FLAG_training",
                          Comparison=="EnotP" & n_testing_E<5~"FLAG_testing",
                          T~"OK")
  )

mod_summary_long %>%
  filter(Comparison=="EnotP" & n_training_E<10) %>%
  as.data.frame()

mod_summary_long %>%
  filter(MetricType2=="Accuracy" & SiteSet=="Training" & Comparison == "EvALI" & IncludeGISPreds) %>%
  filter(Strata=="Large" )



accuracy_performance_metrics_WEST<-
  ggplot(data=mod_summary_long %>% filter(MetricType2=="Accuracy") ,
         aes(x=Strata2, y=value))+
  geom_bar(data = . %>%
             filter(FLAG=="FLAG_training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5,
           stat="identity", width=1  ) +
  geom_bar(data = . %>%
             filter(FLAG=="FLAG_testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5,
           stat="identity", width=1  ) +
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), )+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Region, scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST, filename="Figures/accuracy_performance_metrics_WEST.png", height=7.5, width=10)




accuracy_performance_metrics_WEST2<-ggplot(data=mod_summary_long %>% filter(MetricType2=="Accuracy") %>% filter(Comparison %in% c("PvIvE","EvALI")) , 
                                           aes(x=Strata2, y=value))+
  geom_bar(data = . %>%
             filter(FLAG=="FLAG_training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5,
           stat="identity", width=1  ) +
  geom_bar(data = . %>%
             filter(FLAG=="FLAG_testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5,
           stat="identity", width=1  ) +
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludeGISPreds ), position=position_dodge(width=.25))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Region, scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST2, filename="Figures/accuracy_performance_metrics_WEST2.png", height=7.5, width=10)  



accuracy_performance_metrics_WEST2x<-ggplot(data=mod_summary_long %>% 
                                              filter(MetricType2=="Accuracy") %>% 
                                              filter(Comparison %in% c("EnotP","PnotE")) , 
                                            aes(x=Strata2, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludeGISPreds), position=position_dodge(width=.25))+ 
  geom_bar(data = . %>%
             filter(FLAG=="FLAG_training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5,
           stat="identity", width=1  ) +
  geom_bar(data = . %>%
             filter(FLAG=="FLAG_testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5,
           stat="identity", width=1  ) +
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludeGISPreds), position=position_dodge(width=.25))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Region, scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST2x, filename="Figures/accuracy_performance_metrics_WEST2x.png", 
       height=7.5, width=10)


precision_performance_metrics_WEST<-ggplot(data=mod_summary_long %>% filter(MetricType2=="Precision") , 
                                           aes(x=Strata2, y=value))+
  geom_bar(data = . %>%
             filter(FLAG=="FLAG_training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5,
           stat="identity", width=1  ) +
  geom_bar(data = . %>%
             filter(FLAG=="FLAG_testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5,
           stat="identity", width=1  ) +
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludeGISPreds), position=position_dodge(width=.25))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Region, scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Precision")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(precision_performance_metrics_WEST, filename="Figures/precision_performance_metrics_WEST.png", 
       height=7.5, width=10)  



mod_summary_long_across_strata<-mod_summary_long %>%
  group_by(Stratification, IncludeGISPreds, IncludePNW, SiteSet, Region,
           MetricType2, Metric, Comparison, name) %>%
  summarise(value_unweighted = mean(value),
            lowest_value = min(value)) %>%
  ungroup()


############################################################
#Subpop assessments


xwalk_west_df$STATE = str_sub_all(xwalk_west_df$SiteCode, 1,2) %>% unlist()

mod_summary_assessment_strata <-  mod_summary %>%
  select(Stratification, Strata, ModName, IncludeGISPreds, IncludePNW, Region) %>%
  unique() %>%
  crossing(AssessmentStratum=xwalk_west_df$STATE %>% unique()) %>%
  filter(!AssessmentStratum %in% c("PNW")) 

mod_summary_assessment_strata$n_sites <- sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  pnw.i= F
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & 
             mod_summary$Strata==strat.i & 
             mod_summary$IncludeGISPreds==gis.i & 
             mod_summary$IncludePNW==pnw.i &
             mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i))
  nrow(xdf)
})
mod_summary_assessment_strata %>% filter(n_sites==0) #There are no AW sites in SD. There are no WM sites in TX

mod_summary_assessment_strata<- mod_summary_assessment_strata %>%
  # crossing(IncludeGISPreds=c(T,F)) %>%
  filter(n_sites>0)



mod_summary_assessment_strata$n_P_training<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i &
             mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    filter(Class=="P") 
  nrow(xdf)
})

mod_summary_assessment_strata$n_P_testing<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    filter(Class=="P") 
  nrow(xdf)
})

mod_summary_assessment_strata$n_I_training<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    filter(Class=="I") 
  nrow(xdf)
})

mod_summary_assessment_strata$n_I_testing<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    filter(Class=="I") 
  nrow(xdf)
})

mod_summary_assessment_strata$n_E_training<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    filter(Class=="E") 
  nrow(xdf)
})
mod_summary_assessment_strata$n_E_testing<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    filter(Class=="E") 
  nrow(xdf)
})


mod_summary_assessment_strata$Correct_PvIvE_training<-sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)
  # ifelse(is.na(y),0,y)
})


mod_summary_assessment_strata$Correct_PvIvE_testing<-sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)
})

mod_summary_assessment_strata$Correct_EvALI_training<-sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})


mod_summary_assessment_strata$Correct_EvALI_testing<-sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  reg.i= mod_summary_assessment_strata$Region[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$Region==reg.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_west_df %>% select(SiteCode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})


mod_summary_assessment_strata_long<-mod_summary_assessment_strata %>%
  select(-n_sites) %>%
  pivot_longer(cols=c(starts_with("n"), starts_with("Correct"))) %>%
  group_by(Stratification, IncludeGISPreds, Region, AssessmentStratum, name) %>%
  summarise(value=sum(value)) %>%
  # filter(AssessmentStratum=="SPD" & Stratification=="beta_region")
  pivot_wider(names_from="name", values_from = "value", values_fill = 0) %>%
  ungroup() %>%
  mutate(
    n_training = (n_P_training + n_I_training + n_E_training ),
    n_testing = (n_P_testing + n_I_testing + n_E_testing ),
    Accuracy_PvIvE_training = Correct_PvIvE_training/n_training,
    Accuracy_PvIvE_testing = Correct_PvIvE_testing/n_testing,
    Accuracy_EvALI_training = Correct_EvALI_training/n_training,
    Accuracy_EvALI_testing = Correct_EvALI_testing/n_testing
  ) %>%
  # select(-starts_with("Correct"), -starts_with("n_")) %>%
  pivot_longer(cols=starts_with("Accuracy")) %>%
  mutate(MetricType2 = case_when(str_detect(name,"Accuracy")~"Accuracy",
                                 str_detect(name,"Precision")~"Precision",
                                 T~"Other"),
         Comparison = case_when(str_detect(name,"PvIvE")~"PvIvE",
                                str_detect(name,"EvALI")~"EvALI",
                                str_detect(name,"PnotE")~"PnotE",
                                str_detect(name,"EnotP")~"EnotP",
                                T~"Other"),
         SiteSet = case_when(str_detect(name, "training")~"Training",
                             T~"Testing"),
         Metric = paste(MetricType2, Comparison, sep="_"),
         value=case_when(is.na(value)~0,T~value),
         
         # Stratification2 = case_when(Stratification=="all_region" & IncludePNW~"West (PNW)",
         #                             Stratification=="all_region" & !IncludePNW~"West",
         #                             Stratification=="beta_region"~"Beta",
         #                             Stratification=="corps_region"~"MLRA (PNW)"),
         # AssessmentStratum2 = case_when(Stratification=="all_region"& IncludePNW~paste(AssessmentStratum, "West (PNW)"),
         #                                Stratification=="all_region" & !IncludePNW~paste(AssessmentStratum,"West"),
         #                                Stratification=="beta_region"~paste(AssessmentStratum,"Beta"),
         #                                Stratification=="corps_region"~paste( AssessmentStratum, "MLRA (PNW)"))
  )  %>%
  mutate(FLAG = case_when(SiteSet=="Training" & n_E_training<4 ~ "Low E training",
                          SiteSet=="Testing" & n_E_training<4 ~ "Low E testing",
                          T~"E is fine"),
         FLAG2 = case_when(SiteSet=="Training" & n_training <5~"Low n training",
                           SiteSet=="Testing" & n_testing <5~"Low n testing",
                           T~"Fine"))

subpop_accuracy_plot_WEST_STATES_EvALI<-
  ggplot(data=mod_summary_assessment_strata_long %>%
           filter(Comparison=="EvALI") %>%
           filter(MetricType2=="Accuracy") %>%
           filter(AssessmentStratum!="SD" ) %>%
           # group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>%
           # slice_max(IncludePNW, n=1) %>%
           mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
         aes(x=AssessmentStratum , y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  geom_path(data = . %>% filter(SiteSet=="Training"),
            aes( color=IncludeGISPreds, group=IncludeGISPreds))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Stratification~Region, scales="free_x", space="free")+
  theme_bw()+
  # theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("EvALI Accuracy")+xlab("")
ggsave(subpop_accuracy_plot_WEST_STATES_EvALI, filename="Figures/subpop_accuracy_plot_WEST_STATES_EvALI.png", height=6, width=10)

subpop_accuracy_plot_WEST_STATES_EvALI_FLAGGED<-  
  ggplot(data=mod_summary_assessment_strata_long %>%
           filter(Comparison=="EvALI") %>%
           filter(MetricType2=="Accuracy") %>%
           filter(AssessmentStratum!="SD" ) %>%
           # group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>%
           # slice_max(IncludePNW, n=1) %>%
           mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
         aes(x=AssessmentStratum , y=value))+

  geom_bar(data = . %>%
             filter(FLAG2=="Low n testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5,
           stat="identity", width=1  ) +
  geom_bar(data = . %>%
             filter(FLAG2=="Low n training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5,
           stat="identity", width=1  ) +
  # geom_bar(data = . %>%
  #            filter(FLAG2=="Low n"),
  #          aes(y=Inf), fill="gray", alpha=.5,
  #          stat="identity", width=1
  # ) +
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  geom_path(data = . %>% filter(SiteSet=="Training"),
            aes( color=IncludeGISPreds, group=IncludeGISPreds))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Stratification~Region, scales="free_x", space="free")+
  theme_bw()+
  # theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("EvALI Accuracy")+xlab("")
ggsave(subpop_accuracy_plot_WEST_STATES_EvALI_FLAGGED, filename="Figures/subpop_accuracy_plot_WEST_STATES_EvALI_FLAGGED.png", height=6, width=10)



subpop_accuracy_plot_WEST_STATES_PvIvE<-
  ggplot(data=mod_summary_assessment_strata_long %>%
           filter(Comparison=="PvIvE") %>%
           filter(MetricType2=="Accuracy") %>%
           filter(AssessmentStratum!="SD" ) %>%
           # group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>%
           # slice_max(IncludePNW, n=1) %>%
           mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
         aes(x=AssessmentStratum , y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  geom_path(data = . %>% filter(SiteSet=="Training"),
            aes( color=IncludeGISPreds, group=IncludeGISPreds))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Stratification~Region, scales="free_x", space="free")+
  theme_bw()+
  # theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("PvIvE Accuracy")+xlab("")
ggsave(subpop_accuracy_plot_WEST_STATES_PvIvE, filename="Figures/subpop_accuracy_plot_WEST_STATES_PvIvE.png", height=6, width=10)


subpop_accuracy_plot_WEST_STATES_PvIvE_FLAGGED<-  
  ggplot(data=mod_summary_assessment_strata_long %>%
           filter(Comparison=="PvIvE") %>%
           filter(MetricType2=="Accuracy") %>%
           filter(AssessmentStratum!="SD" ) %>%
           mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
         aes(x=AssessmentStratum , y=value))+
  
  geom_bar(data = . %>%
             filter(FLAG2=="Low n testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5,
           stat="identity", width=1  ) +
  geom_bar(data = . %>%
             filter(FLAG2=="Low n training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5,
           stat="identity", width=1  ) +
  
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  geom_path(data = . %>% filter(SiteSet=="Training"),
            aes( color=IncludeGISPreds, group=IncludeGISPreds))+ 
  
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Stratification~Region, scales="free_x", space="free")+
  theme_bw()+
  
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("PvIvE Accuracy")+xlab("")
ggsave(subpop_accuracy_plot_WEST_STATES_PvIvE_FLAGGED, filename="Figures/subpop_accuracy_plot_WEST_STATES_PvIvE_FLAGGED.png", height=6, width=10)

xwalk_west_df %>% filter(STATE=="NV" & Region_detail=="WM")
main_df %>% filter(str_detect(SiteCode,"NVWM"))

mod_summary_assessment_strata_long %>%
  filter(AssessmentStratum=="NV" & Region=="WM")
###################


#Redo assessments splitting west results into AW or WM

aw_sites<-xwalk_west_df$SiteCode[xwalk_west_df$Region_detail=="AW"]
wm_sites<-xwalk_west_df$SiteCode[xwalk_west_df$Region_detail=="WM"]

mod_summary2<-mod_summary
mod_summary2$Accuracy_PvIvE_training_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})



mod_summary2$Accuracy_PvIvE_testing_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})


mod_summary2$Accuracy_EvALI_training_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary2$Accuracy_EvALI_testing_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})


mod_summary2$Accuracy_EvALI_testing_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary2$Accuracy_PvIvE_training_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary2$Accuracy_PvIvE_testing_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})


mod_summary2$Accuracy_EvALI_training_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary2$Accuracy_EvALI_testing_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})



#
mod_summary2$Accuracy_PnotE_training_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})
mod_summary2$Accuracy_PnotE_training_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})

mod_summary2$Accuracy_PnotE_testing_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})

mod_summary2$Accuracy_PnotE_testing_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})


mod_summary2$Accuracy_EnotP_training_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})
mod_summary2$Accuracy_EnotP_training_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})


mod_summary2$Accuracy_EnotP_testing_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})
mod_summary2$Accuracy_EnotP_testing_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})



mod_summary2$Accuracy_EnotP_training_AW[is.na(mod_summary2$Accuracy_EnotP_training_AW)]<-0
mod_summary2$Accuracy_EnotP_training_WM[is.na(mod_summary2$Accuracy_EnotP_training_WM)]<-0
mod_summary2$Accuracy_EnotP_testing_AW[is.na(mod_summary2$Accuracy_EnotP_testing_AW)]<-0
mod_summary2$Accuracy_EnotP_testing_WM[is.na(mod_summary2$Accuracy_EnotP_testing_WM)]<-0
#


mod_summary2$Precision_PvIvE_training_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(DataType=="Original") %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) 
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary2$Precision_PvIvE_testing_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) 
  
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})


mod_summary2$Precision_EvALI_training_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"))
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary2$Precision_EvALI_testing_AW<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"))
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})



mod_summary2$Precision_PvIvE_training_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(DataType=="Original") %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) 
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary2$Precision_PvIvE_testing_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) 
  
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})


mod_summary2$Precision_EvALI_training_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"))
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary2$Precision_EvALI_testing_WM<-sapply(1:nrow(mod_summary2), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"))
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})


mod_summary_long2<-
  mod_summary2 %>%
  pivot_longer(cols=c(contains("Accuracy"),contains("Precision"))) %>%
  filter(!(str_detect(name, "_AW")& Region!="West")) %>%
  filter(!(str_detect(name, "_WM")& Region!="West")) %>%
  filter(!(Region=="West" & name %in% c(
    "Accuracy_PvIvE_training","Accuracy_PvIvE_testing",
    "Accuracy_EvALI_training","Accuracy_EvALI_testing",
    "Accuracy_PnotE_training","Accuracy_PnotE_testing",
    "Accuracy_EnotP_training","Accuracy_EnotP_testing",
    "Precision_PvIvE_training","Precision_PvIvE_testing",
    "Precision_EvALI_training","Precision_EvALI_testing"
    )) ) %>%
  
  mutate(MetricType2 = case_when(str_detect(name,"Accuracy")~"Accuracy",
                                 str_detect(name,"Precision")~"Precision",
                                 T~"Other"),
         Comparison = case_when(str_detect(name,"PvIvE")~"PvIvE",
                                str_detect(name,"EvALI")~"EvALI",
                                str_detect(name,"PnotE")~"PnotE",
                                str_detect(name,"EnotP")~"EnotP",
                                T~"Other"),
         SiteSet = case_when(str_detect(name, "training")~"Training",
                             T~"Testing"),
         Metric = paste(MetricType2, Comparison, sep="_")
  ) %>%
  select(Stratification, Strata, IncludeGISPreds, IncludePNW, ModName, SiteSet, Region,
         n_training, n_testing,n_training_E, n_testing_E, n_training_P, n_testing_P,
         MetricType2, Comparison,Metric, name, value
  ) %>%
  mutate(Strata2 = factor(Strata, levels=c("West",
                                           "Western Mountains","Arid West",
                                           "Large","Small",
                                           "Snow influenced","Not snow influenced",
                                           "GreaterThanMedian","LessThanMedian")),
         FLAG = case_when(n_training<20~"FLAG_training",
                          n_testing<5~"FLAG_testing",
                          Comparison=="PnotE" & n_training_P<5~"FLAG_training",
                          Comparison=="PnotE" & n_testing_P<5~"FLAG_testing",
                          Comparison=="EnotP" & n_training_E<5~"FLAG_training",
                          Comparison=="EnotP" & n_testing_E<5~"FLAG_testing",
                          T~"OK"),
         AssessmentStratum = case_when(Region!="West"~Region,
                                       str_detect(name,"_AW")~"AW",
                                       str_detect(name,"_WM")~"WM",T~"OTHER"),
         Region2 = case_when(Region=="West"~paste("West", AssessmentStratum),
                             T~Region),
         GeoScale = case_when(Region=="West"~"West",T~"MLRA"),
         GIS = case_when(IncludeGISPreds~"GIS",T~"No GIS")
  )
mod_summary_long2$Region2<-factor(mod_summary_long2$Region2, levels=c("AW","West AW","WM","West WM"))
# mod_summary_long2 %>% select(Region, name, Comparison, AssessmentStratum) %>% unique() %>% print(n=50)



accuracy_performance_metrics_WEST2_AWWM<-
  ggplot(data=mod_summary_long2 %>% filter(MetricType2=="Accuracy") %>% filter(Comparison %in% c("PvIvE","EvALI")) , 
         aes(x=Strata2, y=value))+
  geom_bar(data = . %>% filter(FLAG=="FLAG_training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5, stat="identity", width=1  ) +
  geom_bar(data = . %>% filter(FLAG=="FLAG_testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5, stat="identity", width=1  ) +
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=GeoScale, shape=GeoScale ), position=position_dodge(width=.35))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~AssessmentStratum, scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST2_AWWM, filename="Figures/accuracy_performance_metrics_WEST2_AWWM.png", height=7.5, width=10)  


accuracy_performance_metrics_WEST2x_AWWM<-
  ggplot(data=mod_summary_long2 %>% filter(MetricType2=="Accuracy") %>% filter(Comparison %in% c("EnotP","PnotE")) , 
         aes(x=Strata2, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=GeoScale, shape=GeoScale ), position=position_dodge(width=.35))+ 
  geom_bar(data = . %>% filter(FLAG=="FLAG_training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5, stat="identity", width=1  ) +
  geom_bar(data = . %>% filter(FLAG=="FLAG_testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5, stat="identity", width=1  ) +
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=GeoScale, shape=GeoScale ), position=position_dodge(width=.35))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~AssessmentStratum, scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST2x_AWWM, filename="Figures/accuracy_performance_metrics_WEST2x_AWWM.png", height=7.5, width=10)  



mod_summary_long2 %>% filter(MetricType2=="Accuracy") %>% filter(Comparison %in% c("PvIvE")) %>%
  filter(IncludeGISPreds& Strata2=="West" & GeoScale=="West") %>% as.data.frame()


precision_performance_metrics_WEST_AWWM<- 
  ggplot(data=mod_summary_long2 %>% filter(MetricType2=="Precision") , 
         aes(x=Strata2, y=value))+
  geom_bar(data = . %>% filter(FLAG=="FLAG_training"),
           aes(y=Inf), fill="#fb9a99", alpha=.5, stat="identity", width=1  ) +
  geom_bar(data = . %>% filter(FLAG=="FLAG_testing"),
           aes(y=Inf), fill="#a6cee3", alpha=.5, stat="identity", width=1  ) +
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=GeoScale, shape=GeoScale ), position=position_dodge(width=.35))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~AssessmentStratum, scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(precision_performance_metrics_WEST_AWWM, filename="Figures/precision_performance_metrics_WEST_AWWM.png", 
       height=7.5, width=10)  

##########################

mod_summary2_agg<-mod_summary

mod_summary2_agg$N_training_AW<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  length(xdf$CORRECT)
})
mod_summary2_agg$N_training_WM<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  length(xdf$CORRECT)
})
mod_summary2_agg$N_testing_AW<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  length(xdf$CORRECT)
})
mod_summary2_agg$N_testing_WM<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  length(xdf$CORRECT)
})


mod_summary2_agg$NCorrect_PvIvE_training_AW<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)
})


mod_summary2_agg$NCorrect_PvIvE_testing_AW<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)
})


mod_summary2_agg$NCorrect_EvALI_training_AW<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})

mod_summary2_agg$NCorrect_EvALI_testing_AW<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% aw_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})


mod_summary2_agg$NCorrect_PvIvE_training_WM<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)
})

mod_summary2_agg$NCorrect_PvIvE_testing_WM<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)
})


mod_summary2_agg$NCorrect_EvALI_training_WM<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})

mod_summary2_agg$NCorrect_EvALI_testing_WM<-sapply(1:nrow(mod_summary2_agg), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% wm_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})



mod_summary2_agg_summary<-mod_summary2_agg %>%
  group_by(Region, Stratification, IncludeGISPreds) %>%
  mutate(
    n_training=sum(N_training_AW)+sum(N_training_WM),
    n_testing=sum(N_testing_AW)+sum(N_testing_WM),
    NCorrect_PvIvE_training_AW = sum(NCorrect_PvIvE_training_AW),
    NCorrect_PvIvE_testing_AW = sum(NCorrect_PvIvE_testing_AW),
    NCorrect_PvIvE_training_WM = sum(NCorrect_PvIvE_training_WM),
    NCorrect_PvIvE_testing_WM = sum(NCorrect_PvIvE_testing_WM),
    NCorrect_EvALI_training_AW = sum(NCorrect_EvALI_training_AW),
    NCorrect_EvALI_testing_AW = sum(NCorrect_EvALI_testing_AW),
    NCorrect_EvALI_training_WM = sum(NCorrect_EvALI_training_WM),
    NCorrect_EvALI_testing_WM = sum(NCorrect_EvALI_testing_WM),
  ) %>%
  ungroup() %>%
  mutate(NCorrect_PvIvE_training = NCorrect_PvIvE_training_AW + NCorrect_PvIvE_training_WM,
         NCorrect_PvIvE_testing = NCorrect_PvIvE_testing_AW + NCorrect_PvIvE_testing_WM,
         NCorrect_EvALI_training = NCorrect_EvALI_training_AW + NCorrect_EvALI_training_WM,
         NCorrect_EvALI_testing = NCorrect_EvALI_testing_AW + NCorrect_EvALI_testing_WM,
         Accuracy_PvIvE_training = NCorrect_PvIvE_training/n_training,
         Accuracy_PvIvE_testing = NCorrect_PvIvE_testing/n_testing,
         Accuracy_EvALI_training = NCorrect_EvALI_training/n_training,
         Accuracy_EvALI_testing = NCorrect_EvALI_testing/n_testing,
  )


mod_summary2_agg_summary_plotdat<-mod_summary2_agg_summary %>%
  select(Region,Stratification, n_training, n_testing, starts_with("Accuracy"), IncludeGISPreds) %>%
  mutate( Stratification2 = case_when(Stratification=="all_region"~"Unstratified",
                                      Stratification=="drainage_area_strat"~"Drainage area",
                                      Stratification=="precip_reg_strat"~"Precipitation (reg)",
                                      Stratification=="precip_strat"~"Precipitation (West)",
                                      Stratification=="snow_strat"~"Snow",
                                      T~"Other"
  ),
  FLAG=case_when(n_training<20~"Flag Training",
                 n_testing<20~"Flat Testing", T~"No flag")
  ) %>% 
  pivot_longer(cols=starts_with("Accuracy")) %>%
  mutate(SiteSet = case_when(str_detect(name, "training")~"Training",
                             str_detect(name, "testing")~"Testing",
                             T~"Other"),
         Comparison = case_when(str_detect(name, "PvIvE")~"PvIvE",
                                str_detect(name, "EvALI")~"EvALI",
                                T~"Other")) %>%
  filter(Comparison!="Other") %>%
  mutate(Stratification3=case_when(str_detect(Stratification2,"Precip")~"Precipitation",
                                   T~Stratification2),
         Stratification3=factor(Stratification3, levels=c("Unstratified","Drainage area","Snow","Precipitation")
         ))

mod_summary2_agg_summary_plotdat %>%
  filter(Comparison=="Other") 

mod_summary2_agg_summary_plot_Accuracy1<-ggplot(data=mod_summary2_agg_summary_plotdat,
       aes(x=Stratification3, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludeGISPreds),position=position_dodge(width=.35))+ 
  facet_grid(Comparison~Region, scales="free_x", space="free")+
  scale_size_manual(values=c(1,2))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
        legend.position = "bottom",
        strip.text.y = element_text(angle=0))+
  xlab("")+ylab("")

ggsave(mod_summary2_agg_summary_plot_Accuracy1, filename="Figures/mod_summary2_agg_summary_plot_Accuracy1.png",
       height=7, width=10)



MLRA_Minus_West_plot_dat <-mod_summary_long2 %>% 
  mutate(Stratification=case_when(Stratification %in% c("precip_reg_strat","precip_strat")~"precip",
                                  T~Stratification)) %>%
  select(Stratification, Strata2, IncludeGISPreds, ModName, SiteSet,  Region2, Comparison, MetricType2, value ) %>%
  # filter(str_detect(Stratification, "precip")) %>%
  filter(MetricType2=="Accuracy") %>% 
  filter(Comparison %in% c("PvIvE","EvALI")) %>%
  pivot_wider(names_from=Region2, values_from = value) %>%
  # select(-ModName) %>%
  select(Stratification, Strata2, IncludeGISPreds, SiteSet, Comparison, MetricType2, AW, WM, `West AW`, `West WM`) %>%
  group_by(Stratification, Strata2, IncludeGISPreds, SiteSet, Comparison, MetricType2)  %>%
  summarise(AW=sum(AW, na.rm=T),
            WM=sum(WM, na.rm=T),
            `West AW`=sum(`West AW`, na.rm=T),
            `West WM`=sum(`West WM`, na.rm=T)) %>%
  ungroup() %>%
  mutate(AW_minus_Combined = AW - `West AW`,
         WM_minus_Combined = WM - `West WM`) %>%
  select(-AW, -`West AW`, -WM, -`West WM`) %>%
  pivot_longer(cols=c("AW_minus_Combined", "WM_minus_Combined")) %>%
  mutate(Region=str_sub(name,1,2))

MLRA_Minus_West_plot<-ggplot(data=MLRA_Minus_West_plot_dat, aes(x=Strata2, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Region, scales="free_x", space="free")+
  coord_flip()+
  geom_hline(yintercept=0, linetype="dashed")+
  xlab("")+ylab("MLRA minus Combined West")+
  theme_bw()
ggsave(MLRA_Minus_West_plot, filename="Figures/MLRA_Minus_West_plot.png", height=6, width = 8)
