library(tidyverse)

#Import xwalk<-
xwalk_df<-#read_csv("Data/master_site_class_xwalk_030723_coordinates.csv") %>%
  read_csv("Data/master_site_class_xwalk_08012023_coordinates_REGIONS.csv") %>%
  mutate(
    # Region_detail2 = case_when(Region_detail %in% c("GP_C","GP_N","GP_S","GP_U")~"GP",
    #                                 T~Region_detail) %>%
    #        factor(levels=c("AW","WM","GP","NE","SE","PNW", "CB")),
    #      all_region="USA",
    #      # conus_region=case_when(Region=="Caribbean"~"Non-CONUS",T~"CONUS"),
         Class=case_when(!Class %in% c("P","I","E","U")~"U",T~Class),
         Class=factor(Class, levels=c("P","I","E","U")))

EastDBs<-c("NESE Baseline Revisits v2", "NESE Baseline v1", "NESE Validation v1")
WestDBs<-c("WMBR_1_1","WEx_SDAM_0","WMBR_2","WMV_1","FD003","FD004")
GPDBs<-c("Great Plains Baseline Revisits v3",  "Great Plains SDAM v3",  "Great Plains Validation_v2", "Great Plains Baseline Revisits v2_1_October", "GreatPlains_SDAM_V3_edits")
# xwalk_df$Region_detail2<-factor(xwalk_df$Region_detail2, levels=c("AW","WM","GP","NE","SE", "CB"))

xwalk_df %>%
  group_by(Region_detail2, Class) %>% tally() %>%
  pivot_wider(names_from=Class, values_from = n, values_fill = 0) %>%
  mutate(Total = E+I+P+U,
         TotalKnown=E+I+P,
         Pct_P=P/TotalKnown,
         Pct_I=I/TotalKnown,
         Pct_E=E/TotalKnown) #%>%
# write.table(file="clipboard", sep="\t", row.names=F)

#Import main_df
#### MAIN - Read in main site data table and filter for GP sites ####
library(skimr)


main_raw<-read_csv("https://sdamchecker.sccwrp.org/checker/download/main-all")
# main_raw %>% filter(origin_database %in% EastDBs) %>% select(abundancealgae) %>% View()
# main_raw %>% filter(origin_database %in% WestDBs) %>% View()






# main_df %>%
#   select(Region_DB,Database, SiteCode, CollectionDate, starts_with("Alg")) %>%
#   mutate(AlgScore2 = case_when(Algae_score=="Not recorded"~NA_character_,
#                                T~Algae_score) %>%
#            as.numeric()) %>%
#   filter(is.na(AlgalCover_Live) & !is.na(AlgScore2)) %>% as.data.frame()
# group_by(Region_DB) %>% skim_without_charts()

# main_df %>%
# select(Region_DB,Database, SiteCode, CollectionDate, starts_with("Channel")) %>%
# group_by(Region_DB) %>% skim_without_charts()

main_df<-   main_raw %>%
  # inner_join(main_raw %>% select(-lat, -long)) %>% #read_csv("https://sdamchecker.sccwrp.org/checker/download/main-all") %>%
  # filter(origin_database %in% myDBs) %>%
  filter(sitecode %in% xwalk_df$sitecode) %>%
  transmute( 
    #SITE AND SAMPLE METADATA
    Download_date = Sys.time(),
    Database= origin_database,
    Region_DB = case_when(Database %in% EastDBs ~"East",
                          Database %in% WestDBs~"West",
                          Database %in% GPDBs~"GP",
                          T~"Other"),
    ParentGlobalID = globalid,
    SiteCode = sitecode,
    # Class, lat, long,
    SiteName = sitename,
    Assessors = assessor,
    Recorder= recorder, QA=qa,
    CollectionDate= collectiondate,
    CreationDate= creationdate,
    Creator=creator,
    EditDate=editdate,
    # Lat_field= lat, 
    # Long_field= long,
    Weather=weathernow,
    PctCloudCover = case_when(is.na(cloudynow) == T ~ 0,T~cloudynow), # Replace missing cloud cover with zero
    rain_yesno, #NESE only
    Disturbances=disturbed,
    Disturbances_details=disturbed_note,
    # Landuse=p_landuse,#NESE
    Landuse = case_when(is.na(p_landuse)~s_landuse, T~p_landuse), 
    # Landuse_s=s_landuse, #Now secondary landuse for NESE. Was primary for other datasets
    Landuse_s =case_when(is.na(p_landuse)~NA_character_, T~s_landuse),
    Lanuse_Notes=landuse_note,
    ReachLength_targ= reachlengthcalc,
    ReachLength_actual= reachlength,
    Riparian_yn= riparian, 
    ErosionDeposition_yn=erosion,
    Erosion_Deposition_Score=case_when(ErosionDeposition_yn=="yes"~1,
                                       ErosionDeposition_yn=="no"~0,
                                       T~NA_real_),
    FloodplainConnectivity_yn=floodplain,
    #GEOMORPHIC INDICATORS
    Bankwidth_0 = bankfullwidth0, #drop later
    Bankwidth_15 = bankfullwidth15, #drop later
    Bankwidth_30 = bankfullwidth30, #,#drop later
    # BankWidthMean = (Bankwidth_0+Bankwidth_15+Bankwidth_30)/(!is.na(Bankwidth_0)+!is.na(Bankwidth_15)+!is.na(Bankwidth_30)),
    #main_raw %>% select(Bankwidth_0, Bankwidth_15, Bankwidth_30) %>% rowMeans(na.rm=T), #nearly complete
    Slope=valleyslope,  #nearly complete
    ChannelSlope=Slope,
    SedimentOnPlantsDebris_score= hi_debrisscore, 
    SedimentOnPlantsDebris_notes= debrisnotes,
    Continuity_score = gi_contbbscore, # NESE - Continuity  of channel and bank score
    Continuity_notes = contbbnotes,# NESE
    Depositional_score = gi_depbbscore,# NESE - Depositional bars and benches
    Depositional_notes = depbbnotes,# NESE
    AlluvialDep_score = gi_radscore,# NESE
    AlluvialDep_notes = radnotes,# NESE
    Headcut_score = gi_headcutscore,# NESE
    Headcut_notes = headcutnotes,# NESE
    GradeControl_score = gi_gcscore,# NESE
    GradeControl_notes = gcnotes,# NESE
    NaturalValley_score = gi_nvscore,# NESE
    NaturalValley_notes = nvnotes, # NESE
    Sinuosity_score= si_sinuosityscore,
    Sinuousity_method= sinuositymethod,
    Sinuosiy_notes=sinuositycondition,
    ActiveFloodplain_score = gi_afpscore,# NESE ONLY
    ActiveFloodplain_notes = afpnotes,# NESE
    fp_number,# NESE - number of locations assessed for bankfull width
    fp_bankful,# NESE - Bankfull width, location 1
    fp_floodprone,# NESE - Floodprone width, location 1
    fp_greaterthan,# NESE - 2x bankfull width, location 1
    fp_bankful2,# NESE
    fp_floodprone2,#NESE
    fp_greaterthan2,# NESE
    fp_bankful3,# NESE
    fp_floodprone3,# NESE
    fp_greaterthan3,# NESE
    fp_entrenchmentratio1= case_when(fp_greaterthan=="yes"~2.5,T~fp_floodprone/fp_bankful),
    fp_entrenchmentratio2= case_when(fp_greaterthan2=="yes"~2.5,T~fp_floodprone2/fp_bankful2),
    fp_entrenchmentratio3= case_when(fp_greaterthan3=="yes"~2.5,T~fp_floodprone/fp_bankful3),
    ChannelDimensions_method= dimensionmethod,# Not recorded for NESE
    ChannelDimensions_score_NM=gi_dimensionscore,
    RifflePoolSeq_score= gi_sequencescore, 
    RifflePoolSeq_notes= seqnotes,
    SubstrateSorting_score=gi_substratesorting, 
    SubstrateSorting_notes=ginotes, 
    Substrate_Bedrock = scsc_bedrock, #NESE
    Substrate_Boulder = scsc_boulder,#NESE
    Substrate_BoulderSlab = scsc_boulderslab,#NESE
    Substrate_Cobble = scsc_cobble,#NESE
    Substrate_Gravel = scsc_gravel,#NESE
    Substrate_Sand = scsc_sand,#NESE
    Substrate_Silt = scsc_silt,#NESE
    Substrate_Clay = scsc_clay,#NESE
    Substrate_Muck = scsc_muck,#NESE
    Substrate_Leaf = scsc_leaf,#NESE
    Substrate_Fine = scsc_fine,#NESE
    Substrate_Artificial = scsc_artifical,#NESE
    #HYDROLOGIC INDICATORS
    SurfaceFlow_pct=hi_reachlengthsurface,
    SurfaceSubsurfaceFlow_pct= case_when(
      is.na(hi_reachlengthsub) ==T ~ SurfaceFlow_pct, # Replace missing subsurface flow with surface flow
      T~hi_reachlengthsub),
    IsolatedPools_number= case_when(
      is.na(pools_observed) == T ~ 0, # Replace missing pool number with zero
      T~pools_observed),
    MaxPoolDepth_calc = max_pool_depth_calc, # NESE only
    MaxPoolDepth = max_pool_depth,# NESE only
    SeepsSprings_yn=hi_seepsspring,# All regions
    SeepsSprings_inchannel = case_when(inchannel=="yes"~1,
                                       inchannel %in% c("na","no")~0,
                                       is.na(inchannel)~NA_real_),# NESE only
    springs_score_NM = case_when(SeepsSprings_yn=="present"~1.5,T~0),
    baseflowscore,# NESE only
    baseflow_notes,#NESE
    WaterInChannel_score = case_when(is.na(hi_channelscore) & !is.na(baseflowscore)~2*baseflowscore,
                                     T~hi_channelscore), # CALCULATE DUE TO MISSING FOR NESE
    WaterInChannel_notes=  case_when(is.na(hi_channelscore)~paste("Calculated as twice the value of NC indicator.",baseflow_notes),
                                     T~channelscorenote),
    Wet=WaterInChannel_score>3,
    LeafLitter_score = hi_leaflitter,# NESE
    LeafLitter_notes = leafnotes,# NESE
    ODL_score = hi_odl,# NESE
    ODL_notes = odlnotes, # NESE
    
    HydricSoils_score=#hydricsoils, #nearly complete
      case_when(hydricsoils %in% c("present","3")~3,
                hydricsoils %in% c("not_detected", "notdetected","not_assessed","0")~0, 
                is.na(hydricsoils)~0),
    HydricSoils_locations=locations, 
    HydricSoils_notes= hydricnotes,
    
    WoodyJams_number= case_when( # This will consolidate the two woody jams columns for NESE - needs to be fixed in consolidation script still
      is.na(woody_jams) ==T ~ woodyjams1,
      is.na(woodyjams1) ==T ~ woody_jams,
      T~0 # Replace missing wood jams count with zero
    ), 
    WoodyJams_number = case_when(is.na(WoodyJams_number)~0, T~WoodyJams_number),
    WoodyJams_source=case_when(is.na(woody_material)~trees_shrubs, 
                               T~woody_material),  # Same as trees_shrubs in other databases
    # WoodyJams_notes=woodyjamsnotes,# Not recorded for NESE
    
    SoilMoisture1=locationonemoisture, 
    SoilMoisture2=locationtwomoisture,
    SoilMoisture3=locationthreemoisture,
    SoilTexture1=locationonetexture,
    SoilTexture2=locationtwotexture,
    SoilTexture3=locationthreetexture,
    
    #BIOLOGICAL INDICATORS
    
    
    UplandRootedPlants_score= uplandrootedplants_score, # Both uplandrootedplants score and fiberousroots score ARE recorded for the NESE region (KSM)
    UplandRootedPlants_score2= case_when(UplandRootedPlants_score==3~0,
                                         UplandRootedPlants_score < 3~1),
    UplandRootedPlants_notes= uplandrootedplants_notes, 
    FibrousRootedPlants_score= fibrous_rootscore, # Actually, fibrous roots for NESE in ununified database
    FibrousRootedPlants_notes= fibrous_rootnotes, # Actually, fibrous roots for NESE in ununified database
    
    number_of_fish,#NESE
    Fish_UpTo3=case_when(number_of_fish =="0"~0,
                         number_of_fish =="1"~1,
                         number_of_fish =="2"~2,
                         number_of_fish =="3"~3,
                         number_of_fish ==">3"~3,
                         T~NA_real_),
    Mosquitofish = case_when(is.na(mosquitofish) & is.na(all_mosqfish)~"no",
                             mosquitofish=="yes" | all_mosqfish=="yes"~"yes",
                             T~"no"),
    # Mosquitofish = mosquitofish, #Not NESE
    # all_mosqfish,#NESE
    fishabund_note,#NESE
    Fish_score_NC = case_when(number_of_fish =="0"~0,
                              number_of_fish =="1"~0.5,
                              number_of_fish =="2"~1,
                              number_of_fish =="3"~1,
                              number_of_fish ==">3"~1.5,
                              T~NA_real_),
    Fish_score_NM = abundancescorefish , # Not recorded for NESE
    Fish_PA = case_when(number_of_fish>0~1,
                        number_of_fish==">3"~1,
                        Fish_score_NM>0~1,
                        is.na(Fish_score_NM) & is.na(number_of_fish)~0,
                        T~0), #Both East and Other regions
    Fish_PA_nomosq = (Fish_PA*(Mosquitofish=="no")), #Both East and Other regions
    Fish_UpTo3_nomosq = (Fish_UpTo3*(Mosquitofish=="no")), 
    BMI_score= abundancescorebenthic,# Not recorded for NESE
    BMI_presence = case_when(BMI_score>0~1,T~0),
    Algae_score= case_when(abundancealgae=="Not recorded"~NA_real_,
                           T~as.numeric(abundancealgae)), 
    
    
    
    #Mosquitofish = mosquitofish, # Not recorded for NESE
    #Vertebrate_notes= observedabundancenote, # Not recorded for NESE
    # IOFB_yn= observedfungi, 
    ironox_bfscore_NM = case_when(observedfungi=="present"~3,
                                  observedfungi %in% c("notdetected","notdectected")~0,
                                  is.na(observedfungi)~0,
                                  is.na(ironox_bfscore)~0,
                                  T~ironox_bfscore), 
    ironox_bfscore_PNW = case_when(ironox_bfscore_NM==3~1.5,
                                   ironox_bfscore_NM==0~0,
                                   T~NA_real_),
    ironox_bfnotes,#NESE
    Snakes_yn= case_when(is.na(observedsnakes)~"notdetected", T~observedsnakes), # Not recorded for NESE
    # Snakes_abundance= obsnakesabundance, # Not recorded for NESE
    
    Turtles_yn= observedturtles, # Not recorded for NESE
    # Turtles_abundance=obturtlesabundance, # Not recorded for NESE
    
    Amphibians_yn=observedamphibians, # Not recorded for NESE # Needs transformation from amphibian data
    # Amphibians_abundance= obampiabundance, # Not recorded for NESE #Not meaningful!
    #FrogVocalizations_yn= observedvocalfrogs, # Not recorded for NESE
    BiologicalIndicators_notes= observedabundancenote, # Not recorded for NESE
    AlgaeAbundance_NM = case_when(abundancealgae =="Not recorded"~NA_real_,
                                  T~as.numeric(abundancealgae)),
    AlgalCover_Live=case_when(streambedlive %in% c("notdetected")~0,
                              streambedlive %in% c("<2%")~1,
                              streambedlive %in% c("2 to 10%", "2-10%")~2,
                              streambedlive %in% c("10 to 40%", "10-40%")~3,
                              streambedlive %in% c(">40%")~4),
    AlgalCover_Dead=case_when(streambeddead %in% c("notdetected")~0,
                              streambeddead %in% c("<2%")~1,
                              streambeddead %in% c("2 to 10%", "2-10%")~2,
                              streambeddead %in% c("10 to 40%", "10-40%")~3,
                              streambeddead %in% c(">40%")~4),
    AlgalCover_LiveOrDead = case_when(AlgalCover_Live>=AlgalCover_Dead~AlgalCover_Live, T~AlgalCover_Dead),
    # case_when(AlgalCover_Live==">40%" | AlgalCover_Dead==">40%"~">40%",
    #           AlgalCover_Live=="10 to 40%" | AlgalCover_Dead=="10 to 40%"~"10 to 40%",
    #           AlgalCover_Live=="2 to 10%" | AlgalCover_Dead=="2 to 10%"~"2 to 10%",
    #           AlgalCover_Live=="<2%" | AlgalCover_Dead=="<2%"~"<2%",
    #           AlgalCover_Live=="Not detected" & AlgalCover_Dead=="Not detected"~"Not detected",
    #           T~"Other"
    # ),
    AlgalCover_Upstream=streambeddeadmats,
    AlgalCover_Live_NoUpstream = case_when(AlgalCover_Upstream=="yes"~0,T~AlgalCover_Live),
    AlgalCover_Dead_NoUpstream = case_when(AlgalCover_Upstream=="yes"~0,T~AlgalCover_Dead),
    AlgalCover_LiveOrDead = case_when(AlgalCover_Live>=AlgalCover_Dead~AlgalCover_Live, T~AlgalCover_Dead),
    AlgalCover_LiveOrDead_NoUpstream = case_when(AlgalCover_Live_NoUpstream>=AlgalCover_Dead_NoUpstream~AlgalCover_Live_NoUpstream, T~AlgalCover_Dead_NoUpstream),
    AlgalCover_notes= streambedalgaenotes, 
    
    dens_UU=u_upstream,
    dens_UL=u_left,
    dens_UR=u_right,
    dens_UD=u_downstream,
    dens_MU=m_upstream,
    dens_ML=m_left,
    dens_MR=m_right,
    dens_MD=m_downstream,
    dens_DU=l_upstream,
    dens_DL=l_left,
    dens_DR=l_right,
    dens_DD=l_downstream, 
    
    Moss_cover=#bryophytemosses, 
      case_when(bryophytemosses=="notdetected"~0,
                bryophytemosses=="<2%"~1,
                bryophytemosses=="2-10%"~2,
                bryophytemosses==">10%"~3,
                is.na(bryophytemosses)~0),
    Liverwort_cover=#bryophyteliverworts,
      case_when(bryophyteliverworts=="notdetected"~0,
                bryophyteliverworts=="<2%"~1,
                bryophyteliverworts=="2-10%"~2,
                bryophyteliverworts==">10%"~3,
                is.na(bryophyteliverworts)~0),
    Bryophyte_notes=bryophtyenotes,
    AlgaeAbundanceMossCover_Score=case_when(is.na(Algae_score)~NA_real_,
                                            is.na(Moss_cover)~NA_real_,
                                            Algae_score==0 & Moss_cover ==0 ~ 0,
                                            Algae_score>0  & Algae_score<=2 & Moss_cover <=2 ~ 0.5,
                                            Algae_score==0 & Moss_cover>0 & Moss_cover <=2 ~ 0.5,
                                            Algae_score==3~1,
                                            T~1),
    AlgaeAbundanceMossCover_nm_conversion=case_when(is.na(Algae_score)~NA_real_,
                                            Algae_score==0~0,
                                            Algae_score<=1~1,
                                            Algae_score>1~2),
    AlgaeAbundanceMossCover_algcover_conversion=case_when(is.na(AlgalCover_LiveOrDead)~NA_real_,
                                                          AlgalCover_LiveOrDead==0~0,
                                                          AlgalCover_LiveOrDead<=1~1,
                                                          AlgalCover_LiveOrDead>1~2),
    AlgaeAbundanceMossCover_mosscover_conversion=case_when(is.na(Moss_cover)~NA_real_,
                                                           Moss_cover==0~0,
                                                           Moss_cover<=1~1,
                                                           Moss_cover>1~2),
    DifferencesInVegetation_score=vegetationdifferencescore, 
    RiparianCorridor_score=DifferencesInVegetation_score,
    DifferencesInVegetation_notes=vegenotes, 
    NWPL_checklist= regionalindicators, 
    ai_fieldid, #NESE
    Additional_notes= additionalnotes,
    hydrovegenote #NESE
  ) %>%
  rowwise() %>%
  mutate(fp_entrenchmentratio_mean=mean(c(fp_entrenchmentratio1,fp_entrenchmentratio2,fp_entrenchmentratio3), na.rm=T),
         fp_entrenchmentratio_mean=case_when(fp_entrenchmentratio_mean>2.5~2.5, T~fp_entrenchmentratio_mean),
         ChannelDimensions_score_NC=case_when(fp_entrenchmentratio_mean<1.2~0,
                                              fp_entrenchmentratio_mean<2.5~1.5,
                                              fp_entrenchmentratio_mean>=2.5~3,
                                              T~NA_real_),
         BankWidthMean = mean(c(Bankwidth_0, Bankwidth_15, Bankwidth_30), na.rm = T),
         
         PctShading=mean(c(dens_UU/.17, dens_UL/.17, dens_UR/.17, dens_UD/.17,
                           dens_MU/.17, dens_ML/.17, dens_MR/.17, dens_MD/.17,
                           dens_DU/.17, dens_DL/.17, dens_DR/.17, dens_DD/.17), na.rm = T),
         AlgaeAbundanceMossCover_Score2 = max(AlgaeAbundanceMossCover_nm_conversion,AlgaeAbundanceMossCover_algcover_conversion, AlgaeAbundanceMossCover_mosscover_conversion, na.rm=T),
         ) %>%
  ungroup() %>%
  #Eliminate interim metrics used for calculating other metrics
  select(-Bankwidth_0, -Bankwidth_15, -Bankwidth_30,
         -fp_entrenchmentratio1, -fp_entrenchmentratio2, -fp_entrenchmentratio3, -fp_entrenchmentratio_mean,
         -starts_with("dens_")) %>%
  mutate(ChannelDimensions_score= case_when(!is.na(ChannelDimensions_score_NC)~ChannelDimensions_score_NC,
                                            !is.na(ChannelDimensions_score_NM)~ChannelDimensions_score_NM,
                                            T~NA_real_))

main_df %>% select(contains("moss"))
####Soil moisture
main_df$SoilMoisture1 %>% unique()

soil_moisture_df<-main_df %>%
  transmute(ParentGlobalID=ParentGlobalID,
            WaterInChannel_score = 2*baseflowscore,
            SoilMoisture1=case_when(WaterInChannel_score>0~2, MaxPoolDepth>0~2,
                                    SoilMoisture1=="dry"~0,
                                    SoilMoisture1=="partiallydry"~1,
                                    SoilMoisture1=="saturated"~2,
                                    T~NA_real_),
            SoilMoisture2=case_when(WaterInChannel_score>0~2, MaxPoolDepth>0~2,
                                    SoilMoisture2=="dry"~0,
                                    SoilMoisture2=="partiallydry"~1,
                                    SoilMoisture2=="saturated"~2,
                                    T~NA_real_),
            SoilMoisture3=case_when(WaterInChannel_score>0~2, MaxPoolDepth>0~2,
                                    SoilMoisture3=="dry"~0,
                                    SoilMoisture3=="partiallydry"~1,
                                    SoilMoisture3=="saturated"~2,
                                    T~NA_real_)) %>%
  pivot_longer(cols=c(SoilMoisture1, SoilMoisture2, SoilMoisture3)) %>%
  group_by(ParentGlobalID) %>%
  summarise(SoilMoist_MeanScore = mean(value, na.rm=T),
            SoilMoist_MaxScore = max(value, na.rm=T),
            SoilMeasures=sum(!is.na(value))
  ) %>% 
  ungroup() %>%
  select(-SoilMeasures)

has_moisture<-main_df %>% 
  filter(ParentGlobalID %in% soil_moisture_df$ParentGlobalID[is.na(soil_moisture_df$SoilMoist_MeanScore)]) %>%
  select(Database, ParentGlobalID, SiteCode, CollectionDate, SurfaceFlow_pct,SurfaceSubsurfaceFlow_pct, IsolatedPools_number) %>%
  filter(SurfaceFlow_pct>0 | SurfaceSubsurfaceFlow_pct>0 | IsolatedPools_number>0)

soil_moisture_df$SoilMoist_MeanScore[soil_moisture_df$ParentGlobalID %in% has_moisture$ParentGlobalID]<-2
soil_moisture_df$SoilMoist_MaxScore[soil_moisture_df$ParentGlobalID %in% has_moisture$ParentGlobalID]<-2

main_df<-main_df %>%
  left_join(soil_moisture_df %>% select(ParentGlobalID, SoilMoist_MeanScore,SoilMoist_MaxScore))

# soil_moisture_df %>% filter(is.na(SoilMoist_MeanScore)) %>%
#   left_join(main_df %>% select(Database, ParentGlobalID, SiteCode, CollectionDate) ) %>%
#   select(-ParentGlobalID, -starts_with("Soil")) %>% 
#   arrange(Database, SiteCode, CollectionDate) %>% 
#   clipr::write_clip()

#AMPHIBIANS
#Calculate amphibian p/a from NESE data
salamander_species<-c("Desmognathus","Desmognathus auriculatus","Desmognathus fuscus","Desmognathus monticola","Desmognathus ochrophaeus",
                      "Eurycea","Eurycea bislineata","Eurycea cirrigera","Eurycea longicauda","Eurycea wilderae",
                      "Gyrinophius", "Gyrinophius porphyriticus" ,"Plethodontidae","Plethodon", "Plethodon cinereus", "Urodela")
frog_species<-c("Acris","Acris crepitans","Acris crepitans blanchardi","Acris gryllus",
                "Anaxyurus", "Anaxyurus americanus","Anura","Eleutherodactylus",
                "Hyla","Hyla chrysoscelis",
                "Eleutherodactylidae",
                "Lithobates","Lithobates catesbeianus","Lithobates clamitans","Lithobates palustris","Lithobates pipiens","Lithobates sphenocephalus","Lithobates sphenocephalus utricularius","Lithobates sylvaticus",
                "Lithobates virgatipes","Lithobates hecksheri",
                "Incilius","Incilius nebulifer",
                "Pseudacris","Pseudacris brachyphona","Pseudacris fouquettei","Pseudacris nigrita","Ranidae" )
multiyear_tadpoles<-c("Lithobates catesbeianus", "Lithobates hecksheri","Lithobates virgatipes", "Lithobates clamitans")
setdiff(frog_species, multiyear_tadpoles)


amphib_df<-read_csv("https://sdamchecker.sccwrp.org/checker/download/amphibians-all") %>%
  transmute(
    ParentGlobalID = parentglobalid,
    AmphibGlobalID=globalid,
    SiteCode = am_sitecode, 
    Amphib_Species=amphibians_taxon, 
    Amphib_LifeStage=amphibians_lifestage, 
    Amphib_Abundance=amphibians_abundance, 
    Amphib_Notes=amphibians_notes
  )%>%
  mutate(Amphib_Species = case_when(is.na(Amphib_Species)~"None",
                                    Amphib_Species=="American toad"~"Anaxyurus americanus",
                                    Amphib_Species=="Plethodonitdae"~"Plethodontidae",
                                    T~str_remove(Amphib_Species,"\r\n")),
         Frog = Amphib_Species %in% frog_species,
         Salamander = Amphib_Species %in% salamander_species,
         Multiyear_Tadpole = Amphib_Species %in% multiyear_tadpoles 
  ) %>%
  filter(! Amphib_Species %in% c("None","None observed","Not recorded"))  
setdiff(amphib_df$Amphib_Species , c(frog_species, salamander_species, multiyear_tadpoles))
setdiff( c(frog_species, salamander_species, multiyear_tadpoles),amphib_df$Amphib_Species) 
setdiff(multiyear_tadpoles , frog_species)


amphib_mets<-amphib_df %>%
  group_by(ParentGlobalID) %>%
  summarise(Amphib_richness = Amphib_Species %>% unique() %>% length(),
            Amphib_abundance = sum(Amphib_Abundance, na.rm=T),
            Frog_richness = Amphib_Species[Frog] %>% unique() %>% length(),
            Frog_abundance = sum(Amphib_Abundance[Frog], na.rm=T),
            Salamander_richness = Amphib_Species[Salamander] %>% unique() %>% length(),
            Salamander_abundance = sum(Amphib_Abundance[Salamander], na.rm=T),
            Amphib_Juvenile_richness = Amphib_Species[Amphib_LifeStage!="A"] %>% unique() %>% length(),
            Amphib_Juvenile_abundance = sum(Amphib_Abundance[Amphib_LifeStage!="A"], na.rm=T),
            Salamander_Juvenile_abundance = sum(Amphib_Abundance[Salamander & Amphib_LifeStage %in% c("L","J")], na.rm=T),
            Multiyear_tadpole_abundance = sum(Amphib_Abundance[Multiyear_Tadpole & Amphib_LifeStage=="L"], na.rm=T)
  ) %>%
  right_join(main_df %>%
               filter(Region_DB=="East") %>%
               select(ParentGlobalID)) %>%
  mutate(Amphib_richness=case_when(is.na(Amphib_richness)~0,T~Amphib_richness),
         Amphib_abundance=case_when(is.na(Amphib_abundance)~0,T~Amphib_abundance),
         Frog_richness=case_when(is.na(Frog_richness)~0,T~Frog_richness),
         Frog_abundance=case_when(is.na(Frog_abundance)~0,T~Frog_abundance),
         Salamander_richness=case_when(is.na(Salamander_richness)~0,T~Salamander_richness),
         Salamander_abundance=case_when(is.na(Salamander_abundance)~0,T~Salamander_abundance),
         Amphib_Juvenile_richness=case_when(is.na(Amphib_Juvenile_richness)~0,T~Amphib_Juvenile_richness),
         Amphib_Juvenile_abundance=case_when(is.na(Amphib_Juvenile_abundance)~0,T~Amphib_Juvenile_abundance),
         Salamander_Juvenile_abundance=case_when(is.na(Salamander_Juvenile_abundance)~0,T~Salamander_Juvenile_abundance),
         Multiyear_tadpole_abundance=case_when(is.na(Multiyear_tadpole_abundance)~0,T~Multiyear_tadpole_abundance))
  

amphib_mets %>% skim_without_charts()
main_df<-main_df %>%
  left_join(amphib_mets %>% transmute(ParentGlobalID, 
                                      Amphib_count=Amphib_abundance,
                                      Amphib_abundance,
                                      Amphib_richness, Frog_richness,Frog_abundance,Multiyear_tadpole_abundance)) %>%
  mutate(Amphibians_yn = case_when(Amphibians_yn=="present"~"present",
                                   Amphib_count>0~"present",
                                   Amphibians_yn %in% c("notdetected","notdectected")~"notdetected",
                                   Amphib_count==0~"notdetected",
                                   is.na(Amphib_count) & is.na(Amphibians_yn)~"notdetected",
                                   T~"OTHER"),
         AmphSnake_PA = case_when(Amphibians_yn=="present"~1,
                                  Snakes_yn=="present"~1,
                                  is.na(Snakes_yn) & is.na(Amphibians_yn)~0,
                                  Snakes_yn %in% c("notdetected","notdectected") & Amphibians_yn %in% c("notdetected","notdectected")~0)
         ) %>%
  #Get rid of interim metrics
  select(-Amphib_count) %>%
  mutate(Amphibian_presence = case_when(Amphibians_yn=="present"~1,
                                         Amphibians_yn=="notdectected"~0,
                                         T~0))

# main_df$Amphibians_presence %>% skim()
#### HYDROVEG ####
hydroveg_df<- 
  read_csv("https://sdamchecker.sccwrp.org/checker/download/hydroveg-all") %>%
  transmute(
    ParentGlobalID = parentglobalid,
    VegGlobalID=globalid,
    SiteCode = hv_sitecode, 
    Plant_Species=hv_species, 
    Plant_Status=indicatorstatus, 
    Plant_flag=unusualdistro,
    InChannel = hv_inchannel, #NESE
    Plant_notes=hv_notes
  ) 
write_csv(hydroveg_df, "NotForGit/hydroveg_df.csv")
# hydroveg_df %>% group_by(Plant_Species) %>% tally()  %>%
#   clipr::write_clip()
# 
# hydroveg_df %>%
#   select(ParentGlobalID, Plant_Species, Plant_Status, Plant_notes) %>%
#   inner_join(main_df %>% 
#                select(ParentGlobalID, Region_DB, Database, SiteCode, CollectionDate)) %>%
#   filter(is.na(Plant_notes)) %>%
#   # filter(Plant_Species=="Not recorded")
#   # filter(Plant_Species=="Tri-finger leaf tree") %>%
#   filter(str_detect(Plant_Species,"Yellow"))   %>%
#   select(Database, SiteCode, CollectionDate) %>%
#   unique() %>% as.data.frame()


veg_metrics<-hydroveg_df %>%
  mutate(Plant_Species = str_trim(Plant_Species)) %>% # Remove carriage returns, leading white space and trailing white space
  mutate(Plant_Species = case_when(
    Plant_Species %in% c("NR","?") ~ "No plant",
    T~Plant_Species
  )) %>%
  mutate(Plant_Status = case_when(
    is.na(Plant_Status) == T ~ "nonhydrophyte",
    T~ Plant_Status),
    Plant_flag = case_when(
      is.na(Plant_flag) == T ~ "no",
      T~Plant_flag
    )) %>%
  mutate(hydrophyte = Plant_Status %in% c("hydrophyte", "facw", "obl/sav"),
         obl = Plant_Status %in% c("obl/sav"),
         flagged = Plant_flag=="yes") %>%
  # select(ParentGlobalID, Plant_Species, hydrophyte, obl, flagged, InChannel) %>%
  transmute(ParentGlobalID, Plant_Species, 
            hydrophyte = Plant_Status %in% c("hydrophyte","hydrophyte_facw","hydrophyte_obl","obl/sav","facw"), 
            flagged=Plant_flag=="yes") %>%
  # filter(hydrophyte) %>%
  unique()%>%
  group_by(ParentGlobalID) %>%
  summarise(plants_reported=length(Plant_Species),
            hydrophytes_present = sum(hydrophyte, na.rm=T),
            hydrophytes_present_noflag = sum(hydrophyte[!flagged], na.rm=T),
            
            hydrophytes_present_any = hydrophyte %>% any(na.rm=T) %>% sum(na.rm=T),
            hydrophytes_present_any_noflag = hydrophyte[!flagged] %>% any(na.rm=T) %>% sum(na.rm=T)
            
  ) %>% 
  ungroup()

main_df<-main_df %>%
  left_join(veg_metrics %>% select(ParentGlobalID, hydrophytes_present, hydrophytes_present_noflag, hydrophytes_present_any, hydrophytes_present_any_noflag)) %>%
  mutate(hydrophytes_present = case_when(is.na(hydrophytes_present)~0,T~hydrophytes_present),
         hydrophytes_present_any = case_when(is.na(hydrophytes_present_any)~0,T~hydrophytes_present_any),
         hydrophytes_present_any_noflag = case_when(is.na(hydrophytes_present_any_noflag)~0,T~hydrophytes_present_any_noflag),
         hydrophytes_present_noflag = case_when(is.na(hydrophytes_present_noflag)~0,T~hydrophytes_present_noflag))


#######AQUATIC INVERTEBRATES
#Calculated in 0.5_ai_cleanup.R
ai_metrics<-read_csv("Data/ai_metrics.csv")
interim_metrics<-data.frame(Metric = names(ai_metrics)) %>%
  filter(str_detect(Metric, "_West_") | str_detect(Metric, "_GP_") | str_detect(Metric, "_East_") | 
           str_detect(Metric, "_AW_") | str_detect(Metric, "_WM_") | 
           str_detect(Metric, "_NGP_") | str_detect(Metric, "_SGP_") |
           str_detect(Metric, "_NE_") |str_detect(Metric, "_SE_")) 
  

ai_mets<-setdiff(names(ai_metrics), c("ParentGlobalID",
                                      interim_metrics$Metric
                                      ))

ai_metrics2<-main_df %>%
  select(ParentGlobalID) %>%
  left_join(ai_metrics %>%
              select(ParentGlobalID, all_of(ai_mets)) %>%
              select(-SiteCode, -Region, -Region_detail)) %>%
  mutate(
    Ephemeroptera_SumOfIndividuals = case_when(Ephemeroptera_abundance==0~0, is.na(Ephemeroptera_abundance)~0,
                                               Ephemeroptera_abundance<=5~1,
                                               Ephemeroptera_abundance>5~2,
                                               T~NA_real_),
    Plecoptera_SumOfIndividuals = case_when(Plecoptera_abundance==0~0, is.na(Plecoptera_abundance)~0,
                                            Plecoptera_abundance<=5~1,
                                            Plecoptera_abundance>5~2,
                                            T~NA_real_),
    Trichoptera_SumOfIndividuals = case_when(Trichoptera_abundance==0~0, is.na(Trichoptera_abundance)~0,
                                             Trichoptera_abundance<=5~1,
                                             Trichoptera_abundance>5~2,
                                             T~NA_real_),
    Hemiptera_SumOfIndividuals = case_when(Hemiptera_abundance==0~0, is.na(Hemiptera_abundance)~0,
                                           Hemiptera_abundance<=5~1,
                                           Hemiptera_abundance>5~2,
                                           T~NA_real_),
    Coleoptera_SumOfIndividuals = case_when(Coleoptera_abundance==0~0, is.na(Coleoptera_abundance)~0,
                                            Coleoptera_abundance<=5~1,
                                            Coleoptera_abundance>5~2,
                                            T~NA_real_),
    Odonata_SumOfIndividuals = case_when(Odonata_abundance==0~0, is.na(Odonata_abundance)~0,
                                         Odonata_abundance<=5~1,
                                         Odonata_abundance>5~2,
                                            T~NA_real_),
    Decapoda_SumOfIndividuals = case_when(Decapoda_abundance==0~0, is.na(Decapoda_abundance)~0,
                                          Decapoda_abundance<=5~1,
                                          Decapoda_abundance>5~2,
                                          T~NA_real_),
    Basommatophora_SumOfIndividuals = case_when(Basommatophora_abundance==0~0,  is.na(Basommatophora_abundance)~0,
                                                Basommatophora_abundance<=5~1,
                                                Basommatophora_abundance>5~2,
                                          T~NA_real_),
    OtherMacro_SumOfIndividuals = case_when(OtherMacro_abundance==0~0, is.na(OtherMacro_abundance)~0,
                                            OtherMacro_abundance<=5~1,
                                            OtherMacro_abundance>5~2,
                                                T~NA_real_),
    EPT_SumOfIndividuals = case_when(EPT_abundance==0~0, is.na(EPT_abundance)~0,
                                     EPT_abundance<=5~1,
                                     EPT_abundance>5~2,
                                            T~NA_real_),
    OCH_SumOfIndividuals = case_when(OCH_abundance==0~0, is.na(OCH_abundance)~0,
                                     OCH_abundance<=5~1,
                                     OCH_abundance>5~2,
                                     T~NA_real_),
    TotalAbundance_SumOfIndividuals = case_when(TotalAbundance==0~0, is.na(TotalAbundance)~0,
                                                TotalAbundance<=5~1,
                                                TotalAbundance>5~2,
                                     T~NA_real_),
    EPT_SumOfIndividuals=Ephemeroptera_SumOfIndividuals+Plecoptera_SumOfIndividuals+Trichoptera_SumOfIndividuals,
    OCH_SumOfIndividuals=Odonata_SumOfIndividuals+Coleoptera_SumOfIndividuals+Hemiptera_SumOfIndividuals,
    MacroGroupsPresent = 
      (Ephemeroptera_SumOfIndividuals>0)+
      (Plecoptera_SumOfIndividuals>0)+
      (Trichoptera_SumOfIndividuals>0)+
      (Coleoptera_SumOfIndividuals>0)+
      (Odonata_SumOfIndividuals>0)+
      (Hemiptera_SumOfIndividuals>0)+
      (Basommatophora_SumOfIndividuals>0)+
      (Decapoda_SumOfIndividuals>0)+
      (OtherMacro_SumOfIndividuals>0),
    PerennialPNWMacroPresent = case_when(perennial_PNW_abundance>0~1,T~0),
      )

ai_metrics2[is.na(ai_metrics2)]<-0
ai_metrics2 %>% select(Ephemeroptera_abundance,Plecoptera_abundance, Trichoptera_abundance, MacroGroupsPresent)

ai_metrics2 %>% select(contains ("baso")) %>% skim()
main_df<-main_df %>%
  left_join(ai_metrics2) %>%
  mutate(BMI_presence = case_when(TotalAbundance>0~1,
                                  T~BMI_presence))

# #######GEOSPATIAL METRICS
# #Calculated in 0.5_gis_metric_calculations.R
# gis_metrics_df<-read_csv("Data/GISmetrics/COMPLETE_gis_metrics_df.csv")
# setdiff(main_df$SiteCode, gis_metrics_df$SiteCode)
# main_df<-
#   main_df %>%
#   inner_join(gis_metrics_df)

# drn_df<-read_csv("Data/drainage_areas.csv")
# setdiff(xwalk_df$sitecode, drn_df$SITECODE)
# xwalk_df%>%
#   filter(!sitecode %in% drn_df$SITECODE) %>%
#   select(sitecode, lat, long) %>%
#   clipr::write_clip()

#################

lumets<-read_csv("Data/metric_lookup.csv")
gis_metrics_df<-read_csv("Data/GISmetrics/COMPLETE_gis_metrics_df.csv")

junk<-lumets %>%  filter(MetricType!="Geospatial")
main_long<-main_df %>%
  inner_join(gis_metrics_df) %>%
  select(Region_DB, all_of(junk$Metric)) %>%
  pivot_longer(cols=all_of(junk$Metric))

main_long %>%
  group_by(Region_DB, name) %>%
  summarise(n_tot = length(value),
            length_not_na = sum(!is.na(value))) %>%
  ungroup() %>%
  mutate(pct_complete = length_not_na/n_tot) %>%
  select(-n_tot, -length_not_na) %>%
  mutate(Region_DB=paste0(Region_DB,"_Complete")) %>%
  pivot_wider(names_from=Region_DB, values_from = pct_complete) %>%
  rename(Metric=name)%>%
  right_join(junk) %>%
   write_clip()




write_csv(main_df, "NotForGit/Step1/main_df_step1.csv")
write_csv(xwalk_df, "NotForGit/Step1/xwalk_df.csv")

###################
regionalizations<-c("beta_region", "ohwm_region", "corps_region","all_region")

test_mets<-c(BioPreds, GeomorphPreds,HydroPreds_Indirect,
             "ppt.234", "ppt.567", "ppt.8910", "ppt.11121", 
             "temp.234", "temp.567", "temp.8910", "temp.11121",
             "Elev_m",  "MeanSnowPersistence_10") 
test_mets<-setdiff(test_mets, c("Fish_score_NM","Fish_PA_"))

rf_dat<-xwalk_df %>% 
  rename(SiteCode=sitecode) %>% 
  inner_join(main_df) %>%
  select(SiteCode, Class, CollectionDate, 
         all_of(regionalizations),
         all_of(test_mets)) %>%
  filter(Class!="U") %>%
  droplevels() %>%
  filter(beta_region!="Caribbean") %>%
  na.omit()

skim_without_charts(rf_dat)

rf_dat_long<-rf_dat %>%
  pivot_longer(cols=all_of(regionalizations), names_to = "Regionalization",values_to = "Region_id") 

rf_sum<- rf_dat_long %>% select(Regionalization, Region_id) %>% unique() %>% arrange(Regionalization, Region_id)

library(randomForest)
rf_dat$Class %>% unique()

my_rfs_dat<-lapply(1:nrow(rf_sum), function(i){
  regz.i=rf_sum$Regionalization[i]
  reg.i=rf_sum$Region_id[i]
  xdf=rf_dat_long %>%
    filter(Regionalization==regz.i & Region_id==reg.i) %>%
    select(Class, all_of(test_mets)) 
  # print(paste(regz.i, reg.i))
  # xdf %>%
  #   group_by(Class) %>%
  #   tally() %>%
  #   print()
  # randomForest(Class~., importance=T, na.action = na.roughfix, data=xdf)
})

my_rfs_models<-lapply(1:nrow(rf_sum), function(i){
  xdf<-my_rfs_dat[[i]]
  regz.i=rf_sum$Regionalization[i]
  reg.i=rf_sum$Region_id[i]
  print(paste(regz.i, reg.i))
  xdf %>%
    group_by(Class) %>%
    tally() %>%
    print()
  randomForest(Class~., importance=T, na.action = na.roughfix, data=xdf)
})

rf_sum$P <-sapply(my_rfs_dat, function(x){  x %>%    filter(Class=="P") %>%    nrow() })
rf_sum$I <-sapply(my_rfs_dat, function(x){  x %>%    filter(Class=="I") %>%    nrow() })
rf_sum$E <-sapply(my_rfs_dat, function(x){  x %>%    filter(Class=="E") %>%    nrow() })
rf_sum$N <-sapply(my_rfs_dat, function(x){  x %>%        nrow() })
rf_sum$ErrRate_all<-  sapply(my_rfs_models, function(x){ x$err.rate %>% tail(1)["OOB"]})
rf_sum$ErrRate_P<-  sapply(my_rfs_models, function(x){ x$err.rate %>% tail(1)["P"]})
rf_sum$ErrRate_I<-  sapply(my_rfs_models, function(x){ x$err.rate %>% tail(1)["I"]})
rf_sum$ErrRate_E<-  sapply(my_rfs_models, function(x){ x$err.rate %>% tail(1)["E"]})

junk<-my_rfs_models[[1]]
junk$importance %>% 
  as.data.frame() %>%
  mutate(Regionalization="regz.i",
         Region_id="reg.i",
         Metric=row.names(junk$importance)) %>%
  rename(P_imp=P, I_imp=I, E_imp=E)

# rf_sum_importance<-crossing(rf_sum, tibble(Metric=test_mets))
# rf_sum_importance$Importance_Acc<-sapply()

rf_sum_importance<-lapply(1:nrow(rf_sum), function(i){
  regz.i=rf_sum$Regionalization[i]
  reg.i=rf_sum$Region_id[i]
  mod.i=my_rfs_models[[i]]
  # print(mod.i)
  xmat=mod.i$importance
  xdf=xmat %>%
    as_tibble() %>%
    mutate(Regionalization=regz.i,
           Region_id=reg.i,
           Metric=row.names(xmat)) %>%
    rename(P_imp=P, I_imp=I, E_imp=E)
  
}) %>% bind_rows() %>%
  mutate(MetricType=case_when(Metric %in% GeomorphPreds~"Geomorphic",
                              Metric %in% GISPreds~"GIS",
                              Metric %in% HydroPreds_Indirect~"Hydro",
                              Metric %in% BioPreds~"Bio",
                              T~"Other" ),
         RegLabel = paste(regionalizations, Region_id, sep="-"))

rf_sum_importance %>% group_by(MetricType) %>% tally()
ggplot(rf_sum_importance %>%
         mutate(MeanDecreaseAccuracy=case_when(MeanDecreaseAccuracy<0~0,T~MeanDecreaseAccuracy),
                Region_id = gsub("USACE ","", Region_id)), 
       aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
  geom_tile()+
  scale_fill_viridis_c(trans="sqrt")+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(.~Regionalization, scales="free_x", space="free")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))

rf_sum_importance_long <-rf_sum_importance %>%
  # rename(All_imp=MeanDecreaseAccuracy) %>%
  pivot_longer(cols=c(P_imp, I_imp, E_imp))

ggplot(rf_sum_importance_long, aes(x=Region_id, y=Metric, fill=value))+
  geom_tile()+
  scale_fill_viridis_c()+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(name~Regionalization, scales="free", space="free")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))

rf_sum_plotdat<-rf_sum %>%
  mutate(Weight = N/3165) %>%
  select(Regionalization, Region_id, Weight, starts_with("ErrRate_")) %>%
  rename(P=ErrRate_P, I=ErrRate_I, E=ErrRate_E, Overall=ErrRate_all) %>%
  pivot_longer(cols=c("E","I","P","Overall")) %>%
  mutate(name=factor(name, levels=c("Overall","P","I","E")))
ggplot(data=rf_sum_plotdat, aes(x=Region_id, y=value))+
  geom_point(aes(color=name))+
  geom_point(data = . %>% filter(name=="Overall"),
             aes(color=name), size=3)+
  geom_hline(data= rf_sum_plotdat %>%
               filter(name=="Overall") %>%
               group_by(Regionalization) %>%
               summarise(WeightedMean = weighted.mean(value, Weight)),
             aes(yintercept=WeightedMean),
             linetype="dashed"
  )+
  facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  scale_color_brewer(palette="Set1", name="Error rate")+
  ylab("Error rate")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))
