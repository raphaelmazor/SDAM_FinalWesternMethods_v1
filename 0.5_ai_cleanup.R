library(tidyverse)

#Import xwalk<-
xwalk_df<-read_csv("Data/master_site_class_xwalk_08012023_coordinates_REGIONS.csv") %>%
  mutate(Region_detail2 = case_when(Region_detail %in% c("GP_C","GP_N","GP_S","GP_U")~"GP",
                                    T~Region_detail) %>%
           factor(levels=c("AW","WM","GP","NE","SE", "CB")),
         Class=factor(Class, levels=c("P","I","E","U")))

EastDBs<-c("NESE Baseline Revisits v2", "NESE Baseline v1", "NESE Validation v1")
WestDBs<-c("WMBR_1_1","WEx_SDAM_0","WMBR_2","WMV_1","FD003","FD004")
GPDBs<-c("Great Plains Baseline Revisits v3",  "Great Plains SDAM v3",  "Great Plains Validation_v2", "Great Plains Baseline Revisits v2_1_October", "GreatPlains_SDAM_V3_edits")
# xwalk_df$Region_detail2<-factor(xwalk_df$Region_detail2, levels=c("AW","WM","GP","NE","SE", "CB"))
#Import main_df
#### MAIN - Read in main site data table and filter for GP sites ####
library(skimr)


main_raw<-read_csv("https://sdamchecker.sccwrp.org/checker/download/main-all")
main_df<- main_raw %>% #read_csv("https://sdamchecker.sccwrp.org/checker/download/main-all") %>%
  # filter(origin_database %in% myDBs) %>%
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
    SiteName = sitename,
    Assessors = assessor,
    Recorder= recorder, QA=qa,
    CollectionDate= collectiondate)



#######AQUATIC INVERTEBRATES
ai_df<-
  read_csv("https://sdamchecker.sccwrp.org/checker/download/aquatic_invertebrates-all") %>%
  transmute(
    ParentGlobalID = parentglobalid,
    AIGlobalID=globalid,
    # SiteCode = ai_sitecode, 
    AI_Taxon=taxon,
    AI_Lifestage=lifestage,
    AI_LiveDead=ai_living, 
    AI_Abundance=case_when(ai_abundance<0~NA_real_, T~ai_abundance), #Negative values are actually blanks
    AI_Notes= ai_notes) %>%
  filter(AI_Taxon != "Na" & AI_Taxon != "na" & !(is.na(AI_Taxon))) %>%
  inner_join(main_df %>%
               select(ParentGlobalID, SiteCode, CollectionDate))

#Exclusions
excluded_bugs<-c(
  "Exclude","No invertebrates observed",
  #Terrestrial
  "Aphididae", "Aphrophoridae","Araneae","Arionidae","Braconidae",
  "Chloropidae","Chrysididae","Chrysopidae","Cicadellidae","Cicadidae","Diplopoda",
  "Diprionidae","Halictidae","Ithonidae","Limacidae","Lithobiidae","Lygaeidae","Meinertellidae",
  "Melyridae","Miridae","Mycetophilidae","Pentatomidae","Ptiliidae","Salpingidae","Sciaridae","Stylommatophora",
  "Zygentoma","Formicidae","Crabronidae","Thripidae","Coreidae","Vespidae","Noctuidae",
  "Crambidae", "Hymenoptera","Oxychilidae","Syrphidae",
  "Cyphoderinae",
  "Talitridae",
  "Oniscidae", # ADDED BY KSM DEC 22
  "Oniscidea", # ADDED BY KSM DEC 22
  "Terrestrial beetle", "Terrestrial beetle sp","Terrestrial snail",# ADDED BY KSM DEC 22
  "Terrestrial Lepidoptera",
  "Terrestrial sp", # Added 9/26 KSM
  "Terrestrial crustacean",
  #Saline
  "Canacidae", #Tidal flies
  # "Mysidae", #Mysid shrimp
  #Mosquitos
  "Culicidae"
)

#Opt to exclude semi-aquatic, surface dwellers, etc
semi_aquatic<-c(
  #Beetles
  "Carabidae", "Staphylinidae","Georissidae","Chrysomelidae", "Curculionidae",
  #Hemiptera
  "Veliidae", "Gerridae", "Mesoveliidae","Saldidae","Gelastocoridae","Macroveliidae",
  #Other
  "Talitridae"
)

# CLEANING BASED ON CURRENT AND PRIOR DATASETS

ai_combined <- ai_df %>%
  mutate(AI_Taxon = trimws(AI_Taxon,whitespace = "[ \t\r\n]"), # Remove trailing and leading white spaces
         AI_Taxon = str_replace(AI_Taxon, " sp.","")) %>% #Remove sp.
  mutate(
    AI_Taxon2=case_when(#This is a "cleaned" name based on Taxon 1, but reflects no change to taxonomic resolution
      AI_Taxon == "Simulliidae" ~ "Simuliidae", 
      AI_Taxon %in% c("Small red mite","Mite","Red mite","Arenuridae","Unionicolidae","Hydryphantidae",
                      "Pionidae","Torrenticolidae","Mideopsidae","Arrenuridae","Lebertidae",
                      "Acalyptonotidae","Hydrophantidae","Oribatei","Trombidiformes",
                      "Limnesiidae","Lebertiidae","Hygrobatidae","Sperchontidae",
                      "Hydrocarina","Thyasidae","Spherchontidae")~"Acariformes",
      AI_Taxon %in% c("Aeshniidae","Aeshindae",
                      "Odonata aeshnidae","Odonata Aeshnidae",
                      "aeshnidae", 
                      "Aeshinidae")~"Aeshnidae", 
      AI_Taxon %in% c("Arachniddae","Arachnid",
                      "Achnidae")~"Arachnida",
      AI_Taxon %in% c("Ascellidae","asselidae",
                      "asellidae")~"Asellidae",
      AI_Taxon %in% c("Scuds",
                      "amphipoda", "Amphopoda","Amphipod sp","Amphipod spp",
                      "Amphipod spp.","Amphipodae",
                      "Amohipoda")~"Amphipoda", 
      AI_Taxon %in% c("Odonata Antisoptera", "Dragonfly",
                      "Anisoptera sp")~"Anisoptera",
      
      AI_Taxon %in% c("Ephemeroptera Ameletidae","ameletidae")~"Ameletidae", 
      AI_Taxon %in% c("Hemiptera belostomatida","Hemiptera Belostomatidae","belostomaridae","Belostomaridae",
                      "Belastomatidae","belostomatidae",
                      "Hemiptera belostomatidae")~"Belostomatidae", 
      AI_Taxon %in% c("Bastiscidae","Baetiscidae","Baestiscidae")~"Baetiscidae",
      AI_Taxon %in% c("Caecidotea sp")~"Caecidotea",
      AI_Taxon %in% c("Startiomyidae","Stratiomyiidae")~"Stratiomyidae",
      AI_Taxon %in% c("Canidae","Ceanidae")~"Caenidae",
      AI_Taxon %in% c("Erobdellidae")~"Erpobdellidae",
      AI_Taxon %in% c("Calopterygidae (Calopteryx)","Calyopterygidae",
                      "calopterygidae","Calopteryx")~"Calopterygidae",
      #AI_Taxon %in% c("ceratapogonidae","Ceratopogonidae","Ceratoponidae")~"Ceratapogonidae", # THIS WAS SPELLED WRONG
      AI_Taxon %in% c("ceratapogonidae","Ceratapogonidae","Ceratoponidae")~"Ceratopogonidae", # Corrected the above spelling 9/26 (KSM)
      AI_Taxon %in% c("Copapod","Copapods")~"Copepoda",
      AI_Taxon %in% c("Beetle","Unknown Coleoptera", "Coleoptera + Coleoptera larvae","Coleoptera exuvia","Coleoptera larva","Coleoptera larva?","Coleoptera larvae",
                      "Part of Coleoptera larvae", "Coleoptera spp unknown","Coleoptera spp","Hemiptera and Coleoptera","Small black beetle- coleoptera",
                      "Small beetle with ridges", "Small ridged beetle- coleoptera","Small black beetle","Coleoptera unknown",
                      "Unknown beetle")~"Coleoptera",
      
      AI_Taxon %in% c("Bivalve spp?","Bivalves","Bivalve?")~"Bivalve",
      AI_Taxon %in% c("Brachycentidae","Bracycentridae","Brachycentridae",
                      "Trichoptera brachycentridae",
                      "brachycentridae")~"Brachycentridae",
      AI_Taxon %in% c("Bulk", "Bulk 1", "Bulk 2","bulk sample","Bulk sample", "Bulk Sample","BULK SAMPLE","Bulk sample #1",
                      "Bulk sample #2","Bulk sample #3","Bulk sample #4","Bulk sample 1","Bulk Sample 1","Bulk sample 2", "Bulk sample1" )~"Bulk sample",
      
      AI_Taxon %in% c("Blood worm","Chironomid", "Diptera chironomidae","Diptera, Red midge","Red midge","Red midges",
                      "Diptera- midge","Diptera chironomidae","Diptera, Red midge","Diptera -midge","Midge larvae",
                      "Midge spp unknown","Midge larvae","Midge?","Midges","Small black worm/ midge","Tan midge", 
                      "Diptera spp","Diptera spp.","ChironomidaeIAUVIA","Chironimidae","Chironomidae (pupae)",
                      "Tanypodinae","Midge unknown",
                      "Chironomiidae","Chrinonomidae","Chronomidae",
                      "chironomidae")~"Chironomidae", # Added 9/26 KSM
      
      AI_Taxon %in% c("Trichoptera calamoceratidae","Trichoptera Calamoceratidae" )~"Calamoceratidae",
      AI_Taxon %in% c("Odonata calopterygidae", "Calyopterigidae")~"Calopterygidae", 
      AI_Taxon %in% c("Coenegrionidae",
                      "Odonata Coenagriondae", "Zygoptera (Coenagrionidae)","Coenagrinidae","Coengarionidae")~"Coenagrionidae", 
      AI_Taxon %in% c("Corduligastridae",
                      "Odonata cordulegastridae",
                      "Cordulegasridae")~"Cordulegastridae", 
      AI_Taxon %in% c("Caddis fly","Caddis pupa", "Caddis pupae", "Caddisdly","Caddisflies","Caddisfly", "Pupa and trichoptera case","Possibly a caddisfly",
                      "Trichoptera and cases","Trichoptera case","Trichoptera cases","Trichoptera cases ?","Trichoptera cases and unknown larva",
                      "Trichoptera cases?3","Trichoptera casings","Trichoptera pupae","Tricoptera","Woody case","Large clear amber casing, spp unkn",
                      "trichoptera", "Trichoptera sp", "trichoptera sp (empty case)","Putative caddis","Caddis pupa, possibly",
                      "Trichoptera sp (pupae in case)","Unknown caddis", "Caddisfly case",
                      "Caddisfly cases", "Unknown Caddis 1", "Unknown caddis 2",
                      "trichoptera case", 
                      "Dipseudopsidae", # Added 9/26 KSM - Likely not in SAFIT but reported for one NESE sample
                      "Unknown cased caddis", "Cased caddis")~"Trichoptera", 
      
      AI_Taxon %in% c( "Clam (corbela?)","Corbicula","Fingernail clam",
                       "Corbivula")~"Corbiculidae",
      AI_Taxon %in% c( "Crawfish","Crayfish","Lobster species","crayfish - red swamp","Lobstercies")~"Cambaridae",
      AI_Taxon %in% c( "Crustacean arm","Crustacean")~"Crustacea", # Added 9/26 KSM
      AI_Taxon %in% c( "Anostraca")~"Anostraca", # Added 9/26 KSM
      AI_Taxon %in% c( "Cullicidae","Mosquitos","Culisidae",
                       "Cuclicidae")~"Culicidae",
      AI_Taxon %in% c( "Diptera -unknown spp","Dipterans","Fly larvae", "Unknown diptera","Diptera.",
                       "fly larvae","Diptera -unknown","Diptera",
                       "Bracycera","Brachycera", "Unknown dipteran", "Unknown fly larva")~"Diptera",
      AI_Taxon %in% c( "Diptera dixidae")~"Dixidae",
      AI_Taxon %in% c( "Peltoperlidae","Peltperlidae" )~"Peltoperlidae",
      AI_Taxon %in% c( "Athericidae")~"Athericidae",
      AI_Taxon %in% c( "Diptera simuliidae","Diptera: black fly larvae", "Blackfly larvae - Simuliidae",
                       "Simulidae", "Sinubidae","simulidae","simuliidae","Blackfly larvae - Simmuliidae",
                       "Black Fly","blackfly",
                       "Simullidae")~"Simuliidae", 
      AI_Taxon %in% c( "Dobsonfly")~"Corydalidae",
      AI_Taxon %in% c( "Dreissenidae")~"Dreissenidae",
      AI_Taxon %in% c( "Corydalidae (N-P)","Corydalidae (Neohermes-Protochauliodes group)")~"Corydalidae (Neohermes-Protochauliodes group)",
      AI_Taxon %in% c( "Corydalidae - Orohermes","Corydalidae (Oreohermes-Dysmicohermes group)","Corydalus (O-D group)","Corydalidae (O-D group)",
                       "Corydalidae (orohermes group)","Corydalidae orohermes",
                       "Orohermes")~"Corydalidae (Orohermes-Dysmicohermes group)", # Added 9/26 KSM 
      AI_Taxon %in% c( "Corydalidae (Corydalus)", "Corydalus sp.", 
                       "Corydalis")~"Corydalus", #NESE
      AI_Taxon %in% c( "Corydalidae (Nigronia)","Corydalidae nigronia")~"Nigronia", 
      
      AI_Taxon %in% c( "Ephemeroptera-Ephemerellidae","Ephemeroptera Ephemerellidae",
                       "Emphemerellidae", "Ephemerelidae",
                       "Ephemeroptera ephemerellidae", 
                       "Ephemerrellidae")~"Ephemerellidae", 
      
      AI_Taxon %in% c( "Elmidae (adult)","Coleoptera elmidae")~"Elmidae", 
      AI_Taxon %in% c( "Ephermeridae")~"Ephemeridae", 
      
      AI_Taxon %in% c( "Ephemeroptera exuvia","Ephemeroptera spp","Ephemeroptera spp","Ephemeroptera spp unkn","Mayfly, non-baetidae 1","Mayfly, non-Baetidae 2",
                       "Ephemeroptera? Exuvia","Mayflies","Mayfly","Small mayfly with gills",
                       "Ephemeroptera sp", "ephemeroptera sp","mayfly","Ephemeroptera unkn",
                       "Unknown mayfly")~"Ephemeroptera",
      
      AI_Taxon %in% c( "Ephemeroptera heptagiidae","Ephemeroptera Heptageniidae",
                       "Ephemeroptera heptageniidae")~"Heptageniidae", 
      AI_Taxon %in% c( "Fallceon and Baetis adonis","Fallceon and Baetis adonis and Tricorythodes",
                       "Ephemeroptera Baetidae", "baetidae","Baetiidae")~"Baetidae", 
      AI_Taxon %in% c( "Garridae","Water strider","Water striders",
                       "Hemiptera gerridae")~"Gerridae", 
      AI_Taxon %in% c( "Gastropoda shell","Gastropoda Shell","Gastropoda shells","Spiral snail","Snail","Snail 1","Aquatic snail",
                       "Snail 2","Snail/limpet?","Snails")~"Gastropoda", 
      AI_Taxon %in% c( "Glossosomelidae","Glosso stomadidae")~"Glossosomatidae", 
      AI_Taxon %in% c( "Glossosomatidae (Glossoma)")~"Glossoma", 
      AI_Taxon %in% c( "Gomphiidae", "Odonata Gomphidae","gomphidae")~"Gomphidae", 
      AI_Taxon %in% c( "Gryinidae","Coleoptera Gynnidae")~"Gyrinidae", 
      AI_Taxon %in% c( "Halipidae")~"Haliplidae", 
      AI_Taxon %in% c( "Tricoptera-glossosomatidae")~"Glossosomatidae",
      AI_Taxon %in% c( "Helicopsycidae","Heliopsychidae")~"Helicopsychidae",
      AI_Taxon %in% c( "Hydropsyche occidentalis",
                       "Hydro physidae", "Hydrophychidae", "Hydrophycidae","Hydrophyschidae",
                       "Hydropsididae","Hydrosychidae", "Hyrdopsychidae", "Trichoptera Hydropyschidae", "Trichoptera hydropsychidae",
                       "Trichoptera hydrosychidae","hydropsychidae")~"Hydropsychidae",
      AI_Taxon %in% c( "Hyallelidae")~"Hyalellidae", 
      AI_Taxon %in% c( "Hydrobiid")~"Hydrobiidae", 
      AI_Taxon %in% c( "Hemipteran")~"Hemiptera",
      AI_Taxon %in% c( "Hydrochidae")~"Hydrochidae",
      AI_Taxon %in% c( "Heptegeniidae",
                       "Haptageniidae","Heptagenidae","Heptigeniidae")~"Heptageniidae",
      AI_Taxon %in% c( "Large dytiscidae","Predacious diving beetle","Small dytiscidae","Large dytiscid",
                       "Dyticidae", "Dytiscid", "Dytiscidae (adult)","Dytisidae","Diticidae beetle",
                       "Dytiscidae (larva)","Dytiscidae (larvae)","Large dystiscidae",
                       "Coleoptera dystiscidae","Coleoptera Dytiscidae")~"Dytiscidae",
      AI_Taxon %in% c( "Leach",
                       "Hirudinae", "Hirundinea")~"Hirudinea", 
      AI_Taxon %in% c( "Isonychidae")~"Isonychiidae", 
      AI_Taxon %in% c( "Isopod","Isopods")~"Isopoda", 
      AI_Taxon %in% c( "Lepidodoptera","Lepidoptera (larvae)")~"Lepidoptera", 
      AI_Taxon %in% c( "Leptoceridae (Ceraclea)")~"Ceraclea", 
      AI_Taxon %in% c( "Leptophleniidae","Leptophylebiidae", "leptophlebiidae")~"Leptophlebiidae", 
      AI_Taxon %in% c( "Libuellidae","libellulidae")~"Libellulidae", 
      AI_Taxon %in% c( "Limnaeidae","lymaeidae","Lymnadae","Lymnaedae","Lymnaeid","lymnaeidae",
                       "Lymnaidae")~"Lymnaeidae", 
      AI_Taxon %in% c( "Limnphild","Lmniphild","Limephilidae","Lmniphilid","Lmniphillid","Limnephilidae/phryganeidae","Trichoptera limnephilidae",
                       "Limnophilidae","Limnephillidae","Tricoptera limnephilidae")~"Limnephilidae",
      AI_Taxon %in% c("Mysida","Mysidae")~"Mysidae",
      AI_Taxon %in% c( "Limnephilidae (Onocosmoecus)")~"Onocosmoecus", 
      
      AI_Taxon %in% c( "Hydrophyilidae",
                       "Hydrophildae", "Hydrophilidae (adult)", "Hydrophilidae (larva)",
                       "Hydrophillidae","Hydrophylidae")~"Hydrophilidae", 
      AI_Taxon %in% c( "Hydrachmida","Hydrachnida","Hydracarina" )~"Hydrachnidia",
      
      AI_Taxon %in% c( "N/A","NA", "No bugs","No Bugs", "NO BUGS","No invertebrates","No taxon found","None","No invertebrates observed",
                       "None found.","None seen","None." ,"No invertebrates observed",
                       "No aquatic invertebrates observed", "No benthic macroinvertebrates",
                       "No benthic macroinvertebrates.","No aquatic invertebrates",
                       "No Invertebrates", "No inverts found", # NESE
                       "None observed", "none", "Unknown")~"Exclude", #NESE
      AI_Taxon %in% c( "Nemouriidae","nemouridae" )~"Nemouridae",
      AI_Taxon %in% c( "Leptaceridae" )~"Leptoceridae",
      AI_Taxon %in% c( "Amphipoda","Amphipod","Amphipod." ,"amphipod")~"Amphipoda",
      AI_Taxon %in% c( "Chloroperlidae","Choloperlidae" )~"Chloroperlidae",
      AI_Taxon %in% c( "Nepidae (Ranatra)" )~"Ranatra", 
      AI_Taxon %in% c( "Notonectidae  spp","Notonectidae ",
                       "Nonectidae")~"Notonectidae", 
      AI_Taxon %in% c( "Nauchoridae",
                       "Naucordiae")~"Naucoridae",
      AI_Taxon %in% c(  "Odoceridae", "Odontocenidae")~"Odontoceridae", 
      AI_Taxon %in% c( "Odanata","Odanata spp","Odonata","Odonata exuvia","Odonata spp","Odonata spp unknown",
                       "Odonate","Odonates","Odonata unknown","Xygoptera",
                       "Libellulidae/cordulidae", "Libellulidae/Corduliidae", "Odonata sp.","Damselfly",
                       "Odonata sp", "Unknown zygoptera")~"Odonata",
      AI_Taxon %in% c(  "Oligicheates","Oligochaete","Oligochete","Oligachaete",
                        "Oligiochaeta","Oligochaeta annelida","oligochaeta","Glossiphoniidae",
                        "Annelida oligochaeta")~"Oligochaeta",
      AI_Taxon %in% c(  "Ostracod","Ostrapoda","Ostracods" )~"Ostracoda", 
      AI_Taxon %in% c(  "Ephemerellidae","Ehphmerelidae" )~"Ephemerellidae", 
      AI_Taxon %in% c( "Palaemonidae" )~"Palaemonidae", 
      AI_Taxon %in% c( "Anthericidae","Athericidae" )~"Athericidae", 
      AI_Taxon %in% c( "Parastacidae" )~"Parastacidae", 
      AI_Taxon %in% c( "Ephyrididae" )~"Ephydridae", 
      AI_Taxon %in% c( "Pelodidae", "Perlodid?" )~"Perlodidae",
      AI_Taxon %in% c( "Philopotemidae" )~"Philopotamidae",
      AI_Taxon %in% c( "Physid","Phyidae","Physdiae","plysidae","physidae" )~"Physidae",
      AI_Taxon %in% c( "Pisidae", "Pisiidae", "Pisidiidae","Sphaeridae", "Spheriidae","sphaeriidae","Shaeriidae",
                       "Sphaerridae", "Spheridae",
                       "Pisidium")~"Sphaeriidae", # Added 9/26 KSM
      AI_Taxon %in% c("Planorbid","Planoridae","planorbidae")~"Planorbidae",
      
      AI_Taxon %in% c( "Plecoptera exuvia", "Stonefly","Stoneflies","Stonefly?","Stonefly larvae",
                       "Stoneflys" ,"Plecoptera spp")~"Plecoptera",
      AI_Taxon %in% c( "Plecoptera: Golden stonefly?","Pearlidae" ,
                       "Plecoptera perlidae")~"Perlidae",
      AI_Taxon %in% c( "Pleoroceridae","Pleuroceridae" )~"Pleuroceridae",
      AI_Taxon %in% c("polycentropodidae","Polycentropidae" )~"Polycentropodidae",
      AI_Taxon %in% c("Polycentropodidae (Nyctiophylax)" )~"Nyctiophylax",
      AI_Taxon %in% c("Polymitarcyidae" )~"Polymitarcyidae",
      AI_Taxon %in% c("Vellidae" )~"Veliidae",
      AI_Taxon %in% c("Dolichopididae","Dolochopodidae","Dolicopodidae" )~"Dolichopodidae",
      AI_Taxon %in% c("Pleidae (Neoplea)" )~"Neoplea",
      AI_Taxon %in% c("Pleurocenidae","Pleuroceridae" )~"Pleuroceridae",
      AI_Taxon %in% c( "Psenidae","Psephidae","Psenphenidae","Psphenidae")~"Psephenidae",
      AI_Taxon %in% c("Rhyalophilidae","Rhyacophildae")~"Rhyacophilidae",
      AI_Taxon %in% c( "Siphlonuridae?","Siplonuridae" )~"Siphlonuridae",
      AI_Taxon %in% c( "Blephariceridae","Blepharicaeridae" )~"Blephariceridae",
      AI_Taxon %in% c( "Leptohyphidae exuvia","Leptohydhidae","Leptohyphidae",
                       "Leptohyphyidae","Leptohypidae" )~"Leptohyphidae",
      AI_Taxon %in% c( "Megaloptera sialidae","Megaloptera Sialidae" )~"Sialidae",
      AI_Taxon %in% c( "Sialidae (Sialis)" )~"Sialis",
      AI_Taxon %in% c( "Syrphidae" )~"Syrphidae",
      AI_Taxon %in% c( "elmidae","Elmidae exuvia" )~"Elmidae",
      AI_Taxon %in% c( "Taberidae" )~"Tabanidae",
      AI_Taxon %in% c( "Cnidaria","Hydra","Hydridae" )~"Cnidaria",
      AI_Taxon %in% c( "Tipuliidae","tipulidae","Tipula","Tipula sp" )~"Tipulidae",
      AI_Taxon %in% c( "Tubellaria","Turbellaria platyhelminthes","Flat worms - Turbellaria",
                       "Flatworm - Turbellaria","Flatworms - Turbellaria","Flat worms","Flat Worms",
                       "Platyheleminthes", "Dugesiidae","Tubelaria","Platyhelmenthes","Platyhelminthyes",
                       "Planariidae"
      )~"Turbellaria",
      AI_Taxon %in% c( "Tricorythidae (Tricorythodes)" )~"Tricorythodes",
      AI_Taxon %in% c(  "Thienemannimyia group", "Thinemannimyia group" )~"Thienemannimyia group",
      AI_Taxon %in% c( "Tricorythidae" )~"Tricorythidae",
      AI_Taxon %in% c( "Water boatman","Water boatmen","Corixicae","corixidae",
                       "Hemiptera corixidae","Coxidae")~"Corixidae", 
      AI_Taxon %in% c("Trichoptera Sericostomatidae","Serricostomatidae" )~"Sericostomatidae",
      AI_Taxon %in% c( "Worms" ,"Annelidae", "Annelid", "Annelida","Leeches")~"Annelida",
      AI_Taxon %in% c( "Insect exuviae unknown","Exuvia?","Unknown bugs" ,
                       "Unknown exuvia", "Unknown invert" ,"Unknown inverts" ,
                       "Unknown larvae" ,"Unknown pupa", "unknown spp" ,
                       "Unknown spp" ,"Pupa?","Unknown 1", "Unknown insect larvae",
                       "Unknown1", "Unknown2")~"Insecta",
      AI_Taxon %in% c("Vivaparidae" )~"Viviparidae",
      AI_Taxon %in% c("lestidae" )~"Lestidae",
      AI_Taxon %in% c("Snail - Juga" )~"Juga",
      AI_Taxon %in% c("Nemertea","Tetrastemmatidae" )~"Nemertea",
      AI_Taxon %in% c("Antocha","Hexatoma","Limnophila", "Pseudolimnophila")~"Limoniidae", # Added 9/26 KSM
      AI_Taxon %in% c("Cecidomyiidae")~"Terrestrial", # Added 9/26 KSM
      T~AI_Taxon),
    AI_Family= case_when( #This aggregates Taxon2 to best Family name, or higher if necessary. 
      # No sub-family names should remain
      # All caps if not to desired level
      # AI_Taxon2 %in% c("Arachnida","Archnida","Arachnia","Aracnida")~"Arachnida",
      AI_Taxon2 %in% c("Arachnida","Archnida","Arachnia","Aracnida")~"Acariformes",
      AI_Taxon2 %in% c("Potamanthidae")~"Potamanthidae",
      AI_Taxon2 %in% c("Bithynidae")~"Bithyniidae",
      AI_Taxon2 %in% c("Nemertea")~"Nemertea",
      AI_Taxon2 %in% c("Arthropleidae")~"Arthropleidae",
      AI_Taxon2 %in% c("Arthropoda")~"ARTHROPODA",
      AI_Taxon2 %in% c("Asellidae","Caecidotea","Asellida")~"Isopoda",
      AI_Taxon2 %in% c("Isopoda" )~"Isopoda",
      AI_Taxon2 %in% c("Abedus","Belostomatidae")~"Belostomatidae",
      AI_Taxon2 %in% c("Acariformes","Sperchon","Stygothrombiidae",
                       "Hydrachnidia", "Hydrachnidae",
                       "Acari")~"Acariformes",  # ADDED 9/26 KSM
      AI_Taxon2 %in% c("Aeshnidae","Aeshniae")~"Aeshnidae",
      AI_Taxon2 %in% c("Agabus")~"Dytiscidae",
      AI_Taxon2 %in% c("Deuterophlebiidae")~"Deuterophlebiidae",
      AI_Taxon2 %in% c("Ameletidae")~"Ameletidae",
      AI_Taxon2 %in% c("Hyalella","Amphipoda","Crangonyctidae","Hyalellidae" ,"Gammaridae")~"Amphipoda",
      AI_Taxon2 %in% c("Anisoptera", "Odonata","Zygoptera")~"ODONATA",
      AI_Taxon2 %in% c("Erpobdellidae","Hirudinea","Annelida","Oligochaeta","oligochaeta" )~"Annelida",
      AI_Taxon2 %in% c("Argia","Ceonagrionidae", "Coenagrionidae","coenagrionidae","Soenagrionidae")~"Coenagrionidae",
      AI_Taxon2 %in% c("Athericidae")~"Athericidae",
      AI_Taxon2 %in% c("Baetidae","Baetis adonis","Fallceon")~"Baetidae",
      AI_Taxon2 %in% c("Baetiscidae")~"Baetiscidae",
      AI_Taxon2 %in% c("Bivalve","Mussel")~"BIVALVE",
      AI_Taxon2 %in% c("Blephariceridae")~"Blephariceridae",
      AI_Taxon2 %in% c("Brachycentridae")~"Brachycentridae",
      AI_Taxon2 %in% c("Bulk sample")~"BULK",
      AI_Taxon2 %in% c("Caenidae")~"Caenidae",
      AI_Taxon2 %in% c("Anostraca")~"Anostraca",
      AI_Taxon2 %in% c("Hydrochidae")~"Hydrochidae",
      AI_Taxon2 %in% c("Mysidae")~"Mysidae",
      AI_Taxon2 %in% c("Cambaridae","Procambarus clarkii",
                       "Astacidae","Astacoidea","Parastacidae",
                       "Pseudothelphusidae","Atyidae")~"DECAPODA",
      # AI_Taxon2 %in% c("Astacidae")~"Astacidae",
      AI_Taxon2 %in% c("Cheumatopsyche", "Hydropsychidae","Hydropyschidae","Hydropschyidae")~"Hydropsychidae", 
      AI_Taxon2 %in% c("Chironomidae", "Thienemannimyia group")~"Chironomidae",
      AI_Taxon2 %in% c("Coleoptera")~"COLEOPTERA",
      AI_Taxon2 %in% c("Corbiculidae")~"Corbiculidae",
      AI_Taxon2 %in% c("Cordulegastridae")~"Cordulegastridae",
      AI_Taxon2 %in% c("Corixidae")~"Corixidae",
      AI_Taxon2 %in% c("Crustacea" )~"CRUSTACEA",
      AI_Taxon2 %in% c("Culicidae" )~"Culicidae",
      AI_Taxon2 %in% c("Culicidae" )~"Culicidae",
      AI_Taxon2 %in% c("Dreissenidae" )~"Dreissenidae",
      AI_Taxon2 %in% c("Daphnia","Cladocera","Diplostraca","Daphniidae" )~"Cladocera",
      AI_Taxon2 %in% c("Daphniidae" )~"Cladocera",
      AI_Taxon2 %in% c("Syrphidae" )~"Syrphidae",
      AI_Taxon2 %in% c("Apatanidae","Apataniidae" )~"Apataniidae",
      AI_Taxon2 %in% c("Dixidae" )~"Dixidae",
      AI_Taxon2 %in% c("Dryopidae",
                       "Helichus")~"Dryopidae",
      AI_Taxon2 %in% c("Dugesia","Platyhelminthes",
                       "Turbellaria" )~"Turbellaria",
      AI_Taxon2 %in% c("Dytiscidae" )~"Dytiscidae",
      AI_Taxon2 %in% c("Elmidae","Elmidae exuvia" ,"elmidae")~"Elmidae",
      AI_Taxon2 %in% c("Ephemerellidae" )~"Ephemerellidae",
      AI_Taxon2 %in% c("Ephemeroptera" )~"EPHEMEROPTERA",
      AI_Taxon2 %in% c("Gastropoda","Gastropod" )~"GASTROPODA",
      AI_Taxon2 %in% c("Gerridae" )~"Gerridae",
      AI_Taxon2 %in% c("Glossosomatidae","Glossoma" )~"Glossosomatidae",
      AI_Taxon2 %in% c("Gomphidae" )~"Gomphidae",
      AI_Taxon2 %in% c("Haliplidae" )~"Haliplidae",
      AI_Taxon2 %in% c("Helicopsychidae" )~"Helicopsychidae",
      AI_Taxon2 %in% c("Hydrometridae" )~"Hydrometridae",
      AI_Taxon2 %in% c("Hemiptera",
                       "Heteroptera")~"HEMIPTERA", # Added 9/26 KSM
      AI_Taxon2 %in% c("Heptageniidae","Ephemeroptera Heptageniidae" )~"Heptageniidae",
      AI_Taxon2 %in% c("Hydrobiidae", "Potamopyrgus antipodarum" )~"Hydrobiidae",
      AI_Taxon2 %in% c("Hydrophilidae","Tropisternus" )~"Hydrophilidae",
      AI_Taxon2 %in% c("Isonychiidae" )~"Isonychiidae", 
      AI_Taxon2 %in% c("Insecta" )~"INSECTA",
      AI_Taxon2 %in% c("Isoperla denningi","Perlodidae",
                       "Isoperla")~"Perlodidae", # Added by KSM - NOTE, these taxa were originally reported as Isoperlidae
      AI_Taxon2 %in% c("Lepidoptera")~"LEPIDOPTERA",
      AI_Taxon2 %in% c("Lutrochidae")~"Lutrochidae",
      AI_Taxon2 %in% c("Lepidostoma","Lepidostomatidae" )~"Lepidostomatidae",
      AI_Taxon2 %in% c("Leptohyphidae", "Tricorythodes","Leptohyphidae exuvia",
                       "Tricorythidae")~"Leptohyphidae",
      AI_Taxon2 %in% c("Leptophlebiidae" )~"Leptophlebiidae",
      AI_Taxon2 %in% c("Lestidae","lestidae" )~"Lestidae",
      AI_Taxon2 %in% c("Limnephilidae",
                       "Onocosmoecus" )~"Limnephilidae", 
      AI_Taxon2 %in% c("Mysidacea" )~"DECAPODA",
      AI_Taxon2 %in% c("Muscidae" )~"Muscidae",
      AI_Taxon2 %in% c("Nepidae", "Ranatra" )~"Nepidae", 
      AI_Taxon2 %in% c("Naucoridae" )~"Naucoridae",
      AI_Taxon2 %in% c("Nemouridae" )~"Nemouridae",
      AI_Taxon2 %in% c("No invertebrates observed" )~"No invertebrates observed",
      AI_Taxon2 %in% c("Notonectidae" )~"Notonectidae",
      # AI_Taxon2 %in% c("Oniscidae" )~"Oniscidae",
      AI_Taxon2 %in% c("Cnidaria" )~"Cnidaria",
      AI_Taxon2 %in% c("Paltothemis lineatipes" )~"Libellulidae",
      AI_Taxon2 %in% c( "Parastacidae" )~"DECAPODA", 
      AI_Taxon2 %in% c("Perlidae" )~"Perlidae",
      AI_Taxon2 %in% c("Philopotamidae","Philoptamidae","Philapotamidae" )~"Philopotamidae",
      AI_Taxon2 %in% c("Physa","Physidae" )~"Physidae",
      AI_Taxon2 %in% c("Planariidae" )~"Planariidae",
      
      AI_Taxon2 %in% c("Plecoptera")~"PLECOPTERA", 
      AI_Taxon2 %in% c("Pleidae" ,"Neoplea")~"Pleidae",
      AI_Taxon2 %in% c("Polymitarcyidae" )~"Polymitarcyidae",
      AI_Taxon2 %in% c("Psephenidae" )~"Psephenidae",
      AI_Taxon2 %in% c("Rhyacophila rayneri","Rhyacophilidae","Rhyancophilidae","Rhayn" )~"Rhyacophilidae",
      AI_Taxon2 %in% c("Simuliidae","Simulium" )~"Simuliidae",
      AI_Taxon2 %in% c("Siphlonuridae" )~"Siphlonuridae",
      AI_Taxon2 %in% c("Stratiomyidae" )~"Stratiomyidae",
      AI_Taxon2 %in% c("Tabanidae" )~"Tabanidae",
      AI_Taxon2 %in% c("Taenionema" )~"Taeniopterygidae",
      AI_Taxon2 %in% c("Tipulidae", "Limoniinae",
                       "Limoniidae","Pediciidae",
                       "Tipuloidea",
                       "Tipula", "Tipula sp." )~"Tipulidae", # ADDED 9/26 KSM  
      AI_Taxon2 %in% c("Trichoptera","Tricoptera case","Trichoptera Case","Casing")~"TRICHOPTERA",# ADDED 9/26 KSM  
      AI_Taxon2 %in% c("Molannidae" )~"Molannidae",
      AI_Taxon2 %in% c("Unknown" )~"UNKNOWN",
      AI_Taxon2 %in% c("Veliidae" )~"Veliidae",
      AI_Taxon2 %in% c("Planorbidae" )~"Planorbidae",
      AI_Taxon2 %in% c("Beraeidae" )~"Beraeidae", #Not in SAFIT
      AI_Taxon2 %in% c("Ceratopogonidae","Ceratopogenidae" )~"Ceratopogonidae",
      AI_Taxon2 %in% c("Sphaeriidae" )~"Sphaeriidae",
      AI_Taxon2 %in% c("Odontoceridae","odontoceridae" )~"Odontoceridae",
      AI_Taxon2 %in% c("Amphizoidae" )~"Amphizoidae",
      AI_Taxon2 %in% c("Leptoceridae" ,"Ceraclea","Leptaceridae")~"Leptoceridae",
      AI_Taxon2 %in% c("Gammaridae" )~"Gammaridae",
      AI_Taxon2 %in% c("Sericostomatidae" )~"Sericostomatidae",
      
      AI_Taxon2 %in% c("Psychodidae" )~"Psychodidae",
      AI_Taxon2 %in% c("Copepoda" )~"COPEPODA", #EXCLUDE??? Microcrustacean
      AI_Taxon2 %in% c("Ostracoda" )~"OSTRACODA", #EXCLUDE??? Microcrustacean
      AI_Taxon2 %in% c("Gyrinidae" )~"Gyrinidae",
      AI_Taxon2 %in% c("Decapoda","Palaemonidae","Decopoda")~"DECAPODA",
      AI_Taxon2 %in% c("Lymnaeidae" )~"Lymnaeidae",
      AI_Taxon2 %in% c("Chaoboridae" )~"Chaoboridae",
      AI_Taxon2 %in% c("Sialidae",
                       "Sialis" )~"Sialidae", 
      AI_Taxon2 %in% c("Carabidae" )~"Carabidae", ##Likely terrestrial?
      AI_Taxon2 %in% c("Megaloptera" )~"MEGALOPTERA",
      AI_Taxon2 %in% c("Tateidae" )~"Hydrobiidae", #Not in safit. Mud-snails, most likely
      AI_Taxon2 %in% c("Ephydridae" )~"Ephydridae", #Probably good but often in briny water
      AI_Taxon2 %in% c("Hebridae" )~"Hebridae", #Semi-aquatic
      AI_Taxon2 %in% c("Capniidae" )~"Capniidae",
      AI_Taxon2 %in% c("Rossianidae" )~"Rossianidae",
      AI_Taxon2 %in% c("Hydraenidae" )~"Hydraenidae",
      AI_Taxon2 %in% c("Ephemerellidae" )~"Ephemerellidae",
      AI_Taxon2 %in% c("Apataniidae" )~"Apataniidae",
      AI_Taxon2 %in% c("Bithyniidae" )~"Bithyniidae", #Not in SAFIT. Likely mis-identification for Hydrobiidae
      AI_Taxon2 %in% c("Uenoidae" )~"Uenoidae",
      AI_Taxon2 %in% c("Pomatiopsidae" )~"Pomatiopsidae", 
      AI_Taxon2 %in% c("Chloroperlidae" )~"Chloroperlidae",
      AI_Taxon2 %in% c("Hydroptilidae" )~"Hydroptilidae",
      # AI_Taxon2 %in% c("Limoniidae" )~"Limoniidae",
      AI_Taxon2 %in% c("Hydroscaphidae" )~"Hydroscaphidae",
      AI_Taxon2 %in% c("Staphylinidae" )~"Staphylinidae", #Likely terrestrial?
      AI_Taxon2 %in% c("Georissidae" )~"Georissidae", #Semi-aquatic
      AI_Taxon2 %in% c("Mesoveliidae" )~"Mesoveliidae",#Semi-aquatic
      AI_Taxon2 %in% c("Saldidae" )~"Saldidae",#Semi-aquatic
      AI_Taxon2 %in% c("Goeridae" )~"Goeridae",
      AI_Taxon2 %in% c("Petaluridae" )~"Petaluridae",
      AI_Taxon2 %in% c("Calamoceratidae" )~"Calamoceratidae",
      AI_Taxon2 %in% c("Phryganeidae","Phyganiidae","Phyrysaneidae",
                       "Phryganeiae","Phyganeidae","phryganeidae" )~"Phryganeidae",
      AI_Taxon2 %in% c("Polycentropodidae",
                       "Nyctiophylax" )~"Polycentropodidae", 
      AI_Taxon2 %in% c("Psychomyiidae" )~"Psychomyiidae",
      AI_Taxon2 %in% c("Ephemeridae", "Ephemera" )~"Ephemeridae",
      AI_Taxon2 %in% c("Eulichadidae" )~"Eulichadidae",
      AI_Taxon2 %in% c("Dolichopodidae" )~"Dolichopodidae",
      AI_Taxon2 %in% c("Scirtidae" )~"Scirtidae",
      AI_Taxon2 %in% c("Calopterygidae" )~"Calopterygidae",
      AI_Taxon2 %in% c("Valvatidae" )~"Valvatidae",
      AI_Taxon2 %in% c("Lithoglyphidae" )~"Lithoglyphidae",
      AI_Taxon2 %in% c("Helophoridae" )~"Helophoridae",
      AI_Taxon2 %in% c("Pyralidae","Crambidae","Pyralidae_Crambidae" )~"Crambidae", 
      AI_Taxon2 %in% c("Pteronarcyidae","Pteronarcidae","Pteronarycidae",
                       "Pteronarcys" )~"Pteronarcyidae", # Added 9/26 KSM
      AI_Taxon2 %in% c("Libellulidae" )~"Libellulidae",
      AI_Taxon2 %in% c("Astacoidea" )~"ASTACOIDEA",
      AI_Taxon2 %in% c("Viviparidae" )~"Viviparidae",
      AI_Taxon2 %in% c("Acroloxidae" )~"Acroloxidae", #Not in SAFIT. Mis-identification? 2 observations in iNat
      AI_Taxon2 %in% c("Ptilodactylidae" )~"Ptilodactylidae",
      # AI_Taxon2 %in% c("Cyphoderinae" )~"COLLEMBOLA",#Not in SAFIT, EXCLUDE?
      AI_Taxon2 %in% c("Empididae" )~"Empididae",
      AI_Taxon2 %in% c("Ecnomidae" )~"Ecnomidae", #Not in SAFIT, likely mis-identification
      AI_Taxon2 %in% c("Nematocera","Tipuloidea" )~"DIPTERA",
      AI_Taxon2 %in% c("Pelecorhynchidae" )~"Pelecorhynchidae",
      AI_Taxon2 %in% c("Palaemonidae" )~"Palaemonidae", 
      AI_Taxon2 %in% c("Ptychopteridae" ,"Ptychoptera")~"Ptychopteridae",
      AI_Taxon2 %in% c("Sciomyzidae" )~"Sciomyzidae",
      AI_Taxon2 %in% c("Tanyderidae" )~"Tanyderidae",
      AI_Taxon2 %in% c("Thaumaleidae" )~"Thaumaleidae",
      # AI_Taxon2 %in% c("Talitridae" )~"Talitridae",
      AI_Taxon2 %in% c( "Thiaridae" )~"Thiaridae",
      AI_Taxon2 %in% c( "Tricorythidae" )~"Tricorythidae",
      
      AI_Taxon2 %in% c("Gelastocoridae" )~"Gelastocoridae", #Semi-aquatic
      
      AI_Taxon2 %in% c("Pleuroceridae","Juga" )~"Pleuroceridae", #likely juga
      AI_Taxon2 %in% c("Corduliidae" )~"Corduliidae",
      AI_Taxon2 %in% c("Leuctridae" )~"Leuctridae",
      AI_Taxon2 %in% c("Peltoperlidae" )~"Peltoperlidae",
      AI_Taxon2 %in% c("Taeniopterygidae" )~"Taeniopterygidae",
      AI_Taxon2 %in% c("Hydrobiosidae" )~"Hydrobiosidae",
      AI_Taxon2 %in% c("Macroveliidae" )~"Macroveliidae", #Semi-aquatic
      AI_Taxon2 %in% c("Macromiidae" )~"Macromiidae", #Semi-aquatic
      AI_Taxon2 %in% c("Neuroptera" )~"NEUROPTERA",
      AI_Taxon2 %in% c("Unionida",
                       "Elliptio","Unionidae" )~"Unionidae",
      AI_Taxon2 %in% c("Bivalvia" )~"BIVALVE",
      AI_Taxon2 %in% c("Corydalus","Corydalus cornutus",
                       "Corydalidae (N-P)","Corydalidae (O-D group)", "Corydalidae","Neohermes",
                       "Corydalidae (Orohermes-Dysmicohermes group)" ,"Nigronia",
                       "Corydalidae (Neohermes-Protochauliodes group)")~"Corydalidae", 
      AI_Taxon2 %in% c("Hyalellidae" )~"Hyalellidae",
      AI_Taxon2 %in% c("Thienemannimyia" )~"Chironomidae",
      AI_Taxon2 %in% c("Noteridae" )~"Noteridae",
      AI_Taxon2 %in% c("Margaritiferidae" )~"Margaritiferidae",
      AI_Taxon2 %in% c("Ancylidae" )~"Ancylidae",
      AI_Taxon2 %in% c("Pediciidae", #Not in SAFIT, but treated as Limoniinae
                       "Dicranota")~"Tipulidae", # Added 9/26 KSM
      # AI_Taxon2 %in% c("Mysidae" )~"Mysidae",
      AI_Taxon2 %in% c("Mollusca" )~"MOLLUSCA",# Added 9/26 KSM
      AI_Taxon2 %in% c("Nematomorpha" )~"NEMATOMORPHA",# Added 9/26 KSM
      AI_Taxon2 %in% c("Nematoda" )~"NEMATODA",# Added 9/26 KSM
      AI_Taxon2 %in% c("Diptera")~"DIPTERA",# Added RDM 072823
      AI_Taxon2 %in% c("Hydrophiloidea")~"COLEOPTERA",# Added RDM 072823
      AI_Taxon2 %in% c("No invertebrates observed" ,"BULK","Exclude","Not Recorded","None Observed",
                       "Terrestrial sp", semi_aquatic, excluded_bugs,
                       # "Crambidae",
                       "Arthropoda")~"Exclude",# Added 9/26 KSM
      
      T~"Exclude"),
    
    # ai_combined %>% 
    #   filter(AI_Family=="Exclude") %>%
    #   select(AI_Taxon, AI_Taxon2) %>% unique() %>%
    #   clipr::write_clip()
    #Add order-level identifications
    AI_Order = case_when(
      AI_Family %in% c("Ameletidae", "Baetidae","Baetiscidae", "Caenidae", "Ephemerellidae","Ephemeridae", "EPHEMEROPTERA", 
                       "Heptageniidae", "Isonychiidae", "Leptohyphidae", "Leptophlebiidae", "Arthropleidae",
                       "Polymitarcyidae", "Siphlonuridae", "Tricorythidae","Potamanthidae")~"Ephemeroptera",
      AI_Family %in% c("Capniidae", "Chloroperlidae", "Leuctridae", "Nemouridae","Peltoperlidae", 
                       "Perlidae", "Perlodidae", "PLECOPTERA","Pteronarcyidae", "Taeniopterygidae")~"Plecoptera",
      AI_Family %in% c("Apataniidae", "Brachycentridae", "Calamoceratidae", "Glossosomatidae", "Helicopsychidae", 
                       "Hydropsychidae", "Hydroptilidae","Lepidostomatidae","Leptoceridae", "Limnephilidae", 
                       "Molannidae","Rossianidae","Beraeidae","Ecnomidae",
                       "Odontoceridae",  "Philopotamidae", "Phryganeidae","Polycentropodidae","Hydrobiosidae","Goeridae",
                       "Rhyacophilidae","Sericostomatidae", "TRICHOPTERA","Uenoidae","Psychomyiidae")~"Trichoptera",
      AI_Family %in% c("Aeshnidae", "Calopterygidae", "Coenagrionidae", "Cordulegastridae","Corduliidae", 
                       "Gomphidae", "Lestidae", "Libellulidae","Macromiidae", "ODONATA","Petaluridae")~"Odonata",
      AI_Family %in% c("Belostomatidae", "Corixidae", "Gerridae", "HEMIPTERA", "Naucoridae", "Saldidae","Gelastocoridae",
                       "Hebridae","Macroveliidae","Mesoveliidae",
                       "Hydrometridae",
                       "Nepidae", "Notonectidae", "Pleidae", "Veliidae")~"Hemiptera",
      AI_Family %in% c("COLEOPTERA", "Dryopidae", "Dytiscidae", "Elmidae","Gyrinidae", "Haliplidae",
                       "Lutrochidae","Hydraenidae","Hydraenidae","Helophoridae","Eulichadidae","Georissidae",
                       "Staphylinidae","Amphizoidae","Carabidae","Hydroscaphidae",
                       "Hydrochidae", "Hydrophilidae", "Psephenidae","Ptilodactylidae","Scirtidae")~"Coleoptera",
      AI_Family %in% c("Athericidae", "Blephariceridae", "Ceratopogonidae", "Chaoboridae", "Deuterophlebiidae",
                       "Chironomidae", "Sciomyzidae","Syrphidae","Tanyderidae",
                       "Culicidae","Thaumaleidae","Pelecorhynchidae",
                       "DIPTERA", "Dixidae", "Dolichopodidae","Empididae","Ephydridae","Limoniidae",
                       "Muscidae","Pediciidae","Psychodidae","Ptychopteridae", "Simuliidae", 
                       "Stratiomyidae", "Tabanidae", "Tipulidae")~"Diptera",
      AI_Family %in% c("Ancylidae", "GASTROPODA", "Hydrobiidae","Lithoglyphidae", "Physidae", "Acroloxidae",
                       "Planorbidae", "Bithyniidae","Pomatiopsidae",
                       "Thiaridae","Lymnaeidae","Pleuroceridae","Valvatidae","Viviparidae")~"GASTROPODA" ,#Most are basomatomorpha
      AI_Family %in% c("BIVALVE", "Corbiculidae", "Dreissenidae", "Margaritiferidae", "Sphaeriidae",
                       "Unionidae")~"BIVALVIA",
      AI_Family %in% c("Amphipoda", "Asellidae", "Cambaridae", "Cladocera","COPEPODA","OSTRACODA", "CRUSTACEA","ASTACOIDEA","Daphniidae",
                       "DECAPODA","Isopoda", "Mysidacea", "Palaemonidae", "Parastacidae", "Astacidae","Mysidae","Oniscidae")~"CRUSTACEA",
      AI_Family %in% c("Annelida")~"Annelida",
      AI_Family %in% c("Acariformes","Arachnida")~"Acariformes",
      AI_Family %in% c("Platyheminthes","Planariidae","Turbellaria")~"Turbellaria",
      AI_Family %in% c("NEMATOMORPHA")~"NEMATOMORPHA",
      AI_Family %in% c("NEMATODA")~"NEMATODA",
      AI_Family %in% c("Nemertea")~"Nemertea",
      AI_Family %in% c("Corydalidae","Sialidae","MEGALOPTERA")~"Megaloptera",
      AI_Family %in% c("LEPIDOPTERA","Pyralidae","Crambidae","Pyralidae_Crambidae")~"Lepidoptera",
      AI_Family %in% c("NEUROPTERA")~"Neuroptera",
      AI_Family %in% c("Cnidaria")~"Cnidaria",
      AI_Family %in% c("Exclude") ~"Exclude",
      # AI_Family %in% c("Mollusca")~"Mollusca",
      T~"OtherOrder_OrderNotKnown"
      
    ),
    #This code assigns families to appropriate groups to assist with metric calculation
    AI_Ephemeroptera = AI_Order=="Ephemeroptera",
    AI_Plecoptera = AI_Order=="Plecoptera",
    AI_Trichoptera = AI_Order=="Trichoptera",
    AI_EPT = AI_Ephemeroptera | AI_Plecoptera | AI_Trichoptera,
    AI_Odonata =  AI_Order=="Odonata",
    AI_Hemiptera = AI_Order=="Hemiptera",
    AI_Coleoptera = AI_Order=="Coleoptera",
    AI_Diptera = AI_Order == "Diptera",
    AI_Gastropod = AI_Order=="GASTROPODA",
    AI_Basommatophora = AI_Family %in% c("Acroloxidae","Ancylidae","Lymnaeidae","Physidae","Planorbidae"), #Not really a recognized group anymore
    AI_Bivalve = AI_Order=="BIVALVIA",
    AI_Bivalve_Nonfingernail = AI_Family %in% c("BIVALVE", "Corbiculidae", "Dreissenidae", "Margaritiferidae", "Unionidae"),
    AI_Clams_Fingernail = AI_Family %in% c("Sphaeriidae"),
    AI_Mollusk = (AI_Gastropod | AI_Bivalve),
    AI_Crayfish = AI_Family %in% c("Cambaridae","DECAPODA", "Parastacidae", "Astacidae"),
    AI_Crustacean = AI_Family %in% c("Amphipoda", "Asellidae", "Cambaridae", "Cladocera","COPEPODA", "CRUSTACEA", 
                                     "DECAPODA","Isopoda", "Mysidacea", "Palaemonidae", "Parastacidae", "Astacidae"), 
    AI_Decapoda = AI_Family %in% c( "Cambaridae", "DECAPODA", "Palaemonidae", "Parastacidae", "Astacidae"),
    AI_Noninsect = (AI_Crustacean | AI_Gastropod | AI_Bivalve | AI_Family %in% c("Annelida","Acariformes","Nemertea",
                                                                                 "Platyheminthes","Planariidae",# ADDED BY KSM DEC 23
                                                                                 "MOLLUSCA","NEMATOMORPHA","NEMATODA",
                                                                                 "Turbellaria","Cnidaria")),# Added 9/26 KSM
    AI_PerennialIndicator_PNW = AI_Family %in% c("Pleuroceridae","Ancylidae","Hydrobiidae","Margaritiferidae","Unionidae",
                                                 "Rhyacophilidae","Philopotamidae","Hydropsychidae","Glossosomatidae",
                                                 "Perlidae","Pteronarcyidae","Elmidae","Psephenidae",
                                                 "Gomphidae","Cordulegastridae","Calopterygidae","Corydalidae"),
    AI_PerennialIndicator_NC =
      AI_Family  %in% c("Baetidae", "Caenidae", "Ephemerellidae","Ephemeridae","Heptageniidae","Leptophlebiidae","Siphlonuridae",
                        "Peltoperlidae","Perlidae","Perlodidae",
                        "Hydropsychidae","Lepidostomatidae","Limnephilidae","Molannidae","Odontoceridae","Philopotamidae","Polycentropidae","Psychomyiidae","Rhyacophilidae",
                        "Corydalidae","Sialidae",
                        "Aeshnidae","Calopterygidae","Cordulegastridae","Gomphidae","Libellulidae",
                        "Ptychopteridae",
                        "Elmidae","Psephenidae",
                        "Unionidae","Ancylidae","Planorbidae","Pleuroceridae","Acroloxidae") |
      AI_Taxon2 =="Tipula" |
      (AI_Taxon2 =="Helichus" & AI_Lifestage=="insect adult" ), 
    
    #All ISA-based indicators require a minimum frequency of 5%.
    #All ISA-based results are based on abundance, not PA
    #ARID WEST ISA
    AI_PerennialIndicator_AW = #Added Aug 21, 2023
      AI_Family %in% c("Hydropsychidae","Elmidae","Leptophlebiidae", #IV > 0.5
                       "Heptageniidae","Brachycentridae","Lepidostomatidae","Leptoceridae","Pteronarcyidae","Perlidae" #IV < 0.5 p <0.05
      ), 
    AI_PerIntIndicator_AW = #Added Aug 21, 2023
      AI_Family %in% c("Baetidae","Ceratopogonidae" #IV > 0.5
      ), 
    
    #WM ISA
    AI_PerennialIndicator_WM = #Added Aug 21, 2023
      AI_Family %in% c("Elmidae","Hydropsychidae","Heptageniidae","Ephemerellidae","Leptophlebiidae","Chloroperlidae", #IV > 0.5
                       "Rhyacophilidae","Leptohyphidae","Empididae","Hydroptilidae",#IV < 0.5 p <0.05
                       "Glossosomatidae","Pteronarcyidae","Uenoidae" #IV < 0.5 p <0.05
                       #"Helicopsychidae","Athericidae" #IV < 0.5 p <0.05 but freq < 5%
      ), 
    AI_PerIntIndicator_WM = #Added Aug 21, 2023
      AI_Family %in% c("Baetidae","Simuliidae","Tipulidae","Brachycentridae","Perlodidae","Sphaeriidae", #IV > 0.5
                       "Lepidostomatidae","Physidae","Psychodidae","Perlidae","Amphipoda","Ameletidae","Lymnaeidae","Turbellaria"#IV < 0.5 p <0.05
      ),
    
    #West ISA
    AI_PerennialIndicator_West = #Added Aug 21, 2023
      AI_Family %in% c("Hydropsychidae","Elmidae","Heptageniidae","Leptophlebiidae","Ephemerellidae", #IV > 0.5
                       "Brachycentridae","Chloroperlidae","Leptohyphidae","Glossosomatidae","Rhyacophilidae", #IV < 0.5 p <0.05
                       "Perlidae","Pteronarcyidae","Leptoceridae","Uenoidae" #IV < 0.5 p <0.05
                       # "Athericidae","Corbiculidae","Crambidae" #IV < 0.5 p <0.05, but freq < 5%
      ), 
    AI_PerIntIndicator_West = #Added Aug 21, 2023
      AI_Family %in% c("Baetidae","Simuliidae", #IV > 0.5
                       "Physidae","Sphaeriidae","Perlodidae","Hydroptilidae","Lepidostomatidae","Amphipoda", #IV < 0.5 p <0.05
                       "Empididae","Corixidae","Turbellaria","Psychodidae","Lymnaeidae","Ameletidae" #IV < 0.5 p <0.05
      ),
    
    #NGP ISA
    AI_PerennialIndicator_NGP = #Added 9/7/2023
      AI_Family %in% c("Hydropsychidae", #IV > 0.5
                       "Heptageniidae"#IV < 0.5 p <0.05
      ), 
    AI_EphemeralIndicator_NGP = #Added 9/7/2023
      AI_Family %in% c("Corixidae" #IV>0.5 #only one taxon, but we can use in E+IE metrics
      ),
    AI_EphIntIndicator_NGP = #Added 9/7/2023
      AI_Family %in% c("Physidae", #IV>0.5
                       "Dytiscidae","Hydrophilidae" #IV<0.5 p<0.05
      ),
    
    #SGP ISA
    AI_PerennialIndicator_SGP = #Added 9/7/2023
      AI_Family %in% c("Psephenidae", #IV = 0.5
                       "Lymnaeidae" #IV<0.5,p<0.05
      ), 
    
    AI_PerIntIndicator_SGP = #Added 9/7/2023
      AI_Family %in% c("Coenagrionidae" #IV > 0.5 #Only one taxon but we can use with other metrics
      ),
    AI_EphemeralIndicator_SGP = #Added 9/7/2023
      AI_Family %in% c("Culicidae", #IV >0.5
                       "Haliplidae" #IV < 0.5 p <0.05
      ),
    AI_EphIntIndicator_SGP = #Added 9/7/2023
      AI_Family %in% c("Physidae" #IV > 0.5 #Only one taxon but we can use with other metrics
      ),
    
    #GP ISA
    
    AI_PerennialIndicator_GP = #Added 9/7/2023
      AI_Family %in% c("Heptageniidae", "Turbellaria","Perlidae","Leptohyphidae" #IV < 0.5 p <0.05
      ), 
    AI_EphemeralIndicator_GP = #Added 9/7/2023
      AI_Family %in% c("Culicidae","Haliplidae","Lymnaeidae" #IV < 0.5 p <0.05
      ), 
    AI_EphIntIndicator_GP = #Added 9/7/2023
      AI_Family %in% c("Physidae",#IV>0.5
                       "Dytiscidae","Hydrophilidae"  #IV < 0.5 p <0.05
      ),
    
    #NE ISA
    AI_PerennialIndicator_NE = #Added 9/7/2023
      AI_Family %in% c("Hydropsychidae", #IV > 0.5
                       "Perlidae", "Philopotamidae","Gomphidae" #IV < 0.5 p <0.05
      ), 
    AI_PerennialIndicator_NE = #Added 9/7/2023
      AI_Family %in% c("Hydropsychidae", #IV > 0.5
                       "Perlidae", "Philopotamidae","Gomphidae" #IV < 0.5 p <0.05
      ), 
    AI_EphemeralIndicator_NE = #NOT YET ADDED
      AI_Family %in% c("Annelida" #IV < 0.5 p <0.05
      ), 
    AI_EphIntIndicator_NE = #NOT YET ADDED
      AI_Family %in% c("Dytiscidae","Hydrophilidae" #IV < 0.5 p <0.05
      ), 
    
    #SE ISA
    AI_PerennialIndicator_SE = #Added 9/7/2023
      AI_Family %in% c("Heptageniidae", "Hydropsychidae", #IV > 0.5
                       "Tipulidae","Perlidae","Philopotamidae","Ephemerellidae","Peltoperlidae",  #IV < 0.5 p <0.05
                       "Cordulegastridae","Calopterygidae"  #IV < 0.5 p <0.05
      ), 
    AI_PerIntIndicator_SE = #Added 9/7/2023
      AI_Family %in% c("Baetidae"  #IV < 0.5 p <0.05
      ), 
    AI_EphemeralIndicator_SE = #Added 9/7/2023
      AI_Family %in% c("Culicidae"  #IV < 0.5 p <0.05
      ), 
    AI_EphIntIndicator_SE = #Added 9/7/2023
      AI_Family %in% c("Amphipoda","Isopoda",  #IV >0.5
                       "Dytiscidae" #IV<0.5, p<0.05
      ), 
    
    
    #East ISA
    AI_PerennialIndicator_East = #Added 9/7/2023
      AI_Family %in% c("Heptageniidae", "Hydropsychidae", #IV > 0.5
                       "Perlidae","Ephemerellidae", "Philopotamidae", #IV<0.5 p<0.05
                       "Gomphidae", "Corydalidae","Elmidae","Calopterygidae", #IV<0.5 p<0.05
                       "Cordulegastridae", "Peltoperlidae"  #IV<0.5 p<0.05
      ), 
    AI_PerIntIndicator_East = #Added 9/7/2023
      AI_Family %in% c("Capniidae","Simuliidae","Perlodidae","Nemouridae","Gerridae","Ameletidae"  #IV<0.5 p<0.05
      ), 
    AI_EphemeralIndicator_East = #Added 9/7/2023
      AI_Family %in% c("Culicidae"  #IV<0.5 p<0.05
      ), 
    AI_EphIntIndicator_East = #Added 9/7/2023
      AI_Family %in% c("Amphipoda","Isopoda", #IV>0.5
                       "Dytiscidae","Hydrophilidae"#IV<0.5 p<0.05
      ), 
    
    
    
    AI_GOLD = (AI_Gastropod| (AI_Family %in% "Annelida") | AI_Diptera),
    AI_OCH = (AI_Odonata | AI_Coleoptera | AI_Hemiptera),
    AI_Tolerant = AI_Family %in% c("Annelida","Acariformes",
                                   "Platyheminthes","Planariidae",
                                   "NEMATOMORPHA","NEMATODA",
                                   "Turbellaria",
                                   #Dipterans:
                                   "Chironomidae","Culicidae","Psychodidae",
                                   #Crustaceans
                                   "Amphipoda", "Asellidae","Isopoda",#"Talitridae",
                                   #Snails
                                   "Physidae",
                                   #Odonates
                                   "Libellulidae","Coenagrionidae", 
                                   #Other insects where all taxa have TV > 8
                                   "Belostomatidae","Corixidae","Haliplidae"),
    AI_Tolerant_Alt = (AI_Noninsect  & !(AI_Bivalve|AI_Crayfish) |
                         AI_Family %in% c("Culicidae","Chironomidae")),
    AI_OtherMacro = !AI_EPT & !AI_OCH & !AI_Basommatophora & !AI_Decapoda,
  )

ai_combined %>%
  group_by(AI_Order, AI_Family, AI_Taxon2, AI_Taxon) %>%
  tally() %>%
  arrange(AI_Order, AI_Family, AI_Taxon2, AI_Taxon) %>%
  write_clip()

ai_combined %>%write_csv("NotForGit/ai_combined_07282023.csv")

ai_combined %>%
  select(AI_Family, contains("Indicator")) %>%
  unique() %>%
  write_clip()

ai_combined %>%
  select(-SiteCode) %>%
  inner_join(main_df %>%
               select(SiteCode, CollectionDate, Database, ParentGlobalID)) %>%
  inner_join(xwalk_df %>%
               select(SiteCode=sitecode, Class, Region, Region_detail, Region_detail2, beta_region)) %>%
  write_csv("NotForGit/ai_combined_07282023.csv")
# filter(ParentGlobalID=="e781bcdc-9c0b-4d9e-a4af-fcce91bf72d5")
# filter(SiteCode=="COCB121") %>% group_by(SiteCode, CollectionDate, ParentGlobalID, Database) %>% tally() %>% arrange(CollectionDate)
# main_df %>% filter(ParentGlobalID=="b93f3dd9-891e-44d7-98b2-8d48f3f6ede6") %>% select(BMI_score, Database)

# skim()

filter(SiteCode=="ARNE163_B") %>% as.data.frame()

main_df %>% filter(SiteCode=="COCB1") %>% select(ParentGlobalID, CollectionDate) %>% unique() %>% arrange(CollectionDate)
ai_df %>% filter(ParentGlobalID=="e781bcdc-9c0b-4d9e-a4af-fcce91bf72d5")
ai_combined %>% filter(ParentGlobalID=="e781bcdc-9c0b-4d9e-a4af-fcce91bf72d5")

ai_combined %>% filter(AI_Family =="Isopoda") %>% group_by(AI_Family, AI_Order, AI_Taxon) %>% tally()

ai_combined %>% filter(str_detect(AIGlobalID, "e61df210")) %>% as.data.frame()
ai_df %>% filter(str_detect(AIGlobalID, "e61df210")) %>% as.data.frame()
ai_df %>% filter(str_detect(ParentGlobalID, "7fb47d15"))

main_df %>% filter(str_detect(ParentGlobalID, "15c9fb44"))



ai_combined %>% 
  filter(AI_Family=="Exclude") %>%
  select(AI_Taxon2) %>%
  unique() %>%
  print(n=80)

ai_combined %>% select(AI_Tolerant, AI_Tolerant_Alt, AI_Family) %>% unique() %>% filter(AI_Tolerant_Alt) %>%write.table("clipboard", sep="\t", row.names=F)
ai_combined %>% filter(AI_Tolerant_Alt) %>%group_by(AI_Family) %>% tally() %>% arrange(n) %>%write.table("clipboard", sep="\t", row.names=F)
ai_combined %>% group_by(AI_Order) %>% tally()
ai_combined %>% filter(AI_Order=="OtherOrder_OrderNotKnown") %>% group_by(AI_Family) %>% tally()
# 
#   
ai_combined %>%
  filter(AI_Family!="Exclude") %>%
  filter(AI_Family!="BULK") %>%
  select(starts_with("AI_")) %>% 
  select(-AI_Lifestage, -AI_LiveDead, -AI_Abundance, -AI_Notes) %>%
  unique() %>%
  clipr::write_clip()

# MORE CLEANUP FOR AI DATA BASED ON BUG HABITAT TYPE (TERRESTRIAL, SEMI-AQUATIC)
# This code may be used to drop terrestrial bugs and other taxa we want to exclude from analysis


# check_ai <- ai_combined %>%
#   filter(AI_Family == "DOTHIS") %>%
#   transmute(AI_Taxon, AI_Taxon2,Notes = paste("Changed taxon from ",AI_Taxon," (KSM)",sep = "")) %>%
#   unique() %>%
#   filter(!AI_Taxon2 %in% excluded_bugs) %>%
#   filter(!AI_Taxon2 %in% semi_aquatic)
# 
# 
# write_csv(check_ai, file = "NotForGit/clean_taxonomysamples.csv") 

# Remove terrestrial bugs, semi-aquatic species and other non-insect species from AI taxonomy dataset
ai_combined2<-ai_combined %>%
  filter(!AI_Family %in% excluded_bugs) %>%
  filter(!AI_Taxon2 %in% excluded_bugs) %>%
  filter(!AI_Family %in% semi_aquatic) %>%
  filter(!AI_Taxon2 %in% semi_aquatic) 

ai_combined2 %>% filter(is.na(AI_Abundance))

ai_combined2$AI_Family %>%unique()

ai_metrics<-ai_combined2 %>%
  # inner_join(xwalk_df %>%               select(SiteCode=sitecode, Region, Region_detail)) %>% #Inner join drops sitecode "NEUB115". Verify reason.
  mutate(deadmaterial=case_when(AI_LiveDead=="non-living"~T,
                                is.na(AI_LiveDead)~F,
                                T~F),
         AI_AbundanceReported=AI_Abundance,
         AI_Abundance=case_when(AI_Abundance>10~10, 
                                is.na(AI_Abundance)~1,
                                T~AI_Abundance)) %>%
  # group_by(ParentGlobalID, SiteCode, CollectionDate, Database) %>%
  
  #THERE ARE BAD SITE CODES IN AI_DF
  
  group_by(ParentGlobalID, SiteCode) %>%
  
  
  
  summarise(TotalAbundance=sum(AI_Abundance),
            Richness = AI_Family %>% unique() %>% length(), #Before I did this at taxon level, but family makes more sense
            mayfly_abundance = sum(AI_Ephemeroptera*AI_Abundance),
            # mayfly_gt6 = mayfly_abundance>=6,
            perennial_PNW_abundance=sum(AI_PerennialIndicator_PNW*AI_Abundance),
            perennial_PNW_taxa= AI_Family[AI_PerennialIndicator_PNW] %>% unique() %>% length(),#perennial_taxa=sum(AI_PerennialIndicator),
            perennial_PNW_live_abundance = sum(AI_PerennialIndicator_PNW*AI_Abundance*!deadmaterial),
            perennial_NC_abundance=sum(AI_PerennialIndicator_NC *AI_Abundance),
            perennial_NC_taxa= AI_Family[AI_PerennialIndicator_NC ] %>% unique() %>% length(),#perennial_taxa=sum(AI_PerennialIndicator),
            perennial_NC_live_abundance = sum(AI_PerennialIndicator_NC *AI_Abundance*!deadmaterial),
            
            #New ISA metrics
            perennial_West_abundance = sum(AI_PerennialIndicator_West*AI_Abundance),
            perennial_GP_abundance = sum(AI_PerennialIndicator_GP*AI_Abundance),
            perennial_East_abundance = sum(AI_PerennialIndicator_East*AI_Abundance),
            perennial_AW_abundance = sum(AI_PerennialIndicator_AW*AI_Abundance),
            perennial_WM_abundance = sum(AI_PerennialIndicator_WM*AI_Abundance),
            perennial_NGP_abundance = sum(AI_PerennialIndicator_NGP*AI_Abundance),
            perennial_SGP_abundance = sum(AI_PerennialIndicator_SGP*AI_Abundance),
            perennial_NE_abundance = sum(AI_PerennialIndicator_NE*AI_Abundance),
            perennial_SE_abundance = sum(AI_PerennialIndicator_SE*AI_Abundance),
            
            perintper_West_abundance = sum(AI_PerIntIndicator_West*AI_Abundance) + perennial_West_abundance,
            # perintper_GP_abundance =  NA_real_, #No perint indicator taxa
            perintper_East_abundance = sum(AI_PerIntIndicator_East*AI_Abundance) + perennial_East_abundance,
            perintper_AW_abundance = sum(AI_PerIntIndicator_AW*AI_Abundance) + perennial_AW_abundance,
            perintper_WM_abundance = sum(AI_PerIntIndicator_WM*AI_Abundance) + perennial_WM_abundance,
            # perintper_NGP_abundance = NA_real_, #No perint indicator taxa
            perintper_SGP_abundance = sum(AI_PerIntIndicator_SGP*AI_Abundance) + perennial_SGP_abundance,
            # perintper_NE_abundance = NA_real_, #No perint indicator taxa
            perintper_SE_abundance = sum(AI_PerIntIndicator_SE*AI_Abundance) + perennial_SE_abundance,
            
            # ephemeral_West_abundance = NA_real_, #No ephemeral indicator taxa
            ephemeral_GP_abundance = sum(AI_EphemeralIndicator_GP*AI_Abundance), 
            ephemeral_East_abundance = sum(AI_EphemeralIndicator_East*AI_Abundance), #Only one ephemeral indicator taxon
            # ephemeral_AW_abundance = NA_real_, #No ephemeral indicator taxa
            # ephemeral_WM_abundance = NA_real_, #No ephemeral indicator taxa
            ephemeral_NGP_abundance = sum(AI_EphemeralIndicator_NGP*AI_Abundance), 
            ephemeral_SGP_abundance = sum(AI_EphemeralIndicator_SGP*AI_Abundance), 
            ephemeral_NE_abundance =  sum(AI_EphemeralIndicator_NE*AI_Abundance), #Only one ephemeral indicator taxon
            ephemeral_SE_abundance =  sum(AI_EphemeralIndicator_SE*AI_Abundance), #Only one ephemeral indicator taxon
            
            # ephinteph_West_abundance = NA_real_, #No ephemeral indicator taxa
            ephinteph_GP_abundance = sum(AI_EphIntIndicator_GP*AI_Abundance) + ephemeral_GP_abundance, 
            ephinteph_East_abundance = sum(AI_EphIntIndicator_East*AI_Abundance) + ephemeral_East_abundance, #Only one ephemeral indicator taxon
            # ephinteph_AW_abundance = NA_real_, #No ephemeral indicator taxa
            # ephinteph_WM_abundance = NA_real_, #No ephemeral indicator taxa
            ephinteph_NGP_abundance = sum(AI_EphIntIndicator_NGP*AI_Abundance) + ephemeral_NGP_abundance, 
            ephinteph_SGP_abundance = sum(AI_EphIntIndicator_SGP*AI_Abundance) + ephemeral_SGP_abundance, 
            ephinteph_NE_abundance =  sum(AI_EphIntIndicator_NE*AI_Abundance) + ephemeral_NE_abundance, #Only one ephemeral indicator taxon
            ephinteph_SE_abundance =  sum(AI_EphIntIndicator_SE*AI_Abundance) + ephemeral_SE_abundance, #Only one ephemeral indicator taxon
            
            
            perennial_West_taxa = AI_Family[which(AI_PerennialIndicator_West)] %>% unique() %>% length(),
            perennial_GP_taxa = AI_Family[which(AI_PerennialIndicator_GP)] %>% unique() %>% length(),
            perennial_East_taxa = AI_Family[which(AI_PerennialIndicator_East)] %>% unique() %>% length(),
            perennial_AW_taxa = AI_Family[which(AI_PerennialIndicator_AW)] %>% unique() %>% length(),
            perennial_WM_taxa = AI_Family[which(AI_PerennialIndicator_WM)] %>% unique() %>% length(),
            perennial_NGP_taxa = AI_Family[which(AI_PerennialIndicator_NGP)] %>% unique() %>% length(),
            perennial_SGP_taxa = AI_Family[which(AI_PerennialIndicator_SGP)] %>% unique() %>% length(),
            perennial_NE_taxa = AI_Family[which(AI_PerennialIndicator_NE)] %>% unique() %>% length(),
            perennial_SE_taxa = AI_Family[which(AI_PerennialIndicator_SE)] %>% unique() %>% length(),
            
            perintper_West_taxa = AI_Family[which(AI_PerennialIndicator_West|AI_PerIntIndicator_West)] %>% unique() %>% length(),
            # perintper_GP_taxa = NA_real_,#No perint indicator taxa
            perintper_East_taxa = AI_Family[which(AI_PerennialIndicator_East|AI_PerIntIndicator_East)] %>% unique() %>% length(),
            perintper_AW_taxa = AI_Family[which(AI_PerennialIndicator_AW|AI_PerIntIndicator_AW)] %>% unique() %>% length(),
            perintper_WM_taxa = AI_Family[which(AI_PerennialIndicator_WM|AI_PerIntIndicator_WM)] %>% unique() %>% length(),
            # perintper_NGP_taxa = NA_real_,#No perint indicator taxa
            perintper_SGP_taxa = AI_Family[which(AI_PerennialIndicator_SGP|AI_PerIntIndicator_SGP)] %>% unique() %>% length(),
            # perintper_NE_taxa = NA_real_,#No perint indicator taxa
            perintper_SE_taxa = AI_Family[which(AI_PerennialIndicator_SE|AI_PerIntIndicator_SE)] %>% unique() %>% length(),
            
            # ephemeral_West_taxa = NA_real_, #No ephemeral indicators
            ephemeral_GP_taxa = AI_Family[which(AI_EphemeralIndicator_GP)] %>% unique() %>% length(),
            ephemeral_East_taxa = AI_Family[which(AI_EphemeralIndicator_East)] %>% unique() %>% length(), #Only 1 ephemeral indicator
            # ephemeral_AW_taxa = NA_real_, #No ephemeral indicators
            # ephemeral_WM_taxa = NA_real_, #No ephemeral indicators
            ephemeral_NGP_taxa = AI_Family[which(AI_EphemeralIndicator_NGP)] %>% unique() %>% length(), #Only 1 ephemeral indicator
            ephemeral_SGP_taxa = AI_Family[which(AI_EphemeralIndicator_SGP)] %>% unique() %>% length(),
            ephemeral_NE_taxa = AI_Family[which(AI_EphemeralIndicator_NE)] %>% unique() %>% length(), #Only 1 ephemeral indicator
            ephemeral_SE_taxa = AI_Family[which(AI_EphemeralIndicator_SE)] %>% unique() %>% length(),#Only 1 ephemeral indicator
            
            
            # ephinteph_West_taxa = NA_real_, #No ephemeral or ephint indicators
            ephinteph_GP_taxa = AI_Family[which(AI_EphemeralIndicator_GP|AI_EphIntIndicator_GP)] %>% unique() %>% length(),
            ephinteph_East_taxa = AI_Family[which(AI_EphemeralIndicator_East|AI_EphIntIndicator_East)] %>% unique() %>% length(), #Only 1 ephemeral indicator
            # ephinteph_AW_taxa = NA_real_, #No ephemeral or ephint indicators
            # ephinteph_WM_taxa = NA_real_, #No ephemeral or ephint indicators
            ephinteph_NGP_taxa = AI_Family[which(AI_EphemeralIndicator_NGP|AI_EphIntIndicator_NGP)] %>% unique() %>% length(), #Only 1 ephemeral indicator
            ephinteph_SGP_taxa = AI_Family[which(AI_EphemeralIndicator_SGP|AI_EphIntIndicator_SGP)] %>% unique() %>% length(),
            ephinteph_NE_taxa = AI_Family[which(AI_EphemeralIndicator_NE|AI_EphIntIndicator_NE)] %>% unique() %>% length(), #Only 1 ephemeral indicator
            ephinteph_SE_taxa = AI_Family[which(AI_EphemeralIndicator_SE|AI_EphIntIndicator_SE)] %>% unique() %>% length(),#Only 1 ephemeral indicator
            
            
           
            
            EPT_abundance = sum(AI_EPT*AI_Abundance),
            EPT_taxa = AI_Family[AI_EPT] %>% unique() %>% length() ,#sum(AI_EPT),
            EPT_relabd = case_when(TotalAbundance==0~1,T~(EPT_abundance / TotalAbundance)),
            EPT_reltaxa = case_when(Richness==0~1,T~(EPT_taxa / Richness)),
            Ephemeroptera_abundance = sum(AI_Ephemeroptera*AI_Abundance),
            Plecoptera_abundance = sum(AI_Plecoptera*AI_Abundance),
            Trichoptera_abundance = sum(AI_Trichoptera*AI_Abundance),
            Hemiptera_abundance = sum(AI_Hemiptera*AI_Abundance),
            Coleoptera_abundance = sum(AI_Coleoptera*AI_Abundance),
            Decapoda_abundance = sum(AI_Decapoda*AI_Abundance),
            Odonata_abundance = sum(AI_Odonata*AI_Abundance),
            Basommatophora_abundance = sum(AI_Basommatophora*AI_Abundance),
            OtherMacro_abundance = sum(AI_OtherMacro*AI_Abundance),
            GOLD_abundance = sum(AI_GOLD*AI_Abundance),
            GOLD_taxa = AI_Family[AI_GOLD] %>% unique() %>% length(),#sum(AI_GOLD),
            OCH_abundance = sum(AI_OCH*AI_Abundance),
            OCH_taxa = AI_Family[AI_OCH] %>% unique() %>% length(),#sum(AI_OCH),
            Noninsect_abundance = sum(AI_Noninsect * AI_Abundance),
            Noninsect_taxa=AI_Family[AI_Noninsect] %>% unique() %>% length(),#sum(AI_Noninsect),
            Noninsect_relabund = case_when(TotalAbundance==0~1,T~(Noninsect_abundance / TotalAbundance)),
            Noninsect_reltaxa = case_when(Richness==0~1,T~(sum(AI_Noninsect) / Richness)),
            GOLD_relabd = case_when(TotalAbundance==0~1,T~(GOLD_abundance / TotalAbundance)),
            GOLD_reltaxa = case_when(Richness==0~1,T~(GOLD_taxa / Richness)),
            OCH_relabd = case_when(TotalAbundance==0~1,T~(OCH_abundance / TotalAbundance)),
            OCH_reltaxa = case_when(Richness==0~1,T~(OCH_taxa / Richness)),
            GOLDOCH_relabd = case_when(TotalAbundance==0~1,T~((OCH_abundance + GOLD_abundance) / TotalAbundance)),
            GOLDOCH_reltaxa = case_when(Richness==0~1,T~((OCH_taxa + GOLD_taxa) / Richness)),
            Crayfish_abundance = sum(AI_Crayfish*AI_Abundance),
            Crayfish_taxa = AI_Family[AI_Crayfish] %>% unique() %>% length(),
            Mollusk_abundance = sum(AI_Mollusk*AI_Abundance),
            Mollusk_taxa = AI_Family[AI_Mollusk] %>% unique() %>% length(),
            Bivalves_NonFG_Abundance = sum(AI_Bivalve_Nonfingernail*AI_Abundance),
            Clam_Fingernail_Abundance = sum(AI_Clams_Fingernail*AI_Abundance),
            TolRelAbund =  case_when(TotalAbundance==0~1,T~(sum(AI_Tolerant*AI_Abundance) / TotalAbundance)),
            TolRelAbundAlt =  case_when(TotalAbundance==0~1,T~(sum(AI_Tolerant_Alt*AI_Abundance) / TotalAbundance)),
            NonTolTaxa = AI_Family[!AI_Tolerant] %>% unique() %>% length(),
            NonTolTaxaAlt = AI_Family[!AI_Tolerant_Alt] %>% unique() %>% length(),
            TolTaxa = AI_Family[AI_Tolerant] %>% unique() %>% length(),
            TolTaxaAlt = AI_Family[AI_Tolerant_Alt] %>% unique() %>% length()
  ) %>%
  ungroup() %>%
  inner_join(xwalk_df %>%
               select(SiteCode=sitecode, Region, Region_detail)) %>% #Inner join drops sitecode "NEUB115". Verify reason.
  mutate(perennial_ISA_abundance = case_when(Region=="West"~perennial_West_abundance,
                                             Region=="GP"~perennial_GP_abundance,
                                             Region=="East"~perennial_East_abundance,
                                             T~NA_real_),
         perintper_ISA_abundance = case_when(Region=="West"~perintper_West_abundance,
                                             Region=="GP"~NA_real_, #No perintper indicators in GP
                                             Region=="East"~perintper_East_abundance,
                                             T~NA_real_),
         ephemeral_ISA_abundance = case_when(Region=="West"~NA_real_,#Only 1 indicator in West
                                             Region=="GP"~ephemeral_GP_abundance, 
                                             Region=="East"~NA_real_,#Only 1 indicator in East
                                             T~NA_real_),
         ephinteph_ISA_abundance = case_when(Region=="West"~NA_real_,#Only 1 indicator in West
           Region=="GP"~ephinteph_GP_abundance, 
           Region=="East"~ephinteph_East_abundance,
           T~NA_real_),
         
         perennial_ISA_taxa = case_when(Region=="West"~perennial_West_taxa,
                                             Region=="GP"~perennial_GP_taxa,
                                             Region=="East"~perennial_East_taxa,
                                             T~NA_real_),
         perintper_ISA_taxa = case_when(Region=="West"~perintper_West_taxa,
                                             Region=="GP"~NA_real_, #No perintper indicators in GP
                                             Region=="East"~perintper_East_taxa,
                                             T~NA_real_),
         ephemeral_ISA_taxa = case_when(Region=="West"~NA_real_,#No indicators in West
                                             Region=="GP"~ephemeral_GP_taxa, 
                                             Region=="East"~NA_real_,#Only 1 indicator in East
                                             T~NA_real_),
         ephinteph_ISA_taxa = case_when(Region=="West"~NA_real_,#No indicators in West
                                             Region=="GP"~ephinteph_GP_taxa, 
                                             Region=="East"~ephinteph_East_taxa,
                                             T~NA_real_),
         
         perennial_ISAsubregion_abundance = case_when(Region_detail=="AW"~perintper_AW_abundance,
                                                      Region_detail=="WM"~perintper_WM_abundance,
                                                      Region_detail %in% c("GP_N","GP_U","GP_C")~perennial_NGP_abundance,
                                                      Region_detail=="GP_S"~perennial_SGP_abundance,
                                                      Region_detail=="NE"~perennial_NE_abundance,
                                                      Region_detail=="SE"~perennial_SE_abundance,
                                                      T~NA_real_),
         perintper_ISAsubregion_abundance = case_when(Region_detail=="AW"~perintper_AW_abundance,
                                                      Region_detail=="WM"~perintper_WM_abundance,
                                                      Region_detail %in% c("GP_N","GP_U","GP_C")~NA_real_, #No perint indicators
                                                      Region_detail=="GP_S"~perintper_SGP_abundance,
                                                      Region_detail=="NE"~NA_real_, #No perint indicators
                                                      Region_detail=="SE"~perintper_SE_abundance,
                                                      T~NA_real_),
         ephemeral_ISAsubregion_abundance = case_when(Region_detail=="AW"~NA_real_, #no ephemeral indicators
                                                      Region_detail=="WM"~NA_real_, #only 1 ephemeral indicator
                                                      Region_detail %in% c("GP_N","GP_U","GP_C")~NA_real_, #Only 1 indicator
                                                      Region_detail=="GP_S"~ephemeral_SGP_abundance,
                                                      Region_detail=="NE"~NA_real_, #Only 1 indicator
                                                      Region_detail=="SE"~NA_real_, #Only 1 indicator
                                                      T~NA_real_),
         ephinteph_ISAsubregion_abundance = case_when(Region_detail=="AW"~NA_real_, #no ephemeral indicators
                                                      Region_detail=="WM"~NA_real_, #Only 1 indicator
                                                      Region_detail %in% c("GP_N","GP_U","GP_C")~ephinteph_NGP_abundance, 
                                                      Region_detail=="GP_S"~ephinteph_SGP_abundance,
                                                      Region_detail=="NE"~ephinteph_NE_abundance,
                                                      Region_detail=="SE"~ephinteph_SE_abundance,
                                                      T~NA_real_),
         
         perennial_ISAsubregion_taxa = case_when(Region_detail=="AW"~perintper_AW_taxa,
                                                      Region_detail=="WM"~perintper_WM_taxa,
                                                      Region_detail %in% c("GP_N","GP_U","GP_C")~perennial_NGP_taxa,
                                                      Region_detail=="GP_S"~perennial_SGP_taxa,
                                                      Region_detail=="NE"~perennial_NE_taxa,
                                                      Region_detail=="SE"~perennial_SE_taxa,
                                                      T~NA_real_),
         perintper_ISAsubregion_taxa = case_when(Region_detail=="AW"~perintper_AW_taxa,
                                                      Region_detail=="WM"~perintper_WM_taxa,
                                                      Region_detail %in% c("GP_N","GP_U","GP_C")~NA_real_, #No perint indicators
                                                      Region_detail=="GP_S"~perintper_SGP_taxa,
                                                      Region_detail=="NE"~NA_real_, #No perint indicators
                                                      Region_detail=="SE"~perintper_SE_taxa,
                                                      T~NA_real_),
         ephemeral_ISAsubregion_taxa = case_when(Region_detail=="AW"~NA_real_, #no ephemeral indicators
                                                      Region_detail=="WM"~NA_real_, #only 1 ephemeral indicator
                                                      Region_detail %in% c("GP_N","GP_U","GP_C")~NA_real_, #Only 1 indicator
                                                      Region_detail=="GP_S"~ephemeral_SGP_taxa,
                                                      Region_detail=="NE"~NA_real_, #Only 1 indicator
                                                      Region_detail=="SE"~NA_real_, #Only 1 indicator
                                                      T~NA_real_),
         ephinteph_ISAsubregion_taxa = case_when(Region_detail=="AW"~NA_real_, #no ephemeral indicators
                                                      Region_detail=="WM"~NA_real_, #Only 1 indicator
                                                      Region_detail %in% c("GP_N","GP_U","GP_C")~ephinteph_NGP_taxa, 
                                                      Region_detail=="GP_S"~ephinteph_SGP_taxa,
                                                      Region_detail=="NE"~ephinteph_NE_taxa,
                                                      Region_detail=="SE"~ephinteph_SE_taxa,
                                                      T~NA_real_),
         
         )


ai_metrics %>%
  select(Region, Region_detail, contains("ISA")) %>%
  filter(Region!="CB") %>%
  pivot_longer(cols=contains("ISA")) %>%
  group_by(Region, Region_detail, name) %>%
  summarise(val=sum(!is.na(value))) %>%
  pivot_wider(names_from=name, values_from = val) %>%
  write_clip()

ai_metrics %>%
  select(ParentGlobalID, SiteCode) %>% unique() %>% group_by(ParentGlobalID) %>% tally() %>% filter(n==1)

write_csv(ai_metrics, "Data/ai_metrics.csv")
skim_without_charts(ai_metrics)

junk<-sample_n(ai_metrics, size=1)

junk$perennial_AW_abundance

ai_combined2 %>%
  filter(ParentGlobalID %in% junk$ParentGlobalID) %>%
  
  group_by(AI_PerennialIndicator_AW,AI_Family) %>%
  summarise(TOT = sum(AI_Abundance))

######
new_isa_metrics_wet_plot<-ai_metrics %>%
  inner_join(
    xwalk_df %>%
      filter(Region=="West") %>%
      select(SiteCode=sitecode, Class)
  ) %>%
  select(Class, Region_detail, contains("perennial"), contains("perint"), contains("ephemeral")) %>%
  pivot_longer(cols=c(contains("perennial"), contains("perint"), contains("ephemeral"))) %>%
  filter(str_detect(name,"Beta")|str_detect(name,"West")) %>%
  filter(Class!="U") %>%
  ggplot(aes(x=Class, y=value, fill=Region_detail))+
  geom_boxplot()+
  facet_wrap(~name, scales="free", ncol=4)+
  theme(legend.position = "bottom")
# geom_point(data=. %>% filter(value==0), position=position_jitterdodge(jitter.height=0, jitter.width=.1), color="red", alpha=.02, size=.5)
ggsave(new_isa_metrics_wet_plot, filename="NotForGit/new_isa_metrics_wet_plot.png", width = 10, height=8)
