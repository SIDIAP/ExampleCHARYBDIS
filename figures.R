# Figures

# Going to compare the incidence of a feature (ECMO)
# over 0 through 30 days 
# for a hospitalised cohort
# for a specific set of databases
# and for overall, and then by age and sex strata


results.folder<-"C:/Users/Ed/Dropbox/OHDSI/covid/charybdis/StudyResults"
cohort.names<-c("Persons hospitalized with a COVID-19 diagnosis record or a SARS-CoV-2 positive test with no required prior observation") 
feature.names<-c(#"mechanical ventilation during hospitalization",
                 "tracheostomy during hospitalization",
                 "dialysis during hospitalization",
                 "ECMO during hospitalization"#,"death"
                 )
time.window<-"0 through 30"
databases<-c("CDM_Premier_COVID_v1240", 
             "IQVIA_OpenClaims", 
             "optum_ehr_covid_v1239", 
             "HealthVerity",
             "VA-OMOP")
strata<-c("Age >= 65", "Age < 65", "Sex = Female", "Sex = Male")

# packages-----
library(dplyr)
library(stringr)
library(ggplot2)
library(prevalence)


# functions ------
# for counts- without decimal place
options(scipen = 999)
nice.num.count<-function(x) {
  format(x,
         big.mark=",", nsmall = 0, digits=0)}

# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  format(round(x,1),
         big.mark=",", nsmall = 1, digits=1)}

get.results.set<-
  function(results.folder, 
           merge.results){
if(merge.results==TRUE){
preMergeResultsFiles(results.folder)
  }else {
load(paste0(results.folder, "/PreMerged.RData"), envir = globalenv())
  }
  }

# get results -----
get.results.set(results.folder=results.folder, 
           merge.results=FALSE) # we've already put them together

# cohort counts
cohort_count<-cohortStagingCount %>% 
  filter(name=={{cohort.names}} |
      (str_detect(name, {{cohort.names}}) &
           str_detect(name, paste({{strata}},collapse = '|')))) %>% 
  filter(databaseId %in% {{databases}}) %>% 
  distinct()
cohort_count<-cohort_count %>% 
  select(databaseId, cohortId, name, cohortSubjects) %>% 
  rename(n=cohortSubjects) %>% 
  filter(!is.na(n)) %>% 
  filter(n>0)
cohort_count

# features
features<-cohort_count %>% 
  left_join(featureProportions %>% 
              filter(featureName %in% {{feature.names}}) %>% 
              filter(str_detect(covariateName,{{time.window}})),
            by=c("databaseId","cohortId")) 
features<-features %>% 
  mutate(databaseId=
           ifelse(databaseId=="IQVIA_OpenClaims", "Open Claims",
           ifelse(databaseId=="optum_ehr_covid_v1239", "OPTUM",databaseId))) %>% 
  mutate(var=ifelse(str_detect(name, 
                               paste(c("Age >= 65", "Age < 65"), collapse = "|")),
                    "Age group",
              ifelse(str_detect(name, 
                               paste(c("Sex = Female", "Sex = Male"), collapse = "|")),
                    "Sex", 
                    "Overall"))) %>% 
  mutate(level=
       ifelse(str_detect(name, "Age < 65"),"Age < 65",
       ifelse(str_detect(name, "Age >= 65"),"Age >= 65",       
       ifelse(str_detect(name, "Female"),"Female",    
      ifelse(str_detect(name, "Male"),"Male",
             "Overall")))))
           



# add confidence interval -----
#exact
features<-cbind(features %>% 
                  mutate(featureCount=ifelse(!is.na(featureCount), featureCount,
                                              NA)),
  propCI(x = features$featureCount, n = features$n, method="exact")[,6:7])


# plot-----
features<-features %>% 
  mutate(database.name=
           
 ifelse(
    databaseId=="Open Claims",
          paste0("Open Claims\n(n: ", 
                  nice.num.count(as.numeric(cohort_count %>%
  filter(cohortId=="135") %>% 
    filter(databaseId=="IQVIA_OpenClaims") %>% 
  select(n))), 
  " COVID-19)"), 

ifelse( databaseId=="CDM_Premier_COVID_v1240",
          paste0("Premier\n(n: ", 
                  nice.num.count(as.numeric(cohort_count %>%
  filter(cohortId=="135") %>% 
    filter(databaseId=="CDM_Premier_COVID_v1240") %>% 
  select(n))), 
  " COVID-19)") ,
  
ifelse(
    databaseId=="OPTUM",
          paste0("OPTUM\n(n: ", 
                  nice.num.count(as.numeric(cohort_count %>%
  filter(cohortId=="135") %>% 
    filter(databaseId=="optum_ehr_covid_v1239") %>% 
  select(n))), 
  " COVID-19)") ,
  
  ifelse(
    databaseId=="HealthVerity",
          paste0("HealthVerity\n(n: ", 
                  nice.num.count(as.numeric(cohort_count %>%
  filter(cohortId=="135") %>% 
    filter(databaseId=="HealthVerity") %>% 
  select(n))), 
  " COVID-19)") ,
  
  
  ifelse(
    databaseId=="VA-OMOP",
          paste0("VA-OMOP\n(n: ", 
                  nice.num.count(as.numeric(cohort_count %>%
  filter(cohortId=="135") %>% 
    filter(databaseId=="VA-OMOP") %>% 
  select(n))), 
  " COVID-19)") ,
  
  databaseId))))))



features %>% 
  filter(featureName!="death") %>% 
  filter(!is.na(mean)) %>% 
  filter(mean>0) %>% 
  mutate(featureName=
     ifelse(featureName=="mechanical ventilation during hospitalization",
            "Mechanical ventilation",
     ifelse(featureName=="tracheostomy during hospitalization",
            "Tracheostomy",
     ifelse(featureName=="dialysis during hospitalization",
            "Dialysis",
      ifelse( featureName=="ECMO during hospitalization", 
              "ECMO", NA))))) %>% 
  mutate(featureName=factor(featureName,
                            levels=c(
                              "ECMO",
                              "Tracheostomy",
                             "Dialysis",
                             "Mechanical ventilation"
                            ))) %>% 
  ggplot()+
  geom_point(aes(mean,level, colour=level, shape=var), size=3) + 
  geom_errorbarh(aes(mean, level, xmin=lower, xmax=upper,
                     colour=level), height=0, size=1)+
  facet_grid(database.name~featureName, scales = "free", #space = "free",
             switch = "y") +
  theme_bw()+
  scale_x_continuous(limits=c(0,NA),
    #breaks = seq(0,0.005,0.0025),
                     labels = scales::percent_format())+
  scale_y_discrete(position = "right")+
  theme(panel.spacing.x=unit(0.75, "lines"),
        panel.spacing.y=unit(0.75, "lines"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
        axis.title.y=element_blank(),
     #   axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=14, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0),
        legend.text=element_text(size=11))+
  xlab("\n30-day incidence")
  


# additional: get some counts for paper ----
# n
features %>% 
  filter(var=="Overall") %>% 
    filter(covariateName=="Cohort during day 0 through 30 days start the index: ECMO during hospitalization") %>% 
  select(totalCount) %>% 
  summarise(sum(totalCount))
# ECMO
features %>% 
  filter(var=="Overall") %>% 
  filter(covariateName=="Cohort during day 0 through 30 days start the index: ECMO during hospitalization") %>% 
  select(databaseId, featureCount)

features %>% 
  filter(var=="Overall") %>% 
  filter(covariateName=="Cohort during day 0 through 30 days start the index: ECMO during hospitalization") %>% 
  summarise(f.count=sum(featureCount),
           count=sum(totalCount) ,
           prop=nice.num((f.count/count)*100))


# tracheostomy
features %>% 
  filter(var=="Overall") %>% 
  filter(covariateName=="Cohort during day 0 through 30 days start the index: tracheostomy during hospitalization") %>% 
  select(databaseId, featureCount,totalCount) %>% 
  summarise(f.count=sum(featureCount),
           count=sum(totalCount) ,
           prop=nice.num((f.count/count)*100))


features %>% 
  filter(var=="Overall") %>% 
  filter(covariateName=="Cohort during day 0 through 30 days start the index: tracheostomy during hospitalization") %>% 
  select(databaseId, featureCount,totalCount) %>% 
  summarise(f.count=sum(featureCount),
           count=sum(totalCount) ,
           prop=nice.num((f.count/count)*100))


# dialysis
features %>% 
  filter(var=="Overall") %>% 
  filter(covariateName=="Cohort during day 0 through 30 days start the index: dialysis during hospitalization") %>% 
  select(databaseId, featureCount)

features %>% 
  filter(var=="Overall") %>% 
  filter(covariateName=="Cohort during day 0 through 30 days start the index: dialysis during hospitalization") %>% 
  select(databaseId, featureCount,totalCount) %>% 
  summarise(f.count=sum(featureCount),
           count=sum(totalCount) ,
           prop=nice.num((f.count/count)*100))

# mechanical ventilation
features %>% 
  filter(var=="Overall") %>% 
  filter(covariateName=="Cohort during day 0 through 30 days start the index: mechanical ventilation during hospitalization") %>% 
  select(databaseId, featureCount,totalCount) %>% 
  group_by(databaseId) %>% 
  summarise(f.count=sum(featureCount),
           count=sum(totalCount) ,
           prop=nice.num((f.count/count)*100))

features %>% 
  filter(var=="Overall") %>% 
  filter(covariateName=="Cohort during day 0 through 30 days start the index: mechanical ventilation during hospitalization") %>% 
  select(databaseId, featureCount,totalCount) %>% 
  summarise(f.count=sum(featureCount),
           count=sum(totalCount) ,
           prop=nice.num((f.count/count)*100))
