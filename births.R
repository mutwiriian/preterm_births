library(tidyverse)
library(glue)

births <- readxl::read_xlsx('births.xlsx',
                            na = c('','not indicated','Not Indicated',
                                   'not Indicated','Unkown','Not indicated'))

births<- births %>% 
  mutate(birth_weight=case_when(
    birth_weight>=2500 & birth_weight<=3000~'2500-3000g',
    birth_weight>3000~'>3000g',
    birth_weight<2500~'<2500g',
    TRUE~birth_weight
  ),
  pre_term=if_else(gestational_age<37,TRUE,FALSE),
  across(.cols = c(occupation,education),.fns = ~str_to_title(.)),
  occupation=str_replace_all(occupation,'Self Employed','Self-Employed'),
  mode_of_delivery=if_else(mode_of_delivery=='Ceserian','Caesarean section',mode_of_delivery)
  )

births %>% 
  group_by(pre_term) %>% 
  summarise(
    n=n()
  )

#Complications were common among women with induced on set of labor (60%)
#compared to those who had spontaneous onset of labor(40%)
births %>% 
  select(onset_of_labor,complication) %>% 
  group_by(onset_of_labor) %>% 
  count(complication) %>% 
  mutate(prop=scales::percent(n/sum(n)),
         label=glue("{n}, {prop}")) %>% 
  ggplot(aes(reorder(onset_of_labor,-n),n))+
  geom_col(aes(fill=complication))+
  geom_text(data = . %>% filter(onset_of_labor=='Spontaneous'),
            aes(label=label),vjust=2)+
  annotate(x='Induced',y=22,geom = 'text',label=17)+
  scale_fill_discrete(name='Complication?')

births %>% 
  select(onset_of_labor,complication) %>% 
  group_by(onset_of_labor) %>% 
  count(complication) %>% 
  mutate(prop=n/sum(n),
         label=paste0(n,',',scales::percent(prop))) %>% 
  filter(onset_of_labor=='Spontaneous')

births %>% 
  select(pre_term,anc_visits) %>% 
  group_by(pre_term) %>% 
  summarise(n=n(),
            avg=mean(anc_visits,na.rm=T))

births %>% 
  select(mode_of_delivery,complication) %>% 
  na.omit() %>% 
  group_by(mode_of_delivery,complication) %>% 
  summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(reorder(mode_of_delivery,n),n,fill=complication))+
  geom_col()+
  geom_text(aes(mode_of_delivery,n,label=round(prop,2)))+
  coord_flip()+
  scale_fill_manual(values = color_pal)

births %>% 
  select(birth_weight,complication) %>% 
  infer::observe(formula = complication~birth_weight,stat = 'Chisq')


ttt_ifas <- readxl::read_xlsx('factors.xlsx') %>% 
  select('Tetanus Toxoid','IFAS','Hb')

library(gt)
library(gtsummary)
ttt_ifas %>% 
  group_by(`Tetanus Toxoid`) %>% 
  summarise(n=n())

ttt_ifas %>% 
  group_by(IFAS) %>% 
  summarise(n=n())

ttt_ifas %>% 
  mutate(Hb=as.numeric(Hb))%>% 
  summarise(Mothers=n(),
            Average=mean(Hb,na.rm=T)) %>% 
  gt() %>% 
  tab_header(title = 'Hb levels reported')

births %>% 
  select(blood_group) %>% 
  group_by(blood_group) %>% 
  summarise(n=n()) %>% 
  gt()


null_mod <- glm(nature~1,data = births,family = binomial())
summary(null_mod)

full_mod <- glm(nature~marital_status+mother_age+
                  education+complication+birth_weight+mode_of_delivery+
                  anc_visits+prev_pre_term,family = binomial(),
                data = births %>% na.omit())
summary(full_mod)

back <- step(full_mod,
             scope = list(lower=formula(null_mod),
                          upper=formula(full_mod)),
             direction = 'both',
             trace=0)

glimpse(births)

births %>% 
  select(complication,nature) %>% 
  tbl_summary(by=complication) %>% 
  add_p()

#pre-term births were more common among mothers with induced labor
#in contrast to more term births among mothers with spontaneous labor
#as is confirmed by p<001 from a Chi-squared test. This implies on-set 
#as a risk factor for occurrence of term and pre-term births.


births %>% 
  select(nature,onset_of_labor) %>% 
  ggplot(aes(nature,fill=onset_of_labor))+
  geom_bar()

births %>% 
  select(nature,onset_of_labor) %>% 
  filter(onset_of_labor!='Unknown') %>% 
  tbl_summary(by=nature) %>% 
  add_p()


births %>% 
  group_by(nature) %>% 
  

















