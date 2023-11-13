library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(RColorBrewer)

data<- read_csv("La Responsabilité Sociétale de l'IAE (réponses) - Réponses au formulaire 1.csv")

questions<-tibble(Questions=c(names(data)),nb_var=c(1:ncol(data)))

questions<-questions%>%
  mutate(crochet=if_else(
  str_detect(Questions, "\\[")==TRUE,str_split_i(Questions, "\\[",-1)%>%str_remove(., "\\]"), NA),
  new_name=paste0("Q_",nb_var))

names(data)<-questions$new_name

data<-data%>%
  mutate(Non=ifelse(str_detect(Q_26,"Non")==1,1,0),
         `Mieux comprendre`=ifelse(str_detect(Q_26,"comprendre")==1,1,0),
         `Savoir agir`=ifelse(str_detect(Q_26,"agir")==1,1,0),
         Autre=ifelse(str_detect(Q_26,"acteurs")==1,1,0))


TR<-nrow(data)/114
TRB<-nrow(filter(data, Q_18=="Personnel BIATSS"))/7
TRE<-nrow(filter(data, Q_18!="Personnel BIATSS"))/(114-7)

QO<-tibble(QO=c(questions[20,1],questions[21,1],questions[24,1],questions[25,1],questions[27,1]),
           n=c(filter(data,!is.na(Q_20))%>%nrow(),filter(data,!is.na(Q_21))%>%nrow(),
               filter(data,!is.na(Q_24))%>%nrow(),filter(data,!is.na(Q_25))%>%nrow(),
               filter(data,!is.na(Q_27))%>%nrow()))

QO24<-filter(data,!is.na(Q_24))

QO24%>%corpus(text_field = "Q_24")%>%
  tokens(., remove_punct=T, remove_numbers=T)%>%
  tokens_remove(stopwords("fr"))%>%
  dfm()%>%textplot_wordcloud(min_count=1, color=rev(RColorBrewer::brewer.pal(8, "Set2")))

QO25<-filter(data,!is.na(Q_25))%>%select(Q_25)

QO25%>%corpus(text_field = "Q_25")%>%
  tokens(., remove_punct=T, remove_numbers=T)%>%
  tokens_remove(stopwords("fr"))%>%
  dfm()%>%
  textplot_wordcloud(min_count=1, color=rev(RColorBrewer::brewer.pal(8, "Set2")))


data<-data%>%slice(-1)
  

Q2<-data%>%count(Q_2)%>%mutate(prop=n/sum(n), Q="Q"
                              )

ggplot(Q2,aes(prop,Q,fill=factor(Q_2, levels=c(7,6,5,4,3,2,1))))+
  geom_col(width = 0.4,show.legend = F)+
  # geom_label(aes(label=n), show.legend = F)+
   annotate("text",x=0.6, y=1.3,
            label=paste("Total =", round(sum(filter(Q2,Q_2>=5)$prop),4)*100, "% \n Soit", sum(filter(Q2,Q_2>=5)$n), "répondants"))+
  annotate("rect",xmin=0.265, xmax = 1.01, ymin=0.7, ymax = 1.4, alpha=0.2, color="green4", fill="lightgreen")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_discrete(labels=NULL)+
  scale_fill_manual(values=c("chartreuse","green3","lightgreen","gold","orange","pink","red"))+
  labs(y=NULL, x=NULL, 
       title=questions[2,1], 
       subtitle = paste("Rouge - pas du tout, Vert - tout à fait\nn =", nrow(data), "répondants"))+
  theme_minimal()


ggplot(data, aes(Q_18))+
  geom_bar(aes(fill=Q_18), show.legend = F)+
  geom_label(aes(label=after_stat(count)),stat = "count", show.legend = F)+
  labs(y=NULL, x=NULL, 
       title=questions[18,1], 
       subtitle = paste("n =", nrow(data), "répondants"))+
  theme_light()


data%>%filter(Q_18=="Personnel BIATSS")%>%
  ggplot(aes(Q_19, fill=Q_19))+
  geom_bar(show.legend = F)+coord_flip()+
  labs(y=NULL, x=NULL, title=questions[19,1], 
       subtitle = paste("Personnel BIATSS, n =", nrow(filter(data,Q_18=="Personnel BIATSS")), "répondants"))+
  scale_x_discrete(labels=c("Pas particulièrement","De manière transversale"))+
  theme_light()

Q22<-data%>%filter(Q_18!="Personnel BIATSS")%>%count(Q_22)%>%mutate(prop=n/sum(n), Q="Q")%>%add_row(Q_22=2,n=0,prop=0, Q="Q")

ggplot(Q22,aes(prop,Q,fill=factor(Q_22, levels=c(7,6,5,4,3,2,1))))+
  geom_col(width = 0.4,show.legend = F)+
  # geom_label(aes(label=n), show.legend = F)+
  annotate("text",x=0.6, y=1.3,label=paste("Total =", round(sum(filter(Q22,Q_22>=5)$prop),4)*100, "% \n Soit", sum(filter(Q22,Q_22>=5)$n), "répondants"))+
  annotate("rect",xmin=0.1, xmax = 1.01, ymin=0.7, ymax = 1.4, alpha=0.2, color="green4", fill="lightgreen")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_discrete(labels=NULL)+
  scale_fill_manual(values=c("chartreuse","green3","lightgreen","gold","orange","pink","red"))+
  labs(y=NULL, x=NULL, 
       title=questions[22,1],
       subtitle = paste("Rouge - pas du tout d'accord, Vert - tout à fait d'accord\nPersonnel enseignant, n =", nrow(filter(data,Q_18!="Personnel BIATSS")), "répondants"))+
  theme_light()


data%>%filter(Q_18!="Personnel BIATSS")%>%
  ggplot(aes(Q_23, fill=Q_23))+
  geom_bar(show.legend = F)+coord_flip()+
  labs(y=NULL, x=NULL, title=questions[19,1], 
       subtitle = paste("Personnel enseignant, n =", nrow(filter(data,Q_18!="Personnel BIATSS")), "répondants"))+
  scale_x_discrete(labels=c("Pour les UE qui s'y prêtent","Tout au long des enseignements","Pas particulièrement"))+
  theme_light()



Q26<-data%>%select(Non,`Mieux comprendre`,`Savoir agir`, Autre)%>%
  pivot_longer(everything())%>%
  filter(value==1)%>%
  mutate(name=factor(name, levels=c("Autre", "Savoir agir", "Mieux comprendre", "Non")))%>%
  count(name, value)%>%
  mutate(prop=n/nrow(data))

ggplot(Q26, aes(name, prop))+
  geom_col(aes(fill=name), show.legend = F)+
  geom_label(aes(label=n))+coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_discrete(type=c("#56B4E9", "#F0E442","#009E73", "#D55E00"))+
  labs(x=NULL, y=NULL, title=questions[26,1])+
  theme_light()
  
cor.test(data$Q_2,data$`Non`)



dvq<-questions[3:10,3]%>%
  arrange(row_number()==8)%>%
  arrange(between(row_number(),1,7))%>%
  mutate(crochet=str_wrap(crochet,width = 30))
data%>%select(Q_3:Q_10)%>%
  mutate(across(everything(), ~factor(., levels=c("Oui, tout à fait", "4", "3", "2", "Non, pas du tout"))))%>%
  pivot_longer(everything())%>%
  ggplot(aes(name))+
  geom_bar(aes(fill=value), position = "fill")+
  coord_flip()+
  scale_x_discrete(labels=dvq$crochet)+
  scale_fill_discrete(type=c("green4","lightgreen","gold","pink","darkred"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=NULL, y=NULL, title = "Diriez-vous que ...", fill=NULL,
       subtitle = paste("n =", nrow(data), "répondants"))+
  theme_minimal()


piliers<-questions[11:17,3]%>%
  mutate(crochet=str_wrap(crochet,width = 20))


data%>%select(Q_11:Q_17)%>%
  mutate(across(everything(), ~str_wrap(., width=25)))%>%
  mutate(across(everything(), ~factor(.)))%>%
  pivot_longer(everything())%>%
  ggplot(aes(name))+
  geom_bar(aes(fill=value), position = "fill", show.legend = T)+
  coord_flip()+
  scale_x_discrete(labels=piliers$crochet)+
  scale_fill_discrete(type=c("lightgreen","gold","pink"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=NULL, y=NULL, title = "Les piliers de la RSE", fill=NULL,
       subtitle = paste("n =", nrow(data), "répondants"))+
  theme_minimal()+  theme(legend.position="bottom")+coord_polar(theta = "x",direction = -1, clip = "off")



