data(ilri.sheep)
summary(ilri.sheep)


ggplot(ilri.sheep,aes(x=gen,y=birthwt,fill=sex))+
  geom_boxplot()

anova_example<-lm(birthwt~ewegen*ramgen*sex,data=ilri.sheep)
anova(anova_example)
summary(anova_example)

anova_example2<-lm(birthwt~ewegen+ramgen+sex,data=ilri.sheep)
anova(anova_example2)
summary(anova_example2)

anova_example3<-lm(birthwt~ewegen*ramgen+sex,data=ilri.sheep)
anova(anova_example3)
summary(anova_example3)

anova_example3<-lm(birthwt~ewegen+ramgen+sex+factor(year),data=ilri.sheep)
car::Anova(anova_example3,type="3")
summary(anova_example3)

library(lmerTest)
anova_example4<-lmer(birthwt~ewegen+ramgen+sex+factor(year)+(1|ewe)+(1|ram),data=ilri.sheep)
anova(anova_example4)
summary(anova_example4)
