### Exploring CRQA Parameters

library(psych)

# set working directory
setwd("~/Dropbox/Dyadic_Data_Course/CRQA")

# read in the results of the CRQA created in earlier script
crqa.results <- read.csv("crqa_results.csv")
crqa.profile <- read.csv("crqa_profiles.csv")

# read in data with moderators
labdat <- read.csv("lab.csv")

# format the relationship variables in wide format

# grab just male values
male.vars <- labdat %>%
  filter(sexm == "Men") %>%
  select(-X, -sexm, -ID, -timeCont, -MeanSC, -IBI, -RespRate, -RespAmp) %>%
  distinct()

# just female values
female.vars <- labdat %>%
  filter(sexm == "Women") %>%
  select(-X, -sexm, -ID, -timeCont, -MeanSC, -IBI, -RespRate, -RespAmp) %>%
  distinct()

# renaming variables to be male / female
names(male.vars)[2:15] <- paste(names(male.vars)[2:15], "male", sep="_")
names(female.vars)[2:15] <- paste(names(female.vars)[2:15], "female", sep="_")

# merging male and female
wide.vars <- merge(male.vars, female.vars, by=c("Dyad"))

### Examining Profiles

# reformating the data for plotting profile
prof.wide <- crqa.profile
names(prof.wide)[2:12] <- seq(from=-5, to=5)
  
# creating long form for plotting
prof.long <- prof.wide %>%
  gather("lag", "rr", -max, -dyad) %>%
  mutate(lag=as.numeric(lag), rr=as.numeric(rr))

# add covariates
prof.long2 <- merge(prof.long, wide.vars, by.x="dyad", by.y="Dyad")

# make the profile plot
ggplot(prof.long, aes(y=rr, x=lag, group=dyad))+
  geom_point(aes(color=dyad))+
  geom_line(aes(color=dyad))+
  theme_bw()+
  theme(legend.position="none")

# creating categorical variable for dyadic coping by women
prof.long2$dCope_f_cat <- as.factor(as.numeric(cut(prof.long2$dyadicCope_female, breaks=c(-2, 1, 2, 4))))

# creating categorical variable for influence by men
prof.long2$inf_m_cat <- as.factor(as.numeric(cut(prof.long2$influence_male, breaks=c(-4, -2, -1, 4))))

# plot by values of interesting variables
ggplot(na.omit(prof.long2), aes(y=rr, x=lag))+
  geom_smooth(method="loess")+
  facet_grid(.~dCope_f_cat)+
  geom_vline(xintercept=0, color="red", lty=2)+
  labs(title="Recurrence Profile by Female's Dyadic Coping Score")

ggplot(na.omit(prof.long2), aes(y=rr, x=lag))+
  geom_smooth(method="loess")+
  facet_grid(.~inf_m_cat)+
  geom_vline(xintercept=0, color="red", lty=2)+
  labs(title="Recurrence Profile by Male's Influence Score")

# merging profiles data with relationship variables
prof.wide2 <- merge(crqa.profile, wide.vars, by.x="dyad", by.y="Dyad") %>%
  filter(max != "missing")

# convert variables to numeric
prof.wide2[,2:13] <- apply(prof.wide2[,2:13], 2, function(x) as.numeric(as.character(x)))

# check out what correlates with lag 0 RR
cor(x=prof.wide2[,7], y=prof.wide2[,14:41], use="pairwise.complete.obs")

# check out distribution of highly correlated variables
hist(prof.wide2$dyadicCope_female)
hist(prof.wide2$influence_male)

### Inferential Stats

crqa.wide2 <- merge(crqa.results, wide.vars, by.x="dyad", by.y="Dyad") %>%
  filter(RR != "missing")

# convert CRQA parameters to numeric
crqa.wide2[,2:10] <- apply(crqa.wide2[,2:10], 2, function(x) as.numeric(as.character(x)))

# save values of correlations of CRQA parameters
crqaCors <- cor(crqa.wide2[,2:10], use="pairwise.complete.obs")

# plot correlations of CRQA parameters
pairs.panels(crqa.wide2[,2:10], pch=15)

# examining the dyadic coping variable again
fit <- lm(DET ~ dyadicCope_female + dyadicCope_male, data=crqa.wide2)
summary(fit)

# plotting the interesting results

# main effect of female coping
ggplot(crqa.wide2, aes(y=DET, x=dyadicCope_female))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

# main effect of male coping
ggplot(crqa.wide2, aes(y=DET, x=dyadicCope_male))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()
