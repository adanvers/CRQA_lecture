### Great Cool Fun CRQA
library(ggplot2)
library(crqa)
library(tidyr)

### Step 1: Read in Data and Explore / Plot

# set working directory
setwd("~/Dropbox/Dyadic_Data_Course/CRQA")

# read in the data
labdat <- read.csv("lab.csv")

# plotting a histogram of Mean SC
hist(labdat$MeanSC)

# making plots of people's trajectories of Mean SC
ggplot(labdat, aes(y=MeanSC, x=timeCont, group=as.factor(ID)))+
  geom_smooth(aes(color=as.factor(ID)), se=F)+
  theme_bw()+
  theme(legend.position="none")

# diving the data into three bins
labdat$SC5 <- as.factor(as.numeric(cut(labdat$MeanSC, 5)))

# plotting the division of the data
plot(labdat$SC5) # frequency
plot(labdat$SC5, labdat$MeanSC, ylab="Original SC", xlab="Category") # boxplots

### Step 2: Create CRQA Plots
# Note: the strategy is to create a CRQA plot for each dyad separately, then combine

# get a list of all the dyads
dyads <- unique(labdat$Dyad)

# initialize a data frame to save CRQA parameters
crqa.results <- matrix(NA, nrow=length(dyads), ncol=10)

# initialize a data frame to save cross recurrence profile
crqa.profile <- matrix(NA, nrow=length(dyads), ncol=13)

# cycle through all the dyads in a loop
for (d in 1:length(dyads)) {
  
  # selecting data from just one dyad
  dyad.dat <- labdat[which(labdat$Dyad == dyads[d]),]
  
  # separate out the time series
  male.ts <- as.numeric(dyad.dat[which(dyad.dat$sexm == "Men"), "SC5"])
  female.ts <- as.numeric(dyad.dat[which(dyad.dat$sexm == "Women"), "SC5"])
  
  # if all values of the time series are NA, skip that dyad
  if (sum(is.na(male.ts)) >= 1 | sum(is.na(female.ts)) >= 1) {
    
    crqa.results[d,] <- c(dyads[d], rep("missing", times=9))
    crqa.profile[d,] <- c(dyads[d], rep("missing", times=12))
    
  } else if (sd(male.ts) == 0 | sd(female.ts) == 0) {
    
    crqa.results[d,] <- c(dyads[d], rep("SD0", times=9))
    crqa.profile[d,] <- c(dyads[d], rep("SD0", times=12))
    
  } else  {
    
    # plot the raw data
    rawPlot <- ggplot(dyad.dat, aes(y=SC5, x=timeCont, group=sexm))+
      geom_point(aes(color=sexm))+
      geom_line(aes(color=sexm))+
      theme_bw()+
      labs(x="Time", y="SC Category", title=paste("Dyad", dyads[d], "Raw Categorical Data Plot", paste=" "))
    
    # we are saving all the plots as PDFs for later review
    pdf(paste("plots_raw_cat/dyad", dyads[d], "_rawCat.pdf", sep=""))
    print(rawPlot)
    dev.off()
    
    ### create the CRQA plot
    
    # do the CRQA analysis!
    dyad.crqa <- crqa(male.ts, female.ts, # here we put the two time series!
                      delay=1, # for categorical, this should typically be 1
                      embed=1, # for categorical, this should typically be 1
                      rescale=0, # no need to z-score when using categorical
                      radius=0.01, # just make this a small number
                      normalize=0, # no need to normalize when using categorical
                      mindiagline=2, # min of 2 points needed to define a line
                      minvertline=2, # min of 2 points needed to define a line
                      tw=0,
                      side="both")
    
    # get the CRQA parameters
    crqa.params <- dyad.crqa[1:9]
    
    # put the CRQA parameters in the results data frame
    crqa.results[d,] <- c(dyads[d], unlist(crqa.params))
    
    ### make the CRQA plot
    
    # convert cross-recurrence output into a dataframe for easier plotting
    cross_rec_df = data.frame(points = dyad.crqa$RP@i,
                              loc = seq_along(dyad.crqa$RP@i))
    
    # build the CRP
    crqaPlot <- ggplot(cross_rec_df,aes(x=points,
                                        y=loc)) +
      geom_point(color="blue",size=0.8) +
      theme_classic() +
      theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
      ylab("Female SC") + xlab("Male SC") +
      ggtitle(paste("CRQA Plot for Dyad", dyads[d], sep=" "))
    
    # we are saving all the plots as PDFs for later review
    pdf(paste("plots_CRQA/dyad", dyads[d], "_CRQA.pdf", sep=""))
    print(crqaPlot)
    dev.off()
    
    ### Check Diagonal Recurrence Profile
    drp_results = drpdfromts(male.ts,female.ts,ws=5,datatype="categorical")
    
    # save diagonal recurrence profile
    crqa.profile[d,] <- c(dyads[d], drp_results$profile, drp_results$maxrec)
    
  }
  
}

crqa.results <- as.data.frame(crqa.results)
names(crqa.results) <- c("dyad", "RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

crqa.profile <- as.data.frame(crqa.profile)
names(crqa.profile) <- c("dyad", "n5", "n4", "n3", "n2", "n1", "l0", "p1", "p2", "p3", "p4", "p5", "max")

# save the results
write.csv(crqa.results, "crqa_results.csv")
write.csv(crqa.profile, "crqa_profiles.csv")
  
