# Comment text by typing Cntl+Shift+C

# UNAIDS Sustainability Analysis 
# For Pharos Global Health Advisors
# Stephen Resch Feb 2021


# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)



# Setup directory variables
baseDir <- getwd()
dataDir <- file.path(baseDir, "data")
metaDir <- file.path(baseDir, "meta")
resultsDir <- file.path(baseDir, "results")


# Load data
df_aidsinfovsihme <- read.csv('data/IHMEvAIDSINFO.csv')

df_ihme <- read.csv('data/IHME_HIVAIDS_SPENDING_2000_2017_Y2020M04D23.CSV')
GHED_data <- read_excel("data/GHED_data.XLSX", sheet = "Data")
df_other <- read.csv('data/Data_Nov_15_join.csv')
df_small <- select(df_ihme, iso3, level, year, the_total_mean, ghes_total_mean,ppp_total_mean, 
                   oop_total_mean, care_total_mean, prev_total_mean, other_total_mean, dah_total)
df_small <- filter(df_small, level == "Country")
Trend_of_new_HIV_infections <- read_excel("data/Epidemic transition metrics_Trend of new HIV infections.xlsx", 
                                                                      sheet = "Progress")
Trend_IHME_TAEpPWH <- read_excel("C:/Users/sresc/Google Drive/CONSULTING/Active Project Work/Pharos/UNAIDS_sus/IHME_HIVAIDS_SPENDING_2000_2017_CSV/Trend_IHME_TAEpPWH.xlsx", 
                                 sheet = "Trend_IHME_TAEperPWH")

df_progress_vs_spend <- inner_join(Trend_of_new_HIV_infections,Trend_IHME_TAEpPWH, "ISO")

# Test for Correlation between Progress on incidence reduction 2010-2019 and Average spending over 2005-2017
cor(x, y, method = c("pearson", "kendall", "spearman"))
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

Correlation_IncProg_vs_TAE <- df_progress_vs_spend %>%
  cor(absolute_reduction, Avg_05_17, method = c("pearson"), use = "complete.obs")

mean_GAE_share_by_year <- df_small %>%
  group_by(year) %>%
  summarize(mean_GAE_share = mean(GAE_share, na.rm=TRUE))


TAE_by_year <- df_small %>%
  group_by(year) %>%
  summarize(sum_TAE = sum(the_total_mean, na.rm=TRUE)/1000000)

GAE_by_year <- df_small %>%
  group_by(year) %>%
  summarize(sum_GAE = sum(ghes_total_mean, na.rm=TRUE)/1000000)

DAH_by_year <- df_small %>%
  group_by(year) %>%
  summarize(sum_DAH = sum(dah_total, na.rm=TRUE)/1000000)

df_other <- rename(df_other, iso3 = ISO)
leftJoinDf <- left_join(df_other, df_small, by= "iso3")

#Transform Data

lnTHE <- log(df_small$the_total_mean + 1)



mean_GAE_share_by_year_joined_LMI <- leftJoinDf %>%
  group_by(year) %>%
  filter(WB.Status.2018 == "LMI") %>%
  summarize(mean_GAE_share = mean(GAE_share, na.rm=TRUE))

mean_GAE_share_by_year_joined <- rename(mean_GAE_share_by_year_joined, meanGAEshareJoined = mean_GAE_share)
mean_GAE_share_by_year_joined_LI <- rename(mean_GAE_share_by_year_joined_LI, meanGAEshareLI = mean_GAE_share)
mean_GAE_share_by_year_joined_LMI <- rename(mean_GAE_share_by_year_joined_LMI, meanGAEshareLMI = mean_GAE_share)
mean_GAE_share_by_year_joined_UMI <- rename(mean_GAE_share_by_year_joined_UMI, meanGAEshareUMI = mean_GAE_share)

meanGAEout <- left_join(meanGAEout,mean_GAE_share_by_year_joined_LMI,by = "year")
meanGAEout <- left_join(meanGAEout,mean_GAE_share_by_year_joined_UMI,by = "year")
meanGAEout <- left_join(meanGAEout,mean_GAE_share_by_year,by = "year")




mean_GAE_share_by_year_joined_LI <- leftJoinDf %>%
  group_by(year) %>%
  filter(WB.Status.2018 == "LI") %>%
  summarize(mean_GAE_share = mean(GAE_share, na.rm=TRUE))

mean_GAE_share_by_year_joined_UMI <- leftJoinDf %>%
  group_by(year) %>%
  filter(WB.Status.2018 == "UMI") %>%
  summarize(mean_GAE_share = mean(GAE_share, na.rm=TRUE))





# Regression Model

  model <- lm(df_aidsinfovsihme_clean$TAEpcase_IHME ~ df_aidsinfovsihme_clean$GAEShare_ihme + df_aidsinfovsihme_clean$Year + df_aidsinfovsihme_clean$World.Bank.Status + df_aidsinfovsihme_clean$PWH )
summary(model)


model <- lm(df_aidsinfovsihme_clean$ProgressARTCov1019 ~ df_aidsinfovsihme_clean$TAEpcase_IHME + df_aidsinfovsihme_clean$GAEShare_ihme + df_aidsinfovsihme_clean$PWH + df_aidsinfovsihme_clean$World.Bank.Status)
summary(model)

model <- lm(df_aidsinfovsihme_clean$ProgressARTCov1019 ~ df_aidsinfovsihme_clean$Progress_Inc_Redux_NumCases)
              summary(model)
# Load data
df_aidsinfovsihme <- read.csv('data/IHMEvAIDSINFO.csv')
df_aidsinfovsihme_clean <- df_aidsinfovsihme %>%
  filter(DropMissError == 0)

# Plot Progress Dimensions
df_aidsinfovsihme_clean %>%
  ggplot(aes(x=ProgressIncDECLINE, y=ProgressARTCov1019)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm )  # Add linear regression line 

# plot TAE ihme vs aidsinfo
df_aidsinfovsihme_clean %>%
  ggplot(aes(x=TAE, y=TAE_IHME)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm ) +  # Add linear regression line 
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') + 
  annotation_logticks() 

# plot TAEpcase ihme vs. aidsinfo
df_aidsinfovsihme_clean %>%
  ggplot(aes(x=TAEpcase, y=TAEpcase_IHME)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm ) +  # Add linear regression line 
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') + 
  annotation_logticks() 

 
  
ggplot(df_small, aes(x = GAE_share, color = year)) + geom_histogram(color="darkblue", fill="lightblue")

ggplot(data=mean_GAE_share_by_year, aes(x=year, y=mean_GAE_share, group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Government share of Total AIDS Spending \n by year") +
  xlab("Year") + 
  ylab("GAE Share") +
  coord_cartesian(ylim = c(0.3, 0.7))+
  scale_x_continuous(breaks=2000:2017) +
  theme_bw() +
  theme(text = element_text(size = 18)) 


write.csv(df_progress_vs_spend,"df_progress_vs_spend.csv", row.names = FALSE)
write.csv(meanGAEout,"meanGAEout.csv", row.names = FALSE)

write.csv(TAE_by_year,"TAEout.csv", row.names = FALSE)
write.csv(GAE_by_year,"GAEout.csv", row.names = FALSE)
write.csv(DAH_by_year,"DAHout.csv", row.names = FALSE)
write.csv(ESF_Analysis_March27,"data/IHMEvAIDSINFO.csv", row.names = FALSE)