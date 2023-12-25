# Importing libraries
library(tidyverse)
library(stargazer)

# Importing Data
### Polarization Data
# Code to get annual polarization data, provided by voteview.com
polarization <- read_csv("https://voteview.com/static/data/out/members/HSall_members.csv")

south <- c(40:49,51,53)
polar_dat <- polarization %>% 
  filter(congress>45 & 
           chamber != "President") %>%
  mutate( 
    year = 2*(congress-1) + 1789,
  ) %>%
  group_by(chamber,congress,year) %>% 
  summarize(
    party.mean.diff.d1 = mean(nominate_dim1[party_code==200],na.rm=T) - 
      mean(nominate_dim1[party_code==100],na.rm=T),
    prop.moderate.d1 = mean(abs(nominate_dim1)<0.25,na.rm=T),
    prop.moderate.dem.d1 = mean(abs(nominate_dim1[party_code==100])<0.25,na.rm=T),
    prop.moderate.rep.d1 = mean(abs(nominate_dim1[party_code==200])<0.25,na.rm=T),
    overlap = (sum(nominate_dim1[party_code==200] <
                     max(nominate_dim1[party_code==100],na.rm=T),na.rm=T)  +
                 sum(nominate_dim1[party_code==100] >
                       min(nominate_dim1[party_code==200],na.rm=T),na.rm=T))/
      (sum(!is.na(nominate_dim1[party_code==100]))+
         sum(!is.na(nominate_dim1[party_code==200]))),
    chamber.mean.d1 = mean(nominate_dim1,na.rm=T),
    chamber.mean.d2 = mean(nominate_dim2,na.rm=T),
    dem.mean.d1 = mean(nominate_dim1[party_code==100],na.rm=T),
    dem.mean.d2 = mean(nominate_dim2[party_code==100],na.rm=T),
    rep.mean.d1 = mean(nominate_dim1[party_code==200],na.rm=T),
    rep.mean.d2 = mean(nominate_dim2[party_code==200],na.rm=T),
    north.rep.mean.d1 = mean(nominate_dim1[party_code==200 & 
                                             !(state_icpsr %in% south)],na.rm=T),    
    north.rep.mean.d2 = mean(nominate_dim2[party_code==200 & 
                                             !(state_icpsr %in% south)],na.rm=T),    
    south.rep.mean.d1 = mean(nominate_dim1[party_code==200 & 
                                             (state_icpsr %in% south)],na.rm=T),    
    south.rep.mean.d2 = mean(nominate_dim2[party_code==200 & 
                                             (state_icpsr %in% south)],na.rm=T),    
    north.dem.mean.d1 = mean(nominate_dim1[party_code==100 & 
                                             !(state_icpsr %in% south)],na.rm=T),    
    north.dem.mean.d2 = mean(nominate_dim2[party_code==100 & 
                                             !(state_icpsr %in% south)],na.rm=T),    
    south.dem.mean.d1 = mean(nominate_dim1[party_code==100 & 
                                             (state_icpsr %in% south)],na.rm=T),    
    south.dem.mean.d2 = mean(nominate_dim2[party_code==100 & 
                                             (state_icpsr %in% south)],na.rm=T),    
  ) 

colnames(polar_dat)[2] = "Congress"

### Congressional Work Data
# Data for bills/workload of Congressional houses
housework <- read_csv("./data/house_work.csv")
senwork <- read_csv("./data/senate_work.csv")

# Manually calculating ratio for missing value
senwork[36, "BillsPassedtoBillsIntroduced"] <- round(senwork[36, "BillsPassed"] / senwork[36, "BillsIntroduced"], 3)

### Variable Types
## Data types for initial datasets
# Polarization Data
str(polarization)
# House of Rep. Work Data
str(housework)
# Senate Work Data
str(senwork)

### Merging Data
## Merging polarization data with Congressional work data
# House
polar_house <- polar_dat |>
  filter(chamber == "House") |>
  inner_join(housework, by = "Congress")

# Senate
polar_sen <- polar_dat |>
  filter(chamber == "Senate") |>
  inner_join(senwork, by = "Congress")

### Model/Analysis
## Determined by $\frac{BillsPassedtoBillsIntroduced}{DaysInSession}=party.mean.diff.d1X+b$.
## Regression Model for House Efficiency
# PLOT of regression model
ggplot(polar_house, aes(x=party.mean.diff.d1,
                        y=log(BillsPassedtoBillsIntroduced/DaysInSession))) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Level of Polarity vs Ratio of Bills Passed (House of Representatives)",
       x="Level of Party Polarity",
       y="log(Ratio of Bills Passed to Introduced / Days in Session)") +
  theme_classic()

# Base regression model
house_model <- lm(log(BillsPassedtoBillsIntroduced/DaysInSession) ~ 
                    party.mean.diff.d1, 
                  data = polar_house)
# Regression model with control
house_model_cf <- lm(log(BillsPassedtoBillsIntroduced/DaysInSession) ~ 
                       party.mean.diff.d1+AvgBillsIntroducedPerMember, 
                     data = polar_house)

## Regression Model for Senate Efficiency
# PLOT of regression model
ggplot(polar_sen, aes(x=party.mean.diff.d1,
                      y=log(BillsPassedtoBillsIntroduced/DaysInSession))) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Level of Polarity vs Ratio of Bills Passed (Senate)",
       x="Level of Party Polarity",
       y="log(Ratio of Bills Passed to Introduced / Days in Session)") +
  theme_classic()

# Base regression model
sen_model <- lm(log(BillsPassedtoBillsIntroduced/DaysInSession) ~ 
                  party.mean.diff.d1, 
                data = polar_sen)
# Regression model with control
sen_model_cf <- lm(log(BillsPassedtoBillsIntroduced/DaysInSession) ~ 
                     party.mean.diff.d1+AvgBillsIntroducedPerMember, 
                   data = polar_sen)


#### Other Visualizations
# Combining house/senate data for visualization
df <- rbind(polar_house, polar_sen)

### PLOT: Trend of polarization levels over time
ggplot(df, aes(x=year, y=party.mean.diff.d1, color=chamber)) +
  geom_line() +
  labs(title="Levels of Polarization in Congress over Time",
       subtitle="Dimensions for Chamber in Congress",
       x="Year",
       y="Level of Polarization",
       color="Chamber of Congress") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90),
        legend.position="top",
        legend.justification="left")

# Variables of interest
house_var <- polar_house[, c("Congress","year","party.mean.diff.d1",
                             "BillsIntroduced","AvgBillsIntroducedPerMember",
                             "BillsPassed","BillsPassedtoBillsIntroduced",
                             "RecordedVotes","DaysInSession")]

### HTML table for summary statistics (cleaner)
stargazer(data.frame(house_var), type = "html", 
          title="Descriptive Statistics (House of Rep.)", digits=3, 
          out="visualizations/house_ss.html",
          covariate.labels=c("Session","Year","Polarization Level", 
                             "Bills Introduced",
                             "Avg Bills Introduced/Member","Bills Passed",
                             "Ratio of Bills Passed to Introduced",
                             "Recorded Votes","Days In Session"))

# Variables of Interest
sen_var <- polar_sen[, c("Congress","year","party.mean.diff.d1",
                         "BillsIntroduced","AvgBillsIntroducedPerMember", 
                         "BillsPassed","BillsPassedtoBillsIntroduced",
                         "RecordedVotes","DaysInSession")]

### HTML table for summary statistics (cleaner)
stargazer(data.frame(sen_var), type = "html", 
          title="Descriptive Statistics (Senate)", digits=3, 
          out="visualizations/sen_ss.html",
          covariate.labels=c("Session","Year","Polarization Level", 
                             "Bills Introduced",
                             "Average Bills Introduced/Member","Bills Passed", 
                             "Ratio of Bills Passed to Introduced",
                             "Recorded Votes", "Days In Session"))

### Regression Output Table for House Polarization (cleaner)
stargazer(house_model, house_model_cf, type="html", 
          title="Polarization on Productivity (House of Representatives)", 
          out="visualizations/house_reg.html", 
          covariate.labels=c("Level of Polarization"),
          dep.var.labels=c("log(Ratio of Bill Introduced to Bills Passed / Total Days in Session)"),
          column.labels=c("Without Control", "With Control"))

### Regression Output Table for Senate Polarization (cleaner)
stargazer(sen_model, sen_model_cf, type="html", 
          title="Polarization on Productivity (Senate)", 
          out="visualizations/senate_reg.html", 
          covariate.labels=c("Level of Polarization"),
          dep.var.labels=c("log(Ratio of Bill Introduced to Bills Passed / Total Days in Session)"),
          column.labels=c("Without Control", "With Control"))
