rm(list = ls())
demo = T
require(psych)
require(data.table)
require(dplyr)
require(ggplot2)
if(demo) {setwd("~/Library/Mobile Documents/com~apple~CloudDocs/School Work/Sem 2/BUAN 6337/HW/pset2")}

# Section - 1

ertqks <- fread("earthquakes.csv",header = T)[State %in%  c("Alaska","California"),]
if(demo) {str(ertqks)
  summary(ertqks)
}

summary_ertqks <- ertqks[,':='(mean=mean(Magnitude), median = median(Magnitude),std = sd(Magnitude),min = min(Magnitude),max=max(Magnitude),percentile.25 = quantile(Magnitude,0.25),percentile.75 = quantile(Magnitude,0.75)),by=c("Year","State")]

if(demo) {summary(summary_ertqks)}

summary_ertqks_flt <- summary_ertqks[Year>=2002 & Year<=2011,]
summary_ertqks_flt <- summary_ertqks_flt[order(Year,State)]

if(demo) {str(summary_ertqks_flt)
  summary(summary_ertqks_flt)}

year <- unique(summary_ertqks_flt$Year)

for(i in year) {
  assign(paste0("ertqk",i),as.data.table(summary_ertqks_flt[Year ==i,]))
  str(get(paste0("ertqk",i)))
}

ertqk_summary <- dcast(summary_ertqks_flt,Year ~ State,fun = mean,value.var = c("mean", "median", "std", "min", "max","percentile.25","percentile.75"))
if(demo) {str(ertqk_summary)}

summary_ertqks_flt %>%
  ggplot(aes(Year,mean))+
  geom_line(aes(colour = summary_ertqks_flt$State))+ labs(x = "Year" , y ="Average Mag." )+ facet_wrap(~summary_ertqks_flt$State, nrow = 1)+ theme(legend.position = "none")+
  ggtitle("Trend of Average Magnitude")

boxplot(summary_ertqks_flt$Magnitude ~ summary_ertqks_flt$State)

t.test(summary_ertqks_flt$Magnitude ~ summary_ertqks_flt$State)

# Section - 2

gpa <- fread("study_gpa.csv",header = T)

if(demo) {str(gpa)}

hist_plot <- ggplot(gpa, aes(AveTime)) +
  geom_histogram(aes(y=..density..), binwidth = 5) + # scale histogram y
  geom_density(col = "red")
print(hist_plot + labs(x="Average Time",y = "Frequency"))

summary(gpa$AveTime)

var = names(gpa)[5:7]

forms <- lapply(1:length(var),function(i) formula(paste(var[i], "~", paste(var[-i], collapse = "+"))))

models <- lapply(forms,aov,data = gpa[Section == 2])

for(i in 1:length(forms)) {
  print(paste("Model:",forms[i]))
  print(summary(models[[i]]))
}

# Section - 3

vite <- fread("vite.csv",header = T,na.strings = c("NA",""),sep = "auto",stringsAsFactors = F)

if(demo) {str(vite)
  summary(vite)}

vite_fnl <- dcast(vite,formula = ID+Treatment~Visit,value.var = "Plaque")

colNames <- c("baseline","first_year","second_year")
colnames(vite_fnl)[3:5] <- colNames

##vite_0 <- vite_fnl[vite_fnl$Treatment == 0]

if(demo) {str(vite_fnl)}

t.test(vite_fnl$baseline[vite_fnl$Treatment == 1],vite_fnl$second_year[vite_fnl$Treatment == 1],paired = T)

vite_fnl$diff <-vite_fnl$second_year - vite_fnl$baseline
t.test(vite_fnl$diff[vite_fnl$Treatment == 1],vite_fnl$diff[vite_fnl$Treatment == 0], paired = T)


t.test(data = vite, Alcohol~Treatment)

t.test(data = vite, Smoke~Treatment)



