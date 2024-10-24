##################################################################################
## Visualizations for Face Fit Paper 1
## Created by Melissa McInroe
## Created September 15, 2023
## Only plots that are not commented out are the ones used for the figures
##################################################################################
## Load Data and Packages
#---------------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(nlme)
library(tidyr)
library(egg)
library(cowplot)
library(ggpattern)

setwd("")

demographics<-fread("R_scripts_input/demographics.csv")
names(demographics)
# [1] "Study #"        "Date"           "Age"            "Sex"            "Race/Ethnicity" "Height (cm)"    "Weight (kg)"    "BMI"           
# [9] "subject"  
demographics$subject<-str_sub(demographics$`Study #`, -3,-1)

measurements<-fread("PythonOutput/total_table_scaled_8-14-2023.csv")
names(measurements)
# [1] "subject"               "Condition"             "Bending Mean"          "Bending SD"            "Reading Mean"          "Reading SD"           
# [7] "LR Mean"               "LR SD"                 "UD Mean"               "UD SD"                 "Overall Mean"          "Overall SD"           
# [13] "Bitragion Chin Arc"    "Bitragion Coronal Arc" "Bitragion Frontal Arc" "Head Circumference"    "Neck Circumference"    "Ear Breadth"          
# [19] "Ear Length"            "Lip Length"            "Upper Facial Breadth"  "Menton-Sellion Length" "Nose Breadth"          "Nose Length"          
# [25] "Bigonial Breadth"      "Bizygomatic Breadth"   "Head Breadth"          "Head Length"           "nose_gap_area"         "nose_gap_curve_length"
# [31] "Bizyg_1_11"            "MenSel_6_89"           "EarLength_2_9"         "LipLength_59_65"       "sel-earb"              "nose-earb"            
# [37] "chin-earb"             "lip-earb"              "sphenomaxillary_angle" "ratio_sel-nose_tip"    "ratio_sel-lip"         "ratio_sel-chin"  
measurements$subject<-str_pad(measurements$subject,3,pad="0")

FFE<-fread("R_scripts_input/FFE.csv")

FFE$KN95_diff<-FFE$KN95_clip-FFE$KN95
FFE$Surgical_diff<-FFE$Surgical_clip-FFE$Surgical
FFE$KF94_diff<-FFE$KF94_clip-FFE$KF94
FFE$MKF94_diff<-FFE$MKF94_clip-FFE$MKF94

full_stats<-left_join(measurements,demographics, by="subject")
colnames(full_stats)[11]<-"overall_mean"

#### Difference data set
## Includes subject, sex, condition (mask), diff (FFE with clip - baseline FFE)
## pivoted long to create plots
FFE_diff<-FFE[,c(1,3,15:17)]
FFE_diff<-pivot_longer(FFE_diff,cols=3:5, names_to="Condition", values_to="Difference")
FFE_diff$Condition<-gsub("_diff","",FFE_diff$Condition)
FFE_diff<-as.data.frame(FFE_diff)
FFE_diff$Condition<-factor(FFE_diff$Condition, levels=c("KN95", "Surgical", "KF94"))

#### FFE data set pivoted long to create plots
## includes subject, sex, age, BMI, condition and overall FFE
FFE_long<-pivot_longer(FFE,cols=c(6,7,9,11), names_to="Condition", values_to="Overall_FFE")
FFE_long<-FFE_long[,c(1,3,4,5,15,16)]

FFE_long$Condition<-factor(FFE_long$Condition, levels=c("N95", "KN95", "Surgical", "KF94"))

## Add Percent Change variable for % change of FFE with clip
FFE_long_diff<-left_join(FFE_diff,FFE_long, by=c("Subject", "Sex", "Condition"))
FFE_long_diff$per_change<-FFE_long_diff$Difference/FFE_long_diff$Overall_FFE*100

FFE_long_diff$Condition<-factor(FFE_long_diff$Condition, levels=c("KN95", "Surgical", "KF94"))
names(FFE_long_diff)
## "Subject"     "Sex"         "Condition"   "Difference"  "Age"         "BMI"         "Overall_FFE" "per_change" 

## Subset (Before/After?Difference)
## Does not include N95
FFE_base<-FFE[,c(1,3,7,9,11)]
FFE_base<-pivot_longer(FFE_base,cols=3:5, names_to="Condition", values_to="FFE")
FFE_base$Condition<-factor(FFE_base$Condition, levels=c("KN95", "Surgical", "KF94"))

## Clip data set
## Includes factor variable to show baseline FFE and FFE with clip
FFE_clip<-FFE[,c(1,3,8,10,12)]
colnames(FFE_clip)[3:5]<-c("KN95","Surgical","KF94")
FFE_clip<-pivot_longer(FFE_clip,cols=3:5, names_to="Condition", values_to="FFE_clip")
FFE_clip$Condition<-factor(FFE_clip$Condition, levels=c("KN95", "Surgical", "KF94"))
FFE_change<-left_join(FFE_base, FFE_clip, by=c("Subject","Sex","Condition"))
FFE_change_long<-pivot_longer(FFE_change, cols = c(4,5), names_to="Clip", values_to="FFE")
names(FFE_change_long)
## "Subject"   "Sex"       "Condition" "Clip"      "FFE"   

full_stat_wide <- pivot_wider(full_stats[,c(1,2,11,46)], names_from="Condition", values_from = overall_mean)


################################## Scatterplots ###################################
## Mask FFE baseline~Mask FFE with clip by Sex
## Figure 3B
## contour/density plots with regression lines. 
## Plots also include a line for x = y to show the null condition (no change)
## Figures 3B
###################################################################################
# ggplot(FFE, aes(x=KN95, y=KN95_clip, color = Sex)) +
#   geom_point()+
#   geom_abline(linetype="dashed", linewidth=1) +
#   geom_smooth(method="lm",se=FALSE) +
#   theme(panel.grid=element_blank(),
#         plot.title = element_text(hjust=0.5))+
#         #panel.background = element_blank(),
#         #axis.line = element_line(colour = "black"))+
#   labs(x="FFE Without Clip",y="FFE With Clip", title="KN95") +
#   xlim(20,100) +
#   ylim(20,100)
#   
# ggplot(FFE, aes(x=Surgical ,y=Surgical_clip, color = Sex)) +
#   geom_point()+
#   geom_abline(linetype="dashed", linewidth=1) +
#   geom_smooth(method="lm",se=FALSE) +
#   theme(panel.grid=element_blank(),
#         plot.title = element_text(hjust=0.5))+
#   #panel.background = element_blank(),
#   #axis.line = element_line(colour = "black"))+
#   labs(x="FFE Without Clip",y="FFE With Clip", title="Surgical") +
#   xlim(20,100) +
#   ylim(20,100)
# 
# ggplot(FFE, aes(x=KF94 ,y=KF94_clip, color = Sex)) +
#   geom_point()+
#   geom_abline(linetype="dashed", linewidth=1) +
#   geom_smooth(method="lm",se=FALSE) +
#   theme(panel.grid=element_blank(),
#         plot.title = element_text(hjust=0.5))+
#   #panel.background = element_blank(),
#   #axis.line = element_line(colour = "black"))+
#   labs(x="FFE Without Clip",y="FFE With Clip", title="Large KF94") +
#   xlim(20,100) +
#   ylim(20,100)
# 
# ggplot(FFE, aes(x=MKF94 ,y=MKF94_clip, color = Sex)) +
#   geom_point()+
#   geom_abline(linetype="dashed", linewidth=1) +
#   geom_smooth(method="lm",se=FALSE) +
#   theme(panel.grid=element_blank(),
#         plot.title = element_text(hjust=0.5))+
#   #panel.background = element_blank(),
#   #axis.line = element_line(colour = "black"))+
#   labs(x="FFE Without Clip",y="FFE With Clip", title="Medium KF94") +
#   xlim(20,100) +
#   ylim(20,100)


# den_cloud<-ggplot(FFE, aes(x=KN95, y=KN95_clip, color = Sex, fill = Sex)) +
#   stat_density_2d(geom = "polygon", alpha = .15, contour = T) +
#   labs(x="FFE Without Clip",y="FFE With Clip", title="KN95") +
#   scale_x_continuous(expand =c(0,0), limits=c(20,100)) +
#   scale_y_continuous(expand = c(0,0), limits= c(20,100)) +
#   geom_abline(linetype="dashed", linewidth=1) +
#   theme(legend.title = element_blank(),
#         legend.position = c(.8,.2),
#         plot.title = element_text(hjust=0.5)) +
#   scale_fill_discrete(labels = c("Female","Male"))+
#   guides(color = "none")
# den_legend<-get_legend(den_cloud)
# den_legend<-ggdraw() + draw_grob(den_legend)
# den_cloud +theme(legend.position="none")
#------------------------------------------------------------------------------------


## Density Clouds instead of Scatterplots
## KN94
ggplot(FFE, aes(x=KN95, y=KN95_clip, color = Sex, fill = Sex)) +
  stat_density_2d(geom = "polygon", alpha = .05, contour = T) +
  labs(x="Baseline FFE (%) No Clip",y="FFE (%) With Clip", title="") +
  scale_x_continuous(expand =c(0,0), limits=c(15,105), breaks = c(20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(expand = c(0,0), limits= c(15,105),breaks = c(20,30,40,50,60,70,80,90,100)) +
  geom_abline(linetype="dashed", linewidth=1) +
  geom_vline(xintercept = 80, linetype = 3) +
  geom_hline(yintercept = 80, linetype = 3) +
  #geom_smooth(method="lm",se=FALSE, linewidth=1.5, color = "black") +
  geom_smooth(method="lm",se=FALSE, linewidth=2) +
  annotate(geom="text", x=60, y=30, label = "KN95", color = "black", size =8)+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour="black"),
        axis.text = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"))

## Surgical
ggplot(FFE, aes(x=Surgical, y=Surgical_clip, color = Sex, fill = Sex)) +
  stat_density_2d(geom = "polygon", alpha = .05, contour = T) +               
  labs(x="Baseline FFE (%) No Clip",y="FFE (%) With Clip", title="") +
  scale_x_continuous(expand =c(0,0), limits=c(15,105), breaks = c(20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(expand = c(0,0), limits= c(15,105),breaks = c(20,30,40,50,60,70,80,90,100)) +
  geom_abline(linetype="dashed", linewidth=1) +
  geom_vline(xintercept = 80, linetype = 3) +
  geom_hline(yintercept = 80, linetype = 3) +
  #geom_smooth(method="lm",se=FALSE, linewidth=1.5, color = "black") +    #add black outline, change other geom_smooth linewidth to 1
  geom_smooth(method="lm",se=FALSE, linewidth=2) +
  annotate(geom="text", x=60, y=30, label = "Surgical", color = "black", size =8)+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour="black"),
        axis.text = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"))

## KF94
ggplot(FFE, aes(x=KF94, y=KF94_clip, color = Sex, fill = Sex)) +
  stat_density_2d(geom = "polygon", alpha = .05, contour = T) +
  labs(x="Baseline FFE (%) No Clip",y="FFE (%) With Clip", title="") +
  scale_x_continuous(expand =c(0,0), limits=c(15,105), breaks = c(20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(expand = c(0,0), limits= c(15,105),breaks = c(20,30,40,50,60,70,80,90,100)) +
  geom_abline(linetype="dashed", linewidth=1) +
  geom_vline(xintercept = 80, linetype = 3) +
  geom_hline(yintercept = 80, linetype = 3) +
  #geom_smooth(method="lm",se=FALSE, linewidth=1.5, color = "black") +
  geom_smooth(method="lm",se=FALSE, linewidth=2) +
  annotate(geom="text", x=60, y=30, label = "KF94", color = "black", size =8)+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour="black"),
        axis.text = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"))

## MKF94
# ggplot(FFE, aes(x=MKF94, y=MKF94_clip, color = Sex, fill = Sex)) +
#   stat_density_2d(geom = "polygon", alpha = .15, contour = T) +
#   labs(x="Baseline FFE (%) No Clip",y="FFE With Clip", title="") +
#   scale_x_continuous(expand =c(0,0), limits=c(15,105), breaks = c(20,30,40,50,60,70,80,90,100)) +
#   scale_y_continuous(expand = c(0,0), limits= c(15,105),breaks = c(20,30,40,50,60,70,80,90,100)) +
#   geom_abline(linetype="dashed", linewidth=1) +
#   geom_vline(xintercept = 80, linetype = 3) +
#   geom_hline(yintercept = 80, linetype = 3) +
#   geom_smooth(method="lm",se=FALSE, linewidth=2, color = "black") +
#   geom_smooth(method="lm",se=FALSE, linewidth=1) +
#   annotate(geom="text", x=60, y=30, label = "KF94 (M)", color = "black", size =8, face = "bold")+
#   theme(legend.position = "none",
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(colour="black"),
#         axis.text = element_text(size = 14))




############################# Improvement Plot ################################


### Find extremes to trace across all masks
max(FFE_diff$Difference, na.rm=TRUE)            #52.5
min(FFE_diff$Difference,na.rm=TRUE)             #-22.5
# FFE_diff$Subject[which(FFE_diff$Difference==-22.5)]           #FIT060   (M)
# FFE_diff$Subject[which(FFE_diff$Difference==52.5)]            #FIT055   (F)
# min(filter(FFE_diff, Condition == "KF94")$Difference, na.rm=TRUE)  #-15.9
# FFE_diff$Subject[which(FFE_diff$Difference==-15.9)]           #FIT059    (M)
# min(filter(FFE_diff, Condition == "Surgical")$Difference, na.rm=TRUE)  #-13
# FFE_diff$Subject[which(FFE_diff$Difference==-15.9)]           #FIT091    (M)
# max(filter(FFE_diff, Condition == "Surgical")$Difference, na.rm=TRUE)  #31.5
# FFE_diff$Subject[which(FFE_diff$Difference==31.5)]            #FIT063  (F)
# max(filter(FFE_diff, Condition == "KN95")$Difference, na.rm=TRUE)  # 43.2
# FFE_diff$Subject[which(FFE_diff$Difference==43.2)]            #FIT037   (F)


# subjects_m<-unique(FFE_diff$Subject[FFE_diff$Sex=="M"])
# subjects_f<-unique(FFE_diff$Subject[FFE_diff$Sex=="F"])
# 
# ## Subset Data Randomly, but include the min and max values
# set.seed(104)
# set_female<-sample(subjects_f, size=2)
# set_female<-c(set_female,"FIT055", "FIT037","FIT063")
# set_male<-sample(subjects_m, size=2)
# set_male<-c(set_male,"FIT060", "FIT059", "FIT091")
# ffe_subset<-c(set_male, set_female)
# 
# FFE_subset<-FFE_diff[is.element(FFE_diff$Subject, ffe_subset),]
# 
# 
# ggplot(FFE_subset, aes(x=Condition, y=Difference, color = Sex, shape = Subject)) +
#   geom_point(size = 2, stroke=1) +
#   geom_hline(yintercept = 0) +
#   labs(x="", y="\u2206 FFE (With Clip - Without Clip)" )+
#   scale_shape_manual(values=1:10)

### All differences
# ggplot(FFE_diff, aes(x=Condition, y=Difference, color = Sex)) +
#   geom_point() +
#   geom_hline(yintercept = 0) +
#   labs(x="", y="\u2206 FFE (With Clip - Without Clip)" )


############################ Figure 1 boxplots  ###################################

# plot1<-ggplot(FFE_long, aes(x=Condition, y=Overall_FFE, color = Sex)) +
#   geom_point() +
#   geom_boxplot(color = "black",alpha = 0, width = 0.3) +
#   labs(x="", y="Overall FFE (%)") +
#   scale_color_discrete(labels=c("Female","Male")) +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         legend.position = "none",                            #c(.15,.15), 
#         #legend.title = element_blank(),
#         #legend.direction = "horizontal",
#         #legend.background = element_blank(),
#         #legend.box.background = element_rect(colour = "black"),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.length.x = unit(0,"cm"),
#         plot.margin = unit(c(0.5,1,0,0.5),"cm"),
#         plot.title = element_text(hjust=0.5),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(colour="black"),
#         axis.line.x.bottom = element_line(color="black")) +
#   guides(colour = guide_legend(override.aes=list(size = 3))) +
#   scale_y_continuous(limits = c(15,105), breaks =c(20,30,40,50,60,70,80,90,100))
# 
# plot2<-ggplot(FFE_long, aes(x=Condition, y=Overall_FFE, fill = Sex)) +
#   #geom_point(color = "navyblue") +
#   geom_boxplot() +
#   labs(x="", y="Overall FFE (%)") +
#   # scale_fill_discrete(labels=c("Female","Male")) +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.x = element_text(size=12, face = "bold"),
#         legend.position = "none",
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(colour="black"),
#         plot.margin = unit(c(0,0,.5,.5),"cm")) +
#   scale_y_continuous(limits = c(15,105), breaks =c(20,30,40,50,60,70,80,90,100))
# 
# egg::ggarrange(plot1,plot2,nrow=2) #, labels=c("A","B"), label.args = list(gp=grid::gpar(font=4),vjust=15,hjust=-7))

# ### Confirm Outliers
# median_n95_m<-median(FFE_long$Overall_FFE[which(FFE_long$Condition =="N95" & FFE_long$Sex == "M")])
# std_n95_m<-sd(FFE_long$Overall_FFE[which(FFE_long$Condition =="N95" & FFE_long$Sex == "M")])
# Tmin_n95_m<-median_n95_m-(3*std_n95_m)
# N95_subset_m<-subset(FFE_long, Condition =="N95")
# N95_subset_m<-subset(N95_subset_m, Overall_FFE<Tmin_n95_m)
# 
# #N95 Males
# iqr_n95_m<-IQR(FFE_long$Overall_FFE[which(FFE_long$Condition =="N95" & FFE_long$Sex == "M")])
# q1_n95_m<-as.numeric(quantile(FFE_long$Overall_FFE[which(FFE_long$Condition =="N95" & FFE_long$Sex == "M")], .25))
# lower_bound_n95_m<-q1_n95_m-1.5*iqr_n95_m
# N95_subset_m<-subset(FFE_long, Condition =="N95" & Sex == "M")
# N95_subset_m<-subset(N95_subset_m, Overall_FFE<lower_bound_n95_m)
# 
# #N95 Females
# iqr_n95_f<-IQR(FFE_long$Overall_FFE[which(FFE_long$Condition =="N95" & FFE_long$Sex == "F")])
# q1_n95_f<-as.numeric(quantile(FFE_long$Overall_FFE[which(FFE_long$Condition =="N95" & FFE_long$Sex == "F")], .25))
# lower_bound_n95_f<-q1_n95_f-1.5*iqr_n95_f
# N95_subset_f<-subset(FFE_long, Condition =="N95" & Sex == "F")
# N95_subset_f<-subset(N95_subset_f, Overall_FFE<lower_bound_n95_f)
# 
# #KF94 Females
# iqr_kf94_f<-IQR(FFE_long$Overall_FFE[which(FFE_long$Condition =="KF94" & FFE_long$Sex == "F")])
# q3_kf94_f<-as.numeric(quantile(FFE_long$Overall_FFE[which(FFE_long$Condition =="KF94" & FFE_long$Sex == "F")], .75))
# upper_bound_kf94_f<-q3_kf94_f+1.5*iqr_kf94_f
# kf94_subset_f<-subset(FFE_long, Condition =="KF94" & Sex == "F")
# kf94_subset_f<-subset(kf94_subset_f, Overall_FFE>upper_bound_kf94_f)

### Outliers seem correct if calculated using Q1-1.5*IQR and Q3+1.5*IQR

##########################################################################
## Explainer Graphs 
## Show improve with clip, worse with clip
## Show broadd distribution vs. tight distribution for clouds
##########################################################################

# x<-sample(FFE_long$Overall_FFE, 1000, replace = TRUE)
# y<-sample(FFE_long$Overall_FFE, 1000, replace = TRUE)
# df_samp<-data.frame(cbind(x,y))
# 
# ggplot(df_samp, aes(x=x, y=y)) +
#   stat_density_2d(geom = "polygon", alpha = .15, contour = T) +
#   labs(x="",y="", title="Sample Data") +
#   scale_x_continuous(expand =c(0,0), limits=c(15,105), breaks = c(20,30,40,50,60,70,80,90,100)) +
#   scale_y_continuous(expand = c(0,0), limits= c(15,105),breaks = c(20,30,40,50,60,70,80,90,100)) +
#   geom_abline(linetype="dashed", linewidth=1) +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust=0.5),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(colour="black"))
# 
# x<-sample(FFE$Surgical, 1000, replace = TRUE)
# y<-sample(FFE$Surgical, 1000, replace = TRUE)
# df_samp<-data.frame(cbind(x,y))
# 
# ggplot(df_samp, aes(x=x, y=y)) +
#   stat_density_2d(geom = "polygon", alpha = .15, contour = T) +
#   labs(x="Without Clip",y="With Clip", title="Simulated Data") +
#   scale_x_continuous(expand =c(0,0), limits=c(15,105), breaks = c(20,30,40,50,60,70,80,90,100)) +
#   scale_y_continuous(expand = c(0,0), limits= c(15,105),breaks = c(20,30,40,50,60,70,80,90,100)) +
#   geom_abline(linetype="dashed", linewidth=1) +
#   annotate(geom="text", x=40, y=90, label = "Clip Improved FFE", color = "navyblue")+
#   annotate(geom="text", x=80, y=30, label = "Clip Decreased FFE", color = "red4") +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust=0.5),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(colour="black"))
# 
# 
# x<-rnorm(1000, mean = 40, sd = 8)
# y<-rnorm(1000, mean = 40, sd = 8)
# x_alt<-rnorm(1000, mean = 80, sd = 4)
# y_alt<-rnorm(1000, mean = 80, sd = 4)
# df_samp<-data.frame(cbind(x,y,x_alt, y_alt))
# 
# arrows<-tibble(
#   x1=c(60,60),
#   x2=c(40,80),
#   y1=c(60,60),
#   y2=c(80,40)
# )
# 
# ggplot(df_samp, aes(x=x, y=y)) +
#   stat_density_2d(geom = "polygon", alpha = .15, contour = T) +
#   stat_density_2d(data = df_samp, mapping=aes(x=x_alt, y=y_alt), geom = "polygon", alpha = 0.15, contour = T) +
#   labs(x="Baseline FFE (no clip)",y="FFE with clip", title="Simulated Data") +
#   scale_x_continuous(expand =c(0,0), limits=c(15,105), breaks = c(20,30,40,50,60,70,80,90,100)) +
#   scale_y_continuous(expand = c(0,0), limits= c(15,105),breaks = c(20,30,40,50,60,70,80,90,100)) +
#   geom_abline(linetype="dashed", linewidth=1) +
#   annotate(geom="text", x=40, y=84, label = "Improve", color = "navyblue", size =8)+
#   annotate(geom="text", x=80, y=36, label = "Decline", color = "red4", size = 8) +
#   geom_curve(data = arrows, aes(x=x1, y=y1, xend = x2, yend=y2),
#              arrow = arrow(length = unit(0.5,"cm")), linewidth = 1,
#              color = "gray20", curvature = 0) +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust=0.5),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(colour="black"))
# 
# x<-sample(80:90, 1000, replace = TRUE)
# y<-sample(80:90, 1000, replace = TRUE)
# df_samp<-data.frame(cbind(x,y))
# 

##############################################################
## Box and whisker plot for difference
##############################################################
## Absolute Difference

FFE_diff$Condition<-factor(FFE_diff$Condition, levels=c("KN95", "Surgical", "KF94"))


ggplot(FFE_diff, aes(x=Condition, y=Difference, fill = Sex)) +
  geom_boxplot(width = 0.5) +
  labs(x="", y="\u0394 (With Clip - Baseline No Clip)") +
  # scale_fill_discrete(labels=c("Female","Male")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour="black"))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = function(z) seq(-30, range(z)[2], by = 10))



# # Percent Change
# ggplot(FFE_long_diff, aes(x=Condition, y=per_change, fill = Sex)) +
#   geom_boxplot(width = 0.5) +
#   labs(x="", y="Percent Change") +
#   # scale_fill_discrete(labels=c("Female","Male")) +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.x = element_text(size=12, face = "bold"),
#         legend.position = "none",
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(colour="black"))+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   scale_y_continuous(breaks = function(z) seq(-30, range(z)[2], by = 10))


#######################################################################################
## Subset Figures
## These figures remove  KF94(M)
## These figures were removed for the public health paper due to larger variances
## They will be discussed in the craniometrics paper
#######################################################################################
## Figure 1: Baseline data 

#Separated
## Figure 1A
## Overall FFE by mask with points for individuals
ggplot(FFE_long, aes(x=Condition, y=Overall_FFE, color = Sex)) +
  geom_point() +
  geom_boxplot(color = "black",alpha = 0.5, width = 0.3, fill = "gray20") +
  labs(x="", y="Overall FFE (%)") +
  scale_color_discrete(labels=c("Female","Male")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position = "none", 
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour="black"),) +
  guides(colour = guide_legend(override.aes=list(size = 3))) +
  scale_y_continuous(limits = c(15,105), breaks =c(20,30,40,50,60,70,80,90,100))

## Figure 1B
## boxplots
## Overall FFE by mask and sex
ggplot(FFE_long, aes(x=Condition, y=Overall_FFE, fill = Sex)) +
  geom_boxplot() +
  labs(x="", y="Overall FFE (%)") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour="black"),
        plot.margin = unit(c(0,0,.5,.5),"cm")) +
  scale_y_continuous(limits = c(15,105), breaks =c(20,30,40,50,60,70,80,90,100))


##########################################################################
## Figure 2a 
## box and whisker plot of masks with clip
## points colored by sex but no separation
##########################################################################
### Clip Plot
## box plot with baseline and with clip as boxplots, includes points for individuals colored by sex
all<-ggplot(FFE_change_long, aes(x=Condition, y=FFE,color = Sex,  fill= Clip)) +                             
  geom_point(aes(group = Clip),position= position_dodge(width = .7)) +
  geom_boxplot(color = "black",alpha = .5, width = 0.7) +
  labs(x="", y="Overall FFE (%)") +
  scale_color_discrete(labels=c("Female","Male")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position = "none",    
        plot.margin = unit(c(.3,0,0,0),"cm"),                                 #"bottom"
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.background = element_blank(),
        #panel.border = element_rect(color = "black", fill= NA),
        axis.line = element_line(colour="black"),) +
  guides(colour = guide_legend(override.aes=list(size = 3))) +
  scale_fill_manual(values = c("gray20","gray"))+                                                  #, labels = c("Baseline", "With Clip")
  scale_y_continuous(limits = c(0,100), breaks =c(10,20,30,40,50,60,70,80,90,100))


# ##### Outliers in clip data
# ## Surgical lower
# iqr_surg<-IQR(FFE_change_long$FFE[which(FFE_change_long$Condition =="Surgical" & FFE_change_long$Clip == "FFE_clip")])
# q1_surg<-as.numeric(quantile(FFE_change_long$FFE[which(FFE_change_long$Condition =="Surgical" & FFE_change_long$Clip == "FFE_clip")], .25))
# lower_bound_surg<-q1_surg-1.5*iqr_surg
# surg_subset<-subset(FFE_change_long, Condition =="Surgical" & Clip == "FFE_clip")
# surg_subset<-subset(surg_subset, FFE<lower_bound_surg)
# 
# ## Surgical Higher
# q3_surg<-as.numeric(quantile(FFE_change_long$FFE[which(FFE_change_long$Condition =="Surgical" & FFE_change_long$Clip == "FFE_clip")], .75))
# upper_bound_surg<-q3_surg+1.5*iqr_surg
# surg_subset_up<-subset(FFE_change_long, Condition =="Surgical" & Clip == "FFE_clip")
# surg_subset_up<-subset(surg_subset_up, FFE>upper_bound_surg)
# surg_subset<-rbind(surg_subset,surg_subset_up)
# 
# ## KF94 lower
# iqr_KF94<-IQR(FFE_change_long$FFE[which(FFE_change_long$Condition =="KF94" & FFE_change_long$Clip == "FFE_clip")])
# q1_KF94<-as.numeric(quantile(FFE_change_long$FFE[which(FFE_change_long$Condition =="KF94" & FFE_change_long$Clip == "FFE_clip")], .25))
# lower_bound_kf94<-q1_KF94-1.5*iqr_KF94
# kf94_subset<-subset(FFE_change_long, Condition =="KF94" & Clip == "FFE_clip")
# kf94_subset<-subset(kf94_subset, FFE<lower_bound_kf94)
# 
# surg_subset<-rbind(surg_subset,kf94_subset)

#---------------------------------------------------------------------------------------------------------------
## Figure 2B
### Clip plot separated by sex
## Female plot
FFE_change<-left_join(FFE_base, FFE_clip, by=c("Subject","Sex","Condition"))
FFE_change_F<-subset(FFE_change, Sex =="F")
FFE_change_F<-pivot_longer(FFE_change_F, cols = c(4,5), names_to="Clip", values_to="FFE")
FFE_change_F$Clip<-factor(FFE_change_F$Clip, labels = c("Baseline", "With Clip"))



library(scales)

female<-ggplot(FFE_change_F, aes(x=Condition, y = FFE, fill = Clip))+
  geom_boxplot()+
  labs(y="Overall FFE (%)") +
  #annotate(geom="text", x="Surgical", y=10, label = "Female", color = "black", size =6)+
  geom_hline(yintercept = 80, linetype = "dashed") +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        #panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.position = "none",
        axis.line.x = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black", linewidth = 2),
        #axis.line.y.right = element_line(color = "black", linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = unit(c(.3,0,0,0),"cm"),
        axis.text.x = element_text(size=18, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
        # axis.text.y = element_text(size= 18, face = "bold"),
        # axis.title.y = element_text(size = 14, face = "bold"))+
  scale_fill_manual(values = c("#F8766D","#F8766D55"), labels = c("Female Baseline", "Female with Clip"))+
  scale_y_continuous(expand = c(0,0),limits = c(0,100), breaks =c(10,20,30,40,50,60,70,80,90,100))


## Male Plot
FFE_change_M<-subset(FFE_change, Sex =="M")
FFE_change_M<-pivot_longer(FFE_change_M, cols = c(4,5), names_to="Clip", values_to="FFE")
FFE_change_M$Clip<-factor(FFE_change_M$Clip, labels = c("Baseline", "With Clip"))

male<-ggplot(FFE_change_M, aes(x=Condition, y = FFE, fill = Clip))+
  geom_boxplot()+
  labs(y="Overall FFE (%)")+
  #annotate(geom="text", x="Surgical", y=10, label = "Male", color = "black", size =6)+
  geom_hline(yintercept = 80, linetype = "dashed") +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        #panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.line.x = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black", linetype = "dashed", linewidth = 1.5),
        #axis.line.y.right = element_line(color = "black", linetype = "dashed"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = unit(c(.3,.1,0,0),"cm"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=18, face = "bold"))+
  scale_fill_manual(values = c("#00BFC4", "#00BFC455"), labels = c("Male Baseline", "Male With Clip"))+
  scale_y_continuous(expand = c(0,0),limits = c(0,100), breaks =c(10,20,30,40,50,60,70,80,90,100))

egg::ggarrange(all,female,male, nrow =1, widths = c(2,2,2))


# ## side-by-side plots
# 
# diff_plot<-ggplot(FFE_diff, aes(x=Condition, y=Difference, fill = Sex)) +
#   geom_boxplot(width = 0.5) +
#   labs(x="", y="\u0394 (With Clip - Baseline No Clip)") +
#   annotate(geom="text", x="Surgical", y=47, label = "Difference", color = "black", size =6)+
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.x = element_text(size=12, face = "bold"),
#         legend.position = "none",
#         panel.background = element_blank(),
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#         axis.line = element_line(colour="black"),
#         plot.margin = unit(c(0,0,0,0),"cm")) +
#         #axis.line.y.left = element_line(color="black")) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   scale_y_continuous(breaks = function(z) seq(-30, range(z)[2], by = 10), position = "right")
# 
# base_plot<-ggplot(FFE_base, aes(x=Condition, y=FFE, fill = Sex)) +
#   geom_boxplot(width = 0.5) +
#   labs(x="", y="FFE (%)") +
#   annotate(geom="text", x="Surgical", y=98, label = "Baseline", color = "black", size =6)+
#   geom_hline(yintercept = 80, linetype = "dashed")+
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.x = element_text(size=12, face = "bold"),
#         legend.position = "none",
#         panel.background = element_blank(),
#         plot.title=element_text(hjust=0.5),
#         plot.margin = unit(c(0,0,0,0),"cm"),
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#         axis.line = element_line(colour="black"))+
#   scale_y_continuous(limits = c(15,105), breaks =c(20,30,40,50,60,70,80,90,100))
# 
# clip_plot<-ggplot(FFE_clip, aes(x=Condition, y=FFE_clip, fill = Sex)) +
#   geom_boxplot(width = 0.5) +
#   annotate(geom="text", x="Surgical", y=98, label = "With Clip", color = "black", size =6)+
#   labs(x="", y="FFE (%)") +
#   geom_hline(yintercept = 80, linetype = "dashed") +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.x = element_text(size=12, face = "bold"),
#         legend.position = "none",
#         panel.background = element_blank(),
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         plot.margin = unit(c(0,0,0,0),"cm"),
#         axis.ticks.length.y = unit(0,"cm"),
#         axis.line = element_line(colour="black"))+
#   scale_y_continuous(limits = c(15,105), breaks =c(20,30,40,50,60,70,80,90,100))
# 
# egg::ggarrange(base_plot, clip_plot, diff_plot, nrow =1)

# As bar plot

# FFE_clip_sum<-FFE %>% group_by(Sex) %>%
#   summarize(KN95_mean=mean(KN95,na.rm=TRUE),
#             KN95_sd=sd(KN95,na.rm=TRUE),
#             KN95clip_mean=mean(KN95_clip,na.rm=TRUE), 
#             KN95clip_sd=sd(KN95_clip,na.rm=TRUE),
#             Surgical_mean=mean(Surgical, na.rm=TRUE),
#             Surgical_sd=sd(Surgical, na.rm=TRUE),
#             Surgicalclip_mean=mean(Surgical_clip, na.rm=TRUE),
#             Surgicalclip_sd=sd(Surgical_clip, na.rm=TRUE),
#             KF94_mean=mean(KF94,na.rm=TRUE),
#             KF94_sd=sd(KF94, na.rm=TRUE),
#             KF94clip_mean=mean(KF94_clip, na.rm=TRUE), 
#             KF94clip_sd=sd(KF94_clip, na.rm=TRUE))
# 
# FFE_clip_sum
# 
# FFE_clip_sum_long<-pivot_longer(FFE_clip_sum, cols = 2:13, names_to=c("Condition",".value"),names_pattern="(.*)_(mean|sd)")
# FFE_clip_sum_long$Clip<-ifelse(str_sub(FFE_clip_sum_long$Condition,-4,-1)=="clip",1,0)
# # FFE_clip_sum_long$Condition<-gsub("clip","",FFE_clip_sum_long$Condition)
# FFE_clip_sum_long$lower<-FFE_clip_sum_long$mean-FFE_clip_sum_long$sd
# FFE_clip_sum_long$upper<-FFE_clip_sum_long$mean+FFE_clip_sum_long$sd
# FFE_clip_sum_long$Clip<-as.factor(FFE_clip_sum_long$Clip)
# 
# 
# FFE_clip_sum_long$Sex<-ifelse(FFE_clip_sum_long$Clip == 1, paste0(FFE_clip_sum_long$Sex,"_clip"),FFE_clip_sum_long$Sex)
# FFE_clip_sum_long$Condition<-gsub("clip","",FFE_clip_sum_long$Condition)
# FFE_clip_sum_long$Condition<-factor(FFE_clip_sum_long$Condition, levels = c("KN95","Surgical","KF94"))
# FFE_clip_sum_long$Sex<-factor(FFE_clip_sum_long$Sex, labels = c("F Baseline","F Clip", "M Baseline", "M Clip"))
# 
# FFE_clip_sum_long<-as.data.frame(FFE_clip_sum_long)
# 
# ### Barplot of means with error bars of standard deviations
# 
# ggplot(FFE_clip_sum_long, aes(x=Condition, y = mean, fill = Sex))+
#   #geom_col(width = 0.7, position = position_dodge(width = .7), colour = "black")+
#   geom_col_pattern(position = position_dodge(.9),
#                    pattern = c("none","circle", "none",
#                                "circle","none","circle", "none",
#                                "circle","none","circle", "none",
#                                "circle"),
#                    pattern_density=0.1,
#                    pattern_spacing = 0.04,
#                    pattern_fill = "black",
#                    color = "black")+
#   geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), width = 0.4)+
#   labs(y="Avg FFE (%)")+
#   theme(axis.title.x = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_line(color = "gray"),
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#         legend.title = element_blank())+
#   scale_fill_manual(values = c("#F8766D","#F8766D", "#00BFC4", "#00BFC4"))+
#   scale_y_continuous(expand = c(0,0),limits = c(0,100), breaks =c(10,20,30,40,50,60,70,80,90,100))+
#   guides(fill = guide_legend(override.aes = list(pattern=c("none","circle","none","circle"),
#                                                  pattern_spacing = 0.1)))
  

############################################################################################
## Update 10-6-2023
## New configuration of plots
## panel for female, male and difference
## Figure 3
############################################################################################
## Difference plot
ggplot(FFE_diff, aes(x=Condition, y=Difference, fill = Sex)) +
  geom_boxplot(width = 0.5) +
  labs(x="", y="\u0394 (With Clip - Baseline No Clip)") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=18, face = "bold"),
        axis.text.y = element_text(size= 18, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        #legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.line = element_line(colour="black")) +
  #axis.line.y.left = element_line(color="black")) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 1.5) +
  scale_y_continuous(breaks = function(z) seq(-30, range(z)[2], by = 10))


############################################################################################
## Quantile Plots
## Factor Variable added to show quantile for each mask
## Quantile is then compared with the difference between baseline FFE and FFE with clip
## scatterplot with error bars
## Figure 4?
############################################################################################
FFE_quant<-as.data.frame(FFE[,c(1,3,7,9,11,15:17)])

mask<-c("KN95","Surgical", "KF94")

for(i in 1:3) {
  Mask<-mask[[i]]
  FFE_mask<-FFE_quant[,Mask]
  quant<-ifelse(FFE_mask<quantile(FFE_mask,0.25),"1",
                ifelse(FFE_mask<quantile(FFE_mask,0.50) & FFE_mask>=quantile(FFE_mask,0.25),"2",
                       ifelse(FFE_mask<quantile(FFE_mask,0.75) & FFE_mask>=quantile(FFE_mask,0.50),"3",
                              ifelse(FFE_mask>=quantile(FFE_mask,0.75),"4", 0))))
  FFE_quant<-cbind(FFE_quant,quant)
}

colnames(FFE_quant)[c(3:5,9:11)]<-c("KN95_FFE","Surgical_FFE","KF94_FFE","KN95_quantile","Surgical_quantile","KF94_quantile")



FFE_quant_long<-pivot_longer(FFE_quant, col = 3:11,
                             names_to = c("Condition", ".value"), 
                             names_sep = "_")

quant_sum<-FFE_quant_long %>%
  group_by(Condition,quantile) %>%
  summarise(mean_diff = mean(diff),
            sd_diff = sd(diff),
            median_diff = median(diff),
            min_diff = min(diff),
            max_diff = max(diff))

quant_sum$SE<-quant_sum$sd_diff/sqrt(100)
col_pal<-hcl.colors(3,palette = "viridis")

# ggplot(FFE_quant_long, aes(x = quantile, y = diff, fill = Condition)) +
#   geom_boxplot()

quant_sum$Condition<-factor(quant_sum$Condition, levels = c("KN95","Surgical","KF94"))

ggplot(quant_sum, aes(x = quantile, y = mean_diff,group = Condition, color = Condition)) +
  geom_point(aes(shape = Condition), size = 3) +
  geom_errorbar(aes(ymin = mean_diff - 2*SE, ymax = mean_diff + 2*SE), width = .5, linewidth = 1.5)+
  scale_color_manual(values = col_pal)+
  labs(x="Baseline Quartile", y = "Average Difference (Clip - Baseline)") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=18, face = "bold"),
        axis.text.y = element_text(size= 18, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        #legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.line = element_line(colour="black")) 
