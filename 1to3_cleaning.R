# 資料預處理
data<- read.csv("1_MLBbatting.csv")
colnames(data)[1]<- "last_name"
#kable(head(data, 10))

hr<- which(colnames(data)=='b_home_run')
ab<- which(colnames(data)=='b_ab')
rbi<- which(colnames(data)=='b_rbi')
pa<- which(colnames(data)=='b_total_pa')
gidp<- which(colnames(data)=='b_gnd_into_dp')
gitp<- which(colnames(data)=='b_gnd_into_tp')
hbp<- which(colnames(data)=='b_hit_by_pitch')
ibb<- which(colnames(data)=='b_intent_walk')
bb<- which(colnames(data)=='b_walk')
SacBunt<- which(colnames(data)=='b_sac_bunt')
SacFly<- which(colnames(data)=='b_sac_fly')
Sac<- which(colnames(data)=='b_total_sacrifices')
r<- which(colnames(data)=='r_run')
cs<- which(colnames(data)=='r_total_caught_stealing')
sb<- which(colnames(data)=='r_total_stolen_base')
obp<- which(colnames(data)=='on_base_percent')
avg<- which(colnames(data)=='batting_avg')

# sac bunt、fly補0有點怪怪的 但pca不能有na
data[, hr]<- round(data[, hr]/data[, ab]*100, 2)
data[, rbi]<- round(data[, rbi]/data[, pa], 2)
data[, gidp]<- round((data[, gidp]+ data[, gitp])/data[, ab]*100, 2)
data[, hbp]<- round(data[, hbp]/data[, pa]*100, 2)
data[, ibb]<- ifelse(round(data[, ibb]/data[, bb]*100, 2)=='NaN', 0, round(data[, ibb]/data[, bb]*100, 2))
#data[, SacBunt]<- ifelse(round(data[, SacBunt]/data[, Sac]*100, 2)=='NaN', 0, round(data[, SacBunt]/data[, Sac]*100, 2))
#data[, SacFly]<- ifelse(round(data[, SacFly]/data[, Sac]*100, 2)=='NaN', 0, round(data[, SacFly]/data[, Sac]*100, 2))
data[, r]<- round(data[, r]*100/data[, pa], 2)
data[, Sac]<- round(data[, Sac]/data[, pa]*100, 2)
data[, cs]<- ifelse(round((data[, cs]+data[, sb])*100/(data[, obp]*data[, pa]), 2)=='NaN', 0, round((data[, cs]+data[, sb])*100/(data[, obp]*data[, pa]), 2))
data[, sb]<- ifelse(round(data[, sb]*100/(data[, obp]*data[, pa]), 2)=='NaN', 0, round(data[, sb]*100/(data[, obp]*data[, pa]), 2))
data[, c(12:15)]<- round(data[, c(12:15)]*100, 2)

#View(data)
cleandata<- data[, -c(1:4, bb, gitp, SacBunt, SacFly)]

cname<-c('Age','AB','PA','HR%','K%','BB%', 'AVG', 
         'SLG', 'OBP', 'RBI/PA', 'CSSB%', 'SB%', 'DPTP%', 'HBP%', 
         'IBB%', 'R/PA', 'Sac%', 'Walkoff', 
         'AvgEV(MPH)', 'AvgLA(°)', 'SweetSpot%', 'Barrel%', 
         'SolidContact%', 'Flare/Burner%', 'Under%', 'Topped%', 
         'Poor/Weak%', 'HardHit%', 'ZoneSwing%', 'OutofZoneSwing%', 
         'OutofZoneContact%', 'InZoneContact%', 'Whiff%', 'Swing%', 
         'Pull%', 'StraightAway%', 'Oppo%', 'GB%', 'FB%', 'LD%', 
         'Popup%', 'HPto1B', 'SprintSpeed')  
colnames(cleandata)<- cname


# 填補遺失值
#跑步速度變數缺失
hp1b<- which(colnames(cleandata)=='HPto1B')
speed<- which(colnames(cleandata)=='SprintSpeed')

product<- mean(cleandata[, hp1b]*cleandata[, speed], na.rm = T)
na45<- round(product/cleandata[, speed], 2)
na46<- round(product/cleandata[, hp1b], 2)
cleandata[which(is.na(cleandata[, hp1b])), hp1b]<- na45[which(is.na(cleandata[, hp1b]))]
cleandata[which(is.na(cleandata[, speed])), speed]<- na46[which(is.na(cleandata[, speed]))]

cleandata[which(is.na(cleandata[, hp1b])), hp1b]<- round(mean(cleandata[, hp1b], na.rm = T), 2)
cleandata[which(is.na(cleandata[, speed])), speed]<- round(mean(cleandata[, speed], na.rm = T), 2)

# 短打仔補值
coll<- which(is.na(cleandata))%/%nrow(cleandata)
cleandata[230, coll+1]<- 15
cleandata[230, coll+1]<-  0
write.csv(data.ss, "2_MLBbatting_everybatter.csv", row.names = FALSE)


# 標準化
data<- read.csv("2_MLBbatting_everybatter.csv")
View(data)
data_use<- data[, 7:length(colnames(data))]
data.s = scale(data_use, center = TRUE, scale = TRUE)

data.ss = cbind(data[, 1:6], data.s)
write.csv(data.ss, "3_MLBbatting_scale.csv", row.names = FALSE)
View(data.s)

meanscale<- cbind(attr(data.s,"scaled:center"),attr(data.s,"scaled:scale"))
colnames(meanscale)<- c("center", "scale")
write.csv(meanscale, "3_var_standardize.csv")
