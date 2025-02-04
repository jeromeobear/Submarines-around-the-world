

management_fcn=function(country) {
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(janitor) #to build markdown table (tabyl and adorn functions)
  library(knitr) #to make table in html format (kable function)
  library(dplyr) #to use the %<% sign
  library(ggalluvial)
  library(networkD3) # for flowchart
  library(tidyverse) #for flowcharts
  library(ggrepel) # for pie charts
  library(reshape2)
  library(directlabels)
  
  submarine0<<-readRDS("submarine.rds")
  specs0<<-readRDS("specifics.rds")
  weaps0<<-readRDS("weapons.rds")

  submarine<<-submarine0[(submarine0$Operator==country | submarine0$Builder==country | submarine0$Design==country) & submarine0$Fate!="cancelled" & submarine0$Fate!="ordered",]
  specs<<-specs0[(specs0$Operator==country | specs0$Design==country),]
  weaps<<-weaps0[(weaps0$Operator==country | weaps0$Design==country),]
  
  Type_list<<-unique(submarine$Type)
  Propulsion_list<<-unique(submarine$Propulsion)
  Builder_list<<-unique(submarine$Builder)
  Operator_list<<-unique(submarine$Operator)
  
  Type_list_prod<<-unique(submarine[submarine$Builder==country,]$Type)
  Propulsion_prod<<-unique(submarine[submarine$Builder==country,]$Propulsion)
  Type_list_serv<<-unique(submarine[submarine$Operator==country,]$Type)
  Propulsion_serv<<-unique(submarine[submarine$Operator==country,]$Propulsion)
  Builder_list<<-unique(submarine[submarine$Operator==country,]$Builder)
  Operator_list<<-unique(submarine[submarine$Builder==country,]$Operator)
  Class_list_prod<<-unique(submarine[submarine$Builder==country,]$Class)
  Class_list_prod1<<-unique(submarine[submarine$Builder==country & submarine$Laid<="1920-01-01",]$Class)
  Class_list_prod2<<-unique(submarine[submarine$Builder==country & submarine$Compl>="1920-01-01" & submarine$Laid<="1950-01-01",]$Class)
  Class_list_prod3<<-unique(submarine[submarine$Builder==country & submarine$Compl>="1950-01-01",]$Class)
  Class_list_serv<<-unique(submarine[submarine$Operator==country,]$Class)
  Class_list_serv1<<-unique(submarine[submarine$Operator==country & submarine$Comm<="1920-01-01",]$Class)
  Class_list_serv2<<-unique(submarine[submarine$Operator==country & submarine$Decomm>="1920-01-01" & submarine$Comm<="1950-01-01",]$Class)
  Class_list_serv3<<-unique(submarine[submarine$Operator==country & submarine$Decomm>="1950-01-01",]$Class)
  
  Version_list_prod<<-unique(submarine[submarine$Builder==country,]$Version)
  Version_list_prod1<<-unique(submarine[submarine$Builder==country & submarine$Laid<="1920-01-01",]$Version)
  Version_list_prod2<<-unique(submarine[submarine$Builder==country & submarine$Compl>="1920-01-01" & submarine$Laid<="1950-01-01",]$Version)
  Version_list_prod3<<-unique(submarine[submarine$Builder==country & submarine$Compl>="1950-01-01",]$Version)
  Version_list_serv<<-unique(submarine[submarine$Operator==country,]$Version)
  Version_list_serv1<<-unique(submarine[submarine$Operator==country & submarine$Comm<="1920-01-01",]$Version)
  Version_list_serv2<<-unique(submarine[submarine$Operator==country & submarine$Decomm>="1920-01-01" & submarine$Comm<="1950-01-01",]$Version)
  Version_list_serv3<<-unique(submarine[submarine$Operator==country & submarine$Decomm>="1950-01-01",]$Version)
  
  Batch_list_prod<<-unique(submarine[submarine$Builder==country,]$Batch)
  Batch_list_prod1<<-unique(submarine[submarine$Builder==country & submarine$Laid<="1920-01-01",]$Batch)
  Batch_list_prod2<<-unique(submarine[submarine$Builder==country & submarine$Compl>="1920-01-01" & submarine$Laid<="1950-01-01",]$Batch)
  Batch_list_prod3<<-unique(submarine[submarine$Builder==country & submarine$Compl>="1950-01-01",]$Batch)
  Batch_list_serv<<-unique(submarine[submarine$Operator==country,]$Batch)
  Batch_list_serv1<<-unique(submarine[submarine$Operator==country & submarine$Comm<="1920-01-01",]$Batch)
  Batch_list_serv2<<-unique(submarine[submarine$Operator==country & submarine$Decomm>="1920-01-01" & submarine$Comm<="1950-01-01",]$Batch)
  Batch_list_serv3<<-unique(submarine[submarine$Operator==country & submarine$Decomm>="1950-01-01",]$Batch)
  
  weaps$Torpedo=paste0(weaps$Tubes,"x",weaps$Torp_type," (",weaps$Torpedoes," torp or ",weaps$Tube_mines, " mines)")
  
  specs$Total=specs$Number + specs$Incomplete
  specs$Submarines=paste0(specs$Batch,", ",specs$Boats,", ",specs$Year)
  weaps$Submarines=paste0(weaps$Batch,", ",weaps$Boats,", ",weaps$Year)
  specs$Displacement_t=paste0(specs$Disp_full_surf_t,"/", specs$Disp_full_sub_t)
  specs$Power_HP=paste0(specs$Power_surf_HP,"/", specs$Power_sub_HP)
  specs$Speed_kts=paste0(specs$Speed_surf_kts,"/", specs$Speed_sub_kts)
  specs$Range_nm=paste0(specs$Range_surf_nm,"/", specs$Range_sub_nm)
  
}

summary_fcn=function(country) {
  g1=ggplot(submarine[submarine$Status!="modernized" & submarine$Fate!="incomplete" & submarine$Operator==country,], aes(as.numeric(Comm.y))) + geom_bar(col="black", aes(fill=Status)) + scale_fill_manual(values=c(
    "new"="blue",
    "resumed"="lightblue3",
    "second-hand"="green3")) + ggtitle("Number of submarines commissioned by year") + xlab("year") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  
  g2=ggplot(submarine[submarine$Fate!="modernized" & submarine$Operator==country,], aes(as.numeric(Stricken.y))) + geom_bar(col="black", aes(fill=Fate)) + scale_fill_manual(values=c(
    "lost"="red",
    "total loss"="red",
    "scuttled"="purple",
    "transferred"="forestgreen",
    "transferred incomplete"="green3",
    "captured"="brown",
    "captured incomplete"="brown3",
    "surrendered"="lightblue",
    "transferred incomplete"="lightgreen",
    "retired"="lightcyan",
    "hulked"="lightcyan",
    "museum"="lightcyan",
    "incomplete"="lemonchiffon2",
    "cancelled"="lemonchiffon",
    "active"="darkblue",
    "reserve"="blue",
    "building"="goldenrod1",
    "ordered"="lightgrey")) + ggtitle("Fates of submarines by year") + xlab("year") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  print(g1+g2)
  
  Timeline_op=data.frame(day=seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date("2025-01-01"), by="month"))
  Timeline_op$Incomplete=NA
  Timeline_op$Retired=NA
  Timeline_op$Ex_Captured=NA
  Timeline_op$Captured=NA
  Timeline_op$Ex_Transferred=NA
  Timeline_op$Transferred=NA
  Timeline_op$Scuttled=NA
  Timeline_op$Lost=NA
  Timeline_op$Reserve=NA
  Timeline_op$Service=NA
  Timeline_op$Transit=NA
  Timeline_op$Construction=NA
  
  for (i in 1:nrow(Timeline_op)) {
    Timeline_op$Incomplete[i]=sum(submarine[(submarine$Stricken<=Timeline_op$day[i] & submarine$Fate=="incomplete" & submarine$Operator==country),]$value)
    Timeline_op$Retired[i]=sum(submarine[(submarine$Stricken<=Timeline_op$day[i] & submarine$Fate%in%c("hulked","museum","retired","surrendered") & submarine$Operator==country),]$value)
    Timeline_op$Captured[i]=sum(submarine[(submarine$Stricken<=Timeline_op$day[i] & submarine$Foreign>Timeline_op$day[i] & submarine$Fate%in%c("captured","captured incomplete") & submarine$Operator==country),]$value)
    Timeline_op$Ex_Captured[i]=sum(submarine[(submarine$Foreign<=Timeline_op$day[i] & submarine$Fate%in%c("captured","captured incomplete") & submarine$Operator==country),]$value)
    Timeline_op$Transferred[i]=sum(submarine[(submarine$Stricken<=Timeline_op$day[i] & submarine$Foreign>Timeline_op$day[i] & submarine$Fate%in%c("transferred","transferred incomplete") & submarine$Operator==country),]$value)
    Timeline_op$Ex_Transferred[i]=sum(submarine[(submarine$Foreign<=Timeline_op$day[i] & submarine$Fate%in%c("transferred","transferred incomplete") & submarine$Operator==country),]$value)
    Timeline_op$Scuttled[i]=sum(submarine[(submarine$Stricken<=Timeline_op$day[i] & submarine$Fate=="scuttled" & submarine$Operator==country),]$value)
    Timeline_op$Lost[i]=sum(submarine[(submarine$Stricken<=Timeline_op$day[i] & submarine$Fate%in%c("lost","total loss") & submarine$Operator==country),]$value)
    Timeline_op$Reserve[i]=sum(submarine[(submarine$Decomm<=Timeline_op$day[i] & submarine$Stricken>Timeline_op$day[i] & submarine$Operator==country),]$value)
    Timeline_op$Service[i]=sum(submarine[(submarine$Comm<=Timeline_op$day[i] & submarine$Decomm>Timeline_op$day[i] & submarine$Operator==country),]$value)
    Timeline_op$Transit[i]=sum(submarine[(submarine$Compl<=Timeline_op$day[i] & submarine$Comm>Timeline_op$day[i] & submarine$Status=="new" & submarine$Operator==country),]$value)
    Timeline_op$Construction[i]=sum(submarine[(submarine$Laid<=Timeline_op$day[i] & submarine$Compl>Timeline_op$day[i] & submarine$Status=="new" & submarine$Operator==country),]$value)
  }
  
  Timeline_op2=melt(Timeline_op, id.vars=c("day"),variable.name = "Status")
  
  g3=ggplot(Timeline_op2, aes(day,value)) + geom_area(aes(fill=Status), col="black") + scale_fill_manual(values=c(
    "Incomplete"="lemonchiffon",
    "Retired"="lightgrey",
    "Transferred"="forestgreen",
    "Ex_Transferred"="lightgreen",
    "Captured"="brown",
    "Ex_Captured"="lightsalmon",
    "Scuttled"="purple",
    "Lost"="red",
    "Reserve"="lightblue",
    "Service"="darkblue",
    "Transit"="lightblue",
    "Construction"="goldenrod1")) + ggtitle("Cumulative submarine history") + xlab("date") + ylab("number of submarines") + theme(legend.position="bottom")
  
  
  print(g3)
  
}

weapons_fcn=function(country) {
  Timeline_wep=data.frame(day=seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date(max(submarine$Stricken)+30), by="month"))
  Timeline_wep$Mine=NA
  Timeline_wep$SLBM=NA
  Timeline_wep$VLS=NA
  Timeline_wep$Torpedoes=NA
  Timeline_wep$Torpedo_tubes=NA
  
  
  for (i in 1:nrow(Timeline_wep)) {
    Timeline_wep$Service[i]=sum(submarine[(submarine$Comm<=Timeline_wep$day[i] & submarine$Stricken>Timeline_wep$day[i] & submarine$Operator==country),]$value)
    Timeline_wep$Torpedo_tubes[i]=sum(submarine[(submarine$Operator==country & submarine$Comm<=Timeline_wep$day[i] & submarine$Stricken>Timeline_wep$day[i]),]$Tube)
    Timeline_wep$Torpedoes[i]=sum(submarine[(submarine$Operator==country & submarine$Comm<=Timeline_wep$day[i] & submarine$Stricken>Timeline_wep$day[i]),]$Torpedo)
    Timeline_wep$VLS[i]=sum(submarine[(submarine$Operator==country & submarine$Comm<=Timeline_wep$day[i] & submarine$Stricken>Timeline_wep$day[i]),]$VLS)
    Timeline_wep$SLBM[i]=sum(submarine[(submarine$Operator==country & submarine$Comm<=Timeline_wep$day[i] & submarine$Stricken>Timeline_wep$day[i]),]$SLBM)
    Timeline_wep$Mine[i]=sum(submarine[(submarine$Operator==country & submarine$Comm<=Timeline_wep$day[i] & submarine$Stricken>Timeline_wep$day[i]),]$mine_dedicated)
  }
  
  Timeline_wep2=melt(Timeline_wep[,c(1:6)], id.vars=c("day"),variable.name = "Weapons")
  
  g1=ggplot(Timeline_wep, aes(day,Service)) + geom_area(fill="darkblue", col="black") + ggtitle("Number of submarines in service") + xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank())
  g2=ggplot(Timeline_wep2, aes(day,value)) + geom_area(aes(fill=Weapons), col="black") + scale_fill_manual(values=c(
    "Mine"="brown",
    "SLBM"="orange",
    "VLS"="forestgreen",
    "Torpedoes"="blue",
    "Torpedo_tubes"="darkblue")) + ggtitle("Weapons carried in service") + xlab("date") + ylab("Number of weapons in service") + theme(legend.position="bottom")
  
  print(g1/g2)
}

type_fcn=function(country) {
  Boat_Type = data.frame(month = seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date(max(submarine$Stricken)+30), by="month"))
  Boat_Type$Construction_boat = NA
  Boat_Type$Service_boat = NA
  
  for (i in 1:length(Type_list)) {
    df=Boat_Type
    df$Type=Type_list[i]
    for (j in 1:nrow(Boat_Type)) {
      df$Construction_boat[j]=sum(submarine[(submarine$Type==Type_list[i] & submarine$Laid<=Boat_Type$month[j] & submarine$Compl>Boat_Type$month[j] & submarine$Status%in%c("new","resumed") & submarine$Builder==country),]$value)
      df$Service_boat[j]=sum(submarine[(submarine$Type==Type_list[i] & submarine$Comm<=Boat_Type$month[j] & submarine$Stricken>Boat_Type$month[j] & submarine$Operator==country),]$value)
    }
    if(i==1) {
      Boat_Type_tot=df
    } else if (i>1) {
      Boat_Type_tot=rbind(Boat_Type_tot,df)
    }
  }
  
  Boat_Propulsion = data.frame(month = seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date(max(submarine$Stricken)+30), by="month"))
  Boat_Propulsion$Construction_boat = NA
  Boat_Propulsion$Service_boat = NA
  
  for (i in 1:length(Propulsion_list)) {
    df=Boat_Propulsion
    df$Propulsion=Propulsion_list[i]
    for (j in 1:nrow(Boat_Propulsion)) {
      df$Construction_boat[j]=sum(submarine[(submarine$Propulsion==Propulsion_list[i] & submarine$Laid<=Boat_Propulsion$month[j] & submarine$Compl>Boat_Propulsion$month[j] & submarine$Status%in%c("new","resumed") & submarine$Builder==country),]$value)
      df$Service_boat[j]=sum(submarine[(submarine$Propulsion==Propulsion_list[i] & submarine$Comm<=Boat_Propulsion$month[j] & submarine$Stricken>Boat_Propulsion$month[j] & submarine$Operator==country),]$value)
    }
    if(i==1) {
      Boat_Propulsion_tot=df
    } else if (i>1) {
      Boat_Propulsion_tot=rbind(Boat_Propulsion_tot,df)
    }
  }
  
  g13=ggplot(Boat_Type_tot, aes(month,Construction_boat)) + geom_area(aes(fill=Type), col="black") + ggtitle("submarine numbers under construction by Type") + 
    xlab("date") + ylab("number of submarines")+ theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g14=ggplot(Boat_Type_tot, aes(month,Service_boat)) + geom_area(aes(fill=Type), col="black") + ggtitle("submarine numbers in service by Type") + 
    xlab("date") + ylab("number of submarines")+ theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g15=ggplot(Boat_Propulsion_tot, aes(month,Construction_boat)) + geom_area(aes(fill=Propulsion), col="black") + ggtitle("submarine numbers under construction by Propulsion") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g16=ggplot(Boat_Propulsion_tot, aes(month,Service_boat)) + geom_area(aes(fill=Propulsion), col="black") + ggtitle("submarine numbers in service by Propulsion") + xlab("date") + 
    ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  print((g13+g14)/(g15+g16))
}

builder_fcn=function(country) {
  Boat_Builder = data.frame(month = seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date(max(submarine$Stricken)+30), by="month"))
  Boat_Builder$Construction_boat = NA
  Boat_Builder$Service_boat = NA
  
  for (i in 1:length(Builder_list)) {
    df=Boat_Builder
    df$Builder=Builder_list[i]
    for (j in 1:nrow(Boat_Builder)) {
      df$Construction_boat[j]=sum(submarine[(submarine$Builder==Builder_list[i] & submarine$Laid<=Boat_Builder$month[j] & submarine$Compl>Boat_Builder$month[j] & submarine$Status%in%c("new","resumed") & submarine$Operator==country),]$value)
      df$Service_boat[j]=sum(submarine[(submarine$Builder==Builder_list[i] & submarine$Comm<=Boat_Builder$month[j] & submarine$Stricken>Boat_Builder$month[j] & submarine$Operator==country),]$value)
    }
    if(i==1) {
      Boat_Builder_tot=df
    } else if (i>1) {
      Boat_Builder_tot=rbind(Boat_Builder_tot,df)
    }
  }
  
  g17=ggplot(Boat_Builder_tot, aes(month,Construction_boat)) + geom_area(aes(fill=Builder), col="black") + ggtitle("Submarine numbers under construction by Builder") + xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g18=ggplot(Boat_Builder_tot, aes(month,Service_boat)) + geom_area(aes(fill=Builder), col="black") + ggtitle("Submarine numbers in service by Builder") + xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  print((g17+g18))
}

class_fcn=function(country) {
  Boat_Class_serv = data.frame(month = seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date(max(submarine$Stricken)+30), by="month"))
  Boat_Class_serv$Construction_boat = NA
  Boat_Class_serv$Service_boat = NA
  
  for (i in 1:length(Class_list_serv)) {
    df=Boat_Class_serv
    df$Class=Class_list_serv[i]
    for (j in 1:nrow(Boat_Class_serv)) {
      df$Construction_boat[j]=sum(submarine[(submarine$Class==Class_list_serv[i] & submarine$Laid<=Boat_Class_serv$month[j] & submarine$Compl>Boat_Class_serv$month[j] & submarine$Status%in%c("new","resumed") & submarine$Operator==country),]$value)
      df$Service_boat[j]=sum(submarine[(submarine$Class==Class_list_serv[i] & submarine$Comm<=Boat_Class_serv$month[j] & submarine$Stricken>Boat_Class_serv$month[j] & submarine$Operator==country),]$value)
    }
    if(i==1) {
      Boat_Class_serv_tot=df
    } else if (i>1) {
      Boat_Class_serv_tot=rbind(Boat_Class_serv_tot,df)
    }
  }
  
  g15=ggplot(Boat_Class_serv_tot, aes(month,Construction_boat)) + geom_area(aes(fill=Class), col="black") + ggtitle("Submarines under construction by Class") + xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g16=ggplot(Boat_Class_serv_tot, aes(month,Service_boat)) + geom_area(aes(fill=Class), col="black") + ggtitle("Submarines in service by Class") + xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  print((g15+g16))
}

big_summary_fcn=function(country){
  Boat_Class_serv = data.frame(month = seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date("2025-01-01"), by="month"))
  Boat_Class_serv$Construction_boat = NA
  Boat_Class_serv$Service_boat = NA
  
  for (i in 1:length(Class_list_serv)) {
    df=Boat_Class_serv
    df$Class=Class_list_serv[i]
    for (j in 1:nrow(Boat_Class_serv)) {
      df$Construction_boat[j]=sum(submarine[(submarine$Class==Class_list_serv[i] & submarine$Laid<=Boat_Class_serv$month[j] & submarine$Compl>Boat_Class_serv$month[j] & submarine$Status%in%c("new","resumed") & submarine$Operator==country),]$value)
      df$Service_boat[j]=sum(submarine[(submarine$Class==Class_list_serv[i] & submarine$Comm<=Boat_Class_serv$month[j] & submarine$Stricken>Boat_Class_serv$month[j] & submarine$Operator==country),]$value)
    }
    if(i==1) {
      Boat_Class_serv_tot=df
    } else if (i>1) {
      Boat_Class_serv_tot=rbind(Boat_Class_serv_tot,df)
    }
  }
  
  Boat_Version_serv = data.frame(month = seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date("2025-01-01"), by="month"))
  Boat_Version_serv$Construction_boat = NA
  Boat_Version_serv$Service_boat = NA
  
  for (i in 1:length(Version_list_serv)) {
    df=Boat_Version_serv
    df$Version=Version_list_serv[i]
    for (j in 1:nrow(Boat_Version_serv)) {
      df$Construction_boat[j]=sum(submarine[(submarine$Version==Version_list_serv[i] & submarine$Laid<=Boat_Version_serv$month[j] & submarine$Compl>Boat_Version_serv$month[j] & submarine$Status%in%c("new","resumed") & submarine$Operator==country),]$value)
      df$Service_boat[j]=sum(submarine[(submarine$Version==Version_list_serv[i] & submarine$Comm<=Boat_Version_serv$month[j] & submarine$Stricken>Boat_Version_serv$month[j] & submarine$Operator==country),]$value)
    }
    if(i==1) {
      Boat_Version_serv_tot=df
    } else if (i>1) {
      Boat_Version_serv_tot=rbind(Boat_Version_serv_tot,df)
    }
  }
  
  Boat_Batch_serv = data.frame(month = seq(as.Date(min(submarine$Laid, na.rm=T)-30), as.Date("2025-01-01"), by="month"))
  Boat_Batch_serv$Construction_boat = NA
  Boat_Batch_serv$Service_boat = NA
  
  for (i in 1:length(Batch_list_serv)) {
    df=Boat_Batch_serv
    df$Batch=Batch_list_serv[i]
    for (j in 1:nrow(Boat_Batch_serv)) {
      df$Construction_boat[j]=sum(submarine[(submarine$Batch==Batch_list_serv[i] & submarine$Laid<=Boat_Batch_serv$month[j] & submarine$Compl>Boat_Batch_serv$month[j] & submarine$Status%in%c("new","resumed") & submarine$Operator==country),]$value)
      df$Service_boat[j]=sum(submarine[(submarine$Batch==Batch_list_serv[i] & submarine$Comm<=Boat_Batch_serv$month[j] & submarine$Stricken>Boat_Batch_serv$month[j] & submarine$Operator==country),]$value)
    }
    if(i==1) {
      Boat_Batch_serv_tot=df
    } else if (i>1) {
      Boat_Batch_serv_tot=rbind(Boat_Batch_serv_tot,df)
    }
  }
  
  
  Boat_Class_serv_tot1=Boat_Class_serv_tot[Boat_Class_serv_tot$month<="1920-01-01" & Boat_Class_serv_tot$Class%in%Class_list_serv1,]
  Boat_Version_serv_tot1=Boat_Version_serv_tot[Boat_Version_serv_tot$month<="1920-01-01" & Boat_Version_serv_tot$Version%in%Version_list_serv1,]
  Boat_Batch_serv_tot1=Boat_Batch_serv_tot[Boat_Batch_serv_tot$month<="1920-01-01" & Boat_Batch_serv_tot$Batch%in%Batch_list_serv1,]
  g14=ggplot(Boat_Class_serv_tot1, aes(month,Service_boat)) + geom_area(aes(fill=Class), col="black") + ggtitle("operated submarines in service by Class") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g16=ggplot(Boat_Version_serv_tot1, aes(month,Service_boat)) + geom_area(aes(fill=Version), col="black") + ggtitle("operated submarines in service by Version") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g18=ggplot(Boat_Batch_serv_tot1, aes(month,Service_boat)) + geom_area(aes(fill=Batch), col="black") + ggtitle("operated submarines in service by Batch") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  print(g14+g16+g18)
  
  Boat_Class_serv_tot2=Boat_Class_serv_tot[Boat_Class_serv_tot$month>="1920-01-01" & Boat_Class_serv_tot$month<="1950-01-01" & Boat_Class_serv_tot$Class%in%Class_list_serv2,]
  Boat_Version_serv_tot2=Boat_Version_serv_tot[Boat_Version_serv_tot$month>="1920-01-01" & Boat_Version_serv_tot$month<="1950-01-01" & Boat_Version_serv_tot$Version%in%Version_list_serv2,]
  Boat_Batch_serv_tot2=Boat_Batch_serv_tot[Boat_Batch_serv_tot$month>="1920-01-01" & Boat_Batch_serv_tot$month<="1950-01-01" & Boat_Batch_serv_tot$Batch%in%Batch_list_serv2,]
  g14=ggplot(Boat_Class_serv_tot2, aes(month,Service_boat)) + geom_area(aes(fill=Class), col="black") + ggtitle("operated submarines in service by Class") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g16=ggplot(Boat_Version_serv_tot2, aes(month,Service_boat)) + geom_area(aes(fill=Version), col="black") + ggtitle("operated submarines in service by Version") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g18=ggplot(Boat_Batch_serv_tot2, aes(month,Service_boat)) + geom_area(aes(fill=Batch), col="black") + ggtitle("operated submarines in service by Batch") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  print(g14+g16+g18)
  
  Boat_Class_serv_tot3=Boat_Class_serv_tot[Boat_Class_serv_tot$month>="1950-01-01" & Boat_Class_serv_tot$Class%in%Class_list_serv3,]
  Boat_Version_serv_tot3=Boat_Version_serv_tot[Boat_Version_serv_tot$month>="1950-01-01" & Boat_Version_serv_tot$Version%in%Version_list_serv3,]
  Boat_Batch_serv_tot3=Boat_Batch_serv_tot[Boat_Batch_serv_tot$month>="1950-01-01" & Boat_Batch_serv_tot$Batch%in%Batch_list_serv3,]
  g14=ggplot(Boat_Class_serv_tot3, aes(month,Service_boat)) + geom_area(aes(fill=Class), col="black") + ggtitle("operated submarines in service by Class") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g16=ggplot(Boat_Version_serv_tot3, aes(month,Service_boat)) + geom_area(aes(fill=Version), col="black") + ggtitle("operated submarines in service by Version") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g18=ggplot(Boat_Batch_serv_tot3, aes(month,Service_boat)) + geom_area(aes(fill=Batch), col="black") + ggtitle("operated submarines in service by Batch") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  print(g14+g16+g18)
  
}

specs1_fcn=function() {
  g1=ggplot(specs, aes(Submarines, Total)) + geom_col(col="black", aes(fill=Design), alpha=0.5) +
    geom_col(aes(x=Submarines, y=Number, fill=Design), col="black") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Total_text, x=Submarines, y=0),  hjust=-0.02) + coord_flip() + ggtitle("Number of submarines by class") + labs(x="", y="")
  
  g2=ggplot(specs, aes(Submarines, Disp_full_sub_t)) + geom_col(col="black", fill="blue1") +
    geom_col(aes(x=Submarines, Disp_full_surf_t), col="black", fill="blue3") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=paste(Disp_full_surf_t,"/",Disp_full_sub_t), x=Submarines, y=0), col="red",  hjust=-0.02) + coord_flip() + ggtitle("Displacement by class (t)") + labs(x="", y="")
  
  print((g1+g2))
  
}

specs2_fcn=function() {
  g3=ggplot(specs, aes(Submarines, Power_surf_HP)) + geom_col(col="black", fill="green3") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Power_surf_HP, x=Submarines, y=(0),  hjust=-0.02)) + coord_flip() + ggtitle("Surface horsepower") + labs(x="", y="")
  
  g4=ggplot(specs, aes(Submarines, Speed_surf_kts)) + geom_col(col="black", fill="forestgreen") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Speed_surf_kts, x=Submarines, y=(0),  hjust=-0.02)) + coord_flip() + ggtitle("Surface speed (kts)") + labs(x="", y="")
  
  g5=ggplot(specs, aes(Submarines, Power_sub_HP)) + geom_col(col="black", fill="green3") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Power_sub_HP, x=Submarines, y=(0),  hjust=-0.02)) + coord_flip() + ggtitle("Submerged horsepower") + labs(x="", y="")
  
  g6=ggplot(specs, aes(Submarines, Speed_sub_kts)) + geom_col(col="black", fill="forestgreen") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Speed_sub_kts, x=Submarines, y=(0),  hjust=-0.02)) + coord_flip() + ggtitle("Submerged speed (kts)") + labs(x="", y="")
  
  print((g3+g4)/(g5+g6))
  
  g7=ggplot(specs, aes(Submarines, Fuel_t)) + geom_col(col="black", fill="grey") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Fuel_t, x=Submarines, y=(0),  hjust=-0.02)) + coord_flip() + ggtitle("Fuel storage (t)") + labs(x="", y="")
  
  g8=ggplot(specs, aes(Submarines, Range_surf_nm)) + geom_col(col="black", fill="lightblue") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Range_surf_nm, x=Submarines, y=(0),  hjust=-0.02)) + coord_flip() + ggtitle("Surface range (nm)") + labs(x="", y="")
  
  g9=ggplot(specs, aes(Submarines, Depth_m)) + geom_col(col="black", fill="blue3") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Depth_m, x=Submarines, y=(0),  hjust=-0.02), col="red") + coord_flip() + ggtitle("Diving depth (m)") + labs(x="", y="")
  
  g10=ggplot(specs, aes(Submarines, Range_sub_nm)) + geom_col(col="black", fill="lightblue") +
    scale_x_discrete(limits = rev(levels(as.factor(specs$Submarines)))) +
    geom_text(aes(label=Range_sub_nm, x=Submarines, y=(0),  hjust=-0.02)) + coord_flip() + ggtitle("Submerged range (nm)") + labs(x="", y="")
  
  print((g7+g8)/(g9+g10))
  
  g11=ggplot(weaps, aes(Submarines, Torpedoes)) + geom_col(col="black", fill="blue1") +
    geom_col(aes(x=Submarines, Tubes), col="black", fill="blue3") +
    scale_x_discrete(limits = rev(levels(as.factor(weaps$Submarines)))) +
    geom_text(aes(label=Torpedo_text2, x=Submarines, y=0), col="red",  hjust=-0.02) + coord_flip() + ggtitle("Torpedo armement") + labs(x="", y="")
  
  g12=ggplot(weaps, aes(Submarines, Mine_tot)) + geom_col(col="black", fill="brown2") +
    geom_col(aes(x=Submarines, Mines), col="black", fill="brown") +
    scale_x_discrete(limits = rev(levels(as.factor(weaps$Submarines)))) +
    geom_text(aes(label=Mine_text, x=Submarines, y=0), col="black",  hjust=-0.02) + coord_flip() + ggtitle("Mine armement") + labs(x="", y="")
  
  g13=ggplot(weaps, aes(Submarines, VLS)) + geom_col(fill="forestgreen", col="black") +
    scale_x_discrete(limits = rev(levels(as.factor(weaps$Submarines)))) +
    geom_text(aes(label=VLS_text, x=Submarines, y=0),  hjust=-0.02) + coord_flip() + ggtitle("Vertical launch systems") + labs(x="", y="")
  
  g14=ggplot(weaps, aes(Submarines, SLBLS)) + geom_col(fill="orange", col="black") +
    scale_x_discrete(limits = rev(levels(as.factor(weaps$Submarines)))) +
    geom_text(aes(label=SLBM_text, x=Submarines, y=0),  hjust=-0.02) + coord_flip() + ggtitle("Submarine launched ballistic missiles") + labs(x="", y="")
  
  print((g11+g12)/(g13+g14))
  
}

individual_fcn=function(country) {
  submarine$Submarine=fct_reorder(submarine$Submarine, submarine$Rank)
  print(ggplot(submarine[submarine$Operator==country,], aes(x=as.factor(Submarine), ymin=Laid, ymax=Launch)) +
          geom_linerange(col="goldenrod1", size=6) +
          geom_linerange(aes(ymin=Launch, ymax=Compl), col="goldenrod2", size=6) + 
          geom_linerange(aes(ymin=Compl, ymax=Comm), col="lightgreen", size=6) + 
          geom_linerange(aes(ymin=Comm, ymax=Decomm), col="royalblue", size=6) + 
          geom_linerange(aes(ymin=Decomm, ymax=Stricken), col="lightblue", size=6) + 
          geom_linerange(aes(ymin=Stricken, ymax=Sold), col="lightgrey", size=6) + 
          geom_linerange(aes(ymin=Sold, ymax=Foreign), col="lightgreen", size=6) + 
          geom_text(data=submarine[submarine$Fate%in%c("active","building","captured","captured incomplete","hulked","incomplete",
                                                                   "modernized","museum","reserve","retired","scuttled"),],
                    aes(label=Fate,y=Stricken, col=Fate), size=3.5,hjust=-0.05) +
          scale_color_manual(values=c(
                      "active"="darkblue",
                      "building"="goldenrod1",
                      "hulked"="black",
                      "incomplete"="goldenrod1",
                      "modernized"="goldenrod1",
                      "museum"="black",
                      "reserve"="lightblue",
                      "retired"="black",
                      "scuttled"="purple",
                      "surrendered"="brown")) +
          geom_text(data=submarine[submarine$Fate%in%c("lost","total loss"),],aes(label=paste0(Fate,", ",Receiver,", ",Cause), y=Lost), col="red", size=3.5,hjust=-0.05) +
          geom_text(data=submarine[submarine$Fate%in%c("captured","captured incomplete","surrendered"),],aes(label=paste0(Fate,", ",Receiver,", ",Cause), y=Lost), col="brown", size=3.5,hjust=-0.05) +
          geom_text(data=submarine[submarine$Fate%in%c("transferred","transferred incomplete"),],aes(label=paste0(Fate,", ",Receiver,", ",Cause), y=Sold), col="forestgreen", size=3.5,hjust=-0.05) +
          geom_text(aes(label=Version, y=min(submarine[submarine$Operator==country,]$Order, na.rm=T)), size=3,hjust=-0.05) +
    geom_hline(yintercept=as.Date("1914-07-01"), linetype="dashed", color = "forestgreen", size=1.5) +
    geom_hline(yintercept=as.Date("1918-11-01"), linetype="dashed", color = "forestgreen", size=1.5) +
    geom_hline(yintercept=as.Date("1939-09-01"), linetype="dashed", color = "forestgreen", size=1.5) +
    geom_hline(yintercept=as.Date("1945-09-01"), linetype="dashed", color = "forestgreen", size=1.5) +
    scale_x_discrete(limits = rev(levels(as.factor(submarine[submarine$Operator==country,]$Submarine)))) +
    coord_flip() + xlab("submarine name") + ylab("date") + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()))
}

class_count_fcn=function(class) {
  submarine_class=submarine[submarine$Class==class,]
  
  Boat_Class = data.frame(month = seq(as.Date(min(submarine_class$Laid, na.rm=T)-30), as.Date(max(submarine_class$Stricken, na.rm=T)+30), by="month"))
  Boat_Class$Construction_boat = NA
  Boat_Class$Service_boat = NA
  
  builder_list=unique(submarine_class$Builder)
  operator_list=unique(submarine_class$Operator)
  
  for (i in 1:length(builder_list)) {
    df=Boat_Class
    df$Builder=builder_list[i]
    for (j in 1:nrow(Boat_Class)) {
      df$Construction_boat[j]=sum(submarine_class[(submarine_class$Builder==builder_list[i] & submarine_class$Laid<=Boat_Class$month[j] & submarine_class$Compl>Boat_Class$month[j] & submarine_class$Status%in%c("new","resumed")),]$value)
    }
    if(i==1) {
      Boat_Class_prod=df
    } else if (i>1) {
      Boat_Class_prod=rbind(Boat_Class_prod,df)
    }
  }
  
  for (i in 1:length(operator_list)) {
    df=Boat_Class
    df$Operator=operator_list[i]
    for (j in 1:nrow(Boat_Class)) {
      df$Service_boat[j]=sum(submarine_class[(submarine_class$Operator==operator_list[i] & submarine_class$Comm<=Boat_Class$month[j] & submarine_class$Stricken>Boat_Class$month[j]),]$value)
    }
    if(i==1) {
      Boat_Class_serv=df
    } else if (i>1) {
      Boat_Class_serv=rbind(Boat_Class_serv,df)
    }
  }
  
  g13=ggplot(Boat_Class_prod, aes(month,Construction_boat)) + geom_area(aes(fill=Builder), col="black") + ggtitle("built submarine_class_classs under construction by Builder") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g14=ggplot(Boat_Class_serv, aes(month,Service_boat)) + geom_area(aes(fill=Operator), col="black") + ggtitle("built submarine_class_classs in service by Builder") + 
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  
  print(g13+g14)
}

class_batch_fcn=function(class) {
  submarine_class=submarine[submarine$Class==class,]
  
  Boat_Class = data.frame(month = seq(as.Date(min(submarine_class$Laid, na.rm=T)-30), as.Date(max(submarine_class$Stricken, na.rm=T)+30), by="month"))
  Boat_Class$Construction_boat = NA
  Boat_Class$Service_boat = NA
  
  batch_list=unique(submarine_class$Batch)
  
  for (i in 1:length(batch_list)) {
    df=Boat_Class
    df$Batch=batch_list[i]
    for (j in 1:nrow(Boat_Class)) {
      df$Construction_boat[j]=sum(submarine_class[(submarine_class$Batch==batch_list[i] & submarine_class$Laid<=Boat_Class$month[j] & submarine_class$Compl>Boat_Class$month[j] & submarine_class$Status%in%c("new","resumed")),]$value)
      df$Service_boat[j]=sum(submarine_class[(submarine_class$Batch==batch_list[i] & submarine_class$Comm<=Boat_Class$month[j] & submarine_class$Stricken>Boat_Class$month[j]),]$value)
    }
    if(i==1) {
      Boat_Class_batch=df
    } else if (i>1) {
      Boat_Class_batch=rbind(Boat_Class_batch,df)
    }
  }
  
  g17=ggplot(Boat_Class_batch, aes(month,Construction_boat)) + geom_area(aes(fill=Batch), col="black") + ggtitle("built submarine_class_classs under construction by Version") +
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  g18=ggplot(Boat_Class_batch, aes(month,Service_boat)) + geom_area(aes(fill=Batch), col="black") + ggtitle("built submarine_class_classs in service by Version") +
    xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  print(g17+g18)
}

class_serv_fcn=function(class) {
  submarine_class=submarine[submarine$Class==class,]
  
  Timeline=data.frame(day=seq(as.Date(min(submarine_class$Laid, na.rm=T)-30), as.Date("2025-01-01"), by="month"))
  Timeline$Incomplete=NA
  Timeline$Retired=NA
  Timeline$Ex_Captured=NA
  Timeline$Captured=NA
  Timeline$Ex_Transferred=NA
  Timeline$Transferred=NA
  Timeline$Scuttled=NA
  Timeline$Lost=NA
  Timeline$Reserve=NA
  Timeline$Service=NA
  Timeline$Transit=NA
  Timeline$Construction=NA
  
  for (i in 1:nrow(Timeline)) {
    Timeline$Construction[i]=sum(submarine_class[(submarine_class$Laid<=Timeline$day[i] & submarine_class$Compl>Timeline$day[i] & submarine_class$Status=="new"),]$value)
    Timeline$Transit[i]=sum(submarine_class[(submarine_class$Compl<=Timeline$day[i] & submarine_class$Comm>Timeline$day[i] & submarine_class$Status=="new"),]$value)
    Timeline$Service[i]=sum(submarine_class[(submarine_class$Comm<=Timeline$day[i] & submarine_class$Decomm>Timeline$day[i]),]$value)
    Timeline$Reserve[i]=sum(submarine_class[(submarine_class$Decomm<=Timeline$day[i] & submarine_class$Stricken>Timeline$day[i]),]$value)
    Timeline$Lost[i]=sum(submarine_class[(submarine_class$Stricken<=Timeline$day[i] & submarine_class$Fate%in%c("lost","total loss")),]$value)
    Timeline$Scuttled[i]=sum(submarine_class[(submarine_class$Stricken<=Timeline$day[i] & submarine_class$Fate=="scuttled"),]$value)
    Timeline$Transferred[i]=sum(submarine_class[(submarine_class$Stricken<=Timeline$day[i] & submarine_class$Foreign>Timeline$day[i] & submarine_class$Fate%in%c("transferred","transferred incomplete")),]$value)
    Timeline$Ex_Transferred[i]=sum(submarine_class[(submarine_class$Foreign<=Timeline$day[i] & submarine_class$Fate%in%c("transferred","transferred incomplete")),]$value)
    Timeline$Captured[i]=sum(submarine_class[(submarine_class$Stricken<=Timeline$day[i] & submarine_class$Foreign>Timeline$day[i] & submarine_class$Fate%in%c("captured","captured incomplete")),]$value)
    Timeline$Ex_Captured[i]=sum(submarine_class[(submarine_class$Foreign<=Timeline$day[i] & submarine_class$Fate%in%c("captured","captured incomplete")),]$value)
    Timeline$Incomplete[i]=sum(submarine_class[(submarine_class$Stricken<=Timeline$day[i] & submarine_class$Fate=="incomplete"),]$value)
    Timeline$Retired[i]=sum(submarine_class[(submarine_class$Stricken<=Timeline$day[i] & submarine_class$Fate%in%c("hulked","museum","retired","surrendered")),]$value)
  }
  
  Timeline2=melt(Timeline, id.vars=c("day"),variable.name = "Status")
  
  g23=ggplot(Timeline2, aes(day,value)) + geom_area(aes(fill=Status), col="black") + scale_fill_manual(values=c(
    "Incomplete"="lemonchiffon",
    "Retired"="lightgrey",
    "Transferred"="forestgreen",
    "Ex_Transferred"="lightgreen",
    "Captured"="brown",
    "Ex_Captured"="lightsalmon",
    "Scuttled"="purple",
    "Lost"="red",
    "Reserve"="lightblue",
    "Service"="darkblue",
    "Transit"="lightblue",
    "Construction"="goldenrod1")) + ggtitle(paste(class)) + xlab("date") + ylab("number of submarines") + theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank())
  
  print(g23)
}

class_sub_fcn=function(class) {
  submarine_class=submarine[submarine$Class==class,]
  submarine_class$Submarine=fct_reorder(submarine_class$Submarine, submarine_class$Rank)
  print(ggplot(submarine_class[submarine_class$Class==class & submarine_class$Operator==country,], aes(x=as.factor(Submarine), ymin=Laid, ymax=Launch)) +
          geom_linerange(col="goldenrod1", size=6) +
          geom_linerange(aes(ymin=Launch, ymax=Compl), col="goldenrod2", size=6) + 
          geom_linerange(aes(ymin=Compl, ymax=Comm), col="lightgreen", size=6) + 
          geom_linerange(aes(ymin=Comm, ymax=Decomm), col="royalblue", size=6) + 
          geom_linerange(aes(ymin=Decomm, ymax=Stricken), col="lightblue", size=6) + 
          geom_linerange(aes(ymin=Stricken, ymax=Sold), col="lightgrey", size=6) + 
          geom_linerange(aes(ymin=Sold, ymax=Foreign), col="lightgreen", size=6) + 
          geom_text(data=submarine_class[submarine_class$Fate%in%c("active","building","captured","captured incomplete","hulked","incomplete",
                                                                   "modernized","museum","reserve","retired","scuttled"),],
            aes(label=Fate,y=Stricken, col=Fate), size=3.5,hjust=-0.05) +
          scale_color_manual(values=c(
            "active"="darkblue",
            "building"="goldenrod1",
            "hulked"="black",
            "incomplete"="goldenrod1",
            "modernized"="goldenrod1",
            "museum"="black",
            "reserve"="lightblue",
            "retired"="black",
            "scuttled"="purple",
            "surrendered"="brown")) +
          geom_text(data=submarine_class[submarine_class$Fate%in%c("lost","total loss"),],aes(label=paste0(Fate,", ",Receiver,", ",Cause), y=Lost), col="red", size=3.5,hjust=-0.05) +
          geom_text(data=submarine_class[submarine_class$Fate%in%c("captured","captured incomplete","surrendered"),],aes(label=paste0(Fate,", ",Receiver,", ",Cause), y=Lost), col="brown", size=3.5,hjust=-0.05) +
          geom_text(data=submarine_class[submarine_class$Fate%in%c("transferred","transferred incomplete"),],aes(label=paste0(Fate,", ",Receiver,", ",Cause), y=Sold), col="forestgreen", size=3.5,hjust=-0.05) +
          geom_text(aes(label=Version, y=min(submarine[submarine$Class==class & submarine$Operator==country,]$Order, na.rm=T)), size=3,hjust=-0.05) +
          geom_hline(yintercept=as.Date("1914-07-01"), linetype="dashed", color = "forestgreen", size=1.5) +
          geom_hline(yintercept=as.Date("1918-11-01"), linetype="dashed", color = "forestgreen", size=1.5) +
          geom_hline(yintercept=as.Date("1939-09-01"), linetype="dashed", color = "forestgreen", size=1.5) +
          geom_hline(yintercept=as.Date("1945-09-01"), linetype="dashed", color = "forestgreen", size=1.5) +
          scale_x_discrete(limits = rev(levels(as.factor(submarine[submarine$Class==class & submarine$Operator==country,]$Submarine)))) +
          coord_flip() + xlab("date") + ylab("submarine name") + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()))
}



