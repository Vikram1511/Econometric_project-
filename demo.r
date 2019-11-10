
#importing file
library(readxl)
require(graphics)
Excel1 <- read_excel("Excel1.xlsx",sheet='y1987')
Excel1['NCAGCA'] <- Excel1['NCA']/Excel1['GCA']
Excel1['NIANCA'] <- Excel1['NIA']/Excel1['NCA']
crops<- list("YBAJRA","YWHEAT","YJOWAR","YMAIZE","YRICE")

#fertilizers quantity per hectare as a feature
fertilizers <- list("NITRO","P2O5","K2O")
var_fert="+"
for (fert in fertilizers){
  curr_fert <- paste(fert,"_hec",sep = "")
  var_fert=paste(var_fert,curr_fert,"+",sep = "")
  Excel1[curr_fert] <- Excel1[paste("Q",fert,sep="")]/(Excel1["TOTAREA"]*1000)
  
}
var_fert=substr(var_fert,start=1,stop=nchar(var_fert)-1)

#rainfall variables
rainfall_monthly <- list("RNJAN","RNFEB","RNMAR","RNAPR","RNMAY","RNJUN","RNJUL","RNAUG","RNSEP","RNOCT","RNNOV","RNDEC")
temperature_monthly <- list()
for (i in 1:length(rainfall_monthly)){temperature_monthly[[i]]<- paste("TN",substr(rainfall_monthly[[i]],start=3,stop=nchar(x)),sep="") }
tot_temp<-0
for (temp_month in temperature_monthly){
  tot_temp <- tot_temp+Excel1[temp_month]
}
Excel1["AnnualMeanTN"] <- tot_temp/length(temperature_monthly)
tot_rain<-0
for (rain_month in rainfall_monthly){
  tot_rain <- tot_rain+Excel1[rain_month]
}
Excel1["AnnualMeanRN"] <- tot_rain/length(rainfall_monthly)

for (element in crops){
  print(element)
  curr_crop <- substr(element,start=2,stop=nchar(element))
  price_crop <- paste("P",curr_crop,sep="")
  #proportion of planted area for each specific crop in netcropped area
  element_area <- paste("A",curr_crop,sep="")
  element_aportion <- paste(element_area,"_NCA",sep = "")
  Excel1[element_aportion]<-Excel1[element_area]/Excel1["GCA"]
  
  #proportion of HYV area for each specifiic crop in total HYV area
  hyv_area <- paste("HYV",curr_crop,sep="")
  hyv_portion <- paste(hyv_area,"_AHYV",sep="")
  Excel1[hyv_portion]<- Excel1[hyv_area]/(Excel1['AHYV']/1000)
  
  reg1 <- lm(paste(element,"~",hyv_portion,"+",element_aportion,"+",price_crop,paste("+NIANCA+NCAGCA+QBULLHA+QTRACHA+DMAQ1+DMAQ2+DMAQ3+DMS02+DMS03+DMS04+DMS05+DMS06+DMS06+DMS08+DMS09+DMS10+DMS11+DMS12+DMS13+DMS14+DMS15+DMS16+DMS17+DMS18+DMS19+DMS20+AnnualMeanRN+AnnualMeanTN",var_fert,sep = "")),data=Excel1)
  #opar <- par(mfrow = c(1,2), mar = c(1, 1, 1, 1))
  #plot(reg1, las = 1)      # Residuals, Fitted, ...
  #par(opar)
  print(summary(reg1))
  print(anova(reg1))
  }

