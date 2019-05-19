#載入packages
library(readr)
library(dplyr)

#將指定檔案匯入r
education103 <- read_csv("~/R/hw1/education103.csv")
education104 <- read_csv("~/R/hw1/education104.csv")
education105 <- read_csv("~/R/hw1/education105.csv")
education106 <- read_csv("~/R/hw1/education106.csv")

#103
edu103N3<-data.frame(Job=education103$大職業別,UniWage103=education103$`大學-薪資`,
                     FMUniwage103=education103$`大學-女/男`,stringsAsFactors = F)
edu103N3<-edu103N3[!grepl("—|…",edu103N3$FMUniwage103),]
edu103N3$FMUniwage103<-as.numeric(edu103N3$FMUniwage103)
edu103N3$status<-ifelse(edu103N3$FMUniwage103<100,"M_wage_More_F_wage",
                        ifelse(edu103N3$FMUniwage103>100,"F_wage_More_M_wage"
                               ,"F_wage_Equal_M_wage"))
#104
edu104N3<-data.frame(Job=education104$大職業別,UniWage104=education104$`大學-薪資`,
                     FMUniwage104=education104$`大學-女/男`,stringsAsFactors = F)
edu104N3<-edu104N3[!grepl("—|…",edu104N3$FMUniwage104),]
edu104N3$FMUniwage104<-as.numeric(edu104N3$FMUniwage104)
edu104N3$status<-ifelse(edu104N3$FMUniwage104<100,"M_wage_More_F_wage",
                        ifelse(edu104N3$FMUniwage104>100,"F_wage_More_M_wage"
                               ,"F_wage_Equal_M_wage"))

#105
edu105N3<-data.frame(Job=education105$大職業別,UniWage105=education105$`大學-薪資`,
                     FMUniwage105=education105$`大學-女/男`,stringsAsFactors = F)
edu105N3<-edu105N3[!grepl("—|…",edu105N3$FMUniwage105),]
edu105N3$FMUniwage105<-as.numeric(edu105N3$FMUniwage105)
edu105N3$status<-ifelse(edu105N3$FMUniwage105<100,"M_wage_More_F_wage",
                        ifelse(edu105N3$FMUniwage105>100,"F_wage_More_M_wage"
                               ,"F_wage_Equal_M_wage"))
#106
edu106N3<-data.frame(Job=education106$大職業別,UniWage106=education106$`大學-薪資`,
                     FMUniwage106=education106$`大學-女/男`,stringsAsFactors = F)
edu106N3<-edu106N3[!grepl("—|…",edu106N3$FMUniwage106),]
edu106N3$FMUniwage106<-as.numeric(edu106N3$FMUniwage106)
edu106N3$status<-ifelse(edu106N3$FMUniwage106<100,"M_wage_More_F_wage",
                        ifelse(edu106N3$FMUniwage106>100,"F_wage_More_M_wage"
                               ,"F_wage_Equal_M_wage"))


#請問那些行業女生薪資比男生薪資多?
FMoreM103<-edu103N3[order(edu103N3$FMUniwage103,decreasing = T),]
head(FMoreM103,10)
##<文字說明結果>由FMoreM103得知於103產業中幾乎全部的產業男生薪資高於女生，
##除了:礦業及土石採取業-技術員及助理專業人員、用水供應及污染整治業-服務及銷售工作人員、營造業-服務及銷售工作人
##男女薪資平等沒有差異

#請問哪些行業男生薪資比女生薪資多?
MMoreF103<-edu103N3[order(edu103N3$FMUniwage103),]
head(MMoreF103,10)
##<文字說明結果>由MMoreF103得知於103產業中，
##於前10名中，多數有薪資差異之職位為，大職業別的子職業別為:技藝、機械設備操作及組裝人員
##推測:技藝、機械設備操作及組裝人員之工作內容需強大的勞力與力氣，
##故部分男生做事效率可能會比大部分女生做事效率，故薪資會比女生高


#請問那些行業女生薪資比男生薪資多?
FMoreM104<-edu104N3[order(edu104N3$FMUniwage104,decreasing = T),]
head(FMoreM104,10)
##<文字說明結果>由FMoreM104得知於104產業中，
##「專業、科學及技術服務業-技藝、機械設備操作及組裝人員」女生薪資略高於男生
##而，用水供應及污染整治業-服務及銷售工作人員、不動產業-技藝、機械設備操作及組裝人員、
##醫療保健服務業-服務及銷售工作人員、其他服務業-專業人員
##男女薪資平等沒有差異，其餘的職業男生薪資仍高於女生
##推測，專業、科學及技術服務業-技藝、機械設備操作及組裝人員，
##此職業之刻板印象偏好男生從事，可能此產業少部分女性於此年工作較比男性認真，
##故薪水略高於男生

#請問哪些行業男生薪資比女生薪資多?
MMoreF104<-edu104N3[order(edu104N3$FMUniwage104),]
head(MMoreF104,10)
##<文字說明結果>由MMoreF104得知於104產業中，
##於前10名中，多數有薪資差異之職位為，大職業別的子職業別仍為:技藝、機械設備操作及組裝人員
##推測原因如上述所說


#請問那些行業女生薪資比男生薪資多?
FMoreM105<-edu105N3[order(edu105N3$FMUniwage105,decreasing = T),]
head(FMoreM105,10)
##<文字說明結果>由FMoreM105得知於105產業中，
##「金融及保險業-專業人員」女生薪資略高於男生
##而105年男女薪資平等之職業則比前兩年多一點，
##但，仍有大部分之職業男生薪資仍高於女生
##推測，「金融及保險業-專業人員」女生薪資略高於男生，因此產業之薪水配給可能為業績，
##故，只要女生於今年之業績較好，薪水自然會變高

#請問哪些行業男生薪資比女生薪資多?
MMoreF105<-edu105N3[order(edu105N3$FMUniwage105),]
head(MMoreF105,10)
##<文字說明結果>由MMoreF105得知於105產業中，
##今年男生薪資比女生薪資高的前10名，與前兩年之呈現較不一樣，
##「技藝、機械設備操作及組裝人員」職業之薪資差異度大幅減少，
##


#請問那些行業女生薪資比男生薪資多?
FMoreM106<-edu106N3[order(edu106N3$FMUniwage106,decreasing = T),]
head(FMoreM106,10)
##<文字說明結果>由FMoreM106得知於106產業中，
##「資訊及通訊傳播業-服務及銷售工作人員」女生薪資略高於男生
##而106年男女薪資平等之職業又比去年多，表示，現今產業越來越重視性別平等，
##此外，於此表得知大部分薪資平等所從事的多為「服務及銷售工作人員」
##推測，女生越來越喜歡從事此類職位，且也可能是此類重視服務，故可能只要服務態度好
##被服務的顧客越多，薪資越高

#請問哪些行業男生薪資比女生薪資多?
MMoreF106<-edu106N3[order(edu106N3$FMUniwage106),]
head(MMoreF106,10)
##<文字說明結果>由MMoreF106得知於106產業中，
##今年男生薪資比女生薪資高的前10名，與去年之呈現又較不一樣，
##推測
##
