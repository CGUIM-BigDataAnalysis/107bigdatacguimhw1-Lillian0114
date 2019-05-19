#載入packages
library(dplyr)
library(readr)
#將指定檔案匯入r
education103 <- read_csv("~/R/hw1/education103.csv")
education104 <- read_csv("~/R/hw1/education104.csv")
education105 <- read_csv("~/R/hw1/education105.csv")
education106 <- read_csv("~/R/hw1/education106.csv")

#只取出需要用到的欄位(年度、職業類別、"大學"畢業薪資)，為了讀取方便將選取欄位重新命名
#並為了不讓欄位以Factor之型態儲存，故設定stringsAsFactors = F
education103N1<-data.frame(Job=education103$大職業別,
                         Salary_College=education103$`大學-薪資`,stringsAsFactors = F) 
education106N1<-data.frame(Job=education106$大職業別,
                         Salary_College=education106$`大學-薪資`,stringsAsFactors = F)

#透過職業別inner_join，篩選出103與106都有的職業別，故by用Job欄位進行篩選
#此外，為了讀取方便將選取欄位重新命名
edu103to106<-inner_join(education103N1,education106N1,by="Job")
names(edu103to106)<-c("Job","Salary103_College","Salary106_College")

#將原Dataframe薪資表空值的符號—，轉為NA
edu103to106$Salary103_College<-gsub("—","NA",edu103to106$Salary103_College)
edu103to106$Salary106_College<-gsub("—","NA",edu103to106$Salary106_College)

#因要新增一欄位比較103與106的畢業薪資，故需將原character型態的兩薪資欄位轉為numeric
edu103to106$Salary103_College<-as.numeric(edu103to106$Salary103_College)
edu103to106$Salary106_College<-as.numeric(edu103to106$Salary106_College)
edu103to106$SalaryIncrease<-edu103to106$Salary106_College/edu103to106$Salary103_College

#篩選出薪資106年度較103年度高的職業，因此對row做子集，並將提高的職業另外存成HigherWage的變數
#列出薪資以大到小提高的順序，並呈現前十名的資料，因此用head函數
HigherWage<-filter(edu103to106,SalaryIncrease>1) 
HigherWage<-HigherWage[order(HigherWage$SalaryIncrease,decreasing = T),]
head(HigherWage,10)
##<文字說明結果> 結果得知「其他服務業-技術員及助理專業人員」，是薪水漲幅最高的(共漲幅1.13%)
##此外，前10名中「住宿及餐飲業」職業種類薪資漲幅度就佔了兩名(第2、9名)
##儘管「其他服務業」與「住宿及餐飲業」職業種類薪資有明顯的漲幅度，但從薪水欄位可得知他們起薪相對較低
##如「用水供應及污染整治業」的研究所畢業起薪為31560，但「其他服務業」與「住宿及餐飲業」起薪大概都28000
##因此推測由於他們起薪相對較低，故薪水漲幅度才會較明顯

#將資料不完整(NA)之資料篩選掉(complete.cases)，留下提高超過5%的的職業有哪些(OverFivePercent)
OverFivePercent<-HigherWage[HigherWage$SalaryIncrease>1.05,]
OverFivePercent<-OverFivePercent[complete.cases(OverFivePercent),]
OverFivePercent
##<文字說明結果> 從OverFivePercent表得知薪資有明顯漲幅的職業共有41個，一共有36個職業並未有明顯之漲幅
##(自己另外以nrow(HigherWage)-nrow(OverFivePercent)=36得知)，

#主要的職業種別是哪些種類，故使用strsplit取出職業別中"-" 前面的字串，了解是那些職業種類，
#將後方的子職業篩選掉，留下主要職業種類別，此外，由於切割完之字串為list型態，
#故轉乘向量之型態，最後在用table計算次數，並以次數最多至最小排序列出主要職業別
JobType<-strsplit(OverFivePercent$Job,"-")
for(n in 1:length(JobType)){
  JobType[n]<-JobType[[n]][1]
}
sort(table(unlist(JobType)),decreasing = T)
##<文字說明結果> 由此統計可得知 用水供應及污染整治業、教育服務業、資訊及通訊傳播業
##為主要的薪資漲幅度主要職業別，推測: 因這些產業都需要相對規模專業的知識，故可能為漲幅度之原因