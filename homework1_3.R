#載入packages
library(readr)

#將指定檔案匯入r
education106 <- read_csv("~/R/hw1/education106.csv")

#只取出需要用到的欄位(職業類別、"大學"畢業薪資、"研究所"以上畢業薪資)，
#且為了讀取方便將選取欄位重新命名
GraduateAndCollege<-data.frame(Job=education106$大職業別,
                               CollegeSalary=education106$`大學-薪資`,
                               GraduateSchoolSalary=education106$`研究所及以上-薪資`,
                               stringsAsFactors = F) 

#將原Dataframe薪資表空值的符號—，轉為NA
GraduateAndCollege$CollegeSalary<-gsub("—","NA",GraduateAndCollege$CollegeSalary)
GraduateAndCollege$GraduateSchoolSalary<-gsub("—","NA",GraduateAndCollege$GraduateSchoolSalary)

#因要新增一欄位比較大學與研究所以上的畢業薪資(研究所薪資 / 大學薪資)，
#故需將原character型態的兩薪資欄位轉為numeric
GraduateAndCollege$CollegeSalary<-as.numeric(GraduateAndCollege$CollegeSalary)
GraduateAndCollege$GraduateSchoolSalary<-as.numeric(GraduateAndCollege$GraduateSchoolSalary)
GraduateAndCollege$IncreaseRate<-GraduateAndCollege$GraduateSchoolSalary/GraduateAndCollege$CollegeSalary

#篩選出研究所以上畢業薪資較大學畢業薪資高的職業，因此對row做子集，列出薪資以大到小提高的順序
#並呈現前十名的資料，因此用head函數
GraduateAndCollege<-filter(GraduateAndCollege,IncreaseRate>1) 
GraduateAndCollege<-GraduateAndCollege[order(GraduateAndCollege$IncreaseRate,decreasing = T),]
head(GraduateAndCollege,10)
##<文字說明結果>從答案可知從事「礦業及土石採取業-事務支援人員」職業讀研究所是最好的，
##因相較於大學畢業薪資，研究所畢業後的薪資整整漲幅1.21%，
##此外，「專業_科學及技術服務業」研究所畢業薪資也漲幅很多(1.20%)，
##特別是其中的「事務支援人員」職位，薪資也漲幅1.19%
##然而，儘管「礦業及土石採取業-事務支援人員」薪資漲幅較大，
##但在前10名中，「礦業及土石採取業-事務支援人員」職業的起薪卻是最低的，
##故由此可猜測，薪資漲幅度有顯著差異，可能是因為起薪相對較低

