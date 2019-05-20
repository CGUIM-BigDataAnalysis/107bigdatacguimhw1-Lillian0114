#載入packages
library(readr)

#將檔案匯入r，為確保資料之最新性，故選取最近之資料
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

#因要新增兩欄位比較大學與研究所以上的畢業薪資漲幅度(研究所薪資 / 大學薪資)，
#以及比較大學與研究所以上的畢業薪資明確之相差金額(研究所薪資 - 大學薪資)，
#故需將原character型態的兩薪資欄位轉為numeric
GraduateAndCollege$CollegeSalary<-as.numeric(GraduateAndCollege$CollegeSalary)
GraduateAndCollege$GraduateSchoolSalary<-as.numeric(GraduateAndCollege$GraduateSchoolSalary)
GraduateAndCollege$GapWithGraAndCol<-GraduateAndCollege$GraduateSchoolSalary-GraduateAndCollege$CollegeSalary
GraduateAndCollege$IncreaseRate<-GraduateAndCollege$GraduateSchoolSalary/GraduateAndCollege$CollegeSalary

#篩選自己有興趣的職業別:
#資訊及通訊傳播業、專業_科學及技術服務業、金融及保險業。
#為呈現相對應的大學畢業薪資與研究所畢業薪資，故將不完整之資料篩選掉
Intrested<-filter(GraduateAndCollege,grepl("^資訊及通訊傳播業|^專業_科學及技術服務業|^金融及保險業",Job))
Intrested<-Intrested[complete.cases(Intrested),]
Intrested<-Intrested[order(Intrested$GraduateSchoolSalary,decreasing = T),]
##<文字說明結果>因對金融及保險業有些興趣，以及本身是以資訊為較專業，故選取此三種有涉及的產業。
##但由於在尚未看過此表時，決定考關於資訊相關方面之研究所，
##因除了本身對資訊的興趣，還認為資訊相關之產業為現代社會之主流，故認為資訊產業之薪水會較高
##但從此表得知，有關資訊領域方面之薪水，無論是大學畢業或研究所畢業之薪水並沒有特別高
##然而，多半「金融及保險業」產業薪資意外的高於資訊相關產業
##因此，在起薪方面，與原先之想像有些微不同
##故為進一步了解「金融及保險業」職業之大學畢業與研究所畢業之薪水是否有明顯漲幅度，故在進行下列分析
RateRank<-Intrested[order(Intrested$IncreaseRate,decreasing = T),]
##<文字說明結果>由RateRank表與Intrested表對照可知，儘管「金融及保險業」產業薪資較高，
##但薪資漲幅度卻不比另兩產業顯著，
##故儘管資訊相關之產業畢業薪水並未為高於「金融及保險業」，
##但由於資訊相關產業，研究所畢業與大學畢業之薪資漲幅度仍有明顯之影響，
##且自己對資訊業相對較有興趣，故仍然會考取有關資訊類之研究所