#############
#HW2
################

#A:  Read  the  World  Bank  webpage  and  store  as  a  nice  data  frame.  
#Source:  The  data  can  be  found  here:  wdi.worldbank.org/table/4.1
# Hints:  You  can  try  readHTMLTable
# We  are  interested  in  the  third  table.  [[3]]  will  select  the  third  element  in  a  
# list.  Store  as  a  data  frame.  
# Rename  the  columns  to  be  meaningful. 
require(XML)

url<-"http://wdi.worldbank.org/table/4.1"

# First I try htmlTreeParse, I get a very nasty result, the table, as a list in XML format,
#no idea how to parse it and put the numbers in the right spots
html.raw<-htmlTreeParse(url, useInternalNodes=T,isURL=T)
mytable<-xpathSApply(html.raw,'//*[@id="ucCustomTablePreview_customTablePreview"]/div[2]')

#Then I follow the hint
mytable<-readHTMLTable(url, colClasses=c("character", rep("numeric",10)))[[3]]

#I check my result
str(mytable) 
#I have a dataframe with country names as characters and other numerical columns as numbers, 
#an a lot of NAs where there are no values
#little cleaning up
mytable[ ,1]<-tolower(mytable[ ,1])
#my columns do not have any name
#I try to use htmlTreeParse again to get the names
myname<-xpathSApply(html.raw,'//*[@id="ucCustomTablePreview_customTablePreview"]/div[1]')
#too nasty for my parsing skills
#I try as a table, why not but it does not read the header first line
myname<-readHTMLTable(url)[[2]]
#maybe table is too much constraint, try
l<-readHTMLList(url)
#fail
#shorter to write by hand

names(mytable)[1]<-"Country"

h1<-c("GDP","Agriculture","Industry","Manufacturing","Services")
h2<-c("1990to2000","2000to2012")

k<-2
for(i in h1) {
  for(j in h2) {
    names(mytable)[k]<-paste0(i,j)
    k<-1+k
  }
}

# B:  Write  an  R  funcNon  that  will  take  the name of  a  country  and  return  its  
# Avg Industrial  Growth  value for 2000‐2012.(Use your dataframe as the data source
# for your function to look up)

iG2000<-function(country) {
  #the function assumes my dataframe is in my environment
  country<-tolower(country)
  if (!(country %in%mytable[,"Country"])) { stop ("invalid country name")}
  
  print(paste(country,mytable[mytable$Country==country,"Industry2000to2012"],sep=" "))  
}

iG2000("france")
iG2000("india")

# By  using  R  to  read  from Wikipedia,  create  a  data  frame  that  contains  
#the names  of  countries and  their Healthcare  expenditure.  
# – You  can  use  any wikipedia page  that  has  a  population  table  in  it.  
# You  can  use any measure  of  healthcare  spending  that  you  find  
# or  like

#I will be using this page http://en.wikipedia.org/wiki/Health_care_system
#It contains data about 10 countries and gives the healthcare cost as percent of gdp

url<-"http://en.wikipedia.org/wiki/Health_care_system"
#I take it all
t<-readHTMLTable(url)
str(t)
#I am interested in the first element, columns 1 and 7
mytable<-readHTMLTable(url, colClasses =c("character", rep("numeric",9)), stringsAsFactors=F)[[1]]
healthCost<-mytable[,c(1,8)]
#I am doing it in 2 steps as I failed to import only the columns I am interested in
#I also failed with stringAsFactors=F , !!! because it is strings with an s

str(healthCost)

#BONUS !!
#As I was browsing in search of data for my project, I stumbled upon some CIA's datasets
#Wow, neither James Bond's nor the Inspecteur Clouseau's companies websites do it'
#They happen to have health expenditure data, so let's match wikipedia/ocde and the CIA data 
#CIA
#I download the data from https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_2225.txt
#I first tried to use the html page, with the above method,
#it failed: Warning message: XML content does not seem to be XML
#then I tried the txt
#I use download.file, it failed, I think because of the https, I tried method="curl", failed as well
#sh: 1: curl: not found
#With the RCurl package I tried
#This worked
v<-getURLContent("https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_2225.txt")
#but all was put in one long character vector
#so I ended up saving the page as txt in my working directory
cia<-read.table("ciahealth.txt", sep="\t", quote="", 
                colClasses=c("NULL", "character", "numeric"))
cia[,1]<-tolower(cia[,1])
#I downloaded the data from the http://stats.oecd.org/ website as XLS format, I need to import
#it now
require(xlsx)
#And I failed, I guess the formatting of the file, with links... was the reason
ocde<-read.xlsx2("depensesSante.xls", sheetIndex=2, startRow=9, endRow=42, colIndex=c(1,4))
# Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl,  : 
#java.lang.IllegalArgumentException: Your InputStream was neither an OLE2 stream, nor an OOXML stream
#So I ended up taking the 2 rows I was interested in, cleared the format and save it indepently as csv
#Note that stringAsFactors=F failed , missing an s again !!!
ocde<-read.csv("depensesSante.csv", header=T)
str(ocde)
ocde<-within(ocde, Country<-tolower(as.character(Country)))
ocde<-within(ocde, expense<-as.numeric(as.character(expense)))

#I merge the 2 datasets on country name. since cia has 190 countries and ocde 34 I expect at most 34
cia.ocde<-merge(cia, ocde, by.x="V2", by.y="Country")
dim(cia.ocde)
#I rename
names(cia.ocde)<-c("country", "cia", "ocde")
#I see only 32 countries
ocde$Country[!(ocde$Country %in% cia.ocde$V2)]
#actually those countries appear in the cia set, but with a slightly different name

#However, the most pressing issues is that the 2 numeric columns do not match
with(cia.ocde, sum(cia==ocde, na.rm=T))

#Where is the truth, I guess worldbank data are in order
#I could only get 2012 data, so I will have to look for similar data, not exactly matching
url<-"http://wdi.worldbank.org/table/2.15"
wb<-readHTMLTable(url, colClasses=c("character", rep("numeric",10)), which=3)
#again I cannot download just 2 columns, putting NULL in colClasses
# Error in data.frame(c("Afghanistan", "Albania", "Algeria", "American Samoa",  : 
# arguments imply differing number of rows: 228, 0
#I keep the first 2 columns
wb<-wb[,1:2]
wb<-within(wb, V1<-tolower(V1))

#I merge again
cia.ocde.wb<-merge(cia.ocde,wb, by.x="country", by.y="V1")
colnames(cia.ocde.wb)[4]<-"wb"

#I am not after precision
summary(cia.ocde.wb)

#I see the cia and world bank have similar distributions
# a plot ?
#I order my df by expenses for the wb (arbitrary)
require(reshape2)
ordered<-cia.ocde.wb[order(cia.ocde.wb$wb),]
order.melt<-melt(ordered, id.vars="country" )
#I transform my countries in a factor variable so my x axis is in the order I want and not by 
#alphabetical order
order.melt$country<-factor(order.melt$country, levels=unique(order.melt$country), ordered=T)
require(ggplot2)
p<-ggplot(data = order.melt, aes(x=country, y= value, colour=variable, group=variable))
p<-p + geom_line() + geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p

#The plot makes the case

#As a conclusion, what is the oecd (ocde in french) doing ?

# Read the data file: KCASANTA142.csv	
# Notice	that it	has	one	column	that	has	the	time and	the	date	and	the	time combined	
#Create	new	columns	for	Month,	Day,	Year,	Hour	and	Minute	in	the	data	frame.	
#You	can	write	a	function	and	use	lapply()	

#The file is in my working directory

mydf<-read.csv("KCASANTA142.csv", stringsAsFactors=F)

#I take my Time column and will make a list
timelist<-strsplit(mydf$Time,"\\-+|\\ |\\:")

#I reshape it as a df
library("plyr")
timedf <- ldply(timelist)
colnames(timedf) <- c("year","month","day","hour","minute","second")

#I make my nice new df
mynewdf<-cbind("temperature"=mydf$TemperatureF, timedf[,c("month","day","year",'hour',"minute")])

#aggregate
perhour<-aggregate(temperature~hour+day+month+year, data=mynewdf, FUN=mean)
perhour2<-ddply(mynewdf,.(hour,day,month,year),summarise, meanTemp=mean(temperature))
