library(readr)
library(dplyr)
setwd('/Users/faithshimick/Downloads')
WHRraw = read_csv("DataForFigure2.1WHR2023 (1).csv")
Countriesraw= read_csv("world-data-2023.csv")
View(WHRraw)
View(Countriesraw)
colnames(WHRraw)
WHR.props = WHRraw %>%
  filter(row_number() !=99)%>%
  filter(row_number() !=136)%>%
  filter(row_number() !=135)%>%
  mutate(adjLifeEx=(`Healthy life expectancy`)/100)%>%
  mutate(adjGDP=(`Logged GDP per capita`)/11.66)%>%
  select(`Country name`, `Ladder score`, `Logged GDP per capita`, adjGDP, 
         `Explained by: Log GDP per capita`, `Social support`, 
         `Explained by: Social support`,`Healthy life expectancy`,
         `Explained by: Healthy life expectancy`, adjLifeEx,
         `Freedom to make life choices`, 
         `Explained by: Freedom to make life choices`, Generosity, 
         `Explained by: Generosity`, `Perceptions of corruption`,
         `Explained by: Perceptions of corruption`,
         `Ladder score in Dystopia`, `Dystopia + residual`)
View(WHR.props)
mean_of_var= c((summary(WHR.props$adjGDP)[4]), 
               (summary(WHR.props$`Social support`)[4]),
               (summary(WHR.props$adjLifeEx)[4]),
               (summary(WHR.props$`Freedom to make life choices`)[4]),
               (summary(WHR.props$Generosity)[4]),
               (summary(WHR.props$`Perceptions of corruption`)[4]),
               (summary(WHR.props$`Ladder score`)[4]))
mean_of_var
GDPf=((summary(WHR.props$`Explained by: Log GDP per capita`)[4])*11.66)
SSf=(summary(WHR.props$`Explained by: Social support`)[4])*2.5
LEf=(summary(WHR.props$`Explained by: Healthy life expectancy`)[4]*10)*0.5
FLCf=(summary(WHR.props$`Explained by: Freedom to make life choices`)[4])
Genf=(summary(WHR.props$`Explained by: Generosity`)[4])*0.5
CPf=(summary(WHR.props$`Explained by: Perceptions of corruption`)[4])*2.5
mean_of_exp=c(GDPf, SSf,LEf, FLCf, CPf, Genf)
mean_of_exp
adjGDP=WHR.props$adjGDP
SocSup=WHR.props$`Social support`
adjLifeEx=WHR.props$adjLifeEx
LifeChoice=WHR.props$`Freedom to make life choices`
Gener=WHR.props$Generosity
CorPer=WHR.props$`Perceptions of corruption`
eqscore=(GDPf*adjGDP)+(SSf*SocSup)+(LEf*adjLifeEx)+(FLCf*LifeChoice)+(Genf*Gener)-(CPf*CorPer)
WHR.props=WHR.props%>%
  mutate(eqscore=(GDPf*adjGDP)+(SSf*SocSup)+(LEf*adjLifeEx)+(FLCf*LifeChoice)+(Genf*Gener)+(CPf*(1-CorPer)))%>%
  mutate(eqscore=(eqscore/2)-.[[18]])
colnames(WHR.props)
View(WHR.props)
summary(WHR.props$eqscore)

Countriesnew=Countriesraw%>%
  select(`Country`, `Density\n(P/Km2)`, `Agricultural Land( %)`, `Armed Forces size`,
         `Birth Rate`, `Co2-Emissions`, `CPI`, `CPI Change (%)`, `Fertility Rate`,
         `Forested Area (%)`, `GDP`, `Gross primary education enrollment (%)`,
         `Gross tertiary education enrollment (%)`, `Infant mortality`, 
         `Life expectancy`, `Maternal mortality ratio`, `Out of pocket health expenditure`,
         `Physicians per thousand`, `Population`, `Population: Labor force participation (%)`,
         `Unemployment rate`, `Urban_population`, `Latitude`)%>%
  mutate(`Agricultural Land( %)`=as.numeric(gsub('[\\%,]', '', `Agricultural Land( %)`)))%>%
  mutate(`CPI Change (%)`=as.numeric(gsub('[\\%,]', '', `CPI Change (%)`)))%>%
  mutate(`Forested Area (%)`=as.numeric(gsub('[\\%,]', '', `Forested Area (%)`)))%>%
  mutate(`Gross primary education enrollment (%)`=as.numeric(gsub('[\\%,]', '', `Gross primary education enrollment (%)`)))%>%
  mutate(`Out of pocket health expenditure`=as.numeric(gsub('[\\%,]', '', `Out of pocket health expenditure`)))%>%
  mutate(`Gross tertiary education enrollment (%)`=as.numeric(gsub('[\\%,]', '', `Gross tertiary education enrollment (%)`)))%>%
  mutate(`Population: Labor force participation (%)`=as.numeric(gsub('[\\%,]', '', `Population: Labor force participation (%)`)))%>%
  mutate(`Unemployment rate`=as.numeric(gsub('[\\%,]', '', `Unemployment rate`)))%>%
  mutate(`GDP`=as.numeric(gsub('[\\$,]', '', `GDP`)))%>%
  mutate(GDPpercap=`GDP`/`Population`)
Countriesnew=Countriesnew[complete.cases(Countriesnew),]
View(Countriesnew)
summary(Countriesnew$GDPpercap)
AgriculturalLand=Countriesnew$`Agricultural Land( %)`
Countriesnew=Countriesnew%>%
  mutate(WHAgriculturalLand= AgriculturalLand/82.6)%>%
  mutate(WLAgriculturalLand= (1-(AgriculturalLand/82.6)))%>%
  mutate(WHForestedArea= `Forested Area (%)`/98.3)%>%
  mutate(WLForestedArea= (1-(`Forested Area (%)`/98.3)))%>%
  mutate(WHCHgdp= GDPpercap/110172.4)%>%
  mutate(WLCHgdp= (1-(GDPpercap/110172.4)))%>%
  mutate(WHPrimEd=`Gross primary education enrollment (%)`/142.5)%>%
  mutate(WLPrimEd=(1-(`Gross primary education enrollment (%)`/142.5)))%>%
  mutate(WHTriEd= `Gross tertiary education enrollment (%)`/136.6)%>%
  mutate(WLTriEd= (1-(`Gross tertiary education enrollment (%)`/136.6)))%>%
  mutate(WHCHlifex= `Life expectancy`/84.2)%>%
  mutate(WLCHlifex= (1-(`Life expectancy`/84.2)))%>%
  mutate(WHPhysk= `Physicians per thousand`/7.12)%>%
  mutate(WLPhysk= (1-(`Physicians per thousand`/7.12)))%>%
  mutate(WHLaborForce= `Population: Labor force participation (%)`/86.8)%>%
  mutate(WLLaborForce= (1-(`Population: Labor force participation (%)`/86.8)))%>%
  mutate(WLCo2Emissions= `Co2-Emissions`/9893038)%>%
  mutate(WHCo2Emissions= (1-(`Co2-Emissions`/9893038)))%>%
  mutate(WLCPI=CPI/2740.27)%>%
  mutate(WHCPI=(1-(CPI/2740.27)))%>%
  mutate(WLCPIChange= `CPI Change (%)`/254.9)%>%
  mutate(WHCPIChange= (1-(`CPI Change (%)`/254.9)))%>%
  mutate(WLInfantmort=`Infant mortality`/84.5)%>%
  mutate(WHInfantmort=(1-(`Infant mortality`/84.5)))%>%
  mutate(WLMothermort= `Maternal mortality ratio`/1140)%>%
  mutate(WHMothermort= (1-(`Maternal mortality ratio`/1140)))%>%
  mutate(WLOPHealth= `Out of pocket health expenditure`/81.6)%>%
  mutate(WHOPHealth= (1-(`Out of pocket health expenditure`/81.6)))%>%
  mutate(WLUnempRate= `Unemployment rate`/28.18)%>%
  mutate(WHUnempRate= (1-(`Unemployment rate`/28.18)))%>%
  mutate(WHDensity=`Density\n(P/Km2)`/8358)%>%
  mutate(WLDensity=(1-(`Density\n(P/Km2)`/8358)))%>%
  mutate(WHArmedForces=`Armed Forces size`/3031000)%>%
  mutate(WLArmedForces=(1-(`Armed Forces size`/3031000)))%>%
  mutate(WHFertility= `Fertility Rate`/6.91)%>%
  mutate(WLFertility= (1-(`Fertility Rate`/6.91)))%>%
  mutate(WHUrbanpop= Urban_population/842933962)%>%
  mutate(WLUrbanpop= (1-(Urban_population/842933962)))%>%
  select(`Country`, WHAgriculturalLand, WLAgriculturalLand, WHForestedArea, 
         WLForestedArea, WHCHgdp, WLCHgdp, WHPrimEd, WLPrimEd, WHTriEd, WLTriEd,
         WHCHlifex, WLCHlifex, WHPhysk, WLPhysk, WHLaborForce, WLLaborForce,
         WLCo2Emissions, WHCo2Emissions, WLCPI, WHCPI, WLCPIChange, 
         WHCPIChange, WLInfantmort, WHInfantmort, WLMothermort, WHMothermort,
         WLOPHealth, WHOPHealth, WLUnempRate, WHUnempRate, WHDensity, WLDensity,
         WHArmedForces, WLArmedForces, WHFertility, WLFertility, WHUrbanpop, 
         WLUrbanpop)
View(Countriesnew)
colnames(Countriesnew)

install.packages('shinydashboard')
library(shiny)
library(shinydashboard)
last3= data.frame(Country=character(), Result=numeric())
ui= fluidPage(
  tags$style('.container-fluid {background-color: #E7D1FF;}'),
  titlePanel("Happiness Equation"),
  sidebarLayout(
    sidebarPanel(
    selectInput('columns', 'Pick 6 Columns', choices=colnames(Countriesnew), multiple = T),
    selectInput('country', 'Pick Your Country', choices=Countriesnew$Country, multiple=F),
    actionButton('do_the_math', 'Do the Math!')
  ),
  mainPanel(
    strong(span("Important Information", style='color:purple')),
    p("Pick six factors based on their importance to you. The first one 
      you pick should be the most important, the second second, and so on."),
    p("The letters 'WH' signify that you want the value to be high.", 
      em("For example, if high Fertility was important, you would look for", 
       span("WHFertility", style='color:purple'), "." )),
    p("The same holds true for variables you want to be low."),
    strong(span("Variable Breakdown", style='color:purple')),
    p("Agricultural Land: the percentage of the country used for ag purposes"),
    p("Forested Area: % of the country that is forest"),
    p("CHgdp: GDP per capita"),
    p("PrimEd: gross enrollment ratio for primary education"),
    p("TriEd: gross enrollment ratio for tertiary education"),
    p("CHlifex: healthy life expectancy"),
    p("Physk: # of physicians per 1k people"),
    p("Labor Force: % of pop. in the labor force"),
    p("Co2 Emissions: c'mon, man, this is self explanatory (but in tons)"),
    p("CPI: Consumer Price Index"),
    p("CPI Change: % change of CPI from the last year"),
    p("Infant mort: # of <1 year deaths for every 1k births"),
    p("Mother mort: # of maternal deaths per 100k births"),
    p("OP Health: % of total health costs paid out of pocket"),
    p("Unemp Rate: % of labor force that's unemployed"),
    p("Density: people per km^2"),
    p("Armed Forces: the size. but really, this one's easier than Co2 Emissions"),
    p("Fertility: average # of kids a woman has"),
    p("Urban pop: % of pop. living in urban areas"),
    p(" "))),
    shinydashboard::box(
    title="Result", verbatimTextOutput('resulttext')
  ),
  box(title='Past Results', tableOutput('last3_table'))
  )
server=function(input, output) {
  last3 = reactiveValues(data=data.frame(Country=character(), Score=numeric()))
  observeEvent(input$do_the_math, {
    print("Button clicked")
    print(mean_of_exp)
    selectedcolumns = input$columns
    countryname = input$country
    if(length(selectedcolumns) != 6 || nchar(countryname)==0){
      output$resulttext = renderText({ "I said 6 columns, and 1 country. Be so nice right now."})
      return()
    }
    WHRfactors= c(16.49391767, 2.92184066, 1.83984923, 0.54805482, 0.37010395,
                  0.07508089)
    WHRfactors=as.numeric(WHRfactors)
    rowindex = which(Countriesnew$Country ==countryname)
    if(length(rowindex) ==0) {
      output$resulttext = renderText({
        paste("Where is", countryname, "?  I haven't heard of that.")
      })
      return()
    }
    values = unlist(sapply(selectedcolumns, function(col) {
      Countriesnew[rowindex, col]
    }))
    print(values)
    result = ((sum(values*WHRfactors))/2) -1.775
    Country=input$Country
    newtable=data.frame(Country=countryname, Score=result)
    last3$data=rbind(last3$data,newtable)
    output$resulttext = renderPrint({
      paste("Ta-da!", result)
    })
  })
  output$last3_table = renderTable({tail(last3$data, 3)})}
shinyApp(ui, server)

install.packages('prettydoc')
library(prettydoc)
