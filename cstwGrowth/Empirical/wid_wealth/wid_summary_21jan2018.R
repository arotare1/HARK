wid_summary <- read.csv('/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical/wid_wealth/wid_summary_21jan2018.csv')

wid_summary <- wid_summary[wid_summary$Percentiles!='Macro variable (not applicable)',]
wid_summary <- wid_summary[grep('Wealth', wid_summary$Variable.category),]
wid_summary <- wid_summary[wid_summary$Short.name.of.variable=='Net personal wealth',]
wid_summary <- wid_summary[wid_summary$Type.s..of.variable=='Share',]

