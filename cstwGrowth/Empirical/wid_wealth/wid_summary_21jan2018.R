wid <- read.csv('/Users/andreea/Desktop/wid_summary.csv')

wid <- wid[wid$Percentiles!='Macro variable (not applicable)',]
wid <- wid[grep('Wealth', wid$Variable.category),]
wid <- wid[wid$Short.name.of.variable=='Net personal wealth',]
wid <- wid[wid$Type.s..of.variable=='Share',]
