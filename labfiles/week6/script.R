# BUEC 333
# Tutorial


require(xlsx)


# read the data
dat = read.xlsx('Growth.xlsx', sheetIndex = 1, header = TRUE)


# Create the scatterplot
require(ggplot2)

scatterplot = ggplot(data = dat, aes(x=tradeshare, y=growth, label = country_name))

scatterplot = scatterplot + geom_point() + geom_text(aes(label=country_name, vjust=-0.5))
