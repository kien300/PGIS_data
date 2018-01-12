library(RCurl)
scripts <- getURL('https://raw.githubusercontent.com/kien300/PGIS_data/master/app.R',
                  ssl.verifypeer = FALSE)
eval(parse(text = scripts))
