library(ZillowR)
library(xml2)

# fetch json from zillow api
GetDeepSearchResults(address = "Flintridge Dr.", citystatezip = "Dallas, TX", rentzestimate = FALSE, 
                 zws_id = "X1-ZWz1hkngfgj3m3_aui2x", url = "http://www.zillow.com/webservice/GetSearchResults.htm")ß