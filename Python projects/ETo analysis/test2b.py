# -*- coding: utf-8 -*-
#linuxversion.py

#Have user input version and print response

results = {}
f = open("EToAdjustments.sum", "r")
n = 0
try:
    for line in f:
        n += 1
        if  line[1:3] == "  ":
           #print line
           pass
        else:
            #print n ,"  lines processed"
            #break
            #pass

            Region = line[0:2]
            Deficit = line[line.find("_")+1:line.find("mm")]
            Forecast = line[line.find("mm")+2:line.find("d",6)]
            AdjPercent= line[line.find("d",6)+1:line.find("%")]
            if "forward" in line:
                Adjustment = "forward"
            elif "delayed" in line:
                Adjustment = "delayed"
            else:
                Adjustment = "error"
               
           # print "Region: ", Region, "     Deficit: ", Deficit, "mm  Forecast: ", Forecast, " days \
           # AdjPercent: "  , AdjPercent, "%  Adjustment: ", Adjustment

            if Adjustment != 'error':
                key = Region, int(Forecast), int(Deficit), int(AdjPercent)
                if key not in results:
                    results[key] = {'Avg Yield' : 0.0, 'Avg WUE' : 0.0, 'Irrigation Count' : 0, 'Avg Irrigs' : 0.0,\
                                        'Advanced' : 0, 'Delayed' : 0}
                resultsDtls = results[key]
                print (resultsDtls)
                if Adjustment == 'forward':
                    resultsDtls['Advanced'] += 1
                else:
                    resultsDtls['Delayed'] += 1

                print( 'updated resultsDtls:',resultsDtls)
                results[key] = resultsDtls
                print ('Results dictionary:' , results[key])
                
            
finally:
    f.close()
    print "File is ", n, "  lines long."
    
with open('EToAdjustments.sum', 'r') as f:
    data = f.readlines()

    #for line in data:
        #words = line.split()
        #print words

    print 'The read-in array has ', len(data), ' lines.'
    f.close()
    print data[1000]
    print data[20000]
    print data[40000]
    print data[60000]

   # f.close
    
    
