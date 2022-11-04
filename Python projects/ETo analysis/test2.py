# -*- coding: utf-8 -*-
#linuxversion.py

#ETo simulation summary file analysis

f = open("EToAdjustments.sum", "r")
results = {}  #empty results dictionary
n = 0
try:
    for line in f:
        n += 1
        if  line[1:3] == "  ":
            print(line)
            #pass
        else:
            #print n ,"  lines processed"
            #break
            #pass

            Region = line[0:2]
            Deficit = line[line.find("_")+1:line.find("mm")]
            Forecast = line[line.find("mm")+2:line.find("d",6)]
            AdjPercent= line[line.find("d",6)+1:line.find("%")]
            if "forward" in line:
                Adjustment = "advanced"
            elif "delayed" in line:
                Adjustment = "delayed"
            else:
                Adjustment = "error"
                #errorLine = line
               
           # print( "Region: ", Region, "  Deficit: ", Deficit, \
           #        "mm  Forecast: ", Forecast, " days AdjPercent: " \
           #        , AdjPercent, "%  Adjustment: ", Adjustment)

            if Adjustment != "error":
                key = Region, int(Forecast), int(Deficit), int(AdjPercent), Adjustment
                if key not in results:
                    results[key] = 1
                else:
                    results[key] += 1

            #print (key, results[key])
            
            
finally:
    f.close()
    print( "File is ", n, "  lines long.")

    #sortedResults = sorted(results)
    
    for i, r in sorted(results.items()):
        print(i, r)

   # print('errorLine =', errorLine)
    
        
    
with open('EToAdjustments.sum', 'r') as f:
    data = f.readlines()

    #for line in data:
        #words = line.split()
        #print words

    print( 'The read-in array has ', len(data), ' lines.')
    f.close()
    print( data[1000])
    print( data[20000])
    print( data[40000])
    print( data[60000])

   # f.close
    
    
