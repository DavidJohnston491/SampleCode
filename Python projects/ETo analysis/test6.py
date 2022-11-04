# -*- coding: utf-8 -*-
#linuxversion.py

#ETo simulation analysis

import glob
import os
import subprocess

#subprocess.call("c:\program files (x86)\notepad++\notepad++.exe"

# ETo Summary File analysis
#--------------------------

# Summarize the Adjustments to the Irrigations

f = open("EToAdjustments.sum", "r")
results = {}  #empty results dictionary
n = 0
try:
    for line in f:
        n += 1
        if  line[1:3] == "  ":
            #print(line)
            pass
        else:
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

##            if Adjustment != "error":
##                key = Region, int(Deficit), int(Forecast), int(AdjPercent),\
##                      Adjustment
##                if key not in results:
##                    results[key] = 1
##                else:
##                    results[key] += 1


            if Adjustment != 'error':
                key = Region, int(Deficit), int(Forecast), int(AdjPercent)
                if key not in results:
                    results[key] = {'Avg Yield' : 0.0, 'Avg WUE' : 0.0, 'Irrigation Count' : 0, 'Avg Irrigs' : 0.0,\
                                        'Advanced' : 0, 'Delayed' : 0}
                resultsDtls = results[key]
                #print (resultsDtls)
                if Adjustment == 'advanced':
                    resultsDtls['Advanced'] += 1
                else:
                    resultsDtls['Delayed'] += 1

                #print( 'updated resultsDtls:',resultsDtls)
                results[key] = resultsDtls
                #print ('Results dictionary:' , results[key])
                



            
            
finally:
    f.close()
    #print( "File is ", n, "  lines long.")

    #for i, r in sorted(results.items()):
    #    print(i, r)

#
# Now Summarize the Yield.out files to get a feel of the how the
# adjusted irrigations actually affected the crop performance
#---------------------------------------------------------------

fileList = glob.glob('*_Y.out')
#print (fileList)
for filename in fileList:
    try:
        f = open(filename,"r")

        n = 0
        totalYield = 0.0
        totalWUE = 0.0
        totalIrrig = 0
        try:
            lines = f.readlines()
            #print (lines[21])
            for d in lines[23:]:
                if len(d) < 20: break
                dl = d.split()
                totalYield = round(totalYield + float(dl[3]),1)
                totalWUE = round(totalWUE + float(dl[23]),1)
                totalIrrig = totalIrrig + int(dl[13])
                n += 1
        
        finally:
            f.close
##            print (i,' Avg Yld:', round(totalYield/n,1),\
##                   'Avg WUE:', round(totalWUE/n,1),\
##                   'Total Irrigs:', totalIrrig)

            # Need to parse the filename to generate the dictionary key
            # then recreate the 'value' to be a list of values generated for
            # the treatment.
            Region = []
            Deficit = []
            Forecast = []
            AdjPercent= []
            
            Region = filename[0:2]
            # Need to parse Control file names differently from others
            if 'Control' in filename:
                Region = Region #+ '_Control'
                Deficit = filename[filename.find("Control")+7:filename.find("_Y")]
                Forecast = 0
                AdjPercent = 0.0
            else:
                Deficit = filename[filename.find("_")+1:filename.find("mm")]
                Forecast = filename[filename.find("mm")+2:filename.find("d",6)]
                AdjPercent= filename[filename.find("d",6)+1:filename.find("%")]

            key = Region, int(Deficit), int(Forecast), int(AdjPercent)

            #print (Region, Deficit, Forecast, AdjPercent)
            #print (key)

            if key not in results:
                results[key] = {'Avg Yield' : 0.0, 'Avg WUE' : 0.0, \
                                'Irrigation Count' : 0, 'Avg Irrigs' : 0.0,\
                                'Advanced' : 0, 'Delayed' : 0}

            resultsDtls = results[key]
            resultsDtls['Avg Yield'] = round(totalYield/n,1)
            resultsDtls['Avg WUE'] = round(totalWUE/n,1)
            resultsDtls['Irrigation Count'] = int(totalIrrig)
            resultsDtls['Avg Irrigs'] = round(totalIrrig/n,1)

            #print( 'updated resultsDtls:',resultsDtls)
            results[key] = resultsDtls


            
            
    finally:
        pass

regionID = ''
for i, r in sorted(results.items()):
    if i[2] == 0:
        CtrlFlag = "'Contrl Treatment'"
        advance = '\n'
    else:
        CtrlFlag = ''
        advance = ''
    print(advance,i, r, CtrlFlag)
    if i[0] != regionID:
        regionID = i[0]
        print ('\n',i, r['Avg Yield'], r['Irrigation Count'], \
               r['Avg Irrigs'],r['Avg WUE'], CtrlFlag)
                

         
