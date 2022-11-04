# -*- coding: utf-8 -*-
# Python 3.2.2 on Win32

#Plastic Film simulation analysis
#for Michael Braunack 16 April 2014


import glob
import os
import csv
import itertools
import datetime
import time
import calendar



# Film Effects Summary File analysis
#-----------------------------------

# Summarize the Adjustments to Day of Emergence

def findnth(haystack, needle, n):
    parts= haystack.split(needle, n)
    if len(parts)<=n:
        return -1
    return len(haystack)-len(parts[-1])-len(needle)

# FilmEffects.sum  is the grepped summary file of FCOT.out files
#  listing the day of emergence and DAS of emergence
f = open("FilmEffects.sum", "r")
results = {}  #empty results dictionary
n = 0
year = 0
prevSowMonthDay = ""
try:
    for line in f:
        n += 1
        if(line[0:3] == "   ") | (len(line) < 10):
            #discard blank lines and Region header lines
            #print("Line is: >", line, "<")
            pass
            
        else:
            filename = line[0:line.find("Sow")]
            region = filename[0:findnth(filename, "_", 1)]
            film = filename[findnth(filename, "_", 1)+1 : \
                            findnth(filename, "_", 2)]
            sowDay = int("".join(itertools.takewhile(str.isdigit, \
                       filename[findnth(filename, "_",2)+1 : ])))
            sowMonth = filename[-3:]
            monthNumber = time.strptime(sowMonth, '%b').tm_mon
            emergDAS = int(line[line.find(" ***")-3:line.find(" ***")])
               
##            print( "region:", region, " film:", film, \
##                   "  sow date:", sowDay , sowMonth,\
##                   "  emergence DAS:" , emergDAS \
##                 )

            #The Crop Year is not recorded in the summary file,
            # so a dodgy assumption is that there is one emergence
            # for each year, and the grep has returned these in order
            # while the sow date remains the same
            # Simulation years are 2004 to 2013.
            if (sowMonth + str(sowDay)) != prevSowMonthDay:
                if year not in [0,2013] : print("Error in year: Line",n,"year=",year)
                year = 2004
                prevSowMonthDay = (sowMonth + str(sowDay))
            else:
                year += 1
            
            key = region, monthNumber, sowDay, year
            #print('Key:',key)
            if key not in results:
                results[key] = {'NoFilm emerg' : 0, \
                                'Film emerg' : 0, \
                                'DASDiff' : 0, \
                                'NoFilm yield' : 0 , \
                                'Film yield' : 0 , \
                                'NoFilm ET' : 0 , \
                                'Film ET': 0 , \
                                'NoFilm irrigCnt' : 0 , \
                                'Film irrigCnt': 0
                                }
            resultsDtls = results[key]
            #print (resultsDtls)
            if film == 'Film':
                resultsDtls['Film emerg'] = emergDAS
            else:
                resultsDtls['NoFilm emerg'] = emergDAS

            if (resultsDtls['Film emerg'] != 0) & \
                        (resultsDtls['NoFilm emerg'] != 0)  :
                resultsDtls['DASDiff'] = resultsDtls['NoFilm emerg']\
                                       - resultsDtls['Film emerg']

            #print( 'updated resultsDtls:',resultsDtls)
            results[key] = resultsDtls
            #print ('Results dictionary:' , results[key])
                
            
finally:
    f.close()
   # print( "File is ", n, "  lines long.")

   # for i, r in sorted(results.items()):
   #     print(i, r)



#
# Now Summarize the Yield.out files to get a feel of the how the
# adjusted sowing dates actually affected the crop performance
#---------------------------------------------------------------

fileList = glob.glob('*_Y.out')
#print (fileList)
for filename in fileList:
    try:
        f = open(filename,"r")
        #print('File:',filename)
        n = 0
        year = 0
        lintYield = 0.0
        evapoTrans = 0.0
        irrigCnt = 0
        try:
            lines = f.readlines()
            #print (lines[21])
            for d in lines[23:]:
                if len(d) < 20: break
                #print(d)
                dl = d.split()
                year = int(dl[0])
                if "ended" in d:
                    break
                else:
                    lintYield = round(float(dl[3]),1)
                    evapoTrans = int(dl[12])
                    irrigCnt = int(dl[13])

                #print ('LintYld:',lintYield,"ET:",evapoTrans,'IrrigationsCnt:',irrigCnt)    
                n += 1


                # Need to parse the filename to generate the dictionary key
                # then recreate the 'value' to be a list of values generated for
                # the treatment.
                
                fn = filename[0:filename.find("Sow")]
                region = fn[0:findnth(fn, "_", 1)]
                film = fn[findnth(fn, "_", 1)+1 : \
                                findnth(fn, "_", 2)]
                sowDay = int("".join(itertools.takewhile(str.isdigit, \
                           fn[findnth(fn, "_",2)+1 : ])))
                sowMonth = fn[-3:]
                monthNumber = time.strptime(sowMonth, '%b').tm_mon

                key = region, monthNumber, sowDay, year

                #print (region, monthNumber, sowDay, year)
                #print (key)

                if key not in results:
                    results[key] = {'NoFilm emerg' : 0, \
                                    'Film emerg' : 0, \
                                    'DASDiff' : 0, \
                                    'NoFilm yield' : 0 , \
                                    'Film yield' : 0 , \
                                    'NoFilm ET' : 0 , \
                                    'Film ET': 0 , \
                                    'NoFilm irrigCnt' : 0 , \
                                    'Film irrigCnt': 0
                                    }

                resultsDtls = results[key]
            #<<<<<
               # print( 'resultsDtls:',key,resultsDtls)
                if film == 'Film':
                    resultsDtls['Film yield'] = lintYield
                    resultsDtls['Film ET'] = evapoTrans
                    resultsDtls['Film irrigCnt'] = irrigCnt
                else:
                    resultsDtls['NoFilm yield'] = lintYield
                    resultsDtls['NoFilm ET'] = evapoTrans
                    resultsDtls['NoFilm irrigCnt'] = irrigCnt


           #<<<<<
                # print( 'updated resultsDtls:',key,resultsDtls)
                results[key] = resultsDtls

        finally:
            f.close  #yield.out

            
    finally:         #list of *y.out
        pass

#Now create the summary records from the results dictionary
summaryResults = {}
summaryKey = []
tempKey = []
filmCropCnt = 0
noFilmCropCnt = 0
SumDASDiff = 0
filmSumET = 0
filmSumIrrigCnt = 0
noFilmSumET = 0
noFilmSumIrrigCnt = 0
n = 0

for i, r in sorted(results.items()):
    tempKey =  i[0],i[1],i[2]
    if (tempKey != summaryKey) & (len(summaryKey) > 0):
        #create summary record for previous key
        summaryResults[summaryKey] = {'AvgDASDiff': round((SumDASDiff / n),1),\
                                      'CntNoFilmCrops': noFilmCropCnt, \
                                      'CntFilmCrops': filmCropCnt, \
                                      'AvgETFilmCrops': round((filmSumET / n),1), \
                                      'AvgIrrigCntFilmCrops': round((filmSumIrrigCnt / n),1), \
                                      'AvgETNoFilmCrops': round((noFilmSumET / n),1), \
                                      'AvgIrrigCntNoFilmCrops': round((noFilmSumIrrigCnt / n),1) \
                                     }
        filmCropCnt = 0
        noFilmCropCnt = 0
        SumDASDiff = 0
        filmSumET = 0
        filmSumIrrigCnt = 0
        noFilmSumET = 0
        noFilmSumIrrigCnt = 0
        n = 0

    summaryKey = tempKey
        
    #accumulate values
    #print(i,r)
    if (r['Film yield'] > 0.0)   :
        filmCropCnt += 1
        filmSumET += r['Film ET']
        filmSumIrrigCnt += r['Film irrigCnt']
        #print("film Crop incremented:",filmCropCnt)
    if (r['NoFilm yield'] > 0.0) :
        noFilmCropCnt += 1
        noFilmSumET += r['NoFilm ET']
        noFilmSumIrrigCnt += r['NoFilm irrigCnt']
        #print("noFilm Crop incremented:",noFilmCropCnt)
    SumDASDiff += r['DASDiff']
    n += 1

#lastly create the summary record for the last key processed
summaryResults[summaryKey] = {'AvgDASDiff': round((SumDASDiff / n),1),\
                              'CntNoFilmCrops': int(noFilmCropCnt), \
                              'CntFilmCrops': int(filmCropCnt), \
                              'AvgETFilmCrops': round((filmSumET / n),1), \
                              'AvgIrrigCntFilmCrops': round((filmSumIrrigCnt / n),1), \
                              'AvgETNoFilmCrops': round((noFilmSumET / n),1), \
                              'AvgIrrigCntNoFilmCrops': round((noFilmSumIrrigCnt / n),1) \
                             }

#for i, r in sorted(summaryResults.items()):
#    print (i,r)     
        
                
#open a CSV file for writing
csvfile = open('analysisSummary.csv','w', newline='')

#create a csv writer object
mycsvwriter = csv.writer(csvfile, delimiter=',',
                         quoting=csv.QUOTE_NONNUMERIC)
#write header lines
headers1 = ['','','','','',\
           'Days to Emergence',\
           '','','','','',\
           'Yield','','Yielding',\
           '','','',\
            'ET','','','','Irrigation Cnt' \
            ]
headers2 = ['','','','',\
           'NoFilm','Film','Diff',\
           '','','','',\
           'NoFilm','Film','#Crops','#Crops'\
            '','','','NoFilm','Film','','', \
            'NoFilm','Film' \
            ]

csvfile.write("\n")
mycsvwriter.writerow(headers1)
mycsvwriter.writerow(headers2)
csvfile.write("\n")

#csvfile.close()

#now loop through the sorted results and create the csv file
regionID = ''
summaryKey = []
line = 0

for i, r in sorted(results.items()):
    tempKey =  i[0],i[1],i[2]   #region,month,day of sowing
    #print('tempKey:', tempKey)
   # print ('r:',r)
    if (tempKey != summaryKey) & (len(summaryKey) > 0):
        #write a summary record for previous key
        sr = summaryResults[summaryKey]
        data = ['',' ',' ',' ',\
                " ",'Avg Diff:',sr['AvgDASDiff'],'','','','','','',\
                sr['CntNoFilmCrops'], \
                sr['CntFilmCrops'], '','Avg:',\
                sr['AvgETNoFilmCrops'], \
                sr['AvgETFilmCrops'], '','',\
                sr['AvgIrrigCntNoFilmCrops'], \
                sr['AvgIrrigCntFilmCrops'] \
               ]
        
        mycsvwriter.writerow(data)
        csvfile.write("\n"*2)

        summaryKey = tempKey
        line = 0


    summaryKey = tempKey
    if line == 0:  #new region/sow date so print region name and sow date
        data = [tempKey[0],calendar.month_abbr[tempKey[1]],tempKey[2],i[3],\
                r['NoFilm emerg'], \
                r['Film emerg'], \
                r['DASDiff'], \
                ' ',' ','','',\
                r['NoFilm yield'], \
                r['Film yield'], '' ,'','', '',\
                r['NoFilm ET'], \
                r['Film ET'], '','', \
                r['NoFilm irrigCnt'], \
                r['Film irrigCnt'] \
                ]
        line += 1

    else:    
        data = [' ',' ',' ',i[3],\
                r['NoFilm emerg'], \
                r['Film emerg'], \
                r['DASDiff'], \
                ' ',' ','','', \
                r['NoFilm yield'], \
                r['Film yield'],'','','','', \
                r['NoFilm ET'], \
                r['Film ET'], '','', \
                r['NoFilm irrigCnt'], \
                r['Film irrigCnt']\
               ]
        line += 1
    
    mycsvwriter.writerow(data)



#write final summary record since all the results have been printed
sr = summaryResults[summaryKey]
data = ['',' ',' ',' ',\
        " ",'Avg Diff:',sr['AvgDASDiff'],'','','','','','',\
        sr['CntNoFilmCrops'], \
        sr['CntFilmCrops'], '', 'Avg:',\
        sr['AvgETNoFilmCrops'], \
        sr['AvgETFilmCrops'], '','', \
        sr['AvgIrrigCntNoFilmCrops'], \
        sr['AvgIrrigCntFilmCrops'] \
       ]

mycsvwriter.writerow(data)

#close the csv file
csvfile.close()
print(r"File  'analysisSummary.csv'  created!")
