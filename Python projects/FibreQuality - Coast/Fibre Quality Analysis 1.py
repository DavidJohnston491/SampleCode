# -*- coding: utf-8 -*-
# Python 3.2.2 on Win32

#from time import sleep
#from time import time
import datetime
import csv
import glob
import os
import copy
import numpy as np

# Request Fibre Quality CSV file name
FibreQualityInput = input('Enter Fibre Quality File: ')

# Request Canopy Temperature CSV file name
CanopyTempInput = input('Enter Canopy Temperature File: ')

### Request background weather file name
##File3 = input('Enter Weather File: ')
##Weather = open(File3, 'r')


Treatments = {}   #empty Treatments dictionary
SowDate = {}      #empty SowDate dictionary
ResultsType = {}   #empty ResultsType dictionary
TempReadings = {}   #empty TempReadings dictionary
QualList = {}   #empty Quality Details dictionary

#First read the Fibre Quality file
# Process the first line for the column headings and use these as the
# Quality dictionary keys.
# Then process the data lines 2 thru file end creating the necessary wrapper
# keys to eventually populate the 'Treatments' dictionary.

FQFile = open (FibreQualityInput, 'r')

#Setup the 'Treatment' key values based on the file name
if 'US' in FibreQualityInput:
    location = 'US'
else:
    location = 'AU'
season = FibreQualityInput[-8:-4]
#print ('season :',season)

try:
    fQLines = FQFile.readlines()
    
    #parse for quality parameters
    lintQualNames = fQLines[0].split(",")
    treatmentIdx = lintQualNames.index("Trtmnt")
    sowIdx = lintQualNames.index("Sowing")

    #fibre quality parameters start in the 5th column 
    qualNamesStart = 4

    # now read the data lines and process
    QualList = {}
    for ln in range (1, len(fQLines)):
        qualData = fQLines[ln].split(",")
        for lQIdx in range (qualNamesStart , len(lintQualNames)):
            QualList[lintQualNames[lQIdx]] = qualData[lQIdx]

        #print (QualList)

        #get the name of this irrigation treatment
        irrigTrt = qualData[treatmentIdx].replace(' ','')

        #create the key for the Treatments dictionary
        treatmentKey = location, season, irrigTrt.lower()

        #create the key for the SowDate dictionary
        sowKey = qualData[sowIdx]

        #create the key for the ResultsType dictionary
        resultsKey = 'QSample1'

        # Note: this key will be QSample1 or QSample2
        #       depending on if it is the first or second sample
        #       So check the dictionary to see if a record exists

        if treatmentKey not in Treatments:
            trtDtls = {}
        else:
            trtDtls = copy.deepcopy(Treatments[treatmentKey])

        if sowKey not in trtDtls:
            sowDateDtls = {}
        else:
            sowDateDtls = copy.deepcopy(trtDtls[sowKey])

        #there should be no results for the details that we are about to add
        if resultsKey not in sowDateDtls:     #no record for QSample1
            ResultsDtls = {}
        else:
            resultsKey = 'QSample2'
            ResultsDtls = {}
            
        #now assign the values and back out of the stack   
        ResultsDtls = QualList
        
        sowDateDtls[resultsKey] = ResultsDtls
        print ('sowDateDtls:', sowDateDtls)

        trtDtls[sowKey] = sowDateDtls

        Treatments[treatmentKey] = trtDtls

        #Treatments dictionary is now populated with Fibre Quality data
        
finally:
    FQFile.close

    #clean up temporary data structures
    fQLines = []
    QualList = {}         #empty Quality Details dictionary
    ResultsDtls = {}      #empty Results Details dictionary
    sowDateDtls = {}      #empty sowDateDtls dictionary
    trtDtls = {}          #empty treatment Dtls dictionary
    
#print (Treatments)
print (Treatments['AU','2011','dry']['15/04/2011'])

#
# Now we need to read the Canopy Temperature .csv file and load the data into
# the corresponding treatments and sow date records of the Treatments dictionary.
#

#of the format
# ,,,,,15/04/2011,15/04/2011,15/04/2011,15/04/2011,29/04/2011,
# ,,,,Bare Soil,Dry,1.5 mm,3.0 mm ,6.0 mm,Dry,1.5 mm,3.0 mm ,6.0 mm,
# Date,Time,Ambient.Temp,RH,10884,45298,45271,45268,45289,45270,45311,
# 15/04/2011,0:00:00,7.78,57.6,6.83,7.16,7.41,7.01,7.36,6.98,6.67,7.01,
# 15/04/2011,0:15:00,7.78,56.9,6.92,7.32,7.4,7.15,7.5,7.25,6.78,7.02,

CTFile = open (CanopyTempInput, 'r')

#Setup the 'Treatment' key values based on the file name
if 'US' in CanopyTempInput:
    location = 'US'
else:
    location = 'AU'
season = CanopyTempInput[-8:-4]
#print ('season :',season)

try:
    ctLines = CTFile.readlines()
    
    #parse for Treatment and season values
    sowDateNames = ctLines[0].split(",")     #1st line from 6th column
    treatmentNames = ctLines[1].split(",")   #2nd line from 6th column

    # now read the data lines and process
    EnvironmentReadings = {}     #for Ambient T, Rel Humidity, Soil T
    for ln in range (3, len(ctLines)):
        tempData = ctLines[ln].split(",")
        dateStr = tempData[0].split("/")
        timeStr = tempData[1].split(":")
        readingTime = datetime.datetime(int(dateStr[2]), \
                                        int(dateStr[1]), \
                                        int(dateStr[0]), \
                                        int(timeStr[0]), \
                                        int(timeStr[1]), \
                                        int(timeStr[2]) )

        #store environmental values for this time
        EnvironmentReadings[str(readingTime)] = {"AmbientT" : tempData[2], \
                                                 "RelHumidity" : tempData[3], \
                                                 "BareSoil" : tempData[4] }

        #now loop through the data line recording the temperature values
        for dataIdx in range (5, len(tempData)):
            
            #create a canopy temperature record  (if not NULL)
            if tempData[dataIdx] not in ( '\n', "", " ") : 
                canopyTempRec = {readingTime : tempData[dataIdx]}


                #get the name of this irrigation treatment
                irrigTrt = treatmentNames[dataIdx].replace(' ','')

                #create the key for the Treatments dictionary
                treatmentKey = location, season, irrigTrt.lower()

                #create the key for the SowDate dictionary
                sowKey = sowDateNames[dataIdx]

                #create the key for the ResultsType dictionary
                resultsKey = 'CanopyTemp'

                if treatmentKey not in Treatments:
                    trtDtls = {}
                else:
                    trtDtls = Treatments[treatmentKey]

                if sowKey not in trtDtls:
                    sowDateDtls = {}
                else:
                    sowDateDtls = trtDtls[sowKey]

                if resultsKey not in sowDateDtls:     
                    temperatureDtls = {}
                else:
                    temperatureDtls = sowDateDtls[resultsKey]
                    
                #now create a new temperature entry and back out of the stack   
                temperatureDtls[str(readingTime)] = tempData[dataIdx]
                #print('temperature reading:', str(readingTime),tempData[dataIdx])
                
                sowDateDtls[resultsKey] = temperatureDtls
                trtDtls[sowKey] = sowDateDtls
                Treatments[treatmentKey] = trtDtls

                #Treatments dictionary is now populated with one more temperature data point

            #endif data not NULL

        #end for loop for each data cell in current line
        #Treatments dictionary is now populated with one more row of temperature data

        if ln%1000 == 0 : print(ln, 'lines processed of Canopy Temperature')                      

    #end for loop for each data line
    #Treatments dictionary is now populated with all canopy temperature data
    print(ln, 'lines processed of Canopy Temperature. File End')
        
finally:
    CTFile.close

    #clean up temporary data structures
    ctLines = []
    temperatureDtls = {}  #empty temperature Details dictionary
    sowDateDtls = {}      #empty sowDateDtls dictionary
    trtDtls = {}          #empty treatment Dtls dictionary



# listed are some 'queries' that could be asked

print ('\n\n Quality MIC:' , Treatments['AU','2011','dry']['15/04/2011']['QSample1']['MIC'],\
                             Treatments['AU','2011','dry']['15/04/2011']['QSample2']['MIC'])
#print ('Count of Environmental Readings:',len(EnvironmentReadings.keys()))
