# -*- coding: utf-8 -*-
#Python 3.2.2 (default, Sep  4 2011, 09:07:29)
#[MSC v.1500 64 bit (AMD64)] on win32

#ETo simulation Yield files summary analysis


import os
import glob

        #os.getcwd()
        #os.listdir("c:\\Python32\\UserProjects\\ETo analysis")


        #glob.glob("c:\\Python32\\UserProjects\\ETo analysis\\*_Y.out")
        #cwd = os.getcwd()
        #searchpath = cwd + '\\*.py'

fileList = glob.glob('*_Y.out')
print (fileList)
for i in fileList:
    try:
        f = open(i,"r")
        #results = {}  #empty results dictionary
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
        
            # print (dl[3], n, totalYield, round(totalYield/n,1))
       
        finally:
            f.close
            print (i,' Avg Yld:', round(totalYield/n,1),\
                   'Avg WUE:', round(totalWUE/n,1),\
                   'Total Irrigs:', totalIrrig)

    finally:
        pass

    
    
    
