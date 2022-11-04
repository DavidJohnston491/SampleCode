# -*- coding: utf-8 -*-
# Windows Python 3.2.py

#Rename files test

import glob
import os

#subprocess.call("c:\program files (x86)\notepad++\notepad++.exe"

#
# Rename the agron.inp files ready for batch processing
import glob, os

##def rename(dir, pattern, replacedPattern):
##    for pathAndFilename in glob.iglob(os.path.join(dir, pattern)):
##        title, ext = os.path.splitext(os.path.basename(pathAndFilename))
##        newName = title.replace(pattern, replacedPattern)
##        os.rename(pathAndFilename, 
##                  os.path.join(dir, newName + ext))
##
##rename(r"./",r"AdjHighLow",r"AdjHigh")

for file in glob.iglob("DefIrrig_*.agron"):
    os.rename(file, file.replace("90", "50"))
              
              
##Python 3.2.2 (default, Sep  4 2011, 09:07:29) [MSC v.1500 64 bit (AMD64)] on win32
##Type "copyright", "credits" or "license()" for more information.
##>>> ================================ RESTART ================================
##>>> 
##>>> import os
##>>> os.chdir
##<built-in function chdir>
##>>> os.chdir(r"H:\Simulations\Rose\ETo studies\Inputs\Agronomy\Agron HighAdjs")
##
##>>> os.curdir
##'.'
##
##>>> os.getcwd()
##'H:\\Simulations\\Rose\\ETo studies\\Inputs\\Agronomy\\Agron HighAdjs'
##>>> for file in glob.iglob("*AdjHighLow.agron"):
##    os.rename(file, file.replace("AdjHighLow", "AdjHigh"))
##
##    
##>>> os.chdir(r"..\Agron LowAdjs")
##>>> for file in glob.iglob("*AdjHighLow.agron"):
##    os.rename(file, file.replace("AdjHighLow", "AdjLow"))

