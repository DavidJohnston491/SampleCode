from time import sleep
from time import time
import datetime
import csv
import numpy as np

File1 = input('Enter Fibre Quality File: ')
FibreQuality = open(File1, 'r')
FQ = FibreQuality.read()
File2 = input('Enter Canopy Temperature File: ')
CanopyTemp = open(File2, 'r')
File3 = input('Enter Weather File: ')
Weather = open(File3, 'r')
print (FibreQuality)
