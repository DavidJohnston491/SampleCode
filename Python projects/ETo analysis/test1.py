# -*- coding: utf-8 -*-
#linuxversion.py

#Have user input version and print response

f = open("EToAdjustments.sum", "r")
n = 0
try:
    for line in f:
        n += 1
        if n < 100000:
           #print line
           pass
        else:
            print n ,"  lines processed"
            break
finally:
    f.close()
    print "File is ", n, "  lines long."
    
with open('EToAdjustments.sum', 'r') as f:
    data = f.readlines()

    #for line in data:
        #words = line.split()
        #print words

    print 'The read-in array has ', len(data), ' lines.'
    #f.close()
    print data[1000]
    print data[20000]
    print data[40000]
    print data[60000]

   # f.close
    
    
