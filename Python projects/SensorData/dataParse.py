# -*- coding: utf-8 -*-
# Windows Python 3.2.py

#Parse Sensor Data files  (Tony Nadelko : May 2014)
#   - change date to Australian format
#   - eliminate excess header lines
#   - combine source files to a maximum of 5000 lines



import glob
import os

#---------------------------------------------------
def fileLength(fname):
    i = 0
    try:
        with open(fname) as f:
            for i, l in enumerate(f):
                pass

    except IOError:
        i = 0
        
    return i + 1
#---------------------------------------------------

#---------------------------------------------------
def incSeqNumber(unitID):
    sf = open('seqControl.txt', 'r+')
    lines = sf.readlines()
    fileECDSeq = str(lines[0][-4:-1])
    fileFIDSeq = str(lines[1][-3:])
    # update seq and create new file
    if unitID == 'ECD' : fileECDSeq = str(int(fileECDSeq) + 1).zfill(3)
    if unitID == 'FID' : fileFIDSeq = str(int(fileFIDSeq) + 1).zfill(3)
    #print ('fileECDSeq:',str(fileECDSeq),'fileFIDSeq:',str(fileFIDSeq))
    sf = open('seqControl.txt', 'r+')
    seqlines = ['ECD Sequence: ' + str(fileECDSeq), \
                'FID Sequence: ' + str(fileFIDSeq) \
               ]
    sf.write('\n'.join(seqlines))
    sf.close()
            
#---------------------------------------------------
#---------------------------------------------------
def getSeqNumber(unitID):
    try:
        sf = open('seqControl.txt', 'r+')
        lines = sf.readlines()
        if len(lines) < 1:
            seqlines = ['ECD Sequence: 001', 'FID Sequence: 001']
            sf.write('\n'.join(seqlines))
            ECDSeq = '001'
            FIDSeq = '001'
        else:
            ECDSeq = str(lines[0][-4:-1])
            FIDSeq = str(lines[1][-3:])
            #print ('ECDSeq:',str(ECDSeq),'FIDSeq:',str(FIDSeq))
            
    except IOError:
        print ("Warning: Sequence file not found.  Created")
        sf = open('seqControl.txt', 'w')
        seqlines = ['ECD Sequence: 001', 'FID Sequence: 001']
        sf.write('\n'.join(seqlines))
        ECDSeq = '001'
        FIDSeq = '001'
        sf.close()
        
    else:
       sf.close()

    # increment the file value
    incSeqNumber(unitID)
    
    if unitID == 'ECD':
        return ECDSeq
    else:
        return FIDSeq
#---------------------------------------------------

# source file data has 60 ordered lines with specific codes
# null values should be entered if a line(s) is missing
lineCodes = ['N2O_CAL_01','N2O_C1_S1','N2O_C2_S1','N2O_C3_S1',\
             'N2O_C4_S1','N2O_CAL_02','N2O_C1_S2','N2O_C2_S2',\
             'N2O_C3_S2','N2O_C4_S2','N2O_CAL_03','N2O_C1_S3',\
             'N2O_C2_S3','N2O_C3_S3','N2O_C4_S3','N2O_CAL_04',\
             'N2O_C1_S4','N2O_C2_S4','N2O_C3_S4','N2O_C4_S4', \
             'N2O_CAL_05','N2O_C5_S1','N2O_C6_S1','N2O_C7_S1',\
             'N2O_C8_S1','N2O_CAL_06','N2O_C5_S2','N2O_C6_S2',\
             'N2O_C7_S2','N2O_C8_S2','N2O_CAL_07','N2O_C5_S3',\
             'N2O_C6_S3','N2O_C7_S3','N2O_C8_S3','N2O_CAL_08',\
             'N2O_C5_S4','N2O_C6_S4','N2O_C7_S4','N2O_C8_S4', \
             'N2O_CAL_09','N2O_C9_S1','N2O_C10_S1','N2O_C11_S1',\
             'N2O_C12_S1','N2O_CAL_10','N2O_C9_S2','N2O_C10_S2',\
             'N2O_C11_S2','N2O_C12_S2','N2O_CAL_11','N2O_C9_S3',\
             'N2O_C10_S3','N2O_C11_S3','N2O_C12_S3','N2O_CAL_12',\
             'N2O_C9_S4','N2O_C10_S4','N2O_C11_S4','N2O_C12_S4' \
            ]

ECDSeq = '000'
FIDSeq = '000'
    
# Process files of the form 'ECD_Winter2014_nnn.RES'
#                      and  'FID_Winter2014_nnn.RES'
#  'ECD_*' first
#
for unitID in ['ECD', 'FID']:
    unitIDSeq = '000'    
    dfLines = []
    for datafile in glob.iglob(unitID + "_*.RES"):
        #print (file)
        df = open(datafile,'r')
        dfLines = df.readlines()
        df.close()

        header1 = ('"' + dfLines[0].strip('\n') + '   ' + dfLines[16][:-1] + '"\n')
        #print ('header1:',header1)
        filedate = dfLines[5].split('/')
        header2 = (filedate[1] + '/' + filedate[0] + '/' + filedate[2])
        #print ('header2:', header2)

        #--------------------------------------------
        #Check the ouput file, that it exists and that it is not > 5000 lines

        # Read Sequence Control file for next file number if not yet read
        if unitIDSeq == '000':
            unitIDSeq = getSeqNumber(unitID)
        outputFile = (datafile[0:-7] + unitIDSeq + '.csv')
        print (outputFile)
        outfLen = fileLength(outputFile) #output file open and closed
        if outfLen > 5000:               #limit output .csv to 5000 lines
            # get next seq and create new output file
            unitIDSeq = getSeqNumber(unitID)
            outputFile = (datafile[0:-7] + unitIDSeq + '.csv')
            outfLen = 0
            print ('revised outfile:', outputFile)

        outf = open(outputFile,'a')
        if outfLen < 10:              
            #write blank header lines for a new file
            outf.write('\n' * 10)

        # write the default set of lines for the outfile
        outf.write('\n' * 5)
        outf.write(header1)
        outf.write(header2)
        outf.write('\n' * 5)

        n = 0
        for dataLine in dfLines[21:] :
            # ? check data line number and codes?
            if n > 59: break
            while (lineCodes[n] not in dataLine) :
                print ('Error in line Data:', dataLine, 'lineCode:',lineCodes[n])
                missingDataLine = '"' + lineCodes[n] + '", , \n'
                outf.write(missingDataLine)
                n += 1
                if n > 59: break
            outf.write(dataLine)
            n += 1

        if n != 60:
            print ('Error in line count in',datafile)
            while (n<60):
                missingDataLine = '"' + lineCodes[n] + '", , \n'
                outf.write(missingDataLine)
                n += 1
                
        outf.close()
       
        # move source file to 'processed' subfolder
        if not os.path.exists(r'./processed'):
            os.makedirs(r'./processed')
        os.rename(datafile, './processed/' + datafile)
        
    
                   
    
    
              
