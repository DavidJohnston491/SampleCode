import urllib
import requests
import argparse
import os
import sys
import datetime
import csv


import sys, getopt


class Metfile:
    def __init__(self):

        self.username = "david.johnston@usq.edu.au"
        self.password = ""
        self.url_base = "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php"
        self.download_folder = "/Users/Q2009013/PycharmProjects/HSG/sim_samples/"


metfile = Metfile()

def delete_existing_file(filename):
    try:
        os.remove(filename)
    except OSError:
        pass



def get_met(folder, site, stationName, start_date=None, end_date=None):

    if (start_date == None ):
        start_date = "19000101"

    if (end_date == None):
        #today = datetime.datetime.now()
        #future = today + datetime.timedelta(days=90)
        #yesterday = datetime.datetime.now() + datetime.timedelta(days=-1)
        # end_date = datetime.datetime.now().strftime("%Y%m%d")
        #end_date = yesterday.strftime("%Y%m%d")
        end_date = "20190630"
        #pass

    '''print "start date = {}".format(start_date)
    print "end date = {}".format(end_date)
    print "site no. = {}".format(site)'''

    username = metfile.username
    password = metfile.password
    url_base = metfile.url_base


    # build met file name.
    location = stationName
    #update the line below to name the file on disc
    met_name = os.path.join(folder, location + "_" + str(site) + ".met")

    # delete any existing file.
    delete_existing_file(met_name)



    # RKA Jan'19 new SILO API
    url = url_base + "?username=" + metfile.username
    if (metfile.password != ""):
        url = url + "&password=" + metfile.password
    url = url + "&start=" + start_date
    url = url + "&finish=" + end_date
    url = url + "&station=" + str(site)
    url = url + "&format=apsim"

    print (url)

    with requests.get(url, stream=True) as response:

        # Throw an error for bad status codes
        response.raise_for_status()

        print ("Writing " + met_name)

        with open(met_name, 'wb') as handle:
            for block in response.iter_content(1024):
                handle.write(block)


if __name__ == "__main__":
   #find python read csv file from web
   folderBase='C:\\Users\\u1097253\\Documents\\ARMonline\\NewCropsandSites\\AAClimateFiles'
   with open('metFile.csv') as csvfile:
       readCSV = csv.reader(csvfile, delimiter=',')
       for row in readCSV:
            station=row[0]
            stationID=row[1]
            get_met(folderBase, stationID.strip(), station)

   sys.exit



