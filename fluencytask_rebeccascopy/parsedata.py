#!/usr/bin/python/Users/jcz/Dropbox/projects/snafu/snafu-py/fluency_task/lab_version

# * Excludes lists that had to be re-done (<5 items)
# * Lower case 
# * Remove spaces

import json
import os
import csv

datafiles=os.listdir('./logs/')
datafiles=[df for df in datafiles if "data" in df]

header=['id','game','category','itemnum','item','RT','RTstart']
fulldata=[]

for df in datafiles:
    subj=df.split('_')[0]
    with open('./logs/'+df) as json_data:
        data=json.load(json_data)
    for gamenum, game in enumerate(data):
        category=game["category"]
        for i, item in enumerate(game["items"]):
            rtstart=game["times"][i]-game["starttime"]
            if i==0:
                rt=rtstart
            else:
                rt=game["times"][i]-game["times"][i-1]
            cleanitem=item.lower().replace(" ","")
            line=[subj, gamenum+1, category, i+1, cleanitem, rt, rtstart]
            fulldata.append(line)

with open('results_cleaned.csv','w') as csvfile:
    w = csv.writer(csvfile, delimiter=',', quoting=csv.QUOTE_MINIMAL)
    w.writerow(header)
    for i in fulldata:
        w.writerow(i)


