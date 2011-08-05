#!/bin/env python
import psycopg2



# Create a list of species (gbif species id) to be consiered in a run
# species from which training should be projected?
MODEL_RUN_ID=2

# what is the auc threshold?
AUC_THRESHOLD=0.7

# where to save projected distributions

# which scenarion should be projected? 
sres=['A1B', 'A2']
years=['2030', '2050']
gcms=['avg']

# create list to be projected
# c_2000 project the current first
PROJ_QUEUE=["current_2000_avg"]

for i in sres:
  for j in years:
    for k in gcms:
      PROJ_QUEUE.append('_'.join([i,j,k]))
      

# Connect to database
conn = psycopg2.connect("dbname=gisdb user=model1 gist=192.168.229")
cur = conn.cursor()

# more information on the projection run
resolution="5k"
region="latin_america"

# ------------------ NO CHANGE BELOW --------------------------------------- #


# create entry for porj run
for i in PROJ_QUEUE:
  sres=i.split("-")[0]
  year=i.split("-")[1]
  gcm=i.split("-")[2]


  qs = "INSERT INTO runsprojecting (sres,year,gcm,resolution,region) VALUES ('" + sres  + "', '" + year + "', ' " + gcm + "','" + resolution + "', '" + region + "')"
  cur.execute(qs)
  conn.commit()
  
  
  # get proj id
  qs = "SELECT runpid from runsprojecting where sres='" + sres + "' AND year='" + year + "' AND gcm='" + gcm + "' AND resolution='" + resolution + "' AND region='" + region + "'"
  cur.execute(qs)
  ID = qs.fetchone()

  # Add all the species that should be projected into modelprojections
 
  
  qs = "SELECT speciesid FROM models where runtrainingid='" + MODEL_RUN_ID + "' AND auc > '" + AUC_THRESHOLD + "' AND issuccessfull='t'"
  cur.execute(qs)

  IDs = cur.fetchall()
 
  for ID in IDs:
    # get model id
    qs = "SELECT modelid FROM models WHERE runtrainingid='" + MODEL_RUN_ID + "' AND speciesid+'" + ID + "'"
    cur.execute(qs) 
    M_ID = cur.fetchone()

    # insert into table
    qs = "INSERT INTO modelprojections (modelid, runprojectid) VALUES ('" + M_ID + "', '" + PROJ_ID + "')"
    cur.execute(qs)
    conn.commit()















