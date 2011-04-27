for gcm in A1B_2020_2049_csiro_mk3_0; 
do
   RESULTS_HOST="/home/jsigner/TNC/results";   
   RESULTS_SRV="/mnt/GIS-HD716/TNC_global_plants/results";      
   mkdir $RESULTS_SRV/$gcm;      
   for i in $(seq 1 8);   
   do 
        mkdir $RESULTS_SRV/$gcm/part.$i;
   done;    
   id=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc;select species_id from $gcm where finished is not null and exit_status is null limit 1;");   
   
   while [ -n "$id" ];   
   do        
      part=$(mysql --skip-column-names  -umodel1 -pmaxent -e"use tnc;select part from species where species_id=$id;");          
      
      mv $RESULTS_HOST/$gcm/part.$part/$id.zip $RESULTS_SRV/$gcm/part.$part/$id.zip;          
      
      mysql -umodel1 -pmaxent -e"use tnc;UPDATE $gcm SET exit_status='mv $RESULTS_SRV' where species_id=$id;";          

      sleep 1;          

      id=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc;select species_id from $gcm where finished is not null and exit_status is null limit 1;");   
      
      # wait 10 mins to see that really all files finished
      if [ -z "$id" ] 
      then
        sleep 600
        id=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc;select species_id from $gcm where finished is not null and exit_status is null limit 1;");   
      fi
      
   done; 
done

