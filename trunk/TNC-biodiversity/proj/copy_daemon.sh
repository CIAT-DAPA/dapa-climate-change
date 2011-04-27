for gcm in A1B_2020_2049_csiro_mk3_0; 
do
   FROMDIR="/home/jsigner/TNC/results";   
   TODIR="/mnt/GIS-HD716/TNC_global_plants/results";      
   mkdir $TODIR/$gcm;      
   for i in $(seq 1 8);   
   do 
        mkdir $TODIR/$gcm/part.$i;
   done;    
   id=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc;select species_id from $gcm where finished is not null and exit_status is null limit 1;");   
   while [ -n "$id" ];   
   do        
      part=$(mysql --skip-column-names  -umodel1 -pmaxent -e"use tnc;select part from species where species_id=$id;");          
      
      mv $FROMDIR/$gcm/part.$part/$id.zip $TODIR/$gcm/part.$part/$id.zip;          
      
      mysql -umodel1 -pmaxent -e"use tnc;UPDATE $gcm SET exit_status='mv $TODIR' where species_id=$id;";          

      sleep 1;          

      id=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc;select species_id from $gcm where finished is not null and exit_status is null limit 1;");   
      done; 
done

