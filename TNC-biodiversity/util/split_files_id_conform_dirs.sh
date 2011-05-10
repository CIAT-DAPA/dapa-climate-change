function rearrange {

h=$1

#pid=$(psql -U model1 -d gisdb -t -c "select runpid from runsprojecting where notes = '$h'")
#newdir=proj$(echo ${pid} | sed 's/ //g')
 
 newdir=splt\_$h
  
  if [ -e "$newdir" ]
  then
    echo "everyhting is here already"
  else
    mkdir $newdir

    for i in 1 2 3 4 5 6 7 8; 
    do  
      for j in $h/part.$i/*;  
      do
         id=$(echo $j | cut -f3 -d/);   
         nf=$(echo ${id:0:4});   
         if [ -e "$newdir/$nf" ];   
         then    
            cp $j $newdir/$nf;   
         else    
            mkdir $newdir/$nf;    
            cp $j $newdir/$nf;   
       fi;
      done; 
    done
  fi
}


for h in la_*
do
  rearrange $1 &
done
