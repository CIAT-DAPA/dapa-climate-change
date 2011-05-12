# redo models

mysql -umodel1 -pmaxent -h192.168.20.224 -e "use tnc; alter table species add column size integer;"

while read line
do
 id=$(echo $line | cut -f1 -d" ")
 size=$(echo $line | cut -f2 -d" ")

  mysql -umodel1 -pmaxent -h192.168.20.224 -e "use tnc; update species set size=$size where species_id='$id';"

done < id_size.txt

  
# copy training files

mysql -umodel1 --skip-column-names -pmaxent -h192.168.20.224 -e "use tnc; select species_id from species where la=1 and size < 10 " | while read line


outf="/mnt/GIS-HD716/TNC_global_plants/results/training"
for i in 1 2 3 4 5 6 7 8
do
 for j in part.$i/*
 do
  id=$(echo $j | cut -f2 -d/)
  nf=$(echo ${id:0:4})
  if [ -e "$outf/$nf" ]
  then
   mv -v $j/training/bg.csv $outf/$nf/$id/bg.swd
   mv -v $j/training/sp_swd.csv $outf/$nf/$id/sp.swd;
  else
   mkdir $outf/$nf;
   mv -v $j/training/bg.csv $outf/$nf/$id/bg.swd
   mv -v $j/training/sp_swd.csv $outf/$nf/$id/sp.swd
  fi
 done
done