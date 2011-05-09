
alter table runsprojecting add column resolution varchar(5),add column region varchar(15);

mysql --skip-column-names -umodel1 -pmaxent -h192.168.20.224 -e "use tnc; show tables;" | grep "^la_*" | while read run
do
 sres=$(echo $run | cut -d'_' -f3)
 year=$(echo $run | cut -d'_' -f4,5)
 res="5000"
 area="latin america"
 gcm=$run

 psql -U model1 -d gisdb -c "INSERT INTO RunsProjecting (sres,year,gcm,notes,resolution,region) VALUES ('$sres','$year','$gcm','$run','$res','$area');"
 runId=$(psql -U model1 -d gisdb -t -c "select runpid from runsprojecting where gcm = '$gcm';")
 
 # export the old data 
 mysql --skip-column-names -umodel1 -pmaxent -h192.168.20.224 -e "use tnc; select * from $run where finished is not null" > $run
 
 # create temp table
 psql -U model1 -d gisdb -c "CREATE TABLE tmp1 (speciesid varchar(10),started timestamp,finished timestamp, exit varchar(100));"
 
 # import
 psql -U model1 -d gisdb -c "\copy tmp1 from $run delimiter as '\t'"

 # add model id
 psql -U model1 -d gisdb -c "alter table tmp1 add column model varchar(10);"
 psql -U model1 -d gisdb -c "create table tmp2 as select m.modelid as modelid,t.speciesid as speciesid,t.started as started,t.finished as finished,t.exit as exit from models m join tmp1 t on t.speciesid = m.speciesid;"

 psql -U model1 -d gisdb -c "alter table tmp2 add column runid INTEGER default '$runId'"
 
 # insert into final table
 psql -U model1 -d gisdb -c "insert into modelprojections (modelid,porjectionstarted,projectionfinished,runprojectid,mostrecent,exitstatus) Select modelid,started,finished,runid,'1',exit from tmp2;"
 
 # clean
 psql -U model1 -d gisdb -c "drop table tmp1; drop table tmp2"
done
