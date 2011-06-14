import os, sys

os.system('cls')

ConcatenaStations = open('ConcatenaStations.txt', 'w')
ConcatenaStations.close()

Stations = open('Stations.txt', 'r')
ListStation = Stations.readlines()

years = ""

for LineStation in ListStation:

    print "> Buscando estacion " + LineStation[:-1] + " en StationsYears.txt"
    ConcatenaStations = open('ConcatenaStations.txt', 'a')
    ConcatenaStations.writelines(LineStation)
    ConcatenaStations.close()

    
    StationYears = open('StationsYears.txt', 'r')
    ListStationYears = StationYears.readlines()

    for LineStationYear in ListStationYears:

       if  LineStationYear.find(LineStation[:-1]) >= 0:
          years = years + LineStationYear[:-1]
          print LineStationYear[:-1]
          
          ConcatenaStations = open('ConcatenaStations.txt', 'a')
          ConcatenaStations.writelines('*' + LineStationYear.split('\t')[1])
          ConcatenaStations.close()

    StationYears.close()

Stations.close()
