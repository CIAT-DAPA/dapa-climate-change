To translate climgen files into plain asciifiles (id, lon, lat, value) simply type this command

java -Xmx512m -jar Climgen.jar -a [climgenFilePath\climgenFileName.climgen] [outputPath]

for extracting the whole timeseries, and

java -Xmx512m -jar Climgen.jar -y [year] [climgenFilePath\climgenFileName.climgen] [outputPath]

for extracting an specific year from the file.