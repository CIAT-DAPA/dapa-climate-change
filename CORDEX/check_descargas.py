#####################################V . 2 OK
#Autor: Jaime Tarapues
#Fecha: Noviembre 2012
#Funcion: Registro de los prediodos de las variable, emsembles y modelos
####################################
import os, sys, glob, string, shutil
#python F:\jetarapues\_scripts\check_descargas.py U:\rcm\cordex temp


dirbase= sys.argv[1]
exp = sys.argv[2]
# remove_t = sys.argv[2]
# dirout = sys.argv[2]

var = ['pr','tas','tasmin','tasmax']#['pr','hur','rsds','sfcWind','tas','tasmin','tasmax']
emsemble = ['r1i1p1','r2i1p1','r3i1p1','r4i1p1','r5i1p1','r6i1p1','r7i1p1','r8i1p1','r9i1p1','r10i1p1', 'r11i1p1', 'r12i1p1']

init = []
fin = []
lis_var=[]
emsemb =[] # lista todas las variales de todos los emsembles

# modellist = [name for name in os.listdir(dirbase) if os.path.isdir(os.path.join(dirbase, name)) ] # lista unicamente las carpetas
if not os.path.isdir(dirbase + "\\" + exp):
	os.system('mkdir '+dirbase + "\\" + exp)
	

for root, dirs, files in os.walk(dirbase):
    for name in files:
        if name.endswith((".nc", ".nc")):
			region=name.split("_")[1]
			time= name.split("_")[7]
			scenario=name.split("_")[3]		
			model=name.split("_")[5] 
			ens=name.split("_")[4]
			var=name.split("_")[0]			
			period=name.split("_")[-1]	
			anos = period.split("-")[0][:4]
			anos_f = period.split("-")[1][:4]
			file= dirbase+'\\'+exp+'\\'+ region+'_'+scenario +'_'+ time +'_'+model +'_'+ ens +'_'+ var +'_prueba.t'
			####### if not os.path.isfile(file):			
			print anos			
			outFile = open(file, 'a')
			outFile.write(region+'/'+scenario+'/'+time +'/'+model +'/'+ ens +'/'+ var  +  "/" + anos +"-"+anos_f+ '\n')
			outFile.close()

dirbase=dirbase + "\\" + exp				
os.chdir(dirbase)
files = glob.glob("*.t")
for file_input in files:
	l=open(dirbase +'\\'+ file_input)
	lines = [i for i in l.readlines()]
	indices = [item for item in range(len(lines)) if lines[item] == '\n']
	list = [i for j, i in enumerate(lines) if j not in indices]
	outFile = open(dirbase+'\\'+ exp +'_registro_descargas_cmip5.txt', 'a') # crea archivo por emsemble
	outFile.write(min(list).split("/")[0] + "\t" + min(list).split("/")[1]+ "\t" + min(list).split("/")[2]+ "\t" + min(list).split("/")[3]+ "\t" + min(list).split("/")[4]+ "\t" + min(list).split("/")[5]+ "\t" + min(list).split("/")[6].split("-")[0]+"-"+max(list).split("/")[6].split("-")[1])
	outFile.close()				
				
				
# for root, dirs, files in os.walk(dirbase):
    # for name in files:
        # if name.endswith((".nc", ".nc")):			
			# a = name.split("_")[2] model
			# b = name.split("_")[4] ens
			# c = name.split("_")[0] var
			# anos = (name.split("_")[5]).split("-")[0][:4]
			# anos_f = (name.split("_")[5]).split("-")[1][:4]
			# outFile = open(dirbase+'\\'+ a +'_'+ b +'_'+ c +'_prueba.t', 'a')
			# outFile.write(a +'/'+ b +'/'+ c  +  "/" + anos +"-"+anos_f+ '\n')
			# outFile.close()


# os.chdir(dirbase)
# files = glob.glob("*.t")
# for file_input in files:
	# l=open(dirbase +'\\'+ file_input)
	# lines = [i for i in l.readlines()]
	# indices = [item for item in range(len(lines)) if lines[item] == '\n']
	# list = [i for j, i in enumerate(lines) if j not in indices]
	# outFile = open(dirbase+'\\'+ exp +'_registro_descargas_cmip5.txt', 'a') # crea archivo por emsemble
	# outFile.write(min(list).split("/")[0] + "\t" + min(list).split("/")[1]+ "\t" + min(list).split("/")[2]+ "\t" + min(list).split("/")[3].split("-")[0]+"-"+max(list).split("/")[3].split("-")[1])

	# outFile.close()


os.chdir(dirbase)
files = glob.glob("*.t")
for r in files:
    os.remove(r)

print 'done!!!'


