#-----------------------------------------------------------------------------
# Purpose:      Promediar Archivos de Probabilidades de MarkSim
#
# Author:       Jefferson Valencia
#
# Created:      2012/02/10
#-----------------------------------------------------------------------------

import glob, os, sys

os.system('cls')

def funcion_current(pathFile1):

    Puntos = os.listdir(pathFile1)

    for Punto in Puntos:

        print "\n\t\t******* Punto: " + str(Punto) + " *******\n"        

        archivos = glob.glob (pathFile1 + "\\" + Punto + "\\*.WTG")

        if os.path.exists(pathFile1 + "\\" + Punto + "\\" + Punto + '_Ave.WTG'):
            os.system('del ' + pathFile1 + "\\" + Punto + "\\" + Punto + '_Ave.WTG')

        num_proba = len(archivos)

        outFile = open(pathFile1 + "\\" + Punto + "\\" + Punto + '_Ave.WTG', 'w')    
        Matriz_Sum = [[]*4]*365
        Matriz_Ave = [[0]*4]*365

        for archivo in archivos:

            Matriz_Datos = []
            Matriz_Enca = []

            print "\n\tArchivo: " + ((archivo.split("\\"))[-1:])[0] + "\n"

            inFile = open(archivo, 'r')

            Matriz_Enca = (inFile.read().split("\n"))[0:6]

            inFile.close()               

            inFile = open(archivo, 'r')
            
            Matriz_Datos = (inFile.readlines())[6:371]

            inFile.close()            

            Matriz_Fecha = []
            Matriz_Dato = []
            Matriz_Variables = [[]]

            i = 0
            for datos in Matriz_Datos:

                fecha = datos[0:5]
                srad = datos[7:11]
                tmax = datos[13:17]
                tmin = datos[19:23]
                rain = datos[25:29]

                Matriz_Dato = [float(srad),float(tmax),float(tmin),float(rain)]                
                Matriz_Fecha.append(fecha)
                Matriz_Variables.append(Matriz_Dato)

                i = i + 1

            del Matriz_Variables[0]               
            Matriz_Sum = Matriz_Ave
                        
            Matriz_Ave = [[Matriz_Sum[i][j]+Matriz_Variables[i][j] for j in range(4)] for i in range(365)] #Linea magica       
        
        for line in Matriz_Enca:
            outFile.write(line + "\n")

        j = 0
        for dato in Matriz_Ave:

            srad1 = str(round((Matriz_Ave[j][0])/num_proba,1))
            tmax1 = str(round((Matriz_Ave[j][1])/num_proba,1))
            tmin1 = str(round((Matriz_Ave[j][2])/num_proba,1))
            rain1 = str(round((Matriz_Ave[j][3])/num_proba,1))

            if len(srad1) == 4:
                s1 = "  "
            elif len(srad1) == 3:
                s1 = "   "
            
            if len(tmax1) == 4:
                s2 = "  "
            elif len(tmax1) == 3:
                s2 = "   "

            if len(tmin1) == 4:
                s3 = "  "
            elif len(tmin1) == 3:
                s3 = "   "
                
            if len(rain1) == 4:
                s4 = "  "
            elif len(rain1) == 3:
                s4 = "   "
                
            outFile.write(Matriz_Fecha[j] + s1 + srad1 + s2 + tmax1 + s3 + tmin1 + s4 + rain1 + "\n") 

            j = j + 1
                    
        outFile.close()


def funcion_gcms(pathFile1):

    Modelos = os.listdir(pathFile1)

    for Modelo in Modelos:    

        print "\n\t\t\t******* Modelo: " + str(Modelo) + " *******\n\n"

        Puntos = os.listdir(pathFile1 + "\\" + Modelo)

        for Punto in Puntos:

            print "\n\t\t******* Punto: " + str(Punto) + " *******\n"        

            archivos = glob.glob (pathFile1 + "\\" + Modelo + "\\" + Punto + "\\*.WTG")
            
            if os.path.exists(pathFile1 + "\\" + Modelo + "\\" + Punto + "\\" + Punto + '_Ave.WTG'):
                os.system('del ' + pathFile1 + "\\" + Modelo + "\\" + Punto + "\\" + Punto + '_Ave.WTG')

            num_proba = len(archivos)
            
            outFile = open(pathFile1 + "\\" + Modelo + "\\" + Punto + "\\" + Punto + '_Ave.WTG', 'w')    
            Matriz_Sum = [[]*4]*365
            Matriz_Ave = [[0]*4]*365

            for archivo in archivos:

                Matriz_Datos = []
                Matriz_Enca = []

                print "\n\tArchivo: " + ((archivo.split("\\"))[-1:])[0] + "\n"

                inFile = open(archivo, 'r')

                Matriz_Enca = (inFile.read().split("\n"))[0:6]

                inFile.close()               

                inFile = open(archivo, 'r')
                
                Matriz_Datos = (inFile.readlines())[6:371]

                inFile.close()            

                Matriz_Fecha = []
                Matriz_Dato = []
                Matriz_Variables = [[]]

                i = 0
                for datos in Matriz_Datos:

                    fecha = datos[0:5]
                    srad = datos[7:11]
                    tmax = datos[13:17]
                    tmin = datos[19:23]
                    rain = datos[25:29]

                    Matriz_Dato = [float(srad),float(tmax),float(tmin),float(rain)]                
                    Matriz_Fecha.append(fecha)
                    Matriz_Variables.append(Matriz_Dato)

                    i = i + 1

                del Matriz_Variables[0]               
                Matriz_Sum = Matriz_Ave
                            
                Matriz_Ave = [[Matriz_Sum[i][j]+Matriz_Variables[i][j] for j in range(4)] for i in range(365)] #Linea magica       
            
            for line in Matriz_Enca:
                outFile.write(line + "\n")

            j = 0
            for dato in Matriz_Ave:

                srad1 = str(round((Matriz_Ave[j][0])/num_proba,1))
                tmax1 = str(round((Matriz_Ave[j][1])/num_proba,1))
                tmin1 = str(round((Matriz_Ave[j][2])/num_proba,1))
                rain1 = str(round((Matriz_Ave[j][3])/num_proba,1))

                if len(srad1) == 4:
                    s1 = "  "
                elif len(srad1) == 3:
                    s1 = "   "
                
                if len(tmax1) == 4:
                    s2 = "  "
                elif len(tmax1) == 3:
                    s2 = "   "

                if len(tmin1) == 4:
                    s3 = "  "
                elif len(tmin1) == 3:
                    s3 = "   "
                    
                if len(rain1) == 4:
                    s4 = "  "
                elif len(rain1) == 3:
                    s4 = "   "
                    
                outFile.write(Matriz_Fecha[j] + s1 + srad1 + s2 + tmax1 + s3 + tmin1 + s4 + rain1 + "\n") 

                j = j + 1
                        
            outFile.close()


corrida = raw_input('\n\t\tUsted Correra Current o GCMs? (1: Current, 2: GCMs): ')
pathFile = raw_input('\n\t\t\tDefina el Path: ')

if corrida == '1':
    funcion_current(pathFile)

elif corrida == '2':
    funcion_gcms(pathFile)