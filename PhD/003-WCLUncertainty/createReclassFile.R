################################################################################
#get the command line arguments
args=(commandArgs(TRUE))

#evaluate the arguments
print(args)
for(i in 1:length(args)) {
 eval(parse(text=args[[i]]))
}

print(ar1)
print(ar2)

#R CMD BATCH --no-save --slave "--args ar1='aab'' ar2='ass'" createReclassFile.R rt.Rout