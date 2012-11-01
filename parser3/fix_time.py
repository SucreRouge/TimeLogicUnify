import sys 
line = sys.stdin.readline()
#for line in sys.stdin:
while line:
	split=line.split()
	split[7]=str(int(split[7])/4/1024)
	print ' '.join(split)

	sys.stdout.flush()
	line = sys.stdin.readline()
