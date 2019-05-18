coutns_file = open('C:\\Users\\Round\\Downloads\\counts.txt', 'r')
non_zero_counts = open('C:\\Users\\Round\\Downloads\\counts_processed.txt', 'w')

for line in coutns_file:
	if len(line.split()) > 1 and int(line.split()[1]) != 0:
		non_zero_counts.write(line)


