def mod_file():
	import string
	file_name = 'GCF_000001635.26_GRCm38.p6_genomic.gff'
	file_path = "C:\\Users\\Round\\Desktop\\"

	gff_file = open(file_path + file_name, 'r')

	output_file = open(file_path + "mouse_annot.gff", 'w')

	for line in gff_file:
		if line.startswith('N'):
			line = line.replace('ID', 'gene_id', 1)

		output_file.write(line)

	gff_file.close()
	output_file.close()

def read_file():
	file_name = 'mouse_annot.gff'
	file_path = "C:\\Users\\Round\\Desktop\\"

	gff_file = open(file_path + file_name, 'r')

	i = 0
	for line in gff_file:
		print(line)

		if i == 100:
			break
		i += 1

def check_file():
	file_name = 'mouse_annot.gff'
	file_path = "C:\\Users\\Round\\Desktop\\"

	gff_file = open(file_path + file_name, 'r')

	i = 0
	for line in gff_file:
		if '\tID=' in line and len(line) != 0:
			print(line)

def read_fasta():
	file_name = 'GCF_000001635.26_GRCm38.p6_genomic.fna'
	file_path = "C:\\Users\\Round\\Desktop\\"

	gff_file = open(file_path + file_name, 'r')

	i = 0
	for line in gff_file:
		if '>' in line and len(line) != 0:
			print(line)

mod_file()
#read_file()
check_file()

#read_fasta()

def compare():
	file_name = 'gff.txt'
	file_path = "C:\\Users\\Round\\Desktop\\"

	gff_file = open(file_path + file_name, 'r')
	fasta = open(file_path + 'fasta', 'r')

	gff_array = []
	fasta_array = []

	for line in gff_file:
		l = line.split()
		print(l)
		if 'sequence' in line:
			gff_array.append([l[1]])

	for line in fasta:
		l = line.split()
		if len(l) != 0:
			fasta_array.append([l[0][1:]])

	print(len(gff_array))
	print(len(fasta_array))

	for element in gff_array:
		if element not in fasta_array:
			print(element)

#compare()