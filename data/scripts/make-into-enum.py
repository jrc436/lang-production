f = file("/work/research/actup-production/data/types.txt", "r")
fw = file("/work/research/actup-production/data/types-enum.txt", "w")
lines = f.readlines()
lines_to_write = []
for line in lines:
	newline = '"'+line.strip()+'",\n'
	lines_to_write.append(newline)
fw.writelines(lines_to_write)
