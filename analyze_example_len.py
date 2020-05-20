import os


# want to extract regexes from current benchmarks
directory = "./experiment_original"

res = {}
files = os.listdir(directory)
files.sort()
for filename in files:
    name = os.path.splitext(filename)[0][:4]
    if name[0] == 'n':
        if "_" in name:
            name = name[:3]
        # print(name)
        file_results = ""
        with open(os.path.join(directory, filename), 'r') as f: # open in readonly mode
            last_line = ""
            for line in f:
                if line[:5] == "Level":
                    # print(line.split()[1])
                    file_results = last_line.strip()
                last_line = line
            if len(file_results) != 0:
                # need to replace + with | to work with re library
                res[name] = file_results.replace('+','|')