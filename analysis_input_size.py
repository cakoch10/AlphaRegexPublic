
import os
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# variables for experiment setup

# operation should be set to "plus", "star", or "times"
operation = "same"

directory = "./experiment_input_size"
directory2 = "./input_size_benchmarks"

# start by getting original regexes
orig_regex = {}

files = os.listdir('./input_size_benchmarks')
for filename in files:
    name = os.path.splitext(filename)[0][:4]
    if "_" in name:
        name = name[:3]
    if not name in orig_regex:
        with open(os.path.join(directory2, filename), 'r') as f:
            for line in f:
                if line[:5] == "input":
                    lines = line.split()
                    orig_regex[name] = lines[2]
                    break


# print(orig_regex)


def get_data(directory):
    res = {}
    files = os.listdir(directory)
    files.sort()
    for filename in files:
        name = os.path.splitext(filename)[0][:4]
        if name[0] == 'n' and os.path.splitext(filename)[0][-1] != '0' and name != 'no9_':
            if "_" in name:
                name = name[:3]
            # print(name)
            if not name in res:
                res[name] = []
            with open(os.path.join(directory, filename), 'r') as f: # open in readonly mode
                for line in f:
                    if line[:4] == "Time":
                        tme = line.split()[2]
                        res[name].append(float(tme))
                        break
    return res

def get_data_regex(directory):
    res = {}
    files = os.listdir(directory)
    files.sort()
    for filename in files:
        name = os.path.splitext(filename)[0][:4]
        if name[0] == 'n' and os.path.splitext(filename)[0][-1] != '0' and name != 'no9_':
            if "_" in name:
                name = name[:3]
            # print(name)
            if not name in res:
                res[name] = []
            with open(os.path.join(directory, filename), 'r') as f: # open in readonly mode
                last_line = ''
                for line in f:
                    if line[:5] == "Level":
                        # print(line.split()[1])
                        res[name].append(last_line.strip())
                        break
                    last_line = line
    return res

res = get_data_regex(directory)

for name in res:
    res[name].append(orig_regex[name])

df = pd.DataFrame.from_dict(res, orient='index', columns=['one','half','eigth','sixteenth','thirtieth', 'original'])
# df = pd.DataFrame.from_dict(res)
# df = df.rename(index = {0:'one',1:'half',2:'eigth',3:'sixteenth',4:'thirtieth'})

# lines = df.plot.line()

# lines.set_xlabel("Input Size")
# lines.set_ylabel("Time (sec)")

# plt.tight_layout()
# plt.legend(prop={'size':5})

# plt.savefig("input_size_plot_without_3.png", dpi=1200)

# with open('raw_data_input_size_regexes.txt', "w") as f:
#     f.write(df.to_csv(sep='\t'))


print("Raw data from directory " + directory + "\n")

print(df)
