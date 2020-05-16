
import os
import pandas as pd


# directory = os.fsencode("./experiment_results")

# for file in os.listdir(directory):
#      filename = os.fsdecode(file)
#      with open(os.path.join(os.cwd(), filename), 'r') as f:

res = {}

directory = "./experiment_results"
for filename in os.listdir(directory):
    name = os.path.splitext(filename)[0]
    print(name)
    file_results = []
    with open(os.path.join(directory, filename), 'r') as f: # open in readonly mode
        for line in f:
            if line[:5] == "Level":
                # print(line.split()[1])
                level = line.split()[1]
                file_results.append(int(level))
            elif line[:4] == "Iter":
                # print(line.split()[2])
                ite = line.split()[2]
                file_results.append(int(ite))
            elif line[:4] == "Time":
                tme = line.split()[2]
                file_results.append(float(tme))
        if len(file_results) != 0:
            res[name] = file_results
                # print(line.split()[2])
            # for word in line.split():
            #     if word == "Time":
            #         print(line)


df = pd.DataFrame.from_dict(res)
print(df)

