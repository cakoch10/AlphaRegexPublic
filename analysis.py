
import os
import pandas as pd

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt



# directory = os.fsencode("./experiment_results")

# for file in os.listdir(directory):
#      filename = os.fsdecode(file)
#      with open(os.path.join(os.cwd(), filename), 'r') as f:

res = {}

directory1 = "./experiment_results"
directory2 = "./benchmarks"

def get_data(directory):
    res = {}
    files = os.listdir(directory)
    files.sort()
    for filename in files:
        name = os.path.splitext(filename)[0][:4]
        if name[0] == 'n':
            if "_" in name:
                name = name[:3]
            # print(name)
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
    return res


res = get_data(directory1)
df = pd.DataFrame.from_dict(res, orient='index', columns=['Level', 'Iter', 'Time'])

# print(df)
index = df.index
# print(list(index))
times1 = df['Time']
# print(times)

print(times1)

res2 = get_data(directory2)
df2 = pd.DataFrame.from_dict(res, orient='index', columns=['Level', 'Iter', 'Time'])
# df = df.rename(index=['Level', 'Iter', 'Time'])
# print(df2)

times2 = df2['Time']
print(times2)

dfplot = pd.DataFrame({"Without Depth":times1,"With Depth":times2})
ax = dfplot.plot.bar(rot=0)
ax.set_xlabel("Dataset")
ax.set_ylabel("Time")
plt.savefig("time.png")


df['Key'] = 'WithoutDepth'
df2['Key'] = 'WithDepth'



DF = pd.concat([df,df2],keys=['WithoutDepth','WithDepth'])

# print(DF)

# ax = df.plot(kind='bar',y='Time')
# df2.plot(kind='bar',y='Time',ax=ax)

# plt.tight_layout()
# plt.savefig('time.png')
# ax = df.plot.bar(x='Dataset', y='Time',rot=0)
# print(df)
# df.plot.bar()


