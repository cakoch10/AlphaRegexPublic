
import os
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# variables for experiment setup

# operation should be set to "plus", "star", or "times"
operation = "same"

directory1 = "./experiment_" + operation
directory2 = "./experiment_original"


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
                last_line = ""
                for line in f:
                    if line[:5] == "Level":
                        # print(line.split()[1])
                        file_results.append(last_line.strip())
                        level = line.split()[1]
                        file_results.append(int(level))
                    elif line[:4] == "Iter":
                        # print(line.split()[2])
                        ite = line.split()[2]
                        file_results.append(int(ite))
                    elif line[:4] == "Time":
                        tme = line.split()[2]
                        file_results.append(float(tme))
                    last_line = line
                if len(file_results) != 0 and name != "no24":
                    res[name] = file_results
                        # print(line.split()[2])
                    # for word in line.split():
                    #     if word == "Time":
                    #         print(line)
    return res


res = get_data(directory1)
df = pd.DataFrame.from_dict(res, orient='index', columns=['Regex','Level', 'Iter', 'Time'])

print("Raw data from directory " + directory1 + "\n")

print(df)
index = df.index
# print(list(index))
times1 = df['Time']
# print(times)

print("\n---------------------------------------------\n")

# print(times1)

res2 = get_data(directory2)
df2 = pd.DataFrame.from_dict(res2, orient='index', columns=['Regex','Level', 'Iter', 'Time'])
# df = df.rename(index=['Level', 'Iter', 'Time'])
print("Raw data from directory " + directory2 + "\n")
print(df2)

times2 = df2['Time']

bar_plot_name = "Penalize " + operation

dfplot = pd.DataFrame({bar_plot_name:times1,"Original cost":times2})
ax = dfplot.plot.bar()
ax.set_xlabel("Dataset")
ax.set_ylabel("Time")

plt.tight_layout()
plt.savefig("Graphs/time_penalize_without_24_"+operation+".png", dpi=1200)


dfplot = pd.DataFrame({bar_plot_name:df['Iter'],"Original cost":df2['Iter']})
ax = dfplot.plot.bar()
ax.set_xlabel("Dataset")
ax.set_ylabel("Iter")

plt.tight_layout()
plt.savefig("Graphs/iter_penalize_without_24_" + operation + ".png", dpi=1200)



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


