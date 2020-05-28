import os
import re
import exrex
import itertools
import random

# want to extract regexes from current benchmarks
directory = "./experiment_original"


def generate_ar_input(pos_lst, neg_lst, description):
    string_pos = '\n'.join(pos_lst)
    string_neg = '\n'.join(neg_lst)
    return description+'\n++\n'+string_pos+'\n--\n'+string_neg

# enumerate all binary strings up to length 9
binary_strings = ["".join(seq) for i in range(1,10) for seq in itertools.product("01", repeat=i)]

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
                regexes = {}
                regexes["original"] = name
                regexes["regex"] = file_results
                regexes["formatted"] = "^(" + file_results.replace('+','|').replace('?','{0,1}') + ")$"
                res[name] = regexes

# res is a dictionary mapping benchmark name to corresponding regex

for dataset in res:
    regex_str = res[dataset]["formatted"]
    lim = 1
    pos_lst = list(exrex.generate(regex_str,limit=lim)) # generate positive examples
    while len(pos_lst) < 500:
        lim = lim + 1
        pos_lst = list(exrex.generate(regex_str,limit=lim))
    pos_lst = [p for p in pos_lst if len(p) < 20]
    pos_lst = pos_lst[:500]
    # pos_lst = sorted(pos_lst, key=len)
    # pos_lst = pos_lst[:250]
    neg_lst = []
    for word in binary_strings:
        if not re.match(regex_str, word):
            neg_lst.append(word)
    neg_lst = neg_lst[:500]
    print(str(len(pos_lst[-1])))
    # need to generate subsets
    pn = len(pos_lst)
    nn = len(neg_lst)
    half_pos = random.sample(pos_lst,int(pn/2))
    quarter_pos = random.sample(half_pos,int(pn/4))
    six_pos = random.sample(quarter_pos,int(pn/16))
    thirty_pos = random.sample(six_pos,int(pn/32))
    sixty_pos = random.sample(thirty_pos,int(pn/64))

    pos_sample = [pos_lst,half_pos,quarter_pos,six_pos,thirty_pos,sixty_pos]
    
    print(res[dataset]["original"])
    print(pn)

    half_neg = random.sample(neg_lst,int(nn/2))
    quarter_neg = random.sample(half_neg,int(nn/4))
    six_neg = random.sample(quarter_neg,int(nn/16))
    thirty_neg = random.sample(six_neg,int(nn/32))
    sixty_neg = random.sample(thirty_neg,int(nn/64))

    neg_sample = [neg_lst,half_neg,quarter_neg,six_neg,thirty_neg,sixty_neg]

    for i in range(0,len(pos_sample)):
        p = pos_sample[i]
        n = neg_sample[i]
        description = "input for " + res[dataset]["regex"] + " with " + str(len(p)) 
        description += " positive examples and " + str(len(n)) + " negative examples"
        f_str = generate_ar_input(p, n, description)
        # write f_str to file
        f_name = "input_size_benchmarks/" + res[dataset]["original"] + "_" + str(i)
        with open(f_name, "w") as f:
            f.write(f_str)
        print("Wrote to " + f_name)



    