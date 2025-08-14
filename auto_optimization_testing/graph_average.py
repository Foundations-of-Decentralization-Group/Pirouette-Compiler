# Code from https://matplotlib.org/stable/gallery/lines_bars_and_markers/barchart.html#sphx-glr-gallery-lines-bars-and-markers-barchart-py

import sys
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# === Begin Config ===

num_args = len(sys.argv)

if num_args == 0:
    print("Error, must include output dir and at least one csv file!")
    exit(-1)

files = sys.argv[1:]

title = 'Average for execution times - 15 Runs - 3*10^4 Iterations - 30,31,32 Participants'
ylabel = 'Time in seconds (s)'

# === End Config ===

df = pd.concat([pd.read_csv(f, sep=',') for f in files], ignore_index=True, sort=False)

Experiments = df["test_name"].unique()
Types = df["prog_type"].unique()

# for e in Experiments:
#     temp = df.loc[df["test_name"] == e]
#     for t in Types:
#         print(temp.loc[df["prog_type"] == t])
#         print("------")

experiment_means = {}
experiment_variances = {}
for t in Types:
    experiment_means[t + " Avg"] = []
    experiment_variances[t + " Variance"] = []
for t in Types:
    df_by_type = df.loc[df["prog_type"] == t]
    for e in Experiments:
        df_by_type_exp = df_by_type.loc[df["test_name"] == e]
        mean = df_by_type_exp["time"].mean()
        var = df_by_type_exp["time"].std()
        experiment_means[t + " Avg"].append(mean)
        experiment_variances[t + " Variance"].append(var)

all_means = []
for lst in experiment_means.values():
    all_means.extend(lst)
ylim_upper = max(all_means) * 1.2

plt.rcParams.update({'font.size': 12})

x = np.arange(len(Experiments))  # the label locations
width = 0.25  # the width of the bars
multiplier = 0

fig, ax = plt.subplots(layout='constrained')

for (attr, mean), (attr_var, var) in zip(experiment_means.items(), experiment_variances.items()):
    offset = width * multiplier
    ebars = ax.errorbar(x + offset, mean, fmt=',', color='black', yerr=var, linewidth=2.0, capsize=8)
    rects = ax.bar(x + offset, mean, width, label=attr)
    ax.bar_label(rects, padding=3)
    multiplier += 1

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel(ylabel)
ax.set_title(title)
ax.set_xticks(x + 0.12, Experiments)
ax.legend(loc='upper right', ncol=3)
ax.set_ylim(0, ylim_upper)

plt.show()
 
