# Running a Two-Tailed Two-Sample T-Test in Python
from scipy.stats import ttest_ind

# Generate two independent samples (Run times of unoptimized and optimized program)

unopt_times = [30.20586,30.71441,30.80547,30.69609,30.89923,30.73076,30.75152,30.68948,30.68458,30.79036,31.06961,30.55503,30.74556,30.73415,30.81973,30.78024,30.87978,30.70081,30.74846,30.80092,30.77712,30.89183,30.74194,30.75555,30.86966,31.05708,30.79889,30.76951,30.78856,30.80897,30.87900,30.71954,30.72554,30.80346,30.65461,30.74649,30.73528,30.64232,30.88115,30.62882,30.77189,30.67286,30.76276,30.70160,30.61637,30.57814,30.70965,30.64748,30.63888,30.69493]


opt_times = [25.403322,30.761067,30.734365,30.841923,30.779009,30.748084,30.779062,30.689408,30.856752,30.780257,30.808583,31.077290,30.684076,30.836612,30.710403,30.691995,30.720059,30.915419,30.696385,30.773336,30.765261,30.828928,30.865211,30.767289,30.748925,30.752866,30.751671,30.838184,30.692465,30.891919,30.874752,30.789470,30.688774,30.991906,30.837983,30.931387,30.875175,30.763188,30.871890,30.795566,31.262700,30.866909,30.792858,31.121125,30.738358,30.931944,30.746240,30.859691,30.835699,30.766243]

# Perform two-sample t-test
t_test_result = ttest_ind(unopt_times, opt_times, equal_var=False)

# Output the results
print(f"t-statistic: {t_test_result.statistic}")
print(f"P-value: {t_test_result.pvalue}")
print(f"P-value: {t_test_result.confidence_interval()}")

# check if the result is statistically significant (using a common significance level of 0.05)
if t_test_result.pvalue < 0.05:
    print("There is a significant difference between the two runtimes.")
else:
    print("There is no significant difference between the two runtimes.")

# Returns:
# There is a significant difference between the unoptimized and optimized runtimes.
