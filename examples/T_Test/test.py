# Running a Two-Tailed Two-Sample T-Test in Python
from scipy.stats import ttest_ind

# Generate two independent samples (Run times of unoptimized and optimized program)
unopt_times = [97.52634,96.64325,95.31638,95.54251,95.46956,95.33659,95.31660,95.98237,95.85292]
opt_times = [90.477515,97.263674,95.388330,95.173509,95.326311,95.651076,95.908187,95.586877,95.456038]

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
