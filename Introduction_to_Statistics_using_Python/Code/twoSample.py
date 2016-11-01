''' Comparison of two groups
- Analysis of paired data
- Analysis of unpaired data

'''

'''
Author:  Thomas Haslwanter
Date:    July-2013
Version: 1.6
'''

from numpy import genfromtxt, mean, std
import scipy.stats as stats
import matplotlib.pyplot as plt
from getdata import getData

def paired_data():
    '''Analysis of paired data
    Compare mean daily intake over 10 pre-menstrual and 10 post-menstrual days (in kJ).'''
    
    # Get the data:  daily intake of energy in kJ for 11 women
    data = getData('altman_93.txt', subDir=r'..\Data\data_altman')
    
    mean(data, axis=0)
    std(data, axis=0, ddof=1)
    
    pre = data[:,0]
    post = data[:,1]
    
    # paired t-test: doing two measurments on the same experimental unit
    # e.g., before and after a treatment
    t_statistic, p_value = stats.ttest_1samp(post - pre, 0)
    
    # p < 0.05 => alternative hypothesis:
    # the difference in mean is not equal to 0
    print("paired t-test", p_value)
    
    # alternative to paired t-test when data has an ordinary scale or when not
    # normally distributed
    rankSum, p_value = stats.wilcoxon(post - pre)
    print("paired wilcoxon-test", p_value)
    
    return p_value # should be 0.0033300139117459797


def unpaired_data():
    ''' Then some unpaired comparison: 24 hour total energy expenditure (MJ/day),
    in groups of lean and obese women'''
    
    # Get the data: energy expenditure in mJ and stature (0=obese, 1=lean)
    energ = getData('altman_94.txt', subDir=r'..\Data\data_altman')
    
    # Group them
    group1 = energ[:, 1] == 0
    group1 = energ[group1][:, 0]
    group2 = energ[:, 1] == 1
    group2 = energ[group2][:, 0]
    
    mean(group1)
    mean(group2)
    
    # two-sample t-test
    # null hypothesis: the two groups have the same mean
    # this test assumes the two groups have the same variance...
    # (can be checked with tests for equal variance)
    # independent groups: e.g., how boys and girls fare at an exam
    # dependent groups: e.g., how the same class fare at 2 different exams
    t_statistic, p_value = stats.ttest_ind(group1, group2)
    
    # p_value < 0.05 => alternative hypothesis:
    # they don't have the same mean at the 5% significance level
    print("two-sample t-test", p_value)
    
    # For non-normally distributed data, perform the two-sample wilcoxon test
    # a.k.a Mann Whitney U
    u, p_value = stats.mannwhitneyu(group1, group2)
    print("two-sample wilcoxon-test", p_value)
    
    # Plot the data
    plt.plot(group1, 'bx', label='obese')
    plt.hold(True)
    plt.plot(group2, 'ro', label='lean')
    plt.legend(loc=0)
    plt.show()
    
    return p_value  # should be 0.0010608066929400244

if __name__ == '__main__':
    paired_data()    
    unpaired_data()
