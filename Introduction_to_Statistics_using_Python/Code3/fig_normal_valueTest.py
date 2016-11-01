'''
Short demo of how to check for the significance of an individual value.
'''

# author: Thomas Haslwanter, date: April-2014

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns
import os


# Fit the normal distribution
md, sd = 3.5, 0.76
nd = stats.norm(md, sd)

# Plot the data
sns.set_context(context='poster')

limits = (md-3*sd, md+3*sd)
x = np.linspace(limits[0], limits[1])
y = nd.pdf(x)

checkVal = 2.6
print('p = {0:5.3f}'.format(nd.cdf(checkVal)))

x1 = np.linspace(limits[0], checkVal)
y1 = nd.pdf(x1)
x2 = np.linspace(md + (md-checkVal), limits[1])
y2 = nd.pdf(x2)

plt.plot(x,y)
plt.fill_between(x1, y1, alpha=0.5)
plt.fill_between(x2, y2, alpha=0.2)
plt.xlabel('Weight')
plt.ylabel('P(Weight)')
plt.text(2.1, 0.05, '11.8%', fontsize=20)

outDir = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\Images'
outFile = os.path.join(outDir, 'pdf_checkValue.png')
plt.savefig(outFile, dpi=200)
print('Figure saved to {0}'.format(outFile))

plt.show()
