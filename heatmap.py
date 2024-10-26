# -*- coding: utf-8 -*-
"""
Created on Sat Jun 15 14:23:48 2024

"""

import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib
from matplotlib import cm
import pandas as pd
import numpy as np

## Read Data

df = pd.read_csv('SOC_mean_diff_reverse.csv', index_col=0)
df = df.replace('-', -1000).astype(float)

data = df.to_numpy()
mask = np.ones(data.shape)-np.tri(data.shape[0], data.shape[1], k=-1)
data = np.ma.array(data, mask=mask)

# Create a diverging colormap
cmap = cm.coolwarm  # Red-blue, reversed

# Create a normalization instance centered on 0
#norm = colors.CenteredNorm(vcenter=0)

## Draw heatmap
font = {'family' : 'normal',
        'size'   : 12}

matplotlib.rc('font', **font)


plt.figure(figsize=(10,8))

ax = plt.gca()
# Plot the heatmap
im = ax.imshow(data, norm=colors.CenteredNorm(),cmap=cmap)

# Create colorbar
cbar = ax.figure.colorbar(im, ax=ax)
cbar.ax.set_ylabel('Absolute difference of SOC analysis mean (kg/m\u00B2)', rotation=-90, va="bottom")


x_ticks = [''] + list(df.columns)
y_ticks = [''] + list(df.index)

ax.set_xticklabels(x_ticks, rotation=45)
ax.set_yticklabels(y_ticks, )

ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.get_xaxis().tick_bottom()
ax.get_yaxis().tick_left()

for i in np.arange(mask.shape[0]):
    for j in np.arange(mask.shape[1]):
        if df.iloc[i,j] == -1000: continue
        color = 'black'
        t = format(df.iloc[i,j],'.1f')
  #      if  abs(df.iloc[i,j])>0.8:
   #       color = 'white'
        if (i,j) in [ (8,0),(8,1),(8,2)]:
              color = 'white'
        if (i,j) in [ (1,0),(2,0),(2,1),(7,3),(7,4)]:
            t=t
        else:
            t = t + '*'
        plt.text(j,i, t , color=color, ha='center', va='center' )

plt.savefig('heat1_2012_300dpi.png', dpi=300)      

print(df.iloc[6,3])
