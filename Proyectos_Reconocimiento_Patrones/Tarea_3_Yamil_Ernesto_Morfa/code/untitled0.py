#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 27 13:14:47 2021

@author: yamil
"""

import numpy as np
from sklearn.manifold import TSNE
import matplotlib.pyplot as plt


dom1 = np.linspace(-1, 1, 30)
dom2 = np.linspace(0, 2, 30)
imag1 = -dom1**2 + 1
imag2 =  (dom2 - 1)**2 - 0.5

plt.plot(dom1, imag1, '*r')
plt.plot(dom2, imag2, '*b')
plt.show()

data1 = np.vstack((dom1, imag1)).T
data2 = np.vstack((dom2, imag2)).T

X = np.vstack((data1, data2))

perplexity = [5, 10 , 30, 50 ]

fig, axs = plt.subplots(4)

for i, p in enumerate(perplexity):    
    tsne = TSNE(perplexity=p)
    X_embedded = tsne.fit_transform(X)
    # axs[i].plot(X_embedded[:,0][:30], X_embedded[:,1][0:30], '*r')
    # axs[i].plot(X_embedded[:,0][30:], X_embedded[:,1][30:], '*b')
    axs[i].plot(X_embedded[:,0][:30], np.ones(30), '*r')
    axs[i].plot(X_embedded[:,0][30:], np.ones(30), '*b')