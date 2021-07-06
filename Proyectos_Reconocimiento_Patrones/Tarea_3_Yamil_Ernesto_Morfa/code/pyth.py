#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 27 18:07:39 2021

@author: yamil
"""

from sklearn.manifold import Isomap
from sklearn.datasets import make_swiss_roll
import numpy as np
import matplotlib.pyplot as plt
from numba import jit

X, color = make_swiss_roll(n_samples=500)
n_neighbors = 25
n_components = 2


fig = plt.figure(figsize = (15, 8))
ax = plt.axes(projection ="3d")
ax.scatter3D(X[:,0], X[:,1], X[:,2], c = color)
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
plt.show()

isomap = Isomap(n_neighbors, n_components)
Y = isomap.fit_transform(X)
plt.scatter(Y[:, 0], Y[:, 1], c=color)
plt.show()

new_row = [0, 15 , -7]
new_color = [color[30]]
XX = np.vstack((new_row, X))
color_ = np.hstack((new_color, color))

fig = plt.figure(figsize = (15, 8))
ax = plt.axes(projection ="3d")
ax.scatter3D(XX[:,0], XX[:,1], XX[:,2], c = color_)
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
plt.show()

isomap = Isomap(n_neighbors, n_components)
Y = isomap.fit_transform(XX)
plt.scatter(Y[:, 0], Y[:, 1], c=color_)
plt.show()