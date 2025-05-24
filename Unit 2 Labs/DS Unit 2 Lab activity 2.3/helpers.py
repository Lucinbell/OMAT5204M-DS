#!/usr/bin/env python
import numpy as np
# Disclaimer: code adopted from sklearn
# Plotting classifier decision boundaries
# Decision Regions
# Below is a helper function to plot the decision boundaries of
# any classifier (DT, KNN, Perceptron etc)


def plotClassBoundary(clf, X1, X2, y, ax, title='Classes Boundaries',
                      fontsize=None, psize=100):
    # clf is the classifier
    # X1 and X2 are the two attributes that constitutes teh inout space in 2d
    # (we cannot plot decision regions in more than 2d)
    # y is the class vector of these records [X1, x2]
    # the rest of the input parameters aee self explanatory

    # first we need to get the max and min of the each attribute and we go a
    # bit more and a bit less than these, respectively.
    x_min, x_max = X1.min() - 0.5, X1.max() + 0.5
    y_min, y_max = X2.min() - 0.5, X2.max() + 0.5

    # now we create a meshgrid in order to cover a surface or area in the 2d
    # space that formed by the two attributes X1 and X2
    # refinement_level = 0.1 # reprsents the distances between one grid point
    # and the other
    xx, yy = np.meshgrid(np.arange(x_min, x_max, 0.1),
                         np.arange(y_min, y_max, 0.1))
    # each point in xx and yy reprsents a point in the 2d space

    # to obtain all possibel points we ravel xx and yy, i.e. we flaten them
    # into a simple vectors using xx.ravel(), yy.ravel()
    # then we combine the two vectors into one matrix that have two columns,
    # xx in the first column and yy in the second column
    X12 = np.c_[xx.ravel(), yy.ravel()]

    # now we use the classifier to predict the class which we represent as Z
    Z = clf.predict(X12)
    # note that the points X12 of the mehsgrid do not necessarily correspond to
    # particular datapoints in the dataset, they constitiute hypothetical
    # datapoints that would be classified by the classifier in a cepecific way

    # now we need to reshape the classificaiton for all of these points on the
    # surface into a 2d array that corresponds to xx or yy
    Z = Z.reshape(xx.shape)

    # **plotting decision boundaries**
    # now we can plot the decision region of the classifer
    # the surface area correspond to the grid that we created using meshgrid
    # and the contours colours correspond with the predicted classes.
    # ***This is the main trick in this function***
    # The more refined the meshgrid is, the more refined and detailed the
    # coloured areas.

    ax.contourf(xx, yy, Z, alpha=0.4)

    # now we can plot the different data points that we actually have in the
    # dataset to compare them with the decision regions
    ax.scatter(X1[y == 1], X2[y == 1],     facecolors='none',
               marker='o', edgecolor='r', s=0.8*psize, label='Class1')
    ax.scatter(X1[y == 0], X2[y == 0],     facecolors='b',
               marker='+', edgecolor='b', s=1.0*psize, label='Class2')
    # The above two lines can be replaced by the below
    # (but you will not be able to change the marker though)
    # ax.scatter(X1, X2, c=Y, edgecolor='k')

    # set title and labels and show the legend for the plot
    ax.set_title(title)
    ax.set_xlabel('$x_1$', fontsize=16)
    ax.set_ylabel('$x_2$', fontsize=16)
    ax.legend()
