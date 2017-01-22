# Reference from http://machinelearningmastery.com/naive-bayes-classifier-scratch-python/
import random
import math

# index 0 1 2 3 4 5 6 7 8
# currentline[8] is to classify

def loadfile(filename):
    with open(filename, 'r') as filestream:
        for line in filestream:
            currentline = line.split(",")


def main():
    filename = 'pima-indians-diabetes.data.txt'
    data = loadfile(filename)
