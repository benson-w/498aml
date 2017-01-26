# Reference from http://machinelearningmastery.com/naive-bayes-classifier-scratch-python/
import random
import math

# index 0 1 2 3 4 5 6 7 8
# currentline[8] is to classify

def mean(numbers):
	return sum(numbers)/float(len(numbers))

def stdev(numbers):
	avg = mean(numbers)
	variance = sum([pow(x-avg,2) for x in numbers])/float(len(numbers)-1)
	return math.sqrt(variance)

def splitData(dataset, splitRatio):
	trainSize = int(len(dataset) * splitRatio)
	trainSet = []
	copy = list(dataset)
	while len(trainSet) < trainSize:
		index = random.randrange(len(copy))
		trainSet.append(copy.pop(index))
	return [trainSet, copy]

def loadfile(filename):
    with open(filename, 'r') as filestream:
        for line in filestream:
            currentline = line.split(",")


def main():
    filename = 'pima-indians-diabetes.data.txt'
    data = loadfile(filename)

    data1, data2 = splitData(data, .8)
