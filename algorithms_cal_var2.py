### https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance


data=[1,2,3,4,5,6,7,8,9,10]
def two_pass_variance(data):
    n = len(data)
    mean = sum(data) / n
    variance = sum((x - mean) ** 2 for x in data) / (n - 1)
    return variance


print(two_pass_variance(data))
