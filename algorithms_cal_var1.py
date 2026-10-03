
### https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance


def shifted_data_variance(data, K):
    if len(data) < 2:
        return 0.0
    n = Ex = Ex2 = 0.0
    for x in data:
        n += 1
        Ex += x - K
        Ex2 += (x - K) ** 2
    variance = (Ex2 - Ex**2 / n) / (n - 1)
    # use n instead of (n-1) if want to compute the exact variance of the given data
    # use (n-1) if data are samples of a larger population
    return variance


data=[1,2,3,4,5]
K=2
print(shifted_data_variance(data, K))

