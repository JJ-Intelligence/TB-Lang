

with open("inputs/input20.txt") as f:
    with open("outputs/output20.txt", "w") as outf:
        fib = [1, 1]
        xs = []
        for l in f:
            v = int(l)
            xs += [v]
            sum = 0
            for j in range(len(xs)):
                sum += fib[j+1] * xs[j]
            outf.write(str(sum)+"\n")
            fib = [fib[0] + fib[1]] + fib

