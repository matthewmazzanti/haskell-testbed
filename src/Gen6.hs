data Gen v a = Done a | Next (v (Gen v a))

data CoGen v i = CoGen v (i -> CoGen v i)
