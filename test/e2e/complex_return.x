// Computes ax^2 + bx + c
func eval_quadratic(a: Int, b: Int, c: Int, x: Int) -> Int {
    return a * x * x + b * x + c
}

func main() {
    print eval_quadratic(0, 0, 0, 0)
    print eval_quadratic(1, 1, 1, 1)
}
