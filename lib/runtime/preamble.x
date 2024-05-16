namespace std {
    @decl print_int(Int)
    @decl print_bool(Bool)
    @decl print_char(Char)
    @decl print_string(Char*)
    @decl print_endline()

    namespace math {
        func inc(x: Int) -> Int {
            return x + 1
        }

        func add(x: Int, y: Int) -> Int {
            return x + y
        }
    }
}
