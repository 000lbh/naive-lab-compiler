int a;
const int b = 1, c = 2;

int fib(int a) {
    if (a == 0 || a == 1)
        return 0;
    return fib(a - 1) + fib(a - 2);
}

int main() {
    a = fib(b) + fib(c);
    return fib(a);
}