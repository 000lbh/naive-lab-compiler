int a;
const int b = 1, c = 2;

int fib(int a) {
    if (a == 0 || a == 1)
        return 0;
    return fib(a - 1) + fib(a - 2);
}

void doit(int x) {
    putint(x);
}

int main() {
    a = fib(b) + fib(c);
    doit(a);
    return 0;
}
