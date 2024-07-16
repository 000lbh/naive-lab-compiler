int main() {
    int a;
    a = 10;
    const int b = 2, c = b * b, d = c * c;
    int e = d, f, g = f;
    f = 10;
    g = e + f;
    return g;
}