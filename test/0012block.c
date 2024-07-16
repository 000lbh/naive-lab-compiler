int main() {
    ;;;;;;;;;;;
    int a = 5;
    const int b = 4;
    int result;
    {
        result = b + a;
        a = a + a;
        result = result + a;
        const int a = 10;
        int b = a + 7;
        b = b * b;
        result = a + b;
    }
    return result;
}