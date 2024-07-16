int main() {
    int a = 10;
    {
        int a = 20;
        return a;
    }
    return a;
}