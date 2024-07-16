int main() {
    int a = 0;
    a = 1 + a;
    a = a + a;
    {
        int a = a;
        if (1)
            return a;
        else {
            a = a + 1;
            {
                return 0;
            }
            return 0;
        }
        return 0;
    }
}