int f(int n) {
    return n * n;
}

int main() {

    int x;
    int y;
    bool z;

    y = 10;
    x = f(y);
    print("x equals: ", x);
    y = x * 8;
    print(y)
    z = true;
    z = z + 33; // does not type check
    print(z);

    while (x > 0) {
        if (x % 2 == 0)
            print(x);
        x = x - 1;
    }
}