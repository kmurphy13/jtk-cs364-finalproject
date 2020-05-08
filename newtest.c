int f(int n) {
    return n * n;
}

int main() {

    int x;
    int y;
    bool z;

    y = 5;
    x = f(f(f(2)));
    print("x equals: ", x);
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