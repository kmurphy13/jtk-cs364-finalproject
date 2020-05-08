int f(int n, int f) {
    return n * f;
}

//int exp(int x, int y) {
//    if (y == 0)
//        return 1;
//    else
//        return x*exp(x,y-1);
//    print(y);
//}

int main() {

    int x;
    float y;
    int z;

    y = 5;
    print(y);
    y = 5.2;
    print(y);
    x = f(2, 3-1);
    print("x equals: ", x);
    print(y)
//    z = true;
//    z = z + 33; // does not type check
    print(z);
}