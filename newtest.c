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
    int y;
    bool z;

    y = 5;
    print(y);
    x = f(2, 3-1);
    print("x equals: ", x);
    print(y)
    z = true;
    z = z + 33; // does not type check
    while (x>0){
        print(x);
        x = x-1;

    }
}