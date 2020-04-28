// Test #2 A program that determines if
// a number n is prime
int main() {
    bool prime;
    int i;
    int n;
    i = 2; // where to start checking divisors
    n = 1234567; // number checking to see if prime
    prime = true;
    
    while (prime && i < n/2.0) {
        if (n % i == 0) // is n divisible by i?
            prime = false;
        i = i + 1;
        i = i * (2 - i);
        //i = i ** j ** n;
    }
    print(i-1);
}