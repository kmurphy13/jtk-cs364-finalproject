int sum_3_or_5(int n) {
	int sum;
	int i;

	sum = 0;
	i = 0;
	while (i < 4){
		if (i % 3 == 0 || i % 5 == 0)
	sum = sum + i;
	else
			sum = 0;

	i = i + 1;
	}
return sum;
}

int main() {

	if (1 == 2) 
		print("The answer is: ", 1000, "Woot!");

}

int main() {
	bool prime;
	int i;
	int n;

	i = 2;
	n = 1234567;
	prime = true;
	while (prime && i < n / 2.0){
		if (n % i == 0) 
		prime = false;

	i = i + 1;
	}
	print(i - 1);
}