//  4 (procedury lub funkcje, rekurencja)
//  5 (print)
//  6 a) (przez zmienną i wartość)
//    b) (pętla for)
//  10 (funkcje zwracające wartość)

function hello() -> void
{
	print 3117;
	print "\n";
};

hello();

function factorial(int n) -> int
{
	if n == 1 then
	{
		return 1;
	}
	else
	{
		return n * factorial (n - 1)
	}
};

function factorial2(int n) -> int
{
	int i;
	int result = 1;
	for i from 2 to n do
		result = result * i;

	return result;
};

print factorial(5);
print "\n";
print factorial2(5);
print "\n";


function inc(ref int num, int num2) -> void
{
	num = num + 1;
	num2 = num2 + 1
};

int x = 10;
int y = 10;

inc(ref x, y);

print x;
print "\n";
print y;
print "\n";