//  12 (funkcje zagnieżdżone ze statycznym wiązaniem)

function mul_ten(int number) -> int
{
	function ident(int x) -> Fun<int()>
	{
		return [x] () -> int { return x; };
	};

	return ident(number)() * 10;
};

function identity(int number) -> Fun<int()>
{
	function ident(ref int x) -> Fun<int()>
	{
		return [x] () -> int { return x; };
	};

	return ident(ref number);
};

print mul_ten(20);
print "\n";
int i;
Fun<int()> id = identity(2);
for i from 1 to 20 do
{
	print id();
	print " ";
};