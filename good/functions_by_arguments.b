function is_odd(int x) -> bool
{
	return x % 2 == 1;	
};

function printer(Fun<bool(int)> cond) -> void
{
	int i;
	for i from 1 to 30 do
	{
		if (cond(i)) then
		{
			format "_\n" i;
		};
	};	
};

print "Nieparzyste: \n";
printer(is_odd);

print "Mniejsze od 10: \n";
printer([ ](int x) -> bool { return x < 10; });