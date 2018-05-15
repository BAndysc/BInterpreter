//  1 (dwa typy) int, bool
//  2 (arytmetyka, porównania)
//  3 (while, if)

int x;
x = 10;
bool y;
y = x < 5;
if y then
{
	x = x + 2
}
else 
{
	if not false then
	{
		format "_\n" 120;
	}
	else
	{
		x = x - 2;
	};
};
print x;
	print "\n";

while x < 100 do
	x = x + 1;

print x;
print "\n";
