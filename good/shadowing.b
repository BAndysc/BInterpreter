//  8 (przesłanianie i statyczne wiązanie)

int x = 10;

function func() -> void
{
	x = 11;
	int x;
	x = 12;
	print x;
	print "\n";
	Fun<void()> func = [x] () -> void { x = 13; print x; print "\n"; };
	func();
	print x;
	print "\n";
};

print x;
print "\n";
func();
print x;
print "\n";

// expected:
// 10
// 12
// 13
// 12
// 11