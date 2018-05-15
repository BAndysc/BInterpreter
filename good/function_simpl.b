int x = 10;

function square2(int x) -> int
{
    return x*x;
};
function getDay(int day) -> string
{
    if day == 0 then 
    {
        return "Monday";
    }
    else
    {
        if day == 1 then
        {
            return "Tuesday";
        };
    };
    return "Invalid day!";
};
function isOdd(int x) -> bool
{
    return x % 2 == 1;
};
function hello(int x, ref int y, ref string str, bool z) -> void
{
    int a = 0;
    while true do 
    {
        a = a + 1;
        print a;
        print " ";
        if a > 20 then
        {
            return;
        };
    };
    function square(int x) -> void
    {
        print x;
        print " do kwadratu = ";
        x = x * x;
        print x;
        print  "\n";
    };
    if z then
    {
        square(20);
    	print x;
        print "\n";
        print y;
        print "\n";
    }
    else
    {
        y = 20;
    };

    str = "Funkcja zmodyfikowala";
};
function wypisz(int start, int end, Fun<bool(int)> validator) -> void
{
    int i = start;
    while i < end do
    {
        if validator(i) then
        {
            print i;
            print " ";
        };
        i = i + 1;
    };
    print "\n";
};
string str = "Testowy string";
int x = 120;
Fun<bool(int, int, ref int )> action = [x] (int a, int b, ref int c) -> bool { print "Lambda!\n"; x = 1; return true; };
print "Lambda zwrocila: ";
print action(1, 2, ref x);
print "\n";
print x;
print "\n";
int y = 14;
wypisz(0, 100, [x](int x) -> bool { return x > 50; });
wypisz(0, 100, [x](int x) -> bool { return x % 5 == 0; });
wypisz(0, 100, [y](int x) -> bool { return x % y == 0; });
return;
x = 2;
print "My app!\n";
print "Nowa linia";
print str;
print "\n";
hello(5, ref x, ref str, true);
print str;
print "\n";
print x;
print "\n";
print "40^2 = ";
print square2(40);
print "\n";
print getDay(2);
print "\n";
print "czy 6 jest nieparzyste? ";
print isOdd(6);
print "\n";
