//  b) (listy)
//  f) (funkcje jako parametry)
//  h) (funkcje anonimowe)

function map(List<int> source, Fun<int(int)> func) -> List<int>
{
	List<int> nnew;
	int x;
	for x in source do
		nnew.push func (x);
	return nnew;
};

function apply(List<int> source, Fun<void(int)> func) -> void
{
	int x;
	for x in source do
	{
		func (x);
	};
};

function fold(List<int> source, Fun<int(int, int)> func, int value) -> int
{
	int x;
	for x in source do
		value = func(value, x);
	return value;
};

function sum(List<int> source) -> int
{
	return fold(source, [ ](int current, int item) -> int {return current + item;}, 0);
};

function print_ints(List<int> list) -> void
{
	Fun<void(int)> printer = [ ] (int x) -> void { format "_ " x; };
	apply(list, printer)
};

function print_ints2(List<int> list) -> void
{
	int i;
	for i from 0 to list::length - 1 do
		format "_ " list::at i;
};

function init_my_list(ref List<int> list, int start, int end) -> void
{
	int i;
	for i from start to end do
		list.push i
};

List<int> list = int[];
int i;

init_my_list(ref list, 0, 15);

print_ints (list);
print "\n";

List<int> squares = map(list, [ ] (int x) -> int {return x * x});
print_ints (squares);
print "\n";

format "_\n" sum(list);

format "List length: _\n" squares::length;
format "Print with foreach: \n";
print_ints(list);
print "\n";
format "Print with ::length and ::at: \n";
print_ints2(list);
print "\n";

List<List<List<string>>> z;

List<string> z3;
z3.push "Ala";
z3.push "ma";
z3.push "kota";

List<string> z4;
z4.push "Kot";
z4.push "ma";
z4.push "Ale";

List<List<string>> z2;
z2.push z3;
z2.push z4;

z.push z2;

print z;
