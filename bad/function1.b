/*
function test() -> int
{

};
*/


/*
function test() -> int
{
    return 4>1;
};
*/

/*
function test() -> int
{
    return;
};
*/

/*
function test(int x) -> int
{
    x = 10;
    return 5;
};

bool x = test(4);
*/

/*
function test(int x) -> int
{
    return 5;
};

int x = test(true);
*/

/*
function test(int x) -> int
{
    return 5;
};

bool y = false;
int x = test(y);
*/

/*
function test(int x) -> int
{
    return 5;
};

int y = 10;
int x = test(ref y);
*/

/*
function test(ref int x) -> int
{
    return 5;
};

bool y = 10;
int x = test(ref y);
*/

/*
Fun<int()> f = [x]() -> int { return 5; };
*/

/*
Fun<int()> f = [ ]() -> int { return true; };
*/
