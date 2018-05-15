//struct Point {
//    int x;
//    int y;
//};

struct Point mypoint;

mypoint.x = 5;
mypoint.str = "To jest moj string!";

List<int> lista;
lista.push 4;
lista.push 5;
lista.push 2;
lista.push 8;
lista.push 5;
lista.push 1;

mypoint.s = lista;

mypoint.pole = new Struct;

mypoint.pole.x = 5;

print (mypoint).str;
print "\n";
print ((mypoint).pole).x;
print "\n";
int i;
for i in (mypoint).s do
    print i;

function fff(int x) -> void
{
    print x;
    return;
};
fff(120);