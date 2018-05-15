//  7 (statyczne typowanie)

int var = true;
bool var1 = 4;
int var2 = 2 < 3;

Fun<int(int)> var3 = [ ] () -> int { return 2; };

Fun<void()> var4 = [ ] () -> bool { return true; };

Fun<void()> var5 = [ ] (int x) -> void { };

Fun<void(ref int)> var6 = [ ] (int x) -> void { };

Fun<void(int)> var7 = [ ] (ref int y) -> void { };


Fun<void(Fun<void(ref int)>)> var8 = [ ] (Fun<void(ref int)> func) -> void { };
var8([ ] (int x) -> void { });


function test(int y) -> void { };
test(true);

function test2(ref int y) -> void { };
test2(5);

bool var9 = false;

test2(var9);

test2(ref var9);

int var10 = test(10);
