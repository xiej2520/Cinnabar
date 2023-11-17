# Memory Management, lifetimes, GC

* Want RAII, smart pointers
* Avoid borrow checker complexity?
  * Might still want to avoid mutable aliasing, need borrow checker for that
  * Borrow checker for automatic lifetime types

* GC for memory management?
* Reduce thinking about object lifetimes

```C++
vector<int> v = {0, 1, 2, 3};
const int &val = v[0];
for (int i=0; i<v.size(); i++) { // bug!
  v.push_back(i);
}
int val_now = val; // bug!

```

https://manishearth.github.io/blog/2015/05/17/the-problem-with-shared-mutability/