class GrandParent {
public:
  virtual int foo() { return 0; }
  virtual int bar() { return 20; }
  virtual int baz() { return 33; }
};

class Parent : public GrandParent {
public:
  virtual int foo() { return 1; }
  virtual int bar() { return 10; }
};

class Child : public Parent {
public:
  virtual int foo() { return 2; };
};

int main() {
  GrandParent *p0, *p1, *p2;

  p0 = new GrandParent;
  p1 = new Parent;
  p2 = new Child;

  int a = p0->foo();
  int b = p1->foo();
  int c = p2->foo();
  int d = p0->bar();
  int e = p1->bar();
  int f = p2->bar();
  int g = p0->baz();
  int h = p1->baz();
  int i = p2->baz();

  delete p1;
  delete p2;
}
