num foo(num a, bool b)
{
  num c;
  bool d;

  c = a;

  return c + 10;
}

num main() {
 print(foo(37, false));
 return 0;
}
