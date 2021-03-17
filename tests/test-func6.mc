void foo() {}

num bar(num a, bool b, num c) { return a + c; }

num main()
{
  print(bar(17, false, 25));
  return 0;
}
