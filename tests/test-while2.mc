num foo(int a)
{
  num j;
  j = 0;
  while (a > 0) {
    j = j + 2;
    a = a - 1;
  }
  return j;
}

num main()
{
  print(foo(7));
  return 0;
}
