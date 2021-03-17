num foo(num a, bool b, num c) { }

void bar(num a, void b, num c) {} /* Error: illegal void formal b */

num main()
{
  return 0;
}
