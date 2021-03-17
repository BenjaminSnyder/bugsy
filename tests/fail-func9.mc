void foo(num a, bool b)
{
}

int main()
{
  foo(42, true);
  foo(42, 42); /* Fail: num, not bool */
}
