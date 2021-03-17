void myvoid()
{
  return;
}

num main()
{
  num i;

  i = myvoid(); /* Fail: assigning a void to an integer */
}
