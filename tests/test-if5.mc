num cond(bool b)
{
  num x;
  if (b) {
    x = 42;
  }
  
  else {
    x = 17;
  }
  return x;
}

num main()
{
 print(cond(true));
 print(cond(false));
 return 0;
}
