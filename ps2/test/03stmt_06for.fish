{
  x = 0;
  for (a = 1; a <= 10; a = a + 1)
    for (b = 1; b <= a; b = b + 1)
      for (c = 1; c <= b; c = c + 1)
        x = x + c;
  return x;
}
