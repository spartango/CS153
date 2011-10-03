{
  a = 2;
  b = 2;
  c = 1;
  x = 0;
  if (a == 1)
    if (b == 1)
      if (c == 1) x = 1; else x = 2;
    else
      if (c == 1) x = 3; else x = 4;
  else
    if (b == 1)
      if (c == 1) x = 5; else x = 6;
    else
      if (c == 1) x = 7; else x = 8;
  return x;
}

