/* Add 2-bit numbers 'x' and 'y' (that is, 0..3) using a ripple-carry
 * adder.  Return -1 for overflow.
 * Chris Jeris */
{
  x = 2;
  y = 3;
  /* Bounds check. */
  if (x < 0 || x > 3 || y < 0 || y > 3) {
    return -1;
  }
  /* Compute the bits. */
  if (x >= 2) {
    x1 = 1;
  }
  else {
    x1 = 0;
  }
  x0 = x - 2 * x1;
  if (y >= 2) {
    y1 = 1;
  }
  else {
    y1 = 0;
  }
  y0 = y - 2 * y1;
  /* Add the low-order bits. */
  if ((x0 || y0) && !(x0 && y0)) {
    s0 = 1;
  }
  else {
    s0 = 0;
  }
  if (x0 && y0) {
    c1 = 1;
  }
  else {
    c1 = 0;
  }
  /* Add the high-order bits. */
  if (x1 && y1 && c1
      || x1 && !y1 && !c1
      || !x1 && y1 && !c1
      || !x1 && !y1 && c1) {
    s1 = 1;
  }
  else {
    s1 = 0;
  }
  if (x1 && y1 || x1 && c1 || y1 && c1) {
    c2 = 1;
  }
  else {
    c2 = 0;
  }
  /* Compose and return the result. */
  return 4 * c2 + 2 * s1 + s0;
}
