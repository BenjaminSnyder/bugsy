int main
{
    square x = square(pt(0,0), 72);
    // rotates the square by 72 degrees
    x.rotate(degree: 72, rpm: 1, repeating: false)
	square x = square(pt(0,0), 2);
	// continually rotates the square
	x.rotate(degree: 360, rpm: 2, repeating: true)
	// output something maybe
}
