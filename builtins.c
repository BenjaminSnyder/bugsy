/*
 *  A function illustrating how to link C code to code generated from LLVM
 */

#include <stdio.h>
#include <GL/glut.h>
#include <math.h>
#define pi 3.142857

float s = 1;

float l = 0.5;


void myInit ()

{

    // making background color black as first

    // 3 arguments all are 0.0

    glClearColor(0.0, 0.0, 0.0, 1.0);



    // making picture color green (in RGB mode), as middle argument is 1.0

    glColor3f(0.0, 1.0, 0.0);



    // breadth of picture boundary is 1 pixel

    glPointSize(1.0);

    glMatrixMode(GL_PROJECTION);

    glLoadIdentity();


    // setting window dimension in X- and Y- direction

    gluOrtho2D(-780, 780, -420, 420);

}

void add_point_xy(double x, double y) {
    printf("x: %.2f, y: %.2f\n", x, y);
}

void disp() {
    glClear(GL_COLOR_BUFFER_BIT);
    glBegin(GL_POINTS);
    float x, y, i;
    for (i = 0; i < (2 * pi); i += 0.001) {

        x = 50 * cos(i) + 10;
        y = 50 * sin(i) + 20;
        glVertex2i(x, y);


    }


    glEnd();
    glFlush();

}

void add_circle(double x, double y, double r) {



    glBegin(GL_POINTS);
    for (float i = 0; i < (2 * pi); i += 0.001) {
        float a = r * cos(i) + x;
        float b = r * sin(i) + y;
        glVertex2i(a, b);
    }
    glEnd();


}

void display ()

{

    // glClear(GL_COLOR_BUFFER_BIT);
    //
    // glBegin(GL_POINTS);
    //
    // float x, y, i;
    //
    // // iterate y up to 2*pi, i.e., 360 degree
    //
    // // with small increment in angle as
    //
    // // glVertex2i just draws a point on specified co-ordinate
    //
    // for ( i = 0; i < (2 * pi); i += 0.001)
    //
    // {
    //
    //     // let 200 is radius of circle and as,
    //
    //     // circle is defined as x=r*cos(i) and y=r*sin(i)
    //
    //     x = s * 50 * cos(i);
    //
    //     y = s * 50 * sin(i);
    //
    //
    //
    //     glVertex2i(x, y);
    //
    // }
    //
    // for ( i = 0; i < (2 * pi); i += 0.001)
    //
    // {
    //
    //     // let 200 is radius of circle and as,
    //
    //     // circle is defined as x=r*cos(i) and y=r*sin(i)
    //
    //     x = s * 50 * cos(i)+ (50 * s);
    //
    //     y = s * 50 * sin(i);
    //
    //
    //
    //     glVertex2i(x, y);
    //
    // }
    //
    //
    // for(i = 0; i < s * l * 300; i += 0.001) {
    //     x = i;
    //     y = s * 50;
    //     glVertex2i(x, y);
    // }
    //
    // for(i = 0; i < s * l * 300; i += 0.001) {
    //     x = i;
    //     y = s * 100;
    //     glVertex2i(x, y);
    // }
    //
    // for(i = 0; i < s * 50; i += 0.001) {
    //     x = 0;
    //     y = i + (s * 50);
    //     glVertex2i(x, y);
    // }
    //
    // for ( i = 0; i < (2 * pi); i += 0.001)
    //
    // {
    //
    //     // let 200 is radius of circle and as,
    //
    //     // circle is defined as x=r*cos(i) and y=r*sin(i)
    //
    //     x = s * 30 * cos(i)+(s * l * 300);
    //
    //     y = s * 30 * sin(i) + (s * 75);
    //
    //     glVertex2i(x, y);
    //
    // }
    //
    //
    // glEnd();
    // glFlush();

    add_circle(100, 10, 200);
    glFlush();

}



void add_canvas(double width, double height, double xOffset, double yOffset) {

    int c = 0;
    char ** args;

    glutInit(&c, args);

    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);


    // giving window size in X- and Y- direction

    int w = (int) width;
    int h = (int) height;
    int x = (int) xOffset;
    int y = (int) yOffset;

    glutInitWindowSize(w, h);

    glutInitWindowPosition(x, y);



    // Giving name to widouble x = 0.0;ndow

    glViewport(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    float aspect = (float)width / (float)height;
    glOrtho(-aspect, aspect, -1, 1, -1, 1);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glutCreateWindow("Canvas");



    myInit();

    // loop through the list of objects and call the add_object functions
    // for shape in shapes:



    add_circle(10, 10, 69);

    glFlush();

    glutMainLoop();

}









// void add_circle(double x, double y, double r) {
//
//     printf("x: %.2f, y: %.2f, r: %.2f\n", x, y, r);
//     int c = 0;
//     char ** args;
//
//     glutInit(&c, args);
//
//     glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
//
//
//     // giving window size in X- and Y- direction
//
//     glutInitWindowSize(1366, 768);
//
//     glutInitWindowPosition(0, 0);
//
//
//
//     // Giving name to widouble x = 0.0;ndow
//
//     glutCreateWindow("Circle Drawing");
//
//     myInit();
//
//     glBegin(GL_POINTS);
//     for (float i = 0; i < (2 * pi); i += 0.001) {
//         float a = r * cos(i) + x;
//         float b = r * sin(i) + y;
//         glVertex2i(a, b);
//     }
//
//
//     glEnd();
//     glFlush();
//
//     glutMainLoop();
//
// }


// void printbig(float c)
// {
//   int index = 0;
//   int col, data;
//   if (c >= '0' && c <= '9') index = 8 + (c - '0') * 8;
//   else if (c >= 'A' && c <= 'Z') index = 88 + (c - 'A') * 8;
//   do {
//     data = font[index++];
//     for (col = 0 ; col < 8 ; data <<= 1, col++) {
//       char d = data & 0x80 ? 'X' : ' ';
//       putchar(d); putchar(d);
//     }
//     putchar('\n');
//   } while (index & 0x7);
// }

void keyPressed (unsigned char key, int x, int y) {
    if(key == 'g') {
        l += 0.05;
        glutPostRedisplay();
    }

    if(key == 's') {
        l -= 0.05;

        glutPostRedisplay();
    }

    if(key == 'w') {
        s += 0.05;
        glutPostRedisplay();
    }

    if(key == 'q') {
        s -= 0.05;

        glutPostRedisplay();
    }
}

void demo ()

{

    int c = 0;
    char ** args;

    glutInit(&c, args);

    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);



    // giving window size in X- and Y- direction

    glutInitWindowSize(1366, 768);

    glutInitWindowPosition(0, 0);



    // Giving name to widouble x = 0.0;ndow

    glutCreateWindow("Circle Drawing");

    myInit();

    glutKeyboardFunc(keyPressed);

    glutDisplayFunc(display);

    glutMainLoop();
    //
    // double *x = malloc(sizeof(double));
    //
    // return x;

}



#ifdef BUILD_TEST
int main()
{
  char s[] = "HELLO WORLD09AZ";
  char *c;
  for ( c = s ; *c ; c++) printbig(*c);
}
#endif
