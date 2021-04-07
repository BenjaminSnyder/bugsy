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

void add_circle(double x, double y, double r, double* stroke, double thiccness, double* fill) {

    if(fill[0] >= 0.0) {

        glColor3f(fill[0], fill[1], fill[2]);
        glBegin(GL_TRIANGLE_FAN);
        for (float i = 0; i < (2 * pi); i += 0.001) {
            float a = r * cos(i) + x;
            float b = r * sin(i) + y;
            glVertex2i(a, b);
        }
        glEnd();

    }

    if(stroke[0] >= 0.0) {

        glColor3f(stroke[0], stroke[1], stroke[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
        for (float i = 0; i < (2 * pi); i += 0.1) {
            float a = r * cos(i) + x;
            float b = r * sin(i) + y;
            glVertex2i(a, b);
        }
        glEnd();

    }



    // //filled circle
    // float x1,y1,x2,y2;
    // float angle;
    //
    // x1 = 0.5,y1=0.6;
    // glColor3f(1.0,1.0,0.6);
    //
    // glBegin(GL_TRIANGLE_FAN);
    // glVertex2f(x1,y1);
    //
    // for(angle = 1.0f; angle < 361.0f; angle += 0.2) {
    //     x2 = x1+sin(angle) * r;
    //     y2 = y1+cos(angle) * r;
    //     glVertex2f(x2,y2);
    // }
    //
    // glEnd();

}

void add_square(double x, double y, double s, double* stroke, double thiccness, double* fill) {

    if(fill[0] >= 0.0) {

        glColor3f(fill[0], fill[1], fill[2]);
        glBegin(GL_QUADS);
            glVertex2f(x-(s/2.0), y-(s/2.0));
        	glVertex2f(x-(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y-(s/2.0));
        glEnd();

    }

    if(stroke[0] >= 0.0) {

        glColor3f(stroke[0], stroke[1], stroke[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            glVertex2f(x-(s/2.0), y-(s/2.0));
        	glVertex2f(x-(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y-(s/2.0));
        glEnd();

    }

}


void add_rectangle(double x, double y, double s, double* stroke, double thiccness, double* fill) {

    if(fill[0] >= 0.0) {

        glColor3f(fill[0], fill[1], fill[2]);
        glBegin(GL_QUADS);
            glVertex2f(x-(s/2.0), y-(s/2.0));
        	glVertex2f(x-(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y-(s/2.0));
        glEnd();

    }

    if(stroke[0] >= 0.0) {
        fprintf(stderr, "sus");
        glColor3f(stroke[0], stroke[1], stroke[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            glVertex2f(x-(s/2.0), y-(s/2.0));
        	glVertex2f(x-(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y-(s/2.0));
        glEnd();

    }

}


void add_canvas(double width, double height, double xOffset, double yOffset) {

    int c = 0;
    char ** args;
    double aspect = width / height;

    glutInit(&c, args);

    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);


    // giving window size in X- and Y- direction

    int w = (int) width;
    int h = (int) height;
    int x = (int) xOffset;
    int y = (int) yOffset;

    glutInitWindowSize(w, h);

    glutInitWindowPosition(x, y);

    glutCreateWindow("Canvas");

    myInit();

    // loop through the list of objects and call the add_object functions
    // for shape in shapes:

    double stroke[] = {1.0, 0.5, 0.0};
    double fill[] = {0.0, 1.0, 0.0};

    glClearColor(0.0, 0.0, 0.0, 0.0);         // black background
    glMatrixMode(GL_PROJECTION);              // setup viewing projection
    glLoadIdentity();                           // start with identity matrix
    glOrtho(0.0, w, 0.0, h, 0.0, 1.0);   // setup a 10x10x2 viewing world

    add_circle(250, 250, 100, stroke, 2.0, fill);
    // add_square(20, 20, 300, stroke, 5.0, fill);

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

    //glutDisplayFunc(display);

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
