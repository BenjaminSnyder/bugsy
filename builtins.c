/*
 *  A function illustrating how to link C code to code generated from LLVM
 */

#include <stdio.h>
#include <GL/glut.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
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

double* str_to_arr(char* str_in) {

    char str[strlen(str_in)];
    sprintf(str, "%s", str_in);

    char* delim = " ";
    char *ptr = strtok(str, delim);
    double* arr = malloc(3 * sizeof(double));
    int i = 0;
    char* end;

	while(ptr != NULL) {
        arr[i] = strtod(ptr, &end);
		ptr = strtok(NULL, delim);
        i++;
	}

    return arr;

}

void add_ellipse(double x, double y, double w, double h,  char* stroke, double thiccness, char* fill) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);
    int num_segments = 100;
    float xi = 1;
    float yi = 0;

    float theta = 2 * 3.1415926 / num_segments;
    float c = cosf(theta);//precalculate the sine and cosine
    float s = sinf(theta);

    float t;
    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_TRIANGLE_FAN);
        for(int i = 0; i < num_segments; i++) {
               //apply radius and offset
               glVertex2f(xi * w + x, yi * h + y);//output vertex

               //apply the rotation matrix
               t = xi;
               xi = c * xi - s * yi;
               yi = s * t + c * yi;
           }
        glEnd();

    }

    if(stroke_arr[0] >= 0.0) {

        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glBegin(GL_LINE_LOOP);
        for(int i = 0; i < num_segments; i++) {
               //apply radius and offset
               glVertex2f(xi * w + x, yi * h + y);//output vertex

               //apply the rotation matrix
               t = xi;
               xi = c * xi - s * yi;
               yi = s * t + c * yi;
           }
        glEnd();

    }

}

void add_circle(double x, double y, double r, char* stroke, double thiccness, char* fill) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_TRIANGLE_FAN);
        for (float i = 0; i < (2 * pi); i += 0.001) {
            float a = r * cos(i) + x;
            float b = r * sin(i) + y;
            glVertex2i(a, b);
        }
        glEnd();

    }

    if(stroke_arr[0] >= 0.0) {

        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
        for (float i = 0; i < (2 * pi); i += 0.1) {
            float a = r * cos(i) + x;
            float b = r * sin(i) + y;
            glVertex2i(a, b);
        }
        glEnd();

    }

}

void add_square(double x, double y, double s, char* stroke, double thiccness, char* fill) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_QUADS);
            glVertex2f(x-(s/2.0), y-(s/2.0));
        	glVertex2f(x-(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y-(s/2.0));
        glEnd();

    }

    if(stroke[0] >= 0.0) {

        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            glVertex2f(x-(s/2.0), y-(s/2.0));
        	glVertex2f(x-(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y+(s/2.0));
            glVertex2f(x+(s/2.0), y-(s/2.0));
        glEnd();

    }

}


void add_rectangle(double x, double y, double w, double h, char* stroke, double thiccness, char* fill) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_QUADS);
            glVertex2f(x-(w/2.0), y-(h/2.0));
        	glVertex2f(x-(w/2.0), y+(h/2.0));
            glVertex2f(x+(w/2.0), y+(h/2.0));
            glVertex2f(x+(w/2.0), y-(h/2.0));
        glEnd();

    }

    if(stroke_arr[0] >= 0.0) {
        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            glVertex2f(x-(w/2.0), y-(h/2.0));
            glVertex2f(x-(w/2.0), y+(h/2.0));
            glVertex2f(x+(w/2.0), y+(h/2.0));
            glVertex2f(x+(w/2.0), y-(h/2.0));
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

    char* stroke = malloc(100 * sizeof(char));
    char* fill = malloc(100 * sizeof(char));

    stroke = "1.0 1.0 0.0";
    fill = "1.0 0.0 1.0";

    glClearColor(0.0, 0.0, 0.0, 0.0);         // black background
    glMatrixMode(GL_PROJECTION);              // setup viewing projection
    glLoadIdentity();                           // start with identity matrix
    glOrtho(0.0, w, 0.0, h, 0.0, 1.0);   // setup a 10x10x2 viewing world


    // add_circle(0 + w/2, 0 + h/2, 100, stroke, 2.0, fill);
    // add_square(20, 20, 300, stroke, 5.0, fill);
    // add_rectangle(20 + w/2, 20 + h/2, 300, 10, stroke, 5.0, fill);

    // add_ellipse(w/2,s h/2, 50, 200, stroke, 5.0, fill);

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
