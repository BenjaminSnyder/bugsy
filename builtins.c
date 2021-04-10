/*
 *  A function illustrating how to link C code to code generated from LLVM
 */

#include <stdio.h>
#include <GL/glut.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#define pi 3.142857

float s = 1;
int count = 0;
float l = 0.5;

// struct shape {
//     int circle;
//     int ellipse;
//     int square;
//     int rectangle;
//     int triangle;
//     int polygon;
//     int regagon;
// } shape;

// struct circle {
//     char shape[20];
//     double x;
//     double y;
//     double r;
//     char* stroke;
//     double thiccness;
//     char* fill;
// } circle;
//
// struct ellipse {
//     char shape[20];
//     double x;
//     double y;
//     double w;
//     double h;
//     char* stroke;
//     double thiccness;
//     char* fill;
// } ellipse;
//
// struct square {
//     char shape[20];
//     double x;
//     double y;
//     double s;
//     char* stroke;
//     double thiccness;
//     char* fill;
// } square;
//
// struct rectangle {
//     char shape[20];
//     double x;
//     double y;
//     double w;
//     double h;
//     char* stroke;
//     double thiccness;
//     char* fill;
// } rectangle;
//
// struct polygon {
//     char shape[20];
//     int num_points;
//     double points[][2];
//     char* stroke;
//     double thiccness;
//     char* fill;
// } polygon;
//
// struct triangle {
//     char shape[20];
//     double x;
//     double y;
//     double b;
//     double h;
//     char* stroke;
//     double thiccness;
//     char* fill;
// } triangle;
//
// struct regagon {
//     char shape[20];
//     double x;
//     double y;
//     double n;
//     double r;
//     char* stroke;
//     double thiccness;
//     char* fill;
// } regagon;

struct Shape {
    char shape[20];
    char shapeId[100];
    double x;
    double y;
    double n;
    double r;
    double w;
    double h;
    double b;
    int num_points;
    char stroke[20];
    double thiccness;
    char fill[20];
    double points[10][2];
} Shape;

struct Shape* shapes;

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

void genId(char *dest, size_t length) {
    char charset[] = "0123456789"
                     "abcdefghijklmnopqrstuvwxyz"
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    while (length-- > 0) {
        size_t index = (double) rand() / RAND_MAX * (sizeof charset - 1);
        *dest++ = charset[index];
    }
    *dest = '\0';
}

void add_ellipse(double x, double y, double w, double h,  char* stroke, double thiccness, char* fill, char* id) {




    struct Shape ellipse;

    if(id == NULL) {
        char* shapeId = malloc(sizeof(char) * 100);
        size_t len = 100;
        genId(shapeId, len);
        strcpy(ellipse.shape, "ellipse");
        strcpy(ellipse.shapeId, shapeId);
        ellipse.x = x;
        ellipse.y = y;
        ellipse.w = w;
        ellipse.h = h;
        strcpy(ellipse.stroke, stroke);
        ellipse.thiccness = thiccness;
        strcpy(ellipse.fill, fill);

        struct Shape* front = shapes;
        shapes += sizeof(struct Shape) * count;
        *shapes = ellipse;
        count++;
        shapes = front;
    } else {

        for(int i = 0; i < count; i++) {
            // struct Shape s = *(shapes + sizeof(struct Shape) * i);
            // struct Shape* start = shapes;
            struct Shape s = *shapes;

            if(strcmp(s.shapeId, id) == 0) {
                strcpy(s.shape, "ellipse");
                strcpy(s.shapeId, id);
                s.x = x;
                s.y = y;
                s.w = w;
                s.h = h;
                strcpy(s.stroke, stroke);
                s.thiccness = thiccness;
                strcpy(s.fill, fill);
            }
            // fprintf(stderr, "%f, %f\n", s.x, shape.x);
        }

    }


    // fprintf(stderr, "%s: %f\n", shapes[0].shapeId, shapes[0].x);

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

// void get_rotation(double x, double y, double a, double arr[], double translateX, double translateY) {
//     x += translateX
//     y += translateY
//     arr[0] = x * cos(a) - y * sin(a);
//     arr[1] = x * sin(a) + y * cos(a);
//
// }

void add_triangle(double x, double y, double b, double h, char* stroke, double thiccness, char* fill) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    // double arr[2];
    // get_rotation(x, y, 90, arr);
    //
    // x = arr[0];
    // y = arr[1];
    //
    // fprintf(stderr, "%f", x);
    // fprintf(stderr, "%f\n", y);

    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_TRIANGLES);
            glVertex2f(x, y+(h/2.0));
        	glVertex2f(x-(b/2.0), y-(h/2.0));
            glVertex2f(x+(b/2.0), y-(h/2.0));
        glEnd();

    }

    if(stroke[0] >= 0.0) {

        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            glVertex2f(x, y+(h/2.0));
            glVertex2f(x-(b/2.0), y-(h/2.0));
            glVertex2f(x+(b/2.0), y-(h/2.0));
        glEnd();

    }

}

void add_triangle_down(double x, double y, double b, double h, char* stroke, double thiccness, char* fill) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);


    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_TRIANGLES);
        	glVertex2f(x-(b/2.0), y+(h/2.0));
            glVertex2f(x+(b/2.0), y+(h/2.0));
            glVertex2f(x, y-(h/2));
        glEnd();

    }

    if(stroke[0] >= 0.0) {

        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            glVertex2f(x-(b/2.0), y+(h/2.0));
            glVertex2f(x+(b/2.0), y+(h/2.0));
            glVertex2f(x, y-(h/2));
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

void add_line(double x1, double y1, double x2, double y2, char* stroke, double thiccness) {

    double* stroke_arr = str_to_arr(stroke);

    if(stroke_arr[0] >= 0.0) {
        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINES);
            glVertex2f(x1, y1);
            glVertex2f(x2, y2);
        glEnd();

    }

}

void add_polygon(int num_points, double points[10][2], char* stroke, double thiccness, char* fill) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_POLYGON);
            for(int i = 0; i < num_points; i++) {
                glVertex2f(points[i][0], points[i][1]);
            }
        glEnd();

    }

    if(stroke_arr[0] >= 0.0) {
        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            for(int i = 0; i < num_points; i++) {
                glVertex2f(points[i][0], points[i][1]);
            }
        glEnd();

    }

}

void add_regagon(double x, double y, int n, double r, char* stroke, double thiccness, char* fill) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    double a = 2 / (pi * n);

    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_POLYGON);
            for(int i = 0; i < n; i++) {
                double x_i = x + r * cos(2 * pi * i / n);
                double y_i = y + r * sin(2 * pi * i / n);
                glVertex2f(x_i, y_i);
            }
        glEnd();

    }

    if(stroke_arr[0] >= 0.0) {
        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            for(int i = 0; i < n; i++) {
                double x_i = x + r * cos(2 * pi * i / n);
                double y_i = y + r * sin(2 * pi * i / n);
                glVertex2f(x_i, y_i);
            }
        glEnd();

    }

}

void moveBy(struct Shape shape, double translateX, double translateY, double speed) {

    double movedX = 0.0;
    double movedY = 0.0;
    double incX = 1 * (translateX / abs(translateX));
    double incY = 1 * (translateY / abs(translateY));

    struct Shape* front = shapes;
    while(movedX < translateX) {
        glClear(GL_COLOR_BUFFER_BIT);
        shapes = front;
        for(int i = 0; i < 3; i++) {
            // struct Shape s = *(shapes + sizeof(struct Shape) * i);
            // struct Shape* start = shapes;
            struct Shape s = *shapes;

            if(strcmp(s.shapeId, shape.shapeId) != 0) {
                if(strcmp(s.shapeId, "ellipse") != 0) {
                    add_ellipse(s.x,
                                s.y, s.w, s.h,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                                // fprintf(stderr, "arr num: %d", i);
                }
            }
            // fprintf(stderr, "%f, %f\n", s.x, shape.x);
            shapes += sizeof(struct Shape);
        }

        movedX += incX;
        movedY += incY;
        fprintf(stderr, "%s: %f, %f, %f, %f\n", shape.shapeId, shape.x + movedX, shape.y + movedX, shape.w, shape.h);
        // fprintf(stderr, "movedX = %f\n", shape.x + movedX);
        add_ellipse(shape.x + movedX, shape.y + movedY, shape.w, shape.h,
                    shape.stroke, shape.thiccness, shape.fill, shape.shapeId);

        glFlush();
        usleep(100000);

    }
}


void add_canvas(double width, double height, double xOffset, double yOffset) {

    int c = 0;
    char ** args;
    double aspect = width / height;
    shapes = malloc(sizeof(struct Shape) * 100);

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

    stroke = "-1.0 0.0 0.0";
    fill = "0.0 0.6 0.1";

    glClearColor(0.0, 0.0, 0.0, 0.0);         // black background
    glMatrixMode(GL_PROJECTION);              // setup viewing projection
    glLoadIdentity();                           // start with identity matrix
    glOrtho(0.0, w, 0.0, h, 0.0, 1.0);   // setup a 10x10x2 viewing world


    // add_circle(w/2, h/2-200, 25, stroke, 2.0, fill);
    // add_square(w/2-100, h/2+100, 50, stroke, 5.0, fill);
    //
    // stroke = "0.5 0.0 0.5";
    // fill = "1.0 0.2 0.2";
    //
    // add_triangle(w/2+100, h/2+100, 50, 50, stroke, 5.0, fill);
    // add_line(w/2-100, h/2+200, w/2+100, h/2+200, stroke, 2.0);
    // add_ellipse(w/2-100, h/2-100, 50, 25, stroke, 5.0, fill);
    //
    // double arr[][2] = { {w/2-50, h/2+50}, {w/2+50, h/2 + 50}, {w/2+70, h/2}, {w/2, h/2-100}, {w/2-70, h/2}};
    // add_polygon(5, arr, stroke, 2.0, fill);
    // stroke = "0.5 1.0 0.5";
    // fill = "1.0 1.0 0.2";
    // add_regagon(w/2+100, h/2-100, 10, 60, stroke, 3, fill);

    //dino
    double cx = w/2;
    double cy = h/2;
    //
    // add_rectangle(cx-25, cy+75, 225, 75, stroke, 3.0, fill);
    // add_rectangle(cx-25, cy-75, 225, 75, stroke, 3.0, fill);
    // add_rectangle(cx+100, cy, 75, 225, stroke, 3.0, fill);
    // add_rectangle(cx+125, cy-50, 150, 225, stroke, 3.0, fill);
    //
    // add_rectangle(cx+87.5, cy+125, 100, 50, stroke, 5.0, fill);
    //
    // fill = "0.0 0.0 0.0";
    // add_circle(cx+100, cy+125, 15, stroke, 5.0, fill);
    // fill = "1.0 1.0 1.0";
    // add_circle(cx+100, cy+125, 7.5, stroke, 5.0, fill);
    // fill = "0.0 0.25 0.0";
    // add_ellipse(cx-100, h/2+85, 10, 5, stroke, 5.0, fill);
    //
    // fill = "1.0 1.0 1.0";
    // add_triangle(cx+30, cy-22, 30, 30, stroke, 5.0, fill);
    // add_triangle(cx, cy-22, 30, 30, stroke, 5.0, fill);
    // add_triangle(cx-30, cy-22, 30, 30, stroke, 5.0, fill);
    // add_triangle(cx-60, cy-22, 30, 30, stroke, 5.0, fill);
    // add_triangle(cx-90, cy-22, 30, 30, stroke, 5.0, fill);
    // add_triangle(cx-120, cy-22, 30, 30, stroke, 5.0, fill);
    //
    // add_triangle_down(cx+30, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx-30, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx-60, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx-90, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx-120, cy+22, 30, 30, stroke, 5.0, fill);

    add_ellipse(cx+25, cy, 50, 75, stroke, 5.0, fill, NULL);
    fill = "0.5 0.6 0.1";
    add_ellipse(cx-100, cy+100, 50, 75, stroke, 5.0, fill, NULL);
    add_ellipse(cx-20, cy+200, 50, 75, stroke, 5.0, fill, NULL);
    fprintf(stderr, "%f\n", shapes[2].x);
    moveBy(shapes[0], 300, 300, 1);

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
