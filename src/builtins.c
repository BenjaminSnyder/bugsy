/*
 *  bugsy's BACKBONE
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
int animationCount = 0;
float l = 0.5;

struct Shape {
    char shape[20];
    char shapeId[10];
    double x;
    double y;
    double n;
    double r;
    double w;
    double h;
    double b;
    double s;
    double x1;
    double y1;
    double x2;
    double y2;
    int num_points;
    char stroke[20];
    double thiccness;
    char fill[20];
    double points[10][2];
} Shape;

struct Animation {
    struct Shape shape;
    char animation[20];
    double translateX;
    double translateY;
    double speed;
    double angle;
    double scale;
} Animation;

struct Shape* shapes;
struct Animation* animations;
int id_len = 7;

void printShapes() {
    for(int i = 0; i < count; i++) {
        struct Shape s = shapes[i];
        fprintf(stderr, "\nShape: %s ID: %s\nx: %f y: %f\nn: %f r: %f\n", s.shape, s.shapeId, s.x, s.y, s.n, s.r);
        fprintf(stderr, "w: %f h: %f\nb: %f s: %f\nx1: %f y1: %f\nx2: %f y2: %f\n", s.w, s.h, s.b, s.s, s.x1, s.y1, s.x2, s.y2);
        fprintf(stderr, "stroke: %s thickness: %f\n:fill %s", s.stroke, s.thiccness, s.fill);
    }
}

void myInit () {
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
    // generates random IDs for the shapes upon creation
    char charset[] = "0123456789"
                     "abcdefghijklmnopqrstuvwxyz"
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    while (length-- > 0) {
        size_t index = (double) rand() / RAND_MAX * (sizeof charset - 1);
        *dest++ = charset[index];
    }
    *dest = '\0';
}

char* add_ellipse(double x, double y, double w, double h,  char* stroke, double thiccness, char* fill, char* id) {

    struct Shape shape;


    char* shapeId = malloc(sizeof(char) * 100);
    if(strcmp(id, "") == 0) {
        size_t len = id_len;
        genId(shapeId, len);
        strcpy(shape.shape, "ellipse");
        strcpy(shape.shapeId, shapeId);
        shape.x = x;
        shape.y = y;
        shape.w = w;
        shape.h = h;
        strcpy(shape.stroke, stroke);
        shape.thiccness = thiccness;
        strcpy(shape.fill, fill);

        shapes[count] = shape;
        count++;

    } else {

        for(int i = 0; i < count; i++) {

            if(strcmp(shapes[i].shapeId, id) == 0) {
                fprintf(stderr, "sdsff: %s %s %f\n", shapes[i].shape, shapes[i].shapeId, shapes[i].w);
                strcpy(shapes[i].shape, "ellipse");
                strcpy(shapes[i].shapeId, id);
                shapes[i].x = x;
                shapes[i].y = y;
                fprintf(stderr, "better be increasing: %f\n", w);
                shapes[i].w = w;
                fprintf(stderr, "frigggg: %f\n", shapes[i].w);
                shapes[i].h = h;
                strcpy(shapes[i].stroke, stroke);
                shapes[i].thiccness = thiccness;
                strcpy(shapes[i].fill, fill);
            }
        }
    }

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
    return shapeId;
}

char* add_circle(double x, double y, double r, char* stroke, double thiccness, char* fill, char* id) {


    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    struct Shape shape;

    char* shapeId = malloc(sizeof(char) * 100);
    if(strcmp(id, "") == 0) {
        fprintf(stderr, "1\n");
        size_t len = id_len;
        genId(shapeId, len);
        fprintf(stderr, "2\n");
        strcpy(shape.shape, "circle");
        strcpy(shape.shapeId, shapeId);
        shape.x = x;
        shape.y = y;
        shape.r = r;
        strcpy(shape.stroke, stroke);
        shape.thiccness = thiccness;
        strcpy(shape.fill, fill);
        fprintf(stderr, "3\n");
        shapes[count] = shape;
        count++;
        fprintf(stderr, "1\n");

    } else {

        for(int i = 0; i < count; i++) {
            struct Shape s = shapes[i];

            if(strcmp(shapes[i].shapeId, id) == 0) {
                strcpy(shapes[i].shape, "circle");
                strcpy(shapes[i].shapeId, id);
                shapes[i].x = x;
                shapes[i].y = y;
                shapes[i].r = r;
                strcpy(shapes[i].stroke, stroke);
                shapes[i].thiccness = thiccness;
                strcpy(shapes[i].fill, fill);
            }
        }
    }

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
    return shapeId;
}

char* add_square(double x, double y, double size, char* stroke, double thiccness, char* fill, char* id) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    struct Shape shape;

    char* shapeId = malloc(sizeof(char) * 100);
    if(id == NULL) {
        size_t len = id_len;
        genId(shapeId, len);
        strcpy(shape.shape, "square");
        strcpy(shape.shapeId, shapeId);
        shape.x = x;
        shape.y = y;
        shape.s = size;
        strcpy(shape.stroke, stroke);
        shape.thiccness = thiccness;
        strcpy(shape.fill, fill);

        shapes[count] = shape;
        count++;

    } else {

        for(int i = 0; i < count; i++) {
            struct Shape s = shapes[i];

            if(strcmp(shapes[i].shapeId, id) == 0) {
                strcpy(shapes[i].shape, "square");
                strcpy(shapes[i].shapeId, id);
                shapes[i].x = x;
                shapes[i].y = y;
                shapes[i].s = size;
                strcpy(shapes[i].stroke, stroke);
                shapes[i].thiccness = thiccness;
                strcpy(shapes[i].fill, fill);
            }
        }
    }

    if(fill_arr[0] >= 0.0) {

        glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
        glBegin(GL_QUADS);
            glVertex2f(x-(size/2.0), y-(size/2.0));
        	glVertex2f(x-(size/2.0), y+(size/2.0));
            glVertex2f(x+(size/2.0), y+(size/2.0));
            glVertex2f(x+(size/2.0), y-(size/2.0));
        glEnd();
    }

    if(stroke[0] >= 0.0) {

        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINE_LOOP);
            glVertex2f(x-(size/2.0), y-(size/2.0));
        	glVertex2f(x-(size/2.0), y+(size/2.0));
            glVertex2f(x+(size/2.0), y+(size/2.0));
            glVertex2f(x+(size/2.0), y-(size/2.0));
        glEnd();
    }
    return shapeId;
}

char* add_triangle(double x, double y, double b, double h, char* stroke, double thiccness, char* fill, char* id) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    struct Shape shape;

    char* shapeId = malloc(sizeof(char) * 100);
    if(id == NULL) {
        size_t len = id_len;
        genId(shapeId, len);
        strcpy(shape.shape, "triangle");
        strcpy(shape.shapeId, shapeId);
        shape.x = x;
        shape.y = y;
        shape.b = b;
        shape.h = h;
        strcpy(shape.stroke, stroke);
        shape.thiccness = thiccness;
        strcpy(shape.fill, fill);

        shapes[count] = shape;
        count++;

    } else {

        for(int i = 0; i < count; i++) {
            // struct Shape s = *(shapes + sizeof(struct Shape) * i);
            // struct Shape* start = shapes;
            struct Shape s = shapes[i];

            if(strcmp(shapes[i].shapeId, id) == 0) {
                strcpy(shapes[i].shape, "triangle");
                strcpy(shapes[i].shapeId, id);
                shapes[i].x = x;
                shapes[i].y = y;
                shapes[i].b = b;
                shapes[i].h = h;
                strcpy(shapes[i].stroke, stroke);
                shapes[i].thiccness = thiccness;
                strcpy(shapes[i].fill, fill);
            }
        }
    }

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
    return shapeId;
}

char* add_rectangle(double x, double y, double w, double h, char* stroke, double thiccness, char* fill, char* id) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    struct Shape shape;

    char* shapeId = malloc(sizeof(char) * 100);
    if(id == NULL) {
        size_t len = id_len;
        genId(shapeId, len);
        strcpy(shape.shape, "rectangle");
        strcpy(shape.shapeId, shapeId);
        shape.x = x;
        shape.y = y;
        shape.w = w;
        shape.h = h;
        strcpy(shape.stroke, stroke);
        shape.thiccness = thiccness;
        strcpy(shape.fill, fill);

        shapes[count] = shape;
        count++;

    } else {

        for(int i = 0; i < count; i++) {
            struct Shape s = shapes[i];

            if(strcmp(shapes[i].shapeId, id) == 0) {
                strcpy(shapes[i].shape, "rectangle");
                strcpy(shapes[i].shapeId, id);
                shapes[i].x = x;
                shapes[i].y = y;
                shapes[i].w = w;
                shapes[i].h = h;
                strcpy(shapes[i].stroke, stroke);
                shapes[i].thiccness = thiccness;
                strcpy(shapes[i].fill, fill);
            }
        }
    }

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
    return shapeId;
}

char* add_line(double x1, double y1, double x2, double y2, char* stroke, double thiccness, char* id) {

    double* stroke_arr = str_to_arr(stroke);

    struct Shape shape;

    char* shapeId = malloc(sizeof(char) * 100);
    if(id == NULL) {
        size_t len = 100;
        genId(shapeId, len);
        strcpy(shape.shape, "line");
        strcpy(shape.shapeId, shapeId);
        shape.x1 = x1;
        shape.y1 = y1;
        shape.x2 = x2;
        shape.y2 = y2;
        strcpy(shape.stroke, stroke);
        shape.thiccness = thiccness;

        shapes[count] = shape;
        count++;

    } else {

        for(int i = 0; i < count; i++) {
            struct Shape s = shapes[i];

            if(strcmp(shapes[i].shapeId, id) == 0) {
                strcpy(shapes[i].shape, "line");
                strcpy(shapes[i].shapeId, id);
                shapes[i].x1 = x1;
                shapes[i].y1 = y1;
                shapes[i].x2 = x2;
                shapes[i].y2 = y2;
                strcpy(shapes[i].stroke, stroke);
                shapes[i].thiccness = thiccness;
            }
        }
    }

    if(stroke_arr[0] >= 0.0) {
        glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
        glLineWidth(thiccness);
        glBegin(GL_LINES);
            glVertex2f(x1, y1);
            glVertex2f(x2, y2);
        glEnd();
    }
    return shapeId;
}

// void add_polygon(int num_points, double points[10][2], char* stroke, double thiccness, char* fill, char* id) {
//
//     double* stroke_arr = str_to_arr(stroke);
//     double* fill_arr = str_to_arr(fill);
//
//     struct Shape shape;
//
//     if(id == NULL) {
//         char* shapeId = malloc(sizeof(char) * 100);
//         size_t len = 100;
//         genId(shapeId, len);
//         strcpy(shape.shape, "polygon");
//         strcpy(shape.shapeId, shapeId);
//         shape.num_points = num_points;
//         shape.points = points;
//         strcpy(shape.stroke, stroke);
//         shape.thiccness = thiccness;
//         strcpy(shape.fill, fill);
//
//         struct Shape* front = shapes;
//         shapes += sizeof(struct Shape) * count;
//         *shapes = shape;
//         count++;
//         shapes = front;
//     } else {
//
//         for(int i = 0; i < count; i++) {
//             // struct Shape s = *(shapes + sizeof(struct Shape) * i);
//             // struct Shape* start = shapes;
//             struct Shape s = *shapes;
//
//             if(strcmp(s.shapeId, id) == 0) {
//                 strcpy(s.shape, "polygon");
//                 strcpy(s.shapeId, id);
//                 s.num_points = num_points;
//                 s.points = points;
//                 strcpy(s.stroke, stroke);
//                 s.thiccness = thiccness;
//                 strcpy(s.fill, fill);
//             }
//             // fprintf(stderr, "circle: %f, %f\n", s.x, shape.x);
//
//         }
//     }
//
//     if(fill_arr[0] >= 0.0) {
//
//         glColor3f(fill_arr[0], fill_arr[1], fill_arr[2]);
//         glBegin(GL_POLYGON);
//             for(int i = 0; i < num_points; i++) {
//                 glVertex2f(points[i][0], points[i][1]);
//             }
//         glEnd();
//
//     }
//
//     if(stroke_arr[0] >= 0.0) {
//         glColor3f(stroke_arr[0], stroke_arr[1], stroke_arr[2]);
//         glLineWidth(thiccness);
//         glBegin(GL_LINE_LOOP);
//             for(int i = 0; i < num_points; i++) {
//                 glVertex2f(points[i][0], points[i][1]);
//             }
//         glEnd();
//
//     }
//
// }

char* add_regagon(double x, double y, int n, double r, char* stroke, double thiccness, char* fill, char* id) {

    double* stroke_arr = str_to_arr(stroke);
    double* fill_arr = str_to_arr(fill);

    double a = 2 / (pi * n);

    struct Shape shape;
    char* shapeId = malloc(sizeof(char) * id_len);
    if(id == NULL) {
        size_t len = id_len;
        genId(shapeId, len);
        strcpy(shape.shape, "regagon");
        strcpy(shape.shapeId, shapeId);
        shape.x = x;
        shape.y = y;
        shape.n = n;
        shape.r = r;
        strcpy(shape.stroke, stroke);
        shape.thiccness = thiccness;
        strcpy(shape.fill, fill);

        shapes[count] = shape;
        count++;

    } else {

        for(int i = 0; i < count; i++) {
            struct Shape s = *shapes;

            if(strcmp(shapes[i].shapeId, id) == 0) {
                strcpy(shapes[i].shape, "regagon");
                strcpy(shapes[i].shapeId, id);
                shapes[i].x = x;
                shapes[i].y = y;
                shapes[i].n = n;
                shapes[i].r = r;
                strcpy(shapes[i].stroke, stroke);
                shapes[i].thiccness = thiccness;
                strcpy(shapes[i].fill, fill);
            }
        }
    }

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
    return shapeId;

}

void scaleBy(struct Shape shape, double scale, double speed) {

    double scaled = 0.0;
    double inc = 0.01;
    double abs_val = scale - 1;
    if(abs_val < 0) {
        abs_val *= -1;
    }
    double time = (speed * 1000000) / (abs_val / inc);
    fprintf(stderr, "time: %f\n", time);

    if(scale < 1) {
        scaled = 1.0;
    }

    double wi = 0.0;
    double hi = 0.0;
    double si = 0.0;
    double ri = 0.0;
    double bi = 0.0;

    if(strcmp(shape.shape, "ellipse") == 0) {
        wi = shape.w;
        hi = shape.h;
    } else if(strcmp(shape.shape, "circle") == 0) {
        ri = shape.r;
    } else if(strcmp(shape.shape, "square") == 0) {
        si = shape.s;
    } else if(strcmp(shape.shape, "triangle") == 0) {
        bi = shape.b;
        hi = shape.h;
    } else if(strcmp(shape.shape, "rectangle") == 0) {
        wi = shape.w;
        hi = shape.h;
    } else if(strcmp(shape.shape, "regagon") == 0) {
        ri = shape.r;
    }

    while(1) {
        if(scale >= 1) {
            if(scaled >= scale - 1) {
                break;
            }
        } else {
            if(scaled <= scale) {
                break;
            }
        }
        glClear(GL_COLOR_BUFFER_BIT);

        for(int i = 0; i < count; i++) {
            struct Shape s = shapes[i];
            if(strcmp(shapes[i].shapeId, shape.shapeId) != 0) {
                fprintf(stderr, "s: %s %s\n", s.shape, s.shapeId);
                if(strcmp(shapes[i].shape, "ellipse") == 0) {
                    add_ellipse(shapes[i].x,
                                shapes[i].y, shapes[i].w, shapes[i].h,
                                shapes[i].stroke,
                                shapes[i].thiccness, shapes[i].fill, shapes[i].shapeId);
                } else if(strcmp(shapes[i].shape, "circle") == 0) {
                    add_circle(shapes[i].x,
                                shapes[i].y, shapes[i].r,
                                shapes[i].stroke,
                                shapes[i].thiccness, shapes[i].fill, shapes[i].shapeId);
                } else if(strcmp(shapes[i].shape, "square") == 0) {
                    add_square(shapes[i].x,
                                shapes[i].y, shapes[i].s,
                                shapes[i].stroke,
                                shapes[i].thiccness, shapes[i].fill, shapes[i].shapeId);
                } else if(strcmp(shapes[i].shape, "triangle") == 0) {
                    add_triangle(shapes[i].x,
                                shapes[i].y, shapes[i].b, shapes[i].h,
                                shapes[i].stroke,
                                shapes[i].thiccness, shapes[i].fill, shapes[i].shapeId);
                } else if(strcmp(shapes[i].shape, "rectangle") == 0) {
                    add_rectangle(shapes[i].x,
                                shapes[i].y, shapes[i].w, shapes[i].h,
                                shapes[i].stroke,
                                shapes[i].thiccness, shapes[i].fill, shapes[i].shapeId);
                } else if(strcmp(shapes[i].shape, "regagon") == 0) {
                    add_regagon(shapes[i].x,
                                shapes[i].y, shapes[i].n, shapes[i].r,
                                shapes[i].stroke,
                                shapes[i].thiccness, shapes[i].fill, shapes[i].shapeId);
                }
                // else if(strcmp(s.shape, "line") == 0) {
                //     add_line(s.start[0],
                //                 s.start[1], s.end[0], s.end[1],
                //                 s.stroke,
                //                 s.thiccness, s.shapeId);
                // }
            }
        }

        if(scale >= 1) {
            scaled += inc;
        } else {
            scaled -= inc;
        }
        // glPushMatrix();
        // glTranslatef(shape.x, shape.y, 0);
        // glScalef(scale, scale, 1);
        //
        // if(strcmp(shape.shape, "ellipse") == 0) {
        //     add_ellipse(0, 0, shape.w, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        // } else if(strcmp(shape.shape, "circle") == 0) {
        //     add_circle(0, 0, shape.r, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        // } else if(strcmp(shape.shape, "square") == 0) {
        //     add_square(0, 0, shape.s, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        // } else if(strcmp(shape.shape, "triangle") == 0) {
        //     add_triangle(0, 0, shape.b, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        // } else if(strcmp(shape.shape, "rectangle") == 0) {
        //     add_rectangle(0, 0, shape.w, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        // } else if(strcmp(shape.shape, "regagon") == 0) {
        //     add_regagon(0, 0, shape.n, shape.r, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        // }
        // else if(strcmp(shape.shape, "regagon") == 0) {
        //     add_line(0, 0, shape.n, shape.r, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        // }

        if(strcmp(shape.shape, "ellipse") == 0) {
            if(scale >= 1) {
                fprintf(stderr, "width: %f\n", wi * (1 + scaled));
                add_ellipse(shape.x, shape.y,  wi * (1 + scaled), hi * (1 + scaled),
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            } else {
                fprintf(stderr, "width: %f\n", wi * scaled);
                add_ellipse(shape.x, shape.y,  wi * scaled, hi * scaled,
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            }
        } else if(strcmp(shape.shape, "circle") == 0) {
            if(scale >= 1) {
                add_circle(shape.x, shape.y,  ri * (1 + scaled),
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            } else {
                add_circle(shape.x, shape.y,  ri * scaled,
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            }
        } else if(strcmp(shape.shape, "square") == 0) {
            if(scale >= 1) {
                add_square(shape.x, shape.y,  si * (1 + scaled),
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            } else {
                add_square(shape.x, shape.y,  si * scaled,
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            }
        } else if(strcmp(shape.shape, "triangle") == 0) {
            if(scale >= 1) {
                add_triangle(shape.x, shape.y,   bi * (1 + scaled), hi * (1 + scaled),
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            } else {
                add_triangle(shape.x, shape.y,  bi * scaled, hi * scaled,
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            }
        } else if(strcmp(shape.shape, "rectangle") == 0) {
            if(scale >= 1) {
                add_rectangle(shape.x, shape.y,   wi * (1 + scaled), hi * (1 + scaled),
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            } else {
                add_rectangle(shape.x, shape.y,  wi * scaled, hi * scaled,
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            }
        } else if(strcmp(shape.shape, "regagon") == 0) {
            if(scale >= 1) {
                add_regagon(shape.x, shape.y,  shape.n, ri * (1 + scaled),
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            } else {
                add_regagon(shape.x, shape.y,  shape.n, ri * scaled,
                        shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
            }
        }
        // else if(strcmp(shape.shape, "line") == 0) {
        //     if(scale >= 1) {
        //         add_line(shape.start[0] * (1 + scaled), shape.start[1], shape.end[0] * (1 + scaled),
        //                 shape.end[1],
        //                 shape.stroke, shape.thiccness, shape.shapeId);
        //     } else {
        //         add_line(shape.start[0] - scaled, shape.start[1], shape.end[0] + scaled,
        //                 shape.end[1],
        //                 shape.stroke, shape.thiccness, shape.shapeId);
        //     }
        //
        // }
        // glPopMatrix();
        glFlush();
        usleep(time);
    }

}

void scaleById(char* id, double scale, double speed) {
    for(int i = 0; i < count; i++) {
        if(strcmp(shapes[i].shapeId, id) == 0) {
            struct Animation a;
            a.shape = shapes[i];
            strcpy(a.animation, "scale");
            a.scale = scale;
            a.speed = speed;
            animations[animationCount] = a;
            animationCount++;
            //moveBy(shapes[i], translateX, translateY, speed);
        }
    }
}

void rotateBy(struct Shape shape, double angle, double speed) {

    double rotated = 0.0;
    double inc = 1 * (angle / abs(angle)) * speed;

    double abs_val = angle;
    if(abs_val < 0) {
        abs_val *= -1;
    }
    double time = (speed * 1000000) / (abs_val / inc);

    struct Shape* front = shapes;
    while(rotated < angle) {
        glClear(GL_COLOR_BUFFER_BIT);
        shapes = front;
        for(int i = 0; i < count; i++) {
            struct Shape s = shapes[i];

            if(strcmp(s.shapeId, shape.shapeId) != 0) {
                if(strcmp(s.shape, "ellipse") == 0) {
                    add_ellipse(s.x,
                                s.y, s.w, s.h,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "circle") == 0) {
                    add_circle(s.x,
                                s.y, s.r,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "square") == 0) {
                    add_square(s.x,
                                s.y, s.s,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "triangle") == 0) {
                    add_triangle(s.x,
                                s.y, s.b, s.h,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "rectangle") == 0) {
                    add_rectangle(s.x,
                                s.y, s.w, s.h,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "regagon") == 0) {
                    add_regagon(s.x,
                                s.y, s.n, s.r,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                }
            }
        }

        rotated += inc;

        glPushMatrix();
        glTranslatef(shape.x, shape.y, 0);
        glRotatef(rotated, 0, 0, 1);

        if(strcmp(shape.shape, "ellipse") == 0) {
            add_ellipse(0, 0, shape.w, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "circle") == 0) {
            add_circle(0, 0, shape.r, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "square") == 0) {
            add_square(0, 0, shape.s, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "triangle") == 0) {
            add_triangle(0, 0, shape.b, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "rectangle") == 0) {
            add_rectangle(0, 0, shape.w, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "regagon") == 0) {
            add_regagon(0, 0, shape.n, shape.r, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        }

        glPopMatrix();

        glFlush();
        usleep(time);
    }
}

void rotateById(char* id, double angle, double speed) {
    for(int i = 0; i < count; i++) {
        if(strcmp(shapes[i].shapeId, id) == 0) {
            struct Animation a;
            a.shape = shapes[i];
            strcpy(a.animation, "rotate");
            a.angle = angle;
            a.speed = speed;
            animations[animationCount] = a;
            animationCount++;
            //moveBy(shapes[i], translateX, translateY, speed);
        }
    }
}

void moveBy(struct Shape shape, double translateX, double translateY, double speed) {
    fprintf(stderr, "moveBy\n");
    double movedX = 0.0;
    double movedY = 0.0;
    double incX = 1 * (translateX / abs(translateX));
    double incY = 1 * (translateY / abs(translateY));

    if(translateX > translateY) {
        incX = translateX / abs(translateY);
    } else {
        incY = translateY / abs(translateX);
    }

    double abs_val = translateX;
    if(abs_val < 0) {
        abs_val *= -1;
    }
    double time = (speed * 1000000) / (abs_val / incX);

    struct Shape* front = shapes;
    while(movedX < translateX) {
        glClear(GL_COLOR_BUFFER_BIT);
        shapes = front;
        for(int i = 0; i < count; i++) {
            struct Shape s = shapes[i];

            if(strcmp(s.shapeId, shape.shapeId) != 0) {
                if(strcmp(s.shape, "ellipse") == 0) {
                    add_ellipse(s.x,
                                s.y, s.w, s.h,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "circle") == 0) {
                    add_circle(s.x,
                                s.y, s.r,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "square") == 0) {
                    add_square(s.x,
                                s.y, s.s,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "triangle") == 0) {
                    add_triangle(s.x,
                                s.y, s.b, s.h,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "rectangle") == 0) {
                    add_rectangle(s.x,
                                s.y, s.w, s.h,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "regagon") == 0) {
                    add_regagon(s.x,
                                s.y, s.n, s.r,
                                s.stroke,
                                s.thiccness, s.fill, s.shapeId);
                } else if(strcmp(s.shape, "line") == 0) {
                    add_line(s.x1,
                                s.y1, s.x2, s.y2,
                                s.stroke,
                                s.thiccness, s.shapeId);
                }
            }
        }

        movedX += incX;
        movedY += incY;

        if(strcmp(shape.shape, "ellipse") == 0) {
            add_ellipse(shape.x + movedX, shape.y + movedY, shape.w, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "circle") == 0) {
            add_circle(shape.x + movedX, shape.y + movedY, shape.r, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "square") == 0) {
            add_square(shape.x + movedX, shape.y + movedY, shape.s, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "triangle") == 0) {
            add_triangle(shape.x + movedX, shape.y + movedY, shape.b, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "rectangle") == 0) {
            add_rectangle(shape.x + movedX, shape.y + movedY, shape.w, shape.h, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "regagon") == 0) {
            add_regagon(shape.x + movedX, shape.y + movedY, shape.n, shape.r, shape.stroke, shape.thiccness, shape.fill, shape.shapeId);
        } else if(strcmp(shape.shape, "line") == 0) {
            add_line(shape.x1 + movedX, shape.y1 + movedY, shape.x2 + movedX, shape.y2 + movedY, shape.stroke, shape.thiccness, shape.shapeId);
        }

        glFlush();
        usleep(time);
    }
}

void moveById(char* id, double translateX, double translateY, double speed) {
    fprintf(stderr, "moveById\n");
    for(int i = 0; i < count; i++) {
        if(strcmp(shapes[i].shapeId, id) == 0) {
            struct Animation a;
            a.shape = shapes[i];
            strcpy(a.animation, "move");
            a.translateX = translateX;
            a.translateY = translateY;
            a.speed = speed;
            animations[animationCount] = a;
            animationCount++;
            //moveBy(shapes[i], translateX, translateY, speed);
        }
    }
}

double map(double a, double b, double x) {
    return (x - a) / (b - a) ;
}

void init_canvas() {
    shapes = malloc(sizeof(struct Shape) * 100);
    animations = malloc(sizeof(struct Animation) * 100);
}

void add_canvas(double width, double height, double xOffset, double yOffset) {

    int c = 0;
    char ** args;
    double aspect = width / height;
    // shapes = malloc(sizeof(struct Shape) * 100);

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

    glClearColor(0.0, 0.0, 0.0, 0.0);       // black background
    glMatrixMode(GL_PROJECTION);            // setup viewing projection
    glLoadIdentity();                       // start with identity matrix
    glOrtho(0.0, w, 0.0, h, 0.0, 1.0);      // setup a 10x10x2 viewing world

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
    // add_rectangle(cx-25, cy+75, 225, 75, stroke, 3.0, fill, NULL);
    // add_rectangle(cx-25, cy-75, 225, 75, stroke, 3.0, fill, NULL);
    // add_rectangle(cx+100, cy, 75, 225, stroke, 3.0, fill, NULL);
    // add_rectangle(cx+125, cy-50, 150, 225, stroke, 3.0, fill, NULL);
    //
    // add_rectangle(cx+87.5, cy+125, 100, 50, stroke, 5.0, fill, NULL);
    //
    // fill = "0.0 0.0 0.0";
    // add_circle(cx+100, cy+125, 15, stroke, 5.0, fill, NULL);
    // fill = "1.0 1.0 1.0";
    // add_circle(cx+100, cy+125, 7.5, stroke, 5.0, fill, NULL);
    // fill = "0.0 0.25 0.0";
    // add_ellipse(cx-100, h/2+85, 10, 5, stroke, 5.0, fill, NULL);
    //
    // fill = "1.0 1.0 1.0";
    // add_triangle(cx+30, cy-22, 30, 30, stroke, 5.0, fill, NULL);
    // add_triangle(cx, cy-22, 30, 30, stroke, 5.0, fill, NULL);
    // add_triangle(cx-30, cy-22, 30, 30, stroke, 5.0, fill, NULL);
    // add_triangle(cx-60, cy-22, 30, 30, stroke, 5.0, fill, NULL);
    // add_triangle(cx-90, cy-22, 30, 30, stroke, 5.0, fill, NULL);
    // add_triangle(cx-120, cy-22, 30, 30, stroke, 5.0, fill, NULL);
    //
    // add_triangle_down(cx+30, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx-30, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx-60, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx-90, cy+22, 30, 30, stroke, 5.0, fill);
    // add_triangle_down(cx-120, cy+22, 30, 30, stroke, 5.0, fill);
    //
    // add_ellipse(cx+25, cy, 100, 75, stroke, 5.0, fill, NULL);
    // fill = "0.5 0.6 0.1";
    // add_ellipse(cx-100, cy+100, 50, 75, stroke, 5.0, fill, NULL);
    //
    // fill = "0.1 0.2 0.4";
    // add_square(cx-250, cy+75, 50, stroke, 3.0, fill, NULL);
    //
    // add_triangle(cx-25, cy, 100, 75, stroke, 3.0, fill, NULL);
    // fprintf(stderr, "count: %d\n", count);
    // add_triangle(cx-200, cy, 100, 75, stroke, 3.0, fill, NULL);
    // for(int i = 0; i < 10; i++) {
    //     fprintf(stderr, "shape: %s: %s\n", shapes[i].shapeId, shapes[i].shape);
    // }
    //
    // add_regagon(cx, cy+50, 5, 50, stroke, 3.0, fill, NULL);
    // add_rectangle(cx-30, cy, 100, 2, stroke, 1.0, fill, NULL);
    //
    // // scaleBy(shapes[1], 1.2, 1);
    // stroke = "0.5 0.6 0.1";
    // add_line(cx-100, cy+200, cx+50, cy+200, stroke, 3.0, NULL);
    //
    // for(int i = 0; i < 10; i++) {
    //     fprintf(stderr, "shape: %s\n", shapes[i].shape);
    // }
    // rotateBy(shapes[0], 360, 5);
    // moveBy(shapes[0], 300, 300, 1);

    // scaleBy(shapes[0], 1.5, 5);
    // scaleBy(shapes[1], 1.5, 5);
    // scaleBy(shapes[2], 1.5, 5);
    // scaleBy(shapes[3], 1.5, 5);
    // scaleBy(shapes[4], 1.5, 5);

    // moveBy(shapes[0], 100, 300, 5);
    // moveBy(shapes[1], 30, 30, 1);
    // sleep(5);
    // exit(0);

    // for(int i = 0; i < 5; i++) {
    //     scaleBy(shapes[i], 1.2, 1);
    // }
    printShapes();
    fprintf(stderr, "hellol\n");

    // while(1) {
        for(int i = 0; i < count; i++) {
            struct Shape s = shapes[i];
            if(strcmp(s.shape, "ellipse") == 0) {
                add_ellipse(s.x,
                            s.y, s.w, s.h,
                            s.stroke,
                            s.thiccness, s.fill, s.shapeId);
            } else if(strcmp(s.shape, "circle") == 0) {
                add_circle(s.x,
                            s.y, s.r,
                            s.stroke,
                            s.thiccness, s.fill, s.shapeId);
            } else if(strcmp(s.shape, "square") == 0) {
                add_square(s.x,
                            s.y, s.s,
                            s.stroke,
                            s.thiccness, s.fill, s.shapeId);
            } else if(strcmp(s.shape, "triangle") == 0) {
                add_triangle(s.x,
                            s.y, s.b, s.h,
                            s.stroke,
                            s.thiccness, s.fill, s.shapeId);
            } else if(strcmp(s.shape, "rectangle") == 0) {
                add_rectangle(s.x,
                            s.y, s.w, s.h,
                            s.stroke,
                            s.thiccness, s.fill, s.shapeId);
            } else if(strcmp(s.shape, "regagon") == 0) {
                add_regagon(s.x,
                            s.y, s.n, s.r,
                            s.stroke,
                            s.thiccness, s.fill, s.shapeId);
            } else if(strcmp(s.shape, "line") == 0) {
                add_line(s.x1,
                            s.y1, s.x2, s.y2,
                            s.stroke,
                            s.thiccness, s.shapeId);
            }
        }

        for(int i = 0; i < animationCount; i++) {
            struct Animation a = animations[i];
            if(strcmp(a.animation, "move") == 0) {
                moveBy(a.shape, a.translateX, a.translateY, a.speed);
            }
            else if(strcmp(a.animation, "rotate") == 0) {
                rotateBy(a.shape, a.angle, a.speed);
            }
            else if(strcmp(a.animation, "scale") == 0) {
                scaleBy(a.shape, a.scale, a.speed);
            }
        }

    // }


    glFlush();
    fprintf(stderr, "%s\n", getenv("DEBUG"));
    if(strcmp(getenv("DEBUG"), "1") != 0) {
        glutMainLoop();
    }




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
