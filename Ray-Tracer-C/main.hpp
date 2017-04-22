#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include "glm/ext.hpp"

#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <vector>
#include <string>
#include <algorithm>
#include "iostream"
#include <fstream>
#include <sstream>
#include "object.hpp"

using namespace std;

#define NUM_SAMPLES 9
#define RECURSION_DEPTH 20
#define NEAR -1
#define BACKGROUND_COLOR 255*0.5
#define DEBUG 0

string oFileName;
vector<Object*> objects;
vector<Pigment*> pigments;
vector<Surface> surfaces;
vector<Light> lights;
Image image = Image();
Camera camera = Camera();
glm::vec3 specular = glm::vec3(1.0, 1.0, 1.0);
bool isAntialiasing = true;

void read_input(string iFileName, vector<Object*> & objects, vector<Pigment*> & pigments, vector<Surface> & surfaces, vector<Light> & lights, Image & image, Camera & camera);
void debug();
void init();
glm::vec3 send_ray(Ray ray, int depth, bool isDebug);
glm::vec3 phong(Ray ray, glm::vec3 intersectPoint, Object* intersectObject, bool isDebug);
void trace_debug(int c, int r);
