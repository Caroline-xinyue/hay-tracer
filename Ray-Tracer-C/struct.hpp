#ifndef STRUCT_H
#define STRUCT_H

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include "glm/ext.hpp"
#include "string"

using namespace std;

enum PigmentType {SOLID, CHECKERBOARD};

struct Camera {
	glm::vec3 center;
	glm::vec3 at;
	glm::vec3 up;
	double fovy;
	Camera();
	Camera(glm::vec3 center, glm::vec3 at, glm::vec3 up, double fovy);
};

struct Image {
	int width;
	int height;
	int n = 3;
	unsigned char *data; // maybe use string?
	Image();
	Image(int width, int height, int n, unsigned char *data);
};

struct Light {
	glm::vec3 position;
	glm::vec3 color;
	glm::vec3 attenuation;
	Light();
	Light(glm::vec3 position, glm::vec3 color, glm::vec3 attenuation);
};

struct Ray {
	glm::vec3 origin;
	glm::vec3 direction;
	Ray();
	Ray(glm::vec3 origin, glm::vec3 direction);
};

struct Pigment {
	virtual ~Pigment() = 0;
	virtual glm::vec3 getColor(const glm::vec3 pos) = 0;
	virtual PigmentType getType() = 0;
	virtual void debug_info() = 0;
};

struct Solid : Pigment {
	glm::vec3 color;
	PigmentType type = SOLID;
	Solid();
	Solid(glm::vec3 color);
	Solid(double red, double green, double blue);
	~Solid();
	glm::vec3 getColor(const glm::vec3 pos);
	PigmentType getType();
	void debug_info();
};

struct CheckerBoard : Pigment {
	glm::vec3 color0, color1;
	PigmentType type = CHECKERBOARD;
	double sideLen;
	CheckerBoard();
	CheckerBoard(glm::vec3 color0, glm::vec3 color1, double sideLen);
	CheckerBoard(double red0, double green0, double blue0, double red1, double green1, double blue1, double sideLen);
	~CheckerBoard();
	glm::vec3 getColor(const glm::vec3 pos);
	PigmentType getType();
	void debug_info();
};

struct Surface {
	double Ka, Kd, Ks, alpha; // Phong coefficients
	double Kr, Kt; // Kt: transparency
	double indexOfRefraction;
	Surface();
	Surface(double Ka, double Kd, double Ks, double alpha, double Kr, double Kt, double indexOfRefraction);
};

struct Transformation {
	string transType;
	glm::vec3 transParams;
	Transformation();
	Transformation(string transType, glm::vec3 transParams);
};

#endif