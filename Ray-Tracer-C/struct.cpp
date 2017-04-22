#include <iostream>
#include <vector>
#include <cmath>
#include "struct.hpp"

Camera::Camera() {
	this->center = glm::vec3(0, 0, 0);
	this->at = glm::vec3(0, 0, 0);
	this->up = glm::vec3(0, 0, 0);
	this->fovy = 0;
}

Camera::Camera(glm::vec3 center, glm::vec3 at, glm::vec3 up, double fovy) {
	this->center = center;
	this->at = at;
	this->up = up;
	this->fovy = fovy;
}

Image::Image() {
	this->width = 0;
	this->height = 0;
	this->n = 3;
	this->data = NULL;
}

Image::Image(int width, int height, int n, unsigned char *data) {
	this->width = width;
	this->height = height;
	this->n = n;
	this->data = data;
}

Light::Light(glm::vec3 position, glm::vec3 color, glm::vec3 attenuation) {
	this->position = position;
	this->color = color;
	this->attenuation = attenuation;
}

Ray::Ray(glm::vec3 origin, glm::vec3 direction) {
	this->origin = origin;
	this->direction = direction;
}

Pigment::~Pigment() {

}

Solid::Solid(glm::vec3 color) : Pigment() {
	this->color = color;
	this->type = SOLID;
}

Solid::Solid(double red, double green, double blue) : Pigment() {
	glm::vec3 col = glm::vec3(red, green, blue);
	this->color = col;
	this->type = SOLID;
}

Solid::~Solid() {

}

void Solid::debug_info() {
	cout << "Solid" << endl;
	cout << "type" << type << "color" << glm::to_string(color) << endl;
}

glm::vec3 Solid::getColor(const glm::vec3 pos) {
	return color;
}

PigmentType Solid::getType() {
	return SOLID;
}

CheckerBoard::CheckerBoard(glm::vec3 color0, glm::vec3 color1, double sideLen) : Pigment() {
	this->color0 = color0;
	this->color1 = color1;
	this->type = CHECKERBOARD;
	this->sideLen = sideLen;
}

CheckerBoard::CheckerBoard(double red0, double green0, double blue0, double red1, double green1, double blue1, double sideLen) : Pigment() {
	glm::vec3 col0 = glm::vec3(red0, green0, blue0);
	glm::vec3 col1 = glm::vec3(red1, green1, blue1);
	this->color0 = col0;
	this->color1 = col1;
	this->type = CHECKERBOARD;
	this->sideLen = sideLen;
}

CheckerBoard::~CheckerBoard() {

}

void CheckerBoard::debug_info() {
	cout << "CheckerBoard" << endl;
	cout << "type" << type << "C0: " << glm::to_string(color0) << " C1: " << glm::to_string(color1) << " sideLen: " << sideLen << endl;
}

glm::vec3 CheckerBoard::getColor(const glm::vec3 pos) {
	if(int(floor(pos.x/sideLen) + floor(pos.y/sideLen)  + floor(pos.z/sideLen)) % 2 == 0) {
		return color0;
	} else {
		return color1;
	}
}

PigmentType CheckerBoard::getType() {
	return CHECKERBOARD;
}

Surface::Surface(double Ka, double Kd, double Ks, double alpha, double Kr, double Kt, double indexOfRefraction) {
	this->Ka = Ka;
	this->Kd = Kd;
	this->Ks = Ks;
	this->alpha = alpha;
	this->Kr = Kr;
	this->Kt = Kt;
	this->indexOfRefraction = indexOfRefraction;
}

Transformation::Transformation(string transType, glm::vec3 transParams) {
	this->transType = transType;
	this->transParams = transParams;
}
