#ifndef OBJECT_H
#define OBJECT_H

#include <vector>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include "glm/ext.hpp"
#include "struct.hpp"

using namespace std;

enum ObjectType {SPHERE, PLANE, POLYHEDRON};

struct Object {
	int pigmentIndex;
	int surfaceIndex;

	Object(int pigmentIndex, int surfaceIndex);
	virtual ~Object() = 0;
	// virtual void transform() = 0;
	virtual double getIntersection(const Ray ray) = 0;
	virtual glm::vec3 getNormal(const glm::vec3 point) = 0;
	virtual ObjectType getType() = 0;
	virtual void debug_info() = 0;
};

struct Sphere : Object {
	glm::vec3 center;
	double radius;
	ObjectType type;
	vector<Transformation> transformations;

	Sphere(glm::vec3 center, double radius, vector<Transformation> transformations, int pigmentIndex, int surfaceIndex);
	~Sphere();
	void transform();
	double getIntersection(const Ray ray);
	glm::vec3 getNormal(const glm::vec3 point);
	ObjectType getType();
	void debug_info();
};

struct Plane : Object {
	glm::vec4 plane_coefficient;
	ObjectType type;
	vector<Transformation> transformations;

	Plane(glm::vec4 plane_coefficient, vector<Transformation> transformations, int pigmentIndex, int surfaceIndex);
	~Plane();
	void transform();
	double getIntersection(const Ray ray);
	glm::vec3 getNormal(const glm::vec3 point);
	ObjectType getType();
	void debug_info();
};

struct Polyhedron : Object {
	int plane_index;
	int numFaces;
	vector<glm::vec4> poly_coefficients;
	ObjectType type;
	Polyhedron(int numFaces, vector<glm::vec4> poly_coefficients, int pigmentIndex, int surfaceIndex);
 	~Polyhedron();
	// void transform();
	bool intersectPolyhedron(const glm::vec3 point, const vector<glm::vec4> poly_coefficients);
	double getIntersection(const Ray ray);
	glm::vec3 getNormal(const glm::vec3 point);
	ObjectType getType();
	void debug_info();
};

// struct Cylinder : Object {
// 	glm::vec3 center;
// 	double radius, height;
// 	glm::vec3 upVector;
//	ObjectType type;

// 	Cylinder(glm::vec3 center, glm::vec3 upVector, int pigmentIndex, int surfaceIndex);
//	~Cylinder();
// 	double getIntersection(const Ray ray);
// 	glm::vec3 getNormal(const glm::vec3 point);
//  ObjectType getType();
//  void debug_info();
// };

// struct Cone : Object {
// 	double radius, height, alpha;
// 	glm::vec3 upVector;
// 	glm::vec3 center;
//	ObjectType type;

// 	Cone(double radius, double height, double alpha, glm::vec3 center, glm::vec3 upVector, int pigmentIndex, int surfaceIndex);
//	~Cone();
// 	double getIntersection(const Ray ray);
// 	glm::vec3 getNormal(const glm::vec3 point);
//  ObjectType getType();
//  void debug_info();
// };

// struct Triangle : Object {
// 	glm::vec3 p1, p2, p3;
// 	ObjectType type;

// 	Triangle(glm::vec3 p1, glm::vec3 p2, glm::vec3 p3, int pigmentIndex, int surfaceIndex);
// 	~Triangle();
// 	double getIntersection(const Ray ray);
// 	glm::vec3 getNormal(const glm::vec3 point);
//  ObjectType getType();
//  void debug_info();
// };

#endif