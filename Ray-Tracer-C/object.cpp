#include <iostream>
#include <vector>
#include <cmath>
#include "object.hpp"

Object::Object(int pigmentIndex, int surfaceIndex) {
	this->pigmentIndex = pigmentIndex;
	this->surfaceIndex = surfaceIndex;
}

Object::~Object() {

}

Sphere::Sphere(glm::vec3 center, double radius, vector<Transformation> transformations, int pigmentIndex, int surfaceIndex) : Object(pigmentIndex, surfaceIndex) {
	this->center = center;
	this->radius = radius;
	this->type = SPHERE;
	this->transformations = transformations;
}

Sphere::~Sphere() {

}

void Sphere::transform() {
	for(unsigned int i = 0; i < transformations.size(); i++) {
		if(transformations[i].transType == "scale") {
			// cout << "scale" << endl;
			radius = radius * transformations[i].transParams.x;
		} else if(transformations[i].transType == "translate") {
			// cout << "translate" << endl;
			center = center + transformations[i].transParams;
		}
	}
}

double Sphere::getIntersection(const Ray ray) {
	glm::vec3 origin = ray.origin;
	glm::vec3 direction = ray.direction;
	// cout << glm::to_string(direction) << endl;
	glm::vec3 center_origin = origin - center;
	double A = glm::dot(direction, direction); // pow(glm::length(direction), 2)
	double B = 2 * glm::dot(center_origin, direction);
	double C = glm::dot(center_origin, center_origin) - pow(radius, 2);
	// cout << "origin" << glm::to_string(origin) << "\n direction" << glm::to_string(direction) << "\n center_origin" << glm::to_string(center_origin) << endl;
	// cout << "A: " << A << " B: " << B << " C: " << C << endl;
	double delta = pow(B, 2) - 4 * A * C;
	// cout << delta << endl;

	if(delta < 0) {
		// no solution
		return -1;
	} else if(delta == 0) {
		// one solution
		double t = -B / (2 * A);
		if(t < 0) {
			return -1;
		} else {
			return t;
		}
	} else {
		// two solutions
		double t1 = (-B + sqrt(delta)) / (2 * A);
		double t2 = (-B - sqrt(delta)) / (2 * A);
		if(t1 < 0) {
			if(t2 < 0) {
				return -1;
			} else {
				return t2;
			}
		} else {
			if(t2 < 0) {
				return t1;
			} else {
				return min(t1, t2);
			}
		}
	}
}

glm::vec3 Sphere::getNormal(const glm::vec3 point) {
	// transform();
	glm::vec3 normal = glm::normalize(point - center);
	return normal;
}

ObjectType Sphere::getType() {
	return SPHERE;
}

void Sphere::debug_info() {
	cout << transformations.size() << endl;
	for(unsigned int i = 0; i < transformations.size(); i++) {
		cout << "type" << type << " center: " << glm::to_string(center) << " radius: " << radius << " pigmentIndex: " << pigmentIndex << " surfaceIndex: " << surfaceIndex << "\ntransType: " << transformations[i].transType << " transMat: " << glm::to_string(transformations[i].transParams) << endl;
	}
}

Plane::Plane(glm::vec4 plane_coefficient, vector<Transformation> transformations, int pigmentIndex, int surfaceIndex) : Object(pigmentIndex, surfaceIndex) {
	this->plane_coefficient = plane_coefficient;
	this->type = PLANE;
	this->transformations = transformations;
}

Plane::~Plane() {

}

void Plane::transform() {
	for(unsigned int i = 0; i < transformations.size(); i++) {
		if(transformations[i].transType == "translate") {
			cout << "translate" << endl;
			plane_coefficient = glm::vec4(glm::vec3(plane_coefficient) + transformations[i].transParams, plane_coefficient.w);
		}
	}
}

double Plane::getIntersection(const Ray ray) {
	glm::vec3 origin = ray.origin;
	glm::vec3 direction = ray.direction;
	glm::vec3 normal = glm::normalize(glm::vec3(plane_coefficient));
	double t = -1;
	if(glm::dot(direction, normal) != 0) {
		t = -(plane_coefficient.w + glm::dot(origin, normal))/(glm::dot(direction, normal));
	}
	return t;
}

glm::vec3 Plane::getNormal(const glm::vec3 point) {
	glm::vec3 normal = glm::normalize(glm::vec3(plane_coefficient));
	return normal;
}

ObjectType Plane::getType() {
	return PLANE;
}

void Plane::debug_info() {
	cout << "type" << type << " plane_coefficient: " << glm::to_string(plane_coefficient) << " pigmentIndex: " << pigmentIndex << " surfaceIndex: " << surfaceIndex << endl;
}

Polyhedron::Polyhedron(int numFaces, vector<glm::vec4> poly_coefficients, int pigmentIndex, int surfaceIndex) : Object(pigmentIndex, surfaceIndex) {
	this->plane_index = -1;
	this->numFaces = numFaces;
	this->poly_coefficients = poly_coefficients;
	this->type = POLYHEDRON;
}

Polyhedron::~Polyhedron() {

}

bool Polyhedron::intersectPolyhedron(const glm::vec3 point, const vector<glm::vec4> poly_coefficients) {
	for(int i = 0; i < numFaces; i++) {
		glm::vec3 normal = glm::normalize(glm::vec3(poly_coefficients[i]));
		if(glm::dot(point, normal) + poly_coefficients[i].w > 0.0001f) {
			return false;
		}
	}
	return true;
}

double Polyhedron::getIntersection(const Ray ray) {
	glm::vec3 origin = ray.origin;
	glm::vec3 direction = ray.direction;
	double tmin = INFINITY;
	bool isIntersect = false;
	for(int i = 0; i < numFaces; i++) {
		glm::vec3 normal = glm::normalize(glm::vec3(poly_coefficients[i]));
		if(glm::dot(direction, normal) != 0) {
			double t = -(poly_coefficients[i].w + glm::dot(origin, normal))/(glm::dot(direction, normal));
			glm::vec3 intersectPoint = ray.origin + ray.direction * t;
			if(t >= 0.00001f && intersectPolyhedron(intersectPoint, poly_coefficients) && t < tmin) {
				isIntersect = true;
				plane_index = i;
				tmin = t;
			}
		}
	}
	if(isIntersect) {
		return tmin;
	} else {
		return -1;
	}
}

glm::vec3 Polyhedron::getNormal(const glm::vec3 point) {
	glm::vec3 facenormal = glm::vec3(poly_coefficients[plane_index]);
	if(plane_index <= 0) {
		return glm::normalize(glm::vec3(0, 0, 1));
	} else {
		return glm::normalize(facenormal);
	}
}

ObjectType Polyhedron::getType() {
	return POLYHEDRON;
}

void Polyhedron::debug_info() {
	cout << "numFaces: " << numFaces << endl;
	for(int i = 0; i < numFaces; i++) {
		cout << "type: " << type << " poly_coefficient: " << glm::to_string(poly_coefficients[i]) << endl;
	}
	cout << "pigmentIndex: " << pigmentIndex << " surfaceIndex: " << surfaceIndex << endl;
}


// Triangle::Triangle(glm::vec3 p1, glm::vec3 p2, glm::vec3 p3, int pigmentIndex, int surfaceIndex, glm::mat4 transformMatrix) : Object(pigmentIndex, surfaceIndex, transformMatrix) {
// 	this->p1 = p1;
// 	this->p2 = p2;
// 	this->p3 = p3;
// }

// double Triangle::intersectionPoints(const Ray ray) {
// 	glm::vec3 origin = ray.origin;
// 	glm::vec3 direction = ray.direction;

// 	vector<double> points;
// 	glm::vec3 e1,e2,h,s,q;
// 	double a,f,u,v,t;

// 	e1 = p2 - p1;
// 	e2 = p3 - p1;
// 	h = glm::cross(direction, e2);
// 	a = glm::dot(e1, h);

// 	if (a > -0.00001 && a < 0.00001)
// 		return(-1);

// 	f = 1/a;
// 	s = origin - p1;
// 	u = f * (glm::dot(s, h));

// 	if (u < 0.0 || u > 1.0)
// 		return(-1);

// 	q = glm::cross(s, e1);
// 	v = f * glm::dot(direction, q);

// 	if (v < 0.0 || u + v > 1.0)
// 		return(-1);


// 	t = f * glm::dot(e2, q);

// 	if (t > 0.00001) {
// 		return t;
// 	}
// 	else
// 		return (-1);
// }

// glm::vec3 Triangle::getNormal(const glm::vec3 point){
// 	return glm::normalize(glm::cross(p2 - p1, p3 - p1));
// }
