#include "main.hpp"

int main(int argc, char* argv[]) {
	if(argc != 2) {
		cout << "Usage: " << argv[0] << " scene_description_input_file" << endl;
		return EXIT_FAILURE;
	}
	read_input(argv[1], objects, pigments, surfaces, lights, image, camera);
	// debug();
	init();
	trace_debug(200, 500);

	// free memories
    // for(int i = 0; i < pigments.size(); i++) {
	   //  delete pigments[i];
    // }
    // for(int i = 0; i < objects.size(); i++) {
    //     delete objects[i];
    // }

   	// free memories
	for(unsigned int i = 0; i < pigments.size(); i++) {
		if(pigments[i]->getType() == SOLID) {
			delete (Solid*)pigments[i];
		} else if(pigments[i]->getType() == CHECKERBOARD) {
			delete (CheckerBoard*)pigments[i];
		}
	}
	for(unsigned int i = 0; i < objects.size(); i++) {
		if(objects[i]->getType() == SPHERE) {
			delete (Sphere*)objects[i];
		} else if(objects[i]->getType() == PLANE) {
			delete (Plane*)objects[i];
		} else if(objects[i]->getType() == POLYHEDRON) {
			delete (Polyhedron*)objects[i];
		}
	}
    // for(unsigned int i = 0; i < pigments.size(); i++) {
		// 	if(pigments[i]->getType() == SOLID) {
		// 		free((Solid*)pigments[i]);
		// 	} else if(pigments[i]->getType() == CHECKERBOARD) {
		// 		free((CheckerBoard*)pigments[i]);
		// 	}
    // }
    // for(unsigned int i = 0; i < objects.size(); i++) {
		// 	if(objects[i]->getType() == SPHERE) {
		// 		free((Sphere*)objects[i]);
		// 	} else if(objects[i]->getType() == PLANE) {
		// 		free((Plane*)objects[i]);
		// 	} else if(objects[i]->getType() == POLYHEDRON) {
		// 		free((Polyhedron*)objects[i]);
		// 	}
    // }
		pigments.clear();
		objects.clear();
		surfaces.clear();
		lights.clear();
    return EXIT_SUCCESS;
}

glm::vec3 phong(Ray ray, glm::vec3 intersectPoint, Object* intersectObject, bool isDebug) {
	glm::vec3 finalColor = glm::vec3(0, 0, 0);
	glm::vec3 direction = ray.direction;
	glm::vec3 normal = intersectObject->getNormal(intersectPoint);
	int pigmentIndex = intersectObject->pigmentIndex;
	int surfaceIndex = intersectObject->surfaceIndex;
	glm::vec3 diffuse = pigments[pigmentIndex]->getColor(intersectPoint);
	double Ka = surfaces[surfaceIndex].Ka;
	double Kd = surfaces[surfaceIndex].Kd;
	double Ks = surfaces[surfaceIndex].Ks;
	double alpha = surfaces[surfaceIndex].alpha;
	finalColor += 0.1f * lights.begin()->color * Ka;

	int i = 0;
	// Cast shadow ray
	for(vector<Light>::iterator light = lights.begin() + 1; light != lights.end(); ++light) {
		glm::vec3 light_pos = light->position;
		glm::vec3 light_dir = light_pos - intersectPoint;
		glm::vec3 light_att = light->attenuation;
		glm::vec3 light_col = light->color;
		double dist_to_light = glm::length(light_dir);
		light_dir = glm::normalize(light_dir);
		Ray shadow_ray = Ray(intersectPoint + light_dir * 0.01f, light_dir);
		// determine whether the intersected object is visible
		bool visible = true;
		for(vector<Object*>::iterator object = objects.begin(); object != objects.end(); ++object) {
			double point = (*object)->getIntersection(shadow_ray);
			if(point < 0 || point > glm::distance(intersectPoint, light_pos)) {
				continue;
			} else {
				visible = false;
				break;
			}
		}
		if(isDebug) {
			cout << "light " << i+1 << ": " << endl;
			cout << "normal:" << endl;
			cout << glm::to_string(normal) << endl;
			cout << "light visibility:" << endl;
			cout << visible << endl;
		}
		if(visible) {
			// Calculate diffuse component
			if(Kd > 0) {
				bool isIn = false;
				if(glm::dot(normal, direction) > 0) {
					// ray is inside of the object, flip normal
					normal = normal * -1;
					isIn = true;
				}
				double diffuseIntensity = glm::dot(light_dir, normal);
				// cout << diffuseIntensity << endl;
				double attenuation = light_att.x + light_att.y * dist_to_light + light_att.z * pow(dist_to_light, 2);
				if(attenuation == 0) {
					attenuation = 0.0001f;
				}
				// Is the object facing the light
				if(diffuseIntensity > 0) {
					finalColor += diffuse * light_col * diffuseIntensity * Kd * (1.0f/attenuation);
				}
			}

			// Calculate specular component
			if(Ks > 0) {
				double specularIntensity = pow(glm::dot(normal, glm::normalize(light_dir - direction)), alpha);
				// Is the object facing the light
				if(specularIntensity > 0) {
					finalColor += specular * specularIntensity * Ks;
				}
			}
		}
		i++;
	}
	return finalColor;
}

glm::vec3 send_ray(Ray ray, int depth, bool isDebug) {
	glm::vec3 direction = ray.direction;

	if(depth > RECURSION_DEPTH) {
		return glm::vec3(BACKGROUND_COLOR, BACKGROUND_COLOR, BACKGROUND_COLOR);
	} else {
		// If diffused object or recursion depth reached
		double intersectPos = INFINITY;
		bool isIntersect = false;
		Object* intersectObject = NULL;
		glm::vec3 color;

		// Checking for nearest intersection
		for(vector<Object*>::iterator object = objects.begin(); object != objects.end(); ++object) {
			double t = (*object)->getIntersection(ray);
			if(t < 0) continue;
			else {
				if(t < intersectPos) {
					isIntersect = true;
					intersectPos = t;
					intersectObject = (*object);
				}
			}
		}
		glm::vec3 intersectPoint = ray.origin + ray.direction * intersectPos;
		if(isDebug) {
			cout << "Does the ray intersect the object?" << endl;
			cout << isIntersect << endl;
			if(isIntersect) {
				cout << "intersectPos:" << endl;
				cout << intersectPos << endl;
				cout << "intersectPoint:" << endl;
				cout << glm::to_string(intersectPoint) << endl;
				cout << "intersectObject:" << endl;
				intersectObject->debug_info();
			}
		}
		// No intersection => background colors
		if(!isIntersect) {
			return glm::vec3(BACKGROUND_COLOR, BACKGROUND_COLOR, BACKGROUND_COLOR);
		} else {
			color += phong(ray, intersectPoint, intersectObject, isDebug);
		}

		int surfaceIndex = intersectObject->surfaceIndex;
		glm::vec3 normal = intersectObject->getNormal(intersectPoint);
		double Kr = surfaces[surfaceIndex].Kr;
		double Kt = surfaces[surfaceIndex].Kt;
		double indexOfRefraction = surfaces[surfaceIndex].indexOfRefraction;

		// send reflection ray
 		if(Kr > 0) {
			// specular reflective
			glm::vec3 reflection_dir = glm::normalize(glm::reflect(direction, normal));
			// cout << "reflection_dir" << glm::to_string(reflection_dir - glm::normalize(2 * glm::dot(-direction, normal) * normal + direction)) << endl;
			Ray reflection_ray = Ray(intersectPoint + reflection_dir * 0.01f, reflection_dir);
			if(isDebug) {
				cout << "Reflection Ray:" << endl;
			}
			color += 0.004 * send_ray(reflection_ray, depth + 1, isDebug) * Kr;
		}
		// send refraction ray
		if(Kt > 0) {
			// translucent
			bool isIn = false;
			if(glm::dot(normal, direction) > 0) {
				// ray is inside of the object, flip normal
				normal = normal * -1;
				isIn = true;
			}

			double n1 = 1;
			double n2 = indexOfRefraction;
			double n = isIn ? n2/n1 : n1/n2;
			double c1 = -glm::dot(direction, normal); // cos(i)
			double c2 = 1 - (n * n * (1 - c1 * c1)); // cos(t)^2
			if(c2 > 0) {
				glm::vec3 refraction_dir = glm::normalize((direction * n) + (normal * (n * c1 - sqrt(c2))));
				Ray refraction_ray = Ray(intersectPoint + refraction_dir * 0.01f, refraction_dir);
				if(isDebug) {
					cout << "Refraction Ray:" << endl;
				}
				color += 0.005 * send_ray(refraction_ray, depth + 1, isDebug) * Kt;
			}
		}
		return clamp(255 * color, glm::vec3(0, 0, 0), glm::vec3(255, 255, 255));
	}
}

void init() {
	glm::vec3 Cz = -glm::normalize(camera.at - camera.center);
	glm::vec3 Cx = glm::normalize(glm::cross(camera.up, Cz));
	glm::vec3 Cy = glm::cross(Cz, Cx);            
	double aspectRatio = image.width/(double)image.height;
	double height = 2 * tan(glm::radians(0.5 * camera.fovy));
	double width = height * aspectRatio;
	vector< vector<glm::vec3> > image_data(image.width, vector<glm::vec3>(image.height, glm::vec3(0, 0, 0)));
	for(int r = 0; r < image.height; r++) {
		for(int c = 0; c < image.width; c++) {
			image_data[c][r] = glm::vec3(0, 0, 0);
			for(int sample = 0; sample < NUM_SAMPLES; sample++) {
				double c_super = c + sample % 3;
				double r_super = r + sample / 3;
				double Pc = ((c_super/(double)image.width) - 0.5) * width;
				double Pr = ((0.5 - (r_super/(double)image.height))) * height;
				glm::vec3 ray_direction = glm::normalize(glm::vec3(Pc * Cx + Pr * Cy + NEAR * Cz));
				Ray ray = Ray(camera.center, ray_direction);
				if(sample == 0 || sample == 2 || sample == 6 || sample == 8) {
					glm::vec3 color = send_ray(ray, 0, 0);
					if(NUM_SAMPLES == 1) {
						image_data[c][r] += color; // without anti-aliasing for testing with faster rendering
					} else {
						image_data[c][r] += 0.0625 * color;
					}
				} else if(sample == 1 || sample == 3 || sample == 5 || sample == 7) {
					glm::vec3 color = send_ray(ray, 0, 0);
					image_data[c][r] += 0.125 * color;
				} else {
					glm::vec3 color = send_ray(ray, 0, 0);
					image_data[c][r] += 0.25 * color;
				}
			}
		}
	}
    int i, j;
    ofstream outputFile(oFileName, ios::binary);
    outputFile << "P6" << endl;
    outputFile << image.width << "\n" << image.height << endl;
    outputFile << "255" << endl;
    for (j = 0; j < image.height; ++j) {
        for (i = 0; i < image.width; ++i) {
            char color[3];
            color[0] = image_data[i][j].x;  /* red */
            color[1] = image_data[i][j].y;  /* green */
            color[2] = image_data[i][j].z;  /* blue */
            outputFile.write(color, 3);
        }
    }
    outputFile.close();
}

void trace_debug(int c, int r) {
/* trace only one ray given the pixel row and column number. 
Print a detailed trace of this ray (objects intersected, 
point of closest intersection, normal vector, light visibility, 
reflection ray, etc.)*/
	glm::vec3 Cz = -glm::normalize(camera.at - camera.center);
	glm::vec3 Cx = glm::normalize(glm::cross(camera.up, Cz));
	glm::vec3 Cy = glm::cross(Cz, Cx);
	double aspectRatio = image.width/(double)image.height;
	double height = 2 * tan(glm::radians(0.5 * camera.fovy));
	double width = height * aspectRatio;
	double Pc = ((c/(double)image.width) - 0.5) * width;
	double Pr = ((0.5 - (r/(double)image.height))) * height;
	glm::vec3 ray_direction = glm::normalize(glm::vec3(Pc * Cx + Pr * Cy + NEAR * Cz));
	Ray ray = Ray(camera.center, ray_direction);
	send_ray(ray, 0, DEBUG);
}

void read_input(string iFileName, vector<Object*> & objects, vector<Pigment*> & pigments, vector<Surface> & surfaces, vector<Light> & lights, Image & image, Camera & camera) {

    ifstream inputFile(iFileName);

    string str, antialiasing;
    int line = 0, lightNum = 0, pigmentNums = 0, surfaceNums = 0, transformationNums = 0, objectNums = 0;
    vector<Transformation> transformations;
    while(inputFile){
        getline(inputFile, str);
        stringstream ss(str);

        if(line == 0){
            cout <<"read file: " << iFileName <<endl;
            ss >> oFileName >> antialiasing;
            if(antialiasing == "antialiasing") {
            	isAntialiasing = true;
            }
        } else if(line == 1) {
            ss >> image.width >> image.height;
        // camera
        } else if(line == 2) {
            ss >> camera.center.x >> camera.center.y >> camera.center.z;
        } else if(line == 3) {
            ss >> camera.at.x >> camera.at.y >> camera.at.z;
        } else if(line == 4) {
            ss >> camera.up.x >> camera.up.y >> camera.up.z;
        }else if(line == 5) {
            ss >> camera.fovy;
        // lightNum
        } else if(line == 6) {
			ss >> lightNum;
        // lights
        } else if(line > 6 && line <= 6 + lightNum) {
            Light tempLight = Light(glm::vec3(0,0,0),glm::vec3(0,0,0),glm::vec3(0,0,0));
            ss >> tempLight.position.x >> tempLight.position.y >> tempLight.position.z >> tempLight.color.x >> tempLight.color.y >> tempLight.color.z >> tempLight.attenuation.x>>tempLight.attenuation.y>>tempLight.attenuation.z;
            lights.push_back(tempLight);
        // pigmentNum
        } else if(line == 7 + lightNum) {
            ss >> pigmentNums;
        // pigments
        } else if(line > 7 + lightNum && line <= 7 + lightNum + pigmentNums) {
            string type;
            ss >> type;
            if(type == "solid") {
                double r, g, b;
                ss >> r >> g >> b;
                Solid* tempPigment = new Solid(glm::vec3(r, g, b));
                pigments.push_back(tempPigment);
            } else if(type == "checker") {
                double r1, g1, b1, r2, g2, b2, sideLenTemp;
                ss >> r1 >> g1 >> b1 >> r2 >> g2 >> b2 >> sideLenTemp;
                CheckerBoard* tempPigment = new CheckerBoard(glm::vec3(r1, g1, b1), glm::vec3(r2, g2, b2), sideLenTemp);
                pigments.push_back(tempPigment);
            }
        // surfaceNums
        } else if(line == 8 + lightNum + pigmentNums) {
            ss >> surfaceNums;
        // surfaces
        } else if(line > 8 + lightNum + pigmentNums && line <= 8 + lightNum + pigmentNums +surfaceNums) {
            double Ka, Kd, Ks, alpha, Kr, Kt, indexOfRefraction;
            ss >> Ka >> Kd >> Ks >> alpha >> Kr >> Kt >> indexOfRefraction;
            surfaces.push_back(Surface(Ka, Kd, Ks, alpha, Kr, Kt, indexOfRefraction));
        // transformationNums
        } else if(line == 9 + lightNum + pigmentNums + surfaceNums) {
            ss >> transformationNums;
        // transformations
        } else if(line > 9 + lightNum + pigmentNums + surfaceNums && line <= 9 + lightNum + pigmentNums + surfaceNums + transformationNums) {
            string type;
            double paramA, paramB, paramC;
            ss >> type >> paramA >> paramB >> paramC;
            if(type == "scale") {
                transformations.push_back(Transformation("scale", glm::vec3(paramA, paramB, paramC)));
            } else if(type == "translate") {
                transformations.push_back(Transformation("translate", glm::vec3(paramA, paramB, paramC)));
            }
        // objectNums
        } else if(line == 10 + lightNum + pigmentNums +surfaceNums + transformationNums) {
            ss >> objectNums;
        // objects
        } else if (objectNums > 0) {
            double pigmentIndex, surfaceIndex;
            int transNums, transIndex;
            string type;
            vector<Transformation> transformation_vector;
            ss >> pigmentIndex >> surfaceIndex >> transNums;
            for(int i = 0; i < transNums; i++) {
                ss >> transIndex;
                transformation_vector.push_back(transformations[transIndex]);
            }
            ss >> type;
            if(type == "sphere") {
   	            double x, y, z, r;
				ss >> x >> y >> z >> r;
				// cout << transformation_vector.size() << endl;
                Sphere* tempSphere = new Sphere(glm::vec3(x, y, z), r, transformation_vector, pigmentIndex, surfaceIndex);
                tempSphere->transform();
                objects.push_back(tempSphere);
            } else if(type == "plane") {
            	double x, y, z, w;
            	ss >> x >> y >> z >> w;
            	Plane* tempPlane = new Plane(glm::vec4(x, y, z, w), transformation_vector, pigmentIndex, surfaceIndex);
            	objects.push_back(tempPlane);
            } else if(type == "polyhedron") {
            	double numFaces;
            	ss >> numFaces;
            	vector<glm::vec4> poly_coeffs;
            	for(int i = 0; i < numFaces; i++) {
	            	getline(inputFile, str);
	        		stringstream ss(str);
	            	double x, y, z, w;
	            	ss >> x >> y >> z >> w;
	            	poly_coeffs.push_back(glm::vec4(x, y, z, w));
	            	line++;
            	}
            	Polyhedron* tempPoly = new Polyhedron(numFaces, poly_coeffs, pigmentIndex, surfaceIndex);
            	objects.push_back(tempPoly);
            } else {
            	break;
            }
        }
        line++;
    }
    transformations.clear();
    inputFile.close();
}

void debug() {
	cout << "image information" << endl;
	cout << "height: " << image.width << " height: " << image.height << endl;
	cout << "camera information" << endl;
	cout << "center: " << glm::to_string(camera.center) << "\nat: " << glm::to_string(camera.at) << "\nup: " << glm::to_string(camera.up) << "\nfovy: " << camera.fovy << endl;
	cout << "light information" << endl;
	for(int i = 0; i < lights.size(); i++) {
		Light light = lights[i];
		cout << "position: " << glm::to_string(light.position) << " color: " << glm::to_string(light.color) << " attenuation: " << glm::to_string(light.attenuation) << endl;
	}
	for(int i = 0; i < pigments.size(); i++) {
		Pigment* pigment = pigments[i];
		cout << "pigment " << i << " information" << endl;
		pigment->debug_info();
	}
	cout << "surface information" << endl;
	for(int i = 0; i < surfaces.size(); i++) {
		Surface surface = surfaces[i];
		cout << "Ka: " << surface.Ka << " Kd: " << surface.Kd << " Ks: " << surface.Ks << " alpha: " << surface.alpha << " Kr: " << surface.Kr << " Kt: " << surface.Kt << " indexOfRefraction: " << surface.indexOfRefraction << endl;
	}
	cout << "transformation information" << endl;
	for(int i = 0; i < objects.size(); i++) {
		if(objects[i]->getType() == SPHERE) {
			Object* sphere = objects[i];
			cout << "object(sphere) " << i << " information" << endl;
			sphere->debug_info();
		} else if(objects[i]->getType() == PLANE) {
			Object* plane = objects[i];
			cout << "object(plane) " << i << " information" << endl;
			plane->debug_info();
		} else if(objects[i]->getType() == POLYHEDRON) {
			Object* polyhedron = objects[i];
			cout << "object(polyhedron) " << i << " information" << endl;
			polyhedron->debug_info();
		}
	}
}
