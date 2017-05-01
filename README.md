# Hay Tracer

### Major functions
`DataTypes.hs` contains the definitions of data types used in the project:
- `Vec3`, `Point`, `Color`, `Vec4`, `Vector2`, `Vector3`
- `Object`
- `Camera`
- `Image`
- `Light`
- `Ray`
- `Pigment`
- `PhongCoef`
- `Surface`

---

`Main.hs` contains the following function:
- `main` (reads input and pass data to `sendRay` function, which returns an array of pixels that gets
  written to output by `writePPM6` function)

---

`Rachel.hs` contains the following functions:
1. **Reading input file:**
   - `readImage`
   - `readCamera`
   - `readLights`
   - `readPigments`
   - `readSurfaces`
   - `readObjects`
2. **Writing to output file:**
   - `writePPM6`
   - `writePPM` (`String` to output file)
   - `stringPPM` (`Word8` to `ByteString`)
3. **Calculating intersections and normals:**
   - `getIntersect`
   - `getNormal`

---

`Xinyue.hs` contains the following functions:


---

`Util.hs` contains the following functions:
1. **Vector operations:**
   - `minus`
   - `plus`
   - `plus3` (add three `Vec3`s together)
   - `vlength`
   - `mult` (multiplication of corresponding components of two `Vec3`s)
   - `multScalar` (scalar-vector multiplication)
   - `normalize`
   - `reflect` (calculate reflection vector)
   - `radians` (degrees to radians)
   - `dot` (dot product)
   - `cross` (cross product)
   - `clampVec`
   - `clamp`
   - `vdistance` (distance between two `Point`s)

---

### Reflections
- What Went Well
- What Went Poorly
- What Can Be Improved



Include in your project a README file that describes the project code, outlining where the major functions are written and, in general, what I should look for when reviewing your work. This file should also include reflections on the project process: What went well? What went poorly? What would you do differently if you had more time?
