# NumS
Math and visualization toolkit for Scala

**STILL UNDER CONSTRUCTION**

This Scala library has the aim to extend Scala's (and Java's) native math, data processing and data visualization capabilities to make it easy to work on scientific tasks with the programming language family of Java. Mainly it's purpose is to solve, visualize and automate my math and algorithm homeworks :)
## Features
* Two Graph visualization classes (currently for Points and Functions), even one with OpenGL rendering
* Matrix and Vector Classes
* MathFunction Class
* Fraction Class (converts decimals to Fractions)
* *MORE TO COME*

## Usage
This project is built with SBT so you can simply built it with the given build file (nums/build.sbt).
If you want to use it as a library delete the example usage File (Main.scala) and deploy it (sbt package).
If you want to use this library in a Java context don't forget to import the scala standard libraries.
This library depends on LWJGL3, it is already imported for Linux, if you want a different version/os natives just replace the files in the lib folder.

## Contribute
Open problems im working on are:
* More Graphical Elements (=> Rects, Circles, maybe even Sets)
* Derivatives and Integrals
* More methods for Vector and Matrix class
* More classes (ideas?)
  * Complex Numbers
    * Quaternions
  * Polynoms
  * Algorithms (e.g. Bezier curves, Newton Polynom)
* More Documentation (Java Doc)
Feel free to contribute and improve this project, I am open to criticism :)
